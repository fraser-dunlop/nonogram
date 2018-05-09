{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Nonogram.Server where
import Nonogram.PixExp
import Nonogram.Params
import Yesod hiding (get)
import Yesod.WebSockets
import Data.Text
import GHC.Generics
import Control.Concurrent.STM.Lifted
import Control.Monad.Trans.State
import Control.Monad
import Conduit
import Codec.Picture
import Codec.Picture.Extra
import Data.Text.Encoding
import qualified Data.ByteString.Base64 as BS64
import qualified Data.ByteString.Lazy as BS
import Data.Aeson
import qualified Data.HashMap.Strict as H
import Text.Julius
import qualified Data.Text.Lazy as TL
import Data.Semigroup
import Control.Concurrent (threadDelay)
import Control.Exception hiding (Handler)

data App = App

instance Yesod App

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

defExpr, defWidth, defHeight :: Text
defExpr = "r > 120 & g > 120 & b > 120"
defWidth = "20"
defHeight = "20"

getHomeR :: Handler Html
getHomeR = do
  webSockets nononono
  defaultLayout $ do
    addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/p5.js/0.6.0/p5.min.js"
    addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/p5.js/0.6.1/addons/p5.dom.min.js"
    toWidget [hamlet|
      <div id="sketch-holder">
      <span>
        Width: 
        <textarea id="width-box" rows="1" cols="4" onkeyup="newWidth();">
          #{defWidth} 
        Height:
        <textarea id="height-box" rows="1" cols="4" onkeyup="newHeight();">
          #{defHeight} 
      <span>
        <p>Enter a boolean expression on r g b - you can use < > * / - + | &
        <textarea id="expression-box" rows="4" cols="50" onkeyup="newExpression();">
            #{defExpr}
      <div>
        <code id="param-code" style="white-space:pre-wrap;display:block;">
    |]
    toWidget [julius|
      var url = document.URL,
          conn;
      url = url.replace("http:", "ws:").replace("https:", "wss:");
      conn = new WebSocket(url);
      conn.onmessage = function(e) {
        console.log(e.data);
        window.eval(e.data);
      };

      function setup() {
        var c = createCanvas(710, 400);
        c.parent("sketch-holder");
        background(100);
        c.drop(gotFile);
      }

      function draw() {
        fill(255);
        noStroke();
        textSize(24);
        textAlign(CENTER);
        text('Drag an image file to start.', width/2, height/2);
        noSmooth();
        noLoop();
      }

      function gotFile(file) { 
        console.log(file.data.split(",")[0]);
        console.log(file.data.split(",").length);
        conn.send(JSON.stringify( { "ImageUpload" : { "data" : file.data.split(",")[1]}}));
      }
      function newExpression() {
        var bo = document.getElementById("expression-box"); 
        conn.send(JSON.stringify({"ExpressionUpdate" : { "data" : bo.value }}));
      }
      function newWidth() {
        var bo = document.getElementById("width-box");
        conn.send(JSON.stringify({"WidthUpdate" : { "data" : parseInt(bo.value) }}));
      }
      function newHeight() {
        var bo = document.getElementById("height-box");
        conn.send(JSON.stringify({"HeightUpdate" : { "data" : parseInt(bo.value) }}));
      }


    |]

nononono :: WebSocketsT Handler ()
nononono = do
  writeChan <- atomically $ (newTChan :: STM (TChan Text)) 
  readChan <- atomically $ dupTChan writeChan
  race_
    (runStateT ((forever $ atomically (readTChan readChan) >>= react >> display)
      ) (Nono Nothing (50,50) defExpr) 
    )
    (runConduit $ sourceWS .| mapM_C (\msg -> atomically $ writeTChan writeChan msg))

data NonogramState =
  Nono {
    workingImage :: Maybe DynamicImage
  , scaleXY :: (Int, Int)
  , expr  :: Text
  }


react :: Text -> StateT NonogramState (WebSocketsT Handler) ()
react (decode . BS.fromStrict . encodeUtf8  -> Just (ImageUpload t)) =
         case decodeImage (BS64.decodeLenient $ encodeUtf8 t) of
            Left e -> liftIO $ putStrLn e
            Right im -> do
              liftIO $ putStrLn "Image successfully decoded"
              st <- get
              case workingImage st of
                Nothing -> put $ st { workingImage = Just im }
                Just _ -> liftIO $ putStrLn "Already working on an image"
react (decode . BS.fromStrict . encodeUtf8  -> Just (ExpressionUpdate t)) = do
  st <- get
  put $ st {expr = t }
react (decode . BS.fromStrict . encodeUtf8  -> Just (WidthUpdate t)) = do
  if t > 0
    then do
      st <- get
      put $ st {scaleXY = (t, snd $ scaleXY st) }
    else return ()
react (decode . BS.fromStrict . encodeUtf8  -> Just (HeightUpdate t)) = do
  if t > 0
    then do
      st <- get
      put $ st {scaleXY = (fst $ scaleXY st, t) }
    else return ()
react _ = liftIO $ putStrLn "nomatch"


display :: StateT NonogramState (WebSocketsT Handler) ()
display = do
  st <- get
  case workingImage st of
    Nothing -> return ()
    Just i -> do
      imageident <- lift $ lift newIdent
      imgdata <- lift $ lift newIdent
      ima <- liftIO $ catch (do
                     let !im = pixelMap (\p->evalPixExp p (unpack $ expr st))
                                        $ scaleBilinear (fst $ scaleXY st) (snd $ scaleXY st)
                                        $ convertRGB8 i
                         !pf = essParam im
                     return $ Right (im,pf))
                           (\(e :: SomeException) -> return $ Left e)
      case ima of
        Left _ -> return ()
        Right (im, prm) -> do
          let x = 10 * (fst $ scaleXY st)
              y = 10 * (snd $ scaleXY st)
          sendTextData $ javaText $ [julius|
        resizeCanvas(#{rawJS $ show x}, #{rawJS $ show y});
        var #{rawJS imgdata} = "#{rawJS $ "data:image/png;base64," <> (decodeUtf8 $ BS64.encode $ BS.toStrict $ encodePng im)}";
        var #{rawJS imageident} = createImg(#{rawJS imgdata});
        #{rawJS imageident}.hide();
        |]
          liftIO $ threadDelay 50000
          sendTextData $ javaText $ [julius|
        image(#{rawJS imageident}, 0, 0, #{rawJS $ show x}, #{rawJS $ show y});
        |]
          sendTextData $ javaText $ [julius|
        document.getElementById("param-code").innerHTML = '#{rawJS prm}';
        |]


javaText :: (t -> Javascript) -> Text 
javaText j = do
    TL.toStrict <$> renderJavascript $ j undefined

data Message =
    ImageUpload Text
  | ExpressionUpdate Text
  | WidthUpdate Int
  | HeightUpdate Int

instance FromJSON Message where
  parseJSON (Object v) =
    case H.lookup "ImageUpload" v of
      Just (Object h) -> ImageUpload <$> h .: "data"
      Nothing -> 
        case H.lookup "ExpressionUpdate" v of
          Just (Object h) -> ExpressionUpdate <$> h .: "data"
          Nothing ->
            case H.lookup "WidthUpdate" v of
              Just (Object h) -> WidthUpdate <$> h .: "data"
              Nothing -> 
                case H.lookup "HeightUpdate" v of
                  Just (Object h) -> HeightUpdate <$> h .: "data"
                  Nothing -> mzero 


runServer :: IO ()
runServer = do
  let port = 4321
  putStrLn $ "Nonogram servelet running on port: " ++ show port
  warp port App


