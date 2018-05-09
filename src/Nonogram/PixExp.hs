module Nonogram.PixExp 
  ( evalPixExp
  , ExpressionException(..)
  ) where
import Codec.Picture
import Text.ParserCombinators.Parsec
import Data.Word
import Data.Char (digitToInt)
import Control.Exception

evalPixExp :: PixelRGB8 -> String -> Word8 
evalPixExp p e = if evalB $ parseExp p e
                   then maxBound
                   else minBound

data ExpressionException =
    ParsingError String
  | TypingError String
  deriving Show

instance Exception ExpressionException

data Exp =
    Red     Int 
  | Green   Int
  | Blue    Int
  | EVal    Int
  | EBoo    Bool
  | EGra    Exp Exp 
  | ELes    Exp Exp
  | EAnd    Exp Exp
  | EOrr    Exp Exp
  | EMul    Exp Exp
  | EDiv    Exp Exp
  | EAdd    Exp Exp
  | ESub    Exp Exp
  deriving (Show)


comop :: Parser (Exp->Exp->Exp)
comop = fmap (const EGra) (char '>')
         <|> fmap (const ELes) (char '<')

addop :: Parser (Exp->Exp->Exp)
addop = fmap (const EAdd) (char '+')
          <|> fmap (const ESub) (char '-')

mulop :: Parser (Exp->Exp->Exp)
mulop = fmap (const EDiv) (char '/')
          <|> fmap (const EMul) (char '*')

boolop :: Parser (Exp->Exp->Exp)
boolop = fmap (const EAnd) (char '&')
          <|> fmap (const EOrr) (char '|')

pBool :: Parser Exp
pBool = fmap (\_ -> EBoo True) (string "true")
         <|> fmap (\_ -> EBoo False) (string "false")

pNum :: Parser Exp
pNum = fmap (EVal . fromIntegral . read) (many1 digit)

color :: Exp -> Parser Exp
color (Red w)   = fmap (\_ -> Red w)   (char 'r')
color (Green w) = fmap (\_ -> Green w) (char 'g')
color (Blue w)  = fmap (\_ -> Blue w)  (char 'b')

pConsts :: PixelRGB8 -> Parser Exp
pConsts (PixelRGB8 r g b) = color (Red $ fromIntegral r)
                          <|> color (Green $ fromIntegral g) <|> color (Blue $ fromIntegral b)
                          <|> pNum

spaced :: Parser a -> Parser a
spaced p = spaces *> p <* spaces

pTerm, pTerm', pTerm'', pTerm''',pTerm'''' :: PixelRGB8 -> Parser Exp
pTerm     p = spaced (pConsts p) `chainl1` spaced mulop
pTerm'    p = (pTerm p) `chainl1` spaced addop
pTerm''   p = (pTerm' p) `chainl1` spaced comop
pTerm'''  p = pBool <|> (pTerm'' p)
pTerm'''' p = (pTerm''' p) `chainl1` spaced boolop 

evalW :: Exp -> Int
evalW (Red w) = w
evalW (Green w) = w
evalW (Blue w) = w
evalW (EVal w) = w
evalW (EMul l r) = evalW l * evalW r
evalW (EDiv l r) = evalW l `quot` evalW r
evalW (EAdd l r) = evalW l + evalW r
evalW (ESub l r) = evalW l - evalW r
evalW e = throw $ TypingError $ "Expected Int Expression, got:\n" ++ show e

evalB :: Exp -> Bool
evalB (EBoo b) = b
evalB (EGra l r) = evalW l > evalW r
evalB (ELes l r) = evalW l < evalW r
evalB (EAnd l r) = evalB l && evalB r
evalB (EOrr l r) = evalB l || evalB r
evalB e = throw $ TypingError $ "Expected Boolean Expression, got:\n" ++ show e


parseExp :: PixelRGB8 -> String -> Exp
parseExp pix s =
    case runParser p () "" s of
      Left err  -> throw $ ParsingError $ show err
      Right e   -> e
  where
    p = do spaces
           e <- pTerm'''' pix
           spaces
           eof
           return e


