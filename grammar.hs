{-# LANGUAGE OverloadedStrings #-}

module Grammar (
  Expression(..), parserExpression
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad ((>=>), forM, forM_, foldM, liftM)
import Data.Maybe
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy as B
import qualified Data.List as List
import Text.Parsec.Prim (parserZero, parserReturn)
import qualified Text.Parsec.Pos as ParsecPos
import Text.ParserCombinators.Parsec

{- | Represents an expression which we'll seek out in JSON -}
data Expression
  = NamedEntry String
  | IndexedEntry Int
  | CurrentEntry
  | Children
  | Descendants
  | TerminalNode
  | IfMatch Expression Expression -- Take fst IF snd
  | AnyMatch Expression Expression
  | InSequence Expression Expression
  deriving (Eq, Show)

{- | Parse a name token.  On match return a function which will match any child element matching
     that name.
-}
parserName :: Parser Expression
parserName = do
  otherLetters <- many1 letter
              <|> betweenThese '\''
              <|> betweenThese '\"'
  return $ NamedEntry otherLetters
  where betweenThese :: Char -> Parser String
        betweenThese x = between (char x) (char x) (many $ satisfy (x /=))

-- Variant of "satisfy" which allows us to immediately select a value (rather than just a bool)
satisyChoose :: (Char -> Maybe a) -> Parser a
satisyChoose = tokenPrim (\c -> show [c]) (\pos c _ -> ParsecPos.updatePosChar pos c)

-- | Parse a digit, usable as an array index
digitInt :: Parser Int
digitInt = satisyChoose (`List.elemIndex` "0123456789") <?> "digit"

-- | Parse an index, return the Expression for an array idnexed appropriately
parserIndex :: Parser Expression
parserIndex = do
  followingDigits <- many1 digitInt
  let value = List.foldl ((+) . (*) 10) 0 followingDigits
  return $ IndexedEntry value

-- | Find the current item.  Yes, it's trivial.
parserCurrentItem :: Parser Expression
parserCurrentItem = do
  char '.'
  return CurrentEntry

-- | Match a star.  Return a matcher which will find all children of the current item.
parserAllChildren :: Parser Expression
parserAllChildren = do
  char '*'
  return Children

-- | Match a ellipsis.  Return a matcher which will find all children, recursively.
parserAllDescendants :: Parser Expression
parserAllDescendants = do
  try $ string "..."
  return Descendants

-- | Match an at sign.  Return a matcher which will pass through strings, numbers, and bools.
-- | The matcher will reject any other type of data.
parseTerminal :: Parser Expression
parseTerminal = do
  char '@'
  return TerminalNode

parserSubPart :: Parser x -> Parser x
parserSubPart = between (char '(') (char ')')

parserSuffixConditional :: Parser Expression -> Parser Expression
parserSuffixConditional part = do
  prefixExpr <- part
  option prefixExpr (char '?' >> spaces >> liftM (IfMatch prefixExpr) part)

parserPathPart :: Parser Expression -> Parser Expression
parserPathPart alternater =
  choice $ try <$>
    [ parserAllDescendants
    , parserName
    , parserIndex
    , parserCurrentItem
    , parseTerminal
    , parserAllChildren
    , parserSubPart alternater]

parserPath2 :: Parser Expression -> Parser Expression
parserPath2 alternater =
  chainl1 (parserSuffixConditional $ parserPathPart alternater)
          (spaces >> char '/' >> return InSequence)



parserPartAlternates :: Parser Expression
parserPartAlternates =
  chainl1 (parserPath2 parserPartAlternates)
          (spaces >> char ',' >> return AnyMatch)

parserExpression :: Parser Expression
parserExpression = do
  r <- parserPartAlternates
  spaces
  eof
  return r
