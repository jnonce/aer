{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Monad ((>=>), forM, forM_, foldM, liftM, when, unless)
import Data.Aeson
import Data.Maybe
import Data.Text
import Grammar
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.Vector as Vector
import System.Console.CmdArgs
import System.Exit (exitFailure)
import System.IO (stdout)
import System.IO.Error (tryIOError)
import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Error as ParsecError

data Aer = Aer
  { expr :: String
  , showExpr :: Bool
  , files :: [FilePath] }
  deriving (Data,Typeable,Show,Eq)
aerOpts = Aer
  { expr = def &= argPos 0 &= typ "EXPRESSION" -- &= help "Path within the JSON to scan"
  , showExpr = def &= help "Print the parsed expression for debugging"
  , files = def &= args &= typFile
  } &= summary "Scan JSON for subpieces of data"


type JSONMatch = (Value -> [Value])

matcherOf :: Expression -> JSONMatch
matcherOf (NamedEntry name) = findNamed (pack name)
matcherOf (IndexedEntry index) = findIndexed index
matcherOf CurrentEntry = pure
matcherOf Children = findChildren
matcherOf Descendants = depthSearch (\ a i -> (a, findChildren i)) ()
matcherOf TerminalNode = \value -> [value | isTerminal value]
matcherOf (IfMatch takeThis ifThis) = findIfMatch (matcherOf takeThis) (matcherOf ifThis)
matcherOf (InSequence x y) = matcherOf x >=> matcherOf y
matcherOf (AnyMatch x y) = resultsOfEither (matcherOf x) (matcherOf y)


isTerminal :: Value -> Bool
isTerminal (Aeson.String _) = True
isTerminal (Aeson.Number _) = True
isTerminal (Aeson.Bool _) = True
isTerminal _ = False

findNamed :: Text ->JSONMatch
findNamed name (Aeson.Object obj) =
  maybeToList $ HashMap.lookup name obj
findNamed _ _ = []

findIndexed :: Int -> JSONMatch
findIndexed index (Aeson.Array arry) =
  maybeToList $ (Vector.!?) arry index
findIndexed _ _ = []

findChildren :: JSONMatch
findChildren (Aeson.Array arry) = Vector.toList arry
findChildren (Aeson.Object obj) = HashMap.elems obj
findChildren _ = []

findIfMatch :: JSONMatch -> JSONMatch -> JSONMatch
findIfMatch takeThis ifThis value =
  takeThis value >>= takeOnlyOnMatch
  where takeOnlyOnMatch :: JSONMatch
        takeOnlyOnMatch value
          | List.null innerMatch = []
          | otherwise = [value]
          where innerMatch = ifThis value

resultsOfEither :: JSONMatch -> JSONMatch -> JSONMatch
resultsOfEither x y value = x value ++ y value

depthSearch :: (acc -> a -> (acc, [a])) -> acc -> a -> [a]
depthSearch deepen state start =
  items ++ List.concatMap (depthSearch deepen state2) items
  where (state2, items) = deepen state start


liftAsIO :: (Show a) => Either a b -> IO b
liftAsIO (Left err) = fail $ show err
liftAsIO (Right value) = return value

-- Bind IO and print (rather that)
safeIO :: (x -> IO ()) -> IO x -> IO Bool
safeIO f value = do
  r <- tryIOError value
  case r of
    Left errorValue -> do
      print errorValue
      return False
    Right value -> do
      f value
      return True

processPathOnJSON :: Expression -> FilePath -> IO ()
processPathOnJSON expr filename = do
  let matcherFromExpr = matcherOf expr
  jsonBytes <- B.readFile filename
  jsonRoot <- liftAsIO (decodedJson jsonBytes)
  forM_ (matcherFromExpr jsonRoot) (putStrLnByteString . encode)
  where decodedJson :: B.ByteString -> Either String Value
        decodedJson json = eitherDecode json :: Either String Value
        putStrLnByteString byteString = do
          B.hPutStr stdout byteString
          putStrLn ""

main = do
  parsedOptions <- cmdArgs aerOpts
  expr <- liftAsIO $ parse parserExpression "expr" (expr parsedOptions)
  when (showExpr parsedOptions) $ print expr

  results <- forM (files parsedOptions) (safeIO print . processPathOnJSON expr)
  unless (List.and results) exitFailure
