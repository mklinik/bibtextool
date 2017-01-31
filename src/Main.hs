module Main where

import Text.BibTeX.Parse
import qualified Text.BibTeX.Entry as Entry
import qualified Text.BibTeX.Format as Format
import Text.Parsec.String

import System.Environment (getArgs)
import System.Console.GetOpt

import Data.Char (isSpace, toLower)

data Mode
  = Prettyprint
  deriving (Show,Eq)

data Options = Options
  { bibtexFile :: String
  , mode :: Mode
  }

options :: [OptDescr (Options -> Options)]
options =
  [
  ]

header :: String
header = "Usage: bibtextool <FILE>"

parseOptions :: [String] -> IO Options
parseOptions [] = ioError (userError (usageInfo header options))
parseOptions (file:args) = case getOpt Permute options args of
  (o,_,[]) -> do
    return $ foldl (flip id) (Options file Prettyprint) o
  (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))

main = do
  args <- getArgs
  opts <- parseOptions args

  let bibtexParser = skippingLeadingSpace file

  result <- parseFromFile bibtexParser (bibtexFile opts)
  case result of
    Left err -> print err
    Right items ->
      case mode opts of
        Prettyprint -> prettyprint items


prettyprint = mapM_ (putStr . myEntry)


-- case insensitive string similarity
levenshtein :: String -> String -> Int
levenshtein s t =
    d !! (length s) !! (length t)
    where d = [[distance m n|n<-[0..length t]]|m<-[0..length s]]
          distance i 0 = i
          distance 0 j = j
          distance i j = minimum [d!!(i-1)!!j+1, d!!i!!(j-1)+1, d!!(i-1)!!(j-1) +
            (if (toLower $ s !! (i-1)) == (toLower $ t !! (j-1)) then 0 else 1)]


-- my personal entry formatting: no line breaks in values; normalized spaces

normalizeSpaces :: String -> String
normalizeSpaces [] = []
normalizeSpaces [c] = [c]
normalizeSpaces (c1:c2:rest)
  | isSpace c1 && isSpace c2 = normalizeSpaces (' ':rest)
  | otherwise = (c1 : normalizeSpaces (c2 : rest))

formatValue :: [Entry.FieldValue] -> String
formatValue l = Format.hashSepList (map formatValuePart l)

formatValuePart :: Entry.FieldValue -> String
formatValuePart (Entry.Naked v) = normalizeSpaces v
formatValuePart (Entry.Quoted v) = "{" ++ normalizeSpaces v ++ "}"

myEntry :: Entry.T -> String
myEntry (Entry.Entry entryType bibId items) =
   let formatItem (name, value) =
         "  "++map toLower name++" = "++ formatValue value ++",\n"
   in  "@" ++ entryType ++ "{" ++ bibId ++ ",\n" ++
       concatMap formatItem items ++
       "}\n\n"
myEntry (Entry.BibString name value) =
  "@string{" ++ name ++ " = " ++ formatValue value ++ "}\n\n"
myEntry (Entry.Comment cmt) = cmt
