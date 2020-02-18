module Main where

import Text.BibTeX.Parse
import qualified Text.BibTeX.Entry as Entry
import qualified Text.BibTeX.Format as Format

import System.Environment (getArgs)
import System.Console.GetOpt
import System.Exit (die)

import Text.Parsec.String
import Data.Char (isSpace, toLower, toUpper)
import Text.Regex
import Data.List
import Data.List.Utils

data Mode
  = Prettyprint
  | ExtractBooktitles
  deriving (Show,Eq)

data Options = Options
  { bibtexFile :: String
  , mode :: Mode
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['b'] ["extractBooktitles"] (NoArg (\o -> o { mode = ExtractBooktitles })) "extraction mode"
  , Option ['p'] ["prettyprint"] (NoArg (\o -> o { mode = Prettyprint })) "prettyprint all items"
  ]

header :: String
header = "Usage: bibtextool <FILE>"

parseOptions :: [String] -> IO Options
parseOptions [] = ioError (userError (usageInfo header options))
parseOptions args = case getOpt Permute options args of
  (o,(file:_),[]) -> do
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
        ExtractBooktitles -> extractBooktitles items


prettyprint = mapM_ (putStr . formatEntry)

extractBooktitles [] = return ()
extractBooktitles (item:items) = do
  case item of
    (Entry.Entry eType eIdentifier eFields) -> do
      newItem <- mkStringEntry item
      putStr $ formatEntry newItem
    _ -> putStr $ formatEntry item
  extractBooktitles items

theFieldToExtract "inproceedings" = "booktitle"
theFieldToExtract "incollection" = "booktitle"
theFieldToExtract "proceedings" = "title"

mkStringEntry :: Entry.T -> IO Entry.T
mkStringEntry e@(Entry.Entry eType _ _) =
  if eType `elem` ["inproceedings", "incollection", "proceedings"]
    then extractString (theFieldToExtract eType) e
    else return e

extractString fieldToExtract e@(Entry.Entry eType eIdentifier eFields) =
  case lookup fieldToExtract eFields of
    Nothing -> die $ eIdentifier ++ ": no such field name: " ++ fieldToExtract
    Just [fieldValue@(Entry.Naked _)] -> return e
    Just [fieldValue@(Entry.Quoted _)] -> do
      putStr $ formatEntry $ Entry.BibString mangeledValue [fieldValue]
      return (Entry.Entry eType eIdentifier newFields)
      where
        newFields = addToAL eFields fieldToExtract [Entry.Naked mangeledValue]
        mangeledValue = "str" ++ mangleValue fieldValue
    _ -> die $ eIdentifier ++ ": concatenated value in: " ++ fieldToExtract


-- makes identifier from value
--   for example mangleValue "{P}roc. of {USENIX}'99" -> procOfUsenix99
-- 1. replace all punctuation by whitespace
-- 2. camelCase all words
-- 3. concatenate them
mangleValue :: Entry.FieldValue -> String
mangleValue (Entry.Naked str) = str
mangleValue (Entry.Quoted input) = concat camelCaseWords
  where
    camelCaseWords = map capitalize inputWords
    capitalize [] = []
    capitalize (letter:word) = toUpper letter : map toLower word
    inputWords = words inputWithoutPunctuation
    inputWithoutPunctuation = subRegex (mkRegex "[^[:alnum:]]") inputWithoutBraces " "
    inputWithoutBraces = subRegex (mkRegex "[{}]") input ""

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
normalizeSpaces input =
  subRegex (mkRegex "[[:space:]]+") input " "

formatValue :: [Entry.FieldValue] -> String
formatValue l = Format.hashSepList (map formatValuePart l)

formatValuePart :: Entry.FieldValue -> String
formatValuePart (Entry.Naked v) = normalizeSpaces v
formatValuePart (Entry.Quoted v) = "{" ++ normalizeSpaces v ++ "}"

formatEntry :: Entry.T -> String
formatEntry (Entry.Entry eType eIdentifier eFields) =
   let formatItem (name, value) =
         "  " ++ map toLower name ++ " = " ++ formatValue value ++ ",\n"
   in  "@" ++ map toLower eType ++ "{" ++ eIdentifier ++ ",\n" ++
       concatMap formatItem eFields ++
       "}\n\n"
formatEntry (Entry.BibString name value) =
  "@string{" ++ name ++ " =\n  " ++ formatValue value ++ "}\n\n"
formatEntry (Entry.Comment cmt) = cmt
