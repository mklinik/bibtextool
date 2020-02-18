module Main where

import Text.BibTeX.Parse
import qualified Text.BibTeX.Entry as Entry
import Text.BibTeX.Entry
import qualified Text.BibTeX.Format as Format

import System.Environment (getArgs)
import System.Console.GetOpt

import Text.Parsec.String
import Data.Char (isSpace, toLower, toUpper)
import Text.Regex
import Data.List
import Data.Maybe
import Control.Monad

data Mode
  = Prettyprint
  | FindDuplicates
  deriving (Show,Eq)

data Options = Options
  { bibtexFile :: String
  , mode :: Mode
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['p'] ["prettyprint"] (NoArg (\o -> o { mode = Prettyprint })) "prettyprint all items"
  , Option [] ["find-dupes"] (NoArg (\o -> o { mode = FindDuplicates })) "find and report potential duplicate, based on title entry"
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
        FindDuplicates -> findDuplicates items


prettyprint = mapM_ (putStr . formatEntry)


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
  "@string{" ++ name ++ " = " ++ formatValue value ++ "}\n\n"
formatEntry (Entry.Comment cmt) = cmt



findDuplicates entries = do
  -- all entries that have a title field
  let
    titledEntries =
      [ e
      | e@Entry.Entry{Entry.fields=fs} <- map Entry.lowerCaseFieldNames $ entries
      , isJust $ lookup "title" fs
      ]
    titledEntries_ =
      [ e { Entry.fields = ("mangledTitle", [Entry.Quoted $ mangleValue titleValue]) : fs }
      | e@Entry.Entry{Entry.fields=fs} <- titledEntries
      , let (Just [titleValue]) = lookup "title" fs
      ]
  mapM_ checkSimilarity $ combinations titledEntries_


checkSimilarity :: (Entry.T, Entry.T) -> IO ()
checkSimilarity (entryA@Entry{fields=fieldsA}, entryB@Entry{fields=fieldsB}) = do
  let
    (Just [Quoted mTitleA]) = lookup "mangledTitle" fieldsA
    (Just [Quoted mTitleB]) = lookup "mangledTitle" fieldsB
    distance = levenshtein mTitleA mTitleB
  when (distance < 8) $ do
    putStrLn $ "distance: " ++ show distance
    putStrLn $ Entry.identifier entryA
    print $ fromJust $ lookup "title" fieldsA
    putStrLn $ Entry.identifier entryB
    print $ fromJust $ lookup "title" fieldsB
    putStrLn ""


combinations list = [ (x,y) | (x:xs) <- tails list, y <- xs ]
