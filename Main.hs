{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main
  ( exampleMain
  , main
  ) where

import qualified Data.Text as Text

import Control.Applicative  ( (<|>) )
import Control.Monad        ( void )
import Data.Attoparsec.Text ( (<?>)
                            , IResult( Done
                                     , Fail
                                     , Partial
                                     )
                            , Parser
                            , anyChar
                            , char
                            , endOfInput
                            , endOfLine
                            , manyTill
                            , parse
                            , satisfy
                            , skipSpace
                            , string
                            )
import Data.Char            ( isDigit )
import Data.Text            ( Text )

data Entry = Entry
  { year   :: Year
  , poster :: Poster
  , quote  :: Quote
  , author :: Author
  } deriving (Eq, Ord, Show)

type Author = Text
type Poster = Text
type Quote  = Text
type Year   = Text

main :: IO ()
main = interact transmogrify

transmogrify :: String -> String
transmogrify s = case parse entriesP . Text.pack $ s of
  Fail _remaining contexts e -> renderError contexts e
  Partial cont -> case cont "" of
    Fail _ contexts e -> renderError contexts e
    Partial _ -> error "partial cont after empty"
    Done _ entries -> renderEntries entries
  Done _ entries -> renderEntries entries
  where
    renderError contexts e =
      error $ "transmogrify.fail: " <> show contexts <> ": " <> e
    renderEntries = Text.unpack
      . Text.unlines
      . map renderEntry

entriesP :: Parser [Entry]
entriesP = manyTill entryP endOfInput

entryP :: Parser Entry
entryP = do
  year   <- yearP <?> "entry year"
  poster <- lexeme posterP <?> "entry poster"
  void newlineP
  quote  <- quoteP <?> "entry quote"
  author <- lexeme authorP <?> "author"
  pure $ Entry
    { year = year
    , poster = poster
    , quote = quote
    , author = author
    }

newlineP :: Parser ()
newlineP = void . satisfy $ (== '\n')  -- TODO more

lexeme :: Parser a -> Parser a
lexeme p = do
  void skipSpace
  p

yearP :: Parser Year
yearP = yearP' <?> "a year from a timestamp"
  where
    yearP' = do
      void (char '[' <?> "opening '[' for timestamp")
      void (month <?> "month")
      void (slash <?> "slash after month")
      void (day <?> "day")
      void (slash <?> "slash after day")
      y <- year <?> "year"
      void (space <?> "space after year")
      void (hour <?> "hour")
      void (colon <?> "colon")
      void (minutes <?> "minutes")
      void (space <?> "space after minutes")
      void (meridiem <?> "meridiem")
      void (char ']' <?> "closing ']' for timestamp")
      pure y

    slash = char '/' <?> "a slash"
    space = char ' ' <?> "a space"
    colon = char ':' <?> "a colon"

    month = (digits 2 <|> digits 1) <?> "month digits"
    day   = (digits 2 <|> digits 1) <?> "day digits"
    year  = (digits 4 <?> "year digits")

    hour    = (digits 2 <|> digits 1) <?> "hour digits"
    minutes = digits 2 <?> "minutes digits"

    meridiem = (string "AM" <|> string "PM") <?> "AM or PM"

digits :: Int -> Parser Text
digits = \case
  0 -> pure ""
  n -> do
    d <- digit <?> "a digit in digits"
    Text.cons d <$> digits (n - 1)

digit :: Parser Char
digit = satisfy isDigit <?> "a digit"

posterP :: Parser Poster
posterP = Text.pack <$> manyTill anyChar endOfLine

quoteP :: Parser Quote
quoteP = do
  void . char $ '"'
  Text.pack <$> manyTill anyChar (char '"')

authorP :: Parser Author
authorP = do
  void . char $ '-'
  Text.pack <$> manyTill anyChar endOfLine

renderEntry :: Entry -> Text
renderEntry Entry {..} = mconcat
  [ "\""
  , quote
  , "\" -"
  , author
  , ", "
  , year
  ]

example :: String
example = unlines
  [ "[12/20/2022 1:11 PM] lgastako"
  , ""
  , "\"I like bacon\" -lgastako"
  , "[12/20/2023 1:11 PM] kallyzdo"
  , ""
  , "\"I like bacon\" -Albert Einstein"
  ]

exampleMain :: IO ()
exampleMain = putStrLn . transmogrify $ example
