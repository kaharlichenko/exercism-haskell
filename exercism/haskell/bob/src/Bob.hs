module Bob (responseFor) where

import           Data.Char   (isLetter, isPunctuation, isSpace, isUpper)
import           Data.Text   (Text)
import qualified Data.Text   as T

responseFor :: Text -> Text
responseFor message
  | question && yell = T.pack "Calm down, I know what I'm doing!"
  | question = T.pack "Sure."
  | yell = T.pack "Whoa, chill out!"
  | silence = T.pack "Fine. Be that way!"
  | otherwise = T.pack "Whatever."
  where
    letters  = T.filter isLetter message
    question = T.singleton '?' `T.isSuffixOf` T.filter isPunctuation message
    silence  = T.all isSpace message
    yell
      | T.null letters = False
      | otherwise      = T.all isUpper letters
