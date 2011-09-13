module Backgrounds.Shotwell
( filenames
)
where

import Control.Applicative
import Control.Exception
import Control.Monad
import Database.SQLite
import Data.Int
import qualified Data.List.Split as Split
import Numeric

filenames :: String -> String -> IO (Either String [String])
filenames dbFile tagName =
  withDatabase dbFile (filenames' tagName)

filenames' :: String -> SQLiteHandle -> IO (Either String [String])
filenames' tagName c =
  fmap rowsFilenames
    <$> execParamStatement c query [(":tag_name", Text tagName)]
  where
    rowsFilenames :: [[Row String]] -> [String]
    rowsFilenames rowss =
      do rows       <- rowss
         row        <- rows
         (col, val) <- row
         guard (col == "filename")
         return val

query :: String
query = "select p.filename filename \
        \from PhotoTable p \
        \join TagTable t \
        \  on has_tag(p.id, t.photo_id_list) \
        \where t.name = :tag_name"

withDatabase :: String -> (SQLiteHandle -> IO a) -> IO a
withDatabase filename f =
  bracket (openReadonlyConnection filename)
          closeConnection
          (\c -> createFunction c "has_tag" hasTag >> f c)

hasTag :: Int64 -> String -> Int
hasTag photoId = fromEnum . elem photoId . parseField
  where
    parseField = parseItem <=< Split.endBy ","

    parseItem str = do ("thumb", numS) <- pure (splitAt 5 str)
                       [(num, "")]     <- pure (readHex numS)
                       pure num
