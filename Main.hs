{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import qualified Backgrounds.GnomeSlideshow as GS
import qualified Backgrounds.Shotwell as Shotwell

import Control.Applicative
import Data.Maybe
import System.Console.CmdArgs
import qualified System.Directory

data SBS = SBS { databaseFile       :: String
               , tagName            :: String
               , duration           :: Double
               , transitionDuration :: Maybe Double
               }
  deriving (Data, Typeable, Show)

sbsDef :: String -> SBS
sbsDef dbFile =
  SBS { databaseFile        = dbFile &= opt dbFile &= typFile
                           &= name "database" &= explicit
                           &= help "Shotwell database filename"
      , tagName             = defTagName &= opt defTagName &= typ "NAME"
                           &= name "tag" &= explicit
                           &= help "Shotwell tag name"
      , duration            = defDur &= opt defDur
                           &= name "duration" &= explicit
                           &= help "Per-image duration"
      , transitionDuration  = defTransDur &= opt optTransDur
                           &= name "transition-duration" &= explicit
                           &= help "Transition duration"
      }
  &= summary "shotwell-background-slideshow"
  where
    defTagName :: String
    defTagName  = "Backgrounds"

    defDur :: Double
    defDur = 30*60

    defTransDur, optTransDur :: Maybe Double
    defTransDur = Nothing
    optTransDur = Just 5

main :: IO ()
main =
  do dbFile <-  (++ "/.shotwell/data/photo.db")
            <$> System.Directory.getHomeDirectory

     sbs    <-  cmdArgs (sbsDef dbFile)

     files  <-  either error id
            <$> Shotwell.filenames (databaseFile sbs) (tagName sbs)

     let duration' = duration sbs - fromMaybe 0 (transitionDuration sbs)

     putStrLn (GS.xmlString duration' (transitionDuration sbs) files)
