module Backgrounds.GnomeSlideshow
( xmlString
, document
)
where

import Control.Applicative
import Data.Maybe
import qualified Text.XML.Light as XML

xmlString :: Double -> Maybe Double -> [String] -> String
xmlString duration transDuration files =
  XML.ppContent (document duration transDuration files)

document :: Double -> Maybe Double -> [String] -> XML.Content
document duration transDuration files =
  el "background" (starttime : images duration transDuration files)
  where
    starttime =
      el "starttime" [ el "year"   [ text "2010" ]
                     , el "month"  [ text "01"   ]
                     , el "day"    [ text "01"   ]
                     , el "hour"   [ text "00"   ]
                     , el "minute" [ text "00"   ]
                     , el "second" [ text "00"   ]
                     ]

images :: Double -> Maybe Double -> [String] -> [XML.Content]
images duration transDuration files =
  do (file, fileNext) <- zip files (drop 1 (cycle files))
     static duration file :
       maybeToList ((\d -> transition d file fileNext) <$> transDuration)

static :: Double -> String -> XML.Content
static duration file =
  el "static" [ el "duration" [ text (show duration) ]
              , el "file"     [ text file            ]
              ]

transition :: Double -> String -> String -> XML.Content
transition duration file fileNext =
  el "transition" [ el "duration" [ text (show duration) ]
                  , el "from"     [ text file            ]
                  , el "to"       [ text fileNext        ]
                  ]

el :: String -> [XML.Content] -> XML.Content
el name children = XML.Elem (XML.Element (XML.unqual name) [] children Nothing)

text :: String -> XML.Content
text str = XML.Text (XML.CData XML.CDataText str Nothing)
