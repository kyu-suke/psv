module Parser where

import Prelude

import Effect (Effect)
import Data.Array (init, last)
import Data.Maybe (Maybe(Just, Nothing))
import Data.String.Common (split)
import Data.String.Pattern (Pattern(..))
import Data.String.Regex (regex, replace)
import Data.String.Regex.Flags (RegexFlags(..))
import Data.Foldable (foldl)
import Data.Either (Either(..))

type Delimiter = String
type AfterQuote = Boolean
type InsideQuoteCell = Boolean
type ReadyToEndQuote = Boolean
type ParseParam = { csv :: Array (Array String)
                  , aq :: AfterQuote
                  , iqc :: InsideQuoteCell
                  , rteq :: ReadyToEndQuote 
                  }

regexFlag :: RegexFlags
regexFlag = RegexFlags
  { global: true
  , ignoreCase: false
  , multiline: true
  , sticky: false
  , unicode: true
  }

convertToCrlf :: String -> String
convertToCrlf s = case regex "\r\n|\r|\n" regexFlag of
                    Left r -> "error"
                    Right r -> replace r "\n" s

initValue :: ParseParam
initValue = { csv: []
            , aq: false
            , iqc: false
            , rteq: false 
            }

parse :: String -> Delimiter -> Effect (Array (Array String))
parse s d = do
  let csv = foldl (\val -> \char -> readChar val char) initValue (split (Pattern "") (convertToCrlf s))
  pure csv.csv

addChar :: Array (Array String) -> Array String -> String -> String -> Array (Array String)
addChar csv row str char = csv <> [row <> [str <> char]]

addCell :: Array (Array String) -> Array String -> String -> Array (Array String)
addCell csv row cell = csv <> [row <> [cell]]

addRow :: Array (Array String) -> Array String -> Array (Array String)
addRow csv row = csv <> [row]

getInitCsv :: Array (Array String) -> Array (Array String)
getInitCsv csv = case init csv of
                   Just x -> x
                   Nothing -> []

getLastRow :: Array (Array String) -> Array String
getLastRow csv = case last csv of
                   Just x -> x
                   Nothing -> []

getRow :: Array String -> Array String
getRow lastRow = case init lastRow of
                   Just x -> x
                   Nothing -> []

getStr :: Array String -> String
getStr lastRow = case last lastRow of
                   Just x -> x
                   Nothing -> ""

readChar :: ParseParam -> String -> ParseParam
readChar pp char = if pp.iqc == false then
                     { csv: csv
                     , aq: pp.aq
                     , iqc: if char == "\"" then true else pp.iqc
                     , rteq: pp.rteq
                     }
                   else if char == "\"" && pp.aq == true && pp.iqc == true then
                     { csv: addChar initCsv row str char
                     , aq: false
                     , iqc: pp.iqc
                     , rteq: false
                     }
                   else if pp.aq == true && pp.iqc == true && pp.rteq == true then
                     { csv: resCsv
                     , aq: false
                     , iqc: if char /= "\"" then false else pp.iqc
                     , rteq: false
                     }
                   else if pp.aq == true && pp.iqc == true && pp.rteq == false then
                     { csv: addChar initCsv row str char
                     , aq: false
                     , iqc: pp.iqc
                     , rteq: false
                     }
                   else if char == "\"" && pp.aq == false && pp.iqc == true then
                     { csv: pp.csv
                     , aq: true
                     , iqc: pp.iqc
                     , rteq: true
                     }
                   else if pp.aq == false && pp.iqc == true then
                     { csv: addChar initCsv row str char
                     , aq: pp.aq
                     , iqc: pp.iqc
                     , rteq: pp.rteq
                     }
                   else
                     initValue
  where
      initCsv = getInitCsv pp.csv
      lastRow = getLastRow pp.csv
      row = getRow lastRow
      str = getStr lastRow
      csv = if char == "," then
              addCell initCsv (row <> []) str
            else if char == "\n" then
              addRow (addCell initCsv row str) row
            else if char == "\"" then
              pp.csv
            else
              addChar initCsv row "" (str <> char)
      resCsv = if char == "\"" then
                 addChar initCsv lastRow str char
               else
                 if char == "," then -- "," is delimiter char
                     addCell initCsv (lastRow <> []) ""
                 else if char == "\n" then
                     addRow pp.csv []
                 else
                     pp.csv

