module Main where

import Prelude

import Data.Array (replicate, length, head, reverse, filter, snoc, zip)
import Data.Foldable (foldl)
import Data.Function.Uncurried (Fn1, runFn1, Fn4, runFn4)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing))
import Data.String (length, joinWith) as S
import Data.String.Common (split)
import Data.String.Pattern (Pattern(..))
import Data.Traversable (for)
import Data.Tuple (Tuple, fst, snd)
import Effect (Effect)
import Effect.Console (log, clear)
import Effect.Ref as R
import Node.Encoding(Encoding(UTF8), byteLength)
import Node.FS.Sync(readTextFile)
import Node.Process (stdin, stdout, argv, exit)
import Node.Stream (Readable, Writable)

import Parser (parse)

foreign import getColumns :: Int
foreign import getRows :: Int

-- ref: https://github.com/purescript-node/purescript-node-streams/blob/v4.0.1/src/Node/Stream.purs
foreign import onKeypressImpl :: Fn4 (Readable ()) (Writable ()) (Boolean) (PressedKeyInfo -> Effect Unit) (Effect Unit)
onKeypress :: Readable () -> Writable () -> Boolean -> (PressedKeyInfo -> Effect Unit) -> Effect Unit
onKeypress = runFn4 onKeypressImpl

foreign import onResizeImpl :: Fn1 (Int -> Int -> Effect Unit) (Effect Unit)
onResize :: (Int -> Int -> Effect Unit) -> Effect Unit
onResize = runFn1 onResizeImpl

-- FIXME 1Mのファイル開くのが遅い
-- FIXME クオートで囲まれてないと変になる

main :: Effect Unit
main = do
  let screenCol = getColumns
  let screenRow = getRows

  onResize showSize
  args <- argv

  let fileName = case length args of
                   3 -> getFileName args
                   _ -> Nothing

  let fn = case fileName of
             Just hoge -> hoge
             _ -> ""

  txt <- readTextFile UTF8 fn
  srs <- R.new 0

  csv <- parse txt ","

  let csv_ = filter (\x -> length x > 0) csv -- TODO filter???
  let csvTable = tablize { csv : getCsv csv_ , columnWidth : getCsvWidth csv_ }
  let txtRecord = createTextRecord csvTable

  clear
  showTxtRecord $ filter (\x -> 0 < x.row && x.row < (0 + screenRow)) txtRecord
  onKeypress stdin stdout true (displayTxt screenRow txtRecord srs)

tablize :: CSV -> String
tablize csv_ = 
    table <> "\n" <> bottom
    where
          table = foldl (\v -> \row_ -> csvFold v row_ csv_) "" csv_.csv
          bottom = case head $ filter (\r -> S.length r > 0) $ split (Pattern "\n") table of -- TODO filter???
                        Just h -> h
                        Nothing -> ""


csvFold :: String -> Row -> CSV -> String
csvFold vv row_ csv_ = vv <> "\n" <> makeRow (foldl (\y -> \tupleCell -> {fst: y.fst <> (makeOutLine (snd (tupleCell) + 2)), snd: y.snd <> ((getPt (fst tupleCell) (snd (tupleCell) + 2)))}) {fst: "", snd: ""} (zipRow row_.row csv_.columnWidth)) <> "|"

zipRow :: (Array Cell) -> (Array Int) -> Array (Tuple Cell Int)
zipRow ac i = zip ac i

getPt :: Cell -> Int -> String
getPt c i = "|" <> c.paddingText <> S.joinWith ""  (replicate ((i - getByteLength c.paddingText)) " ")

getByteLength :: String -> Int
getByteLength t = foldl (\x -> \y -> x + (if byteLength y UTF8 == 1 then 1 else 2)) 0 $ split (Pattern "") t

makeOutLine :: Int -> String
makeOutLine i = "+" <> (foldl (\v -> \_ -> v <> "-") "" $ replicate i "-")

makeRow :: {fst :: String, snd :: String} -> String
makeRow r = r.fst <> "+\n" <> r.snd

getCsv :: Array (Array String) -> Array Row
getCsv csv = foldl (\val -> \acc -> 
               val <> [{ row : getRow acc
                       , maxHeight: foldl (\v -> \a -> max v a) 0 (map (\x -> length (split (Pattern "\n") x)) acc)
                       }]
               ) [] csv

getCsvWidth :: Array (Array String) -> Array Int
getCsvWidth csv = foldl (\val -> \acc -> 
                    if length acc == 0 then
                      val
                    else if length val == 0 then
                      map (\str -> foldl (\x -> \y -> x + (if byteLength y UTF8 == 1 then 1 else 2)) 0 $ split (Pattern "") str) acc
                    else map (\x -> if fst x < snd x then snd x else fst x) (zip val (map (\str -> foldl (\x -> \y -> x + (if byteLength y UTF8 == 1 then 1 else 2)) 0 $ split (Pattern "") str) acc))) [] csv

getRow :: Array String -> Array Cell
getRow csv = map (\v -> { text: v
                        , paddingText: " " <> v <> " "
                        , maxHeight: 0
                        }
                 ) csv

type CSV = { csv :: Array Row , columnWidth :: Array Int }

type Row = { row :: Array Cell , maxHeight :: Int }

type Cell = { text :: String , paddingText :: String , maxHeight :: Int }

sepalater :: Pattern
sepalater = Pattern ","

showTxtRecord :: Array TxtRecord -> Effect Unit
showTxtRecord t = do
  clear
  let firstRow = case head t of
              Just h -> h.char
              Nothing -> ""

  a <- for t \x -> do
     let columns = split sepalater x.char
     let rowLength = S.length x.char + length columns
     pure $ foldl (\xx -> \y -> if xx == "" then y else xx <> y) "" columns

  log $ S.joinWith "\n"  a

type TxtRecord = {row:: Int, char:: String}
initTxtRecord :: Array TxtRecord
initTxtRecord = []

createTextRecord :: String -> Array TxtRecord
createTextRecord txt =  foldl (\x -> \y -> snoc x {row: (length x + 1), char: y}) initTxtRecord (split (Pattern "\n") txt)

getFileName :: Array String -> Maybe String
getFileName args = head $ reverse args

displayTxt :: Int -> Array TxtRecord -> (R.Ref Int) -> PressedKeyInfo -> Effect Unit
displayTxt screenRow txtReords srs (PressedKeyInfo pki) = do
  let name = if pki.shift then
               case pki.name of
                "g" -> "end"
                _   -> pki.name
             else
               case pki.name of
                "h" -> "left"
                "j" -> "down"
                "k" -> "up"
                "l" -> "right"
                "f" -> "forward"
                "b" -> "back"
                "g" -> "top"
                "G" -> "end"
                "q" -> "exit"
                _   -> pki.name
  case name of
       "up" -> R.modify_ (\s -> if s < 1 then s else s - 1) srs
       "down" -> R.modify_ (\s -> if (s + screenRow) > (length txtReords) then s else s + 1) srs
       "back" -> R.modify_ (\s -> if s - screenRow < 1 then 0 else if s < 1 then s else s - screenRow) srs
       "forward" -> R.modify_ (\s -> if (s + screenRow) > (length txtReords) - screenRow + 1 then (length txtReords) - screenRow + 1 else s + screenRow) srs
       "top" -> R.modify_ (\s -> 0) srs
       "end" -> R.modify_ (\s -> (length txtReords) - screenRow + 1) srs
       _   -> R.modify_ (\s -> s) srs
  newStartRow  <- R.read srs
  let dtxt = filter (\x -> newStartRow < x.row && x.row < (newStartRow + screenRow)) txtReords

  if name == "exit" then
    exit 0
  else
    showTxtRecord dtxt

showSize :: Int -> Int -> Effect Unit
showSize col row = do
  log "col"
  log $ show col
  log "row"
  log $ show row

newtype PressedKeyInfo = PressedKeyInfo {
  sequence :: String
 ,name :: String
 ,ctrl :: Boolean
 ,meta :: Boolean
 ,shift :: Boolean
}

derive instance genericPressedKeyInfo :: Generic PressedKeyInfo _
instance showPressedKeyInfo :: Show PressedKeyInfo where
  show (PressedKeyInfo {
  sequence : s
 ,name : n
 ,ctrl : c
 ,meta : m
 ,shift : sh
}) = "{ sequence: " <> (show s) <> " ,name: " <> n <> " ,ctrl: " <> (show c) <> " ,meta: " <> (show m) <> " ,shift: " <> (show sh) <> " }"


