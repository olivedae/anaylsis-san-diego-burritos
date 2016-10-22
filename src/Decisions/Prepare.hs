{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Decisions.Prepare
( Burrito (..)
, Score (..)
, Cost (..)
, clean
, toList
, open
, store
, decode
, flattenAll
, flatten
, allFeatures
) where

import qualified Data.ByteString.Char8 as BChar
import Data.Maybe
import qualified Text.Read as Read
import qualified Data.List as List
import Control.Exception (IOException)
import qualified Control.Exception as Exception
import System.IO
import Control.Applicative
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import Data.Csv
  ( DefaultOrdered(headerOrder)
  , FromField(parseField)
  , FromNamedRecord(parseNamedRecord)
  , (.:)
  )
import qualified Data.Csv as Cassava
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Foldable as Foldable

class Category c where
  category
    :: c
    -> Maybe Integer

data Score =
    NoScore
  | LowScore
  | AverageScore
  | HighScore
  deriving (Show, Read, Eq, Ord)

data Cost =
    NoCost
  | LowCost
  | AverageCost
  | HighCost
  deriving (Show, Read, Eq, Ord)

instance Category Score where
  category NoScore = Nothing
  category LowScore = Just 1
  category AverageScore = Just 2
  category HighScore = Just 3

instance Category Cost where
  category NoCost = Nothing
  category LowCost = Just 1
  category AverageCost = Just 2
  category HighCost = Just 3

instance FromField Score where
  parseField str
    | isNothing score = pure NoScore
    | otherwise       = pure $ toScore (fromJust score)
      where score = unwrap str :: Maybe Double
            toScore s
              | s > 4     = LowScore
              | s > 2.5   = AverageScore
              | otherwise = HighScore

instance FromField Cost where
  parseField str
    | isNothing cost = pure NoCost
    | otherwise      = pure $ toCost (fromJust cost)
      where cost = unwrap str :: Maybe Double
            toCost c
              | c > 8.5   = HighCost
              | c > 6.5   = AverageCost
              | otherwise = LowCost

unwrap
  :: (Read a)
  => BChar.ByteString
  -> Maybe a
unwrap = Read.readMaybe . BChar.unpack

data Burrito = Burrito
  { cost        :: !Cost
  , hunger      :: !Score
  , tortilla    :: !Score
  , meat        :: !Score
  , fillings    :: !Score
  , meatToFill  :: !Score
  , uniformity  :: !Score
  , wrap        :: !Score
  , overall     :: !Score
  } deriving (Show, Read, Eq, Ord)

instance FromNamedRecord Burrito where
  parseNamedRecord r = Burrito
    <$> r .: "Cost"
    <*> r .: "Hunger"
    <*> r .: "Tortilla"
    <*> r .: "Meat"
    <*> r .: "Fillings"
    <*> r .: "Meat:filling"
    <*> r .: "Uniformity"
    <*> r .: "Wrap"
    <*> r .: "overall"

store
 :: FilePath
 -> [Burrito]
 -> IO ()
store filepath set = writeFile filepath $ show set

clean
  :: [Burrito]
  -> [Burrito]
clean = List.filter (isJust . (flatten allFeatures))

allFeatures
  :: [Burrito -> Maybe Integer]
allFeatures =
  [ category . cost
  , category . hunger
  , category . tortilla
  , category . meat
  , category . fillings
  , category . meatToFill
  , category . uniformity
  , category . wrap
  , category . overall ]

open
  :: FilePath
  -> IO (Either String (Vector Burrito))
open filepath =
  catchShowIO (ByteString.readFile filepath)
    >>= return . either Left decode

catchShowIO
  :: IO a
  -> IO (Either String a)
catchShowIO action =
  fmap Right action `Exception.catch` handle
  where handle
          :: IOException
          -> IO (Either String a)
        handle = return . Left . show

toList
  :: Vector Burrito
  -> [Burrito]
toList = Vector.toList

flattenAll
  :: [Burrito -> Maybe Integer]
  -> [Burrito]
  -> [Maybe [Integer]]
flattenAll fs burrito = lift burrito
  where lift = fmap (flatten fs)

flatten
  :: [Burrito -> Maybe Integer]
  -> Burrito
  -> Maybe [Integer]
flatten fs burrito = sequenceA res
  where res = getZipList $ ZipList fs <*> ZipList (repeat burrito)

decode
  :: ByteString
  -> Either String (Vector Burrito)
decode =
  fmap snd . Cassava.decodeByName
