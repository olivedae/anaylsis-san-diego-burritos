{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Decisions.Prep
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

import Data.Maybe
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
  parseField ""    = pure NoScore
  parseField "0"   = pure LowScore
  parseField "0.5" = pure LowScore
  parseField "1"   = pure LowScore
  parseField "1.5" = pure LowScore
  parseField "2"   = pure LowScore
  parseField "2.5" = pure LowScore
  parseField "3"   = pure AverageScore
  parseField "3.5" = pure AverageScore
  parseField "4"   = pure AverageScore
  parseField "4.5" = pure HighScore
  parseField "4.5" = pure HighScore
  parseField "5.0" = pure HighScore

-- todo
instance FromField Cost where
  parseField _ = pure AverageCost

data Burrito = Burrito
  { yelp        :: !Score
  , google      :: !Score
  , cost        :: !Cost
  , hunger      :: !Score
  , tortilla    :: !Score
  , temperature :: !Score
  , meat        :: !Score
  , fillings    :: !Score
  , meatToFill  :: !Score
  , uniformity  :: !Score
  , salsa       :: !Score
  , wrap        :: !Score
  , overall     :: !Score
  } deriving (Show, Read, Eq, Ord)

instance FromNamedRecord Burrito where
  parseNamedRecord r = Burrito
    <$> r .: "Yelp"
    <*> r .: "Google"
    <*> r .: "Cost"
    <*> r .: "Hunger"
    <*> r .: "Tortilla"
    <*> r .: "Temp"
    <*> r .: "Meat"
    <*> r .: "Fillings"
    <*> r .: "Meat:filling"
    <*> r .: "Uniformity"
    <*> r .: "Salsa"
    <*> r .: "Wrap"
    <*> r .: "overall"

store
  :: a
  -> a
store = id
-- store
--  :: FilePath
--  -> [Burrito]
--  -> IO ()
-- store filepath set = writeFile filepath $ show set

clean
  :: [Burrito]
  -> [Burrito]
clean = List.filter (isJust . (flatten allFeatures))

allFeatures
  :: [Burrito -> Maybe Integer]
allFeatures =
  [ category . yelp
  , category . google
  , category . cost
  , category . hunger
  , category . tortilla
  , category . temperature
  , category . meat
  , category . fillings
  , category . meatToFill
  , category . uniformity
  , category . salsa
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
flattenAll fs pokemon = lift pokemon
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
