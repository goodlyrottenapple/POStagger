{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Core where

import Data.List (intercalate)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Debug.Trace (trace)

debug = flip trace

type Word = String
type Tag = String

type POS = (Core.Word, [Core.Tag])

instance {-# OVERLAPPING #-} Show POS where
    show (a,b) = intercalate "|" (a:b)

type Sentence = [POS]

instance {-# OVERLAPPING #-} Show Sentence where
    show s = intercalate " " (map show s)

word :: POS -> Core.Word
word = fst

tags :: POS -> [Core.Tag]
tags = snd

-- adds dummy *start* and *end* tags
startEndTags :: Sentence -> Sentence
startEndTags s = (("*start*", ["*start*"]) : s) ++ [("*end*", ["*end*"])]

type TagCountMap = M.Map Tag Int
type TagTagCountMap = M.Map (Tag, Tag) Int
type WordTagCountMap = M.Map (Core.Word, Tag) Int

-- helper function that turns the TagCountMap into a string, to be written into a text file
tagCountMapToStr :: TagCountMap -> String
tagCountMapToStr m = intercalate "\n" $ map (\(k,v) -> k ++ " " ++ show v) $ M.toList m

-- helper function that turns a string (produced by tagCountMapToStr) back into a TagCountMap map
strToTagCountMap :: String -> TagCountMap
strToTagCountMap = M.fromList . (map (\x -> case splitOn " " x of [a,b] -> (a, read b :: Int))) . lines 

-- helper function that turns a `M.Map (String, String) Int` map into a string, to be written into a text file
strStrCountMapToStr :: M.Map (String, String) Int -> String
strStrCountMapToStr m = intercalate "\n" $ map (\((t1,t2),v) -> t1 ++ " " ++ t2 ++ " " ++ show v) $ M.toList m

-- helper function that turns a string (produced by strStrCountMapToStr) back into a `M.Map (String, String) Int` map.
strToStrStrCountMap :: String -> M.Map (String, String) Int
strToStrStrCountMap = M.fromList . (map (\x -> case splitOn " " x of [a1,a2,b] -> ((a1,a2), read b :: Int))) . lines 

