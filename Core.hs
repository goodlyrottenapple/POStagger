{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Core where
import Data.List (intercalate, intersect, sortBy)
import qualified Data.Map as M
import Control.Monad (liftM2)
import Data.Char (toLower)
import Data.Tuple (swap)
import Data.Ord (comparing)
import Data.List.Split (splitOn)

import Debug.Trace
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


prod :: [a] -> [b] -> [(a, b)]
prod = liftM2 (,)

showSentences :: [Sentence] -> String
showSentences = (intercalate "\n\n") . (map show)

-- adds dummy *start* and *end* tags
startEndTags :: Sentence -> Sentence
startEndTags s = (("*start*", ["*start*"]) : s) ++ [("*end*", ["*end*"])]


type TagCountMap = M.Map Tag Int
type TagTagCountMap = M.Map (Tag, Tag) Int
type WordTagCountMap = M.Map (Core.Word, Tag) Int

tagOccsSentence :: Sentence -> TagCountMap
tagOccsSentence = foldl (\m (w,list) -> foldl (\m' t -> M.insertWith (+) t 1 m') m list) M.empty

genTagOccsMap :: [Sentence] -> TagCountMap
genTagOccsMap = foldl (\m s -> M.unionWith (+) m (tagOccsSentence s)) M.empty


-- generates the occurences of consecutive word tags in (w,[t,t2...,tn]) and (w',[t',t'2...,t'm])
-- by incerementing the values (t,t'), (t,t'2), ...(tn,t'm) in the map by one
tagTagOccsSentence :: Sentence -> TagTagCountMap
tagTagOccsSentence [(_,t1),(_,t2)] = foldl (\m pair -> M.insertWith (+) pair 1 m) M.empty (t1 `prod` t2)
tagTagOccsSentence (a:b:xs) = foldl (\m pair -> M.insertWith (+) pair 1 m) (tagTagOccsSentence (b:xs)) (tags a `prod` tags b)
tagTagOccsSentence _ = M.empty

genTagTagOccsMap :: [Sentence] -> TagTagCountMap
genTagTagOccsMap = foldl (\m s -> M.unionWith (+) m (tagTagOccsSentence s)) M.empty


-- folds the list of (Word,[Tag]) pairs into a map indexed by (Word,Tag) pairs.
wordTagOccsSentence :: Sentence -> WordTagCountMap
wordTagOccsSentence = foldl (\m (w,list) -> foldl (\m' t -> M.insertWith (+) (lowerCase w,t) 1 m') m list) M.empty where lowerCase = map toLower

genWordTagOccsMap :: [Sentence] -> WordTagCountMap
genWordTagOccsMap = foldl (\m s -> M.unionWith (+) m (wordTagOccsSentence s)) M.empty



-- creates a mapping, which for (T2,T1), returns the probability of T1 being followed by T2
genTagTagProbFun :: TagCountMap -> TagTagCountMap -> ((Tag, Tag) -> Float)
genTagTagProbFun tagOccsMap tagTagOccsMap = \(j,i) -> (log ((countTT (i,j)) / countT i))
    where   countTT (i,j) = fromIntegral $ M.findWithDefault 1 (i,j) tagTagOccsMap
            countT x = fromIntegral $ M.findWithDefault 1 x tagOccsMap

genWordTagProbFun :: TagCountMap -> WordTagCountMap -> ((Core.Word, Tag) -> Float)
genWordTagProbFun tagOccsMap wordTagOccsMap = \(w,t) -> (log ((countWT (w,t)) / countT t))
    where   countWT (w,t) = fromIntegral $ M.findWithDefault 1 (w,t) wordTagOccsMap
            countT x = fromIntegral $ M.findWithDefault 1 x tagOccsMap




tagCountMapToStr :: TagCountMap -> String
tagCountMapToStr m = intercalate "\n" $ map (\(k,v) -> k ++ " " ++ show v) $ M.toList m

strToTagCountMap :: String -> TagCountMap
strToTagCountMap = M.fromList . (map (\x -> case splitOn " " x of [a,b] -> (a, read b :: Int))) . lines 


strStrCountMapToStr :: M.Map (String, String) Int -> String
strStrCountMapToStr m = intercalate "\n" $ map (\((t1,t2),v) -> t1 ++ " " ++ t2 ++ " " ++ show v) $ M.toList m

strToStrStrCountMap :: String -> M.Map (String, String) Int
strToStrStrCountMap = M.fromList . (map (\x -> case splitOn " " x of [a1,a2,b] -> ((a1,a2), read b :: Int))) . lines 


-- containsTags :: [Tag] -> Sentence -> Bool
-- containsTags list s = [] /= filter (([] /=) . (list `intersect`) . tags) s

-- filterSentencesWTags :: [Tag] -> [Sentence] -> [Sentence]
-- filterSentencesWTags t = filter (containsTags t)

viterbi' :: M.Map Tag Float -> M.Map Tag [Tag] -> ((Core.Word, Tag) -> Float) -> ((Tag, Tag) -> Float) -> [Core.Word] -> [Tag]
viterbi' score backPtr pWT pTT [] = reverse $ backPtr M.! (findMax score)
    where findMax = fst . last . sortBy (comparing snd) . M.toList
viterbi' score backPtr pWT pTT (w:ords) = viterbi' newScore newBackPtr pWT pTT ords `debug` ( w  ++ "\n\n" ++(intercalate "\n" $ map show $ M.toList newScore) ++ "\n\n" ++ (intercalate "\n" $ map show $ M.toList newBackPtr) ++ "\n\n===============================\n\n")
    where 
        newScore = M.fromList [(i, snd $ maxK i) | i <- M.keys score]
        newBackPtr = M.fromList [(i, (fst$maxK i):(backPtr M.! i)) | i <- M.keys backPtr]
        maxK :: Tag -> (Tag, Float)
        maxK i = last $ sortBy (comparing snd) [(k, (score M.! k) + (pTT (i, k)) + (pWT (w, i))) | k <- M.keys score]


initScore :: ((Core.Word, Tag) -> Float) -> ((Tag, Tag) -> Float) -> Core.Word -> [Tag] -> M.Map Tag Float
initScore pWT pTT w tags = M.fromList [(t, pWT (w, t) + pTT (t, "*start*")) | t <- tags]

initBackPtr :: [Tag] -> M.Map Tag [Tag]
initBackPtr tags = M.fromList [(t, []) | t <- tags]


viterbi :: [Tag] -> ((Core.Word, Tag) -> Float) -> ((Tag, Tag) -> Float) -> [Core.Word] -> [Tag]
viterbi tags pWT pTT (w:ords) = viterbi' (initScore pWT pTT w tags) (initBackPtr tags) pWT pTT (ords ++ ["*end*"])

-- score_init :: ((Core.Word, Tag) -> Float) -> ((Tag, Tag) -> Float) -> [Tag] -> Int -> Core.Word -> Matrix Float
-- score_init pWT pTT tags sentenceLength word = matrix (length tags) sentenceLength fun
--     where fun = \(i, w) -> if w > 1 then 0.0 else pWT(word, (tags !! (i-1))) + pTT((tags !! (i-1)), "*start*")



