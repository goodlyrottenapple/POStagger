{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Core where
import Data.List
import qualified Data.Map as Map
import Control.Monad (liftM2)

type Word = String
type Tag = String
type POS = (Core.Word, Core.Tag)

instance {-# OVERLAPPING #-} Show POS where
    show (a,b) = a ++ "|" ++ b

type Sentence = [POS]

instance {-# OVERLAPPING #-} Show Sentence where
    show s = intercalate " " (map show s)

word :: POS -> Core.Word
word = fst

tag :: POS -> Core.Tag
tag = snd

remPOS :: Sentence -> String
remPOS = intercalate " " . map word

-- adds dummy *start* and *end* tags
startEndTags :: Sentence -> Sentence
startEndTags s = (("*start*", "*start*") : s) ++ [("*end*", "*end*")]

-- -- counts the occurences of a single tag in a sentence
-- tagOccs :: Tag -> Sentence -> Int
-- tagOccs t = foldl (\acc word -> if t == tag word then acc+1 else acc) 0 

-- -- counts the occurences of a single tag in a list of sentences
-- tagOccsL :: Tag -> [Sentence] -> Int
-- tagOccsL t = foldl (\acc s -> acc + tagOccs t s) 0

allTagOccsSentence :: Sentence -> Map.Map Tag Int
allTagOccsSentence = foldl (\m (w,t) -> Map.insertWith' (+) t 1 m) Map.empty

allTagOccs :: [Sentence] -> Map.Map Tag Int
allTagOccs = foldl (\m s -> Map.unionWith (+) m (allTagOccsSentence s)) Map.empty


-- -- counts the occurences of a two tags following each other in a sentence
-- twoTagsOccs :: Tag -> Tag -> Sentence -> Int
-- twoTagsOccs t1 t2 [] = 0
-- twoTagsOccs t1 t2 [a] = 0
-- twoTagsOccs t1 t2 [a,b] = if t1 == tag a && t2 == tag b then 1 else 0
-- twoTagsOccs t1 t2 (a:b:xs) = (if t1 == tag a && t2 == tag b then 1 else 0) + twoTagsOccs t1 t2 (b:xs)

-- -- counts the occurences of a two consecutive tags in a list of sentences
-- twoTagsOccsL :: Tag -> Tag -> [Sentence] -> Int
-- twoTagsOccsL t1 t2 = foldl (\acc s -> acc + twoTagsOccs t1 t2 s) 0


allTwoTagsOccsSentence :: Sentence -> Map.Map (Tag,Tag) Int
allTwoTagsOccsSentence [(_,t1),(_,t2)] = Map.fromList [((t1,t2), 1)]
allTwoTagsOccsSentence (a:b:xs) = Map.insertWith' (+) (tag a, tag b) 1 (allTwoTagsOccsSentence (b:xs))
allTwoTagsOccsSentence _ = Map.empty

allTwoTagsOccs :: [Sentence] -> Map.Map (Tag,Tag) Int
allTwoTagsOccs = foldl (\m s -> Map.unionWith (+) m (allTwoTagsOccsSentence s)) Map.empty


-- -- calculates bigram P(Cat_i|Cat_i−1), with added +1 smoothing
-- pTags :: Tag -> Tag -> [Sentence] -> Float
-- pTags t1 t2 list = (fromIntegral $ twoTagsOccsL t1 t2 list + 1) / (fromIntegral $ tagOccsL t1 list)


-- rmDups :: (Ord a) => [a] -> [a]
-- rmDups = map head . group . sort

-- foundTags :: Sentence -> [Core.Tag]
-- foundTags = rmDups . (map tag)

-- foundTagsL :: [Sentence] -> [Core.Tag]
-- foundTagsL = rmDups . concatMap foundTags

-- cartProd :: [a] -> [b] -> [(a, b)]
-- cartProd = liftM2 (,)

pAllTags :: [Sentence] -> Map.Map (Tag, Tag) Float
pAllTags list = Map.mapWithKey (\(t1,_) count -> (fromIntegral $ count+1) / (fromIntegral $ singleTag Map.! t1)) (allTwoTagsOccsL list)
    where singleTag = allTagOccsL list