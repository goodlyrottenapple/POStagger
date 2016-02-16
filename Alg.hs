module Alg where
import Core

import Data.Char (toLower)
import Data.List (intercalate)
import qualified Data.Map.Strict as M
import Control.Monad (liftM2)


prod :: [a] -> [b] -> [(a, b)]
prod = liftM2 (,)


lowerCase = map toLower


-- generates a map of tags with the number of times they occur in a sentence
tagOccsSentence :: Sentence -> TagCountMap
tagOccsSentence = foldl (\m (w,list) -> foldl (\m' t -> M.insertWith (+) t 1 m') m list) M.empty

-- generates a map of tags with the number of times they occur in a corpus
-- calls tagOccsSentence for each sentence and merges the resulting maps
genTagOccsMap :: [Sentence] -> TagCountMap
genTagOccsMap = foldl (\m s -> M.unionWith (+) m (tagOccsSentence s)) M.empty


-- generates the occurences of consecutive word tags in (w,[t,t2...,tn]) and (w',[t',t'2...,t'm])
-- by incrementing the values (t,t'), (t,t'2), ...(tn,t'm) in the map by one
tagTagOccsSentence :: Sentence -> TagTagCountMap
tagTagOccsSentence [(_,t1),(_,t2)] = foldl (\m pair -> M.insertWith (+) pair 1 m) M.empty (t1 `prod` t2)
tagTagOccsSentence (a:b:xs) = foldl (\m pair -> M.insertWith (+) pair 1 m) (tagTagOccsSentence (b:xs)) (tags a `prod` tags b)
tagTagOccsSentence _ = M.empty

-- calls tagTagOccsSentence for each sentence from the corpus and merges the resulting maps
genTagTagOccsMap :: [Sentence] -> TagTagCountMap
genTagTagOccsMap = foldl (\m s -> M.unionWith (+) m (tagTagOccsSentence s)) M.empty


-- folds the list of (Word,[Tag]) pairs into a map indexed by (Word,Tag) pairs.
wordTagOccsSentence :: Sentence -> WordTagCountMap
wordTagOccsSentence = foldl (\m (w,list) -> foldl (\m' t -> M.insertWith (+) (lowerCase w,t) 1 m') m list) M.empty

-- calls wordTagOccsSentence for each sentence and merges the resulting maps
genWordTagOccsMap :: [Sentence] -> WordTagCountMap
genWordTagOccsMap = foldl (\m s -> M.unionWith (+) m (wordTagOccsSentence s)) M.empty


type TagTagProb = (Tag, Tag) -> Float
type WordTagProb = (Core.Word, Tag) -> Float

-- creates a function, which, for (t2,t1), returns the probability of t1 being followed by t2
genTagTagProbFun :: TagCountMap -> TagTagCountMap -> TagTagProb
genTagTagProbFun tagOccsMap tagTagOccsMap = \(j,i) -> log (countTT (i,j) / countT i)
    where   countTT (i,j) = fromIntegral $ M.findWithDefault 0 (i,j) tagTagOccsMap
            countT x = fromIntegral $ M.findWithDefault 0 x tagOccsMap

-- creates a function, which, for (w,t), returns the probability of the word w being associated with tag t. Formally:
--             / log (count(w | t)/ count(t))   if w has been seen in the training data
-- p(w | t) = {
--             \ 0                              otherwise
genWordTagProbFun :: TagCountMap -> WordTagCountMap -> WordTagProb
genWordTagProbFun tagOccsMap wordTagOccsMap = \(w,t) -> if hasBeenSeen w then log (countWT (w,t) / countT t) else 0
    where   countWT (w,t) = fromIntegral $ M.findWithDefault 0 (w,t) wordTagOccsMap
            countT x = fromIntegral $ M.findWithDefault 0 x tagOccsMap
            wordsInMap = M.foldrWithKey (\k _ new -> M.insert (fst k) True new) (M.empty) wordTagOccsMap
            hasBeenSeen x = M.findWithDefault False x wordsInMap


initScore :: WordTagProb -> TagTagProb -> Core.Word -> [Tag] -> M.Map Tag Float
initScore pWT pTT w tags = M.fromList [(t, pWT (w, t) + pTT (t, "*start*")) | t <- tags]

initBackPtr :: [Tag] -> M.Map Tag [Tag]
initBackPtr tags = M.fromList [(t, []) | t <- tags]

viterbi' :: M.Map Tag Float -> M.Map Tag [Tag] -> WordTagProb -> TagTagProb -> [Core.Word] -> [Tag]
viterbi' score backPtr pWT pTT [] = reverse $ backPtr M.! maxK
    where maxK = fst $ M.foldrWithKey (\k v (t, s) -> if v >= s then (k, v) else (t, s)) ("", log(0/1)) score
viterbi' score backPtr pWT pTT (w:ords) = viterbi' score' backPtr' pWT pTT ords 
{-  `debug` ( w  ++ "\n\n" 
        ++ (fst . last . sortBy (comparing snd) . M.toList $ score') ++ "\n\n"
        -- ++ (maxK)
        ++ (intercalate "\n" $ map show $ M.toList score') ++ "\n\n" 
        ++ (intercalate "\n" $ map (\(i, j) -> i ++ "->" ++ show j) $ M.toList backPtr') ++
        "\n\n===============================\n\n")-}
    where 
        score' = M.mapWithKey (\i _ -> snd $ maxK i) score
        backPtr' = M.mapWithKey (\i _ -> let prev = fst $ maxK i in prev:(backPtr M.! prev)) backPtr
        maxK :: Tag -> (Tag, Float)
        maxK i = M.foldrWithKey (\k v (t, s) -> let s' = v + pTT (i, k) + pWT (w, i) in if s' >= s then (k, s') else (t, s)) ("", log(0/1)) score

viterbi :: [Tag] -> WordTagProb -> TagTagProb -> [Core.Word] -> [Tag]
viterbi _ _ _ [] = []
viterbi tags pWT pTT (w:ords) = viterbi' (initScore pWT pTT w tags) (initBackPtr tags) pWT pTT (ords ++ ["*end*"])

