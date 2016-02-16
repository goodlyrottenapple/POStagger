module Parser where  
import System.IO
import System.Directory    
import Text.Regex.Posix
import Text.Regex
import Core
import Control.Monad

import Debug.Trace

debug = flip trace

-- validTags = ["$", "''", "``", "(", ")", ",", ".", ":", "CC", "CD", "DT", "EX", "FW", "IN", "JJ", "JJR", "JJS", "LS", "MD", "NN", "NNP", "NNPS", "NNS", "PDT", "POS", "PRP", "PRP$", "RB", "RBR", "RBS", "RP", "SYM", "TO", "UH", "VB", "VBD", "VBG", "VBN", "VBP", "VBZ", "WDT", "WP", "WP$", "WRB"]

parseWord :: String -> Maybe POS
parseWord str = 
    case splitRegex (mkRegex "\\/") $ subRegex (mkRegex "\\\\/") str "****" of 
        [a,b] -> Just $ (subRegex (mkRegex "\\*\\*\\*\\*") a "/", b') where b' = if '|' `elem` b then splitRegex (mkRegex "\\|") b else [b]
        (a:b) -> Just $ (subRegex (mkRegex "\\*\\*\\*\\*") a "/", b) -- `debug` (a ++ show b)
        rest  -> Nothing -- `debug` (show rest)

parseSentence :: String -> Maybe Sentence
parseSentence = mapM parseWord . words

parseSentences :: String -> Maybe [Sentence]
parseSentences = mapM parseSentence . filter (not . empty) . splitRegex (mkRegex "=+") . (\x -> subRegex (mkRegex "\n|\\[|\\]") x " ")
    where empty = (\x -> subRegex (mkRegex "\n| ") x "" == "")

parseFile :: FilePath -> IO (Maybe [Sentence])
parseFile f = withFile f ReadMode (\h -> hGetContents h >>= \contents -> 
    return $! parseSentences contents)

-- given a list of paths to corpus files, reads each file, calls parseFile
-- and merges the results. If a parse fails, Nothing is returned. Otherwise a list of Sentence is returned.
parseFiles :: [FilePath] -> IO (Maybe [Sentence])
parseFiles = foldM (\acc f -> parseFile f >>= \parsed -> return $ liftM2 (++) acc parsed) (Just [])