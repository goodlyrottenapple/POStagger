module Parser where
import Core

import System.IO
import Text.Regex
import Control.Monad (mapM, foldM, liftM2)

parseWord :: String -> Maybe POS
parseWord str = case splitRegex (mkRegex "\\/") $ subRegex (mkRegex "\\\\/") str "****" of 
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

-- given a list of paths to corpus files, reads each file, calls parseFile and merges the results. 
-- If a parse fails, Nothing is returned. Otherwise a list of parsed sentences is returned.
parseFiles :: [FilePath] -> IO (Maybe [Sentence])
parseFiles = foldM (\acc f -> parseFile f >>= \parsed -> return $ liftM2 (++) acc parsed) (Just [])