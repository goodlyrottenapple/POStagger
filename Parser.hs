module Parser where
import System.IO
import System.Directory    
import Text.Regex.Posix
import Text.Regex
import Control.Monad
import Core
-- import Debug.Trace
import Data.List.Split (splitOn)
import Data.Char (toLower)
import qualified Data.Map as M
import Data.Ord (comparing)
import Data.List (sortBy, intercalate)

-- validTags = ["$", "''", "``", "(", ")", ",", ".", ":", "CC", "CD", "DT", "EX", "FW", "IN", "JJ", "JJR", "JJS", "LS", "MD", "NN", "NNP", "NNPS", "NNS", "PDT", "POS", "PRP", "PRP$", "RB", "RBR", "RBS", "RP", "SYM", "TO", "UH", "VB", "VBD", "VBG", "VBN", "VBP", "VBZ", "WDT", "WP", "WP$", "WRB"]

-- parseWord :: String -> Maybe POS
-- parseWord str = let (_,_,_,res) = ( str =~ "(.+)\\/(.+)" :: (String, String, String,[String]) ) in 
--     case res of 
--         [a, b]  -> Just $ (a, [b])
--         _       -> Nothing


parseWord :: String -> Maybe POS
parseWord str = 
    case splitRegex (mkRegex "\\/") $ subRegex (mkRegex "\\\\/") str "****" of 
        [a,b] -> Just $ (subRegex (mkRegex "\\*\\*\\*\\*") a "/", b') where b' = if '|' `elem` b then splitRegex (mkRegex "\\|") b else [b]
        (a:b) -> Just $ (subRegex (mkRegex "\\*\\*\\*\\*") a "/", b) `debug` (a ++ show b)
        rest  -> Nothing `debug` (show rest)

parseSentence :: String -> Maybe Sentence
parseSentence = mapM parseWord . words

parseSentences :: String -> Maybe [Sentence]
parseSentences = mapM parseSentence . filter (not . empty) . splitRegex (mkRegex "=+") . (\x -> subRegex (mkRegex "\n|\\[|\\]") x " ")
    where empty = (\x -> subRegex (mkRegex "\n| ") x "" == "")

parseFile :: FilePath -> IO (Maybe [Sentence])
parseFile f = withFile f ReadMode (\h -> hGetContents h >>= \contents -> 
    return $! parseSentences contents)

parseFiles :: [FilePath] -> IO (Maybe [Sentence])
parseFiles = foldM (\acc f -> 
    parseFile f >>= \parsed -> return $ parsed >>= 
        \res -> acc >>= 
            \accr -> return $ accr ++ res) (Just [])


getAllDataFilePaths :: FilePath -> IO [FilePath]
getAllDataFilePaths f = do
    allFiles <- getDirectoryContents f
    -- add the ".POS" files to the one returned by ret
    return $ map ((f ++ "/") ++ ) $ filter (\path -> path =~ ".+\\.POS" :: Bool) allFiles


getAllDataFilePathsRec :: FilePath -> IO [FilePath]
getAllDataFilePathsRec f = do
    allFiles <- getDirectoryContents f
    -- compute all the files in the current directory
    dirs <- filterM doesDirectoryExist $ map (f ++) $ filter (\path -> not (path =~ "\\.+" :: Bool)) allFiles
    -- recursively find all the files ending in ".POS"
    ret <- foldM (\acc f -> getAllDataFilePathsRec f >>= \res -> return $ acc ++ res) [] dirs 
    -- add the ".POS" files to the one returned by ret
    return $ (map ((f ++ "/") ++ ) $ filter (\path -> path =~ ".+\\.POS" :: Bool) allFiles) ++ ret


save :: String -> FilePath -> IO ()
save x f = writeFile f x


main = do
{-    files <- getAllDataFilePathsRec "./data/"
    -- files <- getAllDataFilePaths "."
    parsed <- parseFiles files
    case parsed of
        Just r -> do
            writeFile "tCount.txt" (tagCountMapToStr tMap)
            writeFile "ttCount.txt" (strStrCountMapToStr tTMap)
            writeFile "wtCount.txt" (strStrCountMapToStr wTMap)

            where 
                x = map startEndTags r
                wTMap = genWordTagOccsMap x
                tTMap = genTagTagOccsMap x
                tMap = genTagOccsMap x
                -- pTT = genTagTagProbFun tMap tTMap
                -- pWT = genWordTagProbFun tMap wTMap
                -- tags = Map.keys $ tMap
        Nothing -> print "parse failed!!!!"-}

    tMapStr <- readFile "tCount.txt"
    ttMapStr <- readFile "ttCount.txt"
    wtMapStr <- readFile "wtCount.txt"

    let
        wTMap = strToStrStrCountMap wtMapStr
        tTMap = strToStrStrCountMap ttMapStr
        tMap = strToTagCountMap tMapStr
        pTT = genTagTagProbFun tMap tTMap
        pWT = genWordTagProbFun tMap wTMap
        tags = M.keys $ tMap 
        i = initScore pWT pTT "he" tags in
        
        -- putStr $ (show $ initScore pWT pTT "he" tags) ++ "\n\n" ++ (show $ last $ sortBy (comparing snd) $ M.toList $ initScore pWT pTT "he" tags)

        -- putStr $ (show $ last $ sortBy (comparing snd) $ [(t, pWT ("he",t)) | t <- tags]) ++ "\n\n\n" ++ (show $ last $ sortBy (comparing snd) $ [(t, pWT ("was",t)) | t <- tags])  ++ "\n\n\n" ++ (show [(t, pTT ("VBD",t)) | t <- tags])
        -- putStr $ show $ pWT ("he", "NN")

        putStr $ show $ viterbi tags pWT pTT (splitOn " " "he was a sandwich .")

