module Parser where
import System.IO
import System.Directory    
import Text.Regex.Posix
import Text.Regex
import Control.Monad
import Core


parseWord :: String -> Maybe POS
parseWord str = let (_,_,_,res) = ( str =~ "(.+)\\/(.+)" :: (String, String, String,[String]) ) in 
    case res of 
        [a, b]  -> Just $ (a, b)
        _       -> Nothing

parseSentence :: String -> Maybe Sentence
parseSentence = mapM parseWord . words where

parseSentences :: String -> Maybe [Sentence]
parseSentences = mapM parseSentence . filter (""/=) . splitRegex (mkRegex "=+") . (\x -> subRegex (mkRegex "\n|\\[|\\]") x "")

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

main = do
    files <- getAllDataFilePathsRec "./data/"
    print files
    parsed <- parseFiles files
    case parsed of
        Just r -> do
            print $ "Probs"
            print $ pAllTags x
            where x = map startEndTags r
        Nothing -> print "parse failed!!!!"
    -- do 
    --     r <- parseFiles ["WSJ_0200.POS", "WSJ_0201.POS"]
    --     print r