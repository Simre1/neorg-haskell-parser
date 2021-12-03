module Neorg where

import Neorg.Document
import Neorg.Document.Tag
import Neorg.Parser

-- 
-- 
-- test :: IO ()
-- test = do
--   file <- T.readFile "testing/test.norg"
--   let res = parse document "testing/test.norg" file
--   case res of
--     Right doc -> T.putStrLn $ renderDocument doc
--     Left err -> putStrLn $ errorBundlePretty err
-- 

-- parse :: FilePath -> IO (Either String (Document tags))
-- parse path = do
--   file <- T.readFile path
--   pure $ T.parseOnly documentParser file
-- 
-- 
-- toJson :: IO ()
-- toJson = do
--   doc <- either error id <$> parse "test.norg"
--   let json = documentToPandocJson doc
--   B.putStr json
-- 
-- 
-- 
-- write from to = do
--   doc <- either error id <$>  parse from
--   let json = documentToPandocJson doc
--   B.writeFile to json
-- 
