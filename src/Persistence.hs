module Persistence where

import System.Directory (doesFileExist)
import Text.Read (readMaybe)

loadFile :: Read a => FilePath -> IO [a]
loadFile file = do
  exists <- doesFileExist file
  if not exists
    then return []
    else do
      content <- readFile file
      evaluate (length content) -- forçar leitura dos arquivos .db
      return $ maybe [] id (readMaybe content)

-- Salvar lista
saveFile :: Show a => FilePath -> [a] -> IO ()
saveFile file xs = writeFile file (show xs)
