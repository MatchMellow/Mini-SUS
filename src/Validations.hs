module Validations
  ( 
    promptCPF
  , promptCOREN
  , promptNonEmpty
  , promptWith
  , retry3
  , parseDay
  , parseTimeHM
  , parseLocal
  , isUpcomingRelativeTo
  , sortAtendimentosByDateTime
  ) where

import Types
import System.IO (hFlush, stdout)
import Data.Char (isDigit, isSpace, toUpper)
import Data.Maybe (isJust, fromMaybe)
import Data.List (sortOn)
import Data.Time
import Data.Time.Format (defaultTimeLocale, parseTimeM)
putAsk :: String -> IO ()
putAsk s = putStr s >> hFlush stdout

promptWith :: String -> (String -> Maybe a) -> IO (Maybe a)
promptWith label parser = do
  putAsk label
  s <- getLine
  if map toUpper s == "Q"
    then return Nothing
    else return (parser s)

-- tenta 3 vezes e, se falhar, mostra mensagem padrão e retorna Nothing
retry3 :: IO (Maybe a) -> IO (Maybe a)
retry3 action = go 3
  where
    go 0 = return Nothing
    go n = do
      mx <- action
      case mx of
        Just x  -> return (Just x)
        Nothing -> if n == 1
                    then return Nothing
                    else do
                      putStrLn "[ERRO] Entrada inválida. Tente novamente (ou Q para voltar)."
                      go (n - 1)


-- CPF: exatamente 11 dígitos
validateCPF :: String -> Bool
validateCPF s = length s == 11 && all isDigit s

promptCPF :: String -> IO (Maybe String)
promptCPF label = retry3 $ promptWith label (\s -> if validateCPF s then Just s else Nothing)

-- COREN: “12345 SP” (5 dígitos, espaço, UF com 2 letras)
validateCOREN :: String -> Bool
validateCOREN s =
  case words s of
    [num, uf] -> length num == 5 && all isDigit num && length uf == 2 && all (\c -> c >= 'A' && c <= 'Z') (map toUpper uf)
    _         -> False

promptCOREN :: String -> IO (Maybe String)
promptCOREN label = retry3 $
  promptWith label (\s -> if validateCOREN s then Just (fmt s) else Nothing)
  where
    fmt x = let [num, uf] = words x in num ++ " " ++ map toUpper uf

promptNonEmpty :: String -> IO (Maybe String)
promptNonEmpty label = retry3 $ promptWith label (\s -> if any (not . isSpace) s then Just s else Nothing)

-- Espera data no formato AAAA-MM-DD
parseDay :: String -> Maybe Day
parseDay = parseTimeM True defaultTimeLocale "%F"

-- Espera hora no formato HH:MM
parseTimeHM :: String -> Maybe TimeOfDay
parseTimeHM = parseTimeM True defaultTimeLocale "%R"

-- Junta data e hora locais
parseLocal :: String -> String -> Maybe LocalTime
parseLocal ds hs = do
  d <- parseDay ds
  t <- parseTimeHM hs
  return $ LocalTime d t

-- Atendimento é futuro se:
--   data > hoje, ou
--   data == hoje e hora >= agora
isUpcomingRelativeTo :: LocalTime -> Atendimento -> Bool
isUpcomingRelativeTo now a =
  case parseLocal (dataAt a) (horaAt a) of
    Nothing   -> False
    Just when ->
      let (LocalTime d1 t1) = when
          (LocalTime d0 t0) = now
      in  d1 > d0 || (d1 == d0 && t1 >= t0)

-- Ordena por data e hora (inválidos vão pro fim)
sortAtendimentosByDateTime :: [Atendimento] -> [Atendimento]
sortAtendimentosByDateTime xs =
  let key a = (parseDay (dataAt a), parseTimeHM (horaAt a))
  in sortOn key xs
