module Persistence where

import System.Directory (doesFileExist, createDirectoryIfMissing)
import Text.Read (readMaybe)
import Control.Exception (evaluate)
import Types

-- garante que a pasta "data" exista
ensureDataDir :: IO ()
ensureDataDir = createDirectoryIfMissing True "data"

-- carrega uma lista de dados de um arquivo
loadFile :: Read a => FilePath -> IO [a]
loadFile file = do
  exists <- doesFileExist file
  if not exists
    then return []
    else do
      content <- readFile file
      _ <- evaluate (length content) -- força leitura completa do arquivo
      return $ maybe [] id (readMaybe content)

-- salva uma lista de dados em um arquivo
saveFile :: Show a => FilePath -> [a] -> IO ()
saveFile file xs = do
  ensureDataDir
  writeFile file (show xs)

-- caminhos dos arquivos usados pelo sistema
pacientesFile    = "data/pacientes.db"
medicosFile      = "data/medicos.db"
atendimentosFile = "data/atendimentos.db"
prescricoesFile  = "data/prescricoes.db"
agendasFile      = "data/agendas.db"

-- funções específicas para cada tipo de dado
carregarPacientes :: IO [Paciente]
carregarPacientes = loadFile pacientesFile

salvarPacientes :: [Paciente] -> IO ()
salvarPacientes = saveFile pacientesFile

carregarMedicos :: IO [Medico]
carregarMedicos = loadFile medicosFile

salvarMedicos :: [Medico] -> IO ()
salvarMedicos = saveFile medicosFile

carregarAtendimentos :: IO [Atendimento]
carregarAtendimentos = loadFile atendimentosFile

salvarAtendimentos :: [Atendimento] -> IO ()
salvarAtendimentos = saveFile atendimentosFile

carregarPrescricoes :: IO [Prescricao]
carregarPrescricoes = loadFile prescricoesFile

salvarPrescricoes :: [Prescricao] -> IO ()
salvarPrescricoes = saveFile prescricoesFile

carregarAgendas :: IO [AgendaMedico]
carregarAgendas = loadFile agendasFile

salvarAgendas :: [AgendaMedico] -> IO ()
salvarAgendas = saveFile agendasFile

-- salva todos os dados do sistema de uma vez
salvarTudo
  :: [Paciente]
  -> [Medico]
  -> [Atendimento]
  -> [Prescricao]
  -> [AgendaMedico]
  -> IO ()
salvarTudo pacientes medicos atendimentos prescricoes agendas = do
  salvarPacientes pacientes
  salvarMedicos medicos
  salvarAtendimentos atendimentos
  salvarPrescricoes prescricoes
  salvarAgendas agendas
  putStrLn " Dados salvos com sucesso."
