module Main where
import Types
import Persistence
import MenuAdmin   
import MenuPaciente 

main :: IO ()
main = do
  putStrLn "=== Mini-SUS: Sistema de Atendimento Simplificado ==="
  -- carregar os persistentes 
  pacientes    <- loadFile "data/pacientes.db"    :: IO [Paciente]
  medicos      <- loadFile "data/medicos.db"      :: IO [Medico]
  atendimentos <- loadFile "data/atendimentos.db" :: IO [Atendimento]
  menuInicial pacientes medicos atendimentos

menuInicial :: [Paciente] -> [Medico] -> [Atendimento] -> IO ()
menuInicial pacientes medicos atendimentos = do
  putStrLn "\n=== Login Inicial ==="
  putStrLn "1) Administrador"
  putStrLn "2) Paciente"
  putStrLn "q) Sair"
  putStr "Escolha: "
  opcao <- getLine
  case opcao of
    "1" -> loginAdmin pacientes medicos atendimentos        --- abrir a função do menu para administrador 
    "2" -> loginPacienteMenu pacientes medicos atendimentos -- abrir a função pra médicos
    "q" -> putStrLn "Encerrando o Mini-SUS..."
    _   -> putStrLn "Opção inválida!" >> menuInicial pacientes medicos atendimentos
