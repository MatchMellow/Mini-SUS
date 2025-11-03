module Main where

import Types
import Persistence
import MenuAdmin
import MenuPaciente
import MenuMedico

-- ponto de entrada do sistema
main :: IO ()
main = do
  putStrLn "=== Mini-SUS: Sistema de Atendimento Simplificado ==="
  putStrLn "Carregando dados..."
  pacientes    <- carregarPacientes
  medicos      <- carregarMedicos
  atendimentos <- carregarAtendimentos
  prescricoes  <- carregarPrescricoes
  agendas      <- carregarAgendas
  putStrLn "Dados carregados com sucesso."
  menuInicial pacientes medicos atendimentos prescricoes agendas

-- menu inicial de login e acesso
menuInicial
  :: [Paciente]
  -> [Medico]
  -> [Atendimento]
  -> [Prescricao]
  -> [AgendaMedico]
  -> IO ()
menuInicial pacientes medicos atendimentos prescricoes agendas = do
  putStrLn "\n=== Login Inicial ==="
  putStrLn "1) Administrador"
  putStrLn "2) Paciente"
  putStrLn "3) Médico"
  putStrLn "q) Sair"
  putStr "Escolha: "
  opcao <- getLine
  case opcao of
    "1" -> do
      loginAdmin pacientes medicos atendimentos prescricoes agendas
      menuInicial pacientes medicos atendimentos prescricoes agendas
    "2" -> do
      menuPaciente pacientes medicos atendimentos prescricoes agendas
      menuInicial pacientes medicos atendimentos prescricoes agendas
    "3" -> do
      menuLoginMedico medicos atendimentos agendas prescricoes
      menuInicial pacientes medicos atendimentos prescricoes agendas
    "q" -> do
      putStrLn "Salvando dados e encerrando o Mini-SUS..."
      salvarTudo pacientes medicos atendimentos prescricoes agendas
      putStrLn "✅ Encerrado com sucesso."
    _   -> do
      putStrLn "Opção inválida!"
      menuInicial pacientes medicos atendimentos prescricoes agendas
