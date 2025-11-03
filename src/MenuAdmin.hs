module MenuAdmin where

import Types
import CRUD
import Scheduling
import Reports
import Persistence
import Data.List (find)

-- ======================================
-- Login do administrador
-- ======================================
loginAdmin :: [Paciente] -> [Medico] -> [Atendimento] -> IO ()
loginAdmin pacientes medicos atendimentos = do
  putStrLn "\n=== Login Administrador ==="
  putStr "Login: "
  user <- getLine
  putStr "Senha: "
  senha <- getLine
  if user == "admin" && senha == "admin1230"
    then do
      putStrLn "\n[OK] Bem-vindo(a) Administrador!"
      menuAdmin pacientes medicos atendimentos
    else do
      putStrLn "\n[ERRO] Usuário ou senha incorretos!"
      loginAdmin pacientes medicos atendimentos -- força tentar novamente

-- Menu principal do Administrador
menuAdmin :: [Paciente] -> [Medico] -> [Atendimento] -> IO ()
menuAdmin pacientes medicos atendimentos = do
  putStrLn "\n=== Menu Administrador ==="
  putStrLn "1) Cadastrar paciente"
  putStrLn "2) Excluir paciente"
  putStrLn "3) Cadastrar médico"
  putStrLn "4) Excluir médico"
  putStrLn "5) Agendar consulta"
  putStrLn "6) Cancelar consulta"
  putStrLn "7) Relatórios"
  putStrLn "8) Voltar ao menu inicial"
  putStr "Escolha: "
  opcao <- getLine
  case opcao of
    "1" -> cadastrarPaciente pacientes >>= \p -> menuAdmin p medicos atendimentos
    "2" -> excluirPaciente pacientes >>= \p -> menuAdmin p medicos atendimentos
    "3" -> cadastrarMedico medicos >>= \m -> menuAdmin pacientes m atendimentos
    "4" -> excluirMedico medicos >>= \m -> menuAdmin pacientes m atendimentos
    "5" -> agendarConsultaAdmin pacientes medicos atendimentos
    "6" -> cancelarConsultaAdmin pacientes medicos atendimentos
    "7" -> menuRelatorios pacientes medicos atendimentos >> menuAdmin pacientes medicos atendimentos
    "8" -> putStrLn "\nVoltando ao menu inicial..."
    _   -> putStrLn "[ERRO] Opção inválida!" >> menuAdmin pacientes medicos atendimentos

-- Funções de agendamento e cancelamento com prompt de CPF
agendarConsultaAdmin :: [Paciente] -> [Medico] -> [Atendimento] -> IO ()
agendarConsultaAdmin pacientes medicos atendimentos = do
  putStr "Digite CPF do paciente para agendar: "
  c <- getLine
  case find (\p -> cpf p == c) pacientes of
    Nothing -> putStrLn "[ERRO] Paciente não encontrado!" >> menuAdmin pacientes medicos atendimentos
    Just pacienteObj -> agendarConsulta pacienteObj medicos atendimentos >>= \_ -> menuAdmin pacientes medicos atendimentos

cancelarConsultaAdmin :: [Paciente] -> [Medico] -> [Atendimento] -> IO ()
cancelarConsultaAdmin pacientes medicos atendimentos = do
  putStr "Digite CPF do paciente para cancelar: "
  c <- getLine
  case find (\p -> cpf p == c) pacientes of
    Nothing -> putStrLn "[ERRO] Paciente não encontrado!" >> menuAdmin pacientes medicos atendimentos
    Just pacienteObj -> cancelarConsulta pacienteObj atendimentos >>= \_ -> menuAdmin pacientes medicos atendimentos

-- ======================================
-- Menu de relatórios
-- ======================================
menuRelatorios :: [Paciente] -> [Medico] -> [Atendimento] -> IO ()
menuRelatorios pacientes medicos atendimentos = do
  putStrLn "\n=== Menu Relatórios ==="
  putStrLn "1) Histórico de paciente"
  putStrLn "2) Lista de médicos"
  putStrLn "3) Lista de consultas"
  putStr "Escolha: "
  opcao <- getLine
  case opcao of
    "1" -> do
      putStr "Digite CPF do paciente: "
      c <- getLine
      case find (\p -> cpf p == c) pacientes of
        Nothing -> putStrLn "[ERRO] Paciente não encontrado!"
        Just pacienteObj -> mostrarHistorico pacienteObj atendimentos
    "2" -> relatorioMedicos medicos atendimentos
    "3" -> relatorioConsultas atendimentos
    _   -> putStrLn "[ERRO] Opção inválida!"
