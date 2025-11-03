-- src/MenuPaciente.hs
module MenuPaciente where

import Types
import CRUD
import Scheduling
import Persistence
import Reports
import Data.List (find)

-- Menu inicial do paciente
loginPacienteMenu :: [Paciente] -> [Medico] -> [Atendimento] -> IO ()
loginPacienteMenu pacientes medicos atendimentos = do
  putStrLn "\n=== Menu Paciente ==="
  putStrLn "1) Login"
  putStrLn "2) Cadastrar-se"
  putStr "Escolha: "
  opcao <- getLine
  case opcao of
    "1" -> loginPaciente pacientes medicos atendimentos
    "2" -> cadastrarPaciente pacientes >>= \p -> loginPacienteMenu p medicos atendimentos
    _   -> putStrLn "Opção inválida! Tente novamente." >> loginPacienteMenu pacientes medicos atendimentos

-- Login paciente
loginPaciente :: [Paciente] -> [Medico] -> [Atendimento] -> IO ()
loginPaciente pacientes medicos atendimentos = do
  putStrLn "\n=== Login do Paciente ==="
  putStr "Digite seu CPF: "
  c <- getLine
  putStr "Digite sua senha: "
  s <- getLine
  let maybePacienteObj = find (\p -> cpf p == c && senha p == s) pacientes
  case maybePacienteObj of
    Nothing -> do
      putStrLn "CPF ou senha incorretos! Tente novamente."
      loginPacienteMenu pacientes medicos atendimentos -- volta para o menu de login
    Just pacienteObj -> menuPaciente pacienteObj medicos atendimentos

-- Menu do paciente
menuPaciente :: Paciente -> [Medico] -> [Atendimento] -> IO ()
menuPaciente pacienteObj medicos atendimentos = do
  putStrLn $ "\nBem-vindo(a), " ++ nome pacienteObj
  putStrLn "1) Agendar consulta"
  putStrLn "2) Cancelar consulta"
  putStrLn "3) Ver histórico de consultas"
  putStrLn "4) Sair"
  putStr "Escolha: "
  opcao <- getLine
  case opcao of
    "1" -> agendarConsulta pacienteObj medicos atendimentos >>= \a -> menuPaciente pacienteObj medicos a
    "2" -> cancelarConsulta pacienteObj atendimentos >>= \a -> menuPaciente pacienteObj medicos a
    "3" -> mostrarHistorico pacienteObj atendimentos >> menuPaciente pacienteObj medicos atendimentos
    "4" -> putStrLn "Saindo... Volte sempre!" >> loginPacienteMenu (pacientesFromAtendimentos atendimentos) medicos atendimentos
    _   -> putStrLn "Opção inválida! Tente novamente." >> menuPaciente pacienteObj medicos atendimentos

-- Função auxiliar para atualizar a lista de pacientes após alterações
pacientesFromAtendimentos :: [Atendimento] -> [Paciente]
pacientesFromAtendimentos _ = [] -- 
