module MenuAdmin where

import Types
import CRUD
import Scheduling
import Reports
import Persistence
import Data.List (find)

-- login do administrador
loginAdmin
  :: [Paciente]
  -> [Medico]
  -> [Atendimento]
  -> [Prescricao]
  -> [AgendaMedico]
  -> IO ()
loginAdmin pacientes medicos atendimentos prescricoes agendas = do
  putStrLn "\n=== Login Administrador ==="
  putStr "Login: "
  user <- getLine
  putStr "Senha: "
  senha <- getLine
  if user == "admin" && senha == "admin1230"
    then do
      putStrLn "\n[OK] Bem-vindo(a), Administrador!"
      menuAdmin pacientes medicos atendimentos prescricoes agendas
    else do
      putStrLn "\n[ERRO] Usuário ou senha incorretos!"
      loginAdmin pacientes medicos atendimentos prescricoes agendas

-- menu principal do administrador
menuAdmin
  :: [Paciente]
  -> [Medico]
  -> [Atendimento]
  -> [Prescricao]
  -> [AgendaMedico]
  -> IO ()
menuAdmin pacientes medicos atendimentos prescricoes agendas = do
  putStrLn "\n=== Menu Administrador ==="
  putStrLn "1) Cadastrar paciente"
  putStrLn "2) Excluir paciente"
  putStrLn "3) Cadastrar médico"
  putStrLn "4) Excluir médico"
  putStrLn "5) Agendar consulta"
  putStrLn "6) Cancelar consulta"
  putStrLn "7) Relatórios"
  putStrLn "8) Gerenciar agenda médica"
  putStrLn "9) Voltar ao menu inicial"
  putStr "Escolha: "
  opcao <- getLine
  case opcao of
    "1" -> do
      novosPacientes <- cadastrarPaciente pacientes
      menuAdmin novosPacientes medicos atendimentos prescricoes agendas
    "2" -> do
      novosPacientes <- excluirPaciente pacientes
      menuAdmin novosPacientes medicos atendimentos prescricoes agendas
    "3" -> do
      novosMedicos <- cadastrarMedico medicos
      menuAdmin pacientes novosMedicos atendimentos prescricoes agendas
    "4" -> do
      novosMedicos <- excluirMedico medicos
      menuAdmin pacientes novosMedicos atendimentos prescricoes agendas
    "5" -> agendarConsultaAdmin pacientes medicos atendimentos prescricoes agendas
    "6" -> cancelarConsultaAdmin pacientes medicos atendimentos prescricoes agendas
    "7" -> menuRelatorios pacientes medicos atendimentos prescricoes agendas
    "8" -> menuAgendaMedica medicos agendas >> menuAdmin pacientes medicos atendimentos prescricoes agendas
    "9" -> putStrLn "\nVoltando ao menu inicial..."
    _   -> putStrLn "[ERRO] Opção inválida!" >> menuAdmin pacientes medicos atendimentos prescricoes agendas

-- agendar uma consulta pelo admin
agendarConsultaAdmin
  :: [Paciente]
  -> [Medico]
  -> [Atendimento]
  -> [Prescricao]
  -> [AgendaMedico]
  -> IO ()
agendarConsultaAdmin pacientes medicos atendimentos prescricoes agendas = do
  putStr "Digite o CPF do paciente: "
  c <- getLine
  case find (\p -> cpf p == c) pacientes of
    Nothing -> putStrLn "[ERRO] Paciente não encontrado!"
    Just pacienteObj -> do
      agendarConsulta pacienteObj medicos atendimentos
      putStrLn "[OK] Consulta agendada."
  menuAdmin pacientes medicos atendimentos prescricoes agendas

-- cancelar consulta pelo admin
cancelarConsultaAdmin
  :: [Paciente]
  -> [Medico]
  -> [Atendimento]
  -> [Prescricao]
  -> [AgendaMedico]
  -> IO ()
cancelarConsultaAdmin pacientes medicos atendimentos prescricoes agendas = do
  putStr "Digite o CPF do paciente para cancelar: "
  c <- getLine
  case find (\p -> cpf p == c) pacientes of
    Nothing -> putStrLn "[ERRO] Paciente não encontrado!"
    Just pacienteObj -> do
      cancelarConsulta pacienteObj atendimentos
      putStrLn "[OK] Consulta cancelada."
  menuAdmin pacientes medicos atendimentos prescricoes agendas

-- menu de relatórios do administrador
menuRelatorios
  :: [Paciente]
  -> [Medico]
  -> [Atendimento]
  -> [Prescricao]
  -> [AgendaMedico]
  -> IO ()
menuRelatorios pacientes medicos atendimentos prescricoes agendas = do
  putStrLn "\n=== Menu de Relatórios ==="
  putStrLn "1) Histórico de paciente"
  putStrLn "2) Lista de médicos"
  putStrLn "3) Lista de consultas"
  putStrLn "4) Todas as prescrições"
  putStrLn "5) Agenda de um médico"
  putStr "Escolha: "
  opcao <- getLine
  case opcao of
    "1" -> do
      putStr "Digite o CPF do paciente: "
      c <- getLine
      case find (\p -> cpf p == c) pacientes of
        Nothing -> putStrLn "[ERRO] Paciente não encontrado!"
        Just pacienteObj -> mostrarHistorico pacienteObj atendimentos
    "2" -> relatorioMedicos medicos atendimentos
    "3" -> relatorioConsultas atendimentos
    "4" -> listarTodasPrescricoes prescricoes
    "5" -> do
      putStr "Digite o CRM do médico: "
      crmMed <- getLine
      listarAgendaMedico crmMed agendas
    _   -> putStrLn "[ERRO] Opção inválida!"

-- menu para o administrador liberar agendas médicas
menuAgendaMedica :: [Medico] -> [AgendaMedico] -> IO ()
menuAgendaMedica medicos agendas = do
  putStrLn "\n=== Gerenciamento de Agendas Médicas ==="
  putStr "Digite o CRM do médico: "
  crmMed <- getLine
  putStr "Dia da semana (ex.: Segunda): "
  dia <- getLine
  putStr "Horário inicial (ex.: 08:00): "
  hIni <- getLine
  putStr "Horário final (ex.: 16:00): "
  hFim <- getLine
  let novasAgendas = liberarDiaMedico crmMed dia hIni hFim agendas
  salvarAgendas novasAgendas
  putStrLn "[OK] Agenda atualizada com sucesso."
