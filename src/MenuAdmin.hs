module MenuAdmin where

import Types
import qualified CRUD            as CR
import Reports
import Persistence
import DoctorSchedule
import Data.List (find)
import System.IO (hFlush, stdout)

-- ===============================
-- LOGIN DO ADMINISTRADOR
-- ===============================
loginAdmin
  :: [Paciente]
  -> [Medico]
  -> [Atendimento]
  -> [Prescricao]
  -> [AgendaMedico]
  -> IO ()
loginAdmin pacientes medicos atendimentos prescricoes agendas = do
  putStrLn "\n=== Login do Administrador ==="
  putStr "Login: " >> hFlush stdout
  user <- getLine
  putStr "Senha: " >> hFlush stdout
  senha <- getLine
  if user == "admin" && senha == "admin1230"
    then do
      putStrLn "\n[OK] Bem-vindo(a), Administrador!"
      menuAdmin pacientes medicos atendimentos prescricoes agendas
    else do
      putStrLn "\n[ERRO] Usuário ou senha incorretos!"
      loginAdmin pacientes medicos atendimentos prescricoes agendas

-- ===============================
-- MENU PRINCIPAL DO ADMIN
-- ===============================
menuAdmin
  :: [Paciente]
  -> [Medico]
  -> [Atendimento]
  -> [Prescricao]
  -> [AgendaMedico]
  -> IO ()
menuAdmin pacientes medicos atendimentos prescricoes agendas = do
  putStrLn "\n=== MENU ADMINISTRADOR ==="
  putStrLn "1) Cadastrar paciente"
  putStrLn "2) Excluir paciente"
  putStrLn "3) Cadastrar médico"
  putStrLn "4) Excluir médico"
  putStrLn "5) Agendar consulta (pelo CPF)"
  putStrLn "6) Cancelar consulta (pelo CPF)"
  putStrLn "7) Relatórios"
  putStrLn "8) Gerenciar agenda médica"
  putStrLn "9) Voltar"
  putStr   "Escolha: " >> hFlush stdout
  op <- getLine
  case op of
    "1" -> do
      pacientes' <- CR.cadastrarPaciente pacientes
      menuAdmin pacientes' medicos atendimentos prescricoes agendas

    "2" -> do
      pacientes' <- CR.excluirPaciente pacientes
      menuAdmin pacientes' medicos atendimentos prescricoes agendas

    "3" -> do
      medicos' <- CR.cadastrarMedico medicos
      menuAdmin pacientes medicos' atendimentos prescricoes agendas

    "4" -> do
      medicos' <- CR.excluirMedico medicos
      menuAdmin pacientes medicos' atendimentos prescricoes agendas

    "5" -> do
      atendimentos' <- agendarConsultaAdmin pacientes medicos atendimentos
      menuAdmin pacientes medicos atendimentos' prescricoes agendas

    "6" -> do
      atendimentos' <- cancelarConsultaAdmin pacientes atendimentos
      menuAdmin pacientes medicos atendimentos' prescricoes agendas

    "7" -> do
      menuRelatorios pacientes medicos atendimentos prescricoes agendas
      menuAdmin pacientes medicos atendimentos prescricoes agendas

    "8" -> do
      agendas' <- menuAgendaMedica medicos agendas
      menuAdmin pacientes medicos atendimentos prescricoes agendas'

    "9" -> putStrLn "Voltando ao menu inicial..."

    _   -> putStrLn "[ERRO] Opção inválida!" >>
            menuAdmin pacientes medicos atendimentos prescricoes agendas

-- ===============================
-- AGENDAR CONSULTA (ADMIN)
-- ===============================
agendarConsultaAdmin
  :: [Paciente]
  -> [Medico]
  -> [Atendimento]
  -> IO [Atendimento]
agendarConsultaAdmin pacientes medicos atendimentos = do
  putStr "CPF do paciente: " >> hFlush stdout
  cpfPac <- getLine
  case find (\p -> cpf p == cpfPac) pacientes of
    Nothing -> do
      putStrLn "[ERRO] Paciente não encontrado!"
      return atendimentos
    Just pac -> do
      atendimentos' <- CR.agendarConsulta pac medicos atendimentos
      putStrLn "[OK] Consulta agendada!"
      return atendimentos'

-- ===============================
-- CANCELAR CONSULTA (ADMIN)
-- ===============================
cancelarConsultaAdmin
  :: [Paciente]
  -> [Atendimento]
  -> IO [Atendimento]
cancelarConsultaAdmin pacientes atendimentos = do
  putStr "CPF do paciente: " >> hFlush stdout
  cpfPac <- getLine
  case find (\p -> cpf p == cpfPac) pacientes of
    Nothing -> do
      putStrLn "[ERRO] Paciente não encontrado!"
      return atendimentos
    Just pac -> do
      atendimentos' <- CR.cancelarConsulta pac atendimentos
      putStrLn "[OK] Consulta cancelada!"
      return atendimentos'

-- ===============================
-- RELATÓRIOS
-- ===============================
menuRelatorios
  :: [Paciente]
  -> [Medico]
  -> [Atendimento]
  -> [Prescricao]
  -> [AgendaMedico]
  -> IO ()
menuRelatorios pacientes medicos atendimentos prescricoes agendas = do
  putStrLn "\n=== RELATÓRIOS ==="
  putStrLn "1) Histórico de paciente"
  putStrLn "2) Lista de médicos"
  putStrLn "3) Lista de consultas"
  putStrLn "4) Todas as prescrições"
  putStrLn "5) Agenda de um médico"
  putStr   "Escolha: " >> hFlush stdout
  op <- getLine
  case op of
    "1" -> do
      putStr "CPF do paciente: " >> hFlush stdout
      c <- getLine
      case find (\p -> cpf p == c) pacientes of
        Nothing  -> putStrLn "[ERRO] Paciente não encontrado!"
        Just pac -> mostrarHistorico pac atendimentos
    "2" -> relatorioMedicos medicos atendimentos
    "3" -> relatorioConsultas atendimentos
    "4" -> listarTodasPrescricoes prescricoes
    "5" -> do
      putStr "CRM do médico: " >> hFlush stdout
      crmMed <- getLine
      listarAgendaMedico crmMed agendas
    _   -> putStrLn "[ERRO] Opção inválida!"

-- ===============================
-- GERENCIAR AGENDA (ADMIN LIBERA HORÁRIOS)
-- ===============================
menuAgendaMedica :: [Medico] -> [AgendaMedico] -> IO [AgendaMedico]
menuAgendaMedica medicos agendas = do
  putStrLn "\n=== Gerenciar Agenda Médica ==="
  putStrLn "Médicos cadastrados:"
  mapM_ (\m -> putStrLn $ "- " ++ crm m ++ " | " ++ nomeM m) medicos
  putStr "CRM do médico: " >> hFlush stdout
  crmMed <- getLine
  putStr "Dia (ex.: Segunda): " >> hFlush stdout
  dia <- getLine
  putStr "Hora inicial (ex.: 08:00): " >> hFlush stdout
  hIni <- getLine
  putStr "Hora final   (ex.: 16:00): " >> hFlush stdout
  hFim <- getLine
  novasAgendas <- liberarDiaMedicoESalvar crmMed dia hIni hFim agendas
  return novasAgendas
