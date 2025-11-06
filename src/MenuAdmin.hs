module MenuAdmin where

import Types
import qualified CRUD           as CR
import qualified Reports        as R
import DoctorSchedule
import Validations (promptCPF, sortAtendimentosByDateTime)

import Data.List  (find)
import Data.Char  (toUpper)
import System.IO  (hFlush, stdout)

-- ===============================
-- LOGIN DO ADMINISTRADOR (3 tentativas + Q)
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
  let tentativa :: Int -> IO ()
      tentativa 0 = putStrLn "Por favor, entre em contato com a gestão de acessos para regularizar seu login."
      tentativa n = do
        putStr "Login (ou Q para voltar): " >> hFlush stdout
        user <- getLine
        if map toUpper user == "Q"
          then putStrLn "Voltando..."
          else do
            putStr "Senha: " >> hFlush stdout
            senha <- getLine
            if user == "admin" && senha == "admin1230"
              then menuAdmin pacientes medicos atendimentos prescricoes agendas
              else do
                putStrLn "[ERRO] Usuário ou senha incorretos."
                tentativa (n - 1)
  tentativa 3


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
  putStrLn "7) Relatórios (ordenados por data/hora)"
  putStrLn "8) Gerenciar agenda médica (slots de 20min)"
  putStrLn "9) Voltar"
  putStrLn "Q) Voltar"
  putStr   "Escolha: " >> hFlush stdout
  op <- getLine
  case map toUpper op of
    "1" -> CR.cadastrarPaciente pacientes >>= \p' -> menuAdmin p' medicos atendimentos prescricoes agendas
    "2" -> CR.excluirPaciente pacientes    >>= \p' -> menuAdmin p' medicos atendimentos prescricoes agendas
    "3" -> CR.cadastrarMedico medicos      >>= \m' -> menuAdmin pacientes m' atendimentos prescricoes agendas
    "4" -> CR.excluirMedico medicos        >>= \m' -> menuAdmin pacientes m' atendimentos prescricoes agendas
    "5" -> agendarConsultaAdmin pacientes medicos atendimentos >>= \a' -> menuAdmin pacientes medicos a' prescricoes agendas
    "6" -> cancelarConsultaAdmin pacientes atendimentos        >>= \a' -> menuAdmin pacientes medicos a' prescricoes agendas
    "7" -> menuRelatorios pacientes medicos atendimentos prescricoes agendas >> menuAdmin pacientes medicos atendimentos prescricoes agendas
    "8" -> menuAgendaMedica medicos agendas >>= \ag' -> menuAdmin pacientes medicos atendimentos prescricoes ag'
    "9" -> putStrLn "Voltando..."
    "Q" -> putStrLn "Voltando..."
    _   -> putStrLn "[ERRO] Opção inválida!" >> menuAdmin pacientes medicos atendimentos prescricoes agendas


agendarConsultaAdmin
  :: [Paciente]
  -> [Medico]
  -> [Atendimento]
  -> IO [Atendimento]
agendarConsultaAdmin pacientes medicos atendimentos = do
  mCPF <- promptCPF "CPF do paciente (11 dígitos, ou Q para voltar): "
  case mCPF of
    Nothing   -> putStrLn "Voltando..." >> return atendimentos
    Just cpf' ->
      case find (\p -> cpf p == cpf') pacientes of
        Nothing  -> putStrLn "[ERRO] Paciente não encontrado!" >> return atendimentos
        Just pac -> do
          at' <- CR.agendarConsulta pac medicos atendimentos
          putStrLn "[OK] Consulta agendada!"
          return at'


cancelarConsultaAdmin
  :: [Paciente]
  -> [Atendimento]
  -> IO [Atendimento]
cancelarConsultaAdmin pacientes atendimentos = do
  mCPF <- promptCPF "CPF do paciente para cancelar (11 dígitos, ou Q para voltar): "
  case mCPF of
    Nothing   -> putStrLn "Voltando..." >> return atendimentos
    Just cpf' ->
      case find (\p -> cpf p == cpf') pacientes of
        Nothing  -> putStrLn "[ERRO] Paciente não encontrado!" >> return atendimentos
        Just pac -> do
          at' <- CR.cancelarConsulta pac atendimentos
          putStrLn "[OK] Consulta cancelada!"
          return at'

-- ===============================
-- RELATÓRIOS (com ordenação por data/hora)
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
  putStrLn "3) Lista de consultas (ordenadas por data/hora)"
  putStrLn "4) Todas as prescrições"
  putStrLn "5) Agenda de um médico"
  putStrLn "Q) Voltar"
  putStr   "Escolha: " >> hFlush stdout
  op <- getLine
  case map toUpper op of
    "1" -> do
      mCPF <- promptCPF "CPF do paciente (11 dígitos, ou Q para voltar): "
      case mCPF of
        Nothing   -> putStrLn "Voltando..."
        Just cpf' ->
          case find (\p -> cpf p == cpf') pacientes of
            Nothing  -> putStrLn "[ERRO] Paciente não encontrado!"
            Just pac -> R.mostrarHistorico pac atendimentos
    "2" -> R.relatorioMedicos medicos atendimentos
    "3" -> relatorioConsultasOrdenadas atendimentos
    "4" -> R.listarTodasPrescricoes prescricoes
    "5" -> do
      putStr "CRM/COREN do médico: " >> hFlush stdout
      crmMed <- getLine
      R.listarAgendaMedico crmMed agendas
    "Q" -> putStrLn "Voltando..."
    _   -> putStrLn "[ERRO] Opção inválida!"

-- Lista de consultas ordenadas (local, usa Validations.sortAtendimentosByDateTime)
relatorioConsultasOrdenadas :: [Atendimento] -> IO ()
relatorioConsultasOrdenadas atendimentos = do
  putStrLn "\n=== Lista de Consultas (ordenadas por data/hora) ==="
  let ord = sortAtendimentosByDateTime atendimentos
  if null ord
    then putStrLn "Nenhum atendimento registrado."
    else mapM_ printLinha ord
  where
    printLinha a =
      putStrLn $ dataAt a ++ " " ++ horaAt a
             ++ " | Paciente: " ++ paciente a
             ++ " | CRM: " ++ medico a
             ++ " | " ++ show (status a)

-- ===============================
-- GERENCIAR AGENDA (ADMIN LIBERA HORÁRIOS)
-- ===============================
menuAgendaMedica :: [Medico] -> [AgendaMedico] -> IO [AgendaMedico]
menuAgendaMedica medicos agendas = do
  putStrLn "\n=== Gerenciar Agenda Médica ==="
  putStrLn "Médicos cadastrados:"
  mapM_ (\m -> putStrLn $ "- " ++ crm m ++ " | " ++ nomeM m) medicos
  putStrLn "Q) Voltar"
  putStr "CRM do médico (ou Q): " >> hFlush stdout
  c <- getLine
  if map toUpper c == "Q"
    then putStrLn "Voltando..." >> return agendas
    else do
      putStr "Dia (ex.: Segunda): " >> hFlush stdout
      dia <- getLine
      putStr "Hora inicial (ex.: 08:00): " >> hFlush stdout
      hIni <- getLine
      putStr "Hora final   (ex.: 16:00): " >> hFlush stdout
      hFim <- getLine
      novas <- liberarDiaMedicoESalvar c dia hIni hFim agendas
      putStrLn "[OK] Agenda atualizada!"
      return novas
