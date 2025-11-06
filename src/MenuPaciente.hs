module MenuPaciente where

import Types
import qualified CRUD as CR
import Persistence
import Reports
import Validations
import Data.List (find)
import Data.Time
import System.IO (hFlush, stdout)
import Data.Char (toUpper)

menuPaciente
  :: [Paciente]
  -> [Medico]
  -> [Atendimento]
  -> [Prescricao]
  -> [AgendaMedico]
  -> IO ()
menuPaciente pacientes medicos atendimentos prescricoes agendas = do
  putStrLn "\n=== Menu do Paciente ==="
  putStrLn "1) Fazer login"
  putStrLn "2) Abrir chamado de cadastro (em vez de auto-cadastro)"
  putStrLn "Q) Voltar"
  putStr   "Escolha: " >> hFlush stdout
  op <- getLine
  case map toUpper op of
    "1" -> loginPaciente pacientes medicos atendimentos prescricoes agendas
    "2" -> abrirChamadoCadastro >> menuPaciente pacientes medicos atendimentos prescricoes agendas
    "Q" -> putStrLn "Voltando..."
    _   -> putStrLn "Opção inválida!" >>
            menuPaciente pacientes medicos atendimentos prescricoes agendas

-- abrir chamado simples para cadastro (em vez de auto-cadastro)
abrirChamadoCadastro :: IO ()
abrirChamadoCadastro = do
  putStrLn "\n=== Abrir Chamado de Cadastro ==="
  mNome <- promptNonEmpty "Seu nome completo (ou Q para voltar): "
  case mNome of
    Nothing   -> putStrLn "Voltando..."
    Just nome -> do
      mCPF <- promptCPF "Seu CPF (11 dígitos, ou Q para voltar): "
      case mCPF of
        Nothing   -> putStrLn "Voltando..."
        Just cpf' -> do
          appendFile "data/chamados-cadastro.log" (nome ++ " | CPF " ++ cpf' ++ "\n")
          putStrLn "[OK] Chamado registrado. A gestão entrará em contato."

-- ============================
-- Login (3 tentativas + Q)
-- ============================
loginPaciente
  :: [Paciente]
  -> [Medico]
  -> [Atendimento]
  -> [Prescricao]
  -> [AgendaMedico]
  -> IO ()
loginPaciente pacientes medicos atendimentos prescricoes agendas = do
  putStrLn "\n=== Login do Paciente ==="
  let tentativa :: Int -> IO ()
      tentativa 0 = putStrLn "Por favor, contate a gestão de acessos para regularizar seu login."
      tentativa n = do
        mCPF <- promptCPF "CPF (11 dígitos, ou Q para voltar): "
        case mCPF of
          Nothing -> putStrLn "Voltando..."
          Just c  -> do
            putStr "Senha: " >> hFlush stdout
            s <- getLine
            case find (\p -> cpf p == c && senha p == s) pacientes of
              Just pac -> menuPacienteLogado pac pacientes medicos atendimentos prescricoes agendas
              Nothing  -> do
                putStrLn "[ERRO] CPF ou senha inválidos."
                tentativa (n - 1)
  tentativa 3

-- ============================
-- Menu logado do Paciente
-- ============================
menuPacienteLogado
  :: Paciente
  -> [Paciente]
  -> [Medico]
  -> [Atendimento]
  -> [Prescricao]
  -> [AgendaMedico]
  -> IO ()
menuPacienteLogado pac pacientes medicos atendimentos prescricoes agendas = do
  putStrLn $ "\nBem-vindo(a), " ++ nome pac
  putStrLn "1) Agendar consulta"
  putStrLn "2) Cancelar consulta"
  putStrLn "3) Ver histórico"
  putStrLn "4) Ver prescrições"
  putStrLn "5) Ver consultas futuras"
  putStrLn "0) Sair"
  putStrLn "Q) Voltar"
  putStr   "Escolha: " >> hFlush stdout
  op <- getLine
  case map toUpper op of
    -- agendar (modo simples do CRUD)
    "1" -> do
      at' <- CR.agendarConsulta pac medicos atendimentos
      putStrLn "[OK] Consulta agendada!"
      menuPacienteLogado pac pacientes medicos at' prescricoes agendas

    -- cancelar
    "2" -> do
      at' <- CR.cancelarConsulta pac atendimentos
      putStrLn "[OK] Consulta cancelada!"
      menuPacienteLogado pac pacientes medicos at' prescricoes agendas

    -- histórico
    "3" -> mostrarHistorico pac atendimentos >> loop

    -- prescrições do paciente
    "4" -> listarPrescricoesDoPaciente (cpf pac) prescricoes >> loop

    -- futuras (considera data e HORA atuais)
    "5" -> do
      tz  <- getCurrentTimeZone
      now <- utcToLocalTime tz <$> getCurrentTime
      let futuras = filter (\a -> status a == Agendada && isUpcomingRelativeTo now a)
                           atendimentos
      if null futuras
        then putStrLn "Você não possui consultas futuras."
        else mapM_ (\a ->
              putStrLn $ "- " ++ dataAt a ++ " " ++ horaAt a
                       ++ " com médico CRM " ++ medico a
            ) futuras
      loop

    -- sair e salvar
    "0" -> do
      salvarPacientes pacientes
      salvarMedicos medicos
      salvarAtendimentos atendimentos
      salvarPrescricoes prescricoes
      salvarAgendas agendas
      putStrLn "Saindo... até logo!"

    -- voltar
    "Q" -> putStrLn "Voltando..."

    -- inválida
    _   -> putStrLn "Opção inválida!" >> loop
  where
    loop = menuPacienteLogado pac pacientes medicos atendimentos prescricoes agendas
