module MenuPaciente where

import Types
import qualified CRUD            as CR
import Persistence
import Reports
import Data.List (find)

-- menu inicial do paciente (login/cadastro)
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
  putStrLn "2) Cadastrar-se"
  putStrLn "q) Voltar"
  putStr   "Escolha: "
  op <- getLine
  case op of
    "1" -> loginPaciente pacientes medicos atendimentos prescricoes agendas
    "2" -> do
      pacientes' <- CR.cadastrarPaciente pacientes
      menuPaciente pacientes' medicos atendimentos prescricoes agendas
    "q" -> putStrLn "Voltando..."
    _   -> putStrLn "Opção inválida!" >>
            menuPaciente pacientes medicos atendimentos prescricoes agendas

-- login
loginPaciente
  :: [Paciente]
  -> [Medico]
  -> [Atendimento]
  -> [Prescricao]
  -> [AgendaMedico]
  -> IO ()
loginPaciente pacientes medicos atendimentos prescricoes agendas = do
  putStrLn "\n=== Login do Paciente ==="
  putStr "CPF: "
  cpfInput <- getLine
  putStr "Senha: "
  senhaInput <- getLine
  case find (\p -> cpf p == cpfInput && senha p == senhaInput) pacientes of
    Nothing -> do
      putStrLn "[ERRO] CPF ou senha inválidos."
      menuPaciente pacientes medicos atendimentos prescricoes agendas
    Just pac ->
      menuPacienteLogado pac pacientes medicos atendimentos prescricoes agendas

-- depois que loga
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
  putStr   "Escolha: "
  op <- getLine
  case op of
    -- agenda usando o CRUD (modo simples)
    "1" -> do
      atendimentos' <- CR.agendarConsulta pac medicos atendimentos
      putStrLn "[OK] Consulta agendada!"
      menuPacienteLogado pac pacientes medicos atendimentos' prescricoes agendas

    -- cancela usando o CRUD
    "2" -> do
      atendimentos' <- CR.cancelarConsulta pac atendimentos
      putStrLn "[OK] Consulta cancelada!"
      menuPacienteLogado pac pacientes medicos atendimentos' prescricoes agendas

    -- histórico
    "3" -> do
      mostrarHistorico pac atendimentos
      menuPacienteLogado pac pacientes medicos atendimentos prescricoes agendas

    -- prescrições do paciente
    "4" -> do
      listarPrescricoesDoPaciente (cpf pac) prescricoes
      menuPacienteLogado pac pacientes medicos atendimentos prescricoes agendas

    -- consultas futuras
    "5" -> do
      putStrLn "\n=== Consultas Futuras ==="
      let futuras =
            filter (\a -> paciente a == cpf pac && status a == Agendada) atendimentos
      if null futuras
        then putStrLn "Você não possui consultas futuras."
        else mapM_ (\a ->
              putStrLn $
                "- " ++ dataAt a ++ " " ++ horaAt a
                ++ " com médico CRM " ++ medico a
            ) futuras
      menuPacienteLogado pac pacientes medicos atendimentos prescricoes agendas

    -- sair e salvar
    "0" -> do
      salvarPacientes pacientes
      salvarMedicos medicos
      salvarAtendimentos atendimentos
      salvarPrescricoes prescricoes
      salvarAgendas agendas
      putStrLn "Saindo... até logo!"

    _   -> putStrLn "Opção inválida!" >>
            menuPacienteLogado pac pacientes medicos atendimentos prescricoes agendas
