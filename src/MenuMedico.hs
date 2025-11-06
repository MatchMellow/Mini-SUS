module MenuMedico where

import Types
import Reports
import DoctorSchedule
import Scheduling
import Persistence
import Data.List (filter)
import System.IO (hFlush, stdout)

-- login do médico
menuLoginMedico :: [Medico] -> [Atendimento] -> [AgendaMedico] -> [Prescricao] -> IO ()
menuLoginMedico medicos atendimentos agendas prescricoes = do
  putStrLn "\n=== Login do Médico ==="
  putStr "CRM: "
  hFlush stdout
  crmInput <- getLine
  case filter (\m -> crm m == crmInput) medicos of
    [] -> putStrLn "CRM não encontrado."
    (m:_) -> do
      putStrLn $ "\nBem-vindo(a), Dr(a). " ++ nomeM m ++ "!"
      menuMedico m medicos atendimentos agendas prescricoes

-- menu principal do médico
menuMedico :: Medico -> [Medico] -> [Atendimento] -> [AgendaMedico] -> [Prescricao] -> IO ()
menuMedico medicoObj medicos atendimentos agendas prescricoes = do
  putStrLn $ "\n=== Menu do Médico: " ++ nomeM medicoObj ++ " ==="
  putStrLn "1) Ver consultas da semana"
  putStrLn "2) Liberar horários (intervalos de 20min)"
  putStrLn "3) Ver prescrições dos meus pacientes"
  putStrLn "4) Marcar consulta como realizada"
  putStrLn "5) Visualizar minha agenda de horários"
  putStrLn "6) Criar prescrição para paciente"
  putStrLn "0) Sair"
  putStr "Escolha: "
  hFlush stdout
  op <- getLine
  case op of
    "1" -> do
      listarMinhasConsultas medicoObj atendimentos
      menuMedico medicoObj medicos atendimentos agendas prescricoes
    "2" -> do
      agendas' <- liberarHorariosFlow medicoObj agendas
      salvarAgendas agendas'
      menuMedico medicoObj medicos atendimentos agendas' prescricoes
    "3" -> do
      listarPrescricoesDoMedico (crm medicoObj) prescricoes
      menuMedico medicoObj medicos atendimentos agendas prescricoes
    "4" -> do
      atendimentos' <- marcarConsultaFeitaFlow medicoObj atendimentos
      salvarAtendimentos atendimentos'
      menuMedico medicoObj medicos atendimentos' agendas prescricoes
    "5" -> do
      listarAgendaMedico (crm medicoObj) agendas
      menuMedico medicoObj medicos atendimentos agendas prescricoes
    "6" -> do
      prescricoes' <- criarPrescricaoFlow medicoObj prescricoes
      salvarPrescricoes prescricoes'
      menuMedico medicoObj medicos atendimentos agendas prescricoes'
    "0" -> do
      putStrLn "Saindo e salvando alterações..."
      salvarTudo [] medicos atendimentos prescricoes agendas
      putStrLn "Até logo, doutor(a)!"
    _   -> putStrLn "Opção inválida." >> menuMedico medicoObj medicos atendimentos agendas prescricoes

-- listar as consultas agendadas do médico
listarMinhasConsultas :: Medico -> [Atendimento] -> IO ()
listarMinhasConsultas med atds = do
  putStrLn "\n=== Consultas da Semana ==="
  let minhas = filter (\a -> medico a == crm med && status a == Agendada) atds
  if null minhas
    then putStrLn "Nenhuma consulta agendada."
    else mapM_ printConsulta minhas
  where
    printConsulta a =
      putStrLn $
        dataAt a
          ++ " "
          ++ horaAt a
          ++ " - Paciente: "
          ++ paciente a
          ++ " | Tipo: "
          ++ show (tipoAtendimento a)

-- liberar horários com intervalos de 20min
liberarHorariosFlow :: Medico -> [AgendaMedico] -> IO [AgendaMedico]
liberarHorariosFlow med agendas = do
  putStrLn "\n=== Liberar Horários ==="
  putStr "Dia (ex.: Segunda, Terça...): "
  hFlush stdout
  dia <- getLine
  putStr "Horário inicial (ex.: 08:00): "
  hFlush stdout
  ini <- getLine
  putStr "Horário final (ex.: 16:00): "
  hFlush stdout
  fim <- getLine
  let agendas' = liberarDiaMedico (crm med) dia ini fim agendas
  putStrLn "\n[OK] Horários liberados com sucesso!"
  return agendas'

-- mostrar prescrições de um médico
listarPrescricoesDoMedico :: String -> [Prescricao] -> IO ()
listarPrescricoesDoMedico crmMed prescrs = do
  putStrLn "\n=== Prescrições dos Pacientes ==="
  let minhas = filter (\p -> prMedico p == crmMed) prescrs
  if null minhas
    then putStrLn "Nenhuma prescrição registrada."
    else mapM_ printPresc minhas
  where
    printPresc p =
      putStrLn $
        prData p
          ++ " - Paciente: "
          ++ prPaciente p
          ++ "\n→ "
          ++ prTexto p
          ++ "\n"

-- marcar consulta como realizada
marcarConsultaFeitaFlow :: Medico -> [Atendimento] -> IO [Atendimento]
marcarConsultaFeitaFlow med atds = do
  putStrLn "\n=== Marcar Consulta como Realizada ==="
  putStr "Data (AAAA-MM-DD): "
  hFlush stdout
  d <- getLine
  putStr "Hora (HH:MM): "
  hFlush stdout
  h <- getLine
  putStr "Observação (opcional): "
  hFlush stdout
  obs <- getLine
  let atds' = map (marcarSeFor d h obs) atds
  putStrLn "\n[OK] Consulta marcada como realizada!"
  return atds'
  where
    marcarSeFor d h obs a
      | medico a == crm med && dataAt a == d && horaAt a == h =
          a { status = Realizada
            , observacao = if null obs then observacao a else Just obs
            }
      | otherwise = a

-- criar nova prescrição
criarPrescricaoFlow :: Medico -> [Prescricao] -> IO [Prescricao]
criarPrescricaoFlow med prescrs = do
  putStrLn "\n=== Criar Nova Prescrição ==="
  putStr "CPF do paciente: "
  hFlush stdout
  cpfPac <- getLine
  putStr "Descrição da prescrição: "
  hFlush stdout
  texto <- getLine
  putStr "Data da prescrição (AAAA-MM-DD): "
  hFlush stdout
  dt <- getLine
  let nova = Prescricao
        { prPaciente = cpfPac
        , prMedico   = crm med
        , prTexto    = texto
        , prData     = dt
        }
  putStrLn "[OK] Prescrição registrada com sucesso."
  return (nova : prescrs)
