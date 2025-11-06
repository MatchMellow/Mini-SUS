module Reports where
import Types

import Data.List (find)

-- histórico de um paciente
mostrarHistorico :: Paciente -> [Atendimento] -> IO ()
mostrarHistorico pacienteObj atendimentos = do
  let historicoPaciente = filter (\a -> paciente a == cpf pacienteObj) atendimentos
  if null historicoPaciente
    then putStrLn "Não há histórico de consultas."
    else do
      putStrLn $ "\n=== Histórico de " ++ nome pacienteObj ++ " ==="
      mapM_ printAtendimento historicoPaciente

-- imprime um atendimento
printAtendimento :: Atendimento -> IO ()
printAtendimento a = do
  putStrLn $ "Data/Hora: " ++ dataAt a ++ " " ++ horaAt a
  putStrLn $ "Médico (CRM): " ++ medico a ++ " - " ++ show (especialidade a)
  putStrLn $ "Tipo: " ++ show (tipoAtendimento a)
  putStrLn $ "Status: " ++ show (status a)
  case sintomas a of
    Just s  -> putStrLn $ "Sintomas: " ++ s
    Nothing -> return ()
  case observacao a of
    Just o  -> putStrLn $ "Observações: " ++ o
    Nothing -> return ()
  putStrLn "-----------------------------------------------------------"

-- relatório de médicos
relatorioMedicos :: [Medico] -> [Atendimento] -> IO ()
relatorioMedicos medicos atendimentos = do
  putStrLn "\n=== Lista de Médicos ==="
  mapM_ (printMedico atendimentos) medicos

printMedico :: [Atendimento] -> Medico -> IO ()
printMedico atendimentos medicoObj = do
  putStrLn $ "\nMédico: " ++ nomeM medicoObj ++ " (" ++ show (especialidadeM medicoObj) ++ ")"
  let consultas = filter (\a -> medico a == crm medicoObj) atendimentos
  if null consultas
    then putStrLn "Nenhuma consulta registrada para esse médico."
    else mapM_ (\a ->
      putStrLn $ "- " ++ dataAt a ++ " " ++ horaAt a
              ++ " | Paciente CPF: " ++ paciente a
              ++ " | Status: " ++ show (status a)
      ) consultas
  putStrLn "---------------------------------------------------------------"

-- relatório geral de atendimentos
relatorioConsultas :: [Atendimento] -> IO ()
relatorioConsultas atendimentos = do
  putStrLn "\n=== Lista de Consultas e Exames ==="
  if null atendimentos
    then putStrLn "Nenhum atendimento registrado."
    else mapM_ printAtendimento atendimentos

-- lista TODAS as prescrições
listarTodasPrescricoes :: [Prescricao] -> IO ()
listarTodasPrescricoes [] = putStrLn "Nenhuma prescrição registrada."
listarTodasPrescricoes ps = do
  putStrLn "\n=== Prescrições Médicas ==="
  mapM_ printPresc ps
  where
    printPresc p = do
      putStrLn $ prData p
               ++ " | Paciente: " ++ prPaciente p
               ++ " | Médico: "   ++ prMedico p
      putStrLn $ "→ " ++ prTexto p
      putStrLn "-----------------------------------------------------------"

-- prescrições de um paciente específico
listarPrescricoesDoPaciente :: String -> [Prescricao] -> IO ()
listarPrescricoesDoPaciente cpfPaciente prescrs = do
  let minhas = filter (\p -> prPaciente p == cpfPaciente) prescrs
  if null minhas
    then putStrLn "Nenhuma prescrição para este paciente."
    else do
      putStrLn "\n=== Minhas Prescrições ==="
      mapM_ (\p -> putStrLn (prData p ++ " - " ++ prTexto p ++ " (Médico: " ++ prMedico p ++ ")")) minhas

-- agenda de um médico
listarAgendaMedico :: String -> [AgendaMedico] -> IO ()
listarAgendaMedico crmMed agendas =
  case find (\a -> agMedicoCRM a == crmMed) agendas of
    Nothing -> putStrLn "Nenhuma agenda encontrada para este médico."
    Just ag -> do
      putStrLn $ "\n=== Agenda do Médico (CRM: " ++ crmMed ++ ") ==="
      mapM_ printSlot (agSlots ag)
  where
    printSlot s =
      putStrLn $ slDia s ++ " " ++ slHora s ++ " -> " ++ if slLivre s then "Livre" else "Ocupado"
