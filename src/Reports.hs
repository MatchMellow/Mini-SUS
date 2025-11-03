-- src/Reports.hs
module Reports where

import Types
import Persistence
import Data.List (find)

-- ==========================
-- Histórico de Paciente
-- ==========================

mostrarHistorico :: Paciente -> [Atendimento] -> IO ()
mostrarHistorico pacienteObj atendimentos = do
  let historicoPaciente = filter (\a -> paciente a == cpf pacienteObj) atendimentos
  if null historicoPaciente
    then putStrLn "Não há histórico de consultas."
    else do
      putStrLn $ "\n=== Histórico de " ++ nome pacienteObj ++ " ==="
      mapM_ printAtendimento historicoPaciente

printAtendimento :: Atendimento -> IO ()
printAtendimento a = do
  putStrLn $ "Data/Hora: " ++ dataAt a ++ " " ++ horaAt a
  putStrLn $ "Médico: " ++ medico a ++ " - " ++ show (especialidade a)
  putStrLn $ "Status: " ++ show (status a)
  case sintomas a of
    Just s  -> putStrLn $ "Sintomas: " ++ s
    Nothing -> return ()
  if null (prescricoes a)
    then return ()
    else do
      putStrLn "Prescrições:"
      mapM_ (\p -> putStrLn $ "- " ++ medicamento p ++ " (" ++ dosagem p ++ ")") (prescricoes a)
  putStrLn "-----------------------------------------------------------"

-- ==========================
-- Relatório de todos os médicos
-- ==========================

relatorioMedicos :: [Medico] -> [Atendimento] -> IO ()
relatorioMedicos medicos atendimentos = do
  putStrLn "\n=== Lista de Médicos ==="
  mapM_ (printMedico atendimentos) medicos

printMedico :: [Atendimento] -> Medico -> IO ()
printMedico atendimentos medicoObj = do
  putStrLn $ "Médico: " ++ nomeM medicoObj ++ " (" ++ show (especialidadeM medicoObj) ++ ")"
  let consultas = filter (\a -> medico a == nomeM medicoObj) atendimentos
  if null consultas
    then putStrLn "Nenhuma consulta registrada para esse médico"
    else mapM_ (\a -> putStrLn $ "- " ++ dataAt a ++ " " ++ horaAt a ++ " - Paciente: " ++ paciente a ++ " - Status: " ++ show (status a)) consultas
  putStrLn "---------------------------------------------------------------"

-- ==========================
-- Relatório de todas as consultas
-- ==========================

relatorioConsultas :: [Atendimento] -> IO ()
relatorioConsultas atendimentos = do
  putStrLn "\n=== Lista de Consultas ==="
  if null atendimentos
    then putStrLn "Nenhuma consulta registrada"
    else mapM_ printAtendimento atendimentos
