module Scheduling where

import Types
import Persistence
import Data.List (find)



-- Retorna horários livres de um médico considerando atendimentos existentes
horariosLivres :: Medico -> [Atendimento] -> [String]
horariosLivres medicoObj atendimentos =
  let ocupados = [horaAt a | a <- atendimentos, medico a == nomeM medicoObj, status a == Agendada]
  in filter (`notElem` ocupados) (horarios medicoObj)

agendarConsulta :: Paciente -> [Medico] -> [Atendimento] -> IO [Atendimento]
agendarConsulta pacienteObj medicos atendimentos = do
  putStrLn "\n=== Agendar Consulta ==="
  putStrLn "Médicos disponíveis:"
  mapM_ (\(i,m) -> putStrLn $ show i ++ ") " ++ nomeM m ++ " - " ++ show (especialidadeM m)) (zip [1..] medicos)
  putStr "Escolha o número do médico: "
  escolha <- readLn
  let medicoEscolhido = medicos !! (escolha - 1)
  let livres = horariosLivres medicoEscolhido atendimentos
  if null livres
    then do
      putStrLn " Médico sem horários disponíveis."
      return atendimentos
    else do
      putStrLn "Horários disponíveis: "
      mapM_ putStrLn livres
      putStr "Escolha um horário (hh:mm): "
      hora <- getLine
      if hora `elem` livres
        then do
          putStr "Digite a data (dd/mm/aaaa): "
          dataC <- getLine
          putStr "Digite sintomas (opcional): "
          sint <- getLine
          let atendimentoNovo = Atendimento
                                (cpf pacienteObj)
                                (nomeM medicoEscolhido)
                                (especialidadeM medicoEscolhido)
                                dataC
                                hora
                                (if null sint then Nothing else Just sint)
                                []
                                Agendada
          let lista = atendimentos ++ [atendimentoNovo]
          saveFile "data/atendimentos.db" lista
          putStrLn "Consulta agendada!"
          return lista
        else do
          putStrLn "Horário inválido!"
          return atendimentos

cancelarConsulta :: Paciente -> [Atendimento] -> IO [Atendimento]
cancelarConsulta pacienteObj atendimentos = do
  putStrLn "\n=== Cancelar Consulta ==="
  let minhasConsultas = [a | a <- atendimentos, paciente a == cpf pacienteObj, status a == Agendada]
  if null minhasConsultas
    then do
      putStrLn "Não há consultas agendadas para cancelar."
      return atendimentos
    else do
      mapM_ (\(i,a) -> putStrLn $ show i ++ ") " ++ dataAt a ++ " " ++ horaAt a ++ " com Dr. " ++ medico a) (zip [1..] minhasConsultas)
      putStr "Escolha o número da consulta que deseja cancelar: "
      escolha <- readLn
      let consulta = minhasConsultas !! (escolha - 1)
      let atendimentosAtualizados = map (\a -> if a == consulta then a { status = Cancelada } else a) atendimentos
      saveFile "data/atendimentos.db" atendimentosAtualizados
      putStrLn " Consulta cancelada!"
      return atendimentosAtualizados
