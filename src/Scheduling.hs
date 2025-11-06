module Scheduling where

import Types
import Persistence
import DoctorSchedule
import Data.List (find)
import System.IO (hFlush, stdout)

-- horários livres do médico, considerando agenda de 20min + atendimentos já marcados
horariosLivres :: Medico -> [Atendimento] -> [AgendaMedico] -> String -> [String]
horariosLivres medicoObj atendimentos agendas diaSemana =
  let ocupados = [horaAt a | a <- atendimentos, medico a == crm medicoObj, status a == Agendada]
      livresAgenda = [slHora s | s <- slotsLivresDoDia (crm medicoObj) diaSemana agendas]
  in filter (`notElem` ocupados) livresAgenda

-- agendar uma nova consulta (paciente)
agendarConsulta
  :: Paciente
  -> [Medico]
  -> [Atendimento]
  -> [AgendaMedico]
  -> IO ([Atendimento], [AgendaMedico])
agendarConsulta pacienteObj medicos atendimentos agendas = do
  putStrLn "\n=== Agendar Consulta ==="
  putStrLn "Médicos disponíveis:"
  mapM_ (\(i, m) -> putStrLn $ show i ++ ") " ++ nomeM m ++ " - " ++ show (especialidadeM m)) (zip [1..] medicos)
  putStr "Escolha o número do médico: " >> hFlush stdout
  escolha <- readLn
  if escolha < 1 || escolha > length medicos
    then putStrLn "[ERRO] Escolha inválida." >> return (atendimentos, agendas)
    else do
      let medicoEscolhido = medicos !! (escolha - 1)
      putStr "Dia da semana (ex.: Segunda): " >> hFlush stdout
      dia <- getLine
      let livres = horariosLivres medicoEscolhido atendimentos agendas dia
      if null livres
        then putStrLn "Nenhum horário disponível para este dia." >> return (atendimentos, agendas)
        else do
          putStrLn "\nHorários disponíveis:"
          mapM_ putStrLn livres
          putStr "Escolha um horário (HH:MM): " >> hFlush stdout
          hora <- getLine
          if hora `notElem` livres
            then putStrLn "[ERRO] Horário inválido." >> return (atendimentos, agendas)
            else do
              putStr "Data da consulta (AAAA-MM-DD): " >> hFlush stdout
              dataC <- getLine
              putStr "Sintomas (opcional): " >> hFlush stdout
              sint <- getLine
              let novoAtendimento = Atendimento
                    { paciente        = cpf pacienteObj
                    , medico          = crm medicoEscolhido
                    , especialidade   = especialidadeM medicoEscolhido
                    , dataAt          = dataC
                    , horaAt          = hora
                    , sintomas        = if null sint then Nothing else Just sint
                    , prescricoes     = []
                    , status          = Agendada
                    , observacao      = Nothing
                    , tipoAtendimento = Consulta
                    }
              let novosAtendimentos = atendimentos ++ [novoAtendimento]
              -- marca o slot como ocupado na agenda do médico
              let agendasAtualizadas = ocuparSlot (crm medicoEscolhido) dia hora agendas

              salvarAtendimentos novosAtendimentos
              salvarAgendas agendasAtualizadas
              putStrLn "[OK] Consulta agendada com sucesso!"
              return (novosAtendimentos, agendasAtualizadas)

cancelarConsulta
  :: Paciente
  -> [Atendimento]
  -> [AgendaMedico]
  -> IO ([Atendimento], [AgendaMedico])
cancelarConsulta pacienteObj atendimentos agendas = do
  putStrLn "\n=== Cancelar Consulta ==="
  let minhas = filter (\a -> paciente a == cpf pacienteObj && status a == Agendada) atendimentos
  if null minhas
    then putStrLn "Você não possui consultas agendadas." >> return (atendimentos, agendas)
    else do
      mapM_ (\(i,a) -> putStrLn $ show i ++ ") " ++ dataAt a ++ " " ++ horaAt a ++ " com médico CRM " ++ medico a)
            (zip [1..] minhas)
      putStr "Escolha o número da consulta para cancelar: " >> hFlush stdout
      escolha <- readLn
      if escolha < 1 || escolha > length minhas
        then putStrLn "[ERRO] Opção inválida." >> return (atendimentos, agendas)
        else do
          let consultaSel = minhas !! (escolha - 1)
          let atendimentosAtualizados =
                map (\a -> if a == consultaSel then a { status = Cancelada } else a) atendimentos

          let agendasAtualizadas =
                desocuparSlot (medico consultaSel) (horaAt consultaSel) agendas

          salvarAtendimentos atendimentosAtualizados
          salvarAgendas agendasAtualizadas
          putStrLn "[OK] Consulta cancelada e horário liberado!"
          return (atendimentosAtualizados, agendasAtualizadas)
