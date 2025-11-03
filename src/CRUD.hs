module CRUD where
import Types
import Persistence
import Data.List (find, intercalate)
import System.IO (hFlush, stdout)




cadastrarPaciente :: [Paciente] -> IO [Paciente]
cadastrarPaciente pacientesList = do
  putStrLn "\n=== Cadastro de Paciente ==="
  putStr "Digite o nome completo: " >> hFlush stdout
  n <- getLine
  putStr "Digite o CPF (somente números): " >> hFlush stdout
  c <- getLine
  putStr "Digite a idade: " >> hFlush stdout
  i <- readLn
  putStr "Digite a senha: " >> hFlush stdout
  s <- getLine
  putStr "Deseja registrar convênio? (s/n): " >> hFlush stdout
  resp <- getLine
  conv <- if resp `elem` ["s","S"]
          then do
            putStr "Nome do plano: " >> hFlush stdout
            nomePlano <- getLine
            putStr "Número da carteira (11 dígitos): " >> hFlush stdout
            num <- getLine
            if length num /= 11
              then do
                putStrLn "Dado incorreto! Número da carteira inválido. Convênio será ignorado."
                return Nothing
              else return $ Just (Convenio nomePlano num)
          else return Nothing
  let pacienteNovo = Paciente n c i s conv []
      lista = pacientesList ++ [pacienteNovo]
  saveFile "data/pacientes.db" lista
  putStrLn "Paciente cadastrado com sucesso!"
  return lista

-- Login paciente
loginPacienteCRUD :: [Paciente] -> IO (Maybe Paciente)
loginPacienteCRUD pacientesList = do
  putStrLn "\n=== Login Paciente ==="
  putStr "CPF: " >> hFlush stdout
  c <- getLine
  putStr "Senha: " >> hFlush stdout
  s <- getLine
  case find (\p -> cpf p == c && senha p == s) pacientesList of
    Nothing -> putStrLn "Dado incorreto! CPF ou senha inválidos." >> return Nothing
    Just pacienteObj -> do
      putStrLn $ "Bem-vindo(a), " ++ nome pacienteObj ++ "!"
      return $ Just pacienteObj

-- Excluir paciente
excluirPaciente :: [Paciente] -> IO [Paciente]
excluirPaciente pacientesList = do
  putStr "Digite o CPF do paciente que deseja excluir: " >> hFlush stdout
  c <- getLine
  case find (\p -> cpf p == c) pacientesList of
    Nothing -> putStrLn "Dado incorreto! Paciente não encontrado." >> return pacientesList
    Just _ -> do
      let novaLista = filter (\p -> cpf p /= c) pacientesList
      saveFile "data/pacientes.db" novaLista
      putStrLn "Paciente excluído com sucesso!"
      return novaLista

-- ======================
-- MÉDICOS
-- ======================

-- Cadastrar médico
cadastrarMedico :: [Medico] -> IO [Medico]
cadastrarMedico medicosList = do
  putStrLn "\n=== Cadastro de Médico ==="
  putStr "Nome do médico: " >> hFlush stdout
  n <- getLine
  putStr "CRM: " >> hFlush stdout
  crmInput <- getLine
  putStrLn "Escolha a especialidade:"
  mapM_ (\(i,e) -> putStrLn $ show i ++ ") " ++ show e) (zip [1..] [ClinicoGeral .. Ortopedia])
  putStr "Número da especialidade: " >> hFlush stdout
  escolha <- readLn
  if escolha < 1 || escolha > length [ClinicoGeral .. Ortopedia]
    then putStrLn "Dado incorreto! Especialidade inválida." >> return medicosList
    else do
      let especialidadeSelecionada = [ClinicoGeral .. Ortopedia] !! (escolha - 1)
      putStr "Informe os horários disponíveis (hh:mm separados por vírgula, ex: 08:00,09:00): " >> hFlush stdout
      hrs <- getLine
      let horariosSeparados = words $ map (\c -> if c == ',' then ' ' else c) hrs
          medicoNovo = Medico n crmInput especialidadeSelecionada horariosSeparados
          lista = medicosList ++ [medicoNovo]
      saveFile "data/medicos.db" lista
      putStrLn "Médico cadastrado com sucesso!"
      return lista

-- Excluir médico (agora pelo nome)
excluirMedico :: [Medico] -> IO [Medico]
excluirMedico medicosList = do
  putStrLn "\n=== Lista de Médicos ==="
  mapM_ (\(i,m) -> putStrLn $ show i ++ ") " ++ nomeM m) (zip [1..] medicosList)
  putStr "Digite o nome do médico que deseja excluir: " >> hFlush stdout
  nomeInput <- getLine
  case find (\m -> nomeM m == nomeInput) medicosList of
    Nothing -> putStrLn "Dado incorreto! Médico não encontrado." >> return medicosList
    Just _ -> do
      let novaLista = filter (\m -> nomeM m /= nomeInput) medicosList
      saveFile "data/medicos.db" novaLista
      putStrLn "Médico excluído com sucesso!"
      return novaLista

-- ======================
-- ATENDIMENTOS
-- ======================

-- Agendar atendimento
agendarAtendimento :: [Paciente] -> [Medico] -> [Atendimento] -> IO [Atendimento]
agendarAtendimento pacientes medicos atendimentos = do
  putStr "Digite CPF do paciente: " >> hFlush stdout
  c <- getLine
  case find (\p -> cpf p == c) pacientes of
    Nothing -> putStrLn "Dado incorreto! Paciente não encontrado." >> return atendimentos
    Just pacienteObj -> do
      putStrLn "Médicos disponíveis:"
      mapM_ (\(i,m) -> putStrLn $ show i ++ ") " ++ nomeM m ++ " - " ++ show (especialidadeM m))
            (zip [1..] medicos)
      putStr "Escolha o número do médico: " >> hFlush stdout
      escolha <- readLn
      if escolha < 1 || escolha > length medicos
        then putStrLn "Dado incorreto! Médico inválido." >> return atendimentos
        else do
          let medicoEscolhido = medicos !! (escolha - 1)
              horariosDisponiveis = filter (`notElem` [horaAt a | a <- atendimentos, medico a == nomeM medicoEscolhido, status a == Agendada])
                                     (horarios medicoEscolhido)
          if null horariosDisponiveis
            then putStrLn "Dado incorreto! Médico sem horários disponíveis." >> return atendimentos
            else do
              putStrLn "Horários disponíveis:" >> mapM_ putStrLn horariosDisponiveis
              putStr "Escolha o horário (hh:mm): " >> hFlush stdout
              hora <- getLine
              putStr "Digite a data (dd/mm/aaaa): " >> hFlush stdout
              dataC <- getLine
              putStr "Digite sintomas (opcional): " >> hFlush stdout
              sint <- getLine
              let atendimentoNovo = Atendimento (cpf pacienteObj) (nomeM medicoEscolhido) (especialidadeM medicoEscolhido)
                                                dataC hora (if null sint then Nothing else Just sint) [] Agendada
                  lista = atendimentos ++ [atendimentoNovo]
              saveFile "data/atendimentos.db" lista
              putStrLn "Consulta agendada com sucesso!"
              return lista

-- Cancelar atendimento
cancelarAtendimento :: [Paciente] -> [Atendimento] -> IO [Atendimento]
cancelarAtendimento pacientes atendimentos = do
  putStr "Digite CPF do paciente: " >> hFlush stdout
  c <- getLine
  case find (\p -> cpf p == c) pacientes of
    Nothing -> putStrLn "Dado incorreto! Paciente não encontrado." >> return atendimentos
    Just pacienteObj -> do
      let consultasPaciente = filter (\a -> paciente a == cpf pacienteObj && status a == Agendada) atendimentos
      if null consultasPaciente
        then putStrLn "Dado incorreto! Nenhuma consulta agendada." >> return atendimentos
        else do
          mapM_ (\(i,a) -> putStrLn $ show i ++ ") " ++ dataAt a ++ " " ++ horaAt a ++ " com Dr. " ++ medico a)
                (zip [1..] consultasPaciente)
          putStr "Escolha o número da consulta a cancelar: " >> hFlush stdout
          escolha <- readLn
          if escolha < 1 || escolha > length consultasPaciente
            then putStrLn "Dado incorreto! Escolha inválida." >> return atendimentos
            else do
              let consulta = consultasPaciente !! (escolha - 1)
                  novaLista = map (\a -> if a == consulta then a {status = Cancelada} else a) atendimentos
              saveFile "data/atendimentos.db" novaLista
              putStrLn "Consulta cancelada com sucesso!"
              return novaLista
