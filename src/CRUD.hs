module CRUD where

import Types
import Persistence
import Data.List (find)
import System.IO (hFlush, stdout)

todasEspecialidades :: [Especialidade]
todasEspecialidades =
  [ AlergiaEImunologia
  , Cardiologia
  , CirurgiaGeral
  , ClinicaMedica
  , Dermatologia
  , Endocrinologia
  , Fisioterapia
  , Gastroenterologia
  , Genetica
  , GinecologiaEObstetricia
  , Hematologia
  , Infectologia
  , Neurologia
  , Nutricao
  , Oftalmologia
  , OrtopediaETraumatologia
  , Otorrinolaringologia
  , Pediatria
  , Psicologia
  , Psiquiatria
  , Reumatologia
  ]


cadastrarPaciente :: [Paciente] -> IO [Paciente]
cadastrarPaciente pacientes = do
  putStrLn "\n=== Cadastro de Paciente ==="
  putStr "Nome completo: " >> hFlush stdout
  nomeP <- getLine
  putStr "CPF: " >> hFlush stdout
  cpfP <- getLine
  putStr "Idade: " >> hFlush stdout
  idadeP <- readLn
  putStr "Senha: " >> hFlush stdout
  senhaP <- getLine
  putStr "Possui convênio? (s/n): " >> hFlush stdout
  resp <- getLine
  conv <-
    if resp `elem` ["s","S"]
      then do
        putStr "Nome do plano: " >> hFlush stdout
        plano <- getLine
        putStr "Número da carteirinha (11 dígitos): " >> hFlush stdout
        num <- getLine
        if length num /= 11
          then do
            putStrLn "[AVISO] Número inválido. Convênio não será adicionado."
            return Nothing
          else return (Just (Convenio plano num))
      else return Nothing
  let novo = Paciente nomeP cpfP idadeP senhaP conv []
      lista = pacientes ++ [novo]
  salvarPacientes lista
  putStrLn "[OK] Paciente cadastrado com sucesso!"
  return lista

  
excluirPaciente :: [Paciente] -> IO [Paciente]
excluirPaciente pacientes = do
  putStr "CPF do paciente a excluir: " >> hFlush stdout
  c <- getLine
  case find (\p -> cpf p == c) pacientes of
    Nothing -> putStrLn "[ERRO] Paciente não encontrado." >> return pacientes
    Just _  -> do
      let novaLista = filter (\p -> cpf p /= c) pacientes
      salvarPacientes novaLista
      putStrLn "[OK] Paciente removido."
      return novaLista


cadastrarMedico :: [Medico] -> IO [Medico]
cadastrarMedico medicos = do
  putStrLn "\n=== Cadastro de Médico ==="
  putStr "Nome do médico: " >> hFlush stdout
  nomeMed <- getLine
  putStr "CRM: " >> hFlush stdout
  crmMed <- getLine
  putStrLn "Especialidades disponíveis:"
  mapM_ (\(i,e) -> putStrLn $ show i ++ ") " ++ show e) (zip [1..] todasEspecialidades)
  putStr "Escolha o número da especialidade: " >> hFlush stdout
  escolha <- readLn
  if escolha < 1 || escolha > length todasEspecialidades
    then putStrLn "[ERRO] Especialidade inválida." >> return medicos
    else do
      let espec = todasEspecialidades !! (escolha - 1)
      putStr "Informe horários base (hh:mm separados por vírgula) ou deixe vazio: " >> hFlush stdout
      hrs <- getLine
      let horariosSep = words $ map (\c -> if c == ',' then ' ' else c) hrs
          novo = Medico nomeMed crmMed espec horariosSep
          lista = medicos ++ [novo]
      salvarMedicos lista
      putStrLn "[OK] Médico cadastrado com sucesso!"
      return lista


excluirMedico :: [Medico] -> IO [Medico]
excluirMedico medicos = do
  putStrLn "\n=== Lista de Médicos ==="
  mapM_ (\(i,m) -> putStrLn $ show i ++ ") " ++ nomeM m ++ " - " ++ show (especialidadeM m))
        (zip [1..] medicos)
  putStr "Digite o CRM do médico que deseja excluir: " >> hFlush stdout
  crmInput <- getLine
  case find (\m -> crm m == crmInput) medicos of
    Nothing -> putStrLn "[ERRO] Médico não encontrado." >> return medicos
    Just _  -> do
      let novaLista = filter (\m -> crm m /= crmInput) medicos
      salvarMedicos novaLista
      putStrLn "[OK] Médico excluído."
      return novaLista



agendarConsulta :: Paciente -> [Medico] -> [Atendimento] -> IO [Atendimento]
agendarConsulta pac medicos atendimentos = do
  putStrLn "\n=== Agendar Consulta (modo simples) ==="
  if null medicos
    then putStrLn "[ERRO] Não há médicos cadastrados." >> return atendimentos
    else do
      putStrLn "Médicos disponíveis:"
      mapM_ (\(i,m) -> putStrLn $ show i ++ ") " ++ nomeM m ++ " - " ++ show (especialidadeM m))
            (zip [1..] medicos)
      putStr "Escolha o número do médico: " >> hFlush stdout
      escolha <- readLn
      if escolha < 1 || escolha > length medicos
        then putStrLn "[ERRO] Opção inválida." >> return atendimentos
        else do
          let medicoEscolhido = medicos !! (escolha - 1)
          putStr "Data da consulta (AAAA-MM-DD): " >> hFlush stdout
          dataC <- getLine
          putStr "Hora (HH:MM): " >> hFlush stdout
          horaC <- getLine
          putStr "Sintomas (opcional): " >> hFlush stdout
          sint <- getLine
          let novoAt = Atendimento
                { paciente        = cpf pac
                , medico          = crm medicoEscolhido
                , especialidade   = especialidadeM medicoEscolhido
                , dataAt          = dataC
                , horaAt          = horaC
                , sintomas        = if null sint then Nothing else Just sint
                , prescricoes     = []
                , status          = Agendada
                , observacao      = Nothing
                , tipoAtendimento = Consulta
                }
              lista = atendimentos ++ [novoAt]
          salvarAtendimentos lista
          putStrLn "[OK] Consulta agendada com sucesso!"
          return lista

cancelarConsulta :: Paciente -> [Atendimento] -> IO [Atendimento]
cancelarConsulta pac atendimentos = do
  putStrLn "\n=== Cancelar Consulta ==="
  let minhas = filter (\a -> paciente a == cpf pac && status a == Agendada) atendimentos
  if null minhas
    then putStrLn "Nenhuma consulta encontrada." >> return atendimentos
    else do
      mapM_ (\(i,a) -> putStrLn $ show i ++ ") " ++ dataAt a ++ " " ++ horaAt a
                        ++ " com médico CRM " ++ medico a)
            (zip [1..] minhas)
      putStr "Escolha o número da consulta para cancelar: " >> hFlush stdout
      escolha <- readLn
      if escolha < 1 || escolha > length minhas
        then putStrLn "[ERRO] Escolha inválida." >> return atendimentos
        else do
          let consultaSel = minhas !! (escolha - 1)
              novaLista   = map (\a -> if a == consultaSel
                                        then a { status = Cancelada }
                                        else a) atendimentos
          salvarAtendimentos novaLista
          putStrLn "[OK] Consulta cancelada."
          return novaLista


criarPrescricao
  :: String      -- CPF do paciente
  -> String      -- CRM do médico
  -> String      -- Texto da prescrição
  -> String      -- Data
  -> [Prescricao]
  -> IO [Prescricao]
criarPrescricao cpfPac crmMed texto dt prescrs = do
  let nova = Prescricao
        { prPaciente = cpfPac
        , prMedico   = crmMed
        , prData     = dt
        , prTexto    = texto
        }
      lista = nova : prescrs
  salvarPrescricoes lista
  putStrLn "[OK] Prescrição salva com sucesso!"
  return lista
