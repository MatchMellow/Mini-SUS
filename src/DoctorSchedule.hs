module DoctorSchedule where
  
import Types
import Persistence
import Data.List (find)

-- gera lista de horários de 20 em 20 minutos
gerarSlots20min :: String -> String -> String -> [Slot]
gerarSlots20min dia inicio fim =
  let minutosTot = toMin fim
      go m
        | m > minutosTot = []
        | otherwise =
            let hStr = toHora m
            in Slot { slDia = dia, slHora = hStr, slLivre = True } : go (m + 20)
  in go (toMin inicio)
  where
    toMin s =
      let (h, _:m) = span (/=':') s
      in read h * 60 + read m

    toHora m =
      let h  = m `div` 60
          mm = m `mod` 60
          h'  = if h  < 10 then '0':show h  else show h
          mm' = if mm < 10 then '0':show mm else show mm
      in h' ++ ":" ++ mm'

-- médico libera um dia (ex.: "Segunda" 08:00-16:00)
liberarDiaMedico :: String -> String -> String -> String -> [AgendaMedico] -> [AgendaMedico]
liberarDiaMedico crm dia hIni hFim agendas =
  let novosSlots = gerarSlots20min dia hIni hFim
  in case find (\a -> agMedicoCRM a == crm) agendas of
       Nothing ->
         AgendaMedico crm novosSlots : agendas
       Just ag ->
         let outros         = filter (\a -> agMedicoCRM a /= crm) agendas
             slotsSemEsseDia = filter (\s -> slDia s /= dia) (agSlots ag)
             slotsNovos      = slotsSemEsseDia ++ novosSlots
         in AgendaMedico crm slotsNovos : outros

-- marcar um slot como ocupado
ocuparSlot :: String -> String -> String -> [AgendaMedico] -> [AgendaMedico]
ocuparSlot crm dia hora agendas =
  map atualiza agendas
  where
    atualiza ag
      | agMedicoCRM ag /= crm = ag
      | otherwise =
          let slots' = map (\s ->
                             if slDia s == dia && slHora s == hora
                               then s { slLivre = False }
                               else s
                           ) (agSlots ag)
          in ag { agSlots = slots' }

-- desocupar slot (quando consulta é cancelada)
desocuparSlot :: String -> String -> [AgendaMedico] -> [AgendaMedico]
desocuparSlot crm hora agendas =
  map atualiza agendas
  where
    atualiza ag
      | agMedicoCRM ag /= crm = ag
      | otherwise =
          let slots' = map (\s ->
                             if slHora s == hora
                               then s { slLivre = True }
                               else s
                           ) (agSlots ag)
          in ag { agSlots = slots' }

-- listar slots livres de um médico em um dia
slotsLivresDoDia :: String -> String -> [AgendaMedico] -> [Slot]
slotsLivresDoDia crm dia agendas =
  case find (\a -> agMedicoCRM a == crm) agendas of
    Nothing     -> []
    Just agenda -> filter (\s -> slDia s == dia && slLivre s) (agSlots agenda)

-- versão que já salva
liberarDiaMedicoESalvar :: String -> String -> String -> String -> [AgendaMedico] -> IO [AgendaMedico]
liberarDiaMedicoESalvar crm dia ini fim agendas = do
  let novas = liberarDiaMedico crm dia ini fim agendas
  salvarAgendas novas
  putStrLn "[OK] Agenda salva."
  return novas
