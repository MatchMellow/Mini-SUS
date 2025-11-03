module Types where
data Especialidade
  = ClinicoGeral
  | Pediatria
  | Ortopedia
  | Dermatologia
  | Cardiologia
  | Ginecologia
  deriving (Show, Read, Eq, Enum)

data StatusConsulta = Agendada | Realizada | Cancelada
  deriving (Show, Read, Eq)

data Prescricao = Prescricao
  { medicamento :: String
  , dosagem     :: String
  , observacao  :: Maybe String
  } deriving (Show, Read, Eq)

data Atendimento = Atendimento
  {paciente     :: String   --     cpfpac    
  , medico       :: String          -- crmmedico
  , especialidade :: Especialidade
  , dataAt       :: String          
  , horaAt       :: String          
  , sintomas     :: Maybe String
  , prescricoes  :: [Prescricao]
  , status       :: StatusConsulta
  } deriving (Show, Read, Eq)

data Convenio = Convenio
  { nomePlano      :: String
  , numeroCarteira :: String        -- validade carteira
  } deriving (Show, Read, Eq)

data Paciente = Paciente
  { nome      :: String
  , cpf       :: String
  , idade     :: Int
  , senha     :: String             
  , convenio  :: Maybe Convenio     
  , historico :: [Atendimento]      
  } deriving (Show, Read, Eq)


data Medico = Medico
  { nomeM          :: String
  , crm            :: String
  , especialidadeM :: Especialidade
  , horarios       :: [String]      
  } deriving (Show, Read, Eq)

data Usuario = Admin String
             | PacienteUser Paciente
             deriving (Show, Read, Eq)
