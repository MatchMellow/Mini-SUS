module Types where
  
-- ESPECIALIDADES MÉDICAS
data Especialidade
  = AlergiaEImunologia
  | Cardiologia
  | CirurgiaGeral
  | ClinicaMedica
  | Dermatologia
  | Endocrinologia
  | Fisioterapia
  | Gastroenterologia
  | Genetica
  | GinecologiaEObstetricia
  | Hematologia
  | Infectologia
  | Neurologia
  | Nutricao
  | Oftalmologia
  | OrtopediaETraumatologia
  | Otorrinolaringologia
  | Pediatria
  | Psicologia
  | Psiquiatria
  | Reumatologia
  deriving (Show, Read, Eq, Enum)


data StatusConsulta = Agendada | Realizada | Cancelada
  deriving (Show, Read, Eq)

data TipoAtendimento
  = Consulta
  | ExameSangue
  | ExameUrina
  | ExameFezes
  | Hemograma
  | Glicemia
  | Colesterol
  | Triglicerideos
  | Creatinina
  | Ureia
  | Eletrocardiograma
  | Ecocardiograma
  | Ultrassonografia
  | RaioX
  | Tomografia
  | Ressonancia
  | Mamografia
  | DensitometriaOssea
  | Endoscopia
  | Colonoscopia
  | Papanicolau
  | TesteCOVID
  | TesteAlergico
  | Audiometria
  | Espirometria
  | EEG
  | EMG
  deriving (Show, Read, Eq, Enum)

data Prescricao = Prescricao
  { prPaciente :: String
  , prMedico   :: String
  , prData     :: String
  , prTexto    :: String
  } deriving (Show, Read, Eq)


data Atendimento = Atendimento
  { paciente        :: String
  , medico          :: String
  , especialidade   :: Especialidade
  , dataAt          :: String
  , horaAt          :: String
  , sintomas        :: Maybe String
  , prescricoes     :: [Prescricao]
  , status          :: StatusConsulta
  , observacao      :: Maybe String
  , tipoAtendimento :: TipoAtendimento
  } deriving (Show, Read, Eq)

data Convenio = Convenio
  { nomePlano      :: String
  , numeroCarteira :: String
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

--  slot para médicos de 20 e 20 min
data Slot = Slot
  { slDia   :: String
  , slHora  :: String
  , slLivre :: Bool
  } deriving (Show, Read, Eq)

data AgendaMedico = AgendaMedico
  { agMedicoCRM :: String
  , agSlots     :: [Slot]
  } deriving (Show, Read, Eq)

-- tipo usuario
data Usuario
  = Admin String
  | PacienteUser Paciente
  deriving (Show, Read, Eq)
