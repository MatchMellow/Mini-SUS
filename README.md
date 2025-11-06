# Clínica Médica  Sistema Universal de Saúde 

Sistema em **Haskell** para gerenciamento simplificado de pacientes, médicos, consultas e exames.

Prover um modelo didático de sistema médico simples, com menus de texto, persistência em arquivos e módulos bem separados.

- `src/Types.hs` — Tipos principais: Paciente, Médico, Atendimento, etc.
- `src/Persistence.hs` — Carrega e salva dados dos arquivos `.db`.
- `src/CRUD.hs` — Funções de cadastro, exclusão e login.
- `src/Scheduling.hs` — Agendamento e cancelamento de consultas.
- `src/Reports.hs` — Gera relatórios e históricos.
- `src/MenuAdmin.hs` — Menu do administrador.
- `src/MenuPaciente.hs` — Menu do paciente.
- `src/Main.hs` — Ponto de entrada do programa.

abrir o terminal na raiz do projeto (`Mini-SUS`) e digite:

```bash
stack setup   
stack build
stack run

Geração de explicações e documentação: apoio na escrita de descrições didáticas sobre os arquivos .hs e no detalhamento do fluxo interno do programa.

Apoio técnico: auxílio na depuração de erros, interpretação de mensagens do compilador e sugestões de boas práticas de Haskell.

Otimização de código: recomendação de melhorias estruturais e funções mais idiomáticas para o paradigma funcional.

Elaboração da apresentação final: suporte na criação de textos explicativos e roteiro de apresentação.

Todo o código foi analisado, revisado e adaptado manualmente pela equipe, a parte de validations e persistence, tivemos uma ajuda  de ia generativa pra criar e adaptar as funções, 
Nós validamos os tipos com IA e sistemas com IA
Nós tivemos apoio de IA para o .cabal para localizar dependencias de validação e para entender qual imports eram mais importantes.
no type especialidade pedimos para IA colocar 21 tipos de especialidade para demonstração.

Um de nossos integrantes trabalha em hospital e nos guiou na parte de separar as consultas de médico pelo CRM, tivemos a ideia de colocar um login de rede pra paciente mas decidimos que seria separado por CPF.
Em geral alguns sistemas médicos Utilizam esses tipos inseridos no projeto.
Porém se for além do sistema de clinica médica existe tipos pra cirurgia, liberação de leito e enfermeiro que seria uma implementação futura que ultrapassaria os 10 tipos.

Rodrigo: MenuAdmin.Hs, MenuMedico.Hs. MenuPaciente.hs, mini-sus.cabal 
Alberico: Main.Hs , Persistence.hs , Reports.hs, Schendule.hs
João: Reports.hs, Types.hs, Validatios.hs, Crud.hs, DoctorSchendule.hs, mini-sus.cabal atualização para colocar o data e conflito de dados.

garantindo que as decisões de implementação e o conteúdo final fossem compreendidos e validados por todos os integrantes.

LINK: Scripts Usados e Alguns prompts IA: 
Anotações que fizemos no início do projeto:

 https://docs.google.com/document/d/1FzI3YSeYuiM6JnLmXs7TmASsJt5p2DxkRrFzHXYsv8E/edit?usp=sharing


Atenciosamente,
Equipe Mini-Sus- Clinica Médica