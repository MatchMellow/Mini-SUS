# Mini-SUS


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

Para os colaboradoresAbra o terminal na raiz do projeto (`Mini-SUS`) e digite:

```bash
stack setup   
stack build
stack run
