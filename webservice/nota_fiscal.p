DEFINE TEMP-TABLE ttNota
    FIELD estab         LIKE nota-fiscal.cod-estabel
    FIELD serie         LIKE nota-fiscal.serie
    FIELD nota          LIKE nota-fiscal.nr-nota-fis
    FIELD codEmitente   LIKE nota-fiscal.cod-emitente
    FIELD valor         LIKE nota-fiscal.vl-tot-nota
    FIELD nomeRepres    LIKE nota-fiscal.no-ab-reppri.

DEFINE VARIABLE dtIni AS DATE        NO-UNDO INIT 01.01.2001.
DEFINE VARIABLE dtFim AS DATE        NO-UNDO INIT 12.31.2999.
DEFINE VARIABLE nomeAbrevRepresIni LIKE nota-fiscal.no-ab-reppri   NO-UNDO INIT ''.
DEFINE VARIABLE nomeAbrevRepresfim LIKE nota-fiscal.no-ab-reppri   NO-UNDO INIT 'zzzzzzzzzzzzzz'.
DEFINE VARIABLE codEmitenteIni     AS INTEGER     NO-UNDO INIT 0.
DEFINE VARIABLE codEmitenteFim     AS INTEGER     NO-UNDO INIT 9999999.


PROCEDURE setNomeAbrevRepres:
DEFINE INPUT  PARAMETER cNomeAbrev AS CHARACTER   NO-UNDO.
ASSIGN nomeAbrevRepresIni = cNomeAbrev
       nomeAbrevRepresFim = cNomeAbrev.


END PROCEDURE.

PROCEDURE setIntervalDtEmis:
DEFINE INPUT  PARAMETER data AS DATE        NO-UNDO.
ASSIGN dtIni = data
       dtFim = data.

END PROCEDURE.

PROCEDURE setCliente:
DEFINE INPUT  PARAMETER iEmitente AS INTEGER     NO-UNDO.
ASSIGN codEmitenteIni = iEmitente
       codEmitenteFim = iEmitente.
END PROCEDURE.




PROCEDURE getDados:

FOR EACH nota-fiscal NO-LOCK
    WHERE nota-fiscal.dt-emis-nota >= dtIni
    AND   nota-fiscal.dt-emis-nota  <= dtFim
    AND   nota-fiscal.no-ab-reppri >= nomeAbrevRepresIni
    AND   nota-fiscal.no-ab-reppri <= nomeAbrevRepresFim
    AND   nota-fiscal.cod-emitente >= codEmitenteIni
    AND   nota-fiscal.cod-emitente <= codEmitenteFim.
    CREATE ttNota.
    ASSIGN ttNota.estab         = nota-fiscal.cod-estabel
           ttNota.serie         = nota-fiscal.serie
           ttNota.nota          = nota-fiscal.nr-nota-fis
           ttNota.codEmitente   = nota-fiscal.cod-emitente
           ttNota.valor         = nota-fiscal.vl-tot-nota
           ttNota.nomeRepres    = nota-fiscal.no-ab-reppri.


END.

END PROCEDURE.

PROCEDURE retDados:

DEFINE OUTPUT PARAMETER TABLE FOR ttNota.


END PROCEDURE.


