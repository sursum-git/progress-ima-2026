DEFINE VARIABLE cLinha AS CHARACTER   NO-UNDO FORMAT 'x(200)'.
DEFINE VARIABLE cRegistro AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cConta AS CHARACTER   NO-UNDO.
DEFINE VARIABLE ctipo  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cNivel AS CHARACTER   NO-UNDO.
DEFINE VARIABLE logDesconsiderarPai AS LOGICAL   INIT NO  NO-UNDO.

DEFINE TEMP-TABLE ttLinhas
    FIELD Linha AS CHAR FORMAT 'x(200)'
    FIELD LOG_desconsiderar AS LOG INIT NO.
DEFINE TEMP-TABLE ttContas
    FIELD codigo AS CHAR.
DEFINE BUFFER bf-ttContas FOR ttContas.
INPUT FROM c:\temp\saneamento_repetidas.txt.
REPEAT:
    IMPORT UNFORM cLinha.
    CREATE ttLinhas.
    ASSIGN ttLinhas.linha = cLinha.
    ASSIGN cRegistro = ENTRY(2,cLinha,"|").
    IF cRegistro = 'j050' THEN DO:
       ASSIGN cConta = ENTRY(7,cLinha,"|") 
              cTipo  = ENTRY(5,cLinha,"|")
              cNivel = ENTRY(4,cLinha,"|").
       FIND FIRST bf-ttcontas 
           WHERE bf-ttcontas.codigo = cConta
           NO-LOCK NO-ERROR.
       IF AVAIL bf-ttContas THEN DO:
          ASSIGN ttLinhas.LOG_desconsiderar = YES
                 logDesconsiderarPai = YES.
       END.                                       
       ELSE DO:
         CREATE ttContas.
         ASSIGN ttContas.codigo = cConta
                ttLinhas.LOG_desconsiderar = NO
                logDesconsiderarPai = NO.
       END.
    END.    
    /*MESSAGE cTipo SKIP
            cRegistro SKIP
            cNivel
        VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
    IF ctipo = 's' AND cRegistro = 'j051' AND cNivel = '04' THEN DO:
       ASSIGN ttLinhas.LOG_desconsiderar = YES.
       /*MESSAGE  cConta
           VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
    END.
       

    IF ctipo = 'a' AND cRegistro = 'j051' THEN
      ASSIGN ttLinhas.LOG_desconsiderar = NO.



    IF logDesconsiderarPai = YES AND cRegistro = 'j051' THEN
       ASSIGN ttLinhas.LOG_desconsiderar = YES.
   
END.
INPUT CLOSE.

OUTPUT TO c:\temp\novos_registros_j.txt.
FOR EACH ttLinhas
    WHERE ttLinhas.LOG_desconsiderar = NO:
    EXPORT  ttLinhas.linha.
END.
OUTPUT CLOSE.
