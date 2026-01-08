DEFINE VARIABLE cArquivoSaida    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cExtensaoArquivo AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lGerarLog        AS LOGICAL     NO-UNDO INIT NO.
DEFINE TEMP-TABLE tt
       FIELD grupo AS CHAR  FORMAT 'x(50)'
       FIELD texto AS CHAR FORMAT 'x(1000)'.

PROCEDURE arquivoSaida:
    DEFINE INPUT  PARAMETER pArquivoSaida AS CHARACTER   NO-UNDO.
    ASSIGN  cArquivoSaida = pArquivoSaida.
END.

PROCEDURE gerarLog:
    DEFINE INPUT  PARAMETER l AS LOGICAL     NO-UNDO.
    ASSIGN lGerarLog = l.

END PROCEDURE.

PROCEDURE incluirLog:
   DEFINE INPUT  PARAMETER pGrupo AS CHARACTER FORMAT 'x(20)'     NO-UNDO.
   DEFINE INPUT  PARAMETER ptexto AS CHARACTER FORMAT 'x(1000)'   NO-UNDO.
   IF lGerarLog THEN DO:
      CREATE tt.
      ASSIGN tt.grupo = pGrupo
             tt.texto = ptexto.  
   END.                          
END.


PROCEDURE imprimirtxt:
    DEFINE VARIABLE l AS LOGICAL  NO-UNDO INIT NO.
    DEFINE VARIABLE cExtensao AS CHARACTER   NO-UNDO.
    IF  cExtensaoArquivo = '' THEN
        ASSIGN cExtensao = '.txt'.
    ELSE
       ASSIGN cExtensao = cExtensaoARquivo.
    OUTPUT TO value(SESSION:TEMP-DIRECT + cArquivoSaida + string(TIME) + cExtensao).

    FOR EACH tt:
        ASSIGN l = YES.
        PUT texto SKIP.
    END. 
    IF l = NO THEN
       PUT "NÆo h  dados" SKIP.

    OUTPUT CLOSE.
END.

PROCEDURE extensaoArquivo:
    DEFINE INPUT  PARAMETER pExtensao AS CHARACTER   NO-UNDO.
    ASSIGN cExtensaoArquivo = pExtensao.

END.
