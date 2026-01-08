DEFINE VARIABLE cArquivoSaida AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cExtensaoArquivo AS CHARACTER   NO-UNDO.
DEFINE TEMP-TABLE tt
       FIELD grupo AS CHAR  FORMAT 'x(50)'
       FIELD texto AS CHAR FORMAT 'x(1000)'.

PROCEDURE arquivoSaida:
    DEFINE INPUT  PARAMETER pArquivoSaida AS CHARACTER   NO-UNDO.
    ASSIGN  cArquivoSaida = pArquivoSaida.
END.

PROCEDURE incluirLog:
   DEFINE INPUT  PARAMETER pGrupo AS CHARACTER FORMAT 'x(20)'     NO-UNDO.
   DEFINE INPUT  PARAMETER ptexto AS CHARACTER FORMAT 'x(1000)'   NO-UNDO.
   CREATE tt.
   ASSIGN tt.grupo = pGrupo
          tt.texto = ptexto.
END.


PROCEDURE imprimirtxt:
    DEFINE VARIABLE l AS LOGICAL  NO-UNDO INIT NO.
    DEFINE VARIABLE cExtensao AS CHARACTER   NO-UNDO.
    IF  cExtensaoArquivo = '' THEN
        ASSIGN cExtensao = '.txt'.
    ELSE
       ASSIGN cExtensao = cExtensaoARquivo.
    OUTPUT TO value(SESSION:TEMP-DIRECT + cArquivoSaida + string(TIME) + cExtensao).

    FOR EACH tt
        BREAK BY grupo:
        ASSIGN l = YES.
        IF FIRST-OF(grupo) THEN
           PUT grupo SKIP.
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
