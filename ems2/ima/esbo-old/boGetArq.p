/*
programa: esbo/boGetArq
objetivo: Programa para busca de arquivos em diretorios
data: 05/2023
autor: tadeu
*/


   
                                               
DEFINE VARIABLE cDir    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFiltro AS CHARACTER   NO-UNDO.



PROCEDURE setDir:

    DEFINE INPUT  PARAMETER pDir AS CHARACTER   NO-UNDO.
    ASSIGN cDir = pDir.

END PROCEDURE.


PROCEDURE setFiltro:

    DEFINE INPUT  PARAMETER pFiltro AS CHARACTER   NO-UNDO.
    ASSIGN cFiltro = pFiltro.

END PROCEDURE.

PROCEDURE exec:

   DEFINE VARIABLE cArq         AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE cComando     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE cFiltroAtu   AS CHARACTER   NO-UNDO.

   EMPTY TEMP-TABLE ttArquivos.
   ASSIGN cArq         = SESSION:TEMP-DIRECTORY + STRING(TIME) + '.txt'
          cFiltroAtu   = IF cFiltro <> '' THEN  '\*' + cFiltro + '*' ELSE '' 
          cComando     = 'DIR /b /s ' + cDir  + cFiltroAtu  + ' >' + cArq .
   OS-COMMAND SILENT VALUE(cComando).
   INPUT FROM VALUE(cArq).
    REPEAT.
        CREATE ttArquivos.
        IMPORT ttArquivos.

    END.
    INPUT CLOSE.



END PROCEDURE.




PROCEDURE getTT:

    DEFINE OUTPUT PARAMETER table FOR ttArquivos.

END PROCEDURE.

FUNCTION logAchou RETURNS LOGICAL ():

    FIND FIRST ttArquivos  NO-ERROR.
    RETURN AVAIL ttArquivos.

END FUNCTION.








