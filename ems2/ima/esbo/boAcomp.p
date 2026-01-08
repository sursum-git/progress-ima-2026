/********************************************************************************************
Programa    : esbo/boAcomp.p
Autor       : Tadeu Silva Parreiras
Objetivo    : Encapsular o h-acomp para utiliza‡Æo de BOs
Data        : 01/2024 
Modificacoes:
*********************************************************************************************/

DEFINE VARIABLE h-acomp         AS HANDLE      NO-UNDO.
DEFINE VARIABLE lHabilita       AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cTitulo         AS CHARACTER   NO-UNDO.

PROCEDURE inicializar:

    RUN utp/ut-acomp.p PERSIST SET h-acomp.
    RUN pi-inicializar IN h-acomp(cTitulo).

END PROCEDURE.




PROCEDURE setHandle:

    DEFINE INPUT  PARAMETER pAcomp AS HANDLE      NO-UNDO.
    ASSIGN h-acomp = pAcomp.

END PROCEDURE.



PROCEDURE setTitulo:

    DEFINE INPUT  PARAMETER pTitulo AS CHARACTER   NO-UNDO.

    ASSIGN cTitulo  = pTitulo.


END PROCEDURE.


PROCEDURE setHabilita:

    DEFINE INPUT  PARAMETER pHabilita AS LOGICAL     NO-UNDO.
    
    ASSIGN lHabilita = pHabilita.


END PROCEDURE.
              
PROCEDURE acomp:

   DEFINE INPUT  PARAMETER pTexto AS CHARACTER   NO-UNDO.
   IF lHabilita THEN DO:
      IF NOT VALID-HANDLE(h-acomp) THEN DO:
         RUN inicializar.
      END.
      RUN pi-acompanhar IN h-acomp(pTexto).
   END.




END PROCEDURE.

PROCEDURE finalizar:
    IF VALID-HANDLE(h-acomp) THEN
       RUN pi-finalizar IN h-acomp.

    DELETE PROCEDURE THIS-PROCEDURE.

END PROCEDURE.







