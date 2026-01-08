/******************************************************************
programa:getSufixoCalculo.p
objetivo: Facilitar o retorno do sufixo do calculo por meio de API
******************************************************************/

DEFINE INPUT  PARAMETER pcalculo AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER cSufixo  AS CHARACTER   NO-UNDO.

DEFINE VARIABLE hBoCalculo          AS HANDLE      NO-UNDO.
RUN esbo/boCalculos.p       PERSISTENT SET hBoCalculo .
RUN setCalculo IN hBoCalculo(pCalculo). //comissao pedweb
RUN getSufixo IN hBoCalculo(OUTPUT cSufixo).
IF VALID-HANDLE(hboCalculo) THEN
   DELETE PROCEDURE hboCalculo .
