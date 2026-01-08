/***************************************************************************
programa: esapi/verifExist.p
Objetivo: Verificar dinamicamente se existe registro para chave unica
data: 12/2023
Autor:Tadeu Silva
****************************************************************************/
DEFINE INPUT  PARAMETER pTabela AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pCampos AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pCond   AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pInner  AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER lAchou  AS LOGICAL   NO-UNDO.

DEFINE VARIABLE hBo AS HANDLE      NO-UNDO.
RUN esbo/boConsDin.p    PERSIST SET  hBo.
RUN iniciarBos          IN hBo.
RUN setDadosConsulta    IN hBo(pTabela,
                               pCampos,
                               pCond,
                               pInner
                               ).
RUN execConsulta        IN hBo('buffer').
RUN verifExist          IN hBo(OUTPUT lAchou).
RUN finalizarBOs    IN Hbo.
IF VALID-HANDLE(hBo) THEN
   DELETE PROCEDURE hBo.




