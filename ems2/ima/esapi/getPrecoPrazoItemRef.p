/***************************************************
Programa:esapi/getPrecoPrazoItemRef.p
Objetivo: Api para busca simplificada de Pre‡o
Autor: Tadeu Silva Parreiras
Data:02/2022
***************************************************/

DEFINE INPUT  PARAMETER pTbPreco        AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER pDtRefer        AS DATE        NO-UNDO.
DEFINE INPUT  PARAMETER pItCodigo       AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pCodRefer       AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pNrContainer    AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER pTipoBusca      AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER pPrazoMedio     AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER pUfCliente      AS CHARACTER   NO-UNDO.
//DEFINE INPUT  PARAMETER hBoPreco        AS HANDLE      NO-UNDO.
DEFINE OUTPUT PARAMETER precoReal       AS DECIMAL     NO-UNDO.
DEFINE OUTPUT PARAMETER precoDolar      AS DECIMAL     NO-UNDO.
DEFINE OUTPUT PARAMETER precoID         AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER precoOutlet     AS DECIMAL     NO-UNDO.
DEFINE OUTPUT PARAMETER idOutlet        AS int64       NO-UNDO.


DEFINE VARIABLE precoDolarOutlet        AS DECIMAL     NO-UNDO.




DEFINE VARIABLE hBoPrecosItemRef        AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBoCOndPagtoPed         AS HANDLE      NO-UNDO.
/*DEFINE VARIABLE precoReal               AS DECIMAL     NO-UNDO.
DEFINE VARIABLE precoDolar              AS DECIMAL     NO-UNDO.
DEFINE VARIABLE idPreco                 AS INTEGER     NO-UNDO.*/


RUN esbo/boPrecosItemRef.p PERSISTENT SET hBoPrecosItemRef.
RUN iniciarBos      IN hBoPrecosItemRef.     

/*IF hBoPreco = ? THEN DO:
   RUN esbo/boPrecosItemRef.p PERSISTENT SET hBoPrecosItemRef.
   RUN iniciarBos      IN hBoPrecosItemRef.     
END.
ELSE DO:
   ASSIGN hBoPrecosItemRef = hBoPreco .
END.*/            


RUN setTbPreco      IN hBoPrecosItemRef(pTbPreco).
RUN setDtRefer      IN hBoPrecosItemRef(pDtRefer).
RUN setItem         IN hBoPrecosItemRef(pItCodigo).
RUN setRef          IN hBoPrecosItemRef(pCodRefer).
RUN setNrContainer  IN hBoPrecosItemRef(pNrContainer).
RUN setTipoBusca    IN hBoPrecosItemRef(pTipoBusca).
RUN setPrazoMedio   IN hBoPrecosItemRef(pPrazoMedio).
RUN setUfCliente    IN hBoPrecosItemRef(pUfCliente).
RUN buscarPrecos    IN hBoPrecosItemRef.
RUN getPrecoPrazo   IN hBoPrecosItemRef(IF pTipoBusca = 1 THEN 'pe' ELSE 'pi',
                       OUTPUT precoReal,
                       OUTPUT precoDolar,
                       OUTPUT precoID ).
//retornar tamb‚m o outlet
IF pTipoBusca = 1 THEN DO:

  RUN getPrecoPrazo IN hBoPrecosItemRef('outlet',
                       OUTPUT precoOutlet,
                       OUTPUT precoDolarOutlet,
                       OUTPUT idOutlet ).

END.








/*MESSAGE precoReal SKIP
        precoDolar SKIP
         idPreco
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/



/*IF hBoPreco <> ? THEN DO:
   RUN finalizarBos IN hBoPrecosItemRef.
   IF VALID-HANDLE(hBoPrecosItemRef) THEN
      DELETE PROCEDURE hBoPrecosItemRef.
END.*/
RUN finalizarBos IN hBoPrecosItemRef.
IF VALID-HANDLE(hBoPrecosItemRef) THEN
   DELETE PROCEDURE hBoPrecosItemRef.





