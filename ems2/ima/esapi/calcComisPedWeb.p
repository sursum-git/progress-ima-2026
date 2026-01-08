/********************************************************************
programa: esapi/calcComisPedWeb.p
objetivo: api para calculo de comiss∆o a partir do pedido web.
********************************************************************/
{esp/utilApi.i2} //tabelas tempor†rias utilApi.i


DEFINE INPUT  PARAMETER pPedWebId       AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER pTransacao      AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER pLogin          AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pAtualiza       AS LOGICAL     NO-UNDO.
DEFINE INPUT  PARAMETER pCodPrograma    AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR ttRetorno.
DEFINE OUTPUT PARAMETER TABLE FOR ttChave.           

DEFINE VARIABLE iRepres    AS INTEGER     NO-UNDO.
DEFINE VARIABLE iRepres2   AS INTEGER     NO-UNDO.
DEFINE VARIABLE percComis  AS DECIMAL     NO-UNDO.
DEFINE VARIABLE percComis2 AS DECIMAL     NO-UNDO.

DEFINE VARIABLE hBoTransacao        AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBoComis            AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBoCalculo          AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBoPedsWeb          AS HANDLE      NO-UNDO.
DEFINE VARIABLE cSufixo             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iCalculo            AS INTEGER     NO-UNDO.
DEFINE VARIABLE lAchou              AS LOGICAL     NO-UNDO.
RUN esbo/boTransacoes.p     PERSISTENT SET hBoTransacao .
RUN esbo/boComis.p          PERSISTENT SET hBoComis .
RUN esbo/boCalculos.p       PERSISTENT SET hBoCalculo .
RUN esbo/boPedsWeb.p        PERSISTENT SET hBoPedsWeb .


ASSIGN iCalculo = 13. 
IF pTransacao = 0 THEN DO:
   /*buscar Sufixo Calculo*/
   RUN setCalculo IN hBoCalculo(iCalculo). //comissao pedweb
   RUN GETSufixo IN hBoCalculo(OUTPUT cSufixo).
      
    
   /*iniciar transacao*/
   RUN setChave IN hBoTransacao(cSufixo + STRING(pPedWebId)).
   RUN setCodPrograma IN hBoTransacao(pCodPrograma).
   IF pLogin <> '' THEN
      RUN setLogin IN hBoTransacao(pLogin).

   RUN iniciarTransacao IN hBoTransacao.
   RUN getIDTransCorrente IN hBoTransacao(OUTPUT pTransacao).

END.
RUN iniciarBos   IN hBoComis.
RUN setTransacao IN HBoComis(pTransacao).
RUN setCalculo   IN hBoComis(iCalculo).
RUN setPedWebId  IN hBoComis(pPedWebId).
RUN setPedWebId  IN hBoPedsWeb(pPedWebId). 
RUN setPropsPedsWeb IN hBoComis.
RUN setLoginPortal IN hBoComis(pLogin).
RUN calcPercComis IN hBoComis(OUTPUT percComis).
RUN getPercComis2 IN hBoComis(OUTPUT percComis2).
RUN getCodsRepres IN hBoComis(OUTPUT iRepres, OUTPUT iRepres2).


//caso os c¢digos sejam iguais, s¢ existe o primeiro c¢digo repres.
IF iRepres = iRepres2 THEN
   ASSIGN iRepres2 = 0.

IF pAtualiza THEN DO:
    RUN atuPercComis IN hBoPedsWeb(percComis,percComis2, OUTPUT lAchou).
END.


/************************************************
por enquanto n∆o Ç necess†rio retornar os c¢digos
dos representantes, pois Ç de responsabilidade
do portal colocar estes c¢digos e n∆o da BO de Comiss∆o
que s¢ os recebe .

RUN criarTTChave('cod_repres',iRepres).
RUN criarTTChave('cod_repres_2',iRepres2).

************************************************/

RUN criarTTChave('perc_comis',string(percComis)).
RUN criarTTChave('perc_comis_2',string(percComis2)).
RUN criarTTChave('log_achou',string(lAchou)).
RUN finalizarBos IN hBoComis.

RUN finalizarTransacao IN hBoTransacao(1).


IF VALID-HANDLE(hBoComis) THEN
   DELETE PROCEDURE hBoComis.

IF VALID-HANDLE(hBoTransacao) THEN
   DELETE PROCEDURE hBoTransacao.

IF VALID-HANDLE(hBoCalculo) THEN
   DELETE PROCEDURE hBoCalculo .

IF VALID-HANDLE(hBoPedsWeb) THEN
   DELETE PROCEDURE hBoPedsWeb .











