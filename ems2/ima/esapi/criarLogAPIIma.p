/**************************************************************************
Programa: esapi/criarLogAPIIma.p
Objetivo: API que facilita a cria‡Æo de log de transa‡Æo para api's IMA
que recebem informa‡Æo de terceiros
Data: 06/2023
Autor:Tadeu Silva
**************************************************************************/
{esp/using_Json.i}
{utp/ut-glob.i}
{esp/utiljson.i}
DEFINE INPUT  PARAMETER pProgOrigem AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pNumCalculo AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER pChave      AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pObjReq     AS jsonobject  NO-UNDO.

DEFINE VARIABLE hBoTransacao AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBoApiLisa   AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBoMsg       AS HANDLE      NO-UNDO.
DEFINE VARIABLE cLink        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cUrl         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDir         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cArq         AS CHARACTER   NO-UNDO.

DEFINE VARIABLE idTransacao AS INTEGER     NO-UNDO.

RUN esp/instProg.p('esbo/boMsg.p', OUTPUT hBoMsg).
RUN esp/instProg.p('esbo/boTransacoes.p', OUTPUT hBoTransacao).
RUN esp/instProg.p('esbo/boApisLisa.p', OUTPUT hBoAPILisa).

RUN gerarTransacao IN hBoTransacao(
        INPUT pProgOrigem,
        INPUT c-seg-usuario,
        INPUT pNumCalculo,
        INPUT pChave,
        OUTPUT idTransacao).
RUN getUrlJson in hBoApiLisa(output cUrl).
RUN getDirJsonLisa IN hboApiLisa(OUTPUT cDir).

ASSIGN  cArq  = pChave  + "_"  + STRING(TIME) + ".json"
        cLink = curl + "/" + cArq 
        cArq  = cDir + "\" + cArq.

RUN saveJson2File(pObjReq,cArq).  

RUN setTransacaoLogCalculo IN hBoMsg(idTransacao).
RUN setMsg       IN hBoMsg(0,'Dados Enviados:' + cLink, 'log').

RUN gravarLogCalculo IN hBoMsg(pNumCalculo).
 
IF VALID-HANDLE(hBoTransacao) THEN
   RUN finalizarTransacao  IN hBoTransacao(1).





