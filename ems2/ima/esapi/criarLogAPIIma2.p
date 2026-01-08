/**************************************************************************
Programa: esapi/criarLogAPIIma2.p
Objetivo: API que facilita a cria‡Æo de log de transa‡Æo para api's IMA
que recebem informa‡Æo de terceiros.
ESTA VERSÇO 2 TEM O RETORNO DO NOME DO ARQUIVO CRIADO. A VERSÇO 1 NÇO TEM 
E COMO A VERSÇO 1 ESTµ EM DIVERSOS PROGRAMAS, FOI CRIADA NOVA VERSÇO
PARA NÇO QUEBRAR OS PROGRAMAS ANTERIORES.
Data: 12/2023
Autor:Tadeu Silva
Altera‡Æo: 12/2023 - Acrescimo de parametro de retorno do Id da transa‡Æo
**************************************************************************/
{esp/using_Json.i}
{utp/ut-glob.i}
{esp/utiljson.i}
DEFINE INPUT  PARAMETER pProgOrigem AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pNumCalculo AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER pChave      AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pObjReq     AS jsonobject  NO-UNDO.
DEFINE OUTPUT PARAMETER cArq        AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER idTransacao AS INTEGER     NO-UNDO.

DEFINE VARIABLE hBoTransacao        AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBoApiLisa          AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBoMsg              AS HANDLE      NO-UNDO.
DEFINE VARIABLE cLink               AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cUrl                AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDir                AS CHARACTER   NO-UNDO.

//DEFINE VARIABLE cArq         AS CHARACTER   NO-UNDO.
//DEFINE VARIABLE idTransacao AS INTEGER      NO-UNDO.
//OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY + 'criarlogApiIma2_' + STRING(TIME) + '.txt').        
RUN esp/instProg.p('esbo/boMsg.p'        , OUTPUT hBoMsg).
RUN esp/instProg.p('esbo/boTransacoes.p' , OUTPUT hBoTransacao).
RUN esp/instProg.p('esbo/boApisLisa2.p'  , OUTPUT hBoAPILisa).
//PUT UNFORM 'ponto 1' SKIP.
RUN iniciar IN hBoApiLISA.
//PUT UNFORM 'ponto 2' SKIP.
RUN gerarTransacao IN hBoTransacao(INPUT pProgOrigem,INPUT c-seg-usuario,INPUT pNumCalculo,INPUT pChave,OUTPUT idTransacao).
//PUT UNFORM 'ponto 3' SKIP.
CASE pNumCalculo:
    WHEN 56 THEN DO:
        RUN getDirJsonRetornoLisa IN hboApiLisa(OUTPUT cDir).
        RUN getUrlRetornoJson     IN hBoApiLisa(output cUrl).
    END.
    OTHERWISE DO:
        RUN getDirJsonLisa  IN hboApiLisa(OUTPUT cDir).
        RUN getUrlJson      IN hBoApiLisa(output cUrl).
    END.
END CASE.
//PUT UNFORM 'ponto 4' SKIP.
ASSIGN  cArq  = pChave  + "_"  + STRING(TIME) + STRING(RANDOM(1,9999999))  + ".json"
        cLink = curl + "/" + cArq 
        cArq  = cDir + "/" + cArq
        .
IF OPSYS = 'unix' THEN DO: 
    ASSIGN  cArq = REPLACE(cArq,'\','/').
    
END.
//PUT UNFORMAT cDir SKIP.        

//PUT UNFORM 'ponto 5' SKIP.        
//PUT UNFORM "arquivo a ser salvo:" cArq.
//OUTPUT CLOSE.
RUN saveJson2File(pObjReq,cArq) .  

RUN setTransacaoLogCalculo  IN hBoMsg(idTransacao).
RUN setMsg                  IN hBoMsg(0,'Dados Enviados:' + cLink, 'log').
RUN gravarLogCalculo        IN hBoMsg(pNumCalculo).
 
IF VALID-HANDLE(hBoTransacao) THEN
   RUN finalizarTransacao  IN hBoTransacao(1).

IF VALID-HANDLE(hBoApiLISA) THEN
    RUN finalizar IN  hBoApiLISA.



