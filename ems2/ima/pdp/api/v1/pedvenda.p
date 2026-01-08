/*
programa: pdp/v1/pedvenda.p
objetivo: API a ser utilizada para recebimento dos pedidos de venda separados e que precisam de aprovaá∆o.
data: 05/2023                                                               
*/


{esp/utilAPI.i} //include especifica que inclui a utp/ut-api.i dentro dela
{utp/ut-api-action.i  getSolAprovPedVenda     POST ~* }
{esapi/analisarJsonObject2.i}
//{esp/ttChave.i}
{esp/util.i}



PROCEDURE getSolAprovPedVenda:

    //parametros obrigatorios de entrada e saida
    DEF INPUT PARAM jsonInput           AS JsonObject NO-UNDO. 
    DEF OUTPUT PARAM jsonOutput         AS JsonObject NO-UNDO.
    DEFINE VARIABLE aJsonArray          AS JsonArray  NO-UNDO.
    DEFINE VARIABLE tipoRet             AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE jsonAux             AS JsonObject NO-UNDO.
    DEFINE VARIABLE cArqCriado          AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cDirLog             AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cLog                AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE id                  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE idTransacao         AS INTEGER     NO-UNDO.
    ASSIGN id = getNowToNameFile() + "-" + string(RANDOM(1,99999999999)).
    //ASSIGN cDirLog = SESSION:TEMP-DIRECTORY  + 'ret_separacao/' + STRING(id) .
    ASSIGN cDirLog = SESSION:TEMP-DIRECTORY .
    
    

    //cria os diretorios caso n∆o existam
    RUN verifDirs(cDirLog,"/").
    

    aJsonArray = NEW jsonArray().
    jsonOutput = NEW jsonobject().
    RUN esapi/criarLogAPIIMA2.p( 
       INPUT 'pdp_api_v1_pedvenda',
       INPUT 54,
       INPUT 'api_ima_pedvenda_',
       INPUT jsonInput,
       OUTPUT cArqCriado,
       OUTPUT idTransacao
       ).
   

    jsoninput:writeFile(cDirLog + '/jsoninput_sol-aprov-ped-venda.json').
    RUN esapi/analisarJsonObject2.p(INPUT jsonInput , OUTPUT TABLE ttJson).
    CREATE ttJson.
    ASSIGN ttjson.tag_pai = 'arquivo'
           ttJson.tag     = 'separacao'
           ttJson.valor   = cArqCriado
           .

    
    CREATE ttJson.
    ASSIGN ttjson.tag_pai = ''
           ttJson.tag     = 'id_transacao'
           ttJson.valor   = string(idTransacao)
           .


    RUN gravarRelacArqPed(cArqCriado).

    IF SEARCH(cDirLog) = ? THEN
       OS-CREATE-DIR VALUE(cDirLog).

    OUTPUT TO value(cDirLog + '/ttJsonSol-aprov-ped-venda.txt').
        FOR EACH ttJson:
            EXPORT DELIMITER "|" ttJson.
        END.
    OUTPUT CLOSE.
    RUN esapi/retorno-Isf-Lisa.p(INPUT TABLE ttJson,OUTPUT TABLE ttChave) NO-ERROR.
    
    FIND FIRST ttChave NO-ERROR.
    IF AVAIL ttChave THEN DO:
       RUN retJsontt2(INPUT-OUTPUT jsonOutput,TEMP-TABLE ttChave:HANDLE,'erro',401 ) NO-ERROR . 
    END.
    ELSE DO:
       RUN retJsonTt2(INPUT-OUTPUT jsonOutput,TEMP-TABLE ttChave:HANDLE,'vazio',200 ) NO-ERROR. 
    END.
    RUN esapi/criarLogAPIIMA2.p( 
       INPUT 'pdp_api_v1_pedvenda',
       INPUT 54,
       INPUT 'api_ima_pedvenda_retorno',
       INPUT jsonOutput,
       OUTPUT cArqCriado,
       OUTPUT idTransacao
       ).       
    
    jsonOutput:WriteFile(cDirLog + '/log_api_sol-aprov-ped-venda.txt').

    /*jsonOutput = NEW jsonobject().
    jsonOutput:ADD('status','402').
    jsonAux = NEW jsonobject().
    jsonAux:ADD('teste','tadeu').
    jsonOutput:ADD('payload',jsonAux).*/

    CATCH oError AS Progress.Lang.Error : 
    jsonOutput:ADD('status',402).
    jsonOutput:ADD('erro Progress',oError:GetMessage(1) + '-' + oError:CallStack).
    OUTPUT TO value(cDirLog + '/ERRO.txt').
          PUT UNFORM oError:GetMessage(1) SKIP(2)
            oError:CallStack .
        
    OUTPUT CLOSE.
    END CATCH.




END PROCEDURE.



PROCEDURE gravarRelacArqPed.
    DEFINE INPUT  PARAMETER pArqCriado  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE hBoLisaIntegra01    AS HANDLE      NO-UNDO.
    RUN esbo/boLisaIntegra01.p PERSIST SET hBoLisaIntegra01.
    RUN setArquivo          IN hBoLisaIntegra01(pArqCriado).
    RUN setPedido           IN hBoLisaIntegra01(getChaveTTJson('payload', 'pedidoCliente') ).
    RUN setDtInclusaoPedido IN hBoLisaIntegra01(getChaveTTJson('payload', 'dataInclusao')).
    RUN setHrInclusaoPedido IN hBoLisaIntegra01(getChaveTTJson('payload', 'horaInclusao')).
    RUN sincrArq            IN hBoLisaIntegra01.

    IF VALID-HANDLE(hBoLisaIntegra01) THEN
       DELETE PROCEDURE hBoLisaIntegra01.



END PROCEDURE.
