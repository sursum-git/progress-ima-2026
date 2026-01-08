/*
programa: pdp/v1/pedvenda.p
objetivo: API a ser utilizada para recebimento dos pedidos de venda separados e que precisam de aprova‡Æo.
data: 05/2023                                                               
*/


{esp/utilAPI.i} //include especifica que inclui a utp/ut-api.i dentro dela
{utp/ut-api-action.i  getSolAprovPedVenda     POST ~* }
{esapi/analisarJsonObject2.i}
//{esp/ttChave.i}




PROCEDURE getSolAprovPedVenda:

    //parametros obrigatorios de entrada e saida
    DEF INPUT PARAM jsonInput           AS JsonObject NO-UNDO. 
    DEF OUTPUT PARAM jsonOutput         AS JsonObject NO-UNDO.
    DEFINE VARIABLE aJsonArray          AS JsonArray  NO-UNDO.
    DEFINE VARIABLE tipoRet             AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE jsonAux             AS JsonObject NO-UNDO.
    DEFINE VARIABLE cArqCriado          AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE idTransacao         AS INTEGER     NO-UNDO.
    aJsonArray = NEW jsonArray().
    jsonOutput = NEW jsonObject().
    RUN esapi/criarLogAPIIMA2.p( 
       INPUT 'pdp_api_v1_pedvenda',
       INPUT 54,
       INPUT 'api_ima_pedvenda_',
       INPUT jsonInput,
       OUTPUT cArqCriado,
       OUTPUT idTransacao
    ).
   

    jsoninput:writeFile('c:\temp\jsoninput_sol-aprov-ped-venda_v2_' + STRING(TIME)).
    RUN esapi/analisarJsonObject2.p(INPUT jsonInput , OUTPUT TABLE ttJson).
    CREATE ttJson.
    ASSIGN ttjson.tag_pai = 'arquivo'
           ttJson.tag     = 'separacao'
           ttJson.valor   = cArqCriado.


    RUN gravarRelacArqPed(cArqCriado).

    OUTPUT TO c:\temp\tst\ttJsonSol-aprov-ped-venda_v2.txt.
        FOR EACH ttJson:
            EXPORT DELIMITER "|" ttJson.
        END.
    OUTPUT CLOSE.
    //RUN esapi/retorno-Isf-Lisa-v3.p(INPUT TABLE ttJson,OUTPUT TABLE ttChave) NO-ERROR.
    
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
    
    jsonOutput:WriteFile('c:\temp\log_api_sol-aprov-ped-venda_v2_' + STRING(TIME) + '.txt').

    /*jsonOutput = NEW jsonobject().
    jsonOutput:ADD('status','402').
    jsonAux = NEW jsonobject().
    jsonAux:ADD('teste','tadeu').
    jsonOutput:ADD('payload',jsonAux).*/

    CATCH oError AS Progress.Lang.Error : 
        jsonOutput:ADD('status',402).
        jsonOutput:ADD('erro Progress',oError:GetMessage(1) + '-' + oError:CallStack).
        OUTPUT TO value('c:\temp\tst\ERRO_' + STRING(TIME) + ".txt").
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
