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
    DEFINE VARIABLE jsonAux             AS JsonObject NO-UNDO.    
    DEFINE VARIABLE cDirLog             AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE id                  AS CHARACTER  NO-UNDO.   
    ASSIGN id = getNowToNameFile() + "-" + string(RANDOM(1,99999999999)).
    ASSIGN cDirLog = SESSION:TEMP-DIRECTORY  + 'tadeu/silva/parreiras' .

    //cria os diretorios caso n∆o existam
    RUN verifDirs(cDirLog,"/").
    
    
    



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
