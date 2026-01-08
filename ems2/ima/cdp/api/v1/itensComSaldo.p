                                                                             
{esp/utilAPi.i}
{utp/ut-api-action.i    piPrinc    GET ~*}
/*{utp/ut-api-action.i    piCodigo    GET ~* cod_emit=*}
{utp/ut-api-action.i    piNomeAbrev GET ~* nome_abrev=*}*/
{utp/ut-api-notfound.i}
{esbo/boSaldo2.i}
{esp/util.i}

PROCEDURE piPrinc:

    DEF INPUT PARAM jsonInput       AS JsonObject  NO-UNDO.
    DEF OUTPUT PARAM jsonOutput     AS JsonObject  NO-UNDO. 
    DEFINE VARIABLE hBoSaldo2       AS HANDLE      NO-UNDO.
    
    jsonOutput = NEW JsonObject().
    //RUN criarTTRetorno('cod_emit:' + string(iEmit) ,1).                       
    RUN esbo/boSaldo2.p PERSIST SET hBoSaldo2.
    RUN iniciar   IN hBoSaldo2.
    RUN limparTTs IN hBoSaldo2.
    RUN setTTItensFaturaveis   IN hBoSaldo2.
    RUN setParamsLocaisEstoque IN hBoSaldo2.
    RUN getSaldo  IN hBoSaldo2.
    RUN getTTItens IN hBoSaldo2(OUTPUT TABLE ttItens).    
    RUN finalizar IN hBoSaldo2.
   

    RUN retJsontt(INPUT-OUTPUT jsonOutput, TEMP-TABLE ttItens:HANDLE ).

    //RUN retJsontt(INPUT-OUTPUT jsonOutput, TEMP-TABLE ttRetorno:HANDLE ).


END PROCEDURE.















