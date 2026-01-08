
/*
programa: pdp/cdp/v1/cliente.p
objetivo: API a ser utilizada para inclus∆o de cliente no ERP Datasul EMS5.
data: 12/2022

*/

{esp/utilAPI.i} //include especifica que inclui a utp/ut-api.i dentro dela

{utp/ut-api-action.i  incluirCliente     POST ~* }
{utp/ut-api-action.i  incluirCliente2     GET ~* }
/*{utp/ut-api-action.i getUltDp            GET ~* log_ult_dp_venc=1}
{utp/ut-api-action.i getTodasDp          GET ~* log_todas_dp_aberto=1}
{utp/ut-api-action.i getTodasDp          GET ~* log_todas_dp_venc=1}
{utp/ut-api-action.i getTodasDpNF        GET ~* log_todas_dp_nf=1}   */
{utp/ut-api-notfound.i}

 {esapi/clienteConvJson.i}
 {esbo/ttErroCliente.i}

/*DEFINE TEMP-TABLE ttAux LIKE ttEmitente.
DEFINE VARIABLE iUlt    AS INTEGER     NO-UNDO.
DEFINE VARIABLE cErro   AS CHARACTER   NO-UNDO.*/

//DEFINE TEMP-TABLE ttEmitente LIKE emitente.

PROCEDURE incluirCliente:
    
    //parametros obrigatorios de entrada e saida
    DEF INPUT PARAM jsonInput       AS JsonObject NO-UNDO. 
    DEF OUTPUT PARAM jsonOutput     AS JsonObject NO-UNDO.
    DEFINE VARIABLE cErro           AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iMsg            AS INTEGER     NO-UNDO.
    
    //variaveis locais
    DEFINE VARIABLE aJsonArray              AS JsonArray  NO-UNDO.
    DEFINE VARIABLE cTransacao              AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE corpo                   AS jsonObject NO-UNDO.
    DEFINE VARIABLE jsonAux                 AS jsonObject NO-UNDO.
    DEFINE VARIABLE corpoArray              AS jsonArray  NO-UNDO.
    DEFINE VARIABLE principal               AS jsonObject NO-UNDO.

    DEFINE VARIABLE hBoIntCli     AS HANDLE     NO-UNDO.
        
    RUN esbo/boIntegrarCliente.p PERSIST SET hBoIntCli .
    RUN iniciarBos IN hBoIntCli .
                                 
    //pegar parametros querystring
    RUN setJsonInput(JsonInput).
    RUN getQueryParam('id_integracao',OUTPUT cTransacao).

    RUN setIdIntegracao     IN hBoIntCli(INPUT cTransacao).
    RUN setObjJson          IN hBoIntCli(INPUT JsonInput,
                                         INPUT 'payload').

    RUN integrar            IN hBoIntCli('dados').
    RUN getTTErroCliente    IN hBoIntCli(OUTPUT TABLE ttErroCliente).
    RUN finalizarBos        IN hBoIntCli.
    
    //RUN criarTTRetorno(cErro,1).
    //EMPTY TEMP-TABLE ttChave .
     /////aJsonArray = NEW jsonArray().
     //aJsonArray:ADD(principal).
     //aJsonArray:ADD(myLongChar).
     /////aJsonArray:ADD(jsonInput).

     //ASSIGN jsonOutput = JsonAPIResponseBuilder:ok(aJsonArray, false).
    //RETURN. 
    RUN retJsontt(INPUT-OUTPUT jsonOutput,TEMP-TABLE ttErroCliente:HANDLE).



END PROCEDURE.
/*
PROCEDURE incluirCliente2:
    //parametros obrigatorios de entrada e saida
    DEF INPUT PARAM jsonInput       AS JsonObject NO-UNDO.
    DEF OUTPUT PARAM jsonOutput     AS JsonObject NO-UNDO.
    
    
    //variaveis locais
    DEFINE VARIABLE aJsonArray              AS JsonArray  NO-UNDO.
    DEFINE VARIABLE cChave                  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE corpo                   AS jsonObject NO-UNDO.
    
        
    //pegar parametros querystring
    RUN setJsonInput(JsonInput).
    RUN getQueryParam('teste_01',OUTPUT cChave).
    RUN criarTTRetorno(cChave,0).

    //passagem de parametros gerais para a API
    //EMPTY TEMP-TABLE ttRetorno .
    //EMPTY TEMP-TABLE ttChave .
     

    RUN retJsontt(INPUT-OUTPUT jsonOutput,TEMP-TABLE ttRetorno:HANDLE).



END PROCEDURE.

PROCEDURE incluirCliente3:
    //parametros obrigatorios de entrada e saida
    DEF INPUT PARAM jsonInput       AS JsonObject NO-UNDO.
    DEF OUTPUT PARAM jsonOutput     AS JsonObject NO-UNDO.

    //variaveis locais
    DEFINE VARIABLE aJsonArray              AS JsonArray  NO-UNDO.
    aJsonArray = NEW jsonArray().
    aJsonArray:ADD(jsonInput).
END PROCEDURE.

*/





