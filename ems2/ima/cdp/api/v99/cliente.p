
/*
programa: pdp/api/v1/histAvalPedVenda.p
objetivo: API a ser utilizada para troca de informa‡äes referente a avalia‡äes do pedido de venda.(tabela: hist_aval_ped_venda)
data: 10/2021

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


DEFINE TEMP-TABLE ttErroCliente
    FIELD codEmitente AS INT
    FIELD erro        AS CHAR FORMAT 'x(200)'.

DEFINE TEMP-TABLE ttAux LIKE ttEmitente.
DEFINE VARIABLE iUlt    AS INTEGER     NO-UNDO.
DEFINE VARIABLE cErro   AS CHARACTER   NO-UNDO.

//DEFINE TEMP-TABLE ttEmitente LIKE emitente.

PROCEDURE incluirCliente:
    //parametros obrigatorios de entrada e saida
    DEF INPUT PARAM jsonInput       AS JsonObject NO-UNDO. 
    DEF OUTPUT PARAM jsonOutput     AS JsonObject NO-UNDO.
    DEFINE VARIABLE cErro           AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iMsg            AS INTEGER     NO-UNDO.
    
    //variaveis locais
    DEFINE VARIABLE aJsonArray              AS JsonArray  NO-UNDO.
    DEFINE VARIABLE cChave                  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE corpo                   AS jsonObject NO-UNDO.
    DEFINE VARIABLE corpoArray              AS jsonArray NO-UNDO.
    DEFINE VARIABLE principal               AS jsonObject NO-UNDO.

    
        
    //pegar parametros querystring
    RUN setJsonInput(JsonInput).
   

    //RUN getPayLoad(OUTPUT corpo).
    
    /*principal = NEW jsonObject().
    principal = corpoArray:getJsonObject(1) NO-ERROR.*/


    //passagem de parametros gerais para a API
    RUN esapi/criarClienteConvJson.p(INPUT JsonInput, INPUT 'dados',OUTPUT TABLE  ttEmitente, OUTPUT TABLE  ttEmitenteExt, OUTPUT TABLE  ttAtividades).
    FOR EACH ttEmitente :
        DISP ttEmitente.cod-emitente.
        EMPTY TEMP-TABLE ttAux.
        CREATE ttAux.
        BUFFER-COPY ttEmitente TO ttAux.
        DISP ttAux.cod-emitente ttAux.nome-abrev.
        RUN esapi/criarCliente.p(INPUT TABLE ttAux,INPUT TABLE ttEmitenteExt,INPUT TABLE ttAtividades, OUTPUT TABLE rowErrors).
        FOR EACH rowErrors:
            ASSIGN cErro = string(rowErrors.ErrorSequence) + "-" + string(rowErrors.ErrorNumber) + "-" +  rowErrors.ErrorDescription + "-" +  
                IF rowErrors.ErrorParameters = ? THEN 'Sem Parametro' ELSE  rowErrors.ErrorParameters .
            RUN inserirErroCliente(ttEmitente.cod-emitente,cErro).
        END.
    END.
    //RUN criarTTRetorno(cErro,1).
    //EMPTY TEMP-TABLE ttChave .
     //aJsonArray = NEW jsonArray().
     //aJsonArray:ADD(principal).
     //aJsonArray:ADD(myLongChar).
     //aJsonArray:ADD(jsonInput).

     //ASSIGN jsonOutput = JsonAPIResponseBuilder:ok(aJsonArray, false).
     RUN retJsontt(INPUT-OUTPUT jsonOutput,TEMP-TABLE ttErroCliente:HANDLE).



END PROCEDURE.

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





