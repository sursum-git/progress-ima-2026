BLOCK-LEVEL ON ERROR UNDO, THROW.
USING com.totvs.framework.api.JsonAPIUtils.
USING fwk.utils.CustomError.
USING fwk.utils.*.

{utp/ut-api.i}
{utp/ut-api-utils.i}

{fwk/utils/fndApiServices.i}

{include/i-prgvrs.i type 1.00.00.000}

{utp/ut-api-action.i piDefault POST /default/ }
{utp/ut-api-action.i piCustom  POST /custom/ }
{utp/ut-api-action.i piCustom  POST ~* }
{utp/ut-api-notfound.i}
{esp/util.i}
{esapi/analisarJsonObject2.i}
{esapi/getDevolVenda.i}

PROCEDURE piDefault:

    DEFINE INPUT  PARAMETER jsonInput  AS JSONObject NO-UNDO.
    DEFINE OUTPUT PARAMETER jsonOutput AS JSONObject NO-UNDO.

    DEFINE VARIABLE oResponse   AS JsonAPIResponse NO-UNDO.
    DEFINE VARIABLE oPayload    AS JSONObject      NO-UNDO.
    DEFINE VARIABLE codEstabel  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE dtEmisIni   AS DATE        NO-UNDO.
    
    jsonOutput = NEW jsonObject().
    
    RUN esapi/analisarJsonObject3.p(jsonInput,OUTPUT TABLE ttJson) .

    RUN _atribuirFiltros(OUTPUT codEstabel, OUTPUT dtEmisIni).
    
    IF CAN-FIND( FIRST rowErrors) THEN DO:
       jsonOutput = JsonAPIResponseBuilder:asError(TEMP-TABLE RowErrors:HANDLE).       
    END.  
    ELSE DO:
        RUN esapi/getDevolVenda.p(INPUT codEstabel,INPUT dtEmisIni,OUTPUT TABLE ttResult). 
        ASSIGN oPayload = JsonAPIUtils:convertTempTableToJsonObject(TEMP-TABLE ttResult:HANDLE).
        ASSIGN oResponse = NEW JsonAPIResponse(oPayload).
        ASSIGN jsonOutput = oResponse:createJsonResponse(). 
        //ASSIGN oPayload = NEW JsonObject().
        //oPayload:ADD("response", "OK").
        ASSIGN oResponse = NEW JsonAPIResponse(oPayload).
        ASSIGN jsonOutput = oResponse:createJsonResponse().
    END.
    
    
     
    

    

END PROCEDURE.

PROCEDURE piCustom:
    DEFINE INPUT  PARAMETER jsonInput  AS JSONObject NO-UNDO.
    DEFINE OUTPUT PARAMETER jsonOutput AS JSONObject NO-UNDO.

    DEFINE VARIABLE oResponse AS JsonAPIResponse NO-UNDO.
    DEFINE VARIABLE oPayload  AS JSONObject      NO-UNDO.
    DEFINE VARIABLE oHeaders  AS JSONObject      NO-UNDO.

    ASSIGN oPayload = NEW JsonObject().
    oPayload:ADD("response", "OK").

    ASSIGN oResponse = NEW JsonAPIResponse(oPayload).
    ASSIGN jsonOutput = oResponse:createJsonResponse().

    ASSIGN oHeaders = NEW JsonObject().
    oHeaders:ADD("Content-Type", "application/custom.content.type+json").
    jsonOutput:ADD("headers", oHeaders).

END PROCEDURE.




PROCEDURE _atribuirFiltros:

    DEFINE OUTPUT PARAMETER cEstab AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER dtEmis AS DATE        NO-UNDO.

    FIND LAST ttJson 
    WHERE ttJson.tag = 'estab' NO-ERROR.
    IF AVAIL ttJson THEN DO:
        ASSIGN cEstab = ttJson.valor.        
    END.
    ELSE DO:
        RUN _criarErro(1,"Estabelecimento N∆o Informado").
    END.

    FIND LAST ttJson 
    WHERE ttJson.tag = 'dt_ini' NO-ERROR.
    IF AVAIL ttJson THEN DO:
       RUN convDtApi(ttJson.valor, OUTPUT dtEmis) .        
    END.
    ELSE DO:
        RUN _criarErro(1,"Dt.Emiss∆o Inicial N∆o Informada").
    END.


END PROCEDURE.

PROCEDURE _criarErro:
    DEFINE INPUT  PARAMETER pCodigo AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER pDescr  AS CHARACTER   NO-UNDO.

     CREATE RowErrors.
     ASSIGN RowErrors.ErrorNumber      = pCodigo
            RowErrors.ErrorDescription = pDescr
            RowErrors.ErrorSubType     = "ERROR".
            


END PROCEDURE.

