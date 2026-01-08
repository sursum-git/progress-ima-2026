
{utp/ut-api.i}
// {utp/ut-api-action.i pi-send    GET /~*/SEND by=email,address=~* }
// {utp/ut-api-action.i pi-update  POST /~* }
//{utp/ut-api-action.i pi-find    GET /~* }
//{utp/ut-api-action.i pi-default GET }
{utp/ut-api-action.i piTeste   GET /~* }
{utp/ut-api-notfound.i}

PROCEDURE piTeste:

    DEF INPUT PARAM jsonInput AS JsonObject NO-UNDO.
    DEF OUTPUT PARAM jsonOutput AS JsonObject NO-UNDO.
    DEFINE VARIABLE cValor AS CHARACTER   NO-UNDO.

    DEF VAR oJsonObject AS jsonobject NO-UNDO.
    DEF VAR oJsonArray AS jsonArray   NO-UNDO.
    //ASSIGN cValor = jsonInput:getJsonArray('pathParams'):getCharacter(1).
    oJsonObject = NEW JSONObject().
    oJsonObject:ADD('parametro','TESTE').
    ojsonArray:ADD(oJsonObject).
    jsonOutput = JsonAPIResponseBuilder:ok(oJsonArray, false).



END PROCEDURE.

