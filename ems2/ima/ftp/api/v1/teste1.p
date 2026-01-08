{utp/ut-api.i}
{utp/ut-api-action.i pi-send    GET /~*/SEND by=email,address=~* }
{utp/ut-api-action.i pi-update  POST /~* }
{utp/ut-api-action.i pi-find    GET /~* }
{utp/ut-api-action.i pi-default GET }
{utp/ut-api-notfound.i}
   
PROCEDURE pi-send:
    DEF INPUT PARAM jsonInput AS JsonObject NO-UNDO.
    DEF OUTPUT PARAM jsonOutput AS JsonObject NO-UNDO.
      
    DEFINE VARIABLE aJsonArray  AS JsonArray  NO-UNDO.
    DEFINE VARIABLE oJsonObject AS JsonObject NO-UNDO.
  
    aJsonArray = NEW JSONArray().
  
    oJsonObject = NEW JSONObject().
    oJsonObject:ADD("teste", "teste").
    oJsonObject:ADD("teste1", "teste1").
    oJsonObject:ADD("teste2", "teste2").
    aJsonArray:ADD(oJsonObject).
  
    oJsonObject = NEW JSONObject().
    oJsonObject:ADD("teste", "teste").
    oJsonObject:ADD("teste1", "teste1").
    oJsonObject:ADD("teste2", "teste2").
    aJsonArray:ADD(oJsonObject).
  
    jsonOutput = JsonAPIResponseBuilder:ok(aJsonArray, false).
END.
   
PROCEDURE pi-update:
    DEF INPUT PARAM jsonInput AS JsonObject NO-UNDO.
    DEF OUTPUT PARAM jsonOutput AS JsonObject NO-UNDO.
 
    jsonOutput = NEW JSONObject().
    jsonOutput = jsonInput.
END.
  
PROCEDURE pi-find:
    DEF INPUT PARAM jsonInput AS JsonObject NO-UNDO.
    DEF OUTPUT PARAM jsonOutput AS JsonObject NO-UNDO.
     
    jsonOutput = NEW JSONObject().
    jsonOutput:ADD("method", "GET").
    jsonOutput:ADD("procedure", "pi-find").
    jsonOutput:ADD("description", "Test").
END.
 
PROCEDURE pi-default:
    DEF INPUT PARAM jsonInput AS JsonObject NO-UNDO.
    DEF OUTPUT PARAM jsonOutput AS JsonObject NO-UNDO.
     
    jsonOutput = NEW JSONObject().
    jsonOutput:ADD("method", "GET").
    jsonOutput:ADD("procedure", "pi-default").
    jsonOutput:ADD("description", "Test").
END.
