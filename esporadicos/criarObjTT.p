{esp/USING_json.i}

DEFINE INPUT  PARAMETER pTT AS HANDLE      NO-UNDO.
DEFINE OUTPUT PARAMETER obj AS jsonObject  NO-UNDO.
obj = NEW jsonobject().
pTT:WRITE-JSON(  "JsonObject",
                obj).




