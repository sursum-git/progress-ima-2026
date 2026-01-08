                                                                             
{esp/utilAPi.i}
{utp/ut-api-action.i    pi   POST ~*}
{utp/ut-api-notfound.i}
DEFINE TEMP-TABLE ttLog   LIKE logs_calculos.
DEFINE TEMP-TABLE ttTrans LIKE transacoes.
{esapi/analisarJsonObject2.i}


PROCEDURE pi:

    DEF INPUT PARAM jsonInput       AS JsonObject NO-UNDO.
    DEF OUTPUT PARAM jsonOutput     AS JsonObject NO-UNDO. 
    
    RUN esapi/analisarJsonObject2.p(INPUT jsonInput , OUTPUT TABLE ttJson).
    
    OUTPUT TO c:\temp\testeAPI.txt.
        FOR EACH ttJson:
            EXPORT DELIMITER "|" ttJson.
        END.
    OUTPUT CLOSE.



   


END PROCEDURE.









