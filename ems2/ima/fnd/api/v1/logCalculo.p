                                                                             
{esp/utilAPi.i}
{utp/ut-api-action.i    piGetLogs   GET ~*}
{utp/ut-api-notfound.i}
DEFINE TEMP-TABLE ttLog   LIKE logs_calculos.
DEFINE TEMP-TABLE ttTrans LIKE transacoes.


PROCEDURE piGetLogs:

    DEF INPUT PARAM jsonInput       AS JsonObject NO-UNDO.
    DEF OUTPUT PARAM jsonOutput     AS JsonObject NO-UNDO. 
    DEFINE VARIABLE cRetorno        AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cChave          AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iCalculo        AS INTEGER     NO-UNDO.
    DEFINE VARIABLE hBoLogsCalculos AS HANDLE      NO-UNDO.
    jsonOutput = NEW JsonObject().

    RUN setJsonInput(JsonInput).
    RUN getQueryParam('chave',OUTPUT cChave).
    RUN getQueryParam('calculo',OUTPUT cRetorno).
    RUN criarTTRetorno('chave:' + cChave + " - tipo:" + cRetorno,1).
    ASSIGN iCalculo = INT(cRetorno).
    IF cChave <> '' AND iCalculo <> 0 THEN DO:
        RUN esbo/boLogsCalculos.p PERSISTENT SET hBoLogsCalculos.
        RUN iniciarBos IN hBoLogsCalculos.
        RUN setCalculo IN hBoLogsCalculos(iCalculo).
        RUN getLogsUltTransPorChave IN hBoLogsCalculos(cChave, OUTPUT TABLE ttTrans, OUTPUT TABLE ttLog).
        RUN finalizarBos IN hBoLogsCalculos.
        IF VALID-HANDLE(hBoLogsCalculos) THEN
           DELETE PROCEDURE hBoLogsCalculos.
        RUN retJsontt(INPUT-OUTPUT jsonOutput, TEMP-TABLE ttLog:HANDLE ).
    END.
    ELSE  DO:
        RUN criarTTRetorno('‚ obrigatorio passar a chave e o tipo',1).
        RUN retJsontt(INPUT-OUTPUT jsonOutput, TEMP-TABLE ttRetorno:HANDLE ).
    END.
    
    //RUN retJsontt(INPUT-OUTPUT jsonOutput, TEMP-TABLE ttRetorno:HANDLE ).


   


END PROCEDURE.









