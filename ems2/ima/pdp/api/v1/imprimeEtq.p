/*
programa: rep/v1/imprimeEtq.p
objetivo: API a ser utilizada para impress∆o de etiqueta
data: 03/2024
*/


{esp/utilAPI.i} //include especifica que inclui a utp/ut-api.i dentro dela
{utp/ut-api-action.i  imprimirEtq     POST ~* }
{esapi/analisarJsonObject2.i}
{esp/util.i}
{utp/ut-glob.i}

PROCEDURE imprimirEtq:

    //parametros obrigatorios de entrada e saida
    DEF INPUT PARAM jsonInput       AS JsonObject NO-UNDO. 
    DEF OUTPUT PARAM jsonOutput     AS JsonObject NO-UNDO.
    DEFINE VARIABLE aJsonArray      AS JsonArray  NO-UNDO.
    DEFINE VARIABLE cArqLog         AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE logJaProcessado AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE idTransacao     AS INT64      NO-UNDO.
    DEFINE VARIABLE cEstab          AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE numEtq          AS INT        NO-UNDO.
    DEFINE VARIABLE hLog            AS HANDLE     NO-UNDO.
    DEFINE VARIABLE iCalc           AS INTEGER    NO-UNDO.
    ASSIGN iCalc = 68.
    RUN esbo/bomsg.p PERSIST SET hLog.

    aJsonArray = NEW jsonArray().
    aJsonArray:ADD(jsonInput).

    RUN esapi/criarLogAPIIMA2.p( 
       INPUT 'rep_api_imprime_etq',
       INPUT iCalc,
       INPUT 'api_ima_imprime_etq_',
       INPUT jsonInput,
       OUTPUT cArqLog,
       OUTPUT idTransacao 
       ).

    
    RUN esapi/analisarJsonObject2.p(INPUT jsonInput , OUTPUT TABLE ttJson).
    {esp/exportarTabelacsv3.i ttJson " " " "  "ttjsonImpEtq" }
    ASSIGN c-seg-usuario      = getChaveTTJson('payload','usuario')
           v_cod_usuar_corren = c-seg-usuario
           .
    /*OUTPUT TO value('c:\temp\usuario_etq.txt').
        
        PUT UNFORMAT c-seg-usuario SKIP.
        PUT UNFORMAT v_cod_usuar_corren SKIP. 
        PUT UNFORMAT getChaveTTJson('payload','usuario') SKIP.

    OUTPUT CLOSE.*/

    RUN geTtNos('etqs','','json',OUTPUT TABLE ttNos).
    FOR EACH ttNos:
        FOR EACH ttJson
            WHERE ttjson.agrupJson = ttNos.id .
            IF ttJson.tag = 'numero' THEN
               ASSIGN numEtq = int(ttJson.valor).
            IF ttJson.tag = 'estab' THEN
               ASSIGN cEstab = ttJson.valor.
        END.
        RUN esapi/imp-etq-estoque-2.p(INPUT cEstab,
                                      INPUT numEtq,
                                      INPUT FALSE,
                                      INPUT hLog).

        RUN setTransacaoLogCalculo  IN hLog(idTransacao).
        RUN gravarLogCalculo        IN hLog(iCalc).
    END.

    

   
    ASSIGN jsonOutput = JsonAPIResponseBuilder:ok(aJsonArray, false).
    jsonOutput:WriteFile( SESSION:TEMP-DIRECTORY + 'log_api_imprimeEtq_' + STRING(TIME) + '.txt').

END PROCEDURE.
