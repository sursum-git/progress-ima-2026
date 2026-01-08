/*
programa: rep/v1/notaretorno.p
objetivo: API a ser utilizada para recep‡Æo das notas fiscais de retorno
data: 04/2023  
03/2024 - tadeu - alterado para apenas salvar o arquivo json para posterior processamento                                                             
*/


{esp/utilAPI.i} //include especifica que inclui a utp/ut-api.i dentro dela
{utp/ut-api-action.i  getNotaRetorno     POST ~* }
{esapi/analisarJsonObject2.i}



PROCEDURE getNotaRetorno:

    //parametros obrigatorios de entrada e saida
    DEF INPUT PARAM jsonInput       AS JsonObject NO-UNDO. 
    DEF OUTPUT PARAM jsonOutput     AS JsonObject NO-UNDO.
    DEFINE VARIABLE aJsonArray      AS JsonArray  NO-UNDO.
    DEFINE VARIABLE cArqLog         AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE logJaProcessado AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE idTransacao     AS INT64      NO-UNDO.
    DEFINE VARIABLE notaRetorno     AS CHARACTER   NO-UNDO.
    //DEFINE VARIABLE iRand AS INTEGER     NO-UNDO.
    aJsonArray = NEW jsonArray().
    aJsonArray:ADD(jsonInput).
    /*OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY + 'testenfret.txt').
    PUT UNFORM   SEARCH('esapi/criarLogAPIIma2.p') SKIP.
    
    OUTPUT CLOSE.*/
    RUN esapi/analisarJsonObject3.p(jsonInput, OUTPUT TABLE ttJson).
    FIND FIRST ttJson
    WHERE ttJson.tag_pai = 'notasRetorno'
    AND   ttJson.tag     = 'nota' NO-ERROR.
    IF AVAIL ttJson THEN  DO:
        ASSIGN notaRetorno = ttJson.valor.        
    END.
    
    RUN esapi/criarLogAPIIma3.p( 
       INPUT 'rep_api_v1_notaretorno',
       INPUT 56,
       INPUT notaRetorno,
       INPUT jsonInput,
       OUTPUT cArqLog,
       OUTPUT idTransacao 
       ).
    ASSIGN jsonOutput = JsonAPIResponseBuilder:EMPTY().
    //jsonOutput:WriteFile('c:\temp\log_api_notaRetorno_' + STRING(TIME) + '.txt').

END PROCEDURE.
