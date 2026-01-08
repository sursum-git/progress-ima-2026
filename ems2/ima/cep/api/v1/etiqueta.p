/*
programa: cep/v1/etiqueta.p
objetivo: API a ser utilizada para atualiza‡Æo das quantidades das etiquetas.
data: 12/2022                                                               
*/


{esp/utilAPI.i} //include especifica que inclui a utp/ut-api.i dentro dela
{utp/ut-api-action.i  atualizarEtiqueta     POST ~* }
{esapi/analisarJsonObject2.i}

DEFINE STREAM s1.

PROCEDURE atualizarEtiqueta:

    DEFINE VARIABLE cNomeArq    AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cArqCriado  AS CHARACTER   NO-UNDO.
    

    //parametros obrigatorios de entrada e saida
    DEFINE INPUT PARAM jsonInput       AS JsonObject NO-UNDO. 
    DEFINE OUTPUT PARAM jsonOutput     AS JsonObject NO-UNDO.
    DEFINE VARIABLE aJsonArray      AS JsonArray  NO-UNDO.
    DEFINE VARIABLE idTransacao     AS INT64      NO-UNDO.
    DEFINE VARIABLE cChave          AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cAcao           AS CHARACTER   NO-UNDO.
    
    aJsonArray = NEW jsonArray().
    aJsonArray:ADD(jsonInput).
    RUN esapi/analisarJsonObject2.p(INPUT jsonInput , OUTPUT TABLE ttJson).

    OUTPUT STREAM s1 TO value(SESSION:TEMP-DIRECTORY + 'logexecetiqueta-' + string(RANDOM(1,100000)) + STRING(TIME) + '.txt').
    FOR EACH ttJson:
            EXPORT STREAM s1 DELIMITER "|" ttJson.
    END.
    
    ASSIGN cChave = 'SEM-CONTAINER'.
    FIND LAST ttJson
        WHERE ttJson.tag = 'cntr'
        NO-ERROR.
    IF AVAIL ttJson THEN
    DO:
       ASSIGN cChave = ttJson.valor. 
    END.

    FIND LAST ttJson
        WHERE ttjson.tag = 'acao' NO-ERROR.
    IF AVAIL ttJson THEN DO:
       ASSIGN cAcao = ttJson.valor.
    END.

    IF cAcao <> '' THEN
    DO:
        ASSIGN cChave = cChave + "-" +
                        cAcao.
    END.
    
    RUN esapi/criarLogAPIIMA2.p( 
        INPUT 'cep_api_v1_etiqueta',
        INPUT 55,
        INPUT cChave,
        INPUT jsonInput,
        OUTPUT cArqCriado,
        OUTPUT idTransacao
        ).                            

    

    
   IF cAcao = 'corte' THEN DO:
      PUT STREAM s1 "acao igual a corte" SKIP.
      ASSIGN cNomeArq    = 'ttJsonEtiqCorte_' .
     /* comentado em 16/10/25 pois agora o corte ‚ pela separacao
     RUN esapi/retorno-cortes-lisa-forcado.p (INPUT TABLE ttJson,
                                       OUTPUT TABLE ttChave).*/
   END.
   ELSE DO:
      PUT "acao DIFERENTE  DE corte" SKIP.
      ASSIGN cNomeArq    = 'ttJsonEtiqAjusteContainer_' .
      RUN esapi/retorno-romaneio-lisa.p (INPUT TABLE ttJson,
                                       OUTPUT TABLE ttChave).
   END.                                                      


    OUTPUT STREAM s1 CLOSE.

    OUTPUT TO value(SESSION:TEMP-DIRECTORY + cNomeArq + cChave + '.txt').
        FOR EACH ttJson:
            EXPORT DELIMITER "|" ttJson.
        END.
    OUTPUT CLOSE.    
    
    
    ASSIGN jsonOutput = JsonAPIResponseBuilder:ok(aJsonArray, false).
    //RUN retJsontt(INPUT-OUTPUT jsonOutput,TEMP-TABLE ttRetorno:HANDLE).

    //jsonOutput:WriteFile('c:\temp\log_api_etiqueta_' + STRING(TIME) + '.txt').
END PROCEDURE.
