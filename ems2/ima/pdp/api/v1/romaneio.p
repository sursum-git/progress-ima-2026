/*
programa: pdp/v1/romaneio.p
objetivo: API a ser utilizada para gerar o Romaneio e enviar por API.
data: 05/2023                                                               
*/


{esp/utilAPI.i} //include especifica que inclui a utp/ut-api.i dentro dela
{utp/ut-api-action.i  piPrinc     GET ~* }
{esapi/analisarJsonObject2.i}
//{esp/ttChave.i}
{esp/util.i}
{esp/utiljson.i}



PROCEDURE piPrinc:

    //parametros obrigatorios de entrada e saida
    DEF INPUT PARAM jsonInput           AS JsonObject NO-UNDO. 
    DEF OUTPUT PARAM jsonOutput         AS JsonObject NO-UNDO.

    DEFINE VARIABLE aJsonArray          AS JsonArray  NO-UNDO.
    DEFINE VARIABLE tipoRet             AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE oJsonObject         AS JsonObject NO-UNDO.
    DEFINE VARIABLE cArqCriado          AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE nrNotaFis           AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE serie               AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE codEstabel          AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE nrPedido            AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE lcPDF               AS LONGCHAR    NO-UNDO.
    DEFINE VARIABLE cArqPDF             AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cret                AS CHARACTER   NO-UNDO.



    aJsonArray = NEW jsonArray().
    jsonOutput = NEW jsonobject().
    
    jsoninput:writeFile('c:\temp\romaneio_' + STRING(TIME) + ".json").

    RUN esapi/analisarJsonObject2.p(INPUT jsonInput , OUTPUT TABLE ttJson).

    ASSIGN codEstabel= getChaveTTJson('','cod_estabel')
           nrNotaFis = getChaveTTJson('','nr_nota_fis')
           serie     = getChaveTTJson('','serie')
           nrPedido  = getChaveTTJson('','nr_pedido') 
           .   
     
    /*oJsonObject = NEW JSONObject().
    oJsonObject:ADD('romaneio','thiago').
    ajsonArray:ADD(oJsonObject).
    jsonOutput = JsonAPIResponseBuilder:ok(aJsonArray, false).*/
    //RUN saveJson2File(jsonOutput,'c:\temp\aaa.json').

   RUN esapi\dadosRomaneioExcel2.p(codEstabel,serie,nrNotaFis,false,false,OUTPUT cArqPDF) NO-ERROR.
  /* ASSIGN cRet =  RETURN-VALUE .

   oJsonObject = NEW JSONObject().
   IF SEARCH(cArqPDF) = ? OR cRet = 'nok' THEN DO:
      oJsonObject:Add("ErrorNumber", 1).
      oJsonObject:Add("ErrorDescription", "Arquivo PDF NÆo Gerado").
      oJsonObject:Add("ErrorSubType", "error").
      ajsonArray:ADD(oJsonObject).
      jsonOutput = JsonAPIResponseBuilder:asError(aJsonArray,401).
   END.
   ELSE DO:
       RUN encodeFile2LongChar(cArqPDF,'',OUTPUT lcPDF).
       oJsonObject:ADD('romaneio',lcPDF).
       ajsonArray:ADD(oJsonObject).
       jsonOutput = JsonAPIResponseBuilder:ok(aJsonArray, false).
   END.


   CATCH oError AS Progress.Lang.Error : 
    jsonOutput:ADD('status',402).
    jsonOutput:ADD('erro Progress',oError:GetMessage(1) + '-' + oError:CallStack).
    OUTPUT TO value('c:\temp\tst\ERRO_romaneio_' + STRING(TIME) + ".txt").
          PUT UNFORM oError:GetMessage(1) SKIP(2)
            oError:CallStack .
        
    OUTPUT CLOSE.
    END CATCH.*/
   

   
   
   
      
   




END PROCEDURE.




