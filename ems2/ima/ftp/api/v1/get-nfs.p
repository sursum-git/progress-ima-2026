/*********************************************************************************
arquivo:get-nfs.p
objetivo: retorna Dados das notas fiscais conforme filtros 
passados por  body
data:10/2025
autor:Tadeu silva
**********************************************************************************/
{utp/ut-api.i}
{utp/ut-api-action.i piPrinc POST ~*}
{utp/ut-api-notfound.i}
{esp/util.i}
{esapi/analisarJsonObject2.i}
{esapi/getNfsPortal.i}

{method/dbotterr.i}

PROCEDURE piPrinc:

    DEFINE INPUT  PARAMETER jsonInput       AS JsonObject NO-UNDO.
    DEFINE OUTPUT PARAMETER jsonOutput      AS JsonObject NO-UNDO.
    
    
    
    DEFINE VARIABLE oJsonObject AS jsonObject  NO-UNDO.
    DEFINE VARIABLE aJsonArray  AS jsonArray   NO-UNDO.
    
    RUN esapi/analisarJsonObject3.p(jsonInput,OUTPUT TABLE ttJson) .
    RUN esapi/getNfsPortal.p(INPUT TABLE ttJson,
                             OUTPUT TABLE ttNF).   
    ASSIGN jsonoutput = JsonAPIUtils:convertTempTableToJsonObject(TEMP-TABLE ttNf:HANDLE).
   
    /*jsonoutput = NEW jsonobject().            
    jsonOutput:ADD('propath',PROPATH).   
    jsonOutput:ADD('resultado',cResultado).
    jsonOutput:ADD('parametros',jsonInput).*/
      
    /*CATCH eSysError AS Progress.Lang.SysError:
       jsonOutput:ADD('resultado', eSysError:GetMessage(1)).         
    END CATCH.*/
   
  
    

END PROCEDURE.
