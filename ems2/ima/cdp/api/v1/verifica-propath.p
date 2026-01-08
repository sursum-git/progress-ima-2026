/*********************************************************************************
arquivo:verif-propath.p
objetivo: verifica se o programa passado por parametro eh encontrado no propath
data:10/2025
autor:Tadeu silva
**********************************************************************************/
{utp/ut-api.i}
{utp/ut-api-action.i piPrinc POST ~*}
{utp/ut-api-notfound.i}
{esp/util.i}
{esapi/analisarJsonObject2.i}


{method/dbotterr.i}

PROCEDURE piPrinc:

    DEFINE INPUT  PARAMETER jsonInput       AS JsonObject NO-UNDO.
    DEFINE OUTPUT PARAMETER jsonOutput      AS JsonObject NO-UNDO.
    
    DEFINE VARIABLE oJsonObject AS jsonObject  NO-UNDO.
    DEFINE VARIABLE aJsonArray  AS jsonArray   NO-UNDO.
   
    DEFINE VARIABLE cPrograma   AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cResultado  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cLog        AS CHARACTER   NO-UNDO.
    
    RUN esapi/analisarJsonObject2.p(jsonInput,OUTPUT TABLE ttJson) .
   
    FOR EACH ttJson:
        RUN incrvalor(INPUT-OUTPUT clog,ttJson.tag + ":" + ttJson.valor,'<br>'). 
    END.
    ASSIGN cprograma       = getChaveTTJsonTag('programa').
    ASSIGN cResultado      = replace(cPrograma,"\/","/").
    RUN incrvalor(INPUT-OUTPUT clog,"Programa:" + cPrograma,'<br>').
    ASSIGN cResultado      = SEARCH(cResultado).    
    
    IF cResultado = ? THEN
    DO:      
         ASSIGN cResultado = "Programa nao encontrado".        
    END.      
    jsonoutput = NEW jsonobject().        
    jsonOutput:ADD('log',cLog).   
    jsonOutput:ADD('propath',PROPATH).   
    jsonOutput:ADD('resultado',cResultado).   
    jsonOutput:ADD('parametros',jsonInput).
      
    /*CATCH eSysError AS Progress.Lang.SysError:
       jsonOutput:ADD('resultado', eSysError:GetMessage(1)).         
    END CATCH.*/
   
  
    

END PROCEDURE.
