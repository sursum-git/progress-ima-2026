/*********************************************************************************
arquivo:integra-pedido-web.p
objetivo: chama a api que faz a integra‡Æo de pedidos web 
passados por  body
data:10/2025
autor:Tadeu silva
**********************************************************************************/
{utp/ut-api.i}
{utp/ut-api-action.i piPrinc GET ~*}
{utp/ut-api-notfound.i}
{esp/util.i}
{esapi/analisarJsonObject2.i}


{method/dbotterr.i}

PROCEDURE piPrinc:

    DEFINE INPUT  PARAMETER jsonInput       AS JsonObject NO-UNDO.
    DEFINE OUTPUT PARAMETER jsonOutput      AS JsonObject NO-UNDO.
    
    
    
    DEFINE VARIABLE oJsonObject AS jsonObject  NO-UNDO.
    DEFINE VARIABLE aJsonArray  AS jsonArray   NO-UNDO.    
    DEFINE VARIABLE nrPedido    AS INTEGER     NO-UNDO.
    
    RUN esapi/analisarJsonObject3.p(jsonInput,OUTPUT TABLE ttJson) .
    
    ASSIGN nrPedido = INT(getChaveTTJsonTag('nr-pedido-web')).
    
    RUN esapi/integrarPedidosWeb.p(NO,YES,nrPedido). 
    
    ASSIGN jsonoutput = NEW jsonobject() .
    jsonoutput:ADD('retorno',"OK").
   
    {esp/retornarErrosSistemaApi.i}
    
    CATCH appError AS Progress.Lang.AppError:    
       IF NOT VALID-OBJECT(jsonOutput) THEN DO:
         jsonOutput = NEW jsonobject(). 
       END.
       ASSIGN cErro = appError:getMessage(1).
       jsonOutput:ADD('Erro',cErro).   
       
/*        IF nrPedido > 0 THEN                                                */
/*        DO:                                                                 */
/*             FOR FIRST peds_web                                             */
/*                 WHERE peds_web.ped_web_id = nrPedido EXCLUSIVE-LOCK:       */
/*                                                                            */
/*                 ASSIGN peds_web.ind_sit_ped_web = 5                        */
/*                        peds_web.descr_rejei     =  appError:getMessage(1). */
/*                                                                            */
/*             END.                                                           */
/*        END.                                                                */
      
     END CATCH.
  
    

END PROCEDURE.
