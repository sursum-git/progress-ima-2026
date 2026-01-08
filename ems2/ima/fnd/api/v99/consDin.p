/***************************************************************************
programa: consDin
objetivo: Gerar o retorno de dados no formato json a partir de parametros
recebidos via body que far∆o consultas dinamicas no banco de dados.
data: 10/2021
****************************************************************************/
{esp/utilAPi.i}


DEFINE VARIABLE  cTabelas    AS CHARACTER   NO-UNDO FORMAT 'x(200)'.
DEFINE VARIABLE  cCampos     AS CHARACTER   NO-UNDO FORMAT 'x(2000)'.
DEFINE VARIABLE  cCondicao   AS CHARACTER   NO-UNDO FORMAT 'x(16000)'.
DEFINE VARIABLE  cInner      AS CHARACTER   NO-UNDO FORMAT 'x(8000)'.
DEFINE VARIABLE  hResult     AS HANDLE      NO-UNDO.



{utp/ut-api-action.i    getRegUnicoPOST POST ~* log_reg_unicio=1}
{utp/ut-api-action.i    getRegUnicoPOST POST ~* }
{utp/ut-api-action.i    getRegUnicoPOST POST }
{utp/ut-api-action.i    getRegUnicoPOST POST teste=1}
{utp/ut-api-action.i    getRegUnicoGET GET ~* }
/*{utp/ut-api-action.i getUltDp            GET ~* log_ult_dp_venc=1}
{utp/ut-api-action.i getTodasDp          GET ~* log_todas_dp_aberto=1}
{utp/ut-api-action.i getTodasDp          GET ~* log_todas_dp_venc=1}
{utp/ut-api-action.i getTodasDpNF        GET ~* log_todas_dp_nf=1}   */
{utp/ut-api-notfound.i}


PROCEDURE getRegUnicoPOST:

    //parametros obrigatorios de entrada e saida
    DEF INPUT PARAM jsonInput       AS JsonObject NO-UNDO.
    DEF OUTPUT PARAM jsonOutput     AS JsonObject NO-UNDO.


    RUN getTTParam(jsonInput). 
    RUN criarTTRetorno('tabelas:' + cTabelas,1).
    RUN retJsontt(INPUT-OUTPUT jsonOutput). 


END PROCEDURE.


PROCEDURE getRegUnicoGET:

    //parametros obrigatorios de entrada e saida
    DEF INPUT PARAM jsonInput       AS JsonObject NO-UNDO.
    DEF OUTPUT PARAM jsonOutput     AS JsonObject NO-UNDO.
    
    jsonOutput = NEW JsonObject().
 
    //pegar parametros querystring                                           
     RUN setJsonInput(JsonInput).                                            
     RUN getQueryParam('tabelas',OUTPUT cTabelas).                           
     RUN getQueryParam('campos',OUTPUT cCampos).                             
     RUN getQueryParam('condicao',OUTPUT cCondicao).                         
     RUN getQueryParam('inner',OUTPUT cInner).                               
                                                                             
     //executa a query dinamica e retorna o handle da tabela com os dados    
     RUN getDados(OUTPUT jsonOutput).                                                           
                                                                             
     //RUN criarTTRetorno('condicoes:' + cCondicao,1).                       
                                                                             
     //transforma a tabela temporaria em json                                
     //RUN retJsontt(INPUT-OUTPUT jsonOutput, hResult).                        
    
END PROCEDURE.

PROCEDURE getDados:
    
    DEFINE OUTPUT PARAMETER pObJ AS jsonObject   NO-UNDO.
    DEFINE VARIABLE hBoConsDin  AS HANDLE     NO-UNDO.
    RUN esbo/boConsDin.p PERSISTENT SET hBoConsDin.
    RUN iniciarBos IN hBoConsDin.
    RUN setDadosConsulta IN  hBoConsDin(cTabelas,cCampos,cCondicao,cInner).
    RUN execConsulta IN hBoConsDin('buffer').
    //RUN getHandleResult IN  hBoConsDin(OUTPUT hResult).
    RUN getJsonObject IN hBoConsDin(OUTPUT pObj ).
    
    IF VALID-HANDLE(hboConsdin) THEN DO:
       RUN finalizarBos IN hBoConsDin.
       DELETE PROCEDURE hBoConsDin.
    END.
    
END PROCEDURE.



PROCEDURE getTTParam:
    DEFINE INPUT  PARAMETER pJsonInput  AS JsonObject  NO-UNDO.
    DEFINE VARIABLE corpo               AS jsonObject   NO-UNDO.
    DEFINE VARIABLE corpottParam        AS jsonObject   NO-UNDO.
   // DEFINE VARIABLE aCorpo  AS jsonArray   NO-UNDO.
    DEFINE VARIABLE hTtParam            AS HANDLE      NO-UNDO.
    DEFINE VARIABLE oRequestParser      AS JsonAPIRequestParser NO-UNDO.
    oRequestParser = NEW JsonAPIRequestParser(pJsonInput).
    corpo = NEW jsonObject().
    corpo = oRequestParser:getPayload().
    corpoTTParam    = corpo:getjsonObject('ttParam').
    cTabelas        = corpoTTParam:GetCharacter('tabelas').
    cCampos         = corpoTTParam:GetCharacter('campos').
    cCondicao       = corpoTTParam:GetCharacter('condicao').
    cInner          = corpoTTParam:GetCharacter('inner').

    /*aCorpo = NEW jsonArray().
    aCorpo = corpo:getJsonArray('ttParam').*/

END PROCEDURE.






