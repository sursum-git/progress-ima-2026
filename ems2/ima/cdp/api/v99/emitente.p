                                                                             
{esp/utilAPi.i}
//{utp/ut-api-action.i    piCodigo    GET ~*}
{utp/ut-api-action.i    piCodigo    GET ~* cod_emit=*}
{utp/ut-api-action.i    piNomeAbrev GET ~* nome_abrev=*}
{utp/ut-api-notfound.i}
DEFINE TEMP-TABLE ttEmitente LIKE emitente.


PROCEDURE piCodigo:

    DEF INPUT PARAM jsonInput       AS JsonObject NO-UNDO.
    DEF OUTPUT PARAM jsonOutput     AS JsonObject NO-UNDO. 

    jsonOutput = NEW JsonObject().
    //RUN criarTTRetorno('cod_emit:' + string(iEmit) ,1).                       

    RUN getDadosCliente('cod_emit').

    RUN retJsontt(INPUT-OUTPUT jsonOutput, TEMP-TABLE ttEmitente:HANDLE ).

    //RUN retJsontt(INPUT-OUTPUT jsonOutput, TEMP-TABLE ttRetorno:HANDLE ).


END PROCEDURE.


PROCEDURE piNomeAbrev:

    DEF INPUT PARAM jsonInput       AS JsonObject NO-UNDO.
    DEF OUTPUT PARAM jsonOutput     AS JsonObject NO-UNDO.
    
    RUN getDadosCliente('nome_abrev').

    RUN retJsontt(INPUT-OUTPUT jsonOutput, TEMP-TABLE ttEmitente:HANDLE ).
    

END PROCEDURE.


PROCEDURE getDadosCliente:
    DEFINE INPUT  PARAMETER chaveBusca AS CHARACTER   NO-UNDO.

    DEFINE VARIABLE cRetorno        AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iEmit           AS INT         NO-UNDO.
    DEFINE VARIABLE hBoEmitente     AS HANDLE      NO-UNDO.

    RUN setJsonInput(JsonInput).                                            
    RUN getQueryParam(chaveBusca,OUTPUT cRetorno).
    RUN esbo/boEmitente.p PERSISTENT SET hBoEmitente.
    RUN iniciarBos IN hBoEmitente.
    RUN finalizarBos IN hBoEmitente.

    IF chaveBusca = 'cod_emit' THEN DO:
       ASSIGN iEmit = INT(cRetorno). 
       RUN setCodEmitente IN hBoEmitente(iEmit).
    END.
    ELSE DO:
        RUN setNomeAbrev IN hBoEmitente(cRetorno).
    END.
       
    RUN getRegEmit IN hBoEmitente(OUTPUT TABLE ttEmitente).
    



END PROCEDURE.


/*PROCEDURE getRegUnicoGET:

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
*/







