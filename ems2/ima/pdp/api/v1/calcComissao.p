
/*****************************************************************************************************************************************
programa: pdp/api/v1/calcComissao.p
objetivo: calculo de comiss∆o
data: 

****************************************************************************************************************************************/

{esp/utilAPI.i} //include especifica que inclui a utp/ut-api.i dentro dela

{utp/ut-api-action.i  calcComissao    GET ~* ped_web_id=~*}
/*{utp/ut-api-action.i getUltDp            GET ~* log_ult_dp_venc=1}
{utp/ut-api-action.i getTodasDp          GET ~* log_todas_dp_aberto=1}
{utp/ut-api-action.i getTodasDp          GET ~* log_todas_dp_venc=1}
{utp/ut-api-action.i getTodasDpNF        GET ~* log_todas_dp_nf=1}   */
{utp/ut-api-notfound.i}


PROCEDURE calcComissao :
    //parametros obrigatorios de entrada e saida
    DEF INPUT PARAM jsonInput       AS JsonObject NO-UNDO.
    DEF OUTPUT PARAM jsonOutput     AS JsonObject NO-UNDO.
    
    
    //variaveis locais
    DEFINE VARIABLE pedWebId        AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cRetorno        AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE idtransacao     AS INTEGER     NO-UNDO.
    DEFINE VARIABLE codUsuario      AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE lAtualiza       AS LOGICAL     NO-UNDO.
    DEFINE VARIABLE codPrograma     AS CHARACTER   NO-UNDO.


        
    //pegar parametros querystring
    RUN setJsonInput(JsonInput).
    RUN getQueryParam('ped_web_id', OUTPUT cRetorno).
    ASSIGN pedWebId = INT(cRetorno).
    RUN getQueryParam('id_transacao',OUTPUT cRetorno).
    ASSIGN idTransacao = INT(cRetorno) .
    RUN getQueryParam('cod_usuario',OUTPUT codUsuario).
    RUN getQueryParam('log_atualiza',OUTPUT cRetorno).
    ASSIGN lAtualiza = IF cRetorno = '1' THEN TRUE ELSE FALSE.
    RUN getQueryParam('cod_programa',OUTPUT codPrograma).
    


    //passagem de parametros gerais para a API
    EMPTY TEMP-TABLE ttRetorno .
    EMPTY TEMP-TABLE ttChave .
    RUN esapi/calcComisPedWeb.p(INPUT pedWebId,
                                INPUT idTransacao,
                                INPUT codUsuario,
                                INPUT lAtualiza, //se for sim atualiza os percentuais de comiss∆o na tabela peds_web, sen∆o apenas retorna os valores sem atualizar
                                INPUT codPrograma,
                                OUTPUT TABLE ttRetorno,
                                OUTPUT TABLE ttChave
                              ) .

    //RUN criarTTRetorno('teste',1) .
    //acrescenta a temp-table ttRetorno ao retorno da requisiá∆o   
    RUN retJsontt(INPUT-OUTPUT jsonOutput,TEMP-TABLE ttChave:HANDLE).



END PROCEDURE.






