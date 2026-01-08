/*
programa: pdp/api/v1/histAvalPedVenda.p
objetivo: API a ser utilizada para troca de informa‡äes referente a avalia‡äes do pedido de venda.(tabela: hist_aval_ped_venda)
data: 10/2021

*/

{esp/utilAPI.i} //include especifica que inclui a utp/ut-api.i dentro dela

{utp/ut-api-action.i  avaliarPedidos    GET ~* lista_pedidos_aval=~*}
/*{utp/ut-api-action.i getUltDp            GET ~* log_ult_dp_venc=1}
{utp/ut-api-action.i getTodasDp          GET ~* log_todas_dp_aberto=1}
{utp/ut-api-action.i getTodasDp          GET ~* log_todas_dp_venc=1}
{utp/ut-api-action.i getTodasDpNF        GET ~* log_todas_dp_nf=1}   */
{utp/ut-api-notfound.i}

DEFINE VARIABLE hBoTransacao        AS HANDLE           NO-UNDO.
DEFINE VARIABLE hBoHistAvalPedVenda AS HANDLE           NO-UNDO.
DEFINE VARIABLE hMsg                AS HANDLE           NO-UNDO.

PROCEDURE getChave:
    DEFINE INPUT  PARAMETER pPedWeb AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER pChave  AS CHARACTER   NO-UNDO.

    ASSIGN pChave = "PW-GER-" + pPedWeb.

END PROCEDURE.



PROCEDURE avaliarPedidos:
    //parametros obrigatorios de entrada e saida
    DEF INPUT PARAM jsonInput       AS JsonObject NO-UNDO.
    DEF OUTPUT PARAM jsonOutput     AS JsonObject NO-UNDO.
    
    
    //variaveis locais
    DEFINE VARIABLE cListaPed       AS CHARACTER FORMAT 'x(500)'   NO-UNDO.
    DEFINE VARIABLE cOrigem         AS CHARACTER     NO-UNDO.
    DEFINE VARIABLE cMotivo         AS CHARACTER     NO-UNDO.
    DEFINE VARIABLE cSituacao       AS CHARACTER     NO-UNDO.
    DEFINE VARIABLE cCodUsuario     AS CHARACTER     NO-UNDO.
    DEFINE VARIABLE cDescMotivo     AS CHARACTER     NO-UNDO FORMAT 'x(500)'.
    DEFINE VARIABLE cTipo           AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cChave          AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iTrans          AS INTEGER     NO-UNDO.
    
    
        
    //pegar parametros querystring
    RUN setJsonInput(JsonInput).
    RUN getQueryParam('lista_pedidos_aval',OUTPUT cListaPed).
    RUN getQueryParam('num_motivo',OUTPUT cMotivo).
    RUN getQueryParam('num_origem',OUTPUT cOrigem).
    RUN getQueryParam('num_situacao',OUTPUT cSituacao).
    RUN getQueryParam('cod_usuario',OUTPUT cCodUsuario).
    RUN getQueryParam('desc_motivo',OUTPUT cDescMotivo).
    RUN getQueryParam('num_tipo',OUTPUT cTipo).
    

    //passagem de parametros gerais para a API
    EMPTY TEMP-TABLE ttRetorno .
    EMPTY TEMP-TABLE ttChave .
    RUN esapi/apiAvaliarPedsWeb.p(INPUT 'histAvalPedVenda',
                              INPUT cListaPed,
                              INPUT cTipo,
                              INPUT INT(cMotivo),
                              INPUT INT(cOrigem),
                              INPUT INT(cSituacao),
                              INPUT cCodUsuario,
                              INPUT cDescMotivo,
                              OUTPUT TABLE ttRetorno,
                              OUTPUT TABLE ttChave
                              ) .

    //RUN criarTTRetorno('teste',1) .
    //acrescenta a temp-table ttRetorno ao retorno da requisi‡Æo   
    RUN retJsontt(INPUT-OUTPUT jsonOutput,TEMP-TABLE ttRetorno:HANDLE).



END PROCEDURE.





