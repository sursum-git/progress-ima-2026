/* Programa: ESPD003
** Modulo..: Clientes
** Objetivo: 
1- Atualizar a situa‡Æo de pedidos web aprovados que nÆo mudaram para efetivados.
2- Atualizar a situa‡Æo de pedidos web que foram solicitados altera‡Æo e que nÆo
foram mudaram para solicita‡Æo de altera‡Æo.

** Autor...: Tadeu Silva Parreiras - FEV/2022
**
*/

/* Parametros de entrada logica obrigatoria */
{esp/espd003.i}
DEF TEMP-TABLE tt-raw-digita
    FIELD raw-digita                   AS RAW.

DEF INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEF INPUT PARAMETER TABLE FOR tt-raw-digita.       


CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.
{utp/ut-glob.i}
{include/i-rpvar.i}   
{include/i-rpout.i}
//{include/i-rpcab.i}


OUTPUT TO value("p:\aut_sit_ped_" + STRING(TIME) + ".txt") .
FOR EACH peds_web NO-LOCK
    WHERE peds_web.ind_sit_ped_web = 8 
    AND   peds_web.ped_web_id >= tt-param.pedWebIdIni
    AND   peds_web.ped_web_id <= tt-param.pedWebIdFim . // pend. aprov. ger

    //pedido aprovado e nÆo efetivado
    FIND FIRST hist_aval_ped_Venda 
        WHERE hist_aval_ped_venda.ped_web_id = peds_web.ped_web_id
        AND   hist_aval_ped_venda.ind_situacao = 1 //aprovado
        NO-LOCK NO-ERROR.
    IF AVAIL hist_aval_ped_venda THEN DO:
       FIND CURRENT peds_web SHARE-LOCK NO-ERROR.
       IF AVAIL peds_web THEN DO:
          ASSIGN peds_web.ind_sit_ped_web = 2. //efetivado
          PUT "pedido:" peds_web.ped_web_id " - Efetivado" SKIP.
       END.
    END.
    RELEASE peds_web.

    FIND FIRST hist_aval_ped_Venda 
        WHERE hist_aval_ped_venda.ped_web_id = peds_web.ped_web_id
        AND   hist_aval_ped_venda.ind_situacao = 5  //solicitado alteracao  
        NO-LOCK NO-ERROR.
    IF AVAIL hist_aval_ped_venda THEN DO:
       FIND CURRENT peds_web SHARE-LOCK NO-ERROR.
       IF AVAIL peds_web THEN DO:
          ASSIGN peds_web.ind_sit_ped_web = 9 . //em altera‡Æo solicitada ger.
          PUT "pedido:" peds_web.ped_web_id " - Voltou ao carrinho para altera‡Æo" SKIP.
       END.
    END.
    RELEASE peds_web.
END.

