/******************************************************************************
** PROGRAMA: ESCD1607RP.P                                                    **
** DATA    : AGOSTO/2017                                                     **
** AUTOR   : Toninho                                                         **
** OBJETIVO: Exporta‡Æo para o Serasa Experian Positivo                      **
******************************************************************************/
/* Programa de controle de versao e seguran»a do Datasul EMS */

{include/i-prgvrs.i ESCE005 2.06.00.001}

/* Definicao de variaveis globais comuns a todos os programas do EMS */
{utp/ut-glob.i}

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)"
    field modelo-rtf       as char format "x(35)"
    field l-habilitaRtf    as LOG.

/* Parametros de entrada logica obrigatoria */
DEF TEMP-TABLE tt-raw-digita
    FIELD raw-digita                   AS RAW.

DEF INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEF INPUT PARAMETER TABLE FOR tt-raw-digita.       
DEFINE VARIABLE hBoControlePreco AS HANDLE      NO-UNDO.

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

{include/i-rpvar.i}   

RUN esbo/boControlePreco.p PERSISTENT SET hBoControlePreco.
RUN vencerPrecosOutletSemEstoque IN hBoControlePreco.
IF VALID-HANDLE(hboControlePreco) THEN
   DELETE PROCEDURE hBoControlePreco.



/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
RETURN "OK":U.
