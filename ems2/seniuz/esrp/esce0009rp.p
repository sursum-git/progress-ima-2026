/* Programa: ESCE048.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Controle de Estoque
** Objetivo: Listar o catalogo de itens.
** Autor...: Gilvando de Souza Araujo - Maio/2004.
** Obs.....: Especifico da TEAR TEXTIL IND.COM.LTDA.
**
** Conversao para EMS 2.04:
**   Programa: ESCE048.P  =>  ESCE0009RP.P
**   Autor...: Gilvando Souza Araujo
**   Data....: 19/08/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESCE0009RP 2.04.00.000}

DEFINE TEMP-TABLE tt-param NO-UNDO
       FIELD destino          AS INTEGER
       FIELD arquivo          AS CHAR format "x(35)"
       FIELD usuario          AS CHAR format "x(12)"
       FIELD data-exec        AS DATE
       FIELD hora-exec        AS INTEGER
       FIELD classifica       AS INTEGER
       FIELD desc-classifica  AS CHAR FORMAT "x(40)"
       FIELD ini-ge-codigo    LIKE ITEM.ge-codigo
       FIELD fin-ge-codigo    LIKE item.ge-codigo
       FIELD ini-fm-codigo    LIKE item.fm-codigo
       FIELD fin-fm-codigo    LIKE item.fm-codigo 
       FIELD ini-it-codigo    LIKE item.it-codigo
       FIELD fin-it-codigo    LIKE item.it-codigo
       FIELD imp-des-comp     AS LOG FORMAT "Sim/NÆo"
       FIELD imp-param        AS LOG.

define temp-table tt-raw-digita
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* defini‡Æo de vari veis  */
def var h-acomp as handle no-undo.
DEF VAR c-descricao AS CHAR FORMAT "x(36)".
DEF VAR c-composi   AS CHAR FORMAT "x(40)".

form
    "*-------------- Parƒmetros/Sele‡Æo --------------*" SKIP
    tt-param.ini-ge-codigo    label "Grupo Estoque" AT 1
    "a"  AT 33                
    tt-param.fin-ge-codigo    no-labels
    tt-param.ini-fm-codigo    label "Familia......" AT 1
    "a"  AT 33                
    tt-param.fin-fm-codigo    no-labels
    tt-param.ini-it-codigo    label "Item........." AT 1
    "a"  AT 33                
    tt-param.fin-it-codigo    no-labels
    tt-param.imp-des-comp     LABEL "Imp.Desc.Comp" AT 1
    tt-param.desc-classifica  LABEL "Classificacao" AT 1
    with no-box side-labels width 132 stream-io frame f-param.

form
    item.it-codigo       label "Codigo"        FORMAT "x(6)"      
    c-descricao          label "Descricao"
    item.un              label "Un"
    ITEM.ge-codigo       LABEL "GE"
    ITEM.fm-codigo       LABEL "Fami"          FORMAT "x(4)"
    ITEM.peso-liquido    LABEL "PesoLiq"       FORMAT "9.99999"
    item-ext.fator-conv  LABEL "FatorM"        FORMAT "9.9999"
    ITEM.class-fiscal    LABEL "NCM-Cru"  
    item-ext.cod-ncm-est LABEL "NCM-Est"
    item-ext.cod-ncm-bco LABEL "NCM-Bco"
    item-ext.cod-ncm-tto LABEL "NCM-Tto"
    item-ext.largura     LABEL "Larg"
    item-ext.cod-rlgp    LABEL "RL"
    item-ext.cod-composi LABEL "Co" FORMAT "x(40)"
    with no-box NO-LABEL 55 down width 180 STREAM-IO frame f-detalhe.

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i}

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i MATERIAIS * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Cat logo_de_Itens * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
VIEW frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

for each item where item.ge-codigo >= tt-param.ini-ge-codigo
                and item.ge-codigo <= tt-param.fin-ge-codigo
                AND ITEM.fm-codigo >= tt-param.ini-fm-codigo
                AND item.fm-codigo <= tt-param.fin-fm-codigo
                and item.it-codigo >= tt-param.ini-it-codigo
                and item.it-codigo <= tt-param.fin-it-codigo
                    no-lock
              by if tt-param.classifica = 1 then item.it-codigo
                                            else item.descricao-1:
    
    run pi-acompanhar in h-acomp (input "Item: " + string(ITEM.it-codigo)).

    ASSIGN c-descricao = item.descricao-1 + item.descricao-2.
    
    FIND item-ext WHERE item-ext.it-codigo = ITEM.it-codigo NO-LOCK NO-ERROR.
    IF AVAIL item-ext THEN
       ASSIGN c-composi = item-ext.cod-composi.
    ELSE
       ASSIGN c-composi = "".
    FIND composi WHERE composi.cod-composi = item-ext.cod-composi NO-LOCK NO-ERROR.
    IF tt-param.imp-des-comp = YES AND AVAIL composi THEN
       ASSIGN c-composi = c-composi + ' ' + composi.descricao.
    display item.it-codigo
            c-descricao
            item.un
            ITEM.ge-codigo
            ITEM.fm-codigo
            ITEM.peso-liquido
            item-ext.fator-conv  WHEN AVAIL item-ext
            ITEM.class-fiscal
            item-ext.cod-ncm-est WHEN AVAIL item-ext
            item-ext.cod-ncm-bco WHEN AVAIL item-ext
            item-ext.cod-ncm-tto WHEN AVAIL item-ext
            item-ext.largura     WHEN AVAIL item-ext
            item-ext.cod-rlgp    WHEN AVAIL item-ext
            c-composi @ item-ext.cod-composi
            with frame f-detalhe.
    down with frame f-detalhe.           
end.

IF tt-param.imp-param THEN DO:
   PAGE.
   display tt-param.ini-ge-codigo
           tt-param.fin-ge-codigo
           tt-param.ini-fm-codigo
           tt-param.fin-fm-codigo
           tt-param.ini-it-codigo   
           tt-param.fin-it-codigo
           tt-param.imp-des-comp
           tt-param.desc-classifica
           with frame f-param.
END.
 
/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

