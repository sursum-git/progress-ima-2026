/* Programa: ESCE035.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Controle de Estoque
** Objetivo: Relatorio Producao Mensal de Tecido Liso
** Autor...: Gilvando de Souza Araujo - Setembro/1997
** Obs.....: Especifico da CIA.RENASCENCA INDUSTRIAL
**
** Conversao para EMS 2.04:
**   Programa: ESCE035.P  =>  ESSP0077RP.P
**   Autor...: Gilvando Souza Araujo
**   Data....: 07/02/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESSP0077RP 2.04.00.000}

define temp-table tt-param  no-undo
       field destino             as integer
       field arquivo             as char format "x(35)"
       field usuario             as char format "x(12)"
       field data-exec           as date
       field hora-exec           as integer
       field classifica          as integer
       field desc-classifica     as char format "x(40)"
       FIELD fi-ini-data-mov     LIKE mov-est-acbm.data-mov
       FIELD fi-fin-data-mov     LIKE mov-est-acbm.data-mov
       FIELD tp-tecelagem        AS CHAR
       field imp-param           as log.

define temp-table tt-raw-digita
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* include padrÆo para impressÆo de campos editores em relat¢rio  */
{include/tt-edit.i}

/* defini‡Æo de vari veis  */
def var h-acomp as handle no-undo.

def var i-tot-cor-it  as int format ">>>>9".
def var i-tot-cor-ger as int format ">>>>9".
def var i-met-med-cor as int format ">>>>>>9".
def var i-tot-prd-it  as int format ">>,>>>,>>9".
def var i-tot-prd-ger as int format ">>,>>>,>>9".
def var i-tot-reg-it  as int format ">>>>,>>9".
def var i-tot-reg-ger as int format ">>>>,>>9".
def var i-tot-ld-it   as int format ">>>>,>>9".
def var i-tot-ld-ger  as dec format ">>>>,>>9".
def var i-tot-ret-it  as dec format ">>>>,>>9".
def var i-tot-ret-ger as dec format ">>>>,>>9".
def var de-aux-perc1  as dec format ">>9.9".
def var de-aux-perc2  as dec format ">>9.9". 
def var de-aux-perc3  as dec format ">>9.9".
def var i-cont-it     as int.

form 
    "*------------ Parƒmetros/Sele‡Æo -------------*" SKIP
    tt-param.fi-ini-data-mov      label "Data do Movimento"
    "A"  AT 31                    
    tt-param.fi-fin-data-mov      NO-LABELS SKIP
    tt-param.tp-tecelagem         LABEL "Tipo Tecelagem..." FORMAT "x(50)"
    with FRAME f-param side-labels no-box width 132 STREAM-IO.

form
    item.it-codigo      at   1 FORMAT "x(7)"
    item.desc-item      at   9 FORMAT "x(36)"
    i-tot-cor-it        at  46
    i-met-med-cor       at  56
    i-tot-prd-it        at  65
    i-tot-reg-it        at  77
    de-aux-perc1        at  86
    i-tot-ld-it         at  92
    de-aux-perc2        at 101 
    i-tot-ret-it        at 107
    de-aux-perc3        at 116
    with no-box 55 down width 132 STREAM-IO frame f-detalhe no-label.

form header
    "QUANTIDADE  METR-MEDIA  METR-TOTAL"            at 41        
    "*-------------DEFEITOS APURADOS------------*"  at 77
    "ITEM    DESCRICAO"                             at 01
    "TOTAL-CORES     POR COR   PRODUZIDA"           at 40
    " REGULAR    %   LEV.DEF    %   RETALHO    %"   at 77
    "------- ------------------------------"        at 01
    "-----------  ---------- -----------"           at 40
    "-------- ----- -------- ----- -------- -----"  at 77
    with width 132 STREAM-IO no-box page-top frame f-cabecalho.

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i}

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i ESPECIFICO * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Relatorio_Produ‡Æo_Tecidos_Lisos_por_Item * r}
assign c-titulo-relat = trim(return-value).

VIEW FRAME f-cabec.
VIEW FRAME f-cabecalho.
VIEW FRAME f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).
 
FOR EACH mov-est-acbm WHERE mov-est-acbm.data-mov >= tt-param.fi-ini-data-mov 
                        AND mov-est-acbm.data-mov <= tt-param.fi-fin-data-mov 
                        AND substr(mov-est-acbm.cod-refer,7,1) = "0"
                        AND INDEX(tt-param.tp-tecelagem,mov-est-acbm.tipo-tear) <> 0
                      NO-LOCK 
         BREAK BY mov-est-acbm.it-codigo
               BY mov-est-acbm.cod-refer:

    run pi-acompanhar in h-acomp (input "Data: " + string(mov-est-acbm.data-mov) + " Item: " + mov-est-acbm.it-codigo).
    
    assign i-tot-prd-it = i-tot-prd-it +
                          mov-est-acbm.qtd-tot-perf +
                          mov-est-acbm.qtd-tot-def +
                          mov-est-acbm.qtd-tot-sob
           i-tot-prd-ger = i-tot-prd-ger +
                           mov-est-acbm.qtd-tot-perf +
                           mov-est-acbm.qtd-tot-def +
                           mov-est-acbm.qtd-tot-sob.
    
    if last-of(mov-est-acbm.cod-refer) then
       assign i-tot-cor-it = i-tot-cor-it + 1
              i-tot-cor-ger = i-tot-cor-ger + 1.
    
    FOR each mov-est-acbd 
        where mov-est-acbd.data-mov  = mov-est-acbm.data-mov
          and mov-est-acbd.num-lote  = mov-est-acbm.num-lote
          and mov-est-acbd.it-codigo = mov-est-acbm.it-codigo
              no-lock:
        if mov-est-acbd.classific = "Rg" then
           assign i-tot-reg-it = i-tot-reg-it + mov-est-acbd.qtd-defeit
                  i-tot-reg-ger = i-tot-reg-ger + mov-est-acbd.qtd-defeit.
        else
        if mov-est-acbd.classific = "Rt" then
           assign i-tot-ret-it = i-tot-ret-it + mov-est-acbd.qtd-defeit
                  i-tot-ret-ger = i-tot-ret-ger + mov-est-acbd.qtd-defeit.
        else
           assign i-tot-ld-it = i-tot-ld-it + mov-est-acbd.qtd-defeit
                  i-tot-ld-ger = i-tot-ld-ger + mov-est-acbd.qtd-defeit.
 END.      
              
 if last-of(mov-est-acbm.it-codigo) then do:
    find item where
         item.it-codigo = mov-est-acbm.it-codigo no-lock no-error.
    if i-tot-cor-it > 0 then
       assign i-met-med-cor = i-tot-prd-it / i-tot-cor-it.
    else
       assign i-met-med-cor = 0.
    
    if i-tot-prd-it > 0 then
       assign de-aux-perc1 = i-tot-reg-it / i-tot-prd-it * 100
              de-aux-perc2 = i-tot-ld-it  / i-tot-prd-it * 100
              de-aux-perc3 = i-tot-ret-it / i-tot-prd-it * 100.
    else
       assign de-aux-perc1 = 0
              de-aux-perc2 = 0
              de-aux-perc3 = 0.

    display item.it-codigo
            item.desc-item
            i-tot-cor-it
            i-met-med-cor
            i-tot-prd-it
            i-tot-reg-it
            de-aux-perc1
            i-tot-ld-it
            de-aux-perc2
            i-tot-ret-it
            de-aux-perc3
            with frame f-detalhe.
    down with frame f-detalhe.
   
    assign i-tot-cor-it = 0
           i-tot-prd-it = 0
           i-tot-reg-it = 0
           i-tot-ld-it  = 0
           i-tot-ret-it = 0.
    assign i-cont-it = i-cont-it + 1.
 end.
end.                                 

if i-tot-cor-ger > 0 then
   assign i-met-med-cor = i-tot-prd-ger / i-tot-cor-ger.
else
   assign i-met-med-cor = 0.

if i-tot-prd-ger > 0 then
   assign de-aux-perc1 = i-tot-reg-ger / i-tot-prd-ger * 100
          de-aux-perc2 = i-tot-ld-ger  / i-tot-prd-ger * 100
          de-aux-perc3 = i-tot-ret-ger / i-tot-prd-ger * 100.
else
   assign de-aux-perc1 = 0
          de-aux-perc2 = 0
          de-aux-perc3 = 0.

down 1 with frame f-detalhe.

display "Total"        @ item.it-codigo
        i-tot-cor-ger  @ i-tot-cor-it
        i-met-med-cor
        i-tot-prd-ger  @ i-tot-prd-it
        i-tot-reg-ger  @ i-tot-reg-it
        de-aux-perc1  
        i-tot-ld-ger   @ i-tot-ld-it
        de-aux-perc2
        i-tot-ret-ger  @ i-tot-ret-it
        de-aux-perc3
        with frame f-detalhe.
down 2 with frame f-detalhe.

DISPLAY "Media"                    @ item.it-codigo
        i-tot-cor-ger / i-cont-it  @ i-tot-cor-it
        i-met-med-cor / i-cont-it  @ i-met-med-cor
        i-tot-prd-ger / i-cont-it  @ i-tot-prd-it
        i-tot-reg-ger / i-cont-it  @ i-tot-reg-it
        i-tot-ld-ger  / i-cont-it  @ i-tot-ld-it
        i-tot-ret-ger / i-cont-it  @ i-tot-ret-it
        WITH FRAME f-detalhe.
DOWN WITH FRAME f-detalhe.        
    
ASSIGN i-tot-cor-ger = 0     i-tot-prd-ger = 0
       i-tot-reg-ger = 0     i-tot-ld-ger  = 0
       i-tot-ret-ger = 0      i-cont-it    = 0.

IF tt-param.imp-param THEN DO:
   PAGE.
   DISPLAY tt-param.fi-ini-data-mov
           tt-param.fi-fin-data-mov
           tt-param.tp-tecelagem
           WITH FRAME f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
RETURN "OK":U.

