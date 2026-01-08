/* Programa: ESCE034.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Controle de Estoque
** Objetivo: Relatorio Producao Mensal de Tecido Estampado
** Autor...: Gilvando de Souza Araujo - Setembro/1997
** Obs.....: Especifico da CIA.RENASCENCA INDUSTRIAL
**
** Conversao para EMS 2.04:
**   Programa: ESCE034.P  =>  ESSP0076RP.P
**   Autor...: Gilvando Souza Araujo
**   Data....: 07/02/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESSP0076RP 2.04.00.000}

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

def var i-tot-des-it as int format ">>>>9".
def var i-tot-des-ger as int format ">>>>9".
def var i-tot-var-it as int format ">>>>9".
def var i-tot-var-ger as int format ">>>>9".
def var i-tot-cil-it as int format ">>>>9".
def var i-tot-cil-ger as int format ">>>>9".
def var i-tot-prd-it as int format ">>,>>>,>>9".
def var i-tot-prd-ger as int format ">>,>>>,>>9".
def var i-tot-reg-it as int format ">>>>,>>9".
def var i-tot-reg-ger as int format ">>>>,>>9".
def var i-tot-ld-it  as int format ">>>>,>>9".
def var i-tot-ld-ger  as dec format ">>>>,>>9".
def var i-tot-ret-it as dec format ">>>>,>>9".
def var i-tot-ret-ger as dec format ">>>>,>>9".
def var de-aux-perc1  as dec format ">>9.9".
def var de-aux-perc2  as dec format ">>9.9".
def var de-aux-perc3  as dec format ">>9.9".
def var i-met-med-des as int format ">>>>9".
def var i-met-med-var as int format ">>>>9".
def var i-cil-des-it as int format ">>>>9".
def var i-cont-it     as int.

form 
    "*------------ Parƒmetros/Sele‡Æo -------------*" SKIP
    tt-param.fi-ini-data-mov      label "Data do Movimento"
    "A"  AT 35                    
    tt-param.fi-fin-data-mov      NO-LABELS SKIP
    tt-param.tp-tecelagem         LABEL "Tipo Tecelagem..." FORMAT "x(50)"
    with FRAME f-param side-labels no-box width 132 STREAM-IO.

form
    item.it-codigo      at   1 FORMAT "x(7)" 
    item.desc-item      at   9 FORMAT "x(30)"
    i-tot-des-it        at  40
    i-tot-var-it        at  46
    i-tot-cil-it        at  52
    i-met-med-des       at  58
    i-met-med-var       at  64
    i-cil-des-it        at  70
    i-tot-prd-it        at  78
    i-tot-reg-it        at  89
    de-aux-perc1        at  98
    i-tot-ld-it         at 104 
    de-aux-perc2        at 113
    i-tot-ret-it        at 119
    de-aux-perc3        at 128
    with no-box 55 down width 132 STREAM-IO frame f-detalhe no-label.

form header
    "QUANTIDADE-TOTAL  METR-MEDIA  MEDIA"                   at 41
    "METRAGEM *-------------DEFEITOS-APURADOS------------*" at 80 skip
    "ITEM    DESCRICAO"                                     at 01
    "DES.  VAR.  CIL.  DES.  VAR. CIL/DES"                  at 41
    "PRODUZIDA  REGULAR    %   LEV.DEF    %   RETALHO    %" at 79 skip
    "------- ------------------------------"                at 01
    "----- ----- ----- ----- ----- ------- ----------"      at 40
    "-------- ----- -------- ----- -------- -----"          at 89
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
{utp/ut-liter.i Relatorio_Produ‡Æo_Tecidos_Estampados_por_Item * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
VIEW FRAME f-cabecalho.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).
 
for each mov-est-acbm WHERE mov-est-acbm.data-mov >= tt-param.fi-ini-data-mov 
                        AND mov-est-acbm.data-mov <= tt-param.fi-fin-data-mov 
                        AND substr(mov-est-acbm.cod-refer,7,1) <> "0" 
                        AND INDEX(tt-param.tp-tecelagem,mov-est-acbm.tipo-tear) <> 0
                      NO-LOCK 
         break by mov-est-acbm.it-codigo
               by mov-est-acbm.cod-refer:

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
     assign i-tot-des-it = i-tot-des-it + 1
            i-tot-des-ger = i-tot-des-ger + 1
            i-tot-cil-it = i-tot-cil-it +
                           int(substr(mov-est-acbm.cod-refer,3,1))
            i-tot-cil-ger = i-tot-cil-ger +
                            int(substr(mov-est-acbm.cod-refer,3,1)).
   
  assign i-tot-var-it = i-tot-var-it + 1
         i-tot-var-ger = i-tot-var-ger + 1.
  
  for each mov-est-acbd 
      where mov-est-acbd.data-mov  = mov-est-acbm.data-mov
        and mov-est-acbd.num-lote  = mov-est-acbm.num-lote
        and mov-est-acbd.it-codigo = mov-est-acbm.it-codigo
        AND mov-est-acbd.cod-refer = mov-est-acbm.cod-refer 
      no-lock:

      if mov-est-acbd.classific = "Rg" then
         assign i-tot-reg-it = i-tot-reg-it + 
                                mov-est-acbd.qtd-defeit
                i-tot-reg-ger = i-tot-reg-ger +
                                mov-est-acbd.qtd-defeit.
      else
      if mov-est-acbd.classific = "Rt" then
         assign i-tot-ret-it = i-tot-ret-it + 
                                mov-est-acbd.qtd-defeit
                i-tot-ret-ger = i-tot-ret-ger +
                                mov-est-acbd.qtd-defeit.
      else
         assign i-tot-ld-it = i-tot-ld-it + 
                               mov-est-acbd.qtd-defeit
                i-tot-ld-ger = i-tot-ld-ger +
                               mov-est-acbd.qtd-defeit.
  end.      
                
  if last-of(mov-est-acbm.it-codigo) then do:
     find item where
          item.it-codigo = mov-est-acbm.it-codigo no-lock no-error.

     if i-tot-des-it > 0 then
        assign i-met-med-des = i-tot-prd-it / i-tot-des-it
               i-cil-des-it = i-tot-cil-it / i-tot-des-it.
     else
        assign i-met-med-des = 0
               i-cil-des-it = 0.

     if i-tot-var-it > 0 then
        assign i-met-med-var = i-tot-prd-it / i-tot-var-it.
     else
        assign i-met-med-var = 0.
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
             i-tot-des-it
             i-tot-var-it
             i-tot-cil-it
             i-met-med-des
             i-met-med-var
             i-cil-des-it
             i-tot-prd-it
             i-tot-reg-it
             de-aux-perc1
             i-tot-ld-it
             de-aux-perc2
             i-tot-ret-it
             de-aux-perc3
             with frame f-detalhe.
     down with frame f-detalhe.
     
     assign i-tot-des-it = 0
            i-tot-var-it = 0
            i-tot-cil-it = 0
            i-tot-prd-it = 0
            i-tot-reg-it = 0
            i-tot-ld-it  = 0
            i-tot-ret-it = 0.
     assign i-cont-it = i-cont-it + 1.
  end.
end.                                 

if i-tot-des-ger > 0 then
  assign i-met-med-des = i-tot-prd-ger / i-tot-des-ger
         i-cil-des-it = i-tot-cil-ger / i-tot-des-ger.
else
  assign i-met-med-des = 0
         i-cil-des-it = 0.
if i-tot-var-ger > 0 then
  assign i-met-med-var = i-tot-prd-ger / i-tot-var-ger.
else
  assign i-met-med-var = 0.
if i-tot-prd-ger > 0 then
  assign de-aux-perc1 = i-tot-reg-ger / i-tot-prd-ger * 100
         de-aux-perc2 = i-tot-ld-ger  / i-tot-prd-ger * 100
         de-aux-perc2 = i-tot-ret-ger / i-tot-prd-ger * 100.
else
  assign de-aux-perc1 = 0
         de-aux-perc2 = 0
         de-aux-perc3 = 0.

down 1 with frame f-detalhe.

display "Total"        @ item.it-codigo
        i-tot-des-ger  @ i-tot-des-it
        i-tot-var-ger  @ i-tot-var-it
        i-tot-cil-ger  @ i-tot-cil-it
        i-met-med-des
        i-met-med-var
        i-cil-des-it
        i-tot-prd-ger  @ i-tot-prd-it
        i-tot-reg-ger  @ i-tot-reg-it
        i-tot-ld-ger   @ i-tot-ld-it
        de-aux-perc1
        i-tot-ret-ger  @ i-tot-ret-it
        de-aux-perc2
        with frame f-detalhe.
down 2 with frame f-detalhe.

display "Media"                     @ item.it-codigo
        i-tot-des-ger / i-cont-it  @ i-tot-des-it
        i-tot-var-ger / i-cont-it  @ i-tot-var-it
        i-tot-cil-ger / i-cont-it  @ i-tot-cil-it
        i-met-med-des / i-cont-it  @ i-met-med-des
        i-met-med-var / i-cont-it  @ i-met-med-var
        i-cil-des-it / i-cont-it  @ i-cil-des-it
        i-tot-prd-ger / i-cont-it  @ i-tot-prd-it
        i-tot-reg-ger / i-cont-it  @ i-tot-reg-it
        i-tot-ld-ger  / i-cont-it  @ i-tot-ld-it
        i-tot-ret-ger / i-cont-it  @ i-tot-ret-it
        with frame f-detalhe.
down with frame f-detalhe.        
      
assign i-tot-des-ger = 0
       i-tot-var-ger = 0
       i-tot-cil-ger = 0
       i-tot-prd-ger = 0
       i-tot-reg-ger = 0
       i-tot-ld-ger  = 0
       i-tot-ret-ger = 0
       i-cont-it    = 0.

IF tt-param.imp-param THEN DO:
   PAGE.
   display tt-param.fi-ini-data-mov
           tt-param.fi-fin-data-mov
           tt-param.tp-tecelagem
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

