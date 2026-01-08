/* Programa: ESCE032.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Controle de Estoque
** Objetivo: Relatorio Curva ABC Defeitos por Item/Desenho
** Autor...: Gilvando de Souza Araujo - Agosto/1997
** Obs.....: Especifico da CIA.RENASCENCA INDUSTRIAL
**
** Conversao para EMS 2.04:
**   Programa: ESCE032.P  =>  ESSP0075RP.P
**   Autor...: Gilvando Souza Araujo
**   Data....: 04/02/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESSP0075RP 2.04.00.000}

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
       FIELD fi-pto-min          AS INT FORMAT ">>>,>>9"
       FIELD opc-artigo          AS CHAR FORMAT "x"
       FIELD tp-tecelagem        AS CHAR
       FIELD imp-param           as log.

define temp-table tt-raw-digita
       field raw-digita as raw.

def TEMP-TABLE w-work
    field it-codigo   LIKE item.it-codigo
    FIELD cod-refer   LIKE ref-item.cod-refer
    field producao    as dec format ">>>>>>,>>9.99"
    field regular     as dec format ">>>>>>,>>9.99"
    field levedef     as dec format ">>>>>>,>>9.99"
    field retalho     as dec format ">>>>>>,>>9.99"
    field pontos      as int format ">>>>>9"
    INDEX ch-work it-codigo cod-refer.

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

def var de-tot-def-it as dec format ">>>>>>,>>9.99".
def var de-aux-perc1   as dec format ">>9.99".

form 
    "*------------ Parƒmetros/Sele‡Æo -------------*" SKIP
    tt-param.fi-ini-data-mov      label "Data do Movimento"
    "A"  AT 31                    
    tt-param.fi-fin-data-mov      NO-LABELS SKIP
    tt-param.fi-pto-min           LABEL "Pontos M¡nimos..." SKIP
    tt-param.opc-artigo           LABEL "Tipo de Artigo..." SKIP
    tt-param.tp-tecelagem         LABEL "Tipo Tecelagem..." FORMAT "x(50)"
    with FRAME f-param side-labels no-box width 132 STREAM-IO.

form
    w-work.it-codigo     label "Item"       FORMAT "x(7)"
    item.desc-item       label "Descricao"  FORMAT "x(29)" 
    w-work.cod-refer     LABEL "Referencia" 
    w-work.producao      label "Producao"
    w-work.regular       label "Regular"
    w-work.levedef       label "Leve defeito"
    w-work.retalho       label "Retalho"
    de-tot-def-it       label "Tot.defeito"
    de-aux-perc1         label "% Prod"
    w-work.pontos        label "Pontos"
    with no-box 55 down width 132 STREAM-IO frame f-detalhe no-label.

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
{utp/ut-liter.i Relatorio_de_Curva_ABC_de_Defeitos_por_Item * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).
 
for each w-work:
   delete w-work.
end.

FOR EACH mov-est-acbm WHERE mov-est-acbm.data-mov >= tt-param.fi-ini-data-mov 
                        AND mov-est-acbm.data-mov <= tt-param.fi-fin-data-mov
                        AND INDEX(tt-param.tp-tecelagem,mov-est-acbm.tipo-tear) <> 0
                      NO-LOCK,

    EACH item-ext WHERE item-ext.it-codigo = mov-est-acbm.it-codigo
                    AND (item-ext.indigo = YES AND tt-param.opc-artigo = "I" OR
                         item-ext.indigo = NO  AND tt-param.opc-artigo = "O" OR
                                                   tt-param.opc-artigo = "A")
                  NO-LOCK:

    run pi-acompanhar in h-acomp (input "Data: " + string(mov-est-acbm.data-mov)).
   
    find first w-work where
              w-work.it-codigo = mov-est-acbm.it-codigo and
              w-work.cod-refer = mov-est-acbm.cod-refer no-lock no-error.

    if not avail w-work then do:
       create w-work.
       assign w-work.it-codigo = mov-est-acbm.it-codigo
              w-work.cod-refer = mov-est-acbm.cod-refer
              w-work.producao  = mov-est-acbm.qtd-tot-perf +
                                 mov-est-acbm.qtd-tot-def +
                                 mov-est-acbm.qtd-tot-sob
              w-work.regular   = 0
              w-work.levedef   = 0
              w-work.retalho   = 0
              w-work.pontos    = 0.
    end.
    else
       assign w-work.producao = w-work.producao +
                                mov-est-acbm.qtd-tot-perf +
                                mov-est-acbm.qtd-tot-def +
                                mov-est-acbm.qtd-tot-sob.
   
    for each mov-est-acbd 
        where mov-est-acbd.data-mov  = mov-est-acbm.data-mov
          and mov-est-acbd.num-lote  = mov-est-acbm.num-lote
          and mov-est-acbd.it-codigo = mov-est-acbm.it-codigo
          AND mov-est-acbd.cod-refer = mov-est-acbm.cod-refer no-lock:

        if mov-est-acbd.classific = "Rg" then
           assign w-work.regular = w-work.regular +
                                   mov-est-acbd.qtd-defeit
                  w-work.pontos  = w-work.pontos + 
                                   int(mov-est-acbd.qtd-defeit * 2).
        else
        if mov-est-acbd.classific = "Rt" then
           assign w-work.retalho = w-work.retalho +
                                   mov-est-acbd.qtd-defeit
                  w-work.pontos  = w-work.pontos + 
                                   int(mov-est-acbd.qtd-defeit * 2).
        else
           assign w-work.levedef = w-work.levedef +
                                   mov-est-acbd.qtd-defeit
                  w-work.pontos  = w-work.pontos + 
                                   int(mov-est-acbd.qtd-defeit).
    end.
end.

for each w-work by w-work.pontos descending
                by w-work.it-codigo
                BY w-work.cod-refer:
   assign de-tot-def-it = w-work.levedef + w-work.retalho.
   if w-work.producao <> 0 then           
      assign de-aux-perc1 = de-tot-def-it / w-work.producao * 100.
   else
      assign de-aux-perc1 = 0.
   find item where
        item.it-codigo = w-work.it-codigo no-lock no-error.

   if w-work.pontos >= tt-param.fi-pto-min then do:
      display w-work.it-codigo
              item.desc-item
              w-work.cod-refer
              w-work.producao
              w-work.regular
              w-work.levedef
              w-work.retalho
              de-tot-def-it
              de-aux-perc1
              w-work.pontos
              with frame f-detalhe.
     down with frame f-detalhe.
   end.
end.

IF tt-param.imp-param THEN DO:
   PAGE.
   display tt-param.fi-ini-data-mov
           tt-param.fi-fin-data-mov
           tt-param.fi-pto-min
           tt-param.opc-artigo  
           tt-param.tp-tecelagem
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

