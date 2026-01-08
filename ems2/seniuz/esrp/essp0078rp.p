/* Programa: ESCE047.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Controle de Estoque
** Objetivo: Relatorio Producao por Item/Nuance
** Autor...: Gilvando de Souza Araujo - Maio/2003
** Obs.....: Especifico da TEAR TEXTIL IND.COM.LTDA.
**
** Conversao para EMS 2.04:
**   Programa: ESCE047.P  =>  ESSP0078RP.P
**   Autor...: Gilvando Souza Araujo
**   Data....: 07/02/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESSP0078RP 2.04.00.000}

def TEMP-TABLE w-work
    field it-codigo  like item.it-codigo
    field nuance-cla as char format "x(2)"
    field nuance-qtd as dec format ">>>,>>>,>>9.99"
    FIELD item-qtd   AS DEC FORMAT ">>>,>>>,>>9.99"
    INDEX ch-work it-codigo
                  nuance-cla.

def buffer b-w-work for w-work.

DEFINE TEMP-TABLE tt-param  NO-UNDO
       FIELD destino             AS INTEGER
       FIELD arquivo             AS CHAR FORMAT "x(35)"
       FIELD usuario             AS CHAR FORMAT "x(12)"
       FIELD data-exec           AS DATE
       FIELD hora-exec           AS INTEGER
       FIELD classifica          AS INTEGER
       FIELD desc-classifica     AS CHAR FORMAT "x(40)"
       FIELD fi-ini-it-codigo    AS CHAR FORMAT "x(16)"
       FIELD fi-fin-it-codigo    AS CHAR FORMAT "x(16)"
       FIELD fi-ini-data-mov     LIKE mov-est-acbm.data-mov
       FIELD fi-fin-data-mov     LIKE mov-est-acbm.data-mov
       FIELD fi-ini-num-lote     LIKE mov-est-acbm.num-lote
       FIELD fi-fin-num-lote     LIKE mov-est-acbm.num-lote
       FIELD opc-artigo          AS CHAR FORMAT "x"
       FIELD tp-tecelagem        AS CHAR
       FIELD imp-param           AS LOG.

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

def var de-qtd-ger  as dec format ">>,>>>,>>9.99".
def var de-aux-perc as dec format ">>9.99".
def var i-cont      as int.

form 
    "*--------------- Parƒmetros/Sele‡Æo -----------------*" SKIP
    tt-param.fi-ini-it-codigo     LABEL "Item............." 
    "A"  AT 37
    tt-param.fi-fin-it-codigo     NO-LABELS SKIP
    tt-param.fi-ini-data-mov      label "Data do Movimento"
    "A"  AT 37                    
    tt-param.fi-fin-data-mov      NO-LABELS SKIP
    tt-param.opc-artigo           LABEL "Tipo de Artigo..." SKIP
    tt-param.tp-tecelagem         LABEL "Tipo Tecelagem..." FORMAT "x(50)"
    with FRAME f-param side-labels no-box width 132 STREAM-IO.

form
    w-work.it-codigo   label "Item"
    item.desc-item     label "Descricao"
    w-work.nuance-cla  label "Nuance"
    w-work.nuance-qtd  label "Quantidade"
    de-aux-perc        label "%"
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
{utp/ut-liter.i Relatorio_Produ‡Æo_Tecidos_por_Item/Nuance * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

for each mov-est-acbm where mov-est-acbm.data-mov  >= tt-param.fi-ini-data-mov
                        and mov-est-acbm.data-mov  <= tt-param.fi-fin-data-mov
                        and mov-est-acbm.it-codigo >= tt-param.fi-ini-it-codigo
                        and mov-est-acbm.it-codigo <= tt-param.fi-fin-it-codigo
                        AND mov-est-acbm.num-lote  >= tt-param.fi-ini-num-lote
                        AND mov-est-acbm.num-lote  <= tt-param.fi-fin-num-lote 
                        AND INDEX(tt-param.tp-tecelagem,mov-est-acbm.tipo-tear) <> 0
                      NO-LOCK,
    EACH item-ext WHERE item-ext.it-codigo = mov-est-acbm.it-codigo
                    AND (item-ext.indigo = YES AND tt-param.opc-artigo = "I" OR
                         item-ext.indigo = NO  AND tt-param.opc-artigo = "O" OR
                                                   tt-param.opc-artigo = "A")
                  NO-LOCK:

    run pi-acompanhar in h-acomp (input "Data: " + STRING(mov-est-acbm.data-mov) + " Item: " + mov-est-acbm.it-codigo).
   
    do i-cont = 1 to 10:
       if mov-est-acbm.nuance-cla[i-cont] <> "" then do:
          find first w-work
           where w-work.it-codigo  = mov-est-acbm.it-codigo 
             and w-work.nuance-cla = mov-est-acbm.nuance-cla[i-cont]
           no-lock no-error.
          if not avail w-work then do:
             create w-work.
             assign w-work.it-codigo = mov-est-acbm.it-codigo
                    w-work.nuance-cla = mov-est-acbm.nuance-cla[i-cont].
          end.
          assign w-work.nuance-qtd = w-work.nuance-qtd + 
                                     mov-est-acbm.nuance-qtd[i-cont]
                 de-qtd-ger        = de-qtd-ger +
                                     mov-est-acbm.nuance-qtd[i-cont].
       end.
    end.
end.

/*-- Apura total por item, para classificacao do relatorio --*/
FOR EACH w-work BREAK BY w-work.it-codigo:
    ACCUMULATE w-work.nuance-qtd (TOTAL BY w-work.it-codigo).
    IF LAST-OF(w-work.it-codigo) THEN DO:
       FOR EACH b-w-work WHERE b-w-work.it-codigo = w-work.it-codigo:
           ASSIGN b-w-work.item-qtd = (ACCUM TOTAL BY w-work.it-codigo w-work.nuance-qtd).
       END.
    END.
END.

/*-- Imprime relatorio --*/
for each w-work break by w-work.item-qtd DESCEND
                      BY w-work.it-codigo
                      by w-work.nuance-qtd DESCEND:
    if first-of(w-work.it-codigo) then do:
       find item where item.it-codigo = w-work.it-codigo
                no-lock no-error.
       display w-work.it-codigo
           item.desc-item
           with frame f-detalhe.
   end.

   assign de-aux-perc = w-work.nuance-qtd / w-work.item-qtd * 100.

   display w-work.nuance-cla
           w-work.nuance-qtd
           de-aux-perc
           with frame f-detalhe.
   down with frame f-detalhe.
   
   if last-of(w-work.it-codigo) then do:
      assign de-aux-perc = w-work.item-qtd / de-qtd-ger * 100.
      
      display "Total da Item" @ item.desc-item
              w-work.item-qtd @ w-work.nuance-qtd
              de-aux-perc
              with frame f-detalhe.
      down 2 with frame f-detalhe.
   end.
end.

DISPLAY "Total Geral" @ item.desc-item
        de-qtd-ger    @ w-work.nuance-qtd
        WITH FRAME f-detalhe.

IF tt-param.imp-param THEN DO:
   PAGE.
   DISPLAY tt-param.fi-ini-it-codigo
           tt-param.fi-fin-it-codigo
           tt-param.fi-ini-data-mov
           tt-param.fi-fin-data-mov
           tt-param.opc-artigo
           tt-param.tp-tecelagem
           WITH FRAME f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
RUN pi-finalizar IN h-acomp.
RETURN "OK":U.

