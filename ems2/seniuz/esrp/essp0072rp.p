/* Programa: ESCE030.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Controle de Estoque
** Objetivo: Relatorio Defeitos por Item/Tipo Defeito
** Autor...: Gilvando de Souza Araujo - Agosto/1997
** Obs.....: Especifico da CIA.RENASCENCA INDUSTRIAL
**
** Conversao para EMS 2.04:
**   Programa: ESCE030.P  =>  ESSP0072RP.P
**   Autor...: Gilvando Souza Araujo
**   Data....: 04/02/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESSP0072RP 2.04.00.000}

define temp-table tt-param  no-undo
       field destino             as integer
       field arquivo             as char format "x(35)"
       field usuario             as char format "x(12)"
       field data-exec           as date
       field hora-exec           as integer
       field classifica          as integer
       field desc-classifica     as char format "x(40)"
       field fi-ini-it-codigo    like mov-est-acbd.it-codigo
       field fi-fin-it-codigo    like mov-est-acbd.it-codigo
       FIELD fi-ini-cod-refer    LIKE mov-est-acbd.cod-refer
       FIELD fi-fin-cod-refer    LIKE mov-est-acbd.cod-refer
       field fi-desenho          as char format "x(4)"
       FIELD l-inc-exc           AS LOG FORMAT "Inclusive/Exclusive"
       FIELD fi-ini-cod-tipo-def LIKE mov-est-acbd.cod-tipo-def
       FIELD fi-fin-cod-tipo-def LIKE mov-est-acbd.cod-tipo-def
       FIELD fi-ini-data-mov     LIKE mov-est-acbm.data-mov
       FIELD fi-fin-data-mov     LIKE mov-est-acbm.data-mov
       FIELD opc-artigo          AS CHAR FORMAT "x"
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

def var de-tot-reg-tip as dec format ">>,>>>,>>9.99".
def var de-tot-ld-tip  as dec format ">>,>>>,>>9.99".
def var de-tot-ret-tip as dec format ">>,>>>,>>9.99".
def var de-tot-def-tip as dec format ">>>,>>>,>>9.99".
def var de-tot-reg-it  as dec format ">>,>>>,>>9.99".
def var de-tot-ld-it   as dec format ">>,>>>,>>9.99".
def var de-tot-ret-it  as dec format ">>,>>>,>>9.99".
def var de-tot-def-it  as dec format ">>>,>>>,>>9.99".
def var de-tot-reg-ger as dec format ">>,>>>,>>9.99".
def var de-tot-ld-ger  as dec format ">>,>>>,>>9.99".
def var de-tot-ret-ger as dec format ">>,>>>,>>9.99".
def var de-tot-def-ger as dec format ">>>,>>>,>>9.99".
def var de-aux-perc1   as dec format ">>9.99".
def var de-aux-perc2   as dec format ">>9.99".
def var de-aux-perc3   as dec format ">>9.99".
def var de-aux-perc4   as dec format ">>9.99".

form 
    "*------------ Parƒmetros/Sele‡Æo -------------*" SKIP
    tt-param.fi-ini-it-codigo     label "Item............."
    "A"  AT 36                    
    tt-param.fi-fin-it-codigo     NO-LABELS SKIP
    tt-param.fi-ini-cod-refer     label "Referˆncia......."
    "A"  AT 36                    
    tt-param.fi-fin-cod-refer     NO-LABELS SKIP
    tt-param.fi-desenho           LABEL "Desenho.........."
    " "  AT 24                    
    tt-param.l-inc-exc            NO-LABELS SKIP
    tt-param.fi-ini-cod-tipo-def  label "Tipo do Defeito.."
    "A"  AT 36
    tt-param.fi-fin-cod-tipo-def  NO-LABELS SKIP
    tt-param.fi-ini-data-mov      label "Data do Movimento"
    "A"  AT 36                    
    tt-param.fi-fin-data-mov      NO-LABELS SKIP
    tt-param.opc-artigo           LABEL "Tipo de Artigo..." SKIP
    tt-param.tp-tecelagem         LABEL "Tipo Tecelagem..." FORMAT "x(50)"
    with FRAME f-param side-labels no-box width 132 STREAM-IO.

form
    item.it-codigo         label "Item" FORMAT "x(7)"
    item.desc-item         label "Descricao" FORMAT "x(32)" 
    tipo-def.cod-tipo-def  label "Cod"
    tipo-def.descricao     label "Tipo defeito"
    de-tot-reg-tip         label "Regular"
    de-tot-ld-tip          label "Leve defeito"
    de-tot-ret-tip         label "Retalho"
    de-tot-def-tip         label "Total"
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
{utp/ut-liter.i Relatorio_de_Defeitos_por_Item/Tipo_Defeito * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

FOR EACH mov-est-acbm
    WHERE mov-est-acbm.data-mov  >= tt-param.fi-ini-data-mov  
      AND mov-est-acbm.data-mov  <= tt-param.fi-fin-data-mov  
      AND mov-est-acbm.it-codigo >= tt-param.fi-ini-it-codigo 
      AND mov-est-acbm.it-codigo <= tt-param.fi-fin-it-codigo 
      AND mov-est-acbm.cod-refer >= tt-param.fi-ini-cod-refer
      AND mov-est-acbm.cod-refer <= tt-param.fi-fin-cod-refer
      AND INDEX(tt-param.tp-tecelagem,mov-est-acbm.tipo-tear) <> 0
      AND ((substr(mov-est-acbm.cod-refer,3,4) =  tt-param.fi-desenho AND tt-param.l-inc-exc = yes) or 
           (substr(mov-est-acbm.cod-refer,3,4) <> tt-param.fi-desenho and tt-param.l-inc-exc = no) or 
           (tt-param.fi-desenho = ""))  
    no-lock,
    EACH item-ext WHERE item-ext.it-codigo = mov-est-acbm.it-codigo
                    AND (item-ext.indigo = YES AND tt-param.opc-artigo = "I" OR
                         item-ext.indigo = NO  AND tt-param.opc-artigo = "O" OR
                                                   tt-param.opc-artigo = "A")
                  NO-LOCK,
    each mov-est-acbd 
       where mov-est-acbd.data-mov     =  mov-est-acbm.data-mov
         and mov-est-acbd.num-lote     =  mov-est-acbm.num-lote
         and mov-est-acbd.it-codigo    =  mov-est-acbm.it-codigo
         AND mov-est-acbd.cod-refer    =  mov-est-acbm.cod-refer
         and mov-est-acbd.cod-tipo-def >= tt-param.fi-ini-cod-tipo-def
         and mov-est-acbd.cod-tipo-def <= tt-param.fi-fin-cod-tipo-def
       no-lock:
    
    run pi-acompanhar in h-acomp (input "Item: " + mov-est-acbm.it-codigo + " Data: " +
                                  STRING(mov-est-acbm.data-mov)).

    if mov-est-acbd.classific = "Rg" then
       assign de-tot-reg-ger = de-tot-reg-ger + mov-est-acbd.qtd-defeit.
    else
    if mov-est-acbd.classific = "Rt" then
       assign de-tot-ret-ger = de-tot-ret-ger + mov-est-acbd.qtd-defeit.
    else
       assign de-tot-ld-ger = de-tot-ld-ger + mov-est-acbd.qtd-defeit.
end.
assign de-tot-def-ger = de-tot-reg-ger + de-tot-ld-ger + de-tot-ret-ger.

for each mov-est-acbm 
    WHERE mov-est-acbm.data-mov  >= tt-param.fi-ini-data-mov 
      and mov-est-acbm.data-mov  <= tt-param.fi-fin-data-mov 
      and mov-est-acbm.it-codigo >= tt-param.fi-ini-it-codigo
      AND mov-est-acbm.it-codigo <= tt-param.fi-fin-it-codigo
      AND mov-est-acbm.cod-refer >= tt-param.fi-ini-cod-refer
      AND mov-est-acbm.cod-refer <= tt-param.fi-fin-cod-refer
      AND INDEX(tt-param.tp-tecelagem,mov-est-acbm.tipo-tear) <> 0
    no-lock,
    EACH item-ext WHERE item-ext.it-codigo = mov-est-acbm.it-codigo
                    AND (item-ext.indigo = YES AND tt-param.opc-artigo = "I" OR
                         item-ext.indigo = NO  AND tt-param.opc-artigo = "O" OR
                                                   tt-param.opc-artigo = "A")
                  NO-LOCK,
    each mov-est-acbd 
       WHERE mov-est-acbd.data-mov     =  mov-est-acbm.data-mov 
         AND mov-est-acbd.num-lote     =  mov-est-acbm.num-lote 
         AND mov-est-acbd.it-codigo    =  mov-est-acbm.it-codigo
         AND mov-est-acbd.cod-refer    =  mov-est-acbm.cod-refer
         AND mov-est-acbd.cod-tipo-def >= tt-param.fi-ini-cod-tipo-def 
         AND mov-est-acbd.cod-tipo-def <= tt-param.fi-fin-cod-tipo-def 
       no-lock
    break by mov-est-acbm.it-codigo
          by mov-est-acbd.cod-tipo-def:

    if mov-est-acbd.classific = "Rg" then
       assign de-tot-reg-it = de-tot-reg-it + mov-est-acbd.qtd-defeit
              de-tot-reg-tip = de-tot-reg-tip + mov-est-acbd.qtd-defeit.
    else
    if mov-est-acbd.classific = "Rt" then
       assign de-tot-ret-it = de-tot-ret-it + mov-est-acbd.qtd-defeit
              de-tot-ret-tip = de-tot-ret-tip + mov-est-acbd.qtd-defeit.
    else
       assign de-tot-ld-it = de-tot-ld-it + mov-est-acbd.qtd-defeit
              de-tot-ld-tip = de-tot-ld-tip + mov-est-acbd.qtd-defeit.
    
    if first-of(mov-est-acbm.it-codigo) then do:
       find item where
            item.it-codigo = mov-est-acbm.it-codigo no-lock no-error.
       display item.it-codigo
               item.desc-item
               with frame f-detalhe.
    end.
       
    if last-of(mov-est-acbd.cod-tipo-def) then do:
       find tipo-def where
            tipo-def.cod-tipo-def = mov-est-acbd.cod-tipo-def no-lock no-error.
       assign de-tot-def-tip = de-tot-reg-tip + 
                               de-tot-ld-tip  +
                               de-tot-ret-tip.
       display tipo-def.cod-tipo-def
               tipo-def.descricao
               de-tot-reg-tip
               de-tot-ld-tip
               de-tot-ret-tip
               de-tot-def-tip
               with frame f-detalhe.
       down with frame f-detalhe.
        
       assign de-tot-reg-tip = 0
              de-tot-ld-tip  = 0
              de-tot-ret-tip = 0
              de-tot-def-tip = 0.
    end.
    
    if last-of(mov-est-acbm.it-codigo) then do:
       assign de-tot-def-it = de-tot-reg-it + 
                              de-tot-ld-it  +
                              de-tot-ret-it.
       display "Total do Item"  @ tipo-def.descricao
               de-tot-reg-it    @ de-tot-reg-tip
               de-tot-ld-it     @ de-tot-ld-tip
               de-tot-ret-it    @ de-tot-ret-tip
               de-tot-def-it    @ de-tot-def-tip
               with frame f-detalhe.
       down 2 with frame f-detalhe.
    
       if de-tot-reg-ger <> 0 then
          assign de-aux-perc1 = de-tot-reg-it / de-tot-reg-ger * 100.
       else 
          assign de-aux-perc1 = 0.
       if de-tot-ld-ger <> 0 then
          assign de-aux-perc2 = de-tot-ld-it / de-tot-ld-ger * 100.
       else 
          assign de-aux-perc2 = 0.
       if de-tot-ret-ger <> 0 then
          assign de-aux-perc3 = de-tot-ret-it / de-tot-ret-ger * 100.
       else 
          assign de-aux-perc3 = 0.
       if de-tot-def-ger <> 0 then
          assign de-aux-perc4 = de-tot-def-it / de-tot-def-ger * 100.
       else
          assign de-aux-perc4 = 0.
          
       display "% Sobre Total Geral"  @ tipo-def.descricao
               de-aux-perc1           @ de-tot-reg-tip
               de-aux-perc2           @ de-tot-ld-tip
               de-aux-perc3           @ de-tot-ret-tip
               de-aux-perc4           @ de-tot-def-tip
               with frame f-detalhe.
       down 2 with frame f-detalhe.
         
       assign de-tot-reg-it = 0
              de-tot-ld-it  = 0
              de-tot-ret-it = 0
              de-tot-def-it = 0.
    end.
end.
     
display "Total Geral"  @ item.desc-item
        de-tot-reg-ger @ de-tot-reg-tip
        de-tot-ld-ger  @ de-tot-ld-tip
        de-tot-ret-ger @ de-tot-ret-tip
        de-tot-def-ger @ de-tot-def-tip
        with frame f-detalhe.

assign de-tot-reg-ger = 0
       de-tot-ld-ger  = 0
       de-tot-ret-ger = 0.

IF tt-param.imp-param THEN DO:
   PAGE.
   display tt-param.fi-ini-it-codigo
           tt-param.fi-fin-it-codigo
           tt-param.fi-ini-cod-refer
           tt-param.fi-fin-cod-refer
           tt-param.fi-desenho
           tt-param.l-inc-exc
           tt-param.fi-ini-cod-tipo-def
           tt-param.fi-fin-cod-tipo-def
           tt-param.fi-ini-data-mov
           tt-param.fi-fin-data-mov
           tt-param.opc-artigo
           tt-param.tp-tecelagem
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

