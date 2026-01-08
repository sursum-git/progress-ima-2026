/* Programa: ESCE046.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Controle de Estoque
** Objetivo: Relatorio Defeitos por Tipo Defeito/Lote
** Autor...: Gilvando de Souza Araujo - Agosto/1997
** Obs.....: Especifico da CIA.RENASCENCA INDUSTRIAL
**
** Conversao para EMS 2.04:
**   Programa: ESCE046.P  =>  ESSP0074RP.P
**   Autor...: Gilvando Souza Araujo
**   Data....: 04/02/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESSP0074RP 2.04.00.000}

DEF BUFFER b-mov-est-acbd FOR mov-est-acbd.
DEF BUFFER b-mov-est-acbm FOR mov-est-acbm.
DEF BUFFER b-item-ext FOR item-ext.

define temp-table tt-param  no-undo
       field destino             as integer
       field arquivo             as char format "x(35)"
       field usuario             as char format "x(12)"
       field data-exec           as date
       field hora-exec           as integer
       field classifica          as integer
       field desc-classifica     as char format "x(40)"
       field fi-ini-it-codigo    like mov-est-acbm.it-codigo
       field fi-fin-it-codigo    like mov-est-acbm.it-codigo
       field fi-ini-cod-refer    like mov-est-acbm.cod-refer
       field fi-fin-cod-refer    like mov-est-acbm.cod-refer
       field fi-desenho          as char format "x(4)"
       FIELD l-inc-exc           AS LOG FORMAT "Inclusive/Exclusive"
       FIELD fi-ini-cod-tipo-def LIKE mov-est-acbd.cod-tipo-def
       FIELD fi-fin-cod-tipo-def LIKE mov-est-acbd.cod-tipo-def
       FIELD fi-ini-data-mov     LIKE mov-est-acbm.data-mov
       FIELD fi-fin-data-mov     LIKE mov-est-acbm.data-mov
       FIELD l-tipo-rel          AS LOG FORMAT "Sint‚tico/Anal¡tico"
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

def var de-tot-reg-def as dec format ">>>>>>,>>9.99".
def var de-tot-ld-def  as dec format ">>>>>>,>>9.99".
def var de-tot-ret-def as dec format ">>>>>>,>>9.99".
def var de-tot-def-def as dec format ">>>>>>,>>9.99".
def var de-lot-reg-def as dec format ">>>>>>,>>9.99".
def var de-lot-ld-def  as dec format ">>>>>>,>>9.99".
def var de-lot-ret-def as dec format ">>>>>>,>>9.99".
def var de-lot-def-def as dec format ">>>>>>,>>9.99".
def var de-tot-reg-tip as dec format ">>>>>>,>>9.99".
def var de-tot-ld-tip  as dec format ">>>>>>,>>9.99".
def var de-tot-ret-tip as dec format ">>>>>>,>>9.99".
def var de-tot-def-tip as dec format ">>>>>>,>>9.99".
def var de-lot-reg-tip as dec format ">>>>>>,>>9.99".
def var de-lot-ld-tip  as dec format ">>>>>>,>>9.99".
def var de-lot-ret-tip as dec format ">>>>>>,>>9.99".
def var de-lot-def-tip as dec format ">>>>>>,>>9.99".
def var de-tot-reg-ger as dec format ">>>>>>,>>9.99".
def var de-tot-ld-ger  as dec format ">>>>>>,>>9.99".
def var de-tot-ret-ger as dec format ">>>>>>,>>9.99".
def var de-tot-def-ger as dec format ">>>>>>,>>9.99".
def var de-aux-perc1 as dec format ">>9.99".
def var de-aux-perc2 as dec format ">>9.99".
def var de-aux-perc3 as dec format ">>9.99".
def var de-aux-perc4 as dec format ">>9.99".

form 
    "*--------------- Parƒmetros/Sele‡Æo ----------------*" SKIP
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
    tt-param.l-tipo-rel           LABEL "Tipo Relat¢rio..." SKIP
    tt-param.opc-artigo           LABEL "Tipo de Artigo..." SKIP
    tt-param.tp-tecelagem         LABEL "Tipo Tecelagem..." FORMAT "x(50)"
    with FRAME f-param side-labels no-box width 132 STREAM-IO.

form
    tipo-def.cod-tipo-def      label "Cod"
    tipo-def.descricao         label "Tipo defeito/Defeito"
    de-tot-reg-def             label "Regular"
    de-aux-perc1               label "%Part"
    de-tot-ld-def              label "Leve defeito"
    de-aux-perc2               label "%Part"
    de-tot-ret-def             label "Retalho"
    de-aux-perc3               label "%Part"
    de-tot-def-def             label "Total"
    de-aux-perc4               label "%Part"
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

{utp/ut-liter.i ESPECÖFICOS * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Defeitos_por_Tipo_de_Defeito/Lote * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).
 
for each mov-est-acbd
   where mov-est-acbd.data-mov      >= tt-param.fi-ini-data-mov
     and mov-est-acbd.data-mov      <= tt-param.fi-fin-data-mov
     and mov-est-acbd.cod-tipo-def  >= tt-param.fi-ini-cod-tipo-def
     and mov-est-acbd.cod-tipo-def  <= tt-param.fi-fin-cod-tipo-def
     and mov-est-acbd.it-codigo     >= tt-param.fi-ini-it-codigo
     and mov-est-acbd.it-codigo     <= tt-param.fi-fin-it-codigo
     and mov-est-acbd.cod-refer     >= tt-param.fi-ini-cod-refer
     and mov-est-acbd.cod-refer     <= tt-param.fi-fin-cod-refer
     and ((substr(mov-est-acbd.it-codigo,7,4) =  tt-param.fi-desenho and
                                                 tt-param.l-inc-exc = yes) or
          (substr(mov-est-acbd.it-codigo,7,4) <> tt-param.fi-desenho and
                                                 tt-param.l-inc-exc = no) or
                                                (tt-param.fi-desenho = ""))
   NO-LOCK,

   EACH mov-est-acbm NO-LOCK
                     WHERE mov-est-acbm.cod-estabel = mov-est-acbd.cod-estabel
                       AND mov-est-acbm.data-mov    = mov-est-acbd.data-mov
                       AND mov-est-acbm.num-lote    = mov-est-acbd.num-lote
                       AND mov-est-acbm.it-codigo   = mov-est-acbd.it-codigo
                       AND mov-est-acbm.cod-refer   = mov-est-acbd.cod-refer
                       AND INDEX(tt-param.tp-tecelagem,mov-est-acbm.tipo-tear) <> 0,

   EACH item-ext WHERE item-ext.it-codigo = mov-est-acbd.it-codigo
                   AND (item-ext.indigo = YES AND tt-param.opc-artigo = "I" OR
                        item-ext.indigo = NO  AND tt-param.opc-artigo = "O" OR
                                                  tt-param.opc-artigo = "A")
                 NO-LOCK:
    
   run pi-acompanhar in h-acomp (input "Item: " + mov-est-acbd.it-codigo + " Data: " +
                                       STRING(mov-est-acbd.data-mov)).

   if mov-est-acbd.classific = "Rg" then
      assign de-tot-reg-ger = de-tot-reg-ger + mov-est-acbd.qtd-defeit.
   else
   if mov-est-acbd.classific = "Rt" then
      assign de-tot-ret-ger = de-tot-ret-ger + mov-est-acbd.qtd-defeit.
   else
      assign de-tot-ld-ger = de-tot-ld-ger + mov-est-acbd.qtd-defeit.
end.
assign de-tot-def-ger = de-tot-reg-ger + de-tot-ld-ger + de-tot-ret-ger.

for each mov-est-acbd
   where mov-est-acbd.data-mov     >= tt-param.fi-ini-data-mov
     and mov-est-acbd.data-mov     <= tt-param.fi-fin-data-mov
     and mov-est-acbd.cod-tipo-def >= tt-param.fi-ini-cod-tipo-def
     and mov-est-acbd.cod-tipo-def <= tt-param.fi-fin-cod-tipo-def
     and mov-est-acbd.it-codigo    >= tt-param.fi-ini-it-codigo
     and mov-est-acbd.it-codigo    <= tt-param.fi-fin-it-codigo
     and mov-est-acbd.cod-refer    >= tt-param.fi-ini-cod-refer
     and mov-est-acbd.cod-refer    <= tt-param.fi-fin-cod-refer
     and ((substr(mov-est-acbd.cod-refer,3,4) =  tt-param.fi-desenho and
                                                 tt-param.l-inc-exc = yes) or
          (substr(mov-est-acbd.cod-refer,3,4) <> tt-param.fi-desenho and
                                                 tt-param.l-inc-exc = no) or
                                                (tt-param.fi-desenho = ""))
   no-lock,

   
    EACH mov-est-acbm NO-LOCK
                      WHERE mov-est-acbm.cod-estabel = mov-est-acbd.cod-estabel
                        AND mov-est-acbm.data-mov    = mov-est-acbd.data-mov
                        AND mov-est-acbm.num-lote    = mov-est-acbd.num-lote
                        AND mov-est-acbm.it-codigo   = mov-est-acbd.it-codigo
                        AND mov-est-acbm.cod-refer   = mov-est-acbd.cod-refer
                        AND INDEX(tt-param.tp-tecelagem,mov-est-acbm.tipo-tear) <> 0,

   EACH item-ext WHERE item-ext.it-codigo = mov-est-acbd.it-codigo
                   AND (item-ext.indigo = YES AND tt-param.opc-artigo = "I" OR
                        item-ext.indigo = NO  AND tt-param.opc-artigo = "O" OR
                                                  tt-param.opc-artigo = "A")
                 NO-LOCK

   break by mov-est-acbd.cod-tipo-def
         by mov-est-acbd.cod-defeito
         by mov-est-acbd.num-lote:

   if mov-est-acbd.classific = "Rg" then
      assign de-tot-reg-def = de-tot-reg-def + mov-est-acbd.qtd-defeit.
   else
   if mov-est-acbd.classific = "Rt" then
      assign de-tot-ret-def = de-tot-ret-def + mov-est-acbd.qtd-defeit.
   else
      assign de-tot-ld-def = de-tot-ld-def + mov-est-acbd.qtd-defeit.

   if first-of(mov-est-acbd.cod-tipo-def) then do:
      find tipo-def 
           where tipo-def.cod-tipo-def = mov-est-acbd.cod-tipo-def
                 no-lock no-error.
      display tipo-def.cod-tipo-def
              tipo-def.descricao
              with frame f-detalhe.
      down with frame f-detalhe.
      assign de-tot-reg-tip = 0
             de-tot-ld-tip  = 0
             de-tot-ret-tip = 0.

      for each b-mov-est-acbd
         where b-mov-est-acbd.data-mov     >= tt-param.fi-ini-data-mov
           and b-mov-est-acbd.data-mov     <= tt-param.fi-fin-data-mov
           and b-mov-est-acbd.cod-tipo-def >= tt-param.fi-ini-cod-tipo-def
           and b-mov-est-acbd.cod-tipo-def <= tt-param.fi-fin-cod-tipo-def
           and b-mov-est-acbd.it-codigo    >= tt-param.fi-ini-it-codigo
           and b-mov-est-acbd.it-codigo    <= tt-param.fi-fin-it-codigo
           and b-mov-est-acbd.cod-refer    >= tt-param.fi-ini-cod-refer
           and b-mov-est-acbd.cod-refer    <= tt-param.fi-fin-cod-refer
           and ((substr(b-mov-est-acbd.cod-refer,3,4) =  tt-param.fi-desenho and
                                                         tt-param.l-inc-exc = yes) or
                (substr(b-mov-est-acbd.cod-refer,3,4) <> tt-param.fi-desenho and
                                                         tt-param.l-inc-exc = no) or
                                                        (tt-param.fi-desenho = ""))
           and b-mov-est-acbd.cod-tipo-def            =  mov-est-acbd.cod-tipo-def
                NO-LOCK,

           EACH b-mov-est-acbm NO-LOCK
                               WHERE b-mov-est-acbm.cod-estabel = b-mov-est-acbd.cod-estabel
                                 AND b-mov-est-acbm.data-mov    = b-mov-est-acbd.data-mov
                                 AND b-mov-est-acbm.num-lote    = b-mov-est-acbd.num-lote
                                 AND b-mov-est-acbm.it-codigo   = b-mov-est-acbd.it-codigo
                                 AND b-mov-est-acbm.cod-refer   = b-mov-est-acbd.cod-refer
                                 AND INDEX(tt-param.tp-tecelagem,b-mov-est-acbm.tipo-tear) <> 0,

           EACH b-item-ext WHERE b-item-ext.it-codigo = b-mov-est-acbd.it-codigo
                             AND (b-item-ext.indigo = YES AND tt-param.opc-artigo = "I" OR
                                  b-item-ext.indigo = NO  AND tt-param.opc-artigo = "O" OR
                                                              tt-param.opc-artigo = "A")
                           NO-LOCK:

          if b-mov-est-acbd.classific = "Rg" then
             assign de-tot-reg-tip = de-tot-reg-tip + b-mov-est-acbd.qtd-defeit.
          else
          if b-mov-est-acbd.classific = "Rt" then
             assign de-tot-ret-tip = de-tot-ret-tip + b-mov-est-acbd.qtd-defeit.
          else
             assign de-tot-ld-tip = de-tot-ld-tip + b-mov-est-acbd.qtd-defeit.
      end.
      assign de-tot-def-tip = de-tot-reg-tip + 
                              de-tot-ld-tip +
                              de-tot-ret-tip.
   end.   

   if tt-param.l-tipo-rel = NO then do: /* Anal¡tico */
   
      if mov-est-acbd.classific = "Rg" then
         assign de-lot-reg-def = de-lot-reg-def + mov-est-acbd.qtd-defeit.
      else
      if mov-est-acbd.classific = "Rt" then
         assign de-lot-ret-def = de-lot-ret-def + mov-est-acbd.qtd-defeit.
      else
         assign de-lot-ld-def = de-lot-ld-def + mov-est-acbd.qtd-defeit.

      if last-of(mov-est-acbd.num-lote) then do:
         assign de-lot-def-def = de-lot-reg-def +
                                 de-lot-ld-def  +
                                 de-lot-ret-def.
         if de-tot-def-def <> 0 then
            assign de-aux-perc1 = de-lot-reg-def / de-tot-def-tip * 100.
         else
            assign de-aux-perc1 = 0.
         if de-tot-ld-def <> 0 then
            assign de-aux-perc2 = de-lot-ld-def / de-tot-ld-tip * 100.
         else
            assign de-aux-perc2 = 0.
         if de-tot-ret-def <> 0 then
            assign de-aux-perc3 = de-lot-ret-def / de-tot-ret-tip * 100.
         else
            assign de-aux-perc3 = 0.
         if de-tot-def-def <> 0 then
            assign de-aux-perc4 = de-lot-def-def / de-tot-def-tip * 100.
         else
            assign de-aux-perc4 = 0.

         display string(mov-est-acbd.num-lote) @ tipo-def.descricao
                 de-lot-reg-def                @ de-tot-reg-def
                 de-aux-perc1
                 de-lot-ld-def                 @ de-tot-ld-def 
                 de-aux-perc2
                 de-lot-ret-def                @ de-tot-ret-def
                 de-aux-perc3
                 de-lot-def-def                @ de-tot-def-def
                 de-aux-perc4
                 with frame f-detalhe.
         down with frame f-detalhe.
       
         assign de-lot-reg-def = 0
                de-lot-ld-def  = 0
                de-lot-ret-def = 0.
      end.
   end.

   if last-of(mov-est-acbd.cod-defeito) then do: 
      find defeito
           where defeito.cod-tipo-def = mov-est-acbd.cod-tipo-def
             and defeito.cod-defeito  = mov-est-acbd.cod-defeito
           no-lock no-error.
      assign de-tot-def-def = de-tot-reg-def + 
                              de-tot-ld-def +
                              de-tot-ret-def.
      if de-tot-reg-tip <> 0 then
         assign de-aux-perc1 = de-tot-reg-def / de-tot-reg-tip * 100.
      else
         assign de-aux-perc1 = 0.
      if de-tot-ld-tip <> 0 then
         assign de-aux-perc2 = de-tot-ld-def / de-tot-ld-tip * 100.
      else
         assign de-aux-perc2 = 0.
      if de-tot-ret-tip <> 0 then
         assign de-aux-perc3 = de-tot-ret-def / de-tot-ret-tip * 100.
      else
         assign de-aux-perc3 = 0.
      if de-tot-def-tip <> 0 then
         assign de-aux-perc4 = de-tot-def-def / de-tot-def-tip * 100.
      else
         assign de-aux-perc4 = 0.

      display defeito.cod-defeito @ tipo-def.cod-tipo-def format "99"
              defeito.descricao   @ tipo-def.descricao
              de-tot-reg-def
              de-aux-perc1
              de-tot-ld-def
              de-aux-perc2
              de-tot-ret-def
              de-aux-perc3
              de-tot-def-def
              de-aux-perc4
              with frame f-detalhe.
      down with frame f-detalhe.
       
      assign de-tot-reg-def = 0
             de-tot-ld-def  = 0
             de-tot-ret-def = 0.
   end.
   
   if last-of(mov-est-acbd.cod-tipo-def) then do:
      assign de-tot-def-tip = de-tot-reg-tip + 
                              de-tot-ld-tip +
                              de-tot-ret-tip.
      display "Total do tipo defeito" @ tipo-def.descricao
              de-tot-reg-tip          @ de-tot-reg-def
              de-tot-ld-tip           @ de-tot-ld-def
              de-tot-ret-tip          @ de-tot-ret-def
              de-tot-def-tip          @ de-tot-def-def
              with frame f-detalhe.
      down 2 with frame f-detalhe.

      if de-tot-reg-ger <> 0 then
         assign de-aux-perc1 = de-tot-reg-tip / de-tot-reg-ger * 100.
      else 
         assign de-aux-perc1 = 0.
      if de-tot-ld-ger <> 0 then
         assign de-aux-perc2 = de-tot-ld-tip / de-tot-ld-ger * 100.
      else 
         assign de-aux-perc2 = 0.
      if de-tot-ret-ger <> 0 then
         assign de-aux-perc3 = de-tot-ret-tip / de-tot-ret-ger * 100.
      else 
         assign de-aux-perc3 = 0.
      if de-tot-def-ger <> 0 then
         assign de-aux-perc4 = de-tot-def-tip / de-tot-def-ger * 100.
      else
         assign de-aux-perc4 = 0.
         
      display "% Sobre Total Geral"  @ tipo-def.descricao
              de-aux-perc1           
              de-aux-perc2           
              de-aux-perc3           
              de-aux-perc4
              with frame f-detalhe.
      down 2 with frame f-detalhe.
       
      assign de-tot-reg-tip = 0
             de-tot-ld-tip  = 0
             de-tot-ret-tip = 0.
   end.
end.
    
display "Total Geral"  @ tipo-def.descricao
        de-tot-reg-ger @ de-tot-reg-def
        de-tot-ld-ger  @ de-tot-ld-def
        de-tot-ret-ger @ de-tot-ret-def
        de-tot-def-ger @ de-tot-def-def
        with frame f-detalhe.
down with frame f-detalhe.

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
           tt-param.l-tipo-rel
           tt-param.opc-artigo
           tt-param.tp-tecelagem
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

