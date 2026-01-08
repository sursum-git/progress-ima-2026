/* Programa: ESSP0175.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: RevisÆo de Produtos Acabados
** Objetivo: Relatorio Defeitos por Tipo Defeito/Item
** Autor...: Gilvando de Souza Araujo - Maio/2008
** Obs.....: Especifico da TEAR TÒXTIL INDéSTRIA E COMRCIO LTDA.
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESSP0175RP 2.04.00.000}

DEF BUFFER b-mov-est-acbd FOR mov-est-acbd.
DEF BUFFER b-mov-est-acbm FOR mov-est-acbm.
DEF BUFFER b-item-ext FOR item-ext.

DEFINE TEMP-TABLE tt-param  NO-UNDO
       FIELD destino             AS INTEGER
       FIELD arquivo             AS CHAR FORMAT "x(35)"
       FIELD usuario             AS CHAR FORMAT "x(12)"
       FIELD data-exec           AS DATE
       FIELD hora-exec           AS INTEGER
       FIELD classifica          AS INTEGER
       FIELD desc-classifica     AS CHAR FORMAT "x(40)"
       FIELD fi-ini-cod-estabel  LIKE mov-est-acbm.cod-estabel
       FIELD fi-fin-cod-estabel  LIKE mov-est-acbm.cod-estabel
       FIELD fi-ini-it-codigo    LIKE mov-est-acbm.it-codigo
       FIELD fi-fin-it-codigo    LIKE mov-est-acbm.it-codigo
       FIELD fi-ini-cod-refer    LIKE mov-est-acbm.cod-refer
       FIELD fi-fin-cod-refer    LIKE mov-est-acbm.cod-refer
       FIELD fi-desenho          AS CHAR FORMAT "x(4)"
       FIELD l-inc-exc           AS LOG FORMAT "Inclusive/Exclusive"
       FIELD fi-ini-cod-tipo-def LIKE mov-est-acbd.cod-tipo-def
       FIELD fi-fin-cod-tipo-def LIKE mov-est-acbd.cod-tipo-def
       FIELD fi-ini-data-mov     LIKE mov-est-acbm.data-mov
       FIELD fi-fin-data-mov     LIKE mov-est-acbm.data-mov
       FIELD fi-ini-hora-mov     AS CHAR FORMAT "99:99:99"
       FIELD fi-fin-hora-mov     AS CHAR FORMAT "99:99:99"
       FIELD l-tipo-rel          AS LOG FORMAT "Sint‚tico/Anal¡tico"
       FIELD l-excluir-ob        AS LOG FORMAT "Sim/NÆo"
       FIELD cam-nom-lista       AS CHAR FORMAT "x(45)"
       FIELD opc-artigo          AS CHAR FORMAT "x"
       FIELD tp-tecelagem        AS CHAR
       FIELD imp-param           AS LOG.

define temp-table tt-raw-digita
       field raw-digita as raw.

DEF TEMP-TABLE tt-excluir-ob
    FIELD num-lote LIKE mov-est-acbm.num-lote
    INDEX ch-excluir-ob num-lote.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

DEF TEMP-TABLE tt-work
    FIELD cod-tipo-def LIKE mov-est-acbd.cod-tipo-def 
    FIELD cod-defeito  LIKE mov-est-acbd.cod-defeito
    FIELD qtd-total    AS DEC
    INDEX ch-work cod-tipo-def 
                  cod-defeito.
DEF TEMP-TABLE tt-work1
    FIELD cod-tipo-def LIKE mov-est-acbd.cod-tipo-def 
    FIELD cod-defeito  LIKE mov-est-acbd.cod-defeito
    FIELD it-codigo    LIKE mov-est-acbd.it-codigo
    FIELD qtd-total    AS DEC
    INDEX ch-work1 cod-tipo-def 
                   cod-defeito
                   it-codigo.

DEF TEMP-TABLE tt-work2
    FIELD it-codigo LIKE mov-est-acbm.it-codigo 
    FIELD qtd-total AS DEC
    INDEX ch-work it-codigo.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* include padrÆo para impressÆo de campos editores em relat¢rio  */
{include/tt-edit.i}

/* defini‡Æo de vari veis  */
def var h-acomp as handle no-undo.

def var de-tot-prod     as dec format ">>>>>>,>>9.99".
def var de-tot-prd-item as dec format ">>>>>>,>>9.99".
def var de-tot-prd-ger  as dec format ">>>>>>,>>9.99".
def var de-tot-reg-def  as dec format ">>>>>>,>>9.99".
def var de-tot-ld-def   as dec format ">>>>>>,>>9.99".
def var de-tot-ret-def  as dec format ">>>>>>,>>9.99".
def var de-tot-def-def  as dec format ">>>>>>,>>9.99".
def var de-item-reg-def as dec format ">>>>>>,>>9.99".
def var de-item-ld-def  as dec format ">>>>>>,>>9.99".
def var de-item-ret-def as dec format ">>>>>>,>>9.99".
def var de-item-def-def as dec format ">>>>>>,>>9.99".
def var de-tot-reg-tip  as dec format ">>>>>>,>>9.99".
def var de-tot-ld-tip   as dec format ">>>>>>,>>9.99".
def var de-tot-ret-tip  as dec format ">>>>>>,>>9.99".
def var de-tot-def-tip  as dec format ">>>>>>,>>9.99".
def var de-item-reg-tip as dec format ">>>>>>,>>9.99".
def var de-item-ld-tip  as dec format ">>>>>>,>>9.99".
def var de-item-ret-tip as dec format ">>>>>>,>>9.99".
def var de-item-def-tip as dec format ">>>>>>,>>9.99".
def var de-tot-reg-ger  as dec format ">>>>>>,>>9.99".
def var de-tot-ld-ger   as dec format ">>>>>>,>>9.99".
def var de-tot-ret-ger  as dec format ">>>>>>,>>9.99".
def var de-tot-def-ger  as dec format ">>>>>>,>>9.99".
def var de-aux-perc1    as dec format ">>9.99".
def var de-aux-perc2    as dec format ">>9.99".
def var de-aux-perc3    as dec format ">>9.99".
def var de-aux-perc4    as dec format ">>9.99".
def var de-aux-perc5    as dec format ">>9.99".
DEF VAR c-item          AS CHAR.
DEF VAR i-hora1         AS INT.
DEF VAR i-hora2         AS INT.

DEF STREAM aux.

form 
    "*--------------- Parƒmetros/Sele‡Æo ----------------*" SKIP
    tt-param.fi-ini-cod-estabel   label "Estabelecimento.."
    "A"  AT 36                    
    tt-param.fi-fin-cod-estabel   NO-LABELS SKIP
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
    tt-param.fi-ini-hora-mov      label "Hora do Movimento"
    "A"  AT 36                    
    tt-param.fi-fin-hora-mov      NO-LABELS SKIP
    tt-param.l-tipo-rel           LABEL "Tipo Relat¢rio..." SKIP
    tt-param.opc-artigo           LABEL "Tipo de Artigo..." SKIP
    tt-param.tp-tecelagem         LABEL "Tipo Tecelagem..." FORMAT "x(50)" SKIP
    tt-param.l-excluir-ob         LABEL "Excluir OB......."
    with FRAME f-param side-labels no-box width 132 STREAM-IO.

form
    tipo-def.cod-tipo-def label "Cod"
    tipo-def.descricao    label "Tipo defeito/Defeito" FORMAT "x(30)"
    de-tot-prd-item       LABEL "Producao"             FORMAT ">>>>>>,>>9.99"
    de-tot-reg-def        label "Regular"              FORMAT ">>>>>>,>>9.99"
    de-aux-perc1          label "%Part"                FORMAT ">>9.9"
    de-tot-ld-def         label "Leve defeito"         FORMAT ">>>>>,>>9.99"
    de-aux-perc2          label "%Part"                FORMAT ">>9.9"
    de-tot-ret-def        label "Retalho"              FORMAT ">>>>>,>>9.99"
    de-aux-perc3          label "%Part"                FORMAT ">>9.9"
    de-tot-def-def        label "Total"                FORMAT ">>>>>,>>9.99"
    de-aux-perc4          label "%Part"                FORMAT ">>9.9"
    de-aux-perc5          LABEL "%S/Prd"               FORMAT ">>9.9"
    with no-box 55 down width 132 STREAM-IO frame f-detalhe no-label.

IF tt-param.l-excluir-ob = YES THEN DO:
   INPUT STREAM aux FROM VALUE(tt-param.cam-nom-lista) CONVERT SOURCE "ibm850". 
   SET STREAM aux ^.
   REPEAT:
      CREATE tt-excluir-ob.
      IMPORT STREAM aux DELIMITER ";" tt-excluir-ob.
   END.
   INPUT STREAM aux CLOSE.
   FOR EACH tt-excluir-ob WHERE tt-excluir-ob.num-lote = 0:
      DELETE tt-excluir-ob.  
   END.
END.

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
{utp/ut-liter.i Defeitos_por_Tipo_de_Defeito/Item * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).
 
ASSIGN i-hora1 = INT(SUBSTR(tt-param.fi-ini-hora-mov,1,2)) * 3600 +
                 INT(SUBSTR(tt-param.fi-ini-hora-mov,3,2)) * 60 +
                 INT(SUBSTR(tt-param.fi-ini-hora-mov,5,2)).

ASSIGN i-hora2 = INT(SUBSTR(tt-param.fi-fin-hora-mov,1,2)) * 3600 +
                 INT(SUBSTR(tt-param.fi-fin-hora-mov,3,2)) * 60 +
                 INT(SUBSTR(tt-param.fi-fin-hora-mov,5,2)).

for each mov-est-acbd
   WHERE mov-est-acbd.cod-estabel   >= tt-param.fi-ini-cod-estabel
     AND mov-est-acbd.cod-estabel   <= tt-param.fi-fin-cod-estabel
     AND mov-est-acbd.data-mov      >= tt-param.fi-ini-data-mov
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
     AND NOT CAN-FIND(tt-excluir-ob WHERE tt-excluir-ob.num-lote = mov-est-acbd.num-lote)
   NO-LOCK,

   EACH mov-est-acbm WHERE mov-est-acbm.cod-estabel =  mov-est-acbd.cod-estabel
                       AND mov-est-acbm.data-mov    =  mov-est-acbd.data-mov
                       AND mov-est-acbm.hora-mov    >= i-hora1
                       AND mov-est-acbm.hora-mov    <= i-hora2
                       AND mov-est-acbm.num-lote    =  mov-est-acbd.num-lote
                       AND mov-est-acbm.it-codigo   =  mov-est-acbd.it-codigo
                       AND mov-est-acbm.cod-refer   =  mov-est-acbd.cod-refer
                       AND INDEX(tt-param.tp-tecelagem,mov-est-acbm.tipo-tear) <> 0
                      /* NO-LOCK,

   EACH item-ext WHERE item-ext.it-codigo = mov-est-acbd.it-codigo
                   AND (item-ext.indigo = YES AND tt-param.opc-artigo = "I" OR
                        item-ext.indigo = NO  AND tt-param.opc-artigo = "O" OR
                                                  tt-param.opc-artigo = "A") */
                 NO-LOCK:
    
   run pi-acompanhar in h-acomp (input "Defeitos - Item: " + mov-est-acbd.it-codigo + " Data: " +
                                       STRING(mov-est-acbd.data-mov)).

   if mov-est-acbd.classific = "Rg" then
      assign de-tot-reg-ger = de-tot-reg-ger + mov-est-acbd.qtd-defeit.
   else
   if mov-est-acbd.classific = "Rt" then
      assign de-tot-ret-ger = de-tot-ret-ger + mov-est-acbd.qtd-defeit.
   else
      assign de-tot-ld-ger = de-tot-ld-ger + mov-est-acbd.qtd-defeit.

   /*----- Cria arquivo tempor rio para auxiliar na classifica‡Æo do relat¢rio -----*/
   FIND FIRST tt-work WHERE tt-work.cod-tipo-def = mov-est-acbd.cod-tipo-def  
                        AND tt-work.cod-defeito  = mov-est-acbd.cod-defeito  
                      NO-LOCK NO-ERROR.
   IF NOT AVAIL tt-work THEN DO:
      CREATE tt-work.
      ASSIGN tt-work.cod-tipo-def = mov-est-acbd.cod-tipo-def  
             tt-work.cod-defeito  = mov-est-acbd.cod-defeito.
   END.
   ASSIGN tt-work.qtd-total = tt-work.qtd-total + mov-est-acbd.qtd-defeit.

   FIND FIRST tt-work1 WHERE tt-work1.cod-tipo-def = mov-est-acbd.cod-tipo-def  
                         AND tt-work1.cod-defeito  = mov-est-acbd.cod-defeito  
                         AND tt-work1.it-codigo    = mov-est-acbd.it-codigo
                       NO-LOCK NO-ERROR.
   IF NOT AVAIL tt-work1 THEN DO:
      CREATE tt-work1.
      ASSIGN tt-work1.cod-tipo-def = mov-est-acbd.cod-tipo-def  
             tt-work1.cod-defeito  = mov-est-acbd.cod-defeito
             tt-work1.it-codigo    = mov-est-acbd.it-codigo.
   END.
   ASSIGN tt-work1.qtd-total = tt-work1.qtd-total + mov-est-acbd.qtd-defeit.
END.
assign de-tot-def-ger = de-tot-reg-ger + de-tot-ld-ger + de-tot-ret-ger.

FOR EACH mov-est-acbm WHERE
         mov-est-acbm.data-mov  >= tt-param.fi-ini-data-mov  AND
         mov-est-acbm.data-mov  <= tt-param.fi-fin-data-mov  AND 
         mov-est-acbm.hora-mov  >= i-hora1                   AND
         mov-est-acbm.hora-mov  <= i-hora2                   AND
         mov-est-acbm.it-codigo >= tt-param.fi-ini-it-codigo AND
         mov-est-acbm.it-codigo <= tt-param.fi-fin-it-codigo AND
         mov-est-acbm.cod-refer >= tt-param.fi-ini-cod-refer AND
         mov-est-acbm.cod-refer <= tt-param.fi-fin-cod-refer AND
         INDEX(tt-param.tp-tecelagem,mov-est-acbm.tipo-tear) <> 0 AND
         NOT CAN-FIND(tt-excluir-ob WHERE tt-excluir-ob.num-lote = mov-est-acbm.num-lote)
   /* NO-LOCK,
    EACH item-ext WHERE 
         item-ext.it-codigo = mov-est-acbm.it-codigo AND
         (item-ext.indigo = YES AND tt-param.opc-artigo = "I" OR
          item-ext.indigo = NO  AND tt-param.opc-artigo = "O" OR
          tt-param.opc-artigo = "A") */
          NO-LOCK:

    RUN pi-acompanhar IN h-acomp (INPUT "Produ‡Æo - Item: " + mov-est-acbm.it-codigo + " Data: " +
                                        STRING(mov-est-acbm.data-mov)).

    ASSIGN de-tot-prod = de-tot-prod + mov-est-acbm.qtd-tot-perf +
                                       mov-est-acbm.qtd-tot-def +
                                       mov-est-acbm.qtd-tot-sob.

    FIND FIRST tt-work2 WHERE
               tt-work2.it-codigo = mov-est-acbm.it-codigo  NO-LOCK NO-ERROR.
    IF NOT AVAIL tt-work2 THEN DO:
       CREATE tt-work2.
       ASSIGN tt-work2.it-codigo = mov-est-acbm.it-codigo.
    END.
    ASSIGN tt-work2.qtd-total = tt-work2.qtd-total + (mov-est-acbm.qtd-tot-perf +
                                                      mov-est-acbm.qtd-tot-def +
                                                      mov-est-acbm.qtd-tot-sob).
END.

for each mov-est-acbd
   WHERE mov-est-acbd.cod-estabel  >= tt-param.fi-ini-cod-estabel
     AND mov-est-acbd.cod-estabel  <= tt-param.fi-fin-cod-estabel
     AND mov-est-acbd.data-mov     >= tt-param.fi-ini-data-mov
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
     AND NOT CAN-FIND(tt-excluir-ob WHERE tt-excluir-ob.num-lote = mov-est-acbd.num-lote)
   NO-LOCK,
    
   EACH mov-est-acbm WHERE mov-est-acbm.cod-estabel =  mov-est-acbd.cod-estabel
                       AND mov-est-acbm.data-mov    =  mov-est-acbd.data-mov
                       AND mov-est-acbm.hora-mov    >= i-hora1
                       AND mov-est-acbm.hora-mov    <= i-hora2
                       AND mov-est-acbm.num-lote    =  mov-est-acbd.num-lote
                       AND mov-est-acbm.it-codigo   =  mov-est-acbd.it-codigo
                       AND mov-est-acbm.cod-refer   =  mov-est-acbd.cod-refer
                       AND INDEX(tt-param.tp-tecelagem,mov-est-acbm.tipo-tear) <> 0
                     NO-LOCK,

   /* EACH item-ext WHERE item-ext.it-codigo = mov-est-acbd.it-codigo
                   AND (item-ext.indigo = YES AND tt-param.opc-artigo = "I" OR
                        item-ext.indigo = NO  AND tt-param.opc-artigo = "O" OR
                                                  tt-param.opc-artigo = "A")
                 NO-LOCK, */
   EACH tt-work WHERE tt-work.cod-tipo-def = mov-est-acbd.cod-tipo-def 
                  AND tt-work.cod-defeito  = mov-est-acbd.cod-defeito,

   EACH tt-work1 WHERE tt-work1.cod-tipo-def = mov-est-acbd.cod-tipo-def 
                   AND tt-work1.cod-defeito  = mov-est-acbd.cod-defeito
                   AND tt-work1.it-codigo    = mov-est-acbd.it-codigo

   break by mov-est-acbd.cod-tipo-def
         BY tt-work.qtd-total DESCEND
         by mov-est-acbd.cod-defeito
         BY tt-work1.qtd-total DESCEND
         by mov-est-acbd.it-codigo:

   run pi-acompanhar in h-acomp (input "Resumo - Item: " + mov-est-acbd.it-codigo + " Data: " +
                                        STRING(mov-est-acbd.data-mov)).

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
         WHERE b-mov-est-acbd.cod-estabel  >= tt-param.fi-ini-cod-estabel
           AND b-mov-est-acbd.cod-estabel  <= tt-param.fi-fin-cod-estabel
           AND b-mov-est-acbd.data-mov     >= tt-param.fi-ini-data-mov
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

          EACH b-mov-est-acbm WHERE b-mov-est-acbm.cod-estabel =  b-mov-est-acbd.cod-estabel
                                AND b-mov-est-acbm.data-mov    =  b-mov-est-acbd.data-mov
                                AND b-mov-est-acbm.hora-mov    >= i-hora1
                                AND b-mov-est-acbm.hora-mov    <= i-hora2
                                AND b-mov-est-acbm.num-lote    =  b-mov-est-acbd.num-lote
                                AND b-mov-est-acbm.it-codigo   =  b-mov-est-acbd.it-codigo
                                AND b-mov-est-acbm.cod-refer   =  b-mov-est-acbd.cod-refer
                                AND INDEX(tt-param.tp-tecelagem,b-mov-est-acbm.tipo-tear) <> 0
                                AND NOT CAN-FIND(tt-excluir-ob WHERE tt-excluir-ob.num-lote = mov-est-acbm.num-lote)
                              /* NO-LOCK,

           EACH b-item-ext WHERE b-item-ext.it-codigo = b-mov-est-acbd.it-codigo
                             AND (b-item-ext.indigo = YES AND tt-param.opc-artigo = "I" OR
                                  b-item-ext.indigo = NO  AND tt-param.opc-artigo = "O" OR
                                                              tt-param.opc-artigo = "A") */
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

   if FIRST-OF(mov-est-acbd.cod-defeito) then do: 
      find defeito
           where defeito.cod-tipo-def = mov-est-acbd.cod-tipo-def
             and defeito.cod-defeito  = mov-est-acbd.cod-defeito
           no-lock no-error.
      display defeito.cod-defeito @ tipo-def.cod-tipo-def format "99"
              defeito.descricao   @ tipo-def.descricao
              with frame f-detalhe.
      IF tt-param.l-tipo-rel = NO THEN
         DOWN WITH FRAME f-detalhe.
   END.

   if tt-param.l-tipo-rel = NO then do: /* Anal¡tico */
      if mov-est-acbd.classific = "Rg" then
         assign de-item-reg-def = de-item-reg-def + mov-est-acbd.qtd-defeit.
      else
      if mov-est-acbd.classific = "Rt" then
         assign de-item-ret-def = de-item-ret-def + mov-est-acbd.qtd-defeit.
      else
         assign de-item-ld-def = de-item-ld-def + mov-est-acbd.qtd-defeit.

      if last-of(mov-est-acbd.it-codigo) then do:
         assign de-item-def-def = de-item-reg-def +
                                  de-item-ld-def  +
                                  de-item-ret-def.
         if de-tot-def-tip <> 0 then
            assign de-aux-perc1 = de-item-reg-def / de-tot-def-tip * 100.
         else
            assign de-aux-perc1 = 0.
         if de-tot-ld-tip <> 0 then
            assign de-aux-perc2 = de-item-ld-def / de-tot-ld-tip * 100.
         else
            assign de-aux-perc2 = 0.
         if de-tot-ret-tip <> 0 then
            assign de-aux-perc3 = de-item-ret-def / de-tot-ret-tip * 100.
         else
            assign de-aux-perc3 = 0.
         if de-tot-def-tip <> 0 then
            assign de-aux-perc4 = de-item-def-def / de-tot-def-tip * 100.
         else
            assign de-aux-perc4 = 0.
         if tt-work2.qtd-total <> 0 then
            assign de-aux-perc5 = de-item-def-def / tt-work2.qtd-total * 100.
         else
            assign de-aux-perc5 = 0.

         FIND ITEM WHERE ITEM.it-codigo = mov-est-acbd.it-codigo NO-LOCK NO-ERROR.
         FIND tt-work2 WHERE tt-work2.it-codigo = mov-est-acbd.it-codigo NO-LOCK NO-ERROR.
         ASSIGN c-item = TRIM(ITEM.it-codigo) + " " + ITEM.desc-item.
         display c-item             @ tipo-def.descricao
                 tt-work2.qtd-total @ de-tot-prd-item
                 de-item-reg-def    @ de-tot-reg-def
                 de-aux-perc1
                 de-item-ld-def     @ de-tot-ld-def 
                 de-aux-perc2
                 de-item-ret-def    @ de-tot-ret-def
                 de-aux-perc3
                 de-item-def-def    @ de-tot-def-def
                 de-aux-perc4
                 de-aux-perc5
                 with frame f-detalhe.
         down with frame f-detalhe.
       
         assign de-item-reg-def = 0
                de-item-ld-def  = 0
                de-item-ret-def = 0.
      end.
   end.

   if last-of(mov-est-acbd.cod-defeito) then do: 
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

      display "Total do defeito" WHEN tt-param.l-tipo-rel = NO @ tipo-def.descricao 
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
      if de-tot-prod <> 0 then
         assign de-aux-perc5 = de-tot-def-tip / de-tot-prod * 100.
      else
         assign de-aux-perc5 = 0.
         
      display "% Sobre Total Geral"  @ tipo-def.descricao
              de-aux-perc1           
              de-aux-perc2           
              de-aux-perc3           
              de-aux-perc4
              de-aux-perc5
              with frame f-detalhe.
      down 2 with frame f-detalhe.
       
      assign de-tot-reg-tip = 0
             de-tot-ld-tip  = 0
             de-tot-ret-tip = 0.
   end.
end.

IF de-tot-prod <> 0 then
   assign de-aux-perc5 = de-tot-def-ger / de-tot-prod * 100.
else
   assign de-aux-perc5 = 0.

display "Total Geral"  @ tipo-def.descricao
        de-tot-prod    @ de-tot-prd-item
        de-tot-reg-ger @ de-tot-reg-def
        de-tot-ld-ger  @ de-tot-ld-def
        de-tot-ret-ger @ de-tot-ret-def
        de-tot-def-ger @ de-tot-def-def
        de-aux-perc5
        with frame f-detalhe.
DOWN with frame f-detalhe.

IF tt-param.imp-param THEN DO:
   PAGE.
   display tt-param.fi-ini-cod-estabel
           tt-param.fi-fin-cod-estabel
           tt-param.fi-ini-it-codigo
           tt-param.fi-fin-it-codigo
           tt-param.fi-ini-cod-refer
           tt-param.fi-fin-cod-refer
           tt-param.fi-desenho
           tt-param.l-inc-exc
           tt-param.fi-ini-cod-tipo-def
           tt-param.fi-fin-cod-tipo-def
           tt-param.fi-ini-data-mov
           tt-param.fi-fin-data-mov
           tt-param.fi-ini-hora-mov
           tt-param.fi-fin-hora-mov
           tt-param.l-tipo-rel
           tt-param.l-excluir-ob
           tt-param.opc-artigo
           tt-param.tp-tecelagem
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

