/* Programa: ESCE027.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Controle de Estoque
** Objetivo: Listar a Movimentacao de Estoque por Deposito/Especie
** Autor...: Gilvando de Souza Araujo - Outubro/96
** Obs.....: Especifico da TEAR TEXTIL IND.COM.LTDA.
**
** Conversao para EMS 2.04:
**   Programa: ESCE027.P  =>  ESCE0003RP.P
**   Autor...: Gilvando Souza Araujo
**   Data....: 17/02/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESCE0003RP 2.04.00.000}

def TEMP-TABLE w-work
    field it-codigo    like item.it-codigo
    FIELD desc-item    LIKE ITEM.desc-item
    field cod-refer    like ref-item.cod-refer
    field cod-depos    like movto-estoq.cod-depos
    field qtd-entradas as dec format ">>>,>>>,>>9.99"
    field qtd-saidas   as dec format ">>>,>>>,>>9.99"
    field vlr-entradas as dec format ">>>,>>>,>>9.99"
    field vlr-saidas   as dec format ">>>,>>>,>>9.99"
    field qtd-ent-conv as dec format ">>>,>>>,>>9.99"
    field qtd-sai-conv as dec format ">>>,>>>,>>9.99"
    INDEX ch-work it-codigo
                  cod-refer
                  cod-depos.

DEFINE TEMP-TABLE tt-param    NO-UNDO
       FIELD destino          AS INTEGER
       FIELD arquivo          AS CHAR FORMAT "x(35)"
       FIELD usuario          AS CHAR FORMAT "x(12)"
       FIELD data-exec        AS DATE
       FIELD hora-exec        AS INTEGER
       field classifica       AS INTEGER
       FIELD desc-classifica  AS CHAR FORMAT "x(45)"
       FIELD cod-estabel      LIKE movto-estoq.cod-estabel
       FIELD ge-codigo-ini    LIKE ITEM.ge-codigo
       FIELD ge-codigo-fin    LIKE ITEM.ge-codigo
       FIELD it-codigo-ini    LIKE ITEM.it-codigo
       FIELD it-codigo-fin    LIKE ITEM.it-codigo
       FIELD cod-refer-ini    LIKE movto-estoq.cod-refer
       FIELD cod-refer-fin    LIKE movto-estoq.cod-refer
       FIELD dt-trans-ini     LIKE movto-estoq.dt-trans 
       FIELD dt-trans-fin     LIKE movto-estoq.dt-trans
       FIELD serie-docto-ini  LIKE movto-estoq.serie-docto
       FIELD serie-docto-fin  LIKE movto-estoq.serie-docto
       FIELD all-depos        AS LOG FORMAT "Sim/NÆo"
       FIELD cod-depos1       LIKE deposito.cod-depos
       FIELD cod-depos2       LIKE deposito.cod-depos
       FIELD cod-depos3       LIKE deposito.cod-depos
       FIELD cod-depos4       LIKE deposito.cod-depos
       FIELD cod-depos5       LIKE deposito.cod-depos
       FIELD cod-depos6       LIKE deposito.cod-depos
       FIELD cod-depos7       LIKE deposito.cod-depos
       FIELD cod-depos8       LIKE deposito.cod-depos
       FIELD cod-depos9       LIKE deposito.cod-depos
       FIELD cod-depos10      LIKE deposito.cod-depos
       FIELD esp-docto1       LIKE movto-estoq.esp-docto
       FIELD esp-docto2       LIKE movto-estoq.esp-docto
       FIELD esp-docto3       LIKE movto-estoq.esp-docto
       FIELD esp-docto4       LIKE movto-estoq.esp-docto
       FIELD esp-docto5       LIKE movto-estoq.esp-docto
       FIELD esp-docto6       LIKE movto-estoq.esp-docto
       FIELD esp-docto7       LIKE movto-estoq.esp-docto
       FIELD esp-docto8       LIKE movto-estoq.esp-docto
       FIELD esp-docto9       LIKE movto-estoq.esp-docto
       FIELD esp-docto10      LIKE movto-estoq.esp-docto
       FIELD gerar-excel      AS LOG FORMAT "Sim/NÆo"
       FIELD arq-excel        AS CHAR FORMAT "x(45)"
       FIELD tipo-rel         AS INT
       FIELD desc-tipo-rel    AS CHAR FORMAT "x(10)"
       FIELD tipo-valor       AS INT
       FIELD desc-tipo-valor  AS CHAR FORMAT "x(10)"
       FIELD impr-param       AS LOGICAL.

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

DEF VAR i-ct             AS INT.
DEF VAR c-desc-esp-docto AS CHAR.
DEF VAR c-especies       AS CHAR.
DEF VAR c-descricao      AS CHAR FORMAT "x(36)".
def var de-preco-unit    as dec.
def var de-qtd-ent-item  as dec format ">>>,>>>,>>9.99".
def var de-qtd-sai-item  as dec format ">>>,>>>,>>9.99".
def var de-vlr-ent-item  as dec format ">>>,>>>,>>9.99".
def var de-vlr-sai-item  as dec format ">>>,>>>,>>9.99".
def var de-qtd-ent-ger   as dec format ">>>,>>>,>>9.99".
def var de-qtd-sai-ger   as dec format ">>>,>>>,>>9.99".
def var de-vlr-ent-ger   as dec format ">>>,>>>,>>9.99".
def var de-vlr-sai-ger   as dec format ">>>,>>>,>>9.99".
def var de-qtd-conv      as dec format ">>>,>>>,>>9.99".
def var de-qtd-ent-c-ger as dec format ">>>,>>>,>>9.99".
def var de-qtd-sai-c-ger as dec format ">>>,>>>,>>9.99".

DEF STREAM saida.

form
    "*-------------- Parƒmetros/Sele‡Æo ---------------*" SKIP
    tt-param.desc-classific   LABEL "Classifica‡Æo.." AT 1
    tt-param.cod-estabel      label "Estabelecimento" AT 1
    tt-param.ge-codigo-ini    label "Grupo Estoque.." AT 1
    "a"  AT 34                
    tt-param.ge-codigo-fin    no-labels
    tt-param.it-codigo-ini    label "Item..........." AT 1
    "a"  AT 34                
    tt-param.it-codigo-fin    no-labels
    tt-param.cod-refer-ini    label "Referˆncia....." AT 1
    "a"  AT 34                
    tt-param.cod-refer-fin    no-labels
    tt-param.dt-trans-ini     label "Data Transa‡Æo." AT 1
    "a"  AT 34                
    tt-param.dt-trans-fin     no-labels
    tt-param.serie-docto-ini  LABEL "S‚rie Documento" AT 1
    "a"  AT 34                
    tt-param.serie-docto-fin  NO-LABELS
    tt-param.all-depos        LABEL "Todos Dep¢sitos" AT 1
    tt-param.cod-depos1       LABEL "Dep¢sitos......" AT 1
    tt-param.cod-depos2       NO-LABELS               AT 22
    tt-param.cod-depos3       NO-LABELS               AT 26
    tt-param.cod-depos4       NO-LABELS               AT 30
    tt-param.cod-depos5       NO-LABELS               AT 34
    tt-param.cod-depos6       NO-LABELS               AT 38
    tt-param.cod-depos7       NO-LABELS               AT 42
    tt-param.cod-depos8       NO-LABELS               AT 46
    tt-param.cod-depos9       NO-LABELS               AT 50
    tt-param.cod-depos10      NO-LABELS               AT 54
    c-desc-esp-docto          LABEL "Esp‚cies Docto." AT 1   
    tt-param.desc-tipo-rel    LABEL "Tipo Relat¢rio." AT 1
    tt-param.desc-tipo-valor  LABEL "Tipo Valor....." AT 1
    tt-param.gerar-excel      LABEL "Gerar Excel....." AT  1
    tt-param.arq-excel        LABEL "Arquivo Excel..." AT  1
    with no-box side-labels width 132 stream-io frame f-param.

form
    item.it-codigo      label "Item"
    c-descricao         label "Descri‡Æo"
    w-work.cod-depos    label "Dep"
    w-work.qtd-entradas label "Entradas-Qtd"
    w-work.qtd-saidas   label "Saidas-Qtd"
    w-work.vlr-entradas label "Entradas-Vlr"
    w-work.vlr-saidas   label "Saidas-Vlr"
    de-preco-unit       label "Unitario-Vlr"
    with no-box NO-LABEL 55 down width 134 STREAM-IO frame f-detalhe.

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
{utp/ut-liter.i Movimento_de_Estoque_por_Dep¢sito/Esp‚cie * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
VIEW frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

IF tt-param.gerar-excel THEN DO:
   output STREAM saida to value(tt-param.arq-excel) CONVERT SOURCE "ibm850".
   IF tt-param.tipo-rel = 2 THEN
      PUT STREAM saida 
                 "ITEM;REFER;DESCRICAO;DEP;QUANT-ENT;QUANT-SAI;VALOR-ENT;VALOR-SAI;PRE€O-UNI" SKIP.
   ELSE
      PUT STREAM saida
                 "ITEM;DESCRI€ÇO;QUANT-ENT;QUANT-SAI;VALOR-ENT;VALOR-SAI;PRE€O-UNI" SKIP.
END.

for each movto-estoq USE-INDEX data-item
    where movto-estoq.dt-trans >= tt-param.dt-trans-ini
      and movto-estoq.dt-trans <= tt-param.dt-trans-fin
    no-lock,
    each item where item.it-codigo =  movto-estoq.it-codigo
                and item.ge-codigo >= tt-param.ge-codigo-ini
                and item.ge-codigo <= tt-param.ge-codigo-fin
                and item.it-codigo >= tt-param.it-codigo-ini
                and item.it-codigo <= tt-param.it-codigo-fin
              no-lock:

    run pi-acompanhar in h-acomp (input "Data: " + string(movto-estoq.dt-trans) +
                                        " Item: " + movto-estoq.it-codigo).
    
    if movto-estoq.cod-estabel <> tt-param.cod-estabel OR
       movto-estoq.cod-refer   <  tt-param.cod-refer-ini OR
       movto-estoq.cod-refer   >  tt-param.cod-refer-fin OR
       movto-estoq.serie-docto <  tt-param.serie-docto-ini OR
       movto-estoq.serie-docto >  tt-param.serie-docto-fin THEN NEXT.
   
    IF tt-param.all-depos = NO AND
       movto-estoq.cod-depos <> tt-param.cod-depos1 AND
       movto-estoq.cod-depos <> tt-param.cod-depos2 AND 
       movto-estoq.cod-depos <> tt-param.cod-depos3 AND
       movto-estoq.cod-depos <> tt-param.cod-depos4 AND
       movto-estoq.cod-depos <> tt-param.cod-depos5 AND
       movto-estoq.cod-depos <> tt-param.cod-depos6 AND
       movto-estoq.cod-depos <> tt-param.cod-depos7 AND
       movto-estoq.cod-depos <> tt-param.cod-depos8 AND
       movto-estoq.cod-depos <> tt-param.cod-depos9 AND
       movto-estoq.cod-depos <> tt-param.cod-depos10 THEN NEXT.

    IF movto-estoq.esp-docto <> tt-param.esp-docto1 AND
       movto-estoq.esp-docto <> tt-param.esp-docto2 AND
       movto-estoq.esp-docto <> tt-param.esp-docto3 AND
       movto-estoq.esp-docto <> tt-param.esp-docto4 AND
       movto-estoq.esp-docto <> tt-param.esp-docto5 AND
       movto-estoq.esp-docto <> tt-param.esp-docto6 AND
       movto-estoq.esp-docto <> tt-param.esp-docto7 AND
       movto-estoq.esp-docto <> tt-param.esp-docto8 AND
       movto-estoq.esp-docto <> tt-param.esp-docto9 AND
       movto-estoq.esp-docto <> tt-param.esp-docto10 THEN NEXT.

   find ord-prod where ord-prod.nr-ord-produ = movto-estoq.numero-ordem
                 no-lock no-error.
   if  avail ord-prod
   and ord-prod.nr-linha = 4 then next.

   find first w-work where w-work.it-codigo = movto-estoq.it-codigo
                       AND w-work.cod-refer = movto-estoq.cod-refer
                       and w-work.cod-depos = movto-estoq.cod-depos
                     no-lock no-error.

   if not avail w-work then do:
      create w-work.
      assign w-work.it-codigo    = movto-estoq.it-codigo
             w-work.desc-item    = item.desc-item
             w-work.cod-refer    = movto-estoq.cod-refer
             w-work.cod-depos    = movto-estoq.cod-depos
             w-work.qtd-entradas = 0
             w-work.qtd-saidas   = 0
             w-work.vlr-entradas = 0
             w-work.vlr-saidas   = 0.
   end.
   
   FIND LAST sl-it-per WHERE sl-it-per.cod-estabel       =  tt-param.cod-estabel
                         AND sl-it-per.it-codigo         =  movto-estoq.it-codigo
                         AND sl-it-per.periodo           >= tt-param.dt-trans-ini
                         AND sl-it-per.periodo           <= tt-param.dt-trans-fin
                         AND sl-it-per.cod-depos         =  movto-estoq.cod-depos
                         AND sl-it-per.val-unit-mat-m[1] <> 0
                       NO-LOCK NO-ERROR.
   
   IF ITEM.un <> "m" THEN DO:
      FIND item-ext WHERE item-ext.it-codigo = ITEM.it-codigo
                    NO-LOCK NO-ERROR.
      IF AVAIL item-ext THEN
         ASSIGN de-qtd-conv = movto-estoq.quantidade * item-ext.fator-conv.
      ELSE
         ASSIGN de-qtd-conv = movto-estoq.quantidade.
   END.
   ELSE
       ASSIGN de-qtd-conv = movto-estoq.quantidade.

   if tt-param.tipo-valor = 1 then do: /* Entrada */
      if movto-estoq.tipo-trans = 1 then do:
         assign w-work.qtd-entradas = w-work.qtd-entradas +
                                      movto-estoq.quantidade
                w-work.vlr-entradas = w-work.vlr-entradas +
                                      movto-estoq.valor-mat-m[1]
                w-work.qtd-ent-conv = w-work.qtd-ent-conv + de-qtd-conv.
      end.
      else do:
         assign w-work.qtd-saidas = w-work.qtd-saidas +
                                    movto-estoq.quantidade
                w-work.vlr-saidas = w-work.vlr-saidas +
                                    movto-estoq.valor-mat-m[1]
                w-work.qtd-sai-conv = w-work.qtd-sai-conv + de-qtd-conv.
      end.
   end.
   else do: /* Medio */
      if movto-estoq.tipo-trans = 1 then do:
         assign w-work.qtd-entradas = w-work.qtd-entradas +
                                      movto-estoq.quantidade
                w-work.qtd-ent-conv = w-work.qtd-ent-conv + de-qtd-conv.
         if avail sl-it-per then
            assign w-work.vlr-entradas = w-work.vlr-entradas +
                                        (movto-estoq.quantidade *
                                         sl-it-per.val-unit-mat-m[1]) + 
                                        (movto-estoq.quantidade * 
                                         sl-it-per.val-unit-mob-m[1]).
      end.
      else do:
         assign w-work.qtd-saidas = w-work.qtd-saidas + 
                                    movto-estoq.quantidade
                w-work.qtd-sai-conv = w-work.qtd-sai-conv + de-qtd-conv.
         if avail sl-it-per then
            w-work.vlr-saidas = w-work.vlr-saidas + 
                               (movto-estoq.quantidade * 
                                sl-it-per.val-unit-mat-m[1]) + 
                               (movto-estoq.quantidade *
                                sl-it-per.val-unit-mob-m[1]).
      end.
   end.
end.

IF tt-param.classifica = 1 THEN DO: /* Por C¢digo do Item */
   for each w-work break by w-work.it-codigo
                         by w-work.cod-refer
                         by w-work.cod-depos:
   
       assign de-qtd-ent-item  = de-qtd-ent-item + w-work.qtd-entradas
              de-qtd-sai-item  = de-qtd-sai-item + w-work.qtd-saidas
              de-vlr-ent-item  = de-vlr-ent-item + w-work.vlr-entradas
              de-vlr-sai-item  = de-vlr-sai-item + w-work.vlr-saidas
              de-qtd-ent-ger   = de-qtd-ent-ger + w-work.qtd-entradas
              de-qtd-sai-ger   = de-qtd-sai-ger + w-work.qtd-saidas
              de-vlr-ent-ger   = de-vlr-ent-ger + w-work.vlr-entradas
              de-vlr-sai-ger   = de-vlr-sai-ger + w-work.vlr-saidas
              de-qtd-ent-c-ger = de-qtd-ent-c-ger + w-work.qtd-ent-conv
              de-qtd-sai-c-ger = de-qtd-sai-c-ger + w-work.qtd-sai-conv.
       
       if tt-param.tipo-rel = 2 then do: /* Detalhado */
          find referencia where referencia.cod-refer = w-work.cod-refer 
                          NO-LOCK NO-ERROR.
          IF AVAIL referencia THEN
             ASSIGN c-descricao = referencia.descricao.
          ELSE
             ASSIGN c-descricao = "".
   
          assign de-preco-unit = (w-work.vlr-entradas - w-work.vlr-saidas) /
                                 (w-work.qtd-entradas - w-work.qtd-saidas).
          display w-work.cod-refer @ item.it-codigo
                  c-descricao
                  w-work.cod-depos
                  w-work.qtd-entradas
                  w-work.qtd-saidas
                  w-work.vlr-entradas
                  w-work.vlr-saidas
                  de-preco-unit
                  with frame f-detalhe.
          down with frame f-detalhe.

          IF tt-param.gerar-excel THEN DO:
             PUT STREAM saida 
                        w-work.it-codigo ";"
                        w-work.cod-refer ";"
                        c-descricao ";"
                        w-work.cod-depos ";"    
                        w-work.qtd-entradas ";"
                        w-work.qtd-saidas ";"  
                        w-work.vlr-entradas ";"
                        w-work.vlr-saidas ";"  
                        de-preco-unit 
                        SKIP.
          END.
       end.
              
       if last-of(w-work.it-codigo) then do:        
          assign de-preco-unit = (de-vlr-ent-item - de-vlr-sai-item) /
                                 (de-qtd-ent-item - de-qtd-sai-item).
          find ITEM where ITEM.it-codigo = w-work.it-codigo
                    no-lock no-error.
          ASSIGN c-descricao = ITEM.descricao-1 + ITEM.DESCRICAO-2.
              
          display item.it-codigo     
                  c-descricao
                  de-qtd-ent-item  @ w-work.qtd-entradas
                  de-qtd-sai-item  @ w-work.qtd-saidas
                  de-vlr-ent-item  @ w-work.vlr-entradas
                  de-vlr-sai-item  @ w-work.vlr-saidas
                  de-preco-unit
                  with frame f-detalhe.
          
          IF tt-param.tipo-rel = 1 AND tt-param.gerar-excel THEN DO:
             PUT STREAM saida 
                        item.it-codigo ";"
                        c-descricao ";"    
                        de-qtd-ent-item ";"
                        de-qtd-sai-item ";"
                        de-vlr-ent-item ";"
                        de-vlr-sai-item ";"
                        de-preco-unit
                        SKIP.
          END.

          if tt-param.tipo-rel = 2 THEN /* Detalhado */
             down 2 with frame f-detalhe.
          else
             down with frame f-detalhe.
             
          assign de-qtd-ent-item = 0
                 de-qtd-sai-item = 0
                 de-vlr-ent-item = 0
                 de-vlr-sai-item = 0.
       end.
   end.
END.
ELSE DO:  /* Por Descri‡Æo do Item */
    for each w-work break by w-work.desc-item
                          BY w-work.it-codigo
                          by w-work.cod-refer
                          by w-work.cod-depos:

        assign de-qtd-ent-item  = de-qtd-ent-item + w-work.qtd-entradas
               de-qtd-sai-item  = de-qtd-sai-item + w-work.qtd-saidas
               de-vlr-ent-item  = de-vlr-ent-item + w-work.vlr-entradas
               de-vlr-sai-item  = de-vlr-sai-item + w-work.vlr-saidas
               de-qtd-ent-ger   = de-qtd-ent-ger + w-work.qtd-entradas
               de-qtd-sai-ger   = de-qtd-sai-ger + w-work.qtd-saidas
               de-vlr-ent-ger   = de-vlr-ent-ger + w-work.vlr-entradas
               de-vlr-sai-ger   = de-vlr-sai-ger + w-work.vlr-saidas
               de-qtd-ent-c-ger = de-qtd-ent-c-ger + w-work.qtd-ent-conv
               de-qtd-sai-c-ger = de-qtd-sai-c-ger + w-work.qtd-sai-conv.

        if tt-param.tipo-rel = 2 then do: /* Detalhado */
           find referencia where referencia.cod-refer = w-work.cod-refer 
                           NO-LOCK NO-ERROR.
           IF AVAIL referencia THEN
              ASSIGN c-descricao = referencia.descricao.
           ELSE
              ASSIGN c-descricao = "".

           assign de-preco-unit = (w-work.vlr-entradas - w-work.vlr-saidas) /
                                  (w-work.qtd-entradas - w-work.qtd-saidas).
           display w-work.cod-refer @ item.it-codigo
                   c-descricao
                   w-work.cod-depos
                   w-work.qtd-entradas
                   w-work.qtd-saidas
                   w-work.vlr-entradas
                   w-work.vlr-saidas
                   de-preco-unit
                   with frame f-detalhe.
           down with frame f-detalhe.

           IF tt-param.gerar-excel THEN DO:
              PUT STREAM saida 
                         w-work.it-codigo ";"
                         w-work.cod-refer ";"
                         c-descricao ";"
                         w-work.cod-depos ";"    
                         w-work.qtd-entradas ";"
                         w-work.qtd-saidas ";"  
                         w-work.vlr-entradas ";"
                         w-work.vlr-saidas ";"  
                         de-preco-unit 
                         SKIP.
           END.
        end.

        if last-of(w-work.it-codigo) then do:        
           assign de-preco-unit = (de-vlr-ent-item - de-vlr-sai-item) /
                                  (de-qtd-ent-item - de-qtd-sai-item).
           find ITEM where ITEM.it-codigo = w-work.it-codigo
                     no-lock no-error.
           ASSIGN c-descricao = ITEM.descricao-1 + ITEM.DESCRICAO-2.

           display item.it-codigo     
                   c-descricao
                   de-qtd-ent-item  @ w-work.qtd-entradas
                   de-qtd-sai-item  @ w-work.qtd-saidas
                   de-vlr-ent-item  @ w-work.vlr-entradas
                   de-vlr-sai-item  @ w-work.vlr-saidas
                   de-preco-unit
                   with frame f-detalhe.

           IF tt-param.tipo-rel = 1 AND tt-param.gerar-excel THEN DO:
              PUT STREAM saida 
                         item.it-codigo ";"
                         c-descricao ";"    
                         de-qtd-ent-item ";"
                         de-qtd-sai-item ";"
                         de-vlr-ent-item ";"
                         de-vlr-sai-item ";"
                         de-preco-unit
                         SKIP.
           END.

           if tt-param.tipo-rel = 2 THEN /* Detalhado */
              down 2 with frame f-detalhe.
           else
              down with frame f-detalhe.

           assign de-qtd-ent-item = 0
                  de-qtd-sai-item = 0
                  de-vlr-ent-item = 0
                  de-vlr-sai-item = 0.
        end.
    end.
END.

display "Totais:"       @ item.it-codigo
        de-qtd-ent-ger  @ w-work.qtd-entradas
        de-qtd-sai-ger  @ w-work.qtd-saidas
        de-vlr-ent-ger  @ w-work.vlr-entradas
        de-vlr-sai-ger  @ w-work.vlr-saidas
        with frame f-detalhe.
DOWN 2 with frame f-detalhe.

display "(Em M)"          @ item.it-codigo
        de-qtd-ent-c-ger  @ w-work.qtd-entradas
        de-qtd-sai-c-ger  @ w-work.qtd-saidas
        with frame f-detalhe.
down with frame f-detalhe.

IF tt-param.gerar-excel THEN
   OUTPUT STREAM saida CLOSE.

IF tt-param.impr-param THEN DO:

   {esinc/i-dsallrb.i movto-estoq.esp-docto c-especies}
   
   ASSIGN c-desc-esp-docto = "".
   IF tt-param.esp-docto1 > 0 THEN
      ASSIGN c-desc-esp-docto = c-desc-esp-docto + entry(tt-param.esp-docto1,c-especies).
   IF tt-param.esp-docto2 > 0 THEN
      ASSIGN c-desc-esp-docto = c-desc-esp-docto + ", " + entry(tt-param.esp-docto2,c-especies).
   IF tt-param.esp-docto3 > 0 THEN
      ASSIGN c-desc-esp-docto = c-desc-esp-docto + ", " + entry(tt-param.esp-docto3,c-especies).
   IF tt-param.esp-docto4 > 0 THEN
      ASSIGN c-desc-esp-docto = c-desc-esp-docto + ", " + entry(tt-param.esp-docto4,c-especies).
   IF tt-param.esp-docto5 > 0 THEN
      ASSIGN c-desc-esp-docto = c-desc-esp-docto + ", " + entry(tt-param.esp-docto5,c-especies).
   IF tt-param.esp-docto6 > 0 THEN
      ASSIGN c-desc-esp-docto = c-desc-esp-docto + ", " + entry(tt-param.esp-docto6,c-especies).
   IF tt-param.esp-docto7 > 0 THEN
      ASSIGN c-desc-esp-docto = c-desc-esp-docto + ", " + entry(tt-param.esp-docto7,c-especies).
   IF tt-param.esp-docto8 > 0 THEN
      ASSIGN c-desc-esp-docto = c-desc-esp-docto + ", " + entry(tt-param.esp-docto8,c-especies).
   IF tt-param.esp-docto9 > 0 THEN
      ASSIGN c-desc-esp-docto = c-desc-esp-docto + ", " + entry(tt-param.esp-docto9,c-especies).
   IF tt-param.esp-docto10 > 0 THEN
      ASSIGN c-desc-esp-docto = c-desc-esp-docto + ", " + entry(tt-param.esp-docto10,c-especies).
   
   PAGE.
   display tt-param.desc-classific
           tt-param.cod-estabel
           tt-param.ge-codigo-ini
           tt-param.ge-codigo-fin
           tt-param.it-codigo-ini
           tt-param.it-codigo-fin
           tt-param.cod-refer-ini
           tt-param.cod-refer-fin
           tt-param.dt-trans-ini   
           tt-param.dt-trans-fin   
           tt-param.serie-docto-ini    
           tt-param.serie-docto-fin
           tt-param.all-depos
           tt-param.cod-depos1
           tt-param.cod-depos2
           tt-param.cod-depos3
           tt-param.cod-depos4
           tt-param.cod-depos5
           tt-param.cod-depos6
           tt-param.cod-depos7
           tt-param.cod-depos8
           tt-param.cod-depos9
           tt-param.cod-depos10
           c-desc-esp-docto
           tt-param.gerar-excel
           tt-param.arq-excel  
           tt-param.desc-tipo-rel
           tt-param.desc-tipo-valor
           with frame f-param.
END.
 
/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

