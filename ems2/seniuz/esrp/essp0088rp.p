/* Programa: ESCE018
** Sistema.: Magnus da Datasul
** Modulo..: Especifico
** Objetivo: Relatorio do Invent rio da Expedi‡Æo Agrupado Por Item.
** Autor...: Gilvando de Souza Araujo
** Data....: Outubro/1995
** Obs.....: Programa especifico da TEAR TEXTIL IND COM LTDA
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESSP0088RP 2.04.00.000}

DEF BUFFER empresa FOR mgcad.empresa.

DEFINE TEMP-TABLE tt-param  NO-UNDO
       FIELD destino           AS INTEGER
       FIELD arquivo           AS CHAR FORMAT "x(35)"
       FIELD usuario           AS CHAR FORMAT "x(12)"
       FIELD data-exec         AS DATE
       FIELD hora-exec         AS INTEGER
       FIELD classifica        AS INTEGER
       FIELD desc-classifica   AS CHAR FORMAT "x(40)"
       FIELD cod-estabel       AS CHAR
       FIELD dt-invent         AS DATE
       FIELD ini-docto         LIKE inv-acab.docto
       FIELD fin-docto         LIKE inv-acab.docto
       FIELD ini-it-codigo     LIKE inv-acab.it-codigo
       FIELD fin-it-codigo     LIKE inv-acab.it-codigo
       FIELD ini-cod-refer     LIKE inv-acab.cod-refer
       FIELD fin-cod-refer     LIKE inv-acab.cod-refer
       FIELD ini-lote          LIKE inv-acab.lote
       FIELD fin-lote          LIKE inv-acab.lote
       FIELD dt-inventario     LIKE inventario.dt-saldo
       FIELD res-nao-invent    AS LOG
       FIELD est-nao-invent    AS LOG
       FIELD invent-nao-est    AS LOG
       FIELD simples-conf      AS LOG
       FIELD qtd-min           AS DEC
       FIELD atu-ems           AS LOG
       FIELD imp-param         AS LOG.

define temp-table tt-raw-digita
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.
DEFINE STREAM sExcel.
create tt-param.
raw-transfer raw-param to tt-param.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* include padrÆo para impressÆo de campos editores em relat¢rio  */
{include/tt-edit.i}

DEF TEMP-TABLE tt-aux
    FIELD cod-estabel  LIKE inv-acab.cod-estabel
    FIELD it-codigo    LIKE inv-acab.it-codigo
    FIELD cod-refer    LIKE inv-acab.cod-refer
    FIELD lote         LIKE inv-acab.lote
    FIELD qtd-inv      LIKE inv-acab.qtd-inv
    FIELD qtd-est      LIKE saldo-estoq.qtidade-atu
    FIELD origem       AS   CHAR.

DEF TEMP-TABLE tt-res-etq
    FIELD num-etiqueta LIKE ob-etiqueta.num-etiqueta.

DEF BUFFER b-inv-acab FOR inv-acab.


/* defini‡Æo de vari veis  */
def var h-acomp as handle no-undo.

DEF VAR c-situacao   AS CHAR FORMAT "x(12)".

DEF VAR c-descricao  AS CHAR FORMAT "X(20)" LABEL "Descricao".
DEF VAR de-tot-ger   AS DEC FORMAT ">,>>>,>>9.9999".
DEF VAR de-tot-estoq   LIKE saldo-estoq.qtidade-atu.
DEF VAR de-dif         LIKE saldo-estoq.qtidade-atu.
DEF VAR i-nr-ficha     LIKE inventario.nr-ficha.
DEF VAR de-qtidade-atu LIKE saldo-estoq.qtidade-atu.

DEF VAR i-sit-ini      LIKE ob-etiqueta.situacao.
DEF VAR i-sit-fin      LIKE ob-etiqueta.situacao.
DEF VAR c-nr-pedcli    LIKE ped-item-rom.nr-pedcli.
DEF VAR c-nr-nota-fis  LIKE ped-item-res.nr-nota-fis.

FORM 
    "*------------ Parƒmetros/Sele‡Æo -------------*" SKIP
    tt-param.cod-estabel      LABEL "Estabelecimento......." SKIP
    tt-param.dt-invent        LABEL "Data do Inventario...." SKIP
    tt-param.ini-docto        LABEL "N§ do Documento......."
    "A"  AT 35
    tt-param.fin-docto        NO-LABELS SKIP
    tt-param.res-nao-invent  FORMAT "Sim/NÆo" LABEL "Lista Etiquetas Reservadas NÆo Inventariadas" SKIP
    tt-param.est-nao-invent  FORMAT "Sim/NÆo" LABEL "Lista Etiquetas Em Estoque NÆo Inventariadas" SKIP
    tt-param.invent-nao-est  FORMAT "Sim/NÆo" LABEL "Lista Etiquetas Inventariadas que nÆo estavam em Estoque.."
    SKIP(1)
    WITH FRAME f-param SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.

FORM 
   ob-etiqueta.num-etiqueta FORMAT ">>>>>>>>9"  COLUMN-LABEL "Etiqueta" 
   c-situacao LABEL "Situa‡Æo"                
   ob-etiqueta.localizacao  FORMAT "999/999"   
   ob-etiqueta.it-codigo    FORMAT "x(6)"      
   c-descricao              FORMAT "x(20)" 
   ob-etiqueta.cod-refer
   ob-etiqueta.quantidade      
   c-nr-pedcli              COLUMN-LABEL "Pedido/NF" 
   WITH NO-BOX 55 DOWN WIDTH 132 STREAM-IO FRAME f-detalhe.

FORM
   tt-aux.it-codigo FORMAT "x(6)"
   c-descricao        
   tt-aux.cod-refer
   tt-aux.qtd-inv COLUMN-LABEL "Qtd Iventario"              
   tt-aux.qtd-est COLUMN-LABEL "Qtd Estoque" 
   de-dif         COLUMN-LABEL "Diferen‡a"
   WITH NO-BOX 55 DOWN WIDTH 132 STREAM-IO FRAME f-invent.

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i}

/* bloco principal do programa */
FIND FIRST param-global NO-LOCK NO-ERROR.
FIND FIRST empresa
     WHERE empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR. 

ASSIGN c-empresa = (IF AVAIL empresa THEN empresa.razao-social ELSE "").

{utp/ut-liter.i ESPECIFICOS * r}
ASSIGN c-sistema = TRIM(RETURN-VALUE).
{utp/ut-liter.i Etiquetas_NÆo_Inventariadas * r}
ASSIGN c-titulo-relat = TRIM(RETURN-VALUE).

VIEW FRAME f-cabec.
VIEW FRAME f-rodape.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
{utp/ut-liter.i Imprimindo *}
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

ASSIGN i-sit-ini = 0
       i-sit-fin = 0.
IF tt-param.est-nao-invent THEN
   ASSIGN i-sit-ini = 3
          i-sit-fin = 3.

IF tt-param.res-nao-invent THEN DO.
   ASSIGN i-sit-ini = IF i-sit-ini = 0 
                      THEN 4 
                      ELSE i-sit-ini
          i-sit-fin = 4.

   FOR EACH ped-reserva WHERE
           ped-reserva.situacao = 1 NO-LOCK.
      FOR EACH ped-reserva-it OF ped-reserva.
          FOR EACH ped-reserva-etq OF ped-reserva-it NO-LOCK.
              CREATE tt-res-etq.
              ASSIGN tt-res-etq.num-etiqueta = ped-reserva-etq.num-etiqueta.
          END.
      END.
   END.
END.

IF tt-param.est-nao-invent OR
   tt-param.res-nao-invent THEN DO. /* Etiquetas em Estoque ou Reservadas NÆo Inventariadas */

   ASSIGN c-situacao = ""
          de-tot-ger = 0.
   FOR EACH ob-etiqueta WHERE 
            ob-etiqueta.situacao            >= i-sit-ini              AND 
            ob-etiqueta.situacao            <= i-sit-fin              AND 
            ob-etiqueta.cod-estabel          = tt-param.cod-estabel   AND
            ob-etiqueta.it-codigo           >= tt-param.ini-it-codigo AND
            ob-etiqueta.it-codigo           <= tt-param.fin-it-codigo AND
            ob-etiqueta.cod-refer           >= tt-param.ini-cod-refer AND
            ob-etiqueta.cod-refer           <= tt-param.fin-cod-refer
            NO-LOCK.

       RUN pi-acompanhar IN h-acomp (INPUT "Etiqueta: " + STRING(ob-etiqueta.num-etiqueta)).

       IF ob-etiqueta.quantidade <= tt-param.qtd-min THEN NEXT.

       FIND inv-acab WHERE
            inv-acab.cod-estabel  = ob-etiqueta.cod-estabel AND
            inv-acab.data-invent  = tt-param.dt-invent      AND
            inv-acab.num-etiqueta = ob-etiqueta.num-etiqueta NO-LOCK NO-ERROR.
       IF NOT AVAIL inv-acab THEN DO.
          RUN pi-situacao.
          RUN pi-desc-item (INPUT ob-etiqueta.it-codigo).

          ASSIGN c-nr-pedcli = "".
          IF ob-etiqueta.situacao = 4 THEN DO.
             FIND ped-item-rom WHERE
                  ped-item-rom.cod-estabel  = ob-etiqueta.cod-estabel AND
                  ped-item-rom.num-etiqueta = ob-etiqueta.num-etiqueta
                  NO-LOCK NO-ERROR.
             IF AVAIL ped-item-rom THEN
                ASSIGN c-nr-pedcli = ped-item-rom.nr-pedcli.
             ELSE DO.
                FIND tt-res-etq WHERE
                     tt-res-etq.num-etiqueta = ob-etiqueta.num-etiqueta NO-ERROR.

                IF AVAIL tt-res-etq THEN 
                   ASSIGN c-nr-pedcli = "RESERVA".
             END.
          END.

          ASSIGN de-tot-ger = de-tot-ger + ob-etiqueta.quantidade.

          DISPLAY ob-etiqueta.num-etiqueta
                  c-situacao
                  ob-etiqueta.localizacao
                  ob-etiqueta.it-codigo
                  c-descricao
                  ob-etiqueta.cod-refer
                  ob-etiqueta.quantidade
                  c-nr-pedcli
                  WITH FRAME f-detalhe.

          DOWN WITH FRAME f-detalhe.
       END.
   END.

   DOWN 1 WITH FRAME f-detalhe.
   DISPLAY "Total Geral" @ c-descricao
            de-tot-ger   @ ob-etiqueta.quantidade
           WITH FRAME f-detalhe.
   DOWN WITH FRAME f-detalhe.

   IF c-situacao <> "" THEN 
      PAGE.
END.

IF tt-param.invent-nao-est THEN DO. /* Etiquetas Inventariadas Fora do Estoque */
   ASSIGN c-titulo-relat = "Etiquetas Inventariadas que NÇO estÆo em Estoque".
   ASSIGN c-situacao = ""
          de-tot-ger = 0.
   FOR EACH inv-acab WHERE 
            inv-acab.cod-estabel       = tt-param.cod-estabel   AND
            inv-acab.data-invent       = tt-param.dt-invent     AND
            inv-acab.it-codigo        >= tt-param.ini-it-codigo AND
            inv-acab.it-codigo        <= tt-param.fin-it-codigo AND
            inv-acab.cod-refer        >= tt-param.ini-cod-refer AND
            inv-acab.cod-refer        <= tt-param.fin-cod-refer AND 
            inv-acab.situacao          = 1 NO-LOCK.

       FIND ob-etiqueta WHERE
            ob-etiqueta.cod-estabel  = inv-acab.cod-estabel AND
            ob-etiqueta.num-etiqueta = inv-acab.num-etiqueta NO-LOCK NO-ERROR.
       IF AVAIL ob-etiqueta AND 
          ob-etiqueta.situacao <> 3 AND 
          ob-etiqueta.situacao <> 4 THEN DO.

          RUN pi-situacao.
          RUN pi-desc-item (INPUT inv-acab.it-codigo).

          ASSIGN c-nr-nota-fis = 0.
          IF ob-etiqueta.situacao = 5 THEN DO.
             FIND ped-item-rom WHERE
                  ped-item-rom.cod-estabel  = ob-etiqueta.cod-estabel AND
                  ped-item-rom.num-etiqueta = ob-etiqueta.num-etiqueta
                  NO-LOCK NO-ERROR.

             FIND FIRST ped-item-res WHERE
                        ped-item-res.cod-estabel  = ped-item-rom.cod-estabel AND
                        ped-item-res.nome-abrev   = ped-item-rom.nome-abrev  AND
                        ped-item-res.nr-pedcli    = ped-item-rom.nr-pedcli   AND
                        ped-item-res.nr-sequencia = ped-item-rom.nr-sequencia NO-LOCK NO-ERROR.
             IF AVAIL ped-item-res THEN
                ASSIGN c-nr-nota-fis = ped-item-res.nr-nota-fis.
          END.

          ASSIGN de-tot-ger = de-tot-ger + ob-etiqueta.quantidade.

          DISPLAY ob-etiqueta.num-etiqueta
                  c-situacao
                  ob-etiqueta.localizacao
                  ob-etiqueta.it-codigo
                  c-descricao
                  ob-etiqueta.cod-refer
                  ob-etiqueta.quantidade
                  c-nr-nota-fis @ c-nr-pedcli
                  WITH FRAME f-detalhe.

          DOWN WITH FRAME f-detalhe.
       END.
   END.
   DOWN 1 WITH FRAME f-detalhe.
   DISPLAY "Total Geral" @ c-descricao
            de-tot-ger   @ ob-etiqueta.quantidade
           WITH FRAME f-detalhe.
   DOWN WITH FRAME f-detalhe.

   IF c-situacao <> "" THEN
      PAGE.
END.

IF tt-param.simples-conf THEN DO. /* Relatorio de simples conferencia do Inventario */

   EMPTY TEMP-TABLE tt-aux.

   FOR EACH inv-acab WHERE 
            inv-acab.cod-estabel       = tt-param.cod-estabel   AND
            inv-acab.data-invent       = tt-param.dt-invent     AND
            inv-acab.it-codigo        >= tt-param.ini-it-codigo AND
            inv-acab.it-codigo        <= tt-param.fin-it-codigo AND
            inv-acab.cod-refer        >= tt-param.ini-cod-refer AND
            inv-acab.cod-refer        <= tt-param.fin-cod-refer AND
            inv-acab.situacao          = 1 NO-LOCK.

       RUN pi-acompanhar IN h-acomp (INPUT "Etiqueta: " + STRING(inv-acab.num-etiqueta)).

       FIND tt-aux WHERE
            tt-aux.cod-estabel = inv-acab.cod-estabel AND
            tt-aux.it-codigo   = inv-acab.it-codigo   AND
            tt-aux.cod-refer   = inv-acab.cod-refer   AND
            tt-aux.origem      = "INV" NO-LOCK NO-ERROR.

       IF NOT AVAIL tt-aux THEN DO.
          CREATE tt-aux.
          ASSIGN tt-aux.cod-estabel  = inv-acab.cod-estabel
                 tt-aux.it-codigo    = inv-acab.it-codigo 
                 tt-aux.cod-refer    = inv-acab.cod-refer 
                 tt-aux.lote         = inv-acab.cod-refer
                 tt-aux.origem       = "INV".
       END.
       ASSIGN tt-aux.qtd-inv = tt-aux.qtd-inv + inv-acab.qtd-inv.
   END.

   FOR EACH saldo-estoq WHERE
            saldo-estoq.cod-estabel = tt-param.cod-estabel AND 
            saldo-estoq.it-codigo  >= tt-param.ini-it-codigo AND
            saldo-estoq.it-codigo  <= tt-param.fin-it-codigo AND
            saldo-estoq.cod-refer  >= tt-param.ini-cod-refer AND
            saldo-estoq.cod-refer  <= tt-param.fin-cod-refer NO-LOCK,
       FIRST item OF saldo-estoq WHERE
             item.ge-codigo >= 50 AND
             item.ge-codigo <= 60 NO-LOCK.

       IF saldo-estoq.qtidade-atu = 0 THEN NEXT.

       RUN pi-acompanhar IN h-acomp (INPUT "Item: " + saldo-estoq.it-codigo + " / " + saldo-estoq.cod-refer ).

       FIND tt-aux WHERE
            tt-aux.cod-estabel = saldo-estoq.cod-estabel AND
            tt-aux.it-codigo   = saldo-estoq.it-codigo   AND
            tt-aux.cod-refer   = saldo-estoq.cod-refer   
            NO-LOCK NO-ERROR.

       IF NOT AVAIL tt-aux THEN DO.
          CREATE tt-aux.
          ASSIGN tt-aux.cod-estabel  = saldo-estoq.cod-estabel
                 tt-aux.it-codigo    = saldo-estoq.it-codigo 
                 tt-aux.cod-refer    = saldo-estoq.cod-refer.
       END.
       ASSIGN tt-aux.qtd-est = tt-aux.qtd-est + saldo-estoq.qtidade-atu.
   END.


   ASSIGN c-titulo-relat = "Invent rio da Expedi‡Æo " + STRING(tt-param.dt-invent, "99/99/9999").
   OUTPUT STREAM sExcel TO value(SESSION:TEMP-DIRECT + 'invexcel.txt').
   FOR EACH tt-aux :
       FIND FIRST ITEM
           WHERE ITEM.it-codigo  = tt-aux.it-codigo NO-LOCK NO-ERROR.
       EXPORT STREAM sExcel DELIMITER "|" tt-aux.it-codigo  item.desc-item item.un tt-aux.cod-refer  tt-aux.qtd-inv  tt-aux.qtd-est  tt-aux.qtd-est - tt-aux.qtd-inv .
   END.
   OUTPUT STREAM sExcel CLOSE.

   FOR EACH tt-aux NO-LOCK
       BREAK BY tt-aux.it-codigo
             BY tt-aux.cod-refer
             BY tt-aux.lote:
       ACCUMULATE tt-aux.qtd-inv (TOTAL BY tt-aux.it-codigo).
       ACCUMULATE tt-aux.qtd-est (TOTAL BY tt-aux.it-codigo).

       IF FIRST-OF(tt-aux.it-codigo) THEN DO:
          RUN pi-desc-item (INPUT tt-aux.it-codigo).
          DISPLAY tt-aux.it-codigo
                  c-descricao
                  WITH FRAME f-invent.
       END.

       ASSIGN de-dif = tt-aux.qtd-inv - tt-aux.qtd-est.
       IF ABS(de-dif) < tt-param.qtd-min THEN NEXT.

       DISPLAY tt-aux.cod-refer
               tt-aux.qtd-inv
               tt-aux.qtd-est
               de-dif
               WITH FRAME f-invent.
   
       DOWN WITH FRAME f-invent.
          
       ASSIGN de-tot-estoq = de-tot-estoq + tt-aux.qtd-est.
              de-tot-ger = de-tot-ger + tt-aux.qtd-inv.

       IF LAST-OF(tt-aux.it-codigo) THEN DO:
          DISPLAY (ACCUM TOTAL BY tt-aux.it-codigo tt-aux.qtd-est) @ tt-aux.qtd-est
                  (ACCUM TOTAL BY tt-aux.it-codigo tt-aux.qtd-inv) @ tt-aux.qtd-inv
                  (ACCUM TOTAL BY tt-aux.it-codigo tt-aux.qtd-inv) -
                    (ACCUM TOTAL BY tt-aux.it-codigo tt-aux.qtd-est) @ de-dif

                  WITH FRAME f-invent.
          DOWN WITH FRAME f-invent.
       END.
   END.
   
   DOWN 1 WITH FRAME f-invent.
   
   DISPLAY "Total Geral" @ c-descricao
            de-tot-estoq @ tt-aux.qtd-est
            de-tot-ger   @ tt-aux.qtd-inv
            de-tot-ger - de-tot-estoq  @ de-dif
           WITH FRAME f-invent.
   DOWN WITH FRAME f-invent.
END.

IF tt-param.atu-ems THEN DO.
   FOR EACH inv-acab WHERE 
            inv-acab.cod-estabel       = tt-param.cod-estabel   AND
            inv-acab.data-invent       = tt-param.dt-invent     AND
            inv-acab.it-codigo        >= tt-param.ini-it-codigo AND
            inv-acab.it-codigo        <= tt-param.fin-it-codigo AND
            inv-acab.cod-refer        >= tt-param.ini-cod-refer AND
            inv-acab.cod-refer        <= tt-param.fin-cod-refer AND
            inv-acab.situacao          = 1 NO-LOCK.

        RUN pi-acompanhar IN h-acomp (INPUT "Etiqueta: " + STRING(inv-acab.num-etiqueta)).

        FIND tt-aux WHERE
             tt-aux.cod-estabel = inv-acab.cod-estabel AND
             tt-aux.it-codigo   = inv-acab.it-codigo   AND
             tt-aux.cod-refer   = inv-acab.cod-refer   AND
             tt-aux.lote        = inv-acab.cod-refer NO-LOCK NO-ERROR.

        IF NOT AVAIL tt-aux THEN DO.
           CREATE tt-aux.
           ASSIGN tt-aux.cod-estabel  = inv-acab.cod-estabel
                  tt-aux.it-codigo    = inv-acab.it-codigo 
                  tt-aux.cod-refer    = inv-acab.cod-refer 
                  tt-aux.lote         = inv-acab.cod-refer
                  tt-aux.origem       = "INV".
        END.
        ASSIGN tt-aux.qtd-inv = tt-aux.qtd-inv + inv-acab.qtd-inv.
    END.

    FOR EACH saldo-estoq WHERE
             saldo-estoq.cod-estabel = tt-param.cod-estabel AND 
             saldo-estoq.it-codigo  >= tt-param.ini-it-codigo AND
             saldo-estoq.it-codigo  <= tt-param.fin-it-codigo AND
             saldo-estoq.cod-refer  >= tt-param.ini-cod-refer AND
             saldo-estoq.cod-refer  <= tt-param.fin-cod-refer NO-LOCK,
        FIRST item OF saldo-estoq WHERE
              item.ge-codigo >= 50 AND
              item.ge-codigo <= 60 NO-LOCK.

        IF saldo-estoq.qtidade-atu = 0 THEN NEXT.

        RUN pi-acompanhar IN h-acomp (INPUT "Item: " + saldo-estoq.it-codigo + " / " + saldo-estoq.cod-refer ).

        FIND tt-aux WHERE
             tt-aux.cod-estabel = saldo-estoq.cod-estabel AND
             tt-aux.it-codigo   = saldo-estoq.it-codigo   AND
             tt-aux.cod-refer   = saldo-estoq.cod-refer   
             NO-LOCK NO-ERROR.

        IF NOT AVAIL tt-aux THEN DO.
           CREATE tt-aux.
           ASSIGN tt-aux.cod-estabel  = saldo-estoq.cod-estabel
                  tt-aux.it-codigo    = saldo-estoq.it-codigo 
                  tt-aux.cod-refer    = saldo-estoq.cod-refer.
        END.
        ASSIGN tt-aux.qtd-est = tt-aux.qtd-est + saldo-estoq.qtidade-atu.
   END.

   RUN pi-atualiza.
   /*RUN pi-valida-dados.*/
END.


/* fechamento do output do relat¢rio  */
IF tt-param.imp-param THEN DO:
   PAGE.
   DISPLAY tt-param.dt-invent
           tt-param.ini-docto
           tt-param.fin-docto
           tt-param.res-nao-invent
           tt-param.est-nao-invent
           tt-param.invent-nao-est
           WITH FRAME f-param.
END.

{include/i-rpclo.i}
RUN pi-finalizar in h-acomp.
RETURN "OK":U.

/*------------------[ P R O C E D U R E S ]  --------------------*/

PROCEDURE pi-situacao.
    {esinc/i-dsrb.i ob-etiqueta.situacao ob-etiqueta.situacao c-situacao} 
END PROCEDURE.


PROCEDURE pi-desc-item.
    DEF INPUT PARAMETER p-codigo AS CHAR.

    FIND item WHERE
         item.it-codigo = p-codigo NO-LOCK NO-ERROR.
    IF AVAIL item THEN
       ASSIGN c-descricao = item.desc-item.
    ELSE
       ASSIGN c-descricao = "ERRO Item nao cadastrado !!!".
END PROCEDURE.


PROCEDURE pi-atualiza.
    FIND FIRST tt-aux NO-LOCK NO-ERROR.
    IF NOT AVAIL tt-aux THEN DO.
       DOWN 5 WITH FRAME f-detalhe.
       PUT  "*** NAO EXISTEM ITENS PENDENTES DE ATUALIZA€ÇO NA FAIXA DE DOCUMENTOS INFORMADA ***" 
            SKIP.
           
       DOWN 2 WITH FRAME f-detalhe.
       PUT "*** ATUALIZA€ÇO NÇO EFETUADA ***"
           SKIP.

       RETURN 'NOK'.
    END.

    FOR EACH tt-aux WHERE
             tt-aux.qtd-inv > 0 NO-LOCK
        BREAK BY tt-aux.it-codigo
              BY tt-aux.cod-refer
              BY tt-aux.lote:

        RUN pi-acompanhar IN h-acomp (INPUT "Atualizando: " + STRING(tt-aux.it-codigo)).

        FIND LAST movto-estoq WHERE
                  movto-estoq.cod-estabel = tt-aux.cod-estabel AND
                  movto-estoq.it-codigo = tt-aux.it-codigo AND
                  movto-estoq.cod-refer = tt-aux.cod-refer AND
                  movto-estoq.lote = tt-aux.cod-refer AND
                  movto-estoq.tipo-trans = 2
                  NO-LOCK NO-ERROR.

        FIND item-uni-estab WHERE
             item-uni-estab.cod-estabel = tt-aux.cod-estabel AND
             item-uni-estab.it-codigo   = tt-aux.it-codigo NO-LOCK NO-ERROR.

        FIND LAST inventario WHERE
                  inventario.dt-saldo = tt-param.dt-inventario
                  USE-INDEX nr-ficha NO-LOCK NO-ERROR.

        ASSIGN i-nr-ficha = IF AVAIL inventario 
                            THEN inventario.nr-ficha + 1
                            ELSE 1.

        CREATE inventario.
        ASSIGN inventario.dt-saldo       = tt-param.dt-inventario
               inventario.nr-ficha       = i-nr-ficha
               inventario.cod-estabel    = item-uni-estab.cod-estabel
               inventario.cod-depos      = item-uni-estab.deposito-pad
               inventario.it-codigo      = tt-aux.it-codigo
               inventario.cod-refer      = tt-aux.cod-refer
               inventario.lote           = tt-aux.lote
               inventario.dt-ult-entra   = tt-param.dt-inventario
               inventario.dt-ult-saida   = IF AVAIL movto-estoq
                                           THEN movto-estoq.dt-trans
                                           ELSE tt-param.dt-inventario
               inventario.qtidade-atu    = de-qtidade-atu
               inventario.val-apurado[1] = tt-aux.qtd-inv 
               inventario.situacao       = 4.
    END.

    /*
    /* Retirar Etiquetas do Estoque */
    FOR EACH ob-etiqueta WHERE
             ob-etiqueta.cod-estabel          = tt-param.cod-estabel   AND
             (ob-etiqueta.situacao             = 3 OR   /* Estoque */
              ob-etiqueta.situacao             = 4 OR   /* Reservada */
              ob-etiqueta.situacao             = 8) AND /* Bloqueada */
             ob-etiqueta.it-codigo           >= tt-param.ini-it-codigo AND
             ob-etiqueta.it-codigo           <= tt-param.fin-it-codigo AND
             ob-etiqueta.cod-refer           >= tt-param.ini-cod-refer AND
             ob-etiqueta.cod-refer           <= tt-param.fin-cod-refer 
             EXCLUSIVE-LOCK.

        ASSIGN ob-etiqueta.int-1 = ob-etiqueta.situacao  /* Salva Situacao atual */
               ob-etiqueta.situacao = 5  /* Faturada */
               ob-etiqueta.dt-fatur = tt-param.dt-invent.

        /* Limpar Romaneio e Reserva das etiquetas nÆo inventariadas */

        /* Limpar a Marca do romaneio */

    END.
    */


    /*
    /* Colocar Etiquetas Lidas em Estoque e Setar inventario como Atualizado */
    FOR EACH inv-acab WHERE 
             inv-acab.cod-estabel       = tt-param.cod-estabel   AND
             inv-acab.data-invent       = tt-param.dt-invent     AND
             inv-acab.docto            >= tt-param.ini-docto     AND
             inv-acab.docto            <= tt-param.fin-docto     AND
             inv-acab.it-codigo        >= tt-param.ini-it-codigo AND
             inv-acab.it-codigo        <= tt-param.fin-it-codigo AND
             inv-acab.cod-refer        >= tt-param.ini-cod-refer AND
             inv-acab.cod-refer        <= tt-param.fin-cod-refer AND
             inv-acab.situacao          = 1.

        FIND ob-etiqueta WHERE
             ob-etiqueta.cod-estabel  = inv-acab.cod-estabel AND
             ob-etiqueta.num-etiqueta = inv-acab.num-etiqueta NO-ERROR.

        IF AVAIL ob-etiqueta THEN DO.
           ASSIGN ob-etiqueta.dt-fatur = ?
                  ob-etiqueta.situacao = 3.

           CASE ob-etiqueta.int-1.  /* situacao anterior */
               WHEN 3 OR WHEN 5 THEN DO.
                   FOR EACH ped-item-rom WHERE
                            ped-item-rom.cod-estabel = ob-etiqueta.cod-estabel AND
                            ped-item-rom.num-etiqueta = ob-etiqueta.num-etiqueta. 
                       DELETE ped-item-rom.
                   END.
               END.
               WHEN 4 THEN ASSIGN ob-etiqueta.situacao = 4.
           END CASE.
    
           ASSIGN ob-etiqueta.int-1 = 0
                  inv-acab.situacao = 2.
        END.
    END.
    */


    /*
    /* Criar Ficha para Itens nÆo Inventariados */
       DEF VAR i-nr-ficha     LIKE inventario.nr-ficha.
       DEF VAR da-data-inv    LIKE inventario.dt-saldo.

       ASSIGN da-data-inv = 12.31.2015.

       FOR EACH saldo-estoq WHERE
                saldo-estoq.cod-estabel = '5' NO-LOCK,
           FIRST item OF saldo-estoq WHERE
                 item.ge-codigo >= 50 AND
                 item.ge-codigo <= 60 NO-LOCK.
    
           IF saldo-estoq.qtidade-atu = 0 THEN NEXT.

           FIND inventario WHERE
                inventario.dt-saldo  = da-data-inv AND
                inventario.it-codigo = saldo-estoq.it-codigo AND
                inventario.cod-refer = saldo-estoq.cod-refer AND
                inventario.lote = saldo-estoq.lote
                NO-LOCK NO-ERROR.
    
           IF NOT AVAIL inventario THEN DO.
              FIND LAST movto-estoq WHERE
                        movto-estoq.cod-estabel = saldo-estoq.cod-estabel AND
                        movto-estoq.it-codigo = saldo-estoq.it-codigo AND
                        movto-estoq.cod-refer = saldo-estoq.cod-refer AND
                        movto-estoq.lote = saldo-estoq.cod-refer AND
                        movto-estoq.tipo-trans = 2
                        NO-LOCK NO-ERROR.
    
               FIND item-uni-estab WHERE
                    item-uni-estab.cod-estabel = saldo-estoq.cod-estabel AND
                    item-uni-estab.it-codigo   = saldo-estoq.it-codigo NO-LOCK NO-ERROR.
    
               FIND LAST inventario WHERE
                         inventario.dt-saldo = da-data-inv
                         USE-INDEX nr-ficha NO-LOCK NO-ERROR.
    
               ASSIGN i-nr-ficha = IF AVAIL inventario 
                                   THEN inventario.nr-ficha + 1
                                   ELSE 1.
    
               CREATE inventario.
               ASSIGN inventario.dt-saldo       = da-data-inv
                      inventario.nr-ficha       = i-nr-ficha
                      inventario.cod-estabel    = item-uni-estab.cod-estabel
                      inventario.cod-depos      = item-uni-estab.deposito-pad
                      inventario.it-codigo      = saldo-estoq.it-codigo
                      inventario.cod-refer      = saldo-estoq.cod-refer
                      inventario.lote           = saldo-estoq.lote
                      inventario.dt-ult-entra   = da-data-inv
                      inventario.dt-ult-saida   = IF AVAIL movto-estoq
                                                  THEN movto-estoq.dt-trans
                                                  ELSE da-data-inv
                      inventario.qtidade-atu    = saldo-estoq.qtidade-atu
                      inventario.val-apurado[1] = 0
                      inventario.situacao       = 4.
           END.
       END.
    */
END PROCEDURE.

PROCEDURE pi-valida-dados.

    /*
    DOWN WITH FRAME f-detalhe.
    DISP " Etiquetas Reservadas com situacao Diferente de 4 (Reservada) " @ c-descricao
         WITH FRAME f-detalhe.
    DOWN WITH FRAME f-detalhe.
    */

    /*
    FOR EACH ped-item-res WHERE
             ped-item-res.faturado = NO NO-LOCK.

        FOR EACH ped-item-rom WHERE
                 ped-item-rom.nr-pedcli = ped-item-res.nr-pedcli AND
                 ped-item-rom.nome-abrev = ped-item-res.nome-abrev AND 
                 ped-item-rom.nr-sequencia = ped-item-res.nr-sequencia NO-LOCK.

            FIND ob-etiqueta WHERE
                 ob-etiqueta.cod-estabel = ped-item-rom.cod-estabel AND
                 ob-etiqueta.num-etiqueta = ped-item-rom.num-etiqueta
                 NO-LOCK NO-ERROR.

            RUN pi-situacao.
            RUN pi-desc-item (INPUT ob-etiqueta.it-codigo).

            IF ob-etiqueta.situacao <> 4 THEN  DO.
                DISPLAY ob-etiqueta.num-etiqueta
                        c-situacao
                        ob-etiqueta.localizacao
                        ob-etiqueta.it-codigo
                        c-descricao
                        ob-etiqueta.cod-refer
                        ob-etiqueta.nr-lote
                        ob-etiqueta.quantidade
                        WITH FRAME f-detalhe.
               DOWN WITH FRAME f-detalhe.
            END.  
        END.
    END.
    */
    
    /*
    DOWN WITH FRAME f-detalhe.
    DISP " Quantidade Rervada Diferente da Quantidade Pedida " @ c-descricao
         WITH FRAME f-detalhe.
    DOWN WITH FRAME f-detalhe.
    */

    /*
    FOR EACH ped-item-res WHERE
             ped-item-res.faturado = NO.
        FIND ped-item OF ped-item-res NO-LOCK.

        IF ped-item-res.qt-pedida = 0 THEN NEXT.

        IF ped-item.qt-pedida <>  ped-item-res.qt-pedida THEN
           DISP ped-item-res.nr-pedcli
                ped-item-res.nome-abrev
                ped-item-res.nr-sequencia
                ped-item-res.qt-pedida.
    END.
    */
      
    /*  etiqueta reservada sem romaneio e reservas
    DEF TEMP-TABLE tt-etq
        FIELD num-etiqueta LIKE ob-etiqueta.num-etiqueta.
    
    FOR EACH ped-reserva WHERE
             ped-reserva.situacao = 1 NO-LOCK.
        FOR EACH ped-reserva-it OF ped-reserva.
    
            FOR EACH ped-reserva-etq OF ped-reserva-it NO-LOCK.
                CREATE tt-etq.
                ASSIGN tt-etq.num-etiqueta = ped-reserva-etq.num-etiqueta.
            END.
        END.
    END.
    
    FOR EACH ob-etiqueta WHERE
             ob-etiqueta.situacao = 4.
    
        FIND ped-item-rom WHERE
             ped-item-rom.cod-estab = ob-etiqueta.cod-estab AND
             ped-item-rom.num-etiqueta = ob-etiqueta.num-etiqueta
             NO-LOCK NO-ERROR.
    
        IF AVAIL ped-item-rom THEN NEXT.
    
        FIND tt-etq WHERE
             tt-etq.num-etiqueta = ob-etiqueta.num-etiqueta NO-ERROR.
    
        IF AVAIL tt-etq THEN NEXT.
        
        DISP ob-etiqueta.num-etiqueta.
    END.
    */
    
    
    /* Etiquetas  em Estoque com Romaneio
    FOR EACH ob-etiqueta WHERE
             ob-etiqueta.cod-estab = '1' AND
             ob-etiqueta.situacao = 3.
        FIND ped-item-rom WHERE
             ped-item-rom.cod-estab = ob-etiqueta.cod-estab AND
             ped-item-rom.num-etiqueta = ob-etiqueta.num-etiqueta
             NO-LOCK NO-ERROR.
    
        IF AVAIL ped-item-rom THEN
           DISP ob-etiqueta.num-etiqueta
                ped-item-rom.nr-pedcli
                ped-item-rom.nr-sequencia.
    END.
    */
    
    /*
    Etiqueta reservas em reserva j  faturada.
    
    FOR EACH ob-etiqueta WHERE
         ob-etiqueta.cod-estab = '1' AND
         ob-etiqueta.situacao = 4.
         
        FIND ped-item-rom WHERE
             ped-item-rom.cod-estab = ob-etiqueta.cod-estab AND
             ped-item-rom.num-etiqueta = ob-etiqueta.num-etiqueta
             NO-LOCK NO-ERROR.

        FIND ped-item-res WHERE
             ped-item-res.nr-pedcli = ped-item-rom.nr-pedcli AND
             ped-item-res.nome-abrev = ped-item-rom.nome-abrev AND 
             ped-item-res.nr-sequencia = ped-item-rom.nr-sequencia NO-LOCK NO-ERROR.
       
        FIND ped-venda WHERE
             ped-venda.nome-abrev = ped-item-res.nome-abrev AND
             ped-venda.nr-pedcli = ped-item-res.nr-pedcli NO-LOCK NO-ERROR.
    
        FIND ped-venda-ext WHERE
             ped-venda-ext.nr-pedido = ped-venda.nr-pedido NO-LOCK NO-ERROR.
    
        IF NOT AVAIL ped-item-res THEN NEXT.
    
        IF ped-item-res.faturado THEN
           DISP ob-etiqueta.num-etiqueta
                ped-venda.nr-pedcli
                ped-venda-ext.num-reserva WHEN AVAIL ped-venda-ext.
                
    
    END.
    */

END PROCEDURE.


