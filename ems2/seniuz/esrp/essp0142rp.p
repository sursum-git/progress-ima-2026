/* Programa: ESSP0142.W
** Modulo..: Controle de Expedi‡Æo
** Objetivo: Listar a Consulta Itens X Etiquetas
** Autor...: F bio Coelho Lanza - Dezembro/2006
** Obs.....: Especifico da TEAR TEXTIL IND.COM.LTDA.
**
*/
/* include de controle de versÆo */
{include/i-prgvrs.i ESSP0142RP 2.04.00.000}

DEF BUFFER empresa FOR mgcad.empresa.

DEFINE TEMP-TABLE tt-param   NO-UNDO
       FIELD destino            AS INTEGER
       FIELD arquivo            AS CHAR FORMAT "x(35)"
       FIELD usuario            AS CHAR FORMAT "x(12)"
       FIELD data-exec          AS DATE
       FIELD hora-exec          AS INTEGER
       FIELD classifica         AS INTEGER
       FIELD desc-classifica    AS CHAR FORMAT "x(40)"
       FIELD cod-estabel        LIKE saldo-estoq.cod-estabel
       FIELD it-codigo-ini      LIKE saldo-estoq.it-codigo
       FIELD it-codigo-fin      LIKE saldo-estoq.it-codigo
       FIELD cod-refer-ini      LIKE saldo-estoq.cod-refer 
       FIELD cod-refer-fin      LIKE saldo-estoq.cod-refer
       FIELD cod-depos-ini      LIKE saldo-estoq.cod-depos 
       FIELD cod-depos-fin      LIKE saldo-estoq.cod-depos
       FIELD lote-ini           AS CHAR FORMAT "x(2)"       
       FIELD lote-fin           AS CHAR FORMAT "x(2)" 
       FIELD corte-comerc-ini   LIKE corte-comerc.codigo 
       FIELD corte-comerc-fin   LIKE corte-comerc.codigo
       FIELD data-ini           AS DATE
       FIELD data-fin           AS DATE
       FIELD i-opc-acab         AS INT
       FIELD c-tipo-artigo      AS CHAR
       FIELD l-imp              AS LOG
       FIELD l-prod             as LOG
       FIELD l-est              AS LOG 
       FIELD l-res              AS LOG 
       FIELD l-fat              AS LOG       
       FIELD l-reproc           AS LOG
       FIELD l-corte            AS LOG
       FIELD l-bloq             AS LOG
       FIELD l-cons             AS LOG
       FIELD tipo-relatorio     AS INTEGER
       FIELD item-dif           AS LOG
       FIELD item-sem-est       AS LOG
       FIELD desc-tipo-relat    AS CHAR FORMAT "x(20)"
       FIELD imp-param          AS LOG.

DEFINE TEMP-TABLE tt-raw-digita
       FIELD raw-digita as raw.

DEFINE TEMP-TABLE tt-work
       FIELD it-codigo    LIKE saldo-estoq.it-codigo  
       FIELD cod-refer    AS CHAR FORMAT "x(10)"    
       FIELD desc-item    AS CHAR FORMAT "x(30)"     
       FIELD cod-obsoleto AS CHAR
       FIELD tipo-tear    AS CHAR FORMAT "x(1)"
       FIELD num-etiqueta LIKE ob-etiqueta.num-etiqueta
       FIELD localiz      LIKE ob-etiqueta.localizacao   
       FIELD situacao     AS CHAR FORMAT "x(12)"
       FIELD qtd-etq      LIKE ob-etiqueta.quantidade
       FIELD qtd-ems      LIKE saldo-estoq.qtidade-atu
       INDEX ch-work it-codigo
                     cod-refer
                     num-etiqueta.

DEFINE TEMP-TABLE tt-work1
       FIELD it-codigo    LIKE saldo-estoq.it-codigo  
       FIELD cod-refer    AS CHAR FORMAT "x(10)"    
       FIELD desc-item    AS CHAR FORMAT "x(30)"
       FIELD qtd-ems      AS DEC FORMAT "->>>,>>>,>>9.99"                     
       INDEX ch-work it-codigo
                     cod-refer.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* include padrÆo para impressÆo de campos editores em relat¢rio  */
{include/tt-edit.i}

/* defini‡Æo de vari veis  */
def var h-acomp        AS handle no-undo.
DEF VAR de-tot-sld     AS DEC  FORMAT "->>>,>>9.99".
DEF VAR de-tot-res     AS DEC  FORMAT "->>>,>>9.99".
DEF VAR de-tot-etq     AS DEC  FORMAT "->>>,>>9.99".
DEF VAR de-ger-sld     AS DEC  FORMAT "->>>,>>>,>>9.99".
DEF VAR de-ger-etq     AS DEC  FORMAT "->>>,>>>,>>9.99".
DEF VAR de-sld-refer   AS DEC  FORMAT "->>>,>>9.99".
DEF VAR de-etq-refer   AS DEC  FORMAT "->>>,>>9.99".
DEF VAR c-situacao     AS CHAR FORMAT "x(12)".
DEF VAR c-cod-refer    AS CHAR FORMAT "x(11)".
DEF VAR c-tipo-tear    AS CHAR FORMAT "x(1)".
DEF VAR c-corte-comerc AS CHAR FORMAT "x(15)".
DEF VAR c-sit-etq      AS CHAR.
DEF VAR c-est-contabil  AS CHAR.
DEF VAR de-dif-etq     AS DEC  FORMAT "->>>,>>9.9999".
DEF VAR de-tot-dif-etq AS DEC  FORMAT "->>>,>>9.99". 
DEF VAR de-ger-dif-etq AS DEC  FORMAT "->>>,>>9.99".

form 
    "*--------------- Parƒmetros/Sele‡Æo ----------------*" SKIP
    tt-param.it-codigo-ini LABEL  "Item......"
    "A"  AT 40
    tt-param.it-codigo-fin NO-LABELS SKIP
    tt-param.cod-refer-ini LABEL  "Referˆncia"
    "A"  AT 40
    tt-param.cod-refer-fin NO-LABELS SKIP
    tt-param.lote-ini      LABEL  "Lote"
    "A"  AT 40
    tt-param.lote-fin      NO-LABELS SKIP
    tt-param.cod-depos-ini LABEL  "Dep¢sito"
    "A"  AT 40
    tt-param.cod-depos-fin      NO-LABELS SKIP
    tt-param.desc-tipo-rel      LABEL "Tipo Relatorio..." SKIP
    tt-param.item-dif           LABEL "Somente Itens c/Diferenca "
    WITH FRAME f-param SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.

form
    tt-work.it-codigo FORMAT "x(6)"            LABEL "Item"     
    tt-work.cod-refer                          LABEL "Referˆncia"
    tt-work.desc-item FORMAT "x(30)"           LABEL "Descri‡Æo" 
    tt-work1.qtd-ems                           LABEL "Estoque EMS"
    tt-work.cod-obsoleto                       LABEL "CO"
    tt-work.num-etiqueta                       LABEL "N§Etiqueta"
    tt-work.localiz   FORMAT "XXX/XXX"         LABEL "Localiz"
    tt-work.situacao                           LABEL "Situa‡Æo"
    tt-work.qtd-etq   FORMAT "->>>,>>>,>>9.99" LABEL "Qtde" 
    with no-box NO-LABEL 55 down WIDTH 132 STREAM-IO frame f-detalhe.

form
    tt-work.it-codigo FORMAT "x(6)"            LABEL "Item"     
    tt-work.cod-refer                          LABEL "Referˆncia"
    tt-work.desc-item FORMAT "x(30)"           LABEL "Descri‡Æo" 
    tt-work1.qtd-ems                           LABEL "Estoque EMS"
    tt-work.qtd-etq   FORMAT "->>>,>>>,>>9.99" LABEL "Qtde"
    de-dif-etq        FORMAT "->,>>>,>>9.9999" LABEL "Diferen‡a"
    with no-box NO-LABEL 55 down WIDTH 132 STREAM-IO frame f-detalhe-sint.

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
{utp/ut-liter.i Estoque_de_Itens_X_Etiqueta * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

ASSIGN c-sit-etq = "".
ASSIGN c-sit-etq = c-sit-etq + IF tt-param.l-imp    = YES THEN "1," ELSE ",".
ASSIGN c-sit-etq = c-sit-etq + IF tt-param.l-prod   = YES THEN "2," ELSE ",".
ASSIGN c-sit-etq = c-sit-etq + IF tt-param.l-est    = YES THEN "3," ELSE ",".
ASSIGN c-sit-etq = c-sit-etq + IF tt-param.l-res    = YES THEN "4," ELSE ",".
ASSIGN c-sit-etq = c-sit-etq + IF tt-param.l-fat    = YES THEN "5," ELSE ",".
ASSIGN c-sit-etq = c-sit-etq + IF tt-param.l-reproc = YES THEN "6," ELSE ",".
ASSIGN c-sit-etq = c-sit-etq + IF tt-param.l-corte  = YES THEN "7," ELSE ",".
ASSIGN c-sit-etq = c-sit-etq + IF tt-param.l-bloq   = YES THEN "8," ELSE ",".
ASSIGN c-sit-etq = c-sit-etq + IF tt-param.l-cons   = YES THEN "9," ELSE ",".


IF tt-param.item-sem-est THEN
   ASSIGN tt-param.item-dif = YES.

ASSIGN c-est-contabil = tt-param.cod-estabel.
IF tt-param.cod-depos-ini = 'DEF' THEN
   ASSIGN c-est-contabil = '504'.


FOR EACH ITEM WHERE 
         ITEM.it-codigo >= tt-param.it-codigo-ini AND 
         ITEM.it-codigo <= tt-param.it-codigo-fin AND 
         ITEM.ge-codigo >= 50 AND
         ITEM.ge-codigo <= 60 NO-LOCK.

    RUN pi-acompanhar IN h-acomp (INPUT "Item: " + item.it-codigo).

    FIND item-ext WHERE
         item-ext.it-codigo = item.it-codigo NO-LOCK NO-ERROR.

    ASSIGN de-tot-sld = 0.
    FOR EACH saldo-estoq USE-INDEX item WHERE
             saldo-estoq.it-codigo   = ITEM.it-codigo          AND
             saldo-estoq.cod-estabel = c-est-contabil    AND
             saldo-estoq.cod-refer  >= tt-param.cod-refer-ini  AND
             saldo-estoq.cod-refer  <= tt-param.cod-refer-fin  NO-LOCK.

        RUN pi-acompanhar IN h-acomp (INPUT "Saldo Estoque Item: " + saldo-estoq.it-codigo + " Ref. " + saldo-estoq.cod-refer).

        ASSIGN de-tot-sld = de-tot-sld + saldo-estoq.qtidade-atu.

        FIND FIRST tt-work1 WHERE 
                   tt-work1.it-codigo = saldo-estoq.it-codigo AND 
                   tt-work1.cod-refer = saldo-estoq.cod-refer
                   NO-ERROR.
        IF NOT AVAIL tt-work1 THEN DO:
           CREATE tt-work1.
           ASSIGN tt-work1.it-codigo = saldo-estoq.it-codigo
                  tt-work1.cod-refer = saldo-estoq.cod-refer
                  tt-work1.desc-item = ITEM.desc-item.
        END.
        ASSIGN tt-work1.qtd-ems = tt-work1.qtd-ems + de-tot-sld
               de-tot-sld       = 0.

    END. /* KBO SALDO-ESTOQ */    

    FOR EACH ob-etiqueta USE-INDEX indice6 WHERE 
             ob-etiqueta.cod-estabel = tt-param.cod-estabel AND
             ob-etiqueta.it-codigo = ITEM.it-codigo AND           
             ob-etiqueta.cod-refer  >= tt-param.cod-refer-ini AND
             ob-etiqueta.cod-refer  <= tt-param.cod-refer-fin NO-LOCK.
        
        IF ob-etiqueta.num-etiqueta = 0 THEN NEXT.
        IF LOOKUP(STRING(ob-etiqueta.situacao),c-sit-etq) = 0 THEN NEXT.

        ASSIGN c-cod-refer = "".
        IF ITEM.tipo-con-est = 3 OR item.tipo-con-est = 4 THEN
           ASSIGN c-cod-refer = ob-etiqueta.cod-refer.
        
        IF ob-etiqueta.situacao = 5 THEN DO.
           


        END.


        RUN pi-acompanhar in h-acomp (INPUT "Etiquetas " + STRING(ob-etiqueta.num-etiqueta ) + " " + ob-etiqueta.it-codigo + " " + ob-etiqueta.cod-refer).

        {esinc/i-dsrb.i ob-etiqueta.situacao ob-etiqueta.situacao c-situacao} 

        ASSIGN c-corte-comerc = "".
        FIND corte-comerc WHERE corte-comerc.codigo = ob-etiqueta.corte-comerc NO-LOCK NO-ERROR.
        IF AVAIL corte-comerc  THEN 
           ASSIGN c-corte-comerc = corte-comerc.descricao.


        FIND FIRST tt-work WHERE tt-work.it-codigo    = ob-etiqueta.it-codigo
                             AND tt-work.cod-refer    = c-cod-refer 
                             AND tt-work.num-etiqueta = ob-etiqueta.num-etiqueta 
                             NO-ERROR.
        IF NOT AVAIL tt-work THEN DO:
           CREATE tt-work.
           ASSIGN tt-work.it-codigo    = ob-etiqueta.it-codigo
                  tt-work.cod-refer    = c-cod-refer 
                  tt-work.cod-obsoleto = ''
                  tt-work.tipo-tear    = c-tipo-tear
                  tt-work.desc-item    = ITEM.desc-item
                  tt-work.num-etiqueta = ob-etiqueta.num-etiqueta
                  tt-work.localiz      = ob-etiqueta.localizacao
                  tt-work.situacao     = c-situacao.
                  ASSIGN tt-work.qtd-etq = tt-work.qtd-etq + ob-etiqueta.quantidade.
       END.
       /* ASSIGN tt-work.qtd-etq = tt-work.qtd-etq + ob-etiqueta.quantidade. */
    END. /* KBO OB-ETIQUETA */
END. /* KBO ITEM */


/* Inserir Codigo Referencia na Tabela Temporaria da OB-ETIQUETA */
FOR EACH tt-work1.
    FIND FIRST tt-work WHERE 
               tt-work.it-codigo = tt-work1.it-codigo AND 
               tt-work.cod-refer = tt-work1.cod-refer NO-ERROR.
    IF NOT AVAIL tt-work THEN DO:
       CREATE tt-work.
       ASSIGN tt-work.it-codigo    = tt-work1.it-codigo
              tt-work.cod-refer    = tt-work1.cod-refer
              tt-work.desc-item    = tt-work1.desc-item.
    END.
    IF tt-work.qtd-ems = 0 THEN /* Grava QTD EMS Somente na Primeira Referencia */
       ASSIGN tt-work.qtd-ems = tt-work1.qtd-ems.

    IF tt-param.item-sem-est AND
       tt-work1.qtd-ems > 0 THEN 
       DELETE tt-work1.
END.

FOR EACH tt-work. /* Mata Registros com Total Zerados */
    IF tt-work.qtd-ems + tt-work.qtd-etq = 0 THEN DO.
       DELETE tt-work.
       NEXT.
    END.

    IF tt-param.item-sem-est THEN DO.
       FIND FIRST tt-work1 WHERE 
                  tt-work1.it-codigo = tt-work.it-codigo AND 
                  tt-work1.cod-refer = tt-work.cod-refer NO-ERROR.
       IF NOT AVAIL tt-work1 THEN
          DELETE tt-work.
    END.
END.

ASSIGN de-tot-etq = 0
       de-tot-sld = 0
       de-dif-etq = 0.
FOR EACH tt-work BREAK BY tt-work.it-codigo
                       BY tt-work.cod-refer
                       BY tt-work.localiz
                       BY tt-work.tipo-tear
                       BY tt-work.num-etiqueta.

    IF tt-param.tipo-relatorio = 2 THEN DO: /*  ANALITICO */ 
       FIND FIRST tt-work1 WHERE tt-work1.it-codigo = tt-work.it-codigo
                             AND tt-work1.cod-refer = tt-work.cod-refer 
                             NO-ERROR.
       IF AVAIL tt-work1 THEN DO.
           DISPLAY tt-work.it-codigo  WHEN FIRST-OF(tt-work.it-codigo)                                
                   tt-work.cod-refer  WHEN FIRST-OF(tt-work.cod-refer)                              
                   tt-work.desc-item  WHEN FIRST-OF(tt-work.it-codigo)
                   tt-work1.qtd-ems   WHEN FIRST-OF(tt-work.cod-refer)
                   tt-work.cod-obsoleto
                   tt-work.num-etiqueta
                   tt-work.localiz
                   tt-work.situacao
                   tt-work.qtd-etq  
                   WITH FRAME f-detalhe.

           DOWN WITH FRAME f-detalhe.
           ACCUMULATE tt-work.qtd-etq (TOTAL BY tt-work.cod-refer).
           ACCUMULATE tt-work.qtd-etq (TOTAL BY tt-work.it-codigo).
           ACCUMULATE tt-work.qtd-ems (TOTAL BY tt-work.it-codigo).
       END.
       ELSE DO:
            DISPLAY tt-work.it-codigo  WHEN FIRST-OF(tt-work.it-codigo)                                
                    tt-work.cod-refer  WHEN FIRST-OF(tt-work.cod-refer)                              
                    tt-work.desc-item  WHEN FIRST-OF(tt-work.it-codigo)
                    tt-work.cod-obsoleto
                    tt-work.num-etiqueta
                    tt-work.localiz
                    tt-work.situacao
                    tt-work.qtd-etq  
                    WITH FRAME f-detalhe.
            DOWN WITH FRAME f-detalhe.
            ACCUMULATE tt-work.qtd-etq (TOTAL BY tt-work.cod-refer).
            ACCUMULATE tt-work.qtd-etq (TOTAL BY tt-work.it-codigo).
            ACCUMULATE tt-work.qtd-ems (TOTAL BY tt-work.it-codigo).
       END.

       IF LAST-OF(tt-work.cod-refer) THEN DO:
           DISPLAY "TOTAL DA REFERÒNCIA " @ tt-work.desc-item
                   ACCUM TOTAL BY  tt-work.cod-refer tt-work.qtd-etq @ tt-work.qtd-etq
                   WITH FRAME f-detalhe.
           DOWN 1 WITH FRAME f-detalhe.
       END.

       IF LAST-OF(tt-work.it-codigo) THEN DO:
          DISPLAY "TOTAL DO ITEM " @ tt-work.desc-item
                  ACCUM TOTAL BY  tt-work.it-codigo tt-work.qtd-ems @ tt-work1.qtd-ems
                  ACCUM TOTAL BY  tt-work.it-codigo tt-work.qtd-etq @ tt-work.qtd-etq
                  WITH FRAME f-detalhe.
          DOWN 2 WITH FRAME f-detalhe.
       END.
    END.
    ELSE DO: /* SINTETICO */
       IF tt-param.item-dif = YES THEN DO: /* Imprime somente com diferen‡a */
          ASSIGN de-etq-refer = de-etq-refer + tt-work.qtd-etq
                 de-sld-refer = de-sld-refer + tt-work.qtd-ems
                 de-dif-etq =  de-sld-refer - de-etq-refer.

          IF LAST-OF(tt-work.cod-refer) THEN DO:
             IF de-dif-etq <> 0 THEN DO:
                 DISPLAY tt-work.it-codigo 
                         tt-work.cod-refer
                         tt-work.desc-item
                         de-sld-refer  @ tt-work1.qtd-ems
                         de-etq-refer  @ tt-work.qtd-etq
                         de-dif-etq    
                         WITH FRAME f-detalhe-sint.
                 DOWN 1 WITH FRAME f-detalhe-sint.
                 ASSIGN de-tot-etq = de-tot-etq + de-etq-refer
                        de-tot-sld = de-tot-sld + de-sld-refer.
                 
             END.
             ASSIGN de-sld-refer = 0
                    de-etq-refer = 0
                    de-dif-etq   = 0.
          END.
        /*  IF LAST-OF(tt-work.it-codigo) THEN DO:
             IF de-tot-sld <> de-tot-etq THEN DO: 
                ASSIGN de-tot-dif-etq = de-tot-sld - de-tot-etq.
                DISPLAY "TOTAL DO ITEM "  @ tt-work.desc-item
                        de-tot-sld        @ tt-work1.qtd-ems
                        de-tot-etq        @ tt-work.qtd-etq
                        de-tot-dif-etq    @ de-dif-etq
                        WITH FRAME f-detalhe-sint.
                 DOWN 1 WITH FRAME f-detalhe-sint.
                 ASSIGN de-ger-sld = de-ger-sld + de-tot-sld
                        de-ger-etq = de-ger-etq + de-tot-etq
                        de-tot-dif-etq = 0.
             END.
             ASSIGN de-tot-sld     = 0
                    de-tot-etq     = 0
                    de-tot-dif-etq = 0.
          END.  */
       END.
       ELSE DO:  /* Imprime Todos */
          ACCUMULATE tt-work.qtd-etq (TOTAL BY tt-work.cod-refer).
          ACCUMULATE tt-work.qtd-ems (TOTAL BY tt-work.cod-refer).
          ACCUMULATE tt-work.qtd-etq (TOTAL BY tt-work.it-codigo).
          ACCUMULATE tt-work.qtd-ems (TOTAL BY tt-work.it-codigo).
          IF LAST-OF(tt-work.cod-refer) THEN DO:
             DISPLAY tt-work.it-codigo
                     tt-work.cod-refer
                     tt-work.desc-item
                     ACCUM TOTAL BY  tt-work.cod-refer tt-work.qtd-ems @ tt-work1.qtd-ems
                     ACCUM TOTAL BY  tt-work.cod-refer tt-work.qtd-etq @ tt-work.qtd-etq
                     WITH FRAME f-detalhe-sint.
             DOWN 1 WITH FRAME f-detalhe-sint.
          END.
          IF LAST-OF(tt-work.it-codigo) THEN DO:
             DISPLAY "TOTAL DO ITEM 2" @ tt-work.desc-item
                     ACCUM TOTAL BY  tt-work.it-codigo tt-work.qtd-ems @ tt-work1.qtd-ems
                     ACCUM TOTAL BY  tt-work.it-codigo tt-work.qtd-etq @ tt-work.qtd-etq
                     WITH FRAME f-detalhe-sint.
             DOWN 1 WITH FRAME f-detalhe-sint.
          END.
       END.
    END.
END. /* KBO tt-work */

IF tt-param.tipo-relatorio = 2 THEN DO:

    IF tt-param.item-dif = YES AND de-tot-sld + de-tot-etq <> 0 THEN DO: 
       DISPLAY "TOTAL DO ITEM 3"  @ tt-work.desc-item
               de-tot-sld     @ tt-work1.qtd-ems
               de-tot-etq     @ tt-work.qtd-etq
               de-tot-dif-etq @ de-dif-etq
               WITH FRAME f-detalhe.
       DOWN 1 WITH FRAME f-detalhe.
    
       ASSIGN de-tot-sld     = 0
              de-tot-etq     = 0
              de-tot-dif-etq = 0.
    END.

    IF de-ger-sld + de-ger-etq > 0 THEN DO:
       DISPLAY "TOTAL GERAL "  @ tt-work.desc-item
               de-ger-sld      @ tt-work1.qtd-ems
               de-ger-etq      @ tt-work.qtd-etq
               de-ger-dif-etq  @ de-dif-etq
               WITH FRAME f-detalhe.
       DOWN 1 WITH FRAME f-detalhe.
       ASSIGN de-ger-sld     = 0 
              de-ger-etq     = 0
              de-ger-dif-etq = 0.
    END.
END.
ELSE DO:

    ASSIGN de-ger-dif-etq = de-ger-dif-etq +  (de-ger-sld - de-ger-etq).

    IF tt-param.item-dif = YES AND de-tot-sld + de-tot-etq <> 0 THEN DO: 
       DISPLAY "TOTAL DO ITEM "  @ tt-work.desc-item
               de-tot-sld @ tt-work1.qtd-ems
               de-tot-etq @ tt-work.qtd-etq
               de-tot-dif-etq @ de-dif-etq
               WITH FRAME f-detalhe-sint.
       DOWN 1 WITH FRAME f-detalhe-sint.
    
       ASSIGN de-tot-sld     = 0
              de-tot-etq     = 0
              de-tot-dif-etq = 0.
    END.

    IF de-ger-sld + de-ger-etq > 0 THEN DO:
       DISPLAY "TOTAL GERAL "  @ tt-work.desc-item
               de-ger-sld      @ tt-work1.qtd-ems
               de-ger-etq      @ tt-work.qtd-etq
               de-ger-dif-etq  @ de-dif-etq
               WITH FRAME f-detalhe-sint.
       DOWN 1 WITH FRAME f-detalhe-sint.
       ASSIGN de-ger-sld     = 0 
              de-ger-etq     = 0
              de-ger-dif-etq = 0.
    END.
END.

IF tt-param.imp-param THEN DO:
   PAGE.
   DISPLAY tt-param.it-codigo-ini
           tt-param.it-codigo-fin
           tt-param.cod-refer-ini
           tt-param.cod-refer-fin
           tt-param.lote-ini
           tt-param.lote-fin
           tt-param.cod-depos-ini
           tt-param.cod-depos-fin
           tt-param.desc-tipo-rel
           tt-param.item-dif
           WITH FRAME f-param.
END. 

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
RUN pi-finalizar IN h-acomp.

RETURN "OK":U.
