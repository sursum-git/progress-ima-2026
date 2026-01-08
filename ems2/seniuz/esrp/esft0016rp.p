/* Programa: ESFT0016.P
** Sistema.: EMS da DATASUL S/A.
** Modulo..: Faturamento
** Objetivo: Emitir o relat¢iro Faturamento por Item/Vencimento
** Autor...: Gilvando de Souza Araujo - Novembro/2006
** Obs.....: Especifico da TEAR TÒXTIL INDéSTRIA E COMRCIO LTDA.
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESFT0016RP 2.04.00.000}

DEFINE TEMP-TABLE tt-param  NO-UNDO
       FIELD destino           AS INTEGER
       FIELD arquivo           AS CHAR FORMAT "x(35)"
       FIELD usuario           AS CHAR FORMAT "x(12)"
       FIELD data-exec         AS DATE
       FIELD hora-exec         AS INTEGER
       FIELD classifica        AS INTEGER
       FIELD desc-classifica   AS CHAR FORMAT "x(40)"
       FIELD cod-estabel       LIKE nota-fiscal.cod-estabel 
       FIELD dt-emissao-ini    LIKE nota-fiscal.dt-emis-nota
       FIELD dt-emissao-fin    LIKE nota-fiscal.dt-emis-nota
       FIELD it-codigo-ini     LIKE ITEM.it-codigo
       FIELD it-codigo-fin     LIKE ITEM.it-codigo
       FIELD ge-codigo-ini     LIKE ITEM.ge-codigo
       FIELD ge-codigo-fin     LIKE ITEM.ge-codigo
       FIELD imp-param         AS LOG.

define temp-table tt-raw-digita
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

DEFINE TEMP-TABLE tt-work
       FIELD it-codigo  LIKE ITEM.it-codigo
       FIELD avista     AS DEC
       FIELD ate30d     AS DEC
       FIELD ate60d     AS DEC
       FIELD ate90d     AS DEC
       FIELD aci90d     AS DEC
       INDEX ch-work it-codigo.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* include padrÆo para impressÆo de campos editores em relat¢rio  */
{include/tt-edit.i}

/* defini‡Æo de vari veis  */
def var h-acomp as handle no-undo.

def var de-tot-avista  as dec format ">>>>>>,>>9.99".
def var de-tot-ate30d  as dec format ">>>>>>,>>9.99".
def var de-tot-ate60d  as dec format ">>>>>>,>>9.99".
def var de-tot-ate90d  as dec format ">>>>>>,>>9.99".
def var de-tot-aci90d  as dec format ">>>>>>,>>9.99".
def var de-tot-total   as dec format ">>>>>>,>>9.99".
def var de-val-total   as dec format ">>>>>>,>>9.99".
DEF VAR de-sld-parcela LIKE fat-duplic.vl-parcela.
DEF VAR de-val-item    LIKE fat-duplic.vl-parcela.
DEF VAR i-cont-itens   AS INT.
DEF VAR i-seq-nota     AS INT.
DEF VAR i-dias         AS INT.

form 
    "*------------ Parƒmetros/Sele‡Æo -------------*" SKIP
    tt-param.cod-estabel      LABEL "Estabelec....." SKIP
    tt-param.dt-emissao-ini   LABEL "Data Emissao.."
    "A"  AT 30
    tt-param.dt-emissao-fin   NO-LABELS SKIP
    tt-param.it-codigo-ini    LABEL "Item.........."
    "A"  AT 30                                      
    tt-param.it-codigo-fin    NO-LABELS SKIP        
    tt-param.ge-codigo-ini    LABEL "Grupo Estoque."
    "A"  AT 30                                      
    tt-param.ge-codigo-fin    NO-LABELS SKIP        
    skip(1)
    with FRAME f-param side-labels no-box width 132 STREAM-IO.

FORM                                 
    item.it-codigo LABEL "Item"         FORMAT "x(8)"
    item.desc-item LABEL "Descricao"    FORMAT "x(36)"
    tt-work.avista LABEL "a Vista"      FORMAT ">>>>>>,>>9.99"
    tt-work.ate30d LABEL "01 a 30 Dias" FORMAT ">>>>>>,>>9.99"
    tt-work.ate60d LABEL "31 a 60 Dias" FORMAT ">>>>>>,>>9.99"
    tt-work.ate90d LABEL "61 a 90 Dias" FORMAT ">>>>>>,>>9.99"
    tt-work.aci90d LABEL "Mais 90 Dias" FORMAT ">>>>>>,>>9.99"
    de-val-total   LABEL "Total"        FORMAT ">>>>>>,>>9.99"
    WITH NO-BOX 55 DOWN WIDTH 132 STREAM-IO FRAME f-detalhe NO-LABEL.

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i}

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i FATURAMENTO * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Faturamento_por_Item/Vencimento * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

FOR EACH nota-fiscal WHERE nota-fiscal.cod-estabel  =  tt-param.cod-estabel
                       AND nota-fiscal.serie        =  "1"
                       AND nota-fiscal.dt-emis-nota >= tt-param.dt-emissao-ini
                       AND nota-fiscal.dt-emis-nota <= tt-param.dt-emissao-fin
                       AND nota-fiscal.dt-cancela   = ?
                       AND nota-fiscal.emite-dup    = YES
                     NO-LOCK:
    
    RUN pi-acompanhar IN h-acomp (INPUT "Nota Fiscal: " + nota-fiscal.nr-nota-fis).

    FOR EACH fat-duplic WHERE fat-duplic.cod-estabel = nota-fiscal.cod-estabel
                          AND fat-duplic.serie       = nota-fiscal.serie      
                          AND fat-duplic.nr-fatura   = nota-fiscal.nr-nota-fis
                        NO-LOCK:

        ASSIGN de-sld-parcela = fat-duplic.vl-parcela
               i-dias         = fat-duplic.dt-venciment - fat-duplic.dt-emissao
               i-seq-nota     = 0
               i-cont-itens   = 0.

        /* ------ Conta itens da nota fiscal ------*/
        FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK:
            ASSIGN i-cont-itens = i-cont-itens + 1.
        END.

        /*--- Rateia parcelas pelos itens da nota fiscal ---*/
        FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK:
            ASSIGN i-seq-nota = i-seq-nota + 1.
            IF i-seq-nota = i-cont-itens THEN
               ASSIGN de-val-item = de-sld-parcela.
            ELSE
               ASSIGN de-val-item    = ROUND(fat-duplic.vl-parcela / nota-fiscal.vl-tot-nota * it-nota-fisc.vl-tot-item, 2)
                      de-sld-parcela = de-sld-parcela - de-val-item.

            FIND FIRST tt-work WHERE tt-work.it-codigo = it-nota-fisc.it-codigo NO-LOCK NO-ERROR.
            IF NOT AVAIL tt-work THEN DO:
               CREATE tt-work.
               ASSIGN tt-work.it-codigo = it-nota-fisc.it-codigo.
            END.
            IF i-dias = 0 THEN
               ASSIGN tt-work.avista = tt-work.avista + de-val-item.
            ELSE
            IF i-dias <= 30 THEN
               ASSIGN tt-work.ate30d = tt-work.ate30d + de-val-item.
            ELSE
            IF i-dias <= 60 THEN
               ASSIGN tt-work.ate60d = tt-work.ate60d + de-val-item.
            ELSE
            IF i-dias <= 90 THEN
               ASSIGN tt-work.ate90d = tt-work.ate90d + de-val-item.
            ELSE
               ASSIGN tt-work.aci90d = tt-work.aci90d + de-val-item.
        END.
    END.
END.

FOR EACH tt-work,
    EACH ITEM WHERE ITEM.it-codigo =  tt-work.it-codigo
                AND ITEM.it-codigo >= tt-param.it-codigo-ini
                AND ITEM.it-codigo <= tt-param.it-codigo-fin
                AND ITEM.ge-codigo >= tt-param.ge-codigo-ini
                AND ITEM.ge-codigo <= tt-param.ge-codigo-fin
              NO-LOCK
     BREAK BY tt-work.it-codigo:

    RUN pi-acompanhar IN h-acomp (INPUT "Item: " + STRING(tt-work.it-codigo)).

    ASSIGN de-val-total = tt-work.avista +
                          tt-work.ate30d +
                          tt-work.ate60d +
                          tt-work.ate90d +
                          tt-work.aci90d.
    DISPLAY ITEM.it-codigo
            ITEM.desc-item
            tt-work.avista
            tt-work.ate30d
            tt-work.ate60d
            tt-work.ate90d
            tt-work.aci90d
            de-val-total
            WITH FRAME f-detalhe.
    DOWN WITH FRAME f-detalhe.

    ASSIGN de-tot-avista = de-tot-avista + tt-work.avista
           de-tot-ate30d = de-tot-ate30d + tt-work.ate30d
           de-tot-ate60d = de-tot-ate60d + tt-work.ate60d
           de-tot-ate90d = de-tot-ate90d + tt-work.ate90d
           de-tot-aci90d = de-tot-aci90d + tt-work.aci90d
           de-tot-total  = de-tot-total  + de-val-total.
END.

DISPLAY "Total Geral" @ item.it-codigo          
        de-tot-avista @ tt-work.avista
        de-tot-ate30d @ tt-work.ate30d
        de-tot-ate60d @ tt-work.ate60d
        de-tot-ate90d @ tt-work.ate90d
        de-tot-aci90d @ tt-work.aci90d
        de-tot-total  @ de-val-total
        WITH FRAME f-detalhe.

IF tt-param.imp-param THEN DO:
   PAGE.
   DISPLAY tt-param.cod-estabel
           tt-param.dt-emissao-ini
           tt-param.dt-emissao-fin
           tt-param.dt-emissao-ini
           tt-param.dt-emissao-fin
           tt-param.it-codigo-ini 
           tt-param.it-codigo-fin 
           tt-param.ge-codigo-ini 
           tt-param.ge-codigo-fin 
           WITH frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

