/* Programa: ESSP0142.W
** Modulo..: Controle de Expedi‡Æo
** Objetivo: Listar Inconsistencia do REPORTE versus ETIQUETAS.
** Autor...: Fábio Coelho Lanza - JANEIRO/2009
** Obs.....: Especifico da TEAR TEXTIL IND.COM.LTDA.
**
*/
/* include de controle de versÆo */
{include/i-prgvrs.i ESSP0185RP 2.04.00.000}

DEFINE TEMP-TABLE tt-param   NO-UNDO
       FIELD destino            AS INTEGER
       FIELD arquivo            AS CHAR FORMAT "x(35)"
       FIELD usuario            AS CHAR FORMAT "x(12)"
       FIELD data-exec          AS DATE
       FIELD hora-exec          AS INTEGER
       FIELD classifica         AS INTEGER
       FIELD desc-classifica    AS CHAR FORMAT "x(40)"
       FIELD dt-movto-ini       AS DATE       
       FIELD dt-movto-fin       AS DATE
       FIELD vlr-tol            AS DEC
       FIELD imp-param          AS LOG.

DEFINE TEMP-TABLE tt-raw-digita
       FIELD raw-digita as raw.

DEF TEMP-TABLE tt-work
    FIELD dt-trans    LIKE movto-estoq.dt-trans
    FIELD nr-ord-prod LIKE movto-estoq.nr-ord-prod
    FIELD nr-reporte  LIKE movto-estoq.nr-reporte
    FIELD it-codigo   LIKE movto-estoq.it-codigo
    FIELD cod-refer   LIKE movto-estoq.cod-refer
    FIELD lote        LIKE movto-estoq.lote
    FIELD qtd-rep     LIKE movto-estoq.quantidade
    FIELD qtd-etq     LIKE movto-estoq.quantidade
    INDEX ch-work dt-trans 
                  nr-ord-prod
                  it-codigo
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
def var h-acomp     as handle no-undo.
DEF VAR de-dif      AS DEC.
DEF VAR de-qt-etq   AS DEC.

form 
    "*--------------- Parƒmetros/Sele‡Æo ----------------*" SKIP
    tt-param.dt-movto-ini   LABEL  "Data Movimento"
    "A"  AT 40
    tt-param.dt-movto-fin   NO-LABELS SKIP
    tt-param.vlr-tol        LABEL  "Valor de Tolerancia" 
    WITH FRAME f-param SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.

form
    tt-work.dt-trans                        LABEL "Data Movto"
    tt-work.nr-ord-prod                     LABEL "O.Produ‡Æo"
    tt-work.it-codigo FORMAT "x(6)"         LABEL "Item"
    tt-work.cod-refer FORMAT "99-9999-9"    LABEL "Referˆncia"
    tt-work.lote                            LABEL "Lote"
    tt-work.qtd-rep FORMAT ">>>,>>>,>>9.99" LABEL "Qtd. Reporte"
    tt-work.qtd-etq FORMAT ">>>,>>>,>>9.99" LABEL "Qtd. Etiquetas"
    de-dif          FORMAT ">>>,>>>,>>9.99" LABEL "Diferen‡a"
    with no-box NO-LABEL 55 down WIDTH 132 STREAM-IO frame f-detalhe.

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i}

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i ESPECÖFICOS r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Consistencia_REPORTE_x_ETIQUETAS r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).
 
RUN pi-calc (1).  /* ACA - (Acabado/Produzido)  */
RUN pi-calc (8).  /* EAC - (Estorno do Produzido) */

FOR EACH tt-work.
    ASSIGN de-dif = tt-work.qtd-rep - tt-work.qtd-etq.
    IF de-dif >= tt-param.vlr-tol AND de-dif <> 0 THEN DO:
       ACCUMULATE tt-work.qtd-rep (TOTAL).
       ACCUMULATE tt-work.qtd-etq (TOTAL).
       ACCUMULATE de-dif (TOTAL).

       DISP tt-work.dt-trans
            tt-work.nr-ord-prod
            tt-work.it-codigo
            tt-work.cod-refer
            tt-work.lote
            tt-work.qtd-rep
            tt-work.qtd-etq
            de-dif
            WITH FRAME f-detalhe.
       DOWN WITH FRAME f-detalhe.
    END.
END.

DOWN WITH FRAME f-detalhe.
DISP FILL("-",14) FORMAT "x(14)" @ tt-work.qtd-rep
     FILL("-",14) FORMAT "x(14)" @ tt-work.qtd-etq
     FILL("-",14) FORMAT "x(14)" @ de-dif
     WITH FRAME f-detalhe.
DOWN WITH FRAME f-detalhe.
DISP (ACCUM TOTAL tt-work.qtd-rep) @ tt-work.qtd-rep
     (ACCUM TOTAL tt-work.qtd-etq) @ tt-work.qtd-etq
     (ACCUM TOTAL de-dif)          @ de-dif
     WITH FRAME f-detalhe.

IF tt-param.imp-param THEN DO:
   PAGE.
   DISPLAY tt-param.dt-movto-ini FORMAT "99/99/9999"
           tt-param.dt-movto-fin FORMAT "99/99/9999"
           tt-param.vlr-tol
           WITH FRAME f-param.
END. 

/* P R O C E D I M E N T O */  
/*                         */

PROCEDURE pi-calc.
    DEF INPUT PARAMETER p-esp AS INT.

    FOR EACH movto-estoq WHERE
             movto-estoq.esp-docto  = p-esp AND
             movto-estoq.dt-trans  >= tt-param.dt-movto-ini AND
             movto-estoq.dt-trans  <= tt-param.dt-movto-fin NO-LOCK USE-INDEX esp-data.
    
        IF movto-estoq.lote BEGINS 'CA' THEN NEXT.
    
        FIND ord-prod WHERE
             ord-prod.nr-ord-prod = movto-estoq.nr-ord-prod NO-LOCK NO-ERROR.
        IF AVAIL ord-prod AND ord-prod.cd-planej <> 'automatico' THEN NEXT.
    
        ASSIGN de-qt-etq = 0.
        FOR EACH ob-etiqueta WHERE
                 ob-etiqueta.nr-reporte = movto-estoq.nr-reporte NO-LOCK.
            ASSIGN de-qt-etq = de-qt-etq + ob-etiqueta.qtd-ori.
        END.
    
        FIND FIRST tt-work WHERE
                   tt-work.dt-trans  = movto-estoq.dt-trans  AND
                   tt-work.it-codigo = movto-estoq.it-codigo AND 
                   tt-work.lote      = movto-estoq.lote
                   NO-ERROR.      
        IF NOT AVAIL tt-work THEN DO.
           CREATE tt-work.
           ASSIGN tt-work.dt-trans     = movto-estoq.dt-trans   
                  tt-work.nr-ord-prod  = movto-estoq.nr-ord-prod
                  tt-work.nr-reporte   = movto-estoq.nr-reporte 
                  tt-work.it-codigo    = movto-estoq.it-codigo  
                  tt-work.cod-refer    = movto-estoq.cod-refer
                  tt-work.lote         = movto-estoq.lote.
        END.
    
        ASSIGN tt-work.qtd-etq = tt-work.qtd-etq + de-qt-etq.
        IF movto-estoq.esp-docto = 1 THEN /* Acabado/Produzido */
           ASSIGN tt-work.qtd-rep = tt-work.qtd-rep + movto-estoq.quantidade.
        ELSE /* Estorno */
           ASSIGN tt-work.qtd-rep = tt-work.qtd-rep - movto-estoq.quantidade.
    END.
END PROCEDURE.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
RUN pi-finalizar IN h-acomp.
RETURN "OK":U.
