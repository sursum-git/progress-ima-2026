/* Programa: ESSP0120.W
** Modulo..: Controle de Estoque / Controle Acabado
** Objetivo: Listar Itens Reportado EMS Versus Etiquetas
** Autor...: Fábio Coelho Lanza - Junho/2006
** Obs.....: Especifico da TEAR TEXTIL IND.COM.LTDA.
**
*/
/* include de controle de versÆo */
{include/i-prgvrs.i ESSP0120RP 2.04.00.000}

DEFINE temp-table tt-param  no-undo
       field destino             as integer
       field arquivo             as char format "x(35)"
       field usuario             as char format "x(12)"
       field data-exec           as date
       field hora-exec           as integer
       field classifica          as integer
       field desc-classifica     as char format "x(40)"
       field dt-trans-ini        LIKE movto-estoq.dt-trans
       field dt-trans-fin        LIKE movto-estoq.dt-trans
       field it-codigo-ini       LIKE movto-estoq.it-codigo
       field it-codigo-fin       LIKE movto-estoq.it-codigo
       field cod-refer-ini       LIKE ob-etiqueta.cod-refer
       field cod-refer-fin       LIKE ob-etiqueta.cod-refer
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


DEF TEMP-TABLE tt-reporte
    FIELD nr-reporte  LIKE movto-estoq.nr-reporte
    FIELD nr-ord-prod LIKE movto-estoq.nr-ord-prod
    FIELD it-codigo   LIKE movto-estoq.it-codigo
    FIELD cod-refer   LIKE movto-estoq.cod-refer
    FIELD dt-trans    LIKE movto-estoq.dt-trans
    FIELD qtd-ems     LIKE movto-estoq.quantidade
    FIELD qtd-etq     LIKE movto-estoq.quantidade.

/* defini‡Æo de vari veis  */
def var h-acomp as handle no-undo.

def var de-tot-etq     as DECIMAL FORMAT  ">>>>,>>>,>>9.99".

form 
    "*--------------- Parƒmetros/Sele‡Æo ----------------*" SKIP
    tt-param.dt-trans-ini      label "Data Inicial....."
    "A"  AT 36
    tt-param.dt-trans-fin      NO-LABELS SKIP
    tt-param.it-codigo-ini     label "Item............."
    "A"  AT 36                  
    tt-param.it-codigo-fin     NO-LABELS SKIP
    tt-param.cod-refer-ini     label "Referˆncia......."
    "A"  AT 36                    
    tt-param.cod-refer-fin     NO-LABELS SKIP
    with FRAME f-param side-labels no-box width 132 STREAM-IO.

form
    tt-reporte.it-codigo FORMAT "x(6)"              label "Codigo"     
    tt-reporte.cod-refer FORMAT "xx.xxxx.x"         label "Referencia" 
    item.desc-item        FORMAT "x(36)"            label "Descri‡Æo" 
    tt-reporte.dt-trans                             LABEL "Dt Reporte" 
    tt-reporte.nr-ord-prod                          LABEL "Ordem" 
    de-tot-etq                                      label "Qtd Etiq" 
    tt-reporte.qtd-ems                              label "Qtd EMS" 
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

{utp/ut-liter.i ESPECÖFICOS * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Rela‡Æo_da_PRODU€ÇO_X_ETIQUETAS * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).
 
FOR EACH movto-estoq 
   WHERE movto-estoq.dt-trans   >= tt-param.dt-trans-ini   
     AND movto-estoq.dt-trans   <= tt-param.dt-trans-fin
     AND movto-estoq.it-codigo  >= tt-param.it-codigo-ini
     AND movto-estoq.it-codigo  <= tt-param.it-codigo-fin
     AND movto-estoq.cod-refer  >= tt-param.cod-refer-ini
     AND movto-estoq.cod-refer  <= tt-param.cod-refer-fin
     AND (movto-estoq.esp-docto   = 1 OR
          movto-estoq.esp-docto   = 8) NO-LOCK USE-INDEX data-item,
    FIRST ord-prod WHERE
          ord-prod.nr-ord-prod = movto-estoq.nr-ord-prod AND
          ord-prod.cd-planej = "automatico" NO-LOCK,
    FIRST item where 
          item.it-codigo = movto-estoq.it-codigo AND
          item.ge-codigo >= 51 AND
          item.ge-codigo <= 58 NO-LOCK.

    RUN pi-acompanhar IN h-acomp (INPUT "Item: "  + movto-estoq.it-codigo +
                                        " Data: " + STRING (movto-estoq.dt-trans)).

    FIND tt-reporte WHERE
         tt-reporte.nr-reporte = movto-estoq.nr-reporte NO-ERROR.

    IF NOT AVAIL tt-reporte THEN DO.
       CREATE tt-reporte.
       ASSIGN tt-reporte.nr-reporte  = movto-estoq.nr-reporte 
              tt-reporte.nr-ord-prod = movto-estoq.nr-ord-prod
              tt-reporte.it-codigo   = movto-estoq.it-codigo  
              tt-reporte.cod-refer   = movto-estoq.cod-refer  
              tt-reporte.dt-trans    = movto-estoq.dt-trans.
    END.

    IF movto-estoq.esp-docto = 1 THEN
       ASSIGN tt-reporte.qtd-ems = tt-reporte.qtd-ems + movto-estoq.quantidade.
    ELSE
       ASSIGN tt-reporte.qtd-ems = tt-reporte.qtd-ems - movto-estoq.quantidade.
END.

FOR EACH tt-reporte NO-LOCK.
    ASSIGN de-tot-etq = 0.
    FOR EACH ob-etiqueta WHERE
             ob-etiqueta.nr-reporte = tt-reporte.nr-reporte NO-LOCK.
        ASSIGN de-tot-etq = de-tot-etq + ob-etiqueta.qtd-original.
    END.

    IF de-tot-etq <> tt-reporte.qtd-ems THEN DO:
       FIND item WHERE
            item.it-codigo = tt-reporte.it-codigo NO-LOCK NO-ERROR.

       DISPLAY tt-reporte.it-codigo
               tt-reporte.cod-refer
               item.desc-item
               tt-reporte.dt-trans
               tt-reporte.nr-ord-prod
               de-tot-etq
               tt-reporte.qtd-ems
              WITH FRAME f-detalhe.
       DOWN WITH FRAME f-detalhe.
    END.
END.

IF tt-param.imp-param THEN DO:
   PAGE.
   DISPLAY tt-param.it-codigo-ini
           tt-param.it-codigo-fin
           tt-param.cod-refer-ini
           tt-param.cod-refer-fin
           tt-param.dt-trans-ini
           tt-param.dt-trans-fin
           WITH FRAME f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
RUN pi-finalizar IN h-acomp.
RETURN "OK":U.

