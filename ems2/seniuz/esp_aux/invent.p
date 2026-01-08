/* include de controle de versÆo */
{include/i-prgvrs.i ESSP0108RP 2.04.00.000}

define temp-table tt-param  no-undo
       field destino           as integer
       field arquivo           as char format "x(35)"
       field usuario           as char format "x(12)"
       field data-exec         as date
       field hora-exec         as integer
       field classifica        as integer
       field desc-classifica   as char format "x(40)"
       FIELD cod-estabel       LIKE saldo-estoq.cod-estabel
       FIELD cod-depos-ini     LIKE saldo-estoq.cod-depos
       FIELD cod-depos-fim     LIKE saldo-estoq.cod-depos
       FIELD it-codigo-ini     LIKE saldo-estoq.it-codigo
       FIELD it-codigo-fim     LIKE saldo-estoq.it-codigo
       FIELD cod-refer-ini     LIKE saldo-estoq.cod-refer
       FIELD cod-refer-fim     LIKE saldo-estoq.cod-refer
       FIELD lote-ini          LIKE saldo-estoq.lote
       FIELD lote-fim          LIKE saldo-estoq.lote
       FIELD dt-inventario     LIKE inventario.dt-saldo
       FIELD imp-param         AS LOG.

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

DEF TEMP-TABLE tt-saldo
    FIELD cod-estabel LIKE saldo-estoq.cod-estabel
    FIELD cod-depos   LIKE saldo-estoq.cod-depos
    FIELD it-codigo   LIKE saldo-estoq.it-codigo
    FIELD cod-refer   LIKE saldo-estoq.cod-refer
    FIELD lote        LIKE saldo-estoq.lote
    FIELD qtidade-atu LIKE saldo-estoq.qtidade-atu.

/* defini‡Æo de vari veis  */
DEF VAR h-acomp AS HANDLE NO-UNDO.

FORM 
    "*------------ Parƒmetros/Sele‡Æo -------------*" SKIP
    tt-param.cod-depos-ini    LABEL "Dep¢sito........"
    "A"  AT 30
    tt-param.cod-depos-fim    NO-LABELS SKIP
    tt-param.dt-inventario    LABEL "Data Inventario."
    SKIP(1)
    WITH FRAME f-param side-labels no-box width 132 STREAM-IO.

form
    tt-saldo.cod-estabel
    tt-saldo.cod-depos 
    tt-saldo.it-codigo
    tt-saldo.cod-refer
    tt-saldo.lote
    tt-saldo.qtidade-atu
    inventario.qtidade-atu
    with no-box 55 down width 132 STREAM-IO frame f-detalhe.

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i}

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

ASSIGN c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i ESPECIFICOS * r}
ASSIGN c-sistema = TRIM(return-value).
{utp/ut-liter.i Itens_com_Saldo_sem_Inventario * r}
ASSIGN c-titulo-relat = TRIM(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

FOR EACH saldo-estoq WHERE
         saldo-estoq.cod-estabel = tt-param.cod-estabel AND
         saldo-estoq.cod-depos >= tt-param.cod-depos-ini AND
         saldo-estoq.cod-depos <= tt-param.cod-depos-fim AND
         saldo-estoq.it-codigo >= tt-param.it-codigo-ini AND
         saldo-estoq.it-codigo <= tt-param.it-codigo-fim AND
         saldo-estoq.cod-refer >= tt-param.cod-refer-ini AND
         saldo-estoq.cod-refer <= tt-param.cod-refer-fim AND
         saldo-estoq.lote      >= tt-param.lote-ini AND
         saldo-estoq.lote      <= tt-param.lote-fim NO-LOCK.

    FIND tt-saldo WHERE
         tt-saldo.cod-estabel = saldo-estoq.cod-estabel AND
         tt-saldo.cod-depos = saldo-estoq.cod-depos  AND
         tt-saldo.it-codigo = saldo-estoq.it-codigo AND
         tt-saldo.cod-refer = saldo-estoq.cod-refer AND
         tt-saldo.lote = saldo-estoq.lote NO-ERROR.

    IF NOT AVAIL tt-saldo THEN DO.
       CREATE tt-saldo.
       ASSIGN tt-saldo.cod-estabel = saldo-estoq.cod-estabel
              tt-saldo.cod-depos = saldo-estoq.cod-depos  
              tt-saldo.it-codigo = saldo-estoq.it-codigo
              tt-saldo.cod-refer = saldo-estoq.cod-refer
              tt-saldo.lote = saldo-estoq.lote.
    END.
    ASSIGN tt-saldo.qtidade-atu = tt-saldo.qtidade-atu + saldo-estoq.qtidade-atu.
END.

FOR EACH tt-saldo WHERE
         tt-saldo.qtidade-atu <> 0 NO-LOCK.

    FIND inventario WHERE
         inventario.dt-saldo = tt-param.dt-inventario AND
         inventario.cod-estabel = tt-saldo.cod-estabel AND
         inventario.cod-depos = tt-saldo.cod-depos AND
         inventario.it-codigo = tt-saldo.it-codigo AND
         inventario.cod-refer = tt-saldo.cod-refer AND
         inventario.lote = tt-saldo.lote NO-LOCK NO-ERROR.

    DISP tt-saldo.cod-estabel
         tt-saldo.cod-depos
         tt-saldo.it-codigo
         tt-saldo.cod-refer
         tt-saldo.lote
         tt-saldo.qtidade-atu
         inventario.qtidade-atu 
         WITH FRAME f-detalhe.
END.

DOWN WITH FRAME f-detalhe.

IF tt-param.imp-param THEN DO:
   PAGE.
   DISPLAY tt-param.cod-depos-ini
           tt-param.cod-depos-fim
           tt-param.dt-inventario
           WITH FRAME f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
RUN pi-finalizar in h-acomp.
RETURN "OK":U.
