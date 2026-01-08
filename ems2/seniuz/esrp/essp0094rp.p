/* Programa: ESSP0094
** Sistema.: EMS da Datasul
** Modulo..: Engenharia
** Objetivo: Relatorio de Onde se usa por/Item/Data de validade.
** Autor...: Gilvando de Souza Araujo
** Data....: Janeiro/2005
** Obs.....: Programa especifico da TEAR TEXTIL IND COM LTDA
**
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESSP0094RP 2.04.00.000}

define temp-table tt-param  no-undo
       field destino           as integer
       field arquivo           as char format "x(35)"
       field usuario           as char format "x(12)"
       field data-exec         as date
       field hora-exec         as integer
       field classifica        as integer
       field desc-classifica   as char format "x(40)"
       FIELD it-codigo-ini     LIKE estrutura.it-codigo         
       FIELD it-codigo-fin     LIKE estrutura.it-codigo
       FIELD data-inic-val-ini LIKE estrutura.data-inicio
       FIELD data-inic-val-fin LIKE estrutura.data-inicio
       FIELD data-term-val-ini LIKE estrutura.data-termino
       FIELD data-term-val-fin LIKE estrutura.data-termino
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

/* defini‡Æo de vari veis  */
def var h-acomp as handle no-undo.
DEF VAR c-desc-item AS CHAR FORMAT "x(36)".

form 
    "*------------- Parƒmetros/Sele‡Æo --------------*" SKIP
    tt-param.it-codigo-ini     label "Item.............."
    "A"  AT 38
    tt-param.it-codigo-fin     NO-LABELS SKIP
    tt-param.data-inic-val-ini LABEL "Data Inic.Validade"
    "A"  AT 38
    tt-param.data-inic-val-fin NO-LABELS SKIP
    tt-param.data-term-val-ini LABEL "Data Term.Validade"
    "A"  AT 38
    tt-param.data-term-val-fin NO-LABELS SKIP
    skip(1)
    with FRAME f-param side-labels no-box width 132 STREAM-IO.

form
    estrutura.es-codigo 
    c-desc-item            LABEL "Descricao"      
    estrutura.it-codigo
    estrutura.sequencia
    estrutura.fantasma
    estrutura.quant-usada
    estrutura.data-inicio
    with no-box 55 down width 132 STREAM-IO frame f-detalhe.

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i}

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i ESPECIFICOS * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Onde_se_usa_por_Item/Data_de_validade * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

for each estrutura WHERE estrutura.es-codigo    >= tt-param.it-codigo-ini
                     AND estrutura.es-codigo    <= tt-param.it-codigo-fin
                     AND estrutura.data-inicio  >= tt-param.data-inic-val-ini
                     AND estrutura.data-inicio  <= tt-param.data-inic-val-fin
                     AND estrutura.data-termino >= tt-param.data-term-val-ini
                     AND estrutura.data-termino <= tt-param.data-term-val-fin
                   NO-LOCK:

    run pi-acompanhar in h-acomp (input "Item: " + string(estrutura.it-codigo)).

    FIND ITEM WHERE ITEM.it-codigo = estrutura.es-codigo NO-LOCK NO-ERROR.
    IF AVAIL ITEM THEN
       ASSIGN c-desc-item = ITEM.desc-item.
    ELSE
       ASSIGN c-desc-item = "".

    DISPLAY estrutura.es-codigo 
            c-desc-item 
            estrutura.it-codigo
            estrutura.sequencia
            estrutura.fantasma
            estrutura.quant-usada
            estrutura.data-inicio
            with frame f-detalhe.
    DOWN WITH FRAME f-detalhe.
end.

IF tt-param.imp-param THEN DO:
   PAGE.
   display tt-param.it-codigo-ini
           tt-param.it-codigo-fin
           tt-param.data-inic-val-ini
           tt-param.data-inic-val-fin
           tt-param.data-term-val-ini
           tt-param.data-term-val-fin
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

