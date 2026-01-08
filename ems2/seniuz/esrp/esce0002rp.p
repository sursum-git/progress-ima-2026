/* Programa: ESCP002.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Controle de Producao
** Objetivo: Gerar o relatorio de Quantidade Reservadas nas Ordens por Item
** Autor...: Gilvando de Souza Araujo - Julho/96
** Obs.....: Especifico da CIA.RENASCENCA INDUSTRIAL / SANTA ELISABETH
**
** Conversao para EMS 2.04:
**   Programa: ESCP002.P  =>  ESCE0002RP.P
**   Autor...: Prodb - Toninho
**   Data....: 17/02/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESCE0002RP 2.04.00.000}

define temp-table tt-param no-undo
       field destino          as integer
       field arquivo          as char format "x(35)"
       field usuario          as char format "x(12)"
       field data-exec        as date
       field hora-exec        as integer
       FIELD ord-prod-ini     LIKE ord-prod.nr-ord-prod
       FIELD ord-prod-fim     LIKE ord-prod.nr-ord-prod
       FIELD cd-planejad      LIKE ord-prod.cd-planejad
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

def var de-qtd-ordem as dec.

form
    tt-param.ord-prod-ini  column-label "Ordem de Producao"  AT 1
    "a"
    tt-param.ord-prod-fim  no-labels 
    tt-param.cd-planejad   column-label "Planejador" AT 6
    "-" 
    planejad.nome          NO-LABELS
    with no-box side-labels width 132 STREAM-IO frame f-parlis.

form
    reservas.it-codigo label "Item"
    item.descricao-1   label "Descricao"
    de-qtd-ordem       label "Qtd.Original"
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

{utp/ut-liter.i MATERIAIS * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Reservas_por_Ordem_de_Produ‡Æo/Item * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

for each ord-prod where
         ord-prod.nr-ord-produ >= tt-param.ord-prod-ini and
         ord-prod.nr-ord-produ <= tt-param.ord-prod-fim and
         ord-prod.cd-planejado  = tt-param.cd-planejad and
         ord-prod.it-codigo begins "C" no-lock,
     each reservas WHERE
          reservas.nr-ord-prod = ord-prod.nr-ord-prod NO-LOCK 
          break by reservas.it-codigo:

     run pi-acompanhar in h-acomp (INPUT ord-prod.nr-ord-prod).

     assign de-qtd-ordem = de-qtd-ordem + reservas.quant-orig.
     if last-of(reservas.it-codigo) then do:
        find item where
             item.it-codigo = reservas.it-codigo no-lock no-error.

        display reservas.it-codigo
                item.descricao-1
                de-qtd-ordem
                with frame f-detalhe.
        down with frame f-detalhe.

        assign de-qtd-ordem = 0.
     end.
end.

IF tt-param.impr-param THEN DO:
   PAGE.
   PUT "*****----- PAR¶METROS ------*****"
       SKIP(1).

   FIND planejad WHERE
        planejad.cd-planejad = tt-param.cd-planejad NO-LOCK NO-ERROR.

   display tt-param.ord-prod-ini 
           tt-param.ord-prod-fim 
           tt-param.cd-planejad  
           planejad.nome
           with frame f-parlis.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

