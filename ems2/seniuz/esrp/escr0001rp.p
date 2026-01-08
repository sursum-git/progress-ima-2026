/* Programa: ESPD009.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Contas a Receber
** Objetivo: Listar Titulos a Receber com c lculo de juros sobre atraso
** Autor...: Gilvando de Souza Araujo - Julho/95
** Obs.....: Especifico da CIA.RENASCENCA INDUSTRIAL
**
** Conversao para EMS 2.04:
**   Programa: ESPD009.P  =>  ESCR0001RP.P
**   Autor...: Gilvando Souza Araujo
**   Data....: 15/02/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESCR0001RP 2.04.00.000}

define temp-table tt-param    no-undo
       field destino          as integer
       field arquivo          as char format "x(35)"
       field usuario          as char format "x(12)"
       field data-exec        as date
       field hora-exec        as integer
       FIELD ep-codigo        LIKE titulo.ep-codigo
       FIELD cod-estabel      LIKE titulo.cod-estabel
       FIELD cod-port-ini     LIKE titulo.cod-port 
       FIELD cod-port-fin     LIKE titulo.cod-port
       FIELD cod-esp-ini      LIKE titulo.cod-esp
       FIELD cod-esp-fin      LIKE titulo.cod-esp
       FIELD cod-emitente-ini LIKE titulo.cod-emitente
       FIELD cod-emitente-fin LIKE titulo.cod-emitente
       FIELD dt-vencto-ini    LIKE titulo.dt-vencim
       FIELD dt-vencto-fin    LIKE titulo.dt-vencim
       FIELD dt-base-juro     AS DATE FORMAT "99/99/9999"
       FIELD perc-juro        AS DEC FORMAT ">9,9999"
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

def var l-prim-vez       as log.
def var de-vl-tot-juros  as dec format ">>>,>>>,>>>,>>9.99".
def var de-vl-tot-saldo  as dec format ">>>,>>>,>>>,>>9.99".
def var de-vl-tot-devido as dec format ">>>,>>>,>>>,>>9.99".
def var de-tot-juros     as dec format ">>>,>>>,>>>,>>9.99".
def var de-tot-saldo     as dec format ">>>,>>>,>>>,>>9.99".
def var de-tot-devido    as dec format ">>>,>>>,>>>,>>9.99".
def var i-atraso         as int format ">>9".
def var de-vl-juros      as dec format ">>>,>>>,>>>,>>9.99".
def var de-vl-devido     as dec format ">>>,>>>,>>>,>>9.99".

form
    "*------------ Parƒmetros/Sele‡Æo -------------*" SKIP
    tt-param.ep-codigo        LABEL "Empresa........" AT 1
    tt-param.cod-estabel      label "Estabelecimento" AT 1
    tt-param.cod-port-ini     label "Portador......." AT 1
    "a"  AT 30                
    tt-param.cod-port-fin     no-labels
    tt-param.cod-esp-ini      LABEL "Especie........" AT 1
    "a"  AT 30                
    tt-param.cod-esp-fin      NO-LABELS
    tt-param.cod-emitente-ini LABEL "Cliente........" AT 1
    "a"  AT 30
    tt-param.cod-emitente-fin NO-LABELS
    tt-param.dt-vencto-ini    label "Vencimento....." AT 1
    "a"  AT 30
    tt-param.dt-vencto-fin    NO-LABELS
    tt-param.dt-base-juro     LABEL "Data Base Juro." AT 1
    tt-param.perc-juro        LABEL "Percentual Juro" AT 1
    with no-box side-labels width 132 stream-io frame f-param.

form
    titulo.nome-abrev   label "Cliente"
    titulo.dt-vencimen  label "Dt.Vencto"
    titulo.cod-esp      label "Esp"
    titulo.nr-docto     label "Documento"
    titulo.parcela      label "Pa"
    titulo.vl-saldo     label "Valor Titulo"
    i-atraso            label "Dias"
    titulo.perc-juros   label "Tx.Juros"
    de-vl-juros         label "Vlr.Juros"
    de-vl-devido        label "Vlr.Devido"
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

{utp/ut-liter.i FINANCEIRO * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i T¡tulos_a_Receber_com_C lculo_de_Juros_s/Atraso * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
VIEW frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

for each titulo where titulo.ep-codigo    =  tt-param.ep-codigo
                  and titulo.cod-estabel  =  tt-param.cod-estabel
                  and titulo.cod-esp      >= tt-param.cod-esp-ini
                  and titulo.cod-esp      <= tt-param.cod-esp-fin
                  and titulo.cod-port     >= tt-param.cod-port-ini
                  and titulo.cod-port     <= tt-param.cod-port-fin
                  and titulo.cod-emitente >= tt-param.cod-emitente-ini
                  and titulo.cod-emitente <= tt-param.cod-emitente-fin
                  and titulo.dt-vencimen  >= tt-param.dt-vencto-ini
                  and titulo.dt-vencimen  <= tt-param.dt-vencto-fin
                  and titulo.vl-saldo     > 0
                no-lock
                break by titulo.nome-abrev
                      by titulo.dt-vencimen:
    
    run pi-acompanhar in h-acomp (input "Especie: " + titulo.cod-esp + " Docto: " + titulo.nr-docto).

    assign i-atraso     = tt-param.dt-base-juro - titulo.dt-vencimen
           de-vl-juros  = (((tt-param.perc-juro / 30) * i-atraso) / 100) * titulo.vl-saldo
           de-vl-devido = titulo.vl-saldo + de-vl-juros.

    DISPLAY titulo.nome-abrev
            titulo.dt-vencimen
            titulo.cod-esp
            titulo.nr-docto
            titulo.parcela
            titulo.vl-saldo
            i-atraso
            tt-param.perc-juro   @ titulo.perc-juros
            de-vl-juros
            de-vl-devido
            with frame f-detalhe.
    down with frame f-detalhe.
    assign de-vl-tot-saldo  = de-vl-tot-saldo + titulo.vl-saldo
           de-vl-tot-juros  = de-vl-tot-juros + de-vl-juros
           de-vl-tot-devido = de-vl-tot-devido + de-vl-devido
           de-tot-saldo     = de-tot-saldo + titulo.vl-saldo
           de-tot-juros     = de-tot-juros + de-vl-juros
           de-tot-devido    = de-tot-devido + de-vl-devido.

    if last-of(titulo.nome-abrev) then do:
       display "Total Cliente" @ titulo.nome-abrev
               de-tot-saldo    @ titulo.vl-saldo
               de-tot-juros    @ de-vl-juros
               de-tot-devido   @ de-vl-devido
               with frame f-detalhe.
       DOWN 2 with frame f-detalhe.
       assign de-tot-saldo  = 0
              de-tot-juros  = 0
              de-tot-devido = 0.
    end.
end.

display "Total Geral"    @ titulo.nome-abrev
        de-vl-tot-saldo  @ titulo.vl-saldo
        de-vl-tot-juros  @ de-vl-juros
        de-vl-tot-devido @ de-vl-devido
        with frame f-detalhe.
down with frame f-detalhe.

IF tt-param.impr-param THEN DO:
   PAGE.
   display tt-param.ep-codigo      
           tt-param.cod-estabel    
           tt-param.cod-port-ini   
           tt-param.cod-port-fin   
           tt-param.cod-esp-ini    
           tt-param.cod-esp-fin 
           tt-param.cod-emitente-ini
           tt-param.cod-emitente-fin
           tt-param.dt-vencto-ini  
           tt-param.dt-vencto-fin  
           tt-param.dt-base-juro
           tt-param.perc-juro
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

