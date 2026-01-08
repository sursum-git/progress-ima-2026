/* Programa: ESCR007.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Contas a Receber
** Objetivo: Listar notas de debito por representantes
** Autor...: Gilvando de Souza Araujo - Agosto/96
** Obs.....: Especifico da CIA.RENASCENCA INDUSTRIAL
**
** Conversao para EMS 2.04:
**   Programa: ESCR007.P  =>  ESCR0003RP.P
**   Autor...: Gilvando Souza Araujo
**   Data....: 12/02/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESCR0003RP 2.04.00.000}

define temp-table tt-param    no-undo
       field destino          as integer
       field arquivo          as char format "x(35)"
       field usuario          as char format "x(12)"
       field data-exec        as date
       field hora-exec        as integer
       FIELD ep-codigo        LIKE titulo.ep-codigo
       FIELD cod-estabel      LIKE titulo.cod-estabel
       FIELD cod-esp-ini      LIKE titulo.cod-esp
       FIELD cod-esp-fin      LIKE titulo.cod-esp
       FIELD cod-rep-ini      LIKE titulo.cod-rep
       FIELD cod-rep-fin      LIKE titulo.cod-rep
       FIELD dt-emissao-ini   LIKE titulo.dt-emissao
       FIELD dt-emissao-fin   LIKE titulo.dt-emissao
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

def buffer b-titulo for titulo.

def var l-passou1  as log.
def var l-passou2  as log.
def var l-primeiro as log.
def var de-tot-rep like titulo.vl-original.
def var de-tot-cli like titulo.vl-original.
def var de-total   like titulo.vl-original.
def var i-atraso1  as int format ">>9".
def var i-atraso2  as int format ">>9". 

form
    "*------------ Parƒmetros/Sele‡Æo -------------*" SKIP
    tt-param.ep-codigo        LABEL "Empresa........" AT 1
    tt-param.cod-estabel      label "Estabelecimento" AT 1
    tt-param.cod-esp-ini      LABEL "Especie........" AT 1
    "a"  AT 30                
    tt-param.cod-esp-fin      NO-LABELS
    tt-param.cod-rep-ini      LABEL "Cliente........" AT 1
    "a"  AT 30
    tt-param.cod-rep-fin      NO-LABELS
    tt-param.dt-emissao-ini   label "EmissÆo........" AT 1
    "a"  AT 30
    tt-param.dt-emissao-fin   NO-LABELS
    with no-box side-labels width 132 stream-io frame f-param.

form
    titulo.cod-emit      LABEL "Cliente"
    titulo.cod-esp       LABEL "Esp"
    titulo.nr-docto      LABEL "Docto."    FORMAT "x(10)"
    titulo.parcela       LABEL "Pa"
    titulo.dt-vencimen   LABEL "Vencto."
    titulo.dt-ult-pagto  LABEL "Ult.Pgto."
    i-atraso1            LABEL "Atraso"
    titulo.vl-original   LABEL "Valor"     format ">>>,>>9.99"
    b-titulo.cod-esp     LABEL "Esp"
    b-titulo.nr-docto    LABEL "Docto."    FORMAT "x(8)"
    b-titulo.parcela     LABEL "Pa"
    b-titulo.dt-emissao  LABEL "Emissao"
    b-titulo.dt-vencimen LABEL "Vencto."
    i-atraso2            LABEL "Atraso"
    b-titulo.vl-original LABEL "Valor"     format ">>>,>>9.99"
    with no-box NO-LABEL 55 down width 132 STREAM-IO frame f-detalhe.

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
{utp/ut-liter.i Notas_de_D‚bito_por_Representante * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
VIEW frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

for each titulo where titulo.ep-codigo  =  tt-param.ep-codigo
                  and titulo.cod-estab  =  tt-param.cod-estabel
                  and titulo.dt-ult-pag >= tt-param.dt-emissao-ini
                  and titulo.dt-ult-pag <= tt-param.dt-emissao-fin
                  and titulo.cod-esp    >= tt-param.cod-esp-ini
                  and titulo.cod-esp    <= tt-param.cod-esp-fin
                  and titulo.cod-rep    >= tt-param.cod-rep-ini
                  and titulo.cod-rep    <= tt-param.cod-rep-fin 
                no-lock
                break by titulo.cod-rep
                      by titulo.cod-emit:

    run pi-acompanhar in h-acomp (input "Docto: " + titulo.nr-docto).

    if first-of(titulo.cod-rep) then
       assign l-primeiro = yes.
    FIND FIRST esp-doc where esp-doc.tipo = 4 /* Nota de D‚bito */
                       no-lock no-error.
    find b-titulo where b-titulo.ep-codigo = titulo.ep-codigo
                    and b-titulo.cod-estab = titulo.cod-estabel
                    and b-titulo.cod-esp   = esp-doc.cod-esp
                    and b-titulo.nr-docto  = titulo.nr-docto
                    and b-titulo.parcela   = titulo.parcela
                        no-lock no-error.
    if avail b-titulo then do.
       if l-primeiro = yes then do:
          find repres where repres.cod-rep = titulo.cod-rep no-lock
               no-error.
          put "Representante: " titulo.cod-rep " - "
              repres.nome skip(1).
          assign l-primeiro = no.
       end.
       DISPLAY titulo.cod-emit
               titulo.cod-esp
               titulo.nr-docto
               titulo.parcela
               titulo.dt-vencimen
               titulo.dt-ult-pagto
               titulo.dt-ult-pagto - titulo.dt-vencimen @ i-atraso1
               titulo.vl-original
               b-titulo.cod-esp
               b-titulo.nr-docto
               b-titulo.parcela
               b-titulo.dt-emissao
               b-titulo.dt-vencimen
               today - titulo.dt-vencimen @ i-atraso2
               b-titulo.vl-original
               with frame f-detalhe.
       down with frame f-detalhe.
       assign l-passou1  = yes
              l-passou2  = yes
              de-tot-cli = de-tot-cli + b-titulo.vl-original
              de-tot-rep = de-tot-rep + b-titulo.vl-original
              de-total   = de-total + b-titulo.vl-original.
    end.
    if  last-of(titulo.cod-emit)
    and l-passou1 = yes then do.
        DISPLAY "  Total Cli:" @ b-titulo.dt-vencimen
                de-tot-cli     @ b-titulo.vl-original
                with frame f-detalhe.
        down with frame f-detalhe.
        put skip(1).
        assign de-tot-cli = 0
               l-passou1  = no.
    end.
    if  last-of(titulo.cod-rep)
    and l-passou2 = yes then do.
        DISPLAY "  Total Rep:" @ b-titulo.dt-vencimen
                de-tot-rep     @ b-titulo.vl-original
                with frame f-detalhe.
        down with frame f-detalhe.
        put skip(1).
        assign de-tot-rep = 0
               l-passou2  = no.
    end.
end.
DISPLAY "Total Geral:" @ b-titulo.dt-vencimen
        de-total       @ b-titulo.vl-original
        with frame f-detalhe.

IF tt-param.impr-param THEN DO:
   PAGE.
   display tt-param.ep-codigo      
           tt-param.cod-estabel    
           tt-param.cod-esp-ini    
           tt-param.cod-esp-fin 
           tt-param.cod-rep-ini
           tt-param.cod-rep-fin
           tt-param.dt-emissao-ini
           tt-param.dt-emissao-fin
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

