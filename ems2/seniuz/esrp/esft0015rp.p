/* Programa: ESFT012.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Faturamento
** Objetivo: Listar o relatorio de Comissao sobre Vendas nas Lojas.
** Autor...: Gilvando de Souza Araujo - Novembro/95
** Obs.....: Especifico da CIA.RENASCENCA INDUSTRIAL
**
** Conversao para EMS 2.04:
**   Programa: ESFT012.P  =>  ESFT0015RP.P
**   Autor...: Gilvando Souza Araujo
**   Data....: 20/11/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESP ESFT0015RP 2.04.00.000}

define temp-table tt-param no-undo
       field destino          as integer
       field arquivo          as char format "x(35)"
       field usuario          as char format "x(12)"
       field data-exec        as date
       field hora-exec        as integer
       field classifica       as integer
       FIELD estab-ini        like nota-fiscal.cod-estabel              
       FIELD estab-fin        like nota-fiscal.cod-estabel   
       FIELD repres-ini       LIKE nota-fiscal.cod-rep 
       FIELD repres-fin       LIKE nota-fiscal.cod-rep
       FIELD dt-emis-ini      LIKE nota-fiscal.dt-emis-nota
       FIELD dt-emis-fin      LIKE nota-fiscal.dt-emis-nota
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

def var de-com-calc as dec format ">,>>>,>>9.99".
def var de-com-rep  as dec format ">,>>>,>>9.99".
def var de-com-tot  as dec format ">,>>>,>>9.99".
def var de-vnd-rep  as dec format ">>,>>>,>>9.99".
def var de-vnd-tot  as dec format ">>,>>>,>>9.99".
DEF VAR l-prim-vez  AS LOG.

form
    tt-param.estab-ini    LABEL "Estabelecimento de" AT  1
    "a"                                              AT 32
    tt-param.estab-fin    NO-LABELS
    tt-param.repres-ini   LABEL "Representante de.." AT  1
    "a"                                              AT 32
    tt-param.repres-fin   NO-LABELS
    tt-param.dt-emis-ini  LABEL "Data EmissÆo de..." AT  1
    "a"                                              AT 32
    tt-param.dt-emis-fin  NO-LABELS
    with no-box side-labels width 132 STREAM-IO frame f-parlis.

form
    nota-fiscal.no-ab-reppri     label "Vendedor"
    nota-fiscal.cod-estabel      label "Est"
    nota-fiscal.dt-emis-nota     label "Emissao"
    nota-fiscal.nr-nota-fis      label "Num.NF"
    nota-fiscal.serie            label "Ser"
    nota-fiscal.nome-ab-cli      label "Cliente"
    nota-fiscal.vl-tot-nota      label "Valor da Nota"
    repres.comis-direta          label "% Com"
    de-com-calc                  label "Valor Comissao"
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

{utp/ut-liter.i FATURAMENTO * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i ComissÆo_sobre_Vendas_das_Lojas * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

for each nota-fiscal WHERE nota-fiscal.cod-estabel  >= tt-param.estab-ini   
                       and nota-fiscal.cod-estabel  <= tt-param.estab-fin   
                       and nota-fiscal.cod-rep      >= tt-param.repres-ini  
                       and nota-fiscal.cod-rep      <= tt-param.repres-fin  
                       and nota-fiscal.dt-emis-nota >= tt-param.dt-emis-ini 
                       and nota-fiscal.dt-emis-nota <= tt-param.dt-emis-fin
                       AND nota-fiscal.dt-cancela   =  ? 
                     no-lock
                     break by nota-fiscal.no-ab-reppri
                       by nota-fiscal.dt-emis-nota
                       by nota-fiscal.serie
                       by nota-fiscal.nr-nota-fis:

    run pi-acompanhar in h-acomp (input "Nota Fiscal: " + nota-fiscal.nr-nota-fis).

            if first-of(nota-fiscal.no-ab-reppri) then
               assign l-prim-vez = yes.

            find repres where repres.nome-abrev = nota-fiscal.no-ab-reppri
                              no-lock no-error.
            assign de-com-calc = nota-fiscal.vl-tot-nota
                               * repres.comis-direta * 0.01.
            display nota-fiscal.no-ab-reppri  when l-prim-vez
                    nota-fiscal.cod-estabel
                    nota-fiscal.dt-emis-nota
                    nota-fiscal.nr-nota-fis
                    nota-fiscal.serie
                    nota-fiscal.nome-ab-cli
                    nota-fiscal.vl-tot-nota
                    repres.comis-direta
                    de-com-calc
                    with frame f-detalhe.
            down with frame f-detalhe.
            assign de-com-rep = de-com-rep + de-com-calc
                   de-com-tot = de-com-tot + de-com-calc
                   de-vnd-rep = de-vnd-rep + nota-fiscal.vl-tot-nota
                   de-vnd-tot = de-vnd-tot + nota-fiscal.vl-tot-nota
                   l-prim-vez = no.

            if last-of(nota-fiscal.no-ab-reppri) then do:
               display "Total"    @ nota-fiscal.no-ab-reppri
                       de-vnd-rep @ nota-fiscal.vl-tot-nota
                       de-com-rep @ de-com-calc
                       with frame f-detalhe.
               down with frame f-detalhe.
               put skip(1).
               assign de-com-rep = 0
                      de-vnd-rep = 0.
            end.
end. 

IF tt-param.impr-param THEN DO:
   PAGE.
   PUT "*****----- PAR¶METROS ------*****"
       SKIP(1).

   DISPLAY tt-param.estab-ini       
           tt-param.estab-fin                                             
           tt-param.repres-ini      
           tt-param.repres-fin    
           tt-param.dt-emis-ini    
           tt-param.dt-emis-fin       
           WITH FRAME f-parlis.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.



