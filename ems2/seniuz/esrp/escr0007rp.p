/* Programa: ESCR009.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Contas a Receber
** Objetivo: Listar Titulos a Receber por Portador/Vencimento
** Autor...: Gilvando de Souza Araujo - Julho/95
** Obs.....: Especifico da CIA.RENASCENCA INDUSTRIAL
**
** Conversao para EMS 2.04:
**   Programa: ESCR009.P  =>  ESCR0007RP.P
**   Autor...: Gilvando Souza Araujo
**   Data....: 11/02/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESCR0007RP 2.04.00.000}

define temp-table tt-param    no-undo
       field destino          as integer
       field arquivo          as char format "x(35)"
       field usuario          as char format "x(12)"
       field data-exec        as date
       field hora-exec        as integer
       FIELD ep-codigo        LIKE titulo.ep-codigo
       FIELD cod-est-ini      LIKE titulo.cod-estabel
       FIELD cod-est-fin      LIKE titulo.cod-estabel
       FIELD cod-port-ini     LIKE titulo.cod-port 
       FIELD cod-port-fin     LIKE titulo.cod-port
       FIELD cod-esp-ini      LIKE titulo.cod-esp
       FIELD cod-esp-fin      LIKE titulo.cod-esp
       FIELD cod-emitente-ini LIKE titulo.cod-emitente
       FIELD cod-emitente-fin LIKE titulo.cod-emitente
       FIELD dt-vencto-ini    LIKE titulo.dt-vencim
       FIELD dt-vencto-fin    LIKE titulo.dt-vencim
       FIELD dt-corte         LIKE titulo.dt-vencim
       FIELD tipo-rel         AS INT
       FIELD desc-tipo-rel    AS CHAR FORMAT "x(10)"
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

def var l-prim-vez as log.
def var l-prim-por as log.
def var de-saldo   as dec format ">>>,>>>,>>9.99" LABEL "Saldo".
def var de-tot-ven as dec format ">>>,>>>,>>9.99".
def var de-tot-por as dec format ">>>,>>>,>>9.99".
def var de-tot-ger as dec format ">>>,>>>,>>9.99".

form
    "*------------ Parƒmetros/Sele‡Æo -------------*" SKIP
    tt-param.ep-codigo        LABEL "Empresa........" AT 1
    tt-param.cod-est-ini      label "Estabelecimento" AT 1
    "a"  AT 30
    tt-param.cod-est-fin      NO-LABELS
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
    tt-param.dt-corte         LABEL "Data Corte....." AT 1
    tt-param.desc-tipo-rel    LABEL "Tipo Relatorio." AT 1 SKIP(1)
    with no-box side-labels width 132 stream-io frame f-param.

form
    titulo.cod-esp        
    titulo.nr-docto       
    titulo.parcela
    titulo.cod-emitente   
    titulo.nome-abrev     
    titulo.titulo-banco   
    titulo.dt-emissao     
    titulo.dt-vencimen    
    de-saldo              
    with no-box 55 down width 134 STREAM-IO frame f-detalhe.

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
{utp/ut-liter.i T¡tulos_a_Receber_por_Portador/Vencimento * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
VIEW frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

for each titulo where titulo.ep-codigo    =  tt-param.ep-codigo
                  and titulo.cod-estabel  >= tt-param.cod-est-ini
                  AND titulo.cod-estabel  <= tt-param.cod-est-fin
                  and titulo.cod-esp      >= tt-param.cod-esp-ini
                  and titulo.cod-esp      <= tt-param.cod-esp-fin
                  and titulo.cod-port     >= tt-param.cod-port-ini
                  and titulo.cod-port     <= tt-param.cod-port-fin
                  and titulo.cod-emitente >= tt-param.cod-emitente-ini
                  and titulo.cod-emitente <= tt-param.cod-emitente-fin
                  and titulo.dt-vencimen  >= tt-param.dt-vencto-ini
                  and titulo.dt-vencimen  <= tt-param.dt-vencto-fin
                no-lock
                break by titulo.cod-port
                      by titulo.dt-vencimen:
    
    run pi-acompanhar in h-acomp (input "Especie: " + titulo.cod-esp + " Docto: " + titulo.nr-docto).

    {esinc/escr999.i "tt-param.dt-corte" "de-saldo"}

    if first-of(titulo.cod-port) then
       assign l-prim-por = yes.

    if de-saldo > 0 then do:
       if l-prim-por then do.
          find portador where portador.ep-codigo    = titulo.ep-codigo
                          and portador.cod-portador = titulo.cod-port
                          and portador.modalidade   = titulo.modalidade
                              no-lock no-error.
          put "Portador: "
              titulo.cod-port
              " - "
              portador.nome skip(1).
          assign l-prim-por = no.
       end.
       assign de-tot-ger = de-tot-ger + de-saldo
              de-tot-por = de-tot-por + de-saldo
              de-tot-ven = de-tot-ven + de-saldo.

       if  tt-param.tipo-rel = 2
       and de-saldo > 0 then do:
           DISPLAY titulo.cod-esp
                   titulo.nr-docto
                   titulo.parcela
                   titulo.cod-emitente
                   titulo.nome-abrev
                   titulo.titulo-banco
                   titulo.dt-emissao
                   titulo.dt-vencimen
                   de-saldo
                   with frame f-detalhe.
           down with frame f-detalhe.
       END.
    end.

    if last-of(titulo.dt-vencimen) and de-tot-ven <> 0 then do:
       IF tt-param.tipo-rel = 2 THEN
          put "Total do vencimento:"  at 66
              de-tot-ven              at 93 skip(1).
       else
          put "Total Vencto - "                       AT 66
              STRING(titulo.dt-vencimen,"99/99/99") 
              de-tot-ven                              at 93 skip.
       assign de-tot-ven = 0.
    end.
    if last-of(titulo.cod-port) and de-tot-por <> 0 then do.
       put "Total do Portador:"  at  1
           de-tot-por            at 93 skip(1).
       assign de-tot-por = 0.
    end.
end.
put "Total da Empresa:" at  1
    de-tot-ger          at 93.

IF tt-param.impr-param THEN DO:
   PAGE.
   display tt-param.ep-codigo      
           tt-param.cod-est-ini
           tt-param.cod-est-fin
           tt-param.cod-port-ini   
           tt-param.cod-port-fin   
           tt-param.cod-esp-ini    
           tt-param.cod-esp-fin 
           tt-param.cod-emitente-ini
           tt-param.cod-emitente-fin
           tt-param.dt-vencto-ini  
           tt-param.dt-vencto-fin  
           tt-param.dt-corte 
           tt-param.desc-tipo-rel
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

