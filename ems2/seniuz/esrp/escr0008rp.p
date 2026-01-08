/* Programa: ESCR014.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Contas a Receber
** Objetivo: Listar Titulos a Receber por Cliente
** Autor...: Gilvando de Souza Araujo - Agosto/96
** Obs.....: Especifico da CIA.RENASCENCA INDUSTRIAL
**
** Conversao para EMS 2.04:
**   Programa: ESCR014.P  =>  ESCR0008RP.P
**   Autor...: Gilvando Souza Araujo
**   Data....: 11/02/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESCR0008RP 2.04.00.000}

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
       FIELD dt-emissao-ini   LIKE titulo.dt-emissao
       FIELD dt-emissao-fin   LIKE titulo.dt-emissao
       FIELD dt-vencto-ini    LIKE titulo.dt-vencim
       FIELD dt-vencto-fin    LIKE titulo.dt-vencim
       FIELD cod-regiao-ini   AS INT
       FIELD cod-regiao-fin   AS INT
       FIELD dt-corte         LIKE titulo.dt-vencim
       FIELD all-types        AS LOG FORMAT "Sim/NÆo"
       FIELD tp-pedido1       AS CHAR FORMAT "x(2)"
       FIELD tp-pedido2       AS CHAR FORMAT "x(2)"
       FIELD tp-pedido3       AS CHAR FORMAT "x(2)"
       FIELD tp-pedido4       AS CHAR FORMAT "x(2)"
       FIELD tp-pedido5       AS CHAR FORMAT "x(2)"
       FIELD tp-normal        AS LOG FORMAT "Sim/NÆo"
       FIELD repres-excl      LIKE titulo.cod-rep
       FIELD menor-vlr        AS DEC
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
def var l-prim-cli as log.
def var i-regiao   as int.
def var de-saldo   as dec format ">>>,>>>,>>9.99".
def var de-tot-ven as dec format ">>>,>>>,>>9.99".
def var de-tot-cli as dec format ">>>,>>>,>>9.99".
def var de-tot-ger as dec format ">>>,>>>,>>9.99".
def var c-nome-cont like cont-emit.nome.
def var c-fone-cont like cont-emit.telefone.
def var i-atraso as int.

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
    tt-param.dt-emissao-ini   label "EmissÆo........" AT 1
    "a"  AT 30
    tt-param.dt-emissao-fin   NO-LABELS
    tt-param.dt-vencto-ini    label "Vencimento....." AT 1
    "a"  AT 30
    tt-param.dt-vencto-fin    NO-LABELS
    tt-param.cod-regiao-ini   label "RegiÆo........." AT 1
    "a"  AT 30
    tt-param.cod-regiao-fin   NO-LABELS
    tt-param.dt-corte         LABEL "Data Corte....." AT 1
    tt-param.all-types        LABEL "Todos Tipos...." AT 1
    tt-param.tp-pedido1       NO-LABELS AT 23
    tt-param.tp-pedido2       NO-LABELS AT 26
    tt-param.tp-pedido3       NO-LABELS AT 29
    tt-param.tp-pedido4       NO-LABELS AT 32
    tt-param.tp-pedido5       NO-LABELS AT 35 
    tt-param.tp-normal        LABEL "Tipo Normal...." AT 1
    tt-param.repres           LABEL "Repres.Descons." AT 1
    tt-param.menor-vlr        LABEL "Menor Saldo...." AT 1
    with no-box side-labels width 132 stream-io frame f-param.

form
    titulo.ep-codigo     label "Emp"
    titulo.cod-estabel   label "Est"
    titulo.cod-esp       label "Esp"
    titulo.nr-docto      label "Docto."
    titulo.parcela       label "Pa"
    titulo.cod-port      label "Port"
    titulo.dt-emissao    label "Emissao"
    titulo.dt-vencimen   label "Vencimto"
    i-atraso             label "Atraso"  format ">>>9"
    de-saldo             label "Saldo"
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

{utp/ut-liter.i FINANCEIRO * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i T¡tulos_a_Receber_por_Cliente * r}
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
                  and titulo.dt-emissao   >= tt-param.dt-emissao-ini   
                  and titulo.dt-emissao   <= tt-param.dt-emissao-fin   
                  and titulo.dt-vencimen  >= tt-param.dt-vencto-ini
                  and titulo.dt-vencimen  <= tt-param.dt-vencto-fin
                  no-lock
                break by titulo.nome-abrev
                      by titulo.dt-vencimen:

    run pi-acompanhar in h-acomp (input "Especie: " + titulo.cod-esp + " Docto: " + titulo.nr-docto).
    
    if  tt-param.repres-excl <> 0
    and tt-param.repres-excl = titulo.cod-rep then next.
    
    find emitente where emitente.cod-emitente = titulo.cod-emitente
                        no-lock no-error.
    if lookup(emitente.estado,"MG,RJ,ES,MT,MS,GO,DF,TO,RR,RO,AC") <> 0
    or lookup(emitente.estado,"AM,PA,AP,BA,AL,CE,SE,MA,PI,PB,RN") <> 0
    or lookup(emitente.estado,"PE,FN") <> 0 then
       assign i-regiao = 1.
    else
    if lookup(emitente.estado,"SP,PR,SC,RS") <> 0 then   
       assign i-regiao = 2.
    else
       assign i-regiao = 3.

    if i-regiao < tt-param.cod-regiao-ini
    or i-regiao > tt-param.cod-regiao-fin then next.

    find ped-venda 
        where ped-venda.nome-abrev = titulo.nome-abrev
          and ped-venda.nr-pedcli  = titulo.nr-pedcli
        no-lock no-error.
    
    IF tt-param.all-types = NO AND NOT AVAIL ped-venda THEN NEXT.

    IF AVAIL ped-venda THEN
       IF tt-param.all-types = NO THEN DO:
          IF ped-venda.tp-pedido <> tt-param.tp-pedido1 AND
             ped-venda.tp-pedido <> tt-param.tp-pedido2 AND
             ped-venda.tp-pedido <> tt-param.tp-pedido3 AND
             ped-venda.tp-pedido <> tt-param.tp-pedido4 AND
             ped-venda.tp-pedido <> tt-param.tp-pedido5 AND
             ped-venda.tp-pedido <> "" THEN NEXT.
          IF ped-venda.tp-pedido = "" AND tt-param.tp-normal = NO THEN NEXT.
       END.

    {esinc/escr999.i "tt-param.dt-corte" "de-saldo"}

    if first-of(titulo.nome-abrev) then
       assign l-prim-cli = yes.

    if de-saldo >= tt-param.menor-vlr then do:
       if l-prim-cli then do.
          find emitente where emitente.cod-emit = titulo.cod-emitente
                              no-lock no-error.
          find first cont-emit 
               where cont-emit.cod-emitente = emitente.cod-emitente
                     no-lock no-error.
          if avail cont-emit then
             assign c-nome-cont = cont-emit.nome
                    c-fone-cont = cont-emit.telefone.
          else
             assign c-nome-cont = "* Nao Cadastrado *"
                    c-fone-cont = "* Nao Cadastrado *".
          put "Cli: "
              titulo.nome-abrev
              " "
              titulo.cod-emitente
              " "
              emitente.nome-emit
              " - CGC: "
              emitente.cgc
              " - Tel: "
              c-fone-cont
              " "
              c-nome-cont format "x(18)" skip
              emitente.cidade        at 69
              " " emitente.estado
              skip(1).
          assign l-prim-cli = no.
       end.
       assign de-tot-ger = de-tot-ger + de-saldo
              de-tot-cli = de-tot-cli + de-saldo
              i-atraso   = today - titulo.dt-vencimen.
       display titulo.ep-codigo
               titulo.cod-est
               titulo.cod-esp
               titulo.nr-docto
               titulo.parcela
               titulo.cod-port
               titulo.dt-emissao
               titulo.dt-vencimen
               i-atraso             when i-atraso > 0
               de-saldo
               with frame f-detalhe.
       down with frame f-detalhe.
    end.

    if last-of(titulo.nome-abrev) and de-tot-cli <> 0 then do:
       put "Total do cliente:" at  1
           de-tot-cli          at 68
           skip(1).
       assign de-tot-cli = 0.
    end.
end.

put "Total da Empresa:" at  1
    de-tot-ger          at 58.

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
           tt-param.dt-emissao-ini
           tt-param.dt-emissao-fin
           tt-param.dt-vencto-ini  
           tt-param.dt-vencto-fin 
           tt-param.cod-regiao-ini
           tt-param.cod-regiao-fin
           tt-param.dt-corte  
           tt-param.all-types
           tt-param.tp-pedido1
           tt-param.tp-pedido2
           tt-param.tp-pedido3
           tt-param.tp-pedido4
           tt-param.tp-pedido5
           tt-param.tp-normal
           tt-param.repres-excl
           tt-param.menor-vlr
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

