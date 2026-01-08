/* Programa: ESCR002.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Contas a Receber
** Objetivo: Listar a Titulos a Receber por Portador
** Autor...: Gilvando de Souza Araujo - Agosto/96
** Obs.....: Especifico da CIA.RENASCENCA INDUSTRIAL.
**
** Conversao para EMS 2.04:
**   Programa: ESCR002.P  =>  ESCR0006RP.P
**   Autor...: Gilvando Souza Araujo
**   Data....: 10/02/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESCR0006RP 2.04.00.000}

define temp-table tt-param no-undo
       field destino       as integer
       field arquivo       as char format "x(35)"
       field usuario       as char format "x(12)"
       field data-exec     as date
       field hora-exec     as integer
       FIELD ep-codigo     LIKE titulo.ep-codigo
       FIELD cod-est-ini   LIKE titulo.cod-estabel
       FIELD cod-est-fin   LIKE titulo.cod-estabel
       FIELD cod-port-ini  LIKE titulo.cod-port 
       FIELD cod-port-fin  LIKE titulo.cod-port
       FIELD cod-esp-ini   LIKE titulo.cod-esp
       FIELD cod-esp-fin   LIKE titulo.cod-esp
       FIELD dt-vencto-ini LIKE titulo.dt-vencim
       FIELD dt-vencto-fin LIKE titulo.dt-vencim
       FIELD dt-corte      LIKE titulo.dt-vencim
       FIELD tipo-rel      AS INT
       FIELD desc-tipo-rel AS CHAR FORMAT "x(10)"
       FIELD impr-param    AS LOGICAL.

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

def var de-saldo            like titulo.vl-saldo.
def var de-total            as dec format ">>>,>>>,>>9.99".
def var de-desconto         as dec format ">>,>>>,>>9.99". 
def var de-simples          as dec format ">>,>>>,>>9.99". 
def var de-carteira         as dec format ">>,>>>,>>9.99". 
def var de-outros           as dec format ">>,>>>,>>9.99". 
def var de-tot-dia          as dec format ">>,>>>,>>9.99".
def var de-port-desconto    like de-desconto.
def var de-port-carteira    like de-carteira.
def var de-port-simples     like de-simples.
def var de-port-outros      like de-outros.
def var de-port-total       like de-total.
def var de-ger-desconto     like de-desconto.
def var de-ger-carteira     like de-carteira.
def var de-ger-simples      like de-simples.
def var de-ger-outros       like de-outros.
def var de-ger-total        like de-tot-dia.
def var de-tot-desconto     like de-desconto.
def var de-tot-mes-desconto like de-desconto.
def var de-tot-ger-desconto like de-desconto.
def var de-tot-simples      like de-simples.
def var de-tot-mes-simples  like de-simples.
def var de-tot-ger-simples  like de-simples.
def var de-tot-total        like de-total.
def var de-tot-mes-total    like de-total.
def var de-tot-ger-total    like de-total.
def var de-tot-carteira     like de-carteira.
def var de-tot-mes-carteira like de-carteira.
def var de-tot-ger-carteira like de-carteira.
def var de-tot-outros       like de-outros.
def var de-tot-mes-outros   like de-outros.
def var de-tot-ger-outros   like de-outros.

form
    "*------------ Parƒmetros/Sele‡Æo -------------*" SKIP
    tt-param.ep-codigo      LABEL "Empresa........" AT 1
    tt-param.cod-est-ini    label "Estabelecimento" AT 1
    "a"  AT 30
    tt-param.cod-est-fin    NO-LABELS
    tt-param.cod-port-ini   label "Portador......." AT 1
    "a"  AT 30
    tt-param.cod-port-fin   no-labels
    tt-param.cod-esp-ini    LABEL "Especie........" AT 1
    "a"  AT 30
    tt-param.cod-esp-fin    NO-LABELS
    tt-param.dt-vencto-ini  label "Vencimento....." AT 1
    "a"  AT 30
    tt-param.dt-vencto-fin  NO-LABELS
    tt-param.dt-corte       LABEL "Data Corte....." AT 1
    tt-param.desc-tipo-rel  LABEL "Tipo Relatorio." AT 1 SKIP(1)
    with no-box side-labels width 132 stream-io frame f-param.

form
    titulo.cod-port       LABEL "Port"
    portador.nome-abrev   LABEL "Nome Port"
    titulo.dt-vencimen    LABEL "Vencto"    
    titulo.cod-esp        LABEL "Esp"
    titulo.nr-docto       LABEL "Docto"     FORMAT "x(14)"
    titulo.parcela        LABEL "Pa"
    titulo.cod-emit       LABEL "Cliente"
    de-desconto           LABEL "Desconto"
    de-simples            LABEL "Simples"
    de-carteira           LABEL "Carteira"
    de-outros             LABEL "Outros"
    de-total              LABEL "Total"
    with no-box down NO-LABEL width 134 STREAM-IO frame f-detalhe.

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i}

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i ESTOQUE * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i T¡tulos_a_Receber_por_Portador * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

for each titulo where titulo.ep-codigo   =  tt-param.ep-codigo
                  and titulo.cod-est     >= tt-param.cod-est-ini
                  AND titulo.cod-est     <= tt-param.cod-est-fin
                  and titulo.cod-esp     >= tt-param.cod-esp-ini
                  and titulo.cod-esp     <= tt-param.cod-esp-fin
                  and titulo.cod-port    >= tt-param.cod-port-ini
                  and titulo.cod-port    <= tt-param.cod-port-fin
                  and titulo.dt-vencimen >= tt-param.dt-vencto-ini
                  and titulo.dt-vencimen <= tt-param.dt-vencto-fin
                no-lock
                break by titulo.cod-port
                      by year(titulo.dt-vencimen)
                      by month(titulo.dt-vencimen)
                      by day(titulo.dt-vencimen)
                      by titulo.nr-docto:

    run pi-acompanhar in h-acomp (input "Especie: " + titulo.cod-esp + " Docto: " + titulo.nr-docto).

    assign de-saldo = titulo.vl-saldo.

    for each mov-tit where mov-tit.ep-codigo = titulo.ep-codigo
                       and mov-tit.cod-estab = titulo.cod-estab
                       and mov-tit.cod-esp   = titulo.cod-esp
                       AND mov-tit.serie     = titulo.serie
                       and mov-tit.nr-docto  = titulo.nr-docto
                       and mov-tit.parcela   = titulo.parcela
                     NO-LOCK:

        IF mov-tit.dt-trans <= tt-param.dt-corte THEN NEXT.
        
        if  mov-tit.transacao = 14 then
            assign de-saldo = de-saldo - mov-tit.vl-original.
        if  mov-tit.transacao = 2 then
            assign de-saldo = de-saldo + mov-tit.vl-baixa.
        if  mov-tit.transacao = 3 then
            assign de-saldo = de-saldo + mov-tit.vl-baixa.
        if  mov-tit.transacao = 13
        and mov-tit.lancamento = 1 then /* Deb */
            assign de-saldo = de-saldo + mov-tit.vl-baixa.
        if  mov-tit.transacao = 13
        and mov-tit.lancamento = 2 THEN /* Cred */
            assign de-saldo = de-saldo - mov-tit.vl-baixa.
    end.

    find first emitente where emitente.cod-emit = titulo.cod-emit
                        no-lock no-error.

    if  titulo.modalidade = 1 then
        assign de-simples = de-saldo.
    else
    if  titulo.modalidade = 2 then
        assign de-desconto = de-saldo.
    else
    if  titulo.modalidade = 6 then
        assign de-carteira = de-saldo.
    else
        assign de-outros = de-saldo.
    assign de-tot-desconto  = de-tot-desconto + de-desconto
           de-tot-carteira  = de-tot-carteira + de-carteira
           de-tot-simples   = de-tot-simples + de-simples
           de-tot-outros    = de-tot-outros + de-outros
           de-tot-total     = de-tot-total +
                              de-desconto +
                              de-carteira +
                              de-simples +
                              de-outros
           de-port-desconto = de-port-desconto + de-desconto
           de-port-carteira = de-port-carteira + de-carteira
           de-port-simples  = de-port-simples + de-simples
           de-port-outros   = de-port-outros + de-outros
           de-port-total    = de-port-total +
                              de-desconto +
                              de-carteira +
                              de-simples +
                              de-outros
           de-ger-desconto  = de-ger-desconto + de-desconto
           de-ger-carteira  = de-ger-carteira + de-carteira
           de-ger-simples   = de-ger-simples + de-simples
           de-ger-outros    = de-ger-outros + de-outros
           de-ger-total     = de-ger-total +
                              de-desconto +
                              de-carteira +
                              de-simples +
                              de-outros.

    find portador where portador.ep-codigo  = titulo.ep-codigo
                    and portador.cod-port   = titulo.cod-port
                    and portador.modalidade = titulo.modalidade
                        no-lock no-error.
    if  tt-param.tipo-rel = 2
    and de-saldo > 0 then do:
       display titulo.dt-vencimen
               titulo.cod-port
               portador.nome-abrev when avail portador
               titulo.cod-esp
               titulo.nr-docto
               titulo.parcela
               titulo.cod-emit
               de-desconto
               de-carteira
               de-simples
               de-outros
               de-total
               with frame f-detalhe.
       down with frame f-detalhe.
    end.

    assign de-desconto = 0
           de-carteira = 0
           de-simples  = 0
           de-total    = 0
           de-outros   = 0.

    if last-of(titulo.cod-port) then do:
       if de-tot-total <> 0 then do:
          if tt-param.tipo-rel = 2 then /* Detalhado */
             display "Total do Mes"  @ portador.nome-abrev
                     de-tot-desconto @ de-desconto
                     de-tot-carteira @ de-carteira
                     de-tot-simples  @ de-simples
                     de-tot-outros   @ de-outros
                     de-tot-total    @ de-total
                     with frame f-detalhe.
          else
             display "Total Mes "    @ portador.nome-abrev
                     string(month(titulo.dt-vencimen),"99") + "/" +
                     string(year(titulo.dt-vencimen),"9999")
                                     @ titulo.dt-vencimen
                     titulo.cod-port
                     de-tot-desconto @ de-desconto
                     de-tot-carteira @ de-carteira
                     de-tot-simples  @ de-simples
                     de-tot-outros   @ de-outros
                     de-tot-total    @ de-total
                     with frame f-detalhe.
             down with frame f-detalhe.

          assign de-tot-desconto = 0
                 de-tot-carteira = 0
                 de-tot-simples  = 0
                 de-tot-total    = 0
                 de-tot-outros   = 0.
       end.
       if de-port-total <> 0 then do:
          display "Tot Portador"   @ portador.nome-abrev
                  de-port-desconto @ de-desconto
                  de-port-carteira @ de-carteira
                  de-port-simples  @ de-simples
                  de-port-outros   @ de-outros
                  de-port-total    @ de-total
                  with frame f-detalhe.
          down with frame f-detalhe.
          put skip(1).
          assign de-port-desconto = 0
                 de-port-carteira = 0
                 de-port-simples  = 0
                 de-port-total    = 0
                 de-port-outros   = 0.
       end.
    end.
    else
       if last-of(month(titulo.dt-vencimen)) then do:
          if de-tot-total <> 0 then do:
             if tt-param.tipo-rel = 2 then /* Detalhado */
                display "Total do Mes"  @ portador.nome-abrev
                        de-tot-desconto @ de-desconto
                        de-tot-carteira @ de-carteira
                        de-tot-simples  @ de-simples
                        de-tot-outros   @ de-outros
                        de-tot-total    @ de-total
                        with frame f-detalhe.
             else
                display "Total Mes "    @ portador.nome-abrev
                        string(month(titulo.dt-vencimen),"99") + "/" +
                        string(year(titulo.dt-vencimen),"9999")
                                        @ titulo.dt-vencimen
                        titulo.cod-port
                        de-tot-desconto @ de-desconto
                        de-tot-carteira @ de-carteira
                        de-tot-simples  @ de-simples
                        de-tot-outros   @ de-outros
                        de-tot-total    @ de-total
                        with frame f-detalhe.
                down with frame f-detalhe.

             assign de-tot-desconto = 0
                    de-tot-carteira = 0
                    de-tot-simples  = 0
                    de-tot-total    = 0
                    de-tot-outros   = 0.
          end.
       end.
end.

display "Total Geral"   @ portador.nome-abrev
        de-ger-desconto @ de-desconto
        de-ger-carteira @ de-carteira
        de-ger-simples  @ de-simples
        de-ger-outros   @ de-outros
        de-ger-total    @ de-total
        with frame f-detalhe.
down with frame f-detalhe.

IF tt-param.impr-param THEN DO:
   PAGE.
   display tt-param.ep-codigo      
           tt-param.cod-est-ini
           tt-param.cod-est-fin
           tt-param.cod-port-ini   
           tt-param.cod-port-fin   
           tt-param.cod-esp-ini    
           tt-param.cod-esp-fin    
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

