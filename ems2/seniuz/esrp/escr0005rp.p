/* Programa: ESCR001.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Contas a Receber
** Objetivo: Gerar o relatorio Resumo de T¡tulos por Portador/Vencimento
** Autor...: Sandro Wiest/Gilvando de Souza Araujo - Agosto/97
** Obs.....: Especifico da CIA.RENASCENCA INDUSTRIAL / SANTA ELISABETH
**
** Conversao para EMS 2.04:
**   Programa: ESCR001.P  =>  ESCR0005RP.P
**   Autor...: Gilvando Souza Araujo
**   Data....: 10/02/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESCR0005RP 2.04.00.000}

define temp-table tt-param no-undo
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
       FIELD modalidade-ini   LIKE titulo.modalidade
       FIELD modalidade-fin   LIKE titulo.modalidade
       FIELD dt-vencto-ini    LIKE titulo.dt-vencim
       FIELD dt-vencto-fin    LIKE titulo.dt-vencim
       FIELD dt-corte         LIKE titulo.dt-vencim
       FIELD cod-esp-ini      LIKE titulo.cod-esp
       FIELD cod-esp-fin      LIKE titulo.cod-esp
       FIELD categoria        LIKE emitente.categoria
       FIELD tipo-rel         AS INT
       FIELD desc-tipo-rel    AS CHAR FORMAT "x(10)"
       FIELD ignorar-nove     AS LOGICAL FORMAT "Sim/NÆo"
       FIELD impr-param       AS LOGICAL.

define temp-table tt-raw-digita
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

DEFINE TEMP-TABLE w-work
       field dt-vencto  like titulo.dt-vencimen
       field cliente    like emitente.cod-emitente
       field cod-port   like titulo.cod-port
       field modalidade like titulo.modalidade
       field valor      as dec format ">>>,>>>,>>9.99" label "Valor"
       INDEX ch-work dt-vencto
                     cliente
                     cod-port
                     modalidade.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* defini‡Æo de vari veis  */
def var h-acomp as handle no-undo.

def var nlinhas             as int.
def var de-total            as dec format ">>>,>>>,>>9.99" label "Total".
def var de-desconto         as dec format ">>>,>>>,>>9.99" label "Desconto".
def var de-simples          as dec format ">>>,>>>,>>9.99" label "Simples".
def var de-carteira         as dec format ">>>,>>>,>>9.99" label "Carteira".
def var de-outros           as dec format ">>>,>>>,>>9.99" label "Outros".
def var de-tot-dia          as dec format ">>>,>>>,>>9.99".
def var de-des-port         as dec format ">>>,>>>,>>9.99".
def var de-sim-port         as dec format ">>>,>>>,>>9.99".
def var de-car-port         as dec format ">>>,>>>,>>9.99".
def var de-out-port         as dec format ">>>,>>>,>>9.99".
def var de-saldo            like titulo.vl-saldo.
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
    tt-param.modalidade-ini label "Modalidade....." AT 1
    "a"  AT 30
    tt-param.modalidade-fin no-labels
    tt-param.dt-vencto-ini  label "Vencimento....." AT 1
    "a"  AT 30
    tt-param.dt-vencto-fin  NO-LABELS
    tt-param.dt-corte       LABEL "Data Corte....." AT 1
    tt-param.cod-esp-ini    LABEL "Especie........" AT 1
    "a"  AT 30
    tt-param.cod-esp-fin    NO-LABELS
    tt-param.categoria      LABEL "Categoria Excl." AT 1
    tt-param.desc-tipo-rel  LABEL "Tipo Relatorio." AT 1 
    tt-param.ignorar-nove   LABEL "Ignorar Nove..." AT 1 SKIP(1)
    with no-box side-labels width 132 stream-io frame f-param.

form
    w-work.dt-vencto     label "Vencimento"
    w-work.cod-port      label "Portador"
    portador.nome-abrev  label "Nome Abrev"
    de-desconto          label "Desconto"
    de-simples           label "Simples"
    de-carteira          label "Carteira"
    de-outros            label "Outros"
    de-total             label "Total"
    with no-box down width 132 STREAM-IO frame f-detalhe.

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
{utp/ut-liter.i Resumo_de_T¡tulos_por_Portador/Vencimento * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

for each w-work.
    delete w-work.
end.
        
for each titulo where titulo.ep-codigo   =  tt-param.ep-codigo
                  AND titulo.cod-estabel >= tt-param.cod-est-ini
                  AND titulo.cod-estabel <= tt-param.cod-est-fin
                  and titulo.cod-esp     >= tt-param.cod-esp-ini
                  and titulo.cod-esp     <= tt-param.cod-esp-fin
                NO-LOCK USE-INDEX codigo:
   
   IF titulo.dt-vencimen < tt-param.dt-vencto-ini  OR
      titulo.dt-vencimen > tt-param.dt-vencto-fin  OR
      titulo.cod-port    < tt-param.cod-port-ini   OR
      titulo.cod-port    > tt-param.cod-port-fin   OR
      titulo.modalidade  < tt-param.modalidade-ini OR
      titulo.modalidade  > tt-param.modalidade-fin or
      string(titulo.cod-port) BEGINS "9" AND tt-param.ignorar-nove THEN NEXT.

   run pi-acompanhar in h-acomp (input "Especie: " + titulo.cod-esp + " Docto: " + titulo.nr-docto).

   {esinc/escr999.i "tt-param.dt-corte" "de-saldo"}
   if de-saldo <= 0 then next.

   find first emitente where emitente.cod-emit = titulo.cod-emit
                       no-lock no-error.
   if emitente.categoria = tt-param.categoria then next.
   
   find first w-work where w-work.cod-port   = titulo.cod-port
                       and w-work.modalidade = titulo.modalidade
                       and w-work.dt-vencto  = titulo.dt-vencimen
                       and w-work.cliente    = emitente.cod-emit
                           no-error.
   if not avail w-work then do:
      create w-work.
      assign w-work.dt-vencto  = titulo.dt-vencimen
             w-work.cod-port   = titulo.cod-port
             w-work.modalidade = titulo.modalidade
             w-work.cliente    = emitente.cod-emit
             w-work.valor      = 0.
   end.
   assign w-work.valor = w-work.valor + de-saldo.
end.

for each w-work break by year(w-work.dt-vencto)
                      by month(w-work.dt-vencto)
                      by w-work.dt-vencto
                      by w-work.cod-port
                      by w-work.cliente:
    find portador where portador.ep-codigo  = tt-param.ep-codigo
                    and portador.cod-port   = w-work.cod-port
                    and portador.modalidade = w-work.modalidade
                  no-lock no-error.
   
    if w-work.modalidade = 1 then
       assign de-simples = w-work.valor.
    else
    if w-work.modalidade = 2 then
       assign de-desconto = w-work.valor.
    else
    if w-work.modalidade = 6 then
       assign de-carteira = w-work.valor.
    else
       assign de-outros = w-work.valor.
   
    if tt-param.tipo-rel = 2 then do: /* Detalhado */
       find first emitente where emitente.cod-emit = w-work.cliente
                           no-lock no-error.
       if avail emitente then do:                          
          display emitente.cod-emit   @ w-work.cod-port
                  emitente.nome-abrev @ portador.nome-abrev
                  de-desconto
                  de-simples
                  de-carteira
                  de-outros
                  de-desconto + de-simples + de-carteira + de-outros @
                  de-total
                  with frame f-detalhe.
          down with frame f-detalhe.        
       end.
   end.    
   assign de-des-port = de-des-port + de-desconto
          de-sim-port = de-sim-port + de-simples
          de-car-port = de-car-port + de-carteira
          de-out-port = de-out-port + de-outros
          de-desconto = 0
          de-simples  = 0
          de-carteira = 0
          de-outros   = 0.
   if  last-of(w-work.cod-port)
   and (de-des-port <> 0 or
        de-sim-port <> 0 or
        de-car-port <> 0 or
        de-out-port <> 0) then do:
        assign nlinhas = 1.
        if tt-param.tipo-rel = 2 THEN /* Detalhado */
           assign nlinhas = 2.
        display w-work.dt-vencto
                w-work.cod-port
                portador.nome-abrev when avail portador
                de-des-port @ de-desconto
                de-sim-port @ de-simples
                de-car-port @ de-carteira
                de-out-port @ de-outros
                de-des-port + de-sim-port + de-car-port + de-out-port @
                de-total
                with frame f-detalhe.
        down nlinhas with frame f-detalhe.
        
        assign de-tot-desconto     = de-tot-desconto + de-des-port
               de-tot-carteira     = de-tot-carteira + de-car-port
               de-tot-simples      = de-tot-simples + de-sim-port
               de-tot-outros       = de-tot-outros + de-out-port
               de-tot-total        = de-tot-total +
                                     de-des-port + 
                                     de-car-port +
                                     de-sim-port +
                                     de-out-port
               de-tot-mes-desconto = de-tot-mes-desconto + de-des-port
               de-tot-mes-carteira = de-tot-mes-carteira + de-car-port
               de-tot-mes-simples  = de-tot-mes-simples + de-sim-port
               de-tot-mes-outros   = de-tot-mes-outros + de-out-port
               de-tot-mes-total    = de-tot-mes-total +
                                     de-des-port + de-car-port +
                                     de-sim-port + de-out-port
               de-tot-ger-desconto = de-tot-ger-desconto + de-des-port
               de-tot-ger-carteira = de-tot-ger-carteira + de-car-port
               de-tot-ger-simples  = de-tot-ger-simples + de-sim-port
               de-tot-ger-outros   = de-tot-ger-outros + de-out-port
               de-tot-ger-total    = de-tot-ger-total +
                                     de-des-port + de-car-port +
                                     de-sim-port + de-out-port
               de-des-port         = 0
               de-sim-port         = 0
               de-car-port         = 0
               de-out-port         = 0.
   end.
   
   if  last-of(w-work.dt-vencto)
   and (de-tot-desconto <> 0 or
        de-tot-carteira <> 0 or
        de-tot-simples  <> 0 or
        de-tot-outros   <> 0) then do:
        display "Total do Dia"  @ portador.nome-abrev
                de-tot-desconto @ de-desconto
                de-tot-carteira @ de-carteira
                de-tot-simples  @ de-simples
                de-tot-outros   @ de-outros
                de-tot-total    @ de-total
                with frame f-detalhe.
        down 2 with frame f-detalhe.

        assign de-tot-desconto = 0
               de-tot-carteira = 0
               de-tot-simples  = 0
               de-tot-total    = 0
               de-tot-outros   = 0.
   end.

   if  last-of(month(w-work.dt-vencto))
   and (de-tot-mes-desconto <> 0 or
        de-tot-mes-carteira <> 0 or
        de-tot-mes-simples  <> 0 or
        de-tot-mes-outros   <> 0) then do:
        display "Total do Mes" @ portador.nome-abrev
                de-tot-mes-desconto @ de-desconto
                de-tot-mes-carteira @ de-carteira
                de-tot-mes-simples  @ de-simples
                de-tot-mes-outros   @ de-outros
                de-tot-mes-total    @ de-total
                with frame f-detalhe.
        down 2 with frame f-detalhe.
        assign de-tot-mes-desconto = 0
               de-tot-mes-carteira = 0
               de-tot-mes-simples  = 0
               de-tot-mes-total    = 0
               de-tot-mes-outros   = 0.
   end.
end.

display "Total Geral" @ portador.nome-abrev
        de-tot-ger-desconto @ de-desconto
        de-tot-ger-carteira @ de-carteira
        de-tot-ger-simples  @ de-simples
        de-tot-ger-outros   @ de-outros
        de-tot-ger-total    @ de-total
        with frame f-detalhe.
down with frame f-detalhe.

IF tt-param.impr-param THEN DO:
   PAGE.
   display tt-param.ep-codigo      
           tt-param.cod-est-ini
           tt-param.cod-est-fin
           tt-param.cod-port-ini   
           tt-param.cod-port-fin   
           tt-param.modalidade-ini 
           tt-param.modalidade-fin 
           tt-param.dt-vencto-ini  
           tt-param.dt-vencto-fin  
           tt-param.dt-corte       
           tt-param.cod-esp-ini    
           tt-param.cod-esp-fin    
           tt-param.categoria      
           tt-param.desc-tipo-rel
           tt-param.ignorar-nove
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

