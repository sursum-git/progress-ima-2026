/*   Programa: ESCR018.P  =>  ESCR0009RP.P
**   Sistema.: EMS da Datasul
**   M¢dulo..: Contas a Receber
**   Autor...: Gilvando Souza Araujo
**   Data....: 20/11/2006
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESCR0018RP 2.04.00.000}

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
       FIELD dt-liq-ini       LIKE titulo.dt-liq
       FIELD dt-liq-fin       LIKE titulo.dt-liq
       FIELD dt-corte         LIKE titulo.dt-ult-pagto
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
def var de-saldo   as dec format ">>>,>>>,>>9.99".
def var de-vl-bxa  as dec format ">>>,>>>,>>9.99".
def var de-tot-pag as dec format ">>>,>>>,>>9.99".
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
    tt-param.dt-liq-ini       label "Liquida‡Æo....." AT 1
    "a"  AT 30
    tt-param.dt-liq-fin       NO-LABELS
    tt-param.dt-corte         LABEL "Data Corte....." AT 1
    tt-param.desc-tipo-rel    LABEL "Tipo Relatorio." AT 1 SKIP(1)
    with no-box side-labels width 132 stream-io frame f-param.

form header
   "Emp Estab Esp Documento Cliente-Nome Abrev   Numero Banco "         at  1
   "Emissao    Liquida‡Æo          Valor Pago"                          at 67
   "------------------------------------------------------------------" at  1
   "-----------------------------------------"                          at 67
   with frame f-cab-pagto page-top width 132 STREAM-IO no-box down.

form
    titulo.ep-codigo      at  1
    titulo.cod-estabel    at  6
    titulo.cod-esp        at 11
    titulo.nr-docto       at 15 FORMAT "999999"
    titulo.parcela
    titulo.cod-emitente   at 25 FORMAT "9999999"
    titulo.nome-abrev
    titulo.titulo-banco   at 46
    titulo.dt-emissao     at 66
    titulo.dt-liq         at 78
    de-vl-bxa             at 94
    with no-box 55 down width 132 STREAM-IO no-labels frame f-detalhe.

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
{utp/ut-liter.i T¡tulos_Pagos_por_Portador/Data_Liquida‡Æo * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
VIEW FRAME f-cab-pagto.
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
                  and titulo.dt-liq       >= tt-param.dt-liq-ini
                  and titulo.dt-liq       <= tt-param.dt-liq-fin
                no-lock
                break by titulo.cod-port
                      by titulo.dt-ult-pagto:
    
    run pi-acompanhar in h-acomp (input "Especie: " + titulo.cod-esp + " Docto: " + titulo.nr-docto).

    /* Filtra movimentos na data de corte */
    assign de-saldo  = titulo.vl-saldo
           de-vl-bxa = 0.
    for each mov-tit where mov-tit.ep-codigo =  titulo.ep-codigo
                       and mov-tit.cod-estab =  titulo.cod-estab
                       and mov-tit.cod-esp   =  titulo.cod-esp
                       AND mov-tit.serie     =  titulo.serie
                       and mov-tit.nr-docto  =  titulo.nr-docto
                       and mov-tit.parcela   =  titulo.parcela
                       and mov-tit.dt-trans  <= tt-param.dt-corte
                     no-lock:
        if  mov-tit.transacao = 14 then /* IMP */
            assign de-saldo = de-saldo - mov-tit.vl-original.
        else
            if  mov-tit.transacao = 2 /* BAX */
            or  mov-tit.transacao = 3 then /* DEV */
                assign de-saldo  = de-saldo + mov-tit.vl-baixa
                       de-vl-bxa = de-vl-bxa + mov-tit.vl-baixa.
            else
                if  mov-tit.transacao = 13 /* AVA */
                and mov-tit.lancamento = 2 then
                    assign de-saldo  = de-saldo - mov-tit.vl-baixa
                           de-vl-bxa = de-vl-bxa - mov-tit.vl-baixa.
                else
                    if  mov-tit.transacao = 13 /* AVA */
                    and mov-tit.lancamento = 1 then
                        assign de-saldo = de-saldo + mov-tit.vl-baixa
                               de-vl-bxa = de-vl-bxa + mov-tit.vl-baixa.
    end.

    if first-of(titulo.cod-port) then
    assign l-prim-por = yes.

    if de-saldo = 0 then do:
       if l-prim-por then do:
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
       assign de-tot-ger = de-tot-ger + de-vl-bxa
              de-tot-por = de-tot-por + de-vl-bxa
              de-tot-pag = de-tot-pag + de-vl-bxa.
       IF tt-param.tipo-rel = 2 THEN DO:
          display titulo.ep-codigo
                  titulo.cod-est
                  titulo.cod-esp
                  titulo.nr-docto
                  titulo.parcela
                  titulo.cod-emitente
                  titulo.nome-abrev
                  titulo.titulo-banco
                  titulo.dt-emissao
                  titulo.dt-liq
                  de-vl-bxa
                  with frame f-detalhe.
          down with frame f-detalhe.
       END.
    end.

    if last-of(titulo.dt-ult-pag) and de-tot-pag <> 0 then do.
       put "Total do dia"    at 65
           titulo.dt-liq     AT 78
           de-tot-pag        at 94 skip(1).

       assign de-tot-pag = 0.
    end.

    if last-of(titulo.cod-port) and de-tot-por <> 0 then do.
       put "Total do Portador:"  at  1
           de-tot-por            at 94 skip(1).
       assign de-tot-por = 0.
    end.
end.

put "Total da Empresa:" at  1
    de-tot-ger          at 94.

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
           tt-param.dt-liq-ini  
           tt-param.dt-liq-fin  
           tt-param.dt-corte 
           tt-param.desc-tipo-rel
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

