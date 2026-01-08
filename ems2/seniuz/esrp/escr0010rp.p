/* Programa: ESCR018.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Contas a Receber
** Objetivo: Listar titulos pagos por Portador/Data de Vencimento
** Autor...: Gilvando de Souza Araujo - Setembro/1998
** Obs.....: Especifico da CIA.RENASCENCA INDUSTRIAL
**
** Conversao para EMS 2.04:
**   Programa: ESCR018.P  =>  ESCR0010RP.P
**   Autor...: Gilvando Souza Araujo
**   Data....: 07/02/2005
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESCR0010RP 2.04.00.000}

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
       FIELD dt-pagto-ini     LIKE titulo.dt-ult-pagto
       FIELD dt-pagto-fin     LIKE titulo.dt-ult-pagto
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
def var de-saldo    as dec format ">>>>>>,>>9.99".
def var de-vl-bxa   as dec format ">>>>>>,>>9.99".
def var de-vl-bxa1  as dec format ">>>>>>,>>9.99".
def var de-vl-bxa2  as dec format ">>>>>>,>>9.99".
def var de-vl-bxa3  as dec format ">>>>>>,>>9.99".
def var de-tot-pag1 as dec format ">>>>>>,>>9.99".
def var de-tot-pag2 as dec format ">>>>>>,>>9.99".
def var de-tot-pag3 as dec format ">>>>>>,>>9.99".
def var de-tot-ven1 as dec format ">>>>>>,>>9.99".
def var de-tot-ven2 as dec format ">>>>>>,>>9.99".
def var de-tot-ven3 as dec format ">>>>>>,>>9.99".
def var de-tot-por1 as dec format ">>>>>>,>>9.99".
def var de-tot-por2 as dec format ">>>>>>,>>9.99".
def var de-tot-por3 as dec format ">>>>>>,>>9.99".
def var de-tot-ger1 as dec format ">>>>>>,>>9.99".
def var de-tot-ger2 as dec format ">>>>>>,>>9.99".
def var de-tot-ger3 as dec format ">>>>>>,>>9.99".
def var i-atraso    as int format "->>9".
def var de-pmr      as dec format ">>9.99".
def var de-vlr-bru-dia as dec.
def var de-vlr-liq-dia as dec.
def var de-vlr-bru-por as dec.
def var de-vlr-liq-por as dec.
def var de-vlr-bru-ger as dec.
def var de-vlr-liq-ger as dec.
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
    tt-param.dt-pagto-ini     label "Pagamento......" AT 1
    "a"  AT 30
    tt-param.dt-pagto-fin     NO-LABELS
    tt-param.dt-corte         LABEL "Data Corte....." AT 1
    tt-param.desc-tipo-rel    LABEL "Tipo Relatorio." AT 1 SKIP(1)
    with no-box side-labels width 132 stream-io frame f-param.

form header
   "Est Esp Documento Cliente-Nome Abrev    Emissao  Ven"  at   1
   "cimento  Pagamento Pago no Venct Pago Atrasado Pago "  at  53
   " Antecip. Dias     PMR"  at 105
   "--- --- --------- ------------------- ---------  ---"  at   1
   "-------  --------- ------------- ------------- -----"  at  53
   "--------- ----  ------"  at 105
   with frame f-cab-pagto page-top width 132 STREAM-IO no-box down.

form
    titulo.cod-estabel    at   1
    titulo.cod-esp        at   5
    titulo.nr-docto       at   9 FORMAT "999999"
    titulo.parcela
    titulo.cod-emitente   at  19 FORMAT "9999999"
    titulo.nome-abrev
    titulo.dt-emissao     at  39
    titulo.dt-vencimen    at  50
    titulo.dt-ult-pag     at  62
    de-vl-bxa1            at  72
    de-vl-bxa2            at  87
    de-vl-bxa3            at 100
    i-atraso              at 115
    de-pmr                at 121
    with no-box 55 down width 132 STREAM-IO no-labels frame f-detalhe.
form
    titulo.dt-emissao     at  39
    titulo.dt-vencimen    at  50
    titulo.dt-ult-pag     at  62
    de-tot-ven1           at  72
    de-tot-ven2           at  87
    de-tot-ven3           at 100
    i-atraso              at 115
    with no-box 55 down width 132 STREAM-IO no-labels frame f-detalhe1.

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
{utp/ut-liter.i T¡tulos_Pagos_por_Portador/Data_Vencimento * r}
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
                  and titulo.dt-ult-pagto >= tt-param.dt-pagto-ini
                  and titulo.dt-ult-pagto <= tt-param.dt-pagto-fin
                no-lock
                break by titulo.cod-port
                      by titulo.dt-ult-pagto
                      BY titulo.dt-vencimen:
    
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
       if titulo.dt-ult-pag = titulo.dt-vencimen then
          assign de-vl-bxa1  = de-vl-bxa
                 de-vl-bxa2  = 0
                 de-vl-bxa3  = 0
                 de-tot-ger1 = de-tot-ger1 + de-vl-bxa
                 de-tot-por1 = de-tot-por1 + de-vl-bxa
                 de-tot-pag1 = de-tot-pag1 + de-vl-bxa
                 de-tot-ven1 = de-tot-ven1 + de-vl-bxa
                 i-atraso    = 0.
       else
       if titulo.dt-ult-pag > titulo.dt-vencimen then
          assign de-vl-bxa2  = de-vl-bxa
                 de-vl-bxa1  = 0
                 de-vl-bxa3  = 0
                 de-tot-ger2 = de-tot-ger2 + de-vl-bxa
                 de-tot-por2 = de-tot-por2 + de-vl-bxa
                 de-tot-pag2 = de-tot-pag2 + de-vl-bxa
                 de-tot-ven2 = de-tot-ven2 + de-vl-bxa
                 i-atraso    = titulo.dt-ult-pag -
                               titulo.dt-vencimen.
       else
          assign de-vl-bxa3  = de-vl-bxa
                 de-vl-bxa1  = 0
                 de-vl-bxa2  = 0
                 de-tot-ger3 = de-tot-ger3 + de-vl-bxa
                 de-tot-por3 = de-tot-por3 + de-vl-bxa
                 de-tot-pag3 = de-tot-pag3 + de-vl-bxa
                 de-tot-ven3 = de-tot-ven3 + de-vl-bxa
                 i-atraso    = titulo.dt-ult-pag -
                               titulo.dt-vencimen.
       if  i-atraso > 0
       and i-atraso < 100 then
           assign de-vlr-bru-dia = de-vlr-bru-dia +
                                   (de-vl-bxa * i-atraso)
                  de-vlr-bru-por = de-vlr-bru-por +
                                   (de-vl-bxa * i-atraso)
                  de-vlr-bru-ger = de-vlr-bru-ger +
                                   (de-vl-bxa * i-atraso)
                  de-vlr-liq-dia = de-vlr-liq-dia + de-vl-bxa
                  de-vlr-liq-por = de-vlr-liq-por + de-vl-bxa
                  de-vlr-liq-ger = de-vlr-liq-ger + de-vl-bxa.
 
       if tt-param.tipo-rel = 2 then do:
          display titulo.cod-est
                  titulo.cod-esp
                  titulo.nr-docto
                  titulo.parcela
                  titulo.cod-emitente
                  titulo.nome-abrev
                  titulo.dt-emissao
                  titulo.dt-vencimen
                  titulo.dt-ult-pag
                  de-vl-bxa1 when de-vl-bxa1 <> 0
                  de-vl-bxa2 when de-vl-bxa2 <> 0
                  de-vl-bxa3 when de-vl-bxa3 <> 0
                  i-atraso
                  with frame f-detalhe.
          down with frame f-detalhe.
       end.   
    end.

    if  last-of(titulo.dt-vencimen) 
    and (de-tot-ven1 <> 0 or
         de-tot-ven2 <> 0 or
         de-tot-ven3 <> 0) then do:
       if tt-param.tipo-rel = 2 then do:  
          put "Total da data vencimento:"  at  47
              de-tot-ven1                  at  72
              de-tot-ven2                  at  87
              de-tot-ven3                  at 100 skip(1).
          assign de-tot-ven1 = 0
                 de-tot-ven2 = 0
                 de-tot-ven3 = 0.
       end.          
       else
          display titulo.dt-emissao
                  titulo.dt-vencimen
                  titulo.dt-ult-pag
                  de-tot-ven1 when de-tot-ven1 <> 0
                  de-tot-ven2 when de-tot-ven2 <> 0
                  de-tot-ven3 when de-tot-ven3 <> 0
                  i-atraso
                  with frame f-detalhe1.
          down with frame f-detalhe1.
          assign de-tot-ven1 = 0
                 de-tot-ven2 = 0
                 de-tot-ven3 = 0.
    end.
    
    if  last-of(titulo.dt-ult-pag) 
    and (de-tot-pag1 <> 0 or
         de-tot-pag2 <> 0 or
         de-tot-pag3 <> 0) then do:
       
        if de-vlr-liq-dia <> 0 then
           assign de-pmr = de-vlr-bru-dia / de-vlr-liq-dia.
        else
           assign de-pmr = 0.
          
        put "Total da data pagamento:"  at  47
            de-tot-pag1                 at  72
            de-tot-pag2                 at  87
            de-tot-pag3                 at 100
            de-pmr                      at 121  skip(1).
        assign de-tot-pag1    = 0
               de-tot-pag2    = 0
               de-tot-pag3    = 0
               de-vlr-bru-dia = 0
               de-vlr-liq-dia = 0.
    end.
     
    if  last-of(titulo.cod-port) 
    and (de-tot-por1 <> 0 or
         de-tot-por2 <> 0 or
         de-tot-por3 <> 0) then do:
        
        if de-vlr-liq-por <> 0 then
           assign de-pmr = de-vlr-bru-por / de-vlr-liq-por.
        else
           assign de-pmr = 0.
         
       put "Total do Portador:"  at   1
           de-tot-por1           at  72
           de-tot-por2           at  87
           de-tot-por3           at 100
           de-pmr                at 121 skip(1).
       assign de-tot-por1    = 0
              de-tot-por2    = 0
              de-tot-por3    = 0
              de-vlr-bru-por = 0
              de-vlr-liq-por = 0.
    end.
end.

if de-vlr-liq-ger <> 0 then
   assign de-pmr = de-vlr-bru-ger / de-vlr-liq-ger.
else
   assign de-pmr = 0.
         
put "Total da Empresa:" at   1
    de-tot-ger1         at  72
    de-tot-ger2         at  87
    de-tot-ger3         at 100
    de-pmr              at 121.
assign de-tot-ger1    = 0
       de-tot-ger2    = 0
       de-tot-ger3    = 0
       de-tot-ven1    = 0
       de-tot-ven2    = 0
       de-tot-ven3    = 0
       de-vlr-bru-ger = 0
       de-vlr-liq-ger = 0.

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
           tt-param.dt-pagto-ini  
           tt-param.dt-pagto-fin  
           tt-param.dt-corte 
           tt-param.desc-tipo-rel
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

