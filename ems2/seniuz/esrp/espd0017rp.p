/* Programa: ESPD049.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Faturamento
** Objetivo: Listar o relat¢rio de Pedidos de Vendas … Vista p/Solicita‡Æo de Dep¢sito
** Autor...: Fabio Coelho Lanza - Dezembro/98
** Obs.....: Especifico da CIA.RENASCENCA INDUSTRIAL
**
** Conversao para EMS 2.04:
**   Programa: ESPD049.P  =>  ESPD0017RP.P
**   Autor...: Gilvando Souza Araujo
**   Data....: 23/01/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESPD0017RP 2.04.00.000}

define temp-table tt-param no-undo
       field destino          as integer
       field arquivo          as char format "x(35)"
       field usuario          as char format "x(12)"
       field data-exec        as date
       field hora-exec        as integer
       field classifica       as integer
       FIELD repres-ini       LIKE nota-fiscal.no-ab-reppri 
       FIELD repres-fin       LIKE nota-fiscal.no-ab-reppri
       FIELD cliente-ini      LIKE nota-fiscal.nome-ab-cli
       FIELD cliente-fin      LIKE nota-fiscal.nome-ab-cli
       FIELD dt-entrega-ini   LIKE ped-venda.dt-entrega
       FIELD dt-entrega-fin   LIKE ped-venda.dt-entrega
       FIELD dt-implant-ini   LIKE ped-venda.dt-implant
       FIELD dt-implant-fin   LIKE ped-venda.dt-implant
       FIELD impr-param       AS   LOGICAL.

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

def var de-qtd          as dec.
def var de-qtd-tot      as dec format ">>>,>>>,>>9.99". 
def var de-qtd-ger      as dec. 
def var de-valor        as dec. 
def var de-vlr-solic    as dec format ">>>,>>>,>>9.99".
def var de-nao-solic    as dec format ">>>,>>>,>>9.99".
def var de-ger-qtd      as dec format ">>>,>>>,>>9.99".
def var de-ger-solic    as dec format ">>>,>>>,>>9.99".
def var de-ger-valor    as dec.
def var de-ger-nao-sol  as dec format ">>>,>>>,>>9.99". 
def var de-reserva      as dec format ">>9.99".

form
    "*------------ Parƒmetros/Sele‡Æo -------------*" SKIP
    tt-param.repres-ini       LABEL "Representante.." 
    "a" AT 30
    tt-param.repres-fin       NO-LABELS SKIP
    tt-param.cliente-ini      LABEL "Cliente........"
    "a" AT 30
    tt-param.cliente-fin      NO-LABELS SKIP
    tt-param.dt-entrega-ini   LABEL "Data de Entrega"              
    "a" AT 30
    tt-param.dt-entrega-fin   NO-LABELS SKIP 
    tt-param.dt-implant-ini   LABEL "Data de Implant"           
    "a" AT 30
    tt-param.dt-implant-fin   NO-LABELS             
    with no-box side-labels width 132 STREAM-IO frame f-param.

form
    ped-venda.no-ab-reppri  label "REPRESENTANTE"
    ped-venda.nome-abrev    label "CLIENTE"
    ped-venda.dt-entrega    label "ENTREGA"
    ped-venda.dt-implant    label "IMPLANTACAO"
    ped-venda.nr-pedcli     label "PEDIDO"
    de-qtd-tot              label "QUANTIDADE"
    de-reserva              label "% RES."
    de-valor                label "VALOR TOTAL"
    de-vlr-solic            label "VLR. SOLICITADO"
    de-nao-solic            label "VLR. NAO SOLIC."
    with no-box 55 down width 132 with STREAM-IO frame f-detalhe NO-LABEL.

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i}

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i PEDIDOS * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Pedidos_de_Vendas_…_Vista_p/Solicita‡Æo_de_Dep¢sito * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

for each ped-venda where ped-venda.no-ab-reppri  >= tt-param.repres-ini
                     and ped-venda.no-ab-reppri  <= tt-param.repres-fin
                     and ped-venda.nome-abrev    >= tt-param.cliente-ini
                     and ped-venda.nome-abrev    <= tt-param.cliente-fin
                     and ped-venda.dt-entrega    >= tt-param.dt-entrega-ini
                     and ped-venda.dt-entrega    <= tt-param.dt-entrega-fin
                     and ped-venda.dt-implant    >= tt-param.dt-implant-ini
                     and ped-venda.dt-implant    <= tt-param.dt-implant-fin
                     and (ped-venda.cod-sit-ped  < 3 or
                          ped-venda.cod-sit-ped  = 5)
                     and (ped-venda.cod-cond-pag > 0 and
                          ped-venda.cod-cond-pag < 4)
                   no-lock
                   by ped-venda.no-ab-reppri
                   by ped-venda.nome-abrev
                   by ped-venda.dt-entrega:
             
   run pi-acompanhar in h-acomp (input "Pedido: " + ped-venda.nr-pedcli).

   for each ped-item where ped-item.nome-abrev  = ped-venda.nome-abrev
                       and ped-item.nr-pedcli   = ped-venda.nr-pedcli
                       and (ped-item.cod-sit-item < 3 or
                            ped-item.cod-sit-item = 5)
                            no-lock:

       assign de-qtd = ped-item.qt-pedida -
                       ped-item.qt-atendida +
                       ped-item.qt-devolvida
              de-qtd-tot = de-qtd-tot + de-qtd.
                            
       find ped-item-res
            where ped-item-res.nome-abrev   = ped-item.nome-abrev
              and ped-item-res.nr-pedcli    = ped-item.nr-pedcli
              and ped-item-res.it-codigo    = ped-item.it-codigo
              and ped-item-res.nr-sequencia = ped-item.nr-sequencia
                  no-lock no-error.
                                           
       if not avail ped-item-res then
          next.
           
       assign de-valor   = de-valor   + (de-qtd * ped-item.vl-preori)
              de-qtd-ger = de-qtd-ger + de-qtd.
              
       if ped-item-res.sol-deposito then
          assign de-vlr-solic = de-vlr-solic +
                                (de-qtd * ped-item.vl-preori).
       else 
          assign de-nao-solic = de-nao-solic +
                                (de-qtd * ped-item.vl-preori).
                  
   end.
    
   if  de-qtd-tot > 0
   and de-valor   > 0 then do:
       de-reserva = de-qtd-ger / de-qtd-tot * 100.
       display ped-venda.no-ab-reppri
               ped-venda.nome-abrev
               ped-venda.dt-entrega
               ped-venda.dt-implant
               ped-venda.nr-pedcli
               de-qtd-tot
               de-reserva
               de-valor
               de-vlr-solic
               de-nao-solic
               with frame f-detalhe.
       down with frame f-detalhe.
   
       assign de-ger-qtd     = de-ger-qtd     + de-qtd-tot
              de-ger-solic   = de-ger-solic   + de-vlr-solic
              de-ger-nao-sol = de-ger-nao-sol + de-nao-solic
              de-ger-valor   = de-ger-valor   + de-valor.
   end.
              
   assign de-qtd-tot   = 0
          de-qtd-ger   = 0
          de-valor     = 0
          de-vlr-solic = 0
          de-nao-solic = 0.
                     
end.         

if de-ger-qtd > 0 then do:
  display "TOTAL"         @ ped-venda.dt-entrega 
           de-ger-qtd     @ de-qtd-tot
           de-ger-valor   @ de-valor
           de-ger-solic   @ de-vlr-solic
           de-ger-nao-sol @ de-nao-solic
           with frame f-detalhe.
   down 2 with frame f-detalhe.
end.

assign de-ger-qtd     = 0
       de-ger-solic   = 0
       de-ger-nao-sol = 0
       de-ger-valor   = 0.

      
IF tt-param.impr-param THEN DO:
   PAGE.
   display tt-param.repres-ini
           tt-param.repres-fin
           tt-param.cliente-ini
           tt-param.cliente-fin
           tt-param.dt-entrega-ini
           tt-param.dt-entrega-fin
           tt-param.dt-implant-ini
           tt-param.dt-implant-fin
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

