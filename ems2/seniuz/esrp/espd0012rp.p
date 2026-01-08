/* Programa: ESPD0012RP.P
** Autor...: Gilvando Souza Araujo
** Data....: 25/01/2004
** Observ..: Chamado pelo programa ESPD0012.W
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESPD0012RP 2.04.00.000}

define temp-table tt-param no-undo
       field destino         as integer
       field arquivo         as char format "x(35)"
       field usuario         as char format "x(12)"
       field data-exec       as date
       field hora-exec       as integer
       field classifica      as integer
       FIELD it-codigo       LIKE item.it-codigo
       FIELD dt-implant-ini  LIKE ped-venda.dt-implant
       FIELD qtd-disponivel  AS DEC FORMAT ">>>,>>9.99"
       FIELD impr-param      AS LOGICAL.

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

def var da-dat-entreg1 as date format 99/99/9999.
def var da-dat-entreg2 as date format 99/99/9999.
def var de-qtd-disp    as dec format ">>>,>>>,>>9.9999".
def var de-qtd-ating1  as dec format ">>>,>>>,>>9.9999".
def var de-qtd-ating2  as dec format ">>>,>>>,>>9.9999".
def var de-qtd-dia     as dec.

form
    "*------------ Parƒmetros/Sele‡Æo -------------*" SKIP
    tt-param.it-codigo       LABEL "Item................." SKIP 
    tt-param.qtd-disponivel  LABEL "Quantidade Disponivel" SKIP
    tt-param.dt-implant-ini  LABEL "Data Implant.Inicial." SKIP
    with no-box side-labels width 132 STREAM-IO frame f-param.

form
    "                    Item:" at  1
    tt-param.it-codigo          at 27 NO-LABELS FORMAT "x(8)" 
    "-" item.desc-item                NO-LABELS
    "   Quantidade disponivel:" at  1
    tt-param.qtd-disponivel     at 27 NO-LABELS
    " Data de implant inicial:" at  1
    tt-param.dt-implant-ini     at 27 NO-LABELS
    skip(1)
    "Primeira data de entrega:" at  1
    da-dat-entreg1              at 27 NO-LABELS
    "     Quantidade atingida:" at  1
    de-qtd-ating1               at 27 NO-LABELS
    skip(1)
    " Proxima data de entrega:" at  1
    da-dat-entreg2              AT 27 NO-LABELS
    "     Quantidade atingida:" at  1 
    de-qtd-ating2               at 27 NO-LABELS
    with side-labels row 6 NO-BOX WIDTH 132 STREAM-IO frame f-detalhe.

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i}

/* bloco principal do programa */
find first param-global no-lock no-error.
find first ems2cad.empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i FATURAMENTO * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Lote_para_Programa‡Æo_de_Produ‡Æo * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

ASSIGN de-qtd-disp = tt-param.qtd-disponivel.

FOR EACH item WHERE
         item.it-codigo = tt-param.it-codigo NO-LOCK,
    EACH ped-item WHERE
         ped-item.it-codigo = ITEM.it-codigo and
        (ped-item.cod-sit-item < 3 or
         ped-item.cod-sit-item = 5) no-lock,
    EACH ped-venda of ped-item where
         ped-venda.dt-implant >= tt-param.dt-implant-ini no-lock
         break by ped-item.dt-entrega:

    run pi-acompanhar in h-acomp (input "Pedido: " + ped-venda.nr-pedcli).

    find ped-item-res where ped-item-res.nome-abrev   = ped-item.nome-abrev
                        and ped-item-res.nr-pedcli    = ped-item.nr-pedcli
                        and ped-item-res.it-codigo    = ped-item.it-codigo
                        and ped-item-res.nr-sequencia = ped-item.nr-sequencia
                      no-lock no-error.
    if not avail ped-item-res then
       assign de-qtd-dia = de-qtd-dia + ped-item.qt-pedida.

    if last-of(ped-item.dt-entrega) then do:
       if (de-qtd-ating1 + de-qtd-dia) > de-qtd-disp then do:
          assign de-qtd-ating2  = de-qtd-ating1 + de-qtd-dia
                 de-qtd-dia     = 0
                 da-dat-entreg2 = ped-item.dt-entrega.
          leave.        
       end.
       else
          assign de-qtd-ating1  = de-qtd-ating1 + de-qtd-dia
                 de-qtd-dia     = 0
                 da-dat-entreg1 = ped-item.dt-entrega.
    end.
END.

FIND item WHERE
     item.it-codigo = tt-param.it-codigo NO-LOCK NO-ERROR.
display tt-param.it-codigo
        item.desc-item WHEN AVAIL item 
        tt-param.qtd-disponivel
        tt-param.dt-implant-ini
        da-dat-entreg1
        de-qtd-ating1
        da-dat-entreg2
        de-qtd-ating2
        with frame f-detalhe.
      
IF tt-param.impr-param THEN DO:
   PAGE.
   display tt-param.it-codigo
           tt-param.qtd-disponivel
           tt-param.dt-implant-ini
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

