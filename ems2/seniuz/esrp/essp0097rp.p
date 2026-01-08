/* Programa: ESPD017.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Pedidos
** Objetivo: Relatorio Reservas por Data/Situa‡Æo
** Autor...: Gilvando de Souza Araujo - Agosto/1997
** Obs.....: Especifico da CIA.RENASCENCA INDUSTRIAL
**
** Conversao para EMS 2.04:
**   Programa: ESPD017.P  =>  ESSP0097RP.P
**   Autor...: Gilvando Souza Araujo
**   Data....: 16/04/2005
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESSP0097RP 2.04.00.000}

define temp-table tt-param  no-undo
       field destino          as integer
       field arquivo          as char format "x(35)"
       field usuario          as char format "x(12)"
       field data-exec        as date
       field hora-exec        as integer
       field classifica       as integer
       field desc-classifica  as char format "x(40)"
       field it-codigo-ini    like ped-item-res.it-codigo
       field it-codigo-fin    like ped-item-res.it-codigo
       FIELD cod-refer-ini    LIKE ped-item-res.cod-refer
       FIELD cod-refer-fin    LIKE ped-item-res.cod-refer
       FIELD dt-trans-ini     LIKE ped-item-res.dt-trans
       FIELD dt-trans-fin     LIKE ped-item-res.dt-trans
       FIELD sit-abe          AS LOG FORMAT "Sim/NÆo"
       FIELD sit-atp          AS LOG FORMAT "Sim/NÆo"
       FIELD sit-att          AS LOG FORMAT "Sim/NÆo"
       FIELD sit-pen          AS LOG FORMAT "Sim/NÆo"
       FIELD sit-sus          AS LOG FORMAT "Sim/NÆo"
       FIELD sit-can          AS LOG FORMAT "Sim/NÆo"
       FIELD sem-ped          AS LOG FORMAT "Sim/NÆo"
       FIELD imp-param        AS LOG.

define temp-table tt-raw-digita
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* include padrÆo para impressÆo de campos editores em relat¢rio  */
{include/tt-edit.i}

/* defini‡Æo de vari veis  */
def var h-acomp as handle no-undo.

def var de-tot-reg-tip as dec format ">>,>>>,>>9.99".
def var de-tot-ld-tip  as dec format ">>,>>>,>>9.99".
def var de-tot-ret-tip as dec format ">>,>>>,>>9.99".
def var de-tot-def-tip as dec format ">>>,>>>,>>9.99".
def var de-tot-reg-it  as dec format ">>,>>>,>>9.99".
def var de-tot-ld-it   as dec format ">>,>>>,>>9.99".
def var de-tot-ret-it  as dec format ">>,>>>,>>9.99".
def var de-tot-def-it  as dec format ">>>,>>>,>>9.99".
def var de-tot-reg-ger as dec format ">>,>>>,>>9.99".
def var de-tot-ld-ger  as dec format ">>,>>>,>>9.99".
def var de-tot-ret-ger as dec format ">>,>>>,>>9.99".
def var de-tot-def-ger as dec format ">>>,>>>,>>9.99".
def var de-aux-perc1   as dec format ">>9.99".
def var de-aux-perc2   as dec format ">>9.99".
def var de-aux-perc3   as dec format ">>9.99".
def var de-aux-perc4   as dec format ">>9.99".
DEF VAR c-sit-item     AS CHAR FORMAT "x(3)".

form 
    "*--------------- Parƒmetros/Sele‡Æo ----------------*"  SKIP
    tt-param.it-codigo-ini   label "Item............."
    "A"  AT 36                    
    tt-param.it-codigo-fin   NO-LABELS                 SKIP
    tt-param.cod-refer-ini   label "Referˆncia......."
    "A"  AT 36                    
    tt-param.cod-refer-fin   NO-LABELS                 SKIP
    tt-param.dt-trans-ini    label "Data da Transa‡Æo"
    "A"  AT 36                    
    tt-param.dt-trans-fin    NO-LABELS                 SKIP
    tt-param.sit-abe         LABEL "Aberto..........." SKIP
    tt-param.sit-atp         LABEL "Atendido Parcial." SKIP
    tt-param.sit-att         LABEL "Atendido Total..." SKIP
    tt-param.sit-pen         LABEL "Pendente........." SKIP
    tt-param.sit-sus         LABEL "Suspenso........." SKIP
    tt-param.sit-can         LABEL "Cancelado........" SKIP
    tt-param.sem-ped         LABEL "Sem Pedido......."
    with FRAME f-param side-labels no-box width 132 STREAM-IO.

form
    ped-item-res.nome-abrev 
    ped-item-res.nr-pedcli
    ped-item-res.it-codigo
    ped-item-res.cod-refer
    ped-item-res.nr-sequencia
    ped-item-res.qt-pedida
    ped-item-res.volume-ini
    ped-item-res.volume-fim
    ped-item-res.dt-trans
    c-sit-item              LABEL "SIT"
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

{utp/ut-liter.i ESPECIFICO * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Reservas_de_Pedidos_por_Data/Situa‡Æo * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

FOR EACH ped-item-res USE-INDEX indice1
                      WHERE ped-item-res.it-codigo >= tt-param.it-codigo-ini
                        AND ped-item-res.it-codigo <= tt-param.it-codigo-fin
                        AND ped-item-res.cod-refer >= tt-param.cod-refer-ini 
                        AND ped-item-res.cod-refer <= tt-param.cod-refer-fin 
                        AND ped-item-res.dt-trans  >= tt-param.dt-trans-ini 
                        AND ped-item-res.dt-trans  <= tt-param.dt-trans-fin
                       NO-LOCK:


    run pi-acompanhar in h-acomp (input "Item: " + ped-item-res.it-codigo + 
                                        " Refer: " + ped-item-res.cod-refer +
                                        " Data: " +  STRING(ped-item-res.dt-trans)).

    FIND ped-item WHERE ped-item.nome-abrev   = ped-item-res.nome-abrev
                    AND ped-item.nr-pedcli    = ped-item-res.nr-pedcli
                    AND ped-item.it-codigo    = ped-item-res.it-codigo
                    AND ped-item.cod-refer    = ped-item-res.cod-refer
                    AND ped-item.nr-sequencia = ped-item-res.nr-sequencia
                  NO-LOCK NO-ERROR.

    IF NOT AVAIL ped-item AND tt-param.sem-ped = NO THEN NEXT.

    IF AVAIL ped-item THEN DO:
       IF (ped-item.cod-sit-item = 1 AND tt-param.sit-abe = NO) OR
          (ped-item.cod-sit-item = 2 AND tt-param.sit-atp = NO) OR
          (ped-item.cod-sit-item = 3 AND tt-param.sit-att = NO) OR
          (ped-item.cod-sit-item = 4 AND tt-param.sit-pen = NO) OR
          (ped-item.cod-sit-item = 5 AND tt-param.sit-sus = NO) OR
          (ped-item.cod-sit-item = 6 AND tt-param.sit-can = NO) THEN NEXT.
       ASSIGN c-sit-item = SUBSTR("AbeAtpAttPenSusCanBal",INT(ped-item.cod-sit-item) * 3 - 2,3).
    END.
    ELSE
       ASSIGN c-sit-item = "N/E".

    DISPLAY ped-item-res.nome-abrev  
            ped-item-res.nr-pedcli   
            ped-item-res.it-codigo   
            ped-item-res.cod-refer   
            ped-item-res.nr-sequencia
            ped-item-res.qt-pedida   
            ped-item-res.volume-ini  
            ped-item-res.volume-fim  
            ped-item-res.dt-trans
            c-sit-item 
            WITH FRAME f-detalhe.
    DOWN WITH FRAME f-detalhe.
END.

IF tt-param.imp-param THEN DO:
   PAGE.
   display tt-param.it-codigo-ini
           tt-param.it-codigo-fin
           tt-param.cod-refer-ini
           tt-param.cod-refer-fin
           tt-param.dt-trans-ini 
           tt-param.dt-trans-fin 
           tt-param.sit-abe      
           tt-param.sit-atp      
           tt-param.sit-att      
           tt-param.sit-pen      
           tt-param.sit-sus      
           tt-param.sit-can      
           tt-param.sem-ped      
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

