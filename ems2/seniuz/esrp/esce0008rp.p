/* Programa: ESCE024.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Controle de Estoque
** Objetivo: Listar o Saldo Fisico Atual por Item
** Autor...: Gilvando de Souza Araujo - Setembro/2000
** Obs.....: Especifico da TEAT TEXTIL IND.COM.TDA.
**
** Conversao para EMS 2.04:
**   Programa: ESCE024.P  =>  ESCE0008RP.P
**   Autor...: FABIO COELHO LANZA
**   Data....: 19/02/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESFT0008RP 2.04.00.000}

define temp-table tt-param no-undo
    field destino             as integer
    field arquivo             as char format "x(35)"
    field usuario             as char format "x(12)"
    field data-exec           as date
    field hora-exec           as integer
    field classifica          as integer
    field desc-classifica     as char format "x(40)"
    FIELD cod-estabel         LIKE saldo-estoq.cod-estabel
    FIELD ini-it-codigo       LIKE item.it-codigo      
    FIELD fin-it-codigo       LIKE item.it-codigo      
    FIELD ini-cod-obsoleto    LIKE item.cod-obsoleto     
    FIELD fin-cod-obsoleto    LIKE item.cod-obsoleto       
    FIELD ini-cod-depos       LIKE saldo-estoq.cod-depos  
    FIELD fin-cod-depos       LIKE saldo-estoq.cod-depos
    FIELD tipo-estoque        AS INTEGER
    FIELD desc-tipo-estoq     AS CHAR FORMAT "x(20)"
    FIELD imp-param           AS LOG.


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

def var c-it-codigo    like item.it-codigo.
def var c-descricao    as char format "x(33)".
def var de-tot-qtd-ite as dec format "->>>>,>>>,>>9.9999".
DEF VAR l-lista-item   AS LOG.

form 
    "*------------ Parƒmetros/Sele‡Æo -------------*" SKIP
    tt-param.cod-estabel      LABEL "Estabelecimento." SKIP
    tt-param.ini-it-codigo    label "Item............"
    "A"  AT 35
    tt-param.fin-it-codigo    NO-LABELS SKIP
    tt-param.ini-cod-obsoleto label "Obsoleto........"
    "A"  AT 35
    tt-param.fin-cod-obsoleto no-labels SKIP
    tt-param.ini-cod-depos    label "Deposito........"
    "A"  AT 35
    tt-param.fin-cod-depos    no-labels SKIP
    tt-param.desc-tipo-estoq  LABEL "Tipo de Estoque."
    skip(1)
    with FRAME f-param side-labels no-box width 132 STREAM-IO.

form
    c-it-codigo               label "Item"
    c-descricao               label "Descricao"
    item.un                   label "Un"
    saldo-estoq.cod-refer     LABEL "Refer"
    referencia.descricao      LABEL "Descricao"
    saldo-estoq.cod-estabel   label "Est"
    saldo-estoq.cod-depos     label "Dep"
    saldo-estoq.qtidade-atu   label "Quantidade Atual"
    item.cod-obsoleto         label "Cob"
    with no-box NO-LABEL 55 down width 132 STREAM-IO FRAME f-detalhe.

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
{utp/ut-liter.i Saldo_Fisico_do_Estoque_Atual_por_Item * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

for each item where item.it-codigo    >= tt-param.ini-it-codigo
                and item.it-codigo    <= tt-param.fin-it-codigo
                and item.cod-obsoleto >= tt-param.ini-cod-obsoleto
                and item.cod-obsoleto <= tt-param.fin-cod-obsoleto
                    no-lock
              break by if tt-param.classific = 1 then item.it-codigo
                       else item.descricao-1:
    assign c-it-codigo = item.it-codigo
           c-descricao = item.descricao-1 + item.descricao-2.

    for each saldo-estoq where saldo-estoq.cod-estabel  = tt-param.cod-estabel
                           and saldo-estoq.cod-depos   >= tt-param.ini-cod-depos
                           and saldo-estoq.cod-depos   <= tt-param.fin-cod-depos
                           and saldo-estoq.it-codigo    =  item.it-codigo
                               no-lock:
 
        run pi-acompanhar in h-acomp (input item.it-codigo).

        assign l-lista-item = yes.
        if tt-param.tipo-estoque = 1  then
           if saldo-estoq.qtidade-atu <= 0 then
              assign l-lista-item = no.
        if tt-param.tipo-estoque = 2 then
           if saldo-estoq.qtidade-atu <> 0 then
              assign l-lista-item = no.
        if tt-param.tipo-estoque = 3 then
           if saldo-estoq.qtidade-atu >= 0 then
              assign l-lista-item = no.
        if l-lista-item then do:
           FIND referencia WHERE referencia.cod-refer = saldo-estoq.cod-refer
                           NO-LOCK NO-ERROR.
           display c-it-codigo
                   c-descricao
                   item.un
                   saldo-estoq.cod-refer
                   referencia.descricao WHEN AVAIL referencia
                   saldo-estoq.cod-estabel
                   saldo-estoq.cod-depos
                   saldo-estoq.qtidade-atu
                   item.cod-obsoleto
                   with frame f-detalhe.
           down with frame f-detalhe.
           ASSIGN c-it-codigo   = ""
                  c-descricao   = "".
        end.

    end. /* saldo-estoq */

end. /* item */

IF tt-param.imp-param THEN DO:
   PAGE.
   display tt-param.cod-estabel
           tt-param.ini-it-codigo
           tt-param.fin-it-codigo 
           tt-param.ini-cod-obsoleto     
           tt-param.fin-cod-obsoleto     
           tt-param.ini-cod-depos
           tt-param.fin-cod-depos
           tt-param.desc-tipo-estoq
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

