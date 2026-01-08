/* Programa: ESCE045.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Controle de Estoque
** Objetivo: Listar a Movimentacao de Estoque por CCusto
** Autor...: Gilvando Souza Araujo - Junho/2001
** Obs.....: Especifico da TEAR TEXTIL INDUSTRIA E COMERCIO LTDA.
**
** Conversao para EMS 2.04:
**   Programa: ESCE045.P  =>  ESCE0004RP.P
**   Autor...: Gilvando Souza Araujo
**   Data....: 15/02/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESCE0004RP 2.04.00.000}

define temp-table tt-param    no-undo
       field destino          as integer
       field arquivo          as char format "x(35)"
       field usuario          as char format "x(12)"
       field data-exec        as date
       field hora-exec        as integer
       FIELD cod-estabel      LIKE movto-estoq.cod-estabel
       FIELD dt-trans-ini     LIKE movto-estoq.dt-trans 
       FIELD dt-trans-fin     LIKE movto-estoq.dt-trans
       FIELD ct-codigo-ini    LIKE movto-estoq.ct-codigo
       FIELD ct-codigo-fin    LIKE movto-estoq.ct-codigo
       FIELD sc-codigo-ini    LIKE movto-estoq.sc-codigo
       FIELD sc-codigo-fin    LIKE movto-estoq.sc-codigo
       FIELD ct-excluir-1     LIKE movto-estoq.ct-codigo
       FIELD ct-excluir-2     LIKE movto-estoq.ct-codigo
       FIELD ct-excluir-3     LIKE movto-estoq.ct-codigo
       FIELD ct-excluir-4     LIKE movto-estoq.ct-codigo
       FIELD ct-excluir-5     LIKE movto-estoq.ct-codigo
       FIELD ct-excluir-6     LIKE movto-estoq.ct-codigo
       FIELD ct-excluir-7     LIKE movto-estoq.ct-codigo
       FIELD ct-excluir-8     LIKE movto-estoq.ct-codigo
       FIELD ct-excluir-9     LIKE movto-estoq.ct-codigo
       FIELD ct-excluir-10    LIKE movto-estoq.ct-codigo
       FIELD ct-excluir-11    LIKE movto-estoq.ct-codigo
       FIELD ct-excluir-12    LIKE movto-estoq.ct-codigo
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

def var de-tot-vlr-ccu as dec format ">>>,>>>,>>9.99".
def var de-tot-vlr-ger as dec format ">>>,>>>,>>9.99".
def var c-tipo-trans  as char format "x(3)".
DEF VAR c-ct-desconsid AS CHAR FORMAT "x(90)".

form
    "*------------ Parƒmetros/Sele‡Æo -------------*" SKIP
    tt-param.cod-estabel      label "Estabelecimento" AT 1
    tt-param.dt-trans-ini     label "Data Transa‡Æo." AT 1
    "a"  AT 30                
    tt-param.dt-trans-fin     no-labels
    tt-param.ct-codigo-ini    LABEL "Conta.........." AT 1
    "a"  AT 30                
    tt-param.ct-codigo-fin    NO-LABELS
    tt-param.sc-codigo-ini    LABEL "Sub-Conta......" AT 1
    "a"  AT 30
    tt-param.sc-codigo-fin    NO-LABELS
    tt-param.desc-tipo-rel    LABEL "Tipo Relat¢rio." AT 1
    c-ct-desconsid            LABEL "Contas Descons." AT 1
    with no-box side-labels width 132 stream-io frame f-param.

form
    movto-estoq.sc-codigo       label "CCUSTO"
    sub-conta.descricao         label "DESCRICAO"
    movto-estoq.ct-codigo       label "CONTA"
    movto-estoq.dt-trans        label "DATA TRANS"
    movto-estoq.quantidade      label "QUANTIDADE"
    movto-estoq.un              label "UN"
    movto-estoq.valor-mat-m[1]  label "VALOR MATERIAL"
    c-tipo-trans                label "Tip"
    movto-estoq.it-codigo       label "ITEM"           format "x(11)"
    with no-box NO-LABEL 55 down width 134 STREAM-IO frame f-detalhe.

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i}

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i MATERIAIS * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Movimento_de_Estoque_por_Centro_de_Custo * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
VIEW frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

for each movto-estoq where movto-estoq.dt-trans    >= tt-param.dt-trans-ini
                       and movto-estoq.dt-trans    <= tt-param.dt-trans-fin
                       and movto-estoq.ct-codigo   >= tt-param.ct-codigo-ini
                       and movto-estoq.ct-codigo   <= tt-param.ct-codigo-fin
                       and movto-estoq.sc-codigo   >= tt-param.sc-codigo-ini
                       and movto-estoq.sc-codigo   <= tt-param.sc-codigo-fin
                       AND movto-estoq.cod-estabel =  tt-param.cod-estabel
                       AND (movto-estoq.ct-codigo <> tt-param.ct-excluir-1 OR 
                                                     tt-param.ct-excluir-1 = "")
                       AND (movto-estoq.ct-codigo <> tt-param.ct-excluir-2 OR 
                                                     tt-param.ct-excluir-2 = "")
                       AND (movto-estoq.ct-codigo <> tt-param.ct-excluir-3 OR  
                                                     tt-param.ct-excluir-3 = "")
                       AND (movto-estoq.ct-codigo <> tt-param.ct-excluir-4 OR  
                                                     tt-param.ct-excluir-4 = "")
                       AND (movto-estoq.ct-codigo <> tt-param.ct-excluir-5 OR  
                                                     tt-param.ct-excluir-5 = "")
                       AND (movto-estoq.ct-codigo <> tt-param.ct-excluir-6 OR  
                                                     tt-param.ct-excluir-6 = "")
                       AND (movto-estoq.ct-codigo <> tt-param.ct-excluir-7 OR  
                                                     tt-param.ct-excluir-7 = "")
                       AND (movto-estoq.ct-codigo <> tt-param.ct-excluir-8 OR  
                                                     tt-param.ct-excluir-8 = "")
                       AND (movto-estoq.ct-codigo <> tt-param.ct-excluir-9 OR    
                                                     tt-param.ct-excluir-9 = "") 
                       AND (movto-estoq.ct-codigo <> tt-param.ct-excluir-10 OR     
                                                     tt-param.ct-excluir-10 = "") 
                       AND (movto-estoq.ct-codigo <> tt-param.ct-excluir-11 OR    
                                                     tt-param.ct-excluir-11 = "") 
                       AND (movto-estoq.ct-codigo <> tt-param.ct-excluir-12 OR    
                                                     tt-param.ct-excluir-12 = "") 
                     NO-LOCK
                     BREAK BY movto-estoq.sc-codigo
                           BY movto-estoq.ct-codigo:

    run pi-acompanhar in h-acomp (input "Data: " + string(movto-estoq.dt-trans) + 
                                        " Conta: " + STRING(movto-estoq.ct-codigo)).

    if movto-estoq.tipo-trans = 1 then
       assign c-tipo-trans = "Ent".
    else
       assign c-tipo-trans = "Sai".
       
    if tt-param.tipo-rel = 2 then do: /* Analitico */
       find sub-conta where sub-conta.sc-codigo = movto-estoq.sc-codigo
                      no-lock no-error.
       display movto-estoq.sc-codigo
               sub-conta.descricao
               movto-estoq.ct-codigo
               movto-estoq.dt-trans
               movto-estoq.quantidade
               movto-estoq.un
               movto-estoq.valor-mat-m[1]
               c-tipo-trans
               movto-estoq.it-codigo
               with frame f-detalhe.
       down with frame f-detalhe.
    end.
   
    if movto-estoq.tipo-trans = 1 then
       assign de-tot-vlr-ccu = de-tot-vlr-ccu - movto-estoq.valor-mat-m[1]
              de-tot-vlr-ger = de-tot-vlr-ger - movto-estoq.valor-mat-m[1].
    else
       assign de-tot-vlr-ccu = de-tot-vlr-ccu + movto-estoq.valor-mat-m[1]
              de-tot-vlr-ger = de-tot-vlr-ger + movto-estoq.valor-mat-m[1].
    
    if last-of(movto-estoq.sc-codigo) then do:
        find sub-conta where sub-conta.sc-codigo = movto-estoq.sc-codigo
                      no-lock no-error.
        display movto-estoq.sc-codigo
                sub-conta.descricao
                de-tot-vlr-ccu @ movto-estoq.valor-mat-m[1]
                with frame f-detalhe.
        down with frame f-detalhe.
    
        assign de-tot-vlr-ccu = 0.
    end.
end.

down 2 with frame f-detalhe.

display "Total:"       @ movto-estoq.sc-codigo
        de-tot-vlr-ger @ movto-estoq.valor-mat-m[1]
        with frame f-detalhe.
down with frame f-detalhe.


IF tt-param.impr-param THEN DO:
   ASSIGN c-ct-desconsid = STRING(tt-param.ct-excluir-1,"99999999") + " " +
                           STRING(tt-param.ct-excluir-2,"99999999") + " " +
                           STRING(tt-param.ct-excluir-3,"99999999") + " " +
                           STRING(tt-param.ct-excluir-4,"99999999") + " " +
                           STRING(tt-param.ct-excluir-5,"99999999") + " " +
                           STRING(tt-param.ct-excluir-6,"99999999") + " " +
                           STRING(tt-param.ct-excluir-7,"99999999") + " " +
                           STRING(tt-param.ct-excluir-8,"99999999") + " " +
                           STRING(tt-param.ct-excluir-9,"99999999") + " " +
                           STRING(tt-param.ct-excluir-10,"99999999") + " " +
                           STRING(tt-param.ct-excluir-11,"99999999") + " " +
                           STRING(tt-param.ct-excluir-12,"99999999").
   PAGE.
   display tt-param.cod-estabel    
           tt-param.dt-trans-ini   
           tt-param.dt-trans-fin   
           tt-param.ct-codigo-ini    
           tt-param.ct-codigo-fin 
           tt-param.sc-codigo-ini
           tt-param.sc-codigo-fin
           tt-param.desc-tipo-rel
           c-ct-desconsid
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

