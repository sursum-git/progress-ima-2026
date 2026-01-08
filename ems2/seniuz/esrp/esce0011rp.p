/* Programa: ESCE019.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Controle de Estoque
** Objetivo: Listar o estoque disponivel para venda por familia de materiais
** Autor...: Gilvando de Souza Araujo - Dezembro/95
** Obs.....: Especifico da CIA.RENASCENCA INDUSTRIAL
**
** Conversao para EMS 2.04:
**   Autor: Prodb - Toninho
**   Data: 07/01/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESCE0011RP 2.04.00.000}

define temp-table tt-param no-undo
       field destino     as integer
       field arquivo     as char format "x(35)"
       field usuario     as char format "x(12)"
       field data-exec   as date
       field hora-exec   as integer
       FIELD cod-estabel LIKE movto-estoq.cod-estabel
       FIELD c-fam-ini   LIKE familia.fm-codigo
       FIELD c-fam-fim   LIKE familia.fm-codigo
       FIELD impr-param  AS LOGICAL.

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

def var de-tot-qtd-fam as dec.
def var de-tot-res-fam as dec.
def var de-saldo       as dec format "->>>,>>>,>>>,>>9.9999".

form
    tt-param.cod-estabel LABEL "Estabelecimento"  AT 1
    tt-param.c-fam-ini   LABEL "Familia........"  at 1
    "a" AT 31
    tt-param.c-fam-fim   no-labels skip(1)
    with no-box side-labels width 132 STREAM-IO frame f-parlis.

form
    familia.fm-codigo  
    familia.descricao  
    familia.un         
    de-saldo           label "Saldo Dispon¡vel"
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

{utp/ut-liter.i MATERIAIS * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Estoque_Dispon¡vel_por_Fam¡lia * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

for each familia WHERE
         familia.fm-codigo >= tt-param.c-fam-ini and
         familia.fm-codigo <= tt-param.c-fam-fim no-lock
    by familia.fm-codigo:

    run pi-acompanhar in h-acomp (input "Fam¡lia: " + familia.fm-codigo).
    
    for each item where 
             item.fm-codigo = familia.fm-codigo no-lock.
        for each saldo-estoq WHERE
                 saldo-estoq.it-codigo   = item.it-codigo AND
                 saldo-estoq.cod-estabel = tt-param.cod-estabel
            no-lock:
            assign de-tot-qtd-fam = de-tot-qtd-fam + saldo-estoq.qtidade-atu.
        end.
        for each ped-item-res WHERE
                 ped-item-res.it-codigo = item.it-codigo no-lock:
            if ped-item-res.faturado = yes then do:
               find nota-fiscal WHERE
                    nota-fiscal.cod-estabel = ped-item-res.cod-estabel AND
                    nota-fiscal.serie       = ped-item-res.serie AND
                    nota-fiscal.nr-nota-fis = string(ped-item-res.nr-nota-fis)
                    NO-LOCK NO-ERROR.
               if avail nota-fiscal then
                  if  nota-fiscal.ind-sit-nota < 5 AND
                      nota-fiscal.dt-cancela   = ? then
                      assign de-tot-res-fam = de-tot-res-fam + ped-item-res.qt-pedida.
            end.
            else
               assign de-tot-res-fam = de-tot-res-fam + ped-item-res.qt-pedida.
        end.
    end.
    if de-tot-qtd-fam <> 0 then do:
       assign de-saldo = de-tot-qtd-fam - de-tot-res-fam.
       display familia.fm-codigo
               familia.descricao
               familia.un
               de-saldo
               with frame f-detalhe.
       down with frame f-detalhe.
    end.
    assign de-tot-qtd-fam = 0
           de-tot-res-fam = 0.
end.

IF tt-param.impr-param THEN DO.
   PAGE.
   PUT "*****-------- PAR¶METROS --------*****"
       SKIP.
   display tt-param.cod-estabel
           tt-param.c-fam-ini 
           tt-param.c-fam-fim
           with frame f-parlis.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

