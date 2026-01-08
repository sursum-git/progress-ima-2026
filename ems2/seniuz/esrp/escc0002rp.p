/* Programa: ESCC003.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Compras
** Objetivo: Listagem das Ultimas entradas no estoque por item,
**           com variacao de precos.
** Autor...: Fabio Coelho Lanza - Abril/1999
** Obs.....: Especifico da CIA.RENASCENCA INDUSTRIAL
**
** Conversao para EMS 2.04:
**   Programa: ESCC003.P => ESCC0002RP.P
**   Autor...: Gilvando Souza Araujo
**   Data....: 08/02/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESCC0002RP 2.04.00.000}

define temp-table tt-param  no-undo
       field destino          as integer
       field arquivo          as char format "x(35)"
       field usuario          as char format "x(12)"
       field data-exec        as date
       field hora-exec        as integer
       field classifica       as integer
       field desc-classifica  as char format "x(40)"
       FIELD ini-it-codigo    LIKE recebimento.it-codigo
       FIELD fin-it-codigo    LIKE recebimento.it-codigo
       FIELD situacao1        AS LOG FORMAT "Sim/NÆo"
       FIELD situacao2        AS LOG FORMAT "Sim/NÆo"
       FIELD situacao3        AS LOG FORMAT "Sim/NÆo"
       FIELD situacao4        AS LOG FORMAT "Sim/NÆo"
       field imp-param        as log.

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

def var de-aliq-icm like recebimento.aliquota-icm.
def var de-pco-ult  like recebimento.preco-unit.
def var de-tot-ult  as dec format ">>>,>>>,>>9.9999".
def var de-tot      as dec format ">>>,>>>,>>9.9999".
def var de-variacao as dec format "->>>9.99".
def var da-dt-ult   like recebimento.data-nota.

form 
    "*----------------- Parƒmetros/Sele‡Æo -----------------*" SKIP
    tt-param.ini-it-codigo        label "Item................"
    "A"  AT 39                    
    tt-param.fin-it-codigo        NO-LABELS SKIP
    tt-param.situacao1            LABEL "Ativo..............." SKIP
    tt-param.situacao2            LABEL "Obsol.Ord.Automatica" SKIP
    tt-param.situacao3            LABEL "Obsol.Todas Ordens.." SKIP
    tt-param.situacao4            LABEL "Totalmente Obsoleto."
    with FRAME f-param side-labels no-box width 132 STREAM-IO.

form
    item.it-codigo           label "Codigo"
    item.desc-item           label "Descricao" FORMAT "x(40)"
    item.un                  label "Un"
    movto-estoq.dt-trans     label "Ultimas Compras"
    de-aliq-icm              label "Aliq. ICMS"
    de-tot                   label "Valor Total"
    de-variacao              label "% Var"                  
    with no-box 55 down width 132 STREAM-IO frame f-detalhe no-label.

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i}

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i SUPRIMENTOS * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Ötens_com_éltima_Compra_e_Varia‡Æo_de_Pre‡o * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).
 
for each item where item.it-codigo >= tt-param.ini-it-codigo
                and item.it-codigo <= tt-param.fin-it-codigo
                and ((item.cod-obsoleto = 1 AND tt-param.situacao1 = yes) OR
                     (item.cod-obsoleto = 2 AND tt-param.situacao2 = YES) OR
                     (item.cod-obsoleto = 3 AND tt-param.situacao3 = yes) OR
                     (item.cod-obsoleto = 4 AND tt-param.situacao4 = YES))
              no-lock
              by item.desc-item:                 
   
   run pi-acompanhar in h-acomp (input "Ötem: " + item.it-codigo).
   
   find last movto-estoq use-index item-data
        where movto-estoq.it-codigo = item.it-codigo
          and movto-estoq.esp-docto = 21 /* "nfe" */
        no-lock no-error.

   if avail movto-estoq then do:
   
      assign de-tot-ult  = movto-estoq.valor-nota /
                           movto-estoq.quantidade.
             da-dt-ult   = movto-estoq.dt-trans.
             de-aliq-icm = movto-estoq.valor-icm /
                           (movto-estoq.valor-nota -
                            movto-estoq.valor-ipi) * 100.
             
      find prev movto-estoq use-index item-data
           where movto-estoq.it-codigo = item.it-codigo
             and movto-estoq.esp-docto = 21 /* "nfe" */ 
             and movto-estoq.dt-trans <= (da-dt-ult - 1)
           no-lock no-error.

      if avail movto-estoq then do:
         assign de-tot      = (movto-estoq.valor-nota  /
                               movto-estoq.quantidade).
                de-variacao = (de-tot-ult / de-tot) * 100 - 100.
                                 
         display item.it-codigo  
                 item.desc-item            
                 item.un                
                 movto-estoq.dt-trans  
                 de-aliq-icm             
                 de-tot        
                 with frame f-detalhe.
         down  with frame f-detalhe.
                              
         assign de-aliq-icm = movto-estoq.valor-icm /
                              (movto-estoq.valor-nota -
                               movto-estoq.valor-ipi) * 100.

         display da-dt-ult              @ movto-estoq.dt-trans
                 de-aliq-icm                 
                 de-tot-ult             @ de-tot
                 de-variacao        
                 with frame f-detalhe.
         down 2 with frame f-detalhe.
      end.
      else do:
           display item.it-codigo  
                   item.desc-item            
                   item.un                
                   da-dt-ult      @ movto-estoq.dt-trans 
                   de-aliq-icm  
                   de-tot-ult     @ de-tot 
                   with frame f-detalhe.
           down 2 with frame f-detalhe.
      end.
   end.                        
end.  

IF tt-param.imp-param THEN DO:
   PAGE.
   display tt-param.ini-it-codigo
           tt-param.fin-it-codigo
           tt-param.situacao1
           tt-param.situacao2
           tt-param.situacao3
           tt-param.situacao4
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

