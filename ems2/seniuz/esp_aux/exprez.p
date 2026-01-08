/* Programa: exprez.p
** Objetivo: Gerar arquivo texto com TD revertendo lancamentos.
*/

def var c-un like item.un.
def var i-tipo-trans LIKE movto-estoq.tipo-trans.
def var de-valor as dec format "99999999999999".
output to "c:/temp/movto.exp".
for each movto-estoq where movto-estoq.dt-trans  >= 04/30/2005
                       and movto-estoq.dt-trans  <= 04/30/2005
                       and movto-estoq.esp-doc   =  2 /* act */
                       AND movto-estoq.nro-docto <= "899"
                           no-lock:
   find item where item.it-codigo = movto-estoq.it-codigo
      no-error.
   assign c-un = item.un.
   if movto-estoq.tipo-trans = 1 then
      assign i-tipo-trans = 2
             de-valor = movto-estoq.quantidade * 10000.
   else
      assign i-tipo-trans = 1
             de-valor = movto-estoq.quantidade * 10000.

   put movto-estoq.it-codigo
       movto-estoq.cod-estabel
       movto-estoq.cod-depos
       movto-estoq.cod-localiz
       movto-estoq.lote
       "12/31/9999"
       "EstornoACT"
       movto-estoq.numero-ordem format "99999999999"
       STRING(movto-estoq.ct-codigo,"9999999") + STRING(movto-estoq.sc-codigo,"99999") FORMAT "x(12)"
       "    "
       "Estrn"
       "070571          "
       movto-estoq.tipo-trans FORMAT "99" /* era i-tipo-trans FORMAT "99" */
       movto-estoq.quantidade * 10000 FORMAT "99999999999999"
       c-un
       movto-estoq.valor-mat-m[1] * 100 format "99999999999999"
       movto-estoq.valor-mat-m[2] * 100 format "99999999999999"
       movto-estoq.valor-mat-m[3] * 100 format "99999999999999"
       movto-estoq.valor-mob-m[1] * 100 format "99999999999999"
       movto-estoq.valor-mob-m[2] * 100 format "99999999999999"
       movto-estoq.valor-mob-m[3] * 100 format "99999999999999"
       movto-estoq.valor-ggf-m[1] * 100 format "99999999999999"
       movto-estoq.valor-ggf-m[2] * 100 format "99999999999999"
       movto-estoq.valor-ggf-m[3] * 100 format "99999999999999"
       movto-estoq.cod-refer
       "0000000000000000" /* Cta aplica‡Æo */
       "N"
       "000000000"
       "05/01/2005"
       skip.
end.
