/* include escr999.i - calculo dos documentos em aberto
{1} = data corte
{2} = saldo atual
*/

assign {2} = titulo.vl-saldo.
for each mov-tit where mov-tit.ep-codigo   = titulo.ep-codigo
                   and mov-tit.cod-estabel = titulo.cod-estabel
                   and mov-tit.cod-esp     = titulo.cod-esp
                   AND mov-tit.serie       = titulo.serie
                   and mov-tit.nr-docto    = titulo.nr-docto
                   and mov-tit.parcela     = titulo.parcela
                 NO-LOCK USE-INDEX codigo.

     IF mov-tit.dt-trans <= {1} THEN NEXT.

     if  mov-tit.transacao = 1 then
         assign {2} = {2} - mov-tit.vl-original.
     else
     if  mov-tit.transacao = 3
     or  mov-tit.transacao = 4 then
         assign {2} = {2}  + mov-tit.vl-baixa.
     else
     if  mov-tit.transacao = 14
     and mov-tit.lancamento = 2 THEN /* cred */
         assign {2} = {2} - mov-tit.vl-baixa.
     else
     if  mov-tit.transacao = 14
     and mov-tit.lancamento = 1 then /* deb */
         assign {2} = {2} + mov-tit.vl-baixa.
end.

/* fim */


