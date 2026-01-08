/* include ESFT999.I   Converte para metros, outras unidades de acordo
                       com a tabela de fatores

{1} = codigo do Item
{2} = quantidade original
{3} = quantidade calculada
*/
if {1} = "" then
   undo,retry.

find item-ext where item-ext.it-codigo = {1} no-lock no-error.
if  not avail item-ext then do:
    PUT "Nao ha fator de conversao para o Item: " {1}
        skip.
    undo,retry.
end.
else
    assign {3} = {2} * item-ext.fator-conv.

/* fim */


