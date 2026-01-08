DEFINE PARAMETER BUFFER p-table FOR item.
DEFINE PARAMETER BUFFER p-table-old FOR item.

FIND item-ext WHERE 
     item-ext.it-codigo = p-table.it-codigo NO-ERROR.

IF AVAILABLE item-ext THEN 
   ASSIGN item-ext.descricao = p-table.descricao-1 + p-table.descricao-2.

