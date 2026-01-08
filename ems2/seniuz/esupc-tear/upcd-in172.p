DEFINE PARAMETER BUFFER p-table FOR item.

FIND item-ext WHERE 
     item-ext.it-codigo = p-table.it-codigo NO-ERROR.

IF AVAILABLE item-ext THEN 
   DELETE item-ext.

