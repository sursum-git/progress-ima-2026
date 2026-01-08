DEFINE PARAMETER BUFFER p-table FOR ref-item.

FIND ref-item-ext WHERE 
     ref-item-ext.it-codigo = p-table.it-codigo AND
     ref-item-ext.cod-refer = p-table.cod-refer NO-ERROR.

IF AVAILABLE ref-item-ext THEN 
   DELETE ref-item-ext.

