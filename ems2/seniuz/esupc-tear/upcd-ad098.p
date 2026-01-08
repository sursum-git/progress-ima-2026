DEFINE PARAMETER BUFFER p-table FOR emitente.

FIND emitente-ext WHERE 
     emitente-ext.cod-emitente = p-table.cod-emitente NO-ERROR.

IF AVAILABLE emitente-ext THEN 
   DELETE emitente-ext.

