DEFINE PARAMETER BUFFER p-table FOR transporte.

FIND transporte-ext WHERE 
     transporte-ext.cod-transp = p-table.cod-transp NO-ERROR.

IF AVAILABLE transporte-ext THEN 
   DELETE transporte-ext.

