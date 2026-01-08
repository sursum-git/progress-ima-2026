DEFINE PARAMETER BUFFER p-table FOR familia.

FIND familia-ext WHERE 
     familia-ext.fm-codigo = p-table.fm-codigo NO-ERROR.

IF AVAILABLE familia-ext THEN 
   DELETE familia-ext.

