DEFINE PARAMETER BUFFER p-table FOR gr-cli.

FIND gr-cli-ext WHERE 
     gr-cli-ext.cod-gr-cli = p-table.cod-gr-cli NO-ERROR.

IF AVAILABLE gr-cli-ext THEN 
   DELETE gr-cli-ext.

