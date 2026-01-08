DEFINE INPUT  PARAMETER cExcel AS CHARACTER   NO-UNDO.

ASSIGN cExcel = SEARCH(cExcel).
IF cExcel <> ? THEN
OS-COMMAND SILENT VALUE('start excel /t ' + cExcel).
ELSE 
    MESSAGE "Arquivo Excel" cExcel " NÆo Encontrado no Propath"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
