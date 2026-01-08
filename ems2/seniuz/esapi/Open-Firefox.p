DEF INPUT PARAMETER p-site AS CHAR FORMAT "x(100)".

DEFINE VARIABLE c-comando AS CHARACTER NO-UNDO.

/* ASSIGN c-comando = '"C:\Program Files (x86)\Mozilla Firefox\firefox.exe" ' + p-site. */
ASSIGN c-comando = '"start firefox ' + p-site + '"'.

OS-COMMAND SILENT VALUE(c-comando).
