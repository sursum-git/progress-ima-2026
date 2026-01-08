DEF VAR c-comando AS CHAR FORMAT "x(30)".
DEF VAR c-arquivo AS CHAR FORMAT "x(30)".

ASSIGN c-comando = "TASKKILL /S " + "{1}" /F /PID "{2}" /T
OS-COMMAND SILENT VALUE(c-comando). 

