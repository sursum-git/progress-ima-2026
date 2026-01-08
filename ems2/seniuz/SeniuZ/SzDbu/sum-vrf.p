DEF INPUT  PARAMETER p-device AS CHAR.
DEF INPUT  PARAMETER p-pid AS INT.
DEF OUTPUT PARAMETER p-ok AS LOG.

DEF TEMP-TABLE tt-aux
    FIELD c-linha AS CHAR.

DEF VAR c-arquivo AS CHAR.
DEF VAR c-comando AS CHAR FORMAT "x(50)".

ASSIGN c-arquivo = SESSION:TEMP-DIRECTORY + 'sum-vrf.txt'.
ASSIGN c-comando = 'Tasklist /S ' + p-device + ' > ' + c-arquivo.

OS-COMMAND SILENT VALUE(c-comando).

INPUT FROM VALUE(c-arquivo).
REPEAT.
    CREATE tt-aux.
    IMPORT DELIMITER "&%$X" tt-aux.
END.
INPUT CLOSE.
OS-DELETE VALUE(c-arquivo).

ASSIGN p-ok = NO.

FIND FIRST tt-aux WHERE
           c-linha MATCHES "*" + STRING(p-pid) + "*" NO-LOCK NO-ERROR.
IF AVAIL tt-aux THEN 
   ASSIGN p-ok = YES.


