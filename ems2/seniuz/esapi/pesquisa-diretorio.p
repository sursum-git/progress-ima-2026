DEF INPUT PARAMETER i-codclie    AS INT.
DEF OUTPUT PARAMETER c-diretorio AS CHAR.

DEFINE VARIABLE codclie AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE tt-diretorio
    FIELD c-diretorio AS CHAR FORMAT 'x(200)'.

ASSIGN codclie = 10200.

OS-COMMAND SILENT VALUE("Dir /b Z:*(" + string(codclie) + ")* > c:\temp\List-Diretorio.txt") .

INPUT FROM c:\temp\List-Diretorio.txt.
REPEAT:
    CREATE tt-diretorio.
    IMPORT DELIMITER "|" tt-diretorio.   
END.
INPUT CLOSE.

FOR EACH tt-diretorio WHERE tt-diretorio.c-diretorio <> "":
    DISP tt-diretorio WITH 1 COL WIDTH 550.
END.

