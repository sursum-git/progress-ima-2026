DEF INPUT  PARAMETER i-cod-emitente AS CHAR. 
DEF OUTPUT PARAMETER c-pasta-doc    AS CHAR.

DEFINE VARIABLE c-comando AS CHARACTER  NO-UNDO.

DEFINE TEMP-TABLE tt-diretorio
    FIELD c-diretorio AS CHAR FORMAT 'x(200)'.

ASSIGN c-comando = "Dir /b V:\*(" + string(i-cod-emitente) + ")* > c:\temp\List-Diretorio.txt".

OS-COMMAND SILENT VALUE(c-comando) .

INPUT FROM c:\temp\list-diretorio.txt.

REPEAT:
    CREATE tt-diretorio.
    IMPORT DELIMITER "|" tt-diretorio.
END.

INPUT CLOSE.

FOR EACH tt-diretorio WHERE
         tt-diretorio.c-diretorio <> "":
/*          DISP tt-diretorio WITH 1 COL WIDTH 550.*/
         ASSIGN c-pasta-doc = "V:\" + tt-diretorio.c-diretorio.
END.
