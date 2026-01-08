DEFINE INPUT  PARAMETER c-texto  AS CHARACTER.
DEFINE OUTPUT PARAMETER c-result AS CHARACTER.

DEFINE VARIABLE c-car    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE i-ind    AS INTEGER    NO-UNDO.

ASSIGN c-car   = "".
DO i-ind = 1 TO LENGTH(c-texto):
    IF NOT (c-car = " " AND SUBSTRING(c-texto, i-ind, 1) = " ") THEN
        ASSIGN c-result = c-result + SUBSTRING(c-texto, i-ind, 1).

    ASSIGN c-car = SUBSTRING(c-texto, i-ind, 1).
END.

