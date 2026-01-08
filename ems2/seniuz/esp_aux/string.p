DEF VAR a AS CHAR FORMAT "x(20)" INIT "AnƇ C. Paula".
DEF VAR b AS CHAR FORMAT "x(20)".
DEF VAR i AS INT.

ASSIGN b = UPPER(a).

DO i = 1 TO LENGTH(b):
   IF SUBSTR(b,i,1) < "A" OR SUBSTR(b,i,1) > "Z" THEN
      ASSIGN b = REPLACE(b,SUBSTR(b,i,1)," ").
END.
ASSIGN b = REPLACE(b,"  "," ").
DISP a LENGTH(a) b LENGTH(b).
