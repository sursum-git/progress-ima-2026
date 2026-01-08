DEF INPUT PARAMETER p-usuario AS CHAR.
DEF OUTPUT PARAMETER p-cod-estab AS CHAR.

FIND FIRST ob-param NO-LOCK NO-ERROR.

DEF VAR i-ct AS INT.
DEF VAR c-usuario AS CHAR.
DEF VAR c-cod-estab AS CHAR.
/*
DO i-ct = 1 TO NUM-ENTRIES(ob-param.usuar-estab,",").
   ASSIGN c-usuario = ENTRY(1,ENTRY(i-ct,ob-param.usuar-estab,","),":")
          c-cod-estab = ENTRY(2,ENTRY(i-ct,ob-param.usuar-estab,","),":").

   IF c-usuario = p-usuario THEN LEAVE.

   ASSIGN c-usuario = ""
          c-cod-estab = "".
END.

IF c-cod-estab = '' THEN
   ASSIGN p-cod-estab = '1'.
ELSE
   ASSIGN p-cod-estab = c-cod-estab.
*/

ASSIGN p-cod-estab = '1'.


