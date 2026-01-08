DEF OUTPUT PARAMETER p-base AS CHAR.
DEF VAR DATA AS CHARACTER NO-UNDO.
DEFINE VARIABLE cArq AS CHARACTER   NO-UNDO.

GET-KEY-VALUE SECTION "ESPECIFICOS" KEY "BASE" VALUE DATA.
    
ASSIGN p-base = data.

IF p-base = ? THEN
DO:
   ASSIGN cArq = SEARCH('base.txt') .
   IF cArq <> ? THEN
   DO:
       INPUT FROM value(cArq).       
        REPEAT:
            IMPORT p-Base.
        END.                  
       INPUT CLOSE.
       
   END.
    
END.

