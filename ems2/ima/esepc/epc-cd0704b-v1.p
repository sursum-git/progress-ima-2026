DEF NEW GLOBAL SHARED VAR h-tg-messenger1 AS HANDLE.
DEF NEW GLOBAL SHARED VAR h-tg-messenger2 AS HANDLE.
DEF NEW GLOBAL SHARED VAR h-tg-messenger3 AS HANDLE.

IF SELF:SCREEN-VALUE = "" THEN DO.
   CASE SELF:NAME:
       WHEN  "celular1" THEN DO.
          ASSIGN h-tg-messenger1:SENSITIVE = NO.
          ASSIGN h-tg-messenger1:SCREEN-VALUE = "NO".
          APPLY "VALUE-CHANGED" TO h-tg-messenger1.
       END.
       WHEN  "celular2" THEN DO.
          ASSIGN h-tg-messenger2:SENSITIVE = NO.
          ASSIGN h-tg-messenger2:SCREEN-VALUE = "NO".
          APPLY "VALUE-CHANGED" TO h-tg-messenger2.
       END.
       WHEN  "celular3" THEN DO.
          ASSIGN h-tg-messenger3:SENSITIVE = NO.
          ASSIGN h-tg-messenger3:SCREEN-VALUE = "NO".
          APPLY "VALUE-CHANGED" TO h-tg-messenger3.
       END.
   END CASE.
END.
ELSE DO.
    CASE SELF:NAME:
        WHEN  "celular1" THEN
           ASSIGN h-tg-messenger1:SENSITIVE = YES.
        WHEN  "celular2" THEN
           ASSIGN h-tg-messenger2:SENSITIVE = YES.
        WHEN  "celular3" THEN
           ASSIGN h-tg-messenger3:SENSITIVE = YES.
    END CASE.
END.

