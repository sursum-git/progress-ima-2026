DEF NEW GLOBAL SHARED VAR h-cb-aplicativo1 AS HANDLE.
DEF NEW GLOBAL SHARED VAR h-cb-aplicativo2 AS HANDLE.
DEF NEW GLOBAL SHARED VAR h-cb-aplicativo3 AS HANDLE.

IF SELF:SCREEN-VALUE = "NO" THEN DO.
   CASE SELF:NAME:
       WHEN "messenger1" THEN DO.
           ASSIGN h-cb-aplicativo1:SENSITIVE = NO.
           ASSIGN h-cb-aplicativo1:SCREEN-VALUE = " ".
       END.
       WHEN "messenger2" THEN DO.
           ASSIGN h-cb-aplicativo2:SENSITIVE = NO.
           ASSIGN h-cb-aplicativo2:SCREEN-VALUE = " ".
       END.
       WHEN "messenger3" THEN DO.
           ASSIGN h-cb-aplicativo3:SENSITIVE = NO.
           ASSIGN h-cb-aplicativo3:SCREEN-VALUE = " ".
       END.
   END CASE.
END.
ELSE DO.
    CASE SELF:NAME:
        WHEN "messenger1" THEN ASSIGN h-cb-aplicativo1:SENSITIVE = YES.
        WHEN "messenger2" THEN ASSIGN h-cb-aplicativo2:SENSITIVE = YES.
        WHEN "messenger3" THEN ASSIGN h-cb-aplicativo3:SENSITIVE = YES.
    END CASE.
END.

