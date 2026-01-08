DEFINE PARAMETER BUFFER b-tit_acr_new FOR tit_ap.
DEFINE PARAMETER BUFFER b-tit_acr_old FOR tit_ap. 
DEFINE VARIABLE cProgramas AS CHARACTER   NO-UNDO FORMAT 'x(500)'.
DEFINE VARIABLE level      AS INTEGER     NO-UNDO.
MESSAGE 'oioioioioioioioi'
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
FIND FIRST tit_ap
     WHERE ROWID(tit_ap) = rowid(b-tit_acr_old) NO-LOCK NO-ERROR.
REPEAT WHILE PROGRAM-NAME(level) <> ?:  
    ASSIGN cProgramas = IF cProgramas = '' THEN PROGRAM-NAME(level) ELSE cProgramas + "," +  PROGRAM-NAME(level)
           level = level + 1.
END.
MESSAGE cProgramas
    VIEW-AS ALERT-BOX INFO BUTTONS OK.


