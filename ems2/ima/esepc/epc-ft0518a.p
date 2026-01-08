DEF NEW GLOBAL SHARED VAR h-nr-nota-fin AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-cb-envia-email AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-ed-dest-email  AS HANDLE NO-UNDO.

DEFINE INPUT  PARAMETER p-btok AS HANDLE      NO-UNDO.

IF VALID-HANDLE(h-nr-nota-fin) AND
   h-nr-nota-fin:SCREEN-VALUE <> "" AND 
   h-cb-envia-email:SCREEN-VALUE <> 'Nenhum' AND
   h-ed-dest-email:SCREEN-VALUE = "" THEN DO:

   MESSAGE 'Favor Conferir o Destinat rio do E-MAIL'
        VIEW-AS ALERT-BOX ERROR BUTTONS OK.

   APPLY 'VALUE-CHANGED' TO h-cb-envia-email.
   RETURN NO-APPLY.
END.

APPLY 'choose' TO p-btok.
 
