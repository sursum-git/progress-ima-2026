DEF NEW GLOBAL SHARED VAR h-browse AS HANDLE NO-UNDO.

DEF VAR h-cod-refer AS WIDGET-HANDLE NO-UNDO.
DEF VAR h-query       AS WIDGET-HANDLE NO-UNDO.
DEF VAR h-buffer      AS WIDGET-HANDLE NO-UNDO.

DEF VAR i-ct AS INT.

ASSIGN h-query  = h-browse:QUERY.
DO i-ct = 1 TO h-browse:NUM-SELECTED-ROWS.
   IF h-browse:FETCH-SELECTED-ROW(i-ct) THEN DO.
       ASSIGN h-buffer = h-query:GET-BUFFER-HANDLE(1)
              h-cod-refer = h-buffer:BUFFER-FIELD(1).

       MESSAGE h-cod-refer:BUFFER-VALUE
           VIEW-AS ALERT-BOX INFO BUTTONS OK.

   END.
END.

