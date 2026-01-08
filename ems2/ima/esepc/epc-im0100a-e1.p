DEF INPUT PARAMETER h-browse AS WIDGET-HANDLE.

DEF NEW GLOBAL SHARED VAR h-bt-altera AS HANDLE.
DEF NEW GLOBAL SHARED VAR h-bt-save   AS HANDLE.

DEF VAR h-query       AS HANDLE.
DEF VAR h-buffer      AS HANDLE.
DEF VAR h-refer       AS HANDLE.
DEF VAR h-lote        AS HANDLE.
DEF VAR h-dt-val-lote AS HANDLE.         

DEF VAR i-row AS INT.

ASSIGN h-query = h-browse:QUERY.    
ASSIGN h-buffer = h-query:GET-BUFFER-HANDLE(1).

ASSIGN i-row = 0.
IF h-query:NUM-RESULTS > 0 THEN DO.
    ASSIGN h-refer = h-buffer:BUFFER-FIELD(9)
           h-lote = h-buffer:BUFFER-FIELD(7)
           h-dt-val-lote = h-buffer:BUFFER-FIELD(8).
    
    h-query:GET-FIRST() NO-ERROR.
    REPEAT WHILE NOT h-query:QUERY-OFF-END.
       ASSIGN h-lote:BUFFER-VALUE = h-refer:BUFFER-VALUE
              h-dt-val-lote:BUFFER-VALUE = DATE("31/12/9999") .
    
       ASSIGN i-row = i-row + 1.
       h-query:REPOSITION-TO-ROW(i-row).
       APPLY "VALUE-CHANGED" TO h-browse.
       APPLY "CHOOSE" TO h-bt-altera.
       APPLY "CHOOSE" TO h-bt-save.

       h-query:GET-NEXT() NO-ERROR.
    END.

    h-browse:REFRESH().
END.

