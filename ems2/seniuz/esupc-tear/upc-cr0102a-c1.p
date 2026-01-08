DEF INPUT PARAMETER p-h-browse AS HANDLE NO-UNDO.

DEF NEW GLOBAL SHARED VAR wh-max-30  AS WIDGET-HANDLE NO-UNDO.

DEF VAR h-buffer AS HANDLE.
DEF VAR h-browse AS HANDLE.
DEF VAR h-query AS HANDLE.
DEF VAR h-nome-abrev AS HANDLE.

ASSIGN h-browse = p-h-browse.
ASSIGN h-query = h-browse:QUERY.

IF wh-max-30:SCREEN-VALUE <> '3' THEN DO.
   h-query:GET-FIRST NO-ERROR. 
   REPEAT ON ERROR UNDO, LEAVE:
      IF h-query:QUERY-OFF-END THEN LEAVE.
    
      ASSIGN h-buffer = h-query:GET-BUFFER-HANDLE(1)
             h-nome-abrev = h-buffer:BUFFER-FIELD(5).
        
      FIND emitente WHERE
           emitente.nome-abrev = h-nome-abrev:BUFFER-VALUE NO-LOCK NO-ERROR.
    
      IF (wh-max-30:SCREEN-VALUE = '1' AND LENGTH(emitente.nome-emit) > 30) OR
         (wh-max-30:SCREEN-VALUE = '2' AND LENGTH(emitente.nome-emit) <= 30) THEN DO.
          h-query:REPOSITION-TO-ROW(h-query:CURRENT-RESULT-ROW). 
          h-browse:SELECT-FOCUSED-ROW().        
          h-query:DELETE-RESULT-LIST-ENTRY().
          h-browse:DESELECT-ROWS().        
      END.
      h-query:GET-NEXT() NO-ERROR.
   END.
   h-query:GET-FIRST NO-ERROR.
   h-query:REPOSITION-TO-ROW(h-query:CURRENT-RESULT-ROW). 
END.

