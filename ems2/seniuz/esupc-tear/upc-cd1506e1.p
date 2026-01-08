DEF NEW GLOBAL SHARED VAR h-browse AS HANDLE NO-UNDO.

IF SELF:INPUT-VALUE <> "" THEN DO.
   IF LAST-EVENT:FUNCTION = "RETURN" THEN DO.
      FIND referencia WHERE
           referencia.cod-refer = SELF:INPUT-VALUE NO-LOCK NO-ERROR.

      IF AVAIL referencia THEN DO.
         h-browse:QUERY:REPOSITION-TO-ROW(1).
         h-browse:QUERY:REPOSITION-TO-ROWID(ROWID(referencia)).
         h-browse:SELECT-ROW(h-browse:GET-REPOSITIONED-ROW()).
        
         ASSIGN SELF:SCREEN-VALUE = "".
      END.
      ELSE
         MESSAGE "Referància n∆o Cadastrada...." VIEW-AS ALERT-BOX INFO BUTTONS OK.
   END.
END.

IF LAST-EVENT:LABEL = 'F8' THEN 
   h-browse:DESELECT-ROWS().

APPLY 'entry' TO SELF.

