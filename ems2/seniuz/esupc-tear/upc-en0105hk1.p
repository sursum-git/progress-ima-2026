DEF VAR h-query AS HANDLE.
DEF VAR h-buffer AS HANDLE.
DEF VAR h-first-col  AS HANDLE.
DEF NEW GLOBAL SHARED VAR v-search AS CHAR.

ASSIGN h-query = SELF:QUERY.
ASSIGN v-search = v-search + KEYFUNCTION(LASTKEY).

h-query:GET-CURRENT() NO-ERROR.
IF LENGTH(v-search) = 1 THEN
   h-query:GET-FIRST() NO-ERROR.

ASSIGN h-buffer = h-query:GET-BUFFER-HANDLE(1)
       h-first-col = h-buffer:BUFFER-FIELD(1).

REPEAT. 
   IF h-query:QUERY-OFF-END OR
      h-first-col:BUFFER-VALUE = v-search THEN DO.
      h-query:REPOSITION-TO-ROWID(h-buffer:ROWID) NO-ERROR.
      ASSIGN v-search = "".
      LEAVE.
   END.

   IF SUBSTR(h-first-col:BUFFER-VALUE,1,LENGTH(v-search)) = v-search THEN DO.
      h-query:REPOSITION-TO-ROWID(h-buffer:ROWID) NO-ERROR.
      LEAVE.
   END.

   h-query:GET-NEXT() NO-ERROR.
   ASSIGN h-buffer = h-query:GET-BUFFER-HANDLE(1)
          h-first-col = h-buffer:BUFFER-FIELD(1).
END.
