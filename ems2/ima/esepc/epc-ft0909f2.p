DEF NEW GLOBAL SHARED VAR h-browse AS WIDGET-HANDLE NO-UNDO.

DEF NEW GLOBAL SHARED VAR c-chave AS CHAR.

DEF VAR hQuery   AS HANDLE.
DEF VAR hBuffer  AS HANDLE.

ASSIGN hQuery = h-browse:QUERY:HANDLE.
ASSIGN hbuffer = hQuery:GET-BUFFER-HANDLE(1).
    
hquery:GET-FIRST().

IF hbuffer:AVAIL THEN DO.
   ASSIGN c-chave = hbuffer:BUFFER-FIELD(1):BUFFER-VALUE +
                    hbuffer:BUFFER-FIELD(2):BUFFER-VALUE +
                    hbuffer:BUFFER-FIELD(3):BUFFER-VALUE + "_" +
                    hbuffer:BUFFER-FIELD(4):BUFFER-VALUE + "_CCe.xml".

   RUN esp/esft0909f.w.
END.
