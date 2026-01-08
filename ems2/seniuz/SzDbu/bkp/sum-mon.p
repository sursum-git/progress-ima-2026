DEF VAR i-ct AS INT.
DO i-ct = 1 TO NUM-DBS.
   CREATE ALIAS DICTDB FOR DATABASE VALUE(LDBNAME(i-ct)) NO-ERROR.

   RUN /usr/suporte/szdbu/sum-kill.r.
END.


