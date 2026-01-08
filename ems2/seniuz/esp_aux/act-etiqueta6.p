DEF VAR i-ct AS INT.
DEF VAR i-tot AS INT.

DEF TEMP-TABLE tt-aux
    FIELD num-etiqueta LIKE ob-etiqueta.num-etiqueta.

FIND LAST ob-etiqueta USE-INDEX indice4.
ASSIGN i-tot = ob-etiqueta.num-etiqueta.

DO i-ct = 1 TO i-tot.
   FIND ob-etiqueta WHERE
        ob-etiqueta.num-etiqueta = i-ct NO-LOCK NO-ERROR.

   IF NOT AVAIL ob-etiqueta THEN DO.
      CREATE tt-aux.
      ASSIGN tt-aux.num-etiqueta = i-ct.
   END.
END.

FOR EACH tt-aux.
    FIND FIRST ob-etiqueta WHERE
               ob-etiqueta.num-etiqueta = 0.
    ASSIGN ob-etiqueta.num-etiqueta = tt-aux.num-etiqueta.
END.


