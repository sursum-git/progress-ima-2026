DEFINE TEMP-TABLE tt LIKE referencia
  INDEX ordem int-2.
FOR EACH referencia.
    CREATE tt.
    BUFFER-COPY referencia TO tt.
END.

FOR EACH tt USE-INDEX ordem.
    DISP tt.cod-refer . 
END.
