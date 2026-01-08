DEF TEMP-TABLE tt-aux
    FIELD campo-1 AS CHAR
    FIELD campo-2 AS CHAR FORMAT "x(100)".

DEF VAR i AS INT.

INPUT FROM n:\especificos\esdoc\perm.csv.
    REPEAT.
       CREATE tt-aux.
       IMPORT DELIMITER ";" tt-aux.
    END.
INPUT CLOSE.

FOR EACH tt-aux BY tt-aux.campo-1.
    DO i = 1 TO NUM-ENTRIES(tt-aux.campo-2).
       FIND prog_dtsul_segur WHERE 
            prog_dtsul_segur.cod_prog_dtsul = ENTRY(1,tt-aux.campo-1,".") AND
            prog_dtsul_segur.cod_grp_usuar = ENTRY(i,tt-aux.campo-2)
            NO-LOCK NO-ERROR. 

       IF NOT AVAIL prog_dtsul_segur THEN DO.
          CREATE prog_dtsul_segur.
          ASSIGN prog_dtsul_segur.cod_prog_dtsul = ENTRY(1,tt-aux.campo-1,".")
                 prog_dtsul_segur.cod_grp_usuar = ENTRY(i,tt-aux.campo-2).
       END.
    END.
END.

