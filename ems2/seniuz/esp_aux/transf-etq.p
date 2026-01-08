DEF BUFFER b-etiqueta FOR ob-etiqueta.

DEF TEMP-TABLE tt-aux
    FIELD num-etiqueta LIKE ob-etiqueta.num-etiqueta
    FIELD etq-nova LIKE ob-etiqueta.num-etiqueta.

DEF VAR c-linha AS CHAR FORMAT "x(70)".

INPUT FROM c:\temp\etq-transf.txt NO-ECHO.
REPEAT.
   CREATE tt-aux.
   IMPORT DELIMITER ";" tt-aux.
END.
INPUT CLOSE.

FOR EACH tt-aux.

    FIND ob-etiqueta where
         ob-etiqueta.cod-estab = '2' AND 
         ob-etiqueta.num-etiqueta = tt-aux.num-etiqueta NO-ERROR.

    IF NOT AVAIL ob-etiqueta THEN NEXT.

    
    MESSAGE tt-aux.num-etiqueta
           tt-aux.etq-nova
           VIEW-AS ALERT-BOX INFO BUTTONS OK.



    /*
    CREATE b-etiqueta.
    BUFFER-COPY ob-etiqueta TO b-etiqueta
        ASSIGN b-etiqueta.cod-estab = '1'
               b-etiqueta.num-etiqueta = NEXT-VALUE(seq-etq-estab1)
               b-etiqueta.situacao = 3
              SUBSTR(b-etiqueta.char-1,1330,1) ="T".
    */

    /*
    RUN esapi/imp-etq-estoque.p (INPUT b-etiqueta.num-etiqueta,
                                 INPUT NO).

    IF ob-etiqueta.situacao <> 5 THEN
       ASSIGN ob-etiqueta.situacao = 5.
    */   
END.


