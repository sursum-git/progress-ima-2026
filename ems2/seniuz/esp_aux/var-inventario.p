DEF TEMP-TABLE tt-aux
    FIELD it-codigo LIKE ped-item-res.it-codigo
    FIELD lote      LIKE ped-item-res.lote
    FIELD cod-refer LIKE ped-item-res.cod-refer
    FIELD qt-ctb    AS DEC
    FIELD vl-ctb    AS DEC
    FIELD qt-apur   AS DEC 
    FIELD vl-apur   AS DEC
    FIELD qt-var    AS DEC
    FIELD vl-var    AS DEC
    FIELD qt-reserva LIKE ped-item-res.qt-pedida
    FIELD saldo      AS DEC FORMAT "->>>,>>>,>>9.99".

INPUT FROM c:\temp\var-estoque.csv NO-ECHO.
REPEAT.
   CREATE tt-aux.
   IMPORT DELIMITER ";" tt-aux.
END.


FOR EACH tt-aux.
    FOR EACH ped-item-res WHERE
             ped-item-res.it-codigo = tt-aux.it-codigo AND
             ped-item-res.cod-refer = tt-aux.cod-refer AND
             ped-item-res.faturado = NO  NO-LOCK.
        ASSIGN tt-aux.qt-reserva = tt-aux.qt-reserva + ped-item-res.qt-pedida.
    END.
END.

OUTPUT TO m:\var-reservas.csv.
FOR EACH tt-aux.
    ASSIGN tt-aux.saldo = tt-aux.qt-apur - tt-aux.qt-reserva.
    EXPORT DELIMITER ";" tt-aux.
END.
OUTPUT CLOSE.
