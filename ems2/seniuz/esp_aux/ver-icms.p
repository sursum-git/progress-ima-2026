DEF TEMP-TABLE tt-aux
    FIELD cfop AS CHAR
    FIELD uf AS CHAR
    FIELD vl-contab LIKE doc-fiscal.vl-cont-doc.

def var h-acomp as handle no-undo.
run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Pesquisando *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

FOR EACH doc-fiscal WHERE
         doc-fiscal.dt-docto >= 10.01.2005 AND
         doc-fiscal.dt-docto <= 09.30.2006 NO-LOCK.

    RUN pi-acompanhar IN h-acomp (INPUT "Data: " + STRING(doc-fiscal.dt-docto)).

    FIND natur-oper WHERE
         natur-oper.nat-operacao = doc-fiscal.nat-oper 
         NO-LOCK NO-ERROR.

    IF SUBSTR(natur-oper.char-1,45,4) <> '2101' THEN NEXT. 

    FIND FIRST tt-aux WHERE
               tt-aux.cfop = SUBSTR(natur-oper.char-1,45,4) AND
               tt-aux.uf = doc-fiscal.estado
               NO-LOCK NO-ERROR.
    IF NOT AVAIL tt-aux THEN DO.
        CREATE tt-aux.
        ASSIGN tt-aux.cfop = SUBSTR(natur-oper.char-1,45,4)
               tt-aux.uf = doc-fiscal.estado.
    END.
    ASSIGN tt-aux.vl-contab = tt-aux.vl-contab + doc-fiscal.vl-cont-doc.

END.
run pi-finalizar in h-acomp.


OUTPUT TO PRINTER.
FOR EACH tt-aux WHERE tt-aux.cfop = '2101'.
    DISP tt-aux.cfop
         tt-aux.uf
         tt-aux.vl-contab (TOTAL).
END.

OUTPUT CLOSE.
