DEF TEMP-TABLE tt-aux
    FIELD nr-reporte LIKE ob-etiqueta.nr-reporte
    FIELD qtd-etq AS DEC
    FIELD qtd-mov AS DEC.

FOR EACH ob-etiqueta WHERE
         ob-etiqueta.dt-emis >= 04.01.2010 /*AND
         ob-etiqueta.it-codigo = '500272'  */ NO-LOCK
         BREAK BY ob-etiqueta.nr-reporte.

    IF ob-etiqueta.tipo-ordem <> 1 THEN NEXT.
    IF ob-etiqueta.situacao    < 3 THEN NEXT.
    IF ob-etiqueta.nr-lote BEGINS 'CA' THEN NEXT.

    FIND tt-aux WHERE
         tt-aux.nr-reporte = ob-etiqueta.nr-reporte NO-ERROR.
    IF NOT AVAIL tt-aux THEN DO.
       CREATE tt-aux.
       ASSIGN tt-aux.nr-reporte = ob-etiqueta.nr-reporte.
    END.
    ASSIGN tt-aux.qtd-etq = tt-aux.qtd-etq + ob-etiqueta.qtd-orig.

    IF LAST-OF(ob-etiqueta.nr-reporte) THEN DO.
        FOR EACH movto-estoq WHERE
                 movto-estoq.nr-reporte = ob-etiqueta.nr-reporte AND
                 movto-estoq.esp-docto = 1 NO-LOCK.
        
            IF movto-estoq.lote BEGINS 'CA' THEN NEXT.
        
            ASSIGN tt-aux.qtd-mov = tt-aux.qtd-mov + movto-estoq.quantidade.
        END.
    END.
END.

FOR EACH tt-aux WHERE
         tt-aux.qtd-etq <> tt-aux.qtd-mov NO-LOCK.
    DISP tt-aux.
END.
