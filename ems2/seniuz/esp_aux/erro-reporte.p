DEF VAR de-qt-etq AS DEC.

FOR EACH ob-etiqueta WHERE
         ob-etiqueta.dt-emissao = 03.30.2006 AND
         ob-etiqueta.it-codigo = '502971' AND
         ob-etiqueta.nr-lote = 'RP' NO-LOCK
         BREAK BY ob-etiquet.nr-reporte.

    /*
    ASSIGN de-qt-etq = de-qt-etq + ob-etiqueta.quantidade.

    IF LAST-OF(ob-etiqueta.nr-reporte) THEN DO.
       FIND movto-estoq WHERE
            movto-estoq.dt-trans = 03.30.2006 AND
            movto-estoq.it-codigo = '502971' AND
            movto-estoq.nr-reporte = ob-etiqueta.nr-reporte 
            NO-LOCK.

       DISP movto-estoq.nr-reporte
            de-qt-etq
            movto-estoq.quantidade.
    END.
    */
END.


