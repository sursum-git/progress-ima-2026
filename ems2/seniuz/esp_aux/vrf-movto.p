DEF VAR de-qtd-etq AS DEC.
DEF VAR de-qtd-movto AS DEC.

FOR EACH movto-estoq WHERE
         movto-estoq.dt-trans >= 01.01.2007  AND 
         (movto-estoq.esp-docto = 1 OR
          movto-estoq.esp-docto = 8) NO-LOCK 
         BREAK BY movto-estoq.nr-reporte.

    FIND ITEM WHERE
         ITEM.it-codigo = movto-estoq.it-codigo NO-LOCK NO-ERROR.

    IF ITEM.ge-codigo < 51 OR
       ITEM.ge-codigo > 58 THEN NEXT.

    IF movto-estoq.tipo-trans = 1 THEN
       ASSIGN de-qtd-movto = de-qtd-movto + movto-estoq.quantidade.
    ELSE
       ASSIGN de-qtd-movto = de-qtd-movto - movto-estoq.quantidade.

    IF LAST-OF(movto-estoq.nr-reporte) THEN DO.
        ASSIGN de-qtd-etq = 0.
        FOR EACH ob-etiqueta WHERE
                 ob-etiqueta.nr-reporte = movto-estoq.nr-reporte NO-LOCK.
            ASSIGN de-qtd-etq = de-qtd-etq + ob-etiqueta.quantidade.
        END.

        IF de-qtd-etq = 0 AND de-qtd-movto <> 0 THEN
           DISP movto-estoq.nr-ord-prod
                movto-estoq.nr-reporte
                movto-estoq.dt-trans
                movto-estoq.it-codigo
                movto-estoq.cod-refer
                movto-estoq.lote
                movto-estoq.quantidade
                de-qtd-etq
                movto-estoq.quantidade - de-qtd-etq
                WITH WIDTH 550.

        ASSIGN de-qtd-movto = 0.
    END.
END.
