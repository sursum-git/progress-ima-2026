DEFINE VARIABLE d AS DECIMAL     NO-UNDO.
FOR EACH nota-fiscal
    WHERE nota-fiscal.dt-emis-nota 
     >= 12.05.2016
    AND dt-cancela = ? NO-LOCK.
    FIND FIRST ped-venda-ext 
        WHERE ped-venda-ext.nr-pedido = INT(nota-fiscal.nr-pedcli)
        AND   ped-venda-ext.cod-estabel = nota-fiscal.cod-estabel
        AND   ped-venda-ext.nr-container = 152816
        NO-LOCK NO-ERROR.
    IF NOT AVAIL ped-venda-ext THEN NEXT.
    FOR EACH it-nota-fisc OF nota-fiscal 
        WHERE it-codigo = '530340' NO-LOCK:
        ASSIGN d = d + it-nota-fisc.qt-faturada[1].
    END.
END.

DISP d.
