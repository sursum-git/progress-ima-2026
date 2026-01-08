DEFINE TEMP-TABLE ttItem NO-UNDO
    FIELD itCodigo      AS CHAR
    FIELD codRefer      AS CHAR
    FIELD quantidade    AS DECIMAL.

    
FOR EACH nota-fiscal NO-LOCK
    WHERE nota-fiscal.dt-emis-nota >= 05.28.2024
    AND nota-fiscal.cod-estabel = '505'
    AND nota-fiscal.dt-cancel = ?.
    FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK.
        FIND ttItem
            WHERE ttitem.itCodigo = it-nota-fisc.it-codigo
            AND ttItem.codRefer = it-nota-fisc.cod-refer
            NO-LOCK NO-ERROR.
        IF NOT AVAIL ttItem THEN DO:
           CREATE ttItem.
           ASSIGN ttItem.itCodigo  = it-nota-fisc.it-codigo
                   ttItem.codRefer = it-nota-fisc.cod-refer.
        END.
        ASSIGN ttItem.quantidade = it-nota-fisc.qt-faturada[1].
    END.
END.
OUTPUT TO c:\temp\ttItem.txt.

FOR EACH ttItem:
    DISP ttItem.
END.
