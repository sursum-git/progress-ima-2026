DEFINE TEMP-TABLE tt
    FIELD codigo AS INT
    FIELD qt     AS INT
    FIELD valor  AS DECIMAL.

FOR EACH nota-fiscal
    WHERE dt-emis-nota >= TODAY - 720
    AND dt-cancel = ?.
    FIND tt
        WHERE tt.codigo = nota-fiscal.cod-emitente
        NO-ERROR.
    IF NOT AVAIL tt THEN DO:
        CREATE tt.
        ASSIGN tt.codigo = nota-fiscal.cod-emitente.
    END.
    ASSIGN tt.qt = qt + 1
           tt.valor = tt.valor + nota-fiscal.vl-tot-nota.
END.
OUTPUT TO c:\temp\qt_clientes.txt.
FOR EACH tt.
    EXPORT DELIMITER "|" tt.
END.
