DEFINE TEMP-TABLE tt
    FIELD codigo AS INT
    FIELD qt     AS INT.

FOR EACH docum-est
    WHERE dt-trans >= TODAY - 720.
    FIND tt
        WHERE tt.codigo = docum.cod-emitente
        NO-ERROR.
    IF NOT AVAIL tt THEN DO:
        CREATE tt.
        ASSIGN tt.codigo = nota-fiscal.cod-emitente.
    END.
    ASSIGN tt.qt = qt + 1.
           /*tt.valor = tt.valor + nota-fiscal.vl-tot-nota.*/
END.
OUTPUT TO c:\temp\qt_fornecedores.txt.
FOR EACH tt.
    EXPORT DELIMITER "|" tt.
END.
