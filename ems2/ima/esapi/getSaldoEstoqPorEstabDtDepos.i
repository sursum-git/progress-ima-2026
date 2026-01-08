DEFINE TEMP-TABLE ttQt NO-UNDO
    FIELD itCodigo     LIKE ITEM.it-codigo
    FIELD codRefer     AS CHAR
    FIELD qtSaldoAtu   AS  DECIMAL FORMAT "->>>,>>>,>>9.99"
    FIELD qtEntradaPos AS  DECIMAL FORMAT "->>>,>>>,>>9.99"
    FIELD qtSaidaPos   AS  DECIMAL FORMAT "->>>,>>>,>>9.99"
    FIELD qtSaldoData  AS  DECIMAL FORMAT "->>>,>>>,>>9.99"
    .
