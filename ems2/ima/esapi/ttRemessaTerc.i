DEFINE TEMP-TABLE ttRetorno   NO-UNDO
    FIELD id            AS INT
    FIELD itCodigo      AS CHAR
    FIELD codRefer      AS CHAR
    FIELD nrNota        AS CHAR
    FIELD dtEntrada     AS DATE
    FIELD quantidade    AS DECIMAL FORMAT '->>>,>>>,>>9.9999'
    FIELD valorUnit     AS DECIMAL FORMAT '->>>,>>>,>>9.9999'
    FIELD valorTotal    AS DECIMAL FORMAT '->>>,>>>,>>9.9999'
    FIELD nfBaixada     AS CHAR
    FIELD nfERP         AS CHAR
    FIELD logLancERP    AS LOGICAL
    FIELD qtNaoBaixada  AS DECIMAL
    FIELD nrPedido      AS CHAR
    FIELD memoriaCalc   AS CHAR FORMAT 'x(2000)'
    INDEX pri IS PRIMARY id
    INDEX ret itCodigo CodRefer nrNota.
