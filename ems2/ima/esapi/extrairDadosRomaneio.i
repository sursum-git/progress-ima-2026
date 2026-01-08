DEFINE TEMP-TABLE tt-Romaneio NO-UNDO
    FIELD codEstabel AS CHAR
    FIELD serie      AS CHAR
    FIELD notaFiscal AS CHAR
    FIELD qtEtq      AS INT
    FIELD totVolume  AS INT 
    FIELD totPeso    AS DECIMAL
    FIELD totEtq     AS INT
    FIELD impEtq     AS INT
    FIELD totQtd     AS DECIMAL
    FIELD cPedido    AS CHAR
    FIELD cNomeAbrev AS CHAR
    FIELD cTransp    AS CHAR 
    INDEX ind-pri IS PRIMARY codEstabel serie notaFiscal                            .
    .


DEFINE TEMP-TABLE tt-itens-romaneio NO-UNDO
    FIELD codEstabel AS CHAR
    FIELD serie      AS CHAR
    FIELD notaFiscal AS CHAR
    FIELD numEtq     AS INT
    FIELD qtidade    AS DEC
    FIELD peso       AS DEC
    FIELD seq        AS INT
    FIELD idItem     AS INT 
    INDEX ind-item  idItem.

DEFINE TEMP-TABLE tt-dados-itens-romaneio NO-UNDO
    FIELD itCodigo AS CHAR
    FIELD codRefer AS CHAR
    FIELD descItem AS CHAR
    FIELD id       AS INTEGER 
    INDEX primario IS PRIMARY IS UNIQUE id .

