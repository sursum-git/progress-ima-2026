

DEFINE TEMP-TABLE ttEtq NO-UNDO
    FIELD id            AS INT64
    FIELD itCodigo      AS CHAR
    FIELD descricao     AS CHAR
    FIELD codRefer      AS CHAR
    FIELD nrContainer   AS INTEGER
    FIELD numRolo       AS INTEGER
    FIELD idEtq         AS CHAR
    FIELD pedido        AS CHAR
    FIELD pedidoCliente AS CHAR
    FIELD prePedido     AS CHAR
    FIELD quantidade    AS DECIMAL
    FIELD codSituacao   AS CHAR
    FIELD origem        AS CHAR
    FIELD qtPeca        AS INT
    FIELD localiz       AS CHAR
    INDEX chave IS PRIMARY itCodigo codRefer nrcontainer numRolo
    INDEX unico IS UNIQUE id
    INDEX ind-etq idEtq
    .
