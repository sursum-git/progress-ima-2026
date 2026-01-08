DEFINE TEMP-TABLE ttQtItemRef NO-UNDO
    FIELD id                AS INT
    FIELD retornoLisaId     AS INT   COLUMN-LABEL "ID RETORNO"
    FIELD itCodigo          AS CHAR  COLUMN-LABEL "Produto"
    FIELD codRefer          AS CHAR  COLUMN-LABEL "Referˆncia"
    FIELD qt                AS DECIMAL COLUMN-LABEL "Qte."
    INDEX pri IS PRIMARY retornoLisaId itCodigo codRefer
    INDEX ind-id id.
