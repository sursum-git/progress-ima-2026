DEFINE TEMP-TABLE ttItem
    FIELD itcodigo      LIKE ITEM.it-codigo
    FIELD descItem      LIKE ITEM.desc-item
    FIELD codRefer      AS CHAR
    FIELD nrContainer   AS INT 
    FIELD vlTbPadrao    AS DECIMAL EXTENT 4
    FIELD vlOutlet      AS DECIMAL EXTENT 2
    FIELD vlTbRubi      AS DECIMAL EXTENT 4
    FIELD vlTbRubix     AS DECIMAL EXTENT 2           
    INDEX primario IS PRIMARY itCodigo codRefer nrContainer.
