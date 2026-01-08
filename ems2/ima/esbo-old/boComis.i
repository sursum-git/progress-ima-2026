DEFINE TEMP-TABLE ttPedItem
    FIELD itCodigo LIKE ITEM.it-codigo
    FIELD codRefer AS CHAR
    FIELD logOutLet AS LOGICAL
    FIELD vlInf     AS DECIMAL
    FIELD vlTb      AS DECIMAL
    FIELD vlOutlet  AS DECIMAL
    FIELD qtPedida  AS DECIMAL.

DEFINE TEMP-TABLE ttCondPed LIKE tt-cond-ped.

