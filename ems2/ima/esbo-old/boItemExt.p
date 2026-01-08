/**********************************************
BO Item Ext
programa:boItemExt.p
Objetivo: Manuten‡Æo da tabela extendida de item
Autor: Tadeu Silva
Data: 06/2020
*************************************************/
DEFINE VARIABLE cItem LIKE ITEM.it-codigo  NO-UNDO.
DEFINE VARIABLE rRowid AS ROWID       NO-UNDO.

PROCEDURE setItem:
    DEFINE INPUT  PARAMETER pItem LIKE cItem   NO-UNDO.
    ASSIGN cItem  = pItem.
    FIND item-ext
        WHERE item-ext.it-codigo = cItem
        NO-LOCK NO-ERROR.
    IF NOT AVAIL item-ext THEN
       RUN criarItemExt(cItem,OUTPUT rRowid).
    ELSE
       ASSIGN rRowid = ROWID(item-ext).

END PROCEDURE.

PROCEDURE setTipoDesign:

    DEFINE INPUT  PARAMETER iTipo AS INTEGER     NO-UNDO.
    FIND item-ext
        WHERE ROWID(item-ext) = rRowid EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL item-ext THEN DO:
       ASSIGN item-ext.cod_tipo_item = iTipo.
    END.
    FIND CURRENT item-ext NO-LOCK NO-ERROR.



END PROCEDURE.

PROCEDURE setformaExibicao:

    DEFINE INPUT  PARAMETER iForma AS INTEGER     NO-UNDO.
    FIND item-ext
        WHERE ROWID(item-ext) = rRowid EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL item-ext THEN DO:
       ASSIGN item-ext.cod_form_exib_ref = iForma.
    END.
    FIND CURRENT item-ext NO-LOCK NO-ERROR.


END PROCEDURE.

PROCEDURE criarItemExt:

    DEFINE INPUT  PARAMETER pItem LIKE cItem      NO-UNDO.
    DEFINE OUTPUT PARAMETER pRowid AS ROWID       NO-UNDO.
    CREATE item-ext.
    ASSIGN item-ext.it-codigo = pItem
           pRowid = ROWID(item-ext).
    

END PROCEDURE.


