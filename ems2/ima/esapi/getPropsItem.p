{esapi/getPropsitem.i}
DEFINE INPUT  PARAMETER pRowid AS ROWID       NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR ttDados.


FIND ITEM NO-LOCK
    WHERE ROWID(ITEM) = pRowid NO-ERROR.
IF AVAIL ITEM THEN DO:
    FIND item-ext OF ITEM NO-LOCK NO-ERROR.
    CREATE ttDados.
    ASSIGN ttDados.largura      = REPLACE( ENTRY(2,ENTRY(5,item.narrativa,CHR(10)),":") ,CHR(13),'') 
           ttDados.gramatura    = IF AVAIL item-ext THEN DECIMAL(TRIM(STRING(item-ext.gramatura,">>9"))) ELSE 0
           ttDados.paisOrigem   = REPLACE(ENTRY(2,ENTRY(4,ITEM.narrativa,CHR(10)),":"),CHR(13),'') 
           ttDados.composicao   = REPLACE(ENTRY(2,ENTRY(6,ITEM.narrativa,CHR(10)),":"),CHR(13),'') 
           ttDados.rendimento   = REPLACE(ENTRY(2,ENTRY(7,ITEM.narrativa,CHR(10)),":"),CHR(13),'') NO-ERROR .

END.


