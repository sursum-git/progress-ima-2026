DEFINE VARIABLE iTime AS INTEGER     NO-UNDO.
iTime = ETIME(TRUE).
{esapi/getPropsItem.i}
OUTPUT TO c:\temp\ITEM_props.txt.
PUT UNFORM "Item;Descri‡Æo;Largura;Gramatura;Rendimento;Composi‡Æo;" SKIP.
FOR EACH ITEM NO-LOCK 
    WHERE ITEM.ind-item-fat = YES
    AND item.ge-codigo >= 50 
    AND ITEM.ge-codigo <= 60.
    RUN esapi/getPropsItem.p(ROWID(ITEM), OUTPUT TABLE ttDados).
    PUT ITEM.it-codigo  ";" ITEM.desc-item  ";".
    FIND FIRST ttDados NO-ERROR.
    PUT ttDados.largura ";" ttDados.gramatura  ";"
         ttDados.rendimento  ";" ttdados.composicao SKIP.
END.

OUTPUT CLOSE.

DISP ETIME.
    

