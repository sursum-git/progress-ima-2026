FIND nota-fiscal WHERE
     nota-fiscal.cod-estabel = '1' AND
     nota-fiscal.serie = '3' AND
     nota-fiscal.nr-nota-fis = '0046198'.

FOR EACH it-nota-fisc OF nota-fiscal.
    FIND item WHERE
         item.it-codigo = it-nota-fisc.it-codigo NO-LOCK NO-ERROR.

    DISP it-nota-fisc.it-codigo
         item.class-fiscal  COLUMN-LABEL 'Item'
         it-nota-fisc.class-fiscal COLUMN-LABEL 'Item da Nota'.

    ASSIGN it-nota-fisc.class-fiscal = ITEM.class-fiscal.

END.
