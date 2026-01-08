DEF VAR l-indigo AS CHAR FORMAT "X".
OUTPUT TO m:\prod_x_corte-jan.txt.

FOR EACH ob-etiqueta WHERE
         ob-etiqueta.dt-emissao >= 01.01.2010 AND
         ob-etiqueta.dt-emissao <= 01.31.2010 AND
         ob-etiqueta.situacao   >= 2 NO-LOCK
         BREAK BY ob-etiqueta.it-codigo
               BY ob-etiqueta.nr-lote
               BY ob-etiqueta.corte-comerc.

    ACCUMULATE ob-etiqueta.quantidade (TOTAL BY ob-etiqueta.it-codigo).
    ACCUMULATE ob-etiqueta.quantidade (TOTAL BY ob-etiqueta.nr-lote).
    ACCUMULATE ob-etiqueta.quantidade (TOTAL BY ob-etiqueta.corte-comerc).

    FIND item-ext WHERE
         item-ext.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.
    ASSIGN l-indigo = ''.
    IF AVAIL item-ext AND item-ext.indigo THEN
       ASSIGN l-indigo = 'X'.

    FIND corte-comerc WHERE
         corte-comerc.codigo = ob-etiqueta.corte-comerc NO-LOCK NO-ERROR.

    FIND ITEM WHERE
         ITEM.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.

    IF FIRST-OF(ob-etiqueta.it-codigo) THEN 
       PUT "JAN/2010"                           AT 1
           ob-etiqueta.it-codigo FORMAT "X(8)"  AT 10
           ITEM.desc-item        FORMAT "x(40)" AT 20.

    IF FIRST-OF(ob-etiqueta.nr-lote) THEN
       PUT ob-etiqueta.nr-lote   FORMAT "x(3)"  AT 65.

    IF FIRST-OF(ob-etiqueta.corte-comerc) THEN
       PUT corte-comerc.descricao FORMAT "x(20)" AT 70.

    /*
    IF LAST-OF(ob-etiqueta.nr-lote) THEN
       PUT (ACCUM TOTAL BY ob-etiqueta.nr-lote ob-etiqueta.quantidade) AT 85
           SKIP.
    */   

    IF LAST-OF(ob-etiqueta.corte-comerc) THEN
       PUT (ACCUM TOTAL BY ob-etiqueta.corte-comerc ob-etiqueta.quantidade) AT 95
           SKIP.
         
    IF LAST-OF(ob-etiqueta.it-codigo) THEN DO.
       PUT 'Total' AT 15
            (ACCUM TOTAL BY ob-etiqueta.it-codigo ob-etiqueta.quantidade) AT 95
           SKIP(1).
    END.
END.

OUTPUT CLOSE.
