DEFINE PARAMETER BUFFER p-table FOR ref-item.
DEFINE PARAMETER BUFFER p-table-old FOR ref-item.

DEF VAR i-list-fat  AS INT EXTENT 13 INIT[6,5,4,3,2,9,8,7,6,5,4,3,2].
DEF VAR i-digito    AS INT FORMAT "9" LABEL "DV".
DEF VAR i-cont      AS INT.
DEF VAR i-soma      AS INT.
DEF VAR c-item-ref  AS CHAR FORMAT "x(13)".

FIND ref-item-ext WHERE
     ref-item-ext.it-codigo = p-table.it-codigo AND
     ref-item-ext.cod-refer = p-table.cod-refer NO-ERROR.

IF NOT AVAIL ref-item-ext THEN DO:
   CREATE ref-item-ext.
   ASSIGN ref-item-ext.it-codigo = p-table.it-codigo
          ref-item-ext.cod-refer = p-table.cod-refer.
   /* Gilvando - 14.10.2009
   FIND ITEM WHERE ITEM.it-codigo = p-table.it-codigo NO-LOCK NO-ERROR.
   IF AVAIL ITEM THEN
      ASSIGN ref-item-ext.cod-ncm = ITEM.class-fiscal.
   */
   ASSIGN c-item-ref = p-table.it-codigo + p-table.cod-refer.
   ASSIGN i-soma = 0.

    DO i-cont = 1 TO 13:
       IF SUBSTR(c-item-ref,i-cont,1) >= "0" AND
          SUBSTR(c-item-ref,i-cont,1) <= "9" THEN
          ASSIGN i-soma = i-soma + INT(SUBSTR(c-item-ref,i-cont,1)) * i-list-fat[i-cont].
    END.
    ASSIGN i-digito = 11 - (i-soma MODULO 11).
    IF i-digito > 9 THEN
       ASSIGN i-digito = 0.

    ASSIGN ref-item-ext.dv = STRING(i-digito,"9").
END.

FIND referencia-ext WHERE
     referencia-ext.cod-refer = p-table.cod-refer NO-LOCK NO-ERROR.
IF AVAIL referencia-ext THEN 
   ASSIGN ref-item-ext.cod-obsoleto = referencia-ext.cod-obsoleto
          ref-item-ext.colecao = referencia-ext.colecao
          ref-item-ext.cod-fundo = referencia-ext.cod-fundo 
          ref-item-ext.cor = referencia-ext.cor.



