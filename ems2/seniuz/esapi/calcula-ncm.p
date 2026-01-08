DEF INPUT  PARAMETER p-item LIKE ITEM.it-codigo.
DEF INPUT  PARAMETER p-cod-refer LIKE referencia.cod-refer. 
DEF OUTPUT PARAMETER p-ncm LIKE ITEM.class-fiscal.

FIND ITEM WHERE
     ITEM.it-codigo = p-item NO-LOCK NO-ERROR.

FIND item-ext OF ITEM NO-LOCK NO-ERROR.

FIND FIRST param-dis NO-LOCK NO-ERROR.

IF AVAIL item-ext THEN DO:
   IF SUBSTR(p-item,6,1) = '0' OR  /* Cru, Indigo ou Desenho+Variante 00010 ==> Tratado como Cru */
      item-ext.indigo OR 
      SUBSTR(p-cod-refer,3,5) = '00010' THEN
      ASSIGN p-ncm = ITEM.class-fiscal.
   ELSE DO.
      IF SUBSTR(p-cod-refer,7,1) <> '0' THEN
         ASSIGN p-ncm = item-ext.cod-ncm-est.
      ELSE DO.
         IF LOOKUP(SUBSTR(p-cod-refer,3,4),param-dis.cores-branq) > 0 THEN 
            ASSIGN p-ncm = item-ext.cod-ncm-bco. 
         ELSE
            ASSIGN p-ncm = item-ext.cod-ncm-tto. 
      END.
   END.
END.
ELSE 
   ASSIGN p-ncm = ITEM.class-fiscal.
