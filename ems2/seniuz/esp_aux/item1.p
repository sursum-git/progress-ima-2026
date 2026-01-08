DEF VAR l-suspeito AS LOG.
FOR EACH ITEM WHERE ITEM.it-codigo >= "50"
                AND ITEM.it-codigo <= "50z"
              NO-LOCK.
    ASSIGN l-suspeito = NO.

    IF (SUBSTR(ITEM.it-codigo,6,1) = "0" OR SUBSTR(ITEM.it-codigo,6,1) = "9") THEN DO:
       IF ITEM.tipo-con-est <> 1 OR 
          ITEM.ind-imp-desc <> 1 THEN
          ASSIGN l-suspeito = YES.
    END.

    IF (SUBSTR(ITEM.it-codigo,6,1) >= "1" AND SUBSTR(ITEM.it-codigo,6,1) <= "8") THEN DO:
       IF ITEM.ind-imp-desc <> 4 OR 
          ITEM.tipo-con-est <> 4 OR 
          ITEM.un <> "M" THEN
          ASSIGN l-suspeito = YES.
    END.

    IF (SUBSTR(ITEM.it-codigo,6,1) = "9") AND
       (ITEM.tipo-con-est <> 1 OR ITEM.un <> "Kg") THEN
       ASSIGN l-suspeito = YES.

    IF l-suspeito THEN
    DISP ITEM.it-codigo
         ITEM.descricao-1
         ITEM.un
         ITEM.tipo-con-est VIEW-AS FILL-IN
         ITEM.ind-imp-desc VIEW-AS FILL-IN.
END.


/* Corre‡Æo */
/*
FOR EACH ITEM WHERE ITEM.it-codigo >= "50"
                AND ITEM.it-codigo <= "509".

    IF (SUBSTR(ITEM.it-codigo,6,1) = "0" OR SUBSTR(ITEM.it-codigo,6,1) = "9") AND
       ITEM.tipo-con-est <> 1 THEN
       ASSIGN ITEM.tipo-con-est = 1.

    IF (SUBSTR(ITEM.it-codigo,6,1) >= "1" AND SUBSTR(ITEM.it-codigo,6,1) <= "8") AND
       ITEM.tipo-con-est <> 4 THEN
       ASSIGN ITEM.tipo-con-est = 4.
END.
*/
