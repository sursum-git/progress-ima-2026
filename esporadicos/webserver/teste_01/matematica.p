
   DEFINE INPUT  PARAMETER n1        AS DECIMAL     NO-UNDO.
   DEFINE INPUT  PARAMETER n2        AS DECIMAL     NO-UNDO.
   DEFINE INPUT  PARAMETER operacao  AS CHAR        NO-UNDO.
   DEFINE OUTPUT PARAMETER resultado AS DECIMAL     NO-UNDO.

   CASE operacao:
       WHEN '+' THEN 
           ASSIGN resultado = n1 + n2.
      WHEN '-' THEN 
           ASSIGN resultado = n1 - n2.
      WHEN '*' THEN 
           ASSIGN resultado = n1 * n2.
     WHEN '/' THEN 
           ASSIGN resultado = n1 / n2.
     WHEN ':' THEN 
           ASSIGN resultado = n1 / n2.
   END CASE.



