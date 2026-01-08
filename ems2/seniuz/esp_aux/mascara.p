/* Programa: mascara.p
**           Exemplo de uso de m scara para parƒmetros de relat¢rios.
*/

DEF VAR c-mascara AS CHAR FORMAT "x(6)".

REPEAT :
   UPDATE c-mascara.
   FOR EACH ITEM WHERE ITEM.it-codigo >= "50"
                   AND ITEM.it-codigo <= "5z"
                   AND (SUBSTR(ITEM.it-codigo,1,1) = SUBSTR(c-mascara,1,1) OR SUBSTR(c-mascara,1,1) = "*")
                   AND (SUBSTR(ITEM.it-codigo,2,1) = SUBSTR(c-mascara,2,1) OR SUBSTR(c-mascara,2,1) = "*")
                   AND (SUBSTR(ITEM.it-codigo,3,1) = SUBSTR(c-mascara,3,1) OR SUBSTR(c-mascara,3,1) = "*")
                   AND (SUBSTR(ITEM.it-codigo,4,1) = SUBSTR(c-mascara,4,1) OR SUBSTR(c-mascara,4,1) = "*")
                   AND (SUBSTR(ITEM.it-codigo,5,1) = SUBSTR(c-mascara,5,1) OR SUBSTR(c-mascara,5,1) = "*")
                   AND (SUBSTR(ITEM.it-codigo,6,1) = SUBSTR(c-mascara,6,1) OR SUBSTR(c-mascara,6,1) = "*")
                 NO-LOCK.
       DISP ITEM.it-codigo
            c-mascara.
   END.
END.
