/* Fun‡Æo para converter Hora em Segundos */

DEF INPUT PARAMETER c-hora AS CHAR.
DEF OUTPUT PARAMETER i-hora AS INT.

ASSIGN i-hora = (INT(SUBSTR(c-hora,1,2)) * 3600)+
                (INT(SUBSTR(c-hora,4,2)) * 60) +
                (INT(SUBSTR(c-hora,7,2)))  

