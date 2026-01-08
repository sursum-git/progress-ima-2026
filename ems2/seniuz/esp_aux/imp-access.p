/* Programa: imp-access.p
** Objetivo: Importar o log de acesso … internet e exportar para excel. 
*/

DEF TEMP-TABLE tt-access
    FIELD campo1        AS CHAR
    FIELD campo2        AS CHAR 
    FIELD campo3        AS CHAR 
    FIELD campo4        AS CHAR 
    FIELD campo5        AS CHAR  
    FIELD campo6        AS CHAR  
    FIELD campo7        AS CHAR  
    FIELD campo8        AS CHAR  
    FIELD campo9        AS CHAR 
    FIELD campo10       AS CHAR.
    
input from "c:/lixo/access.log".
SET ^.

repeat:
   create tt-access.
   import delimiter " " tt-access.
end.
input close.

OUTPUT TO "c:/lixo/access.txt".
FOR EACH tt-access WHERE tt-access.campo8 = "giselesouza"
                   /*AND tt-access.campo3 = "192.168.0.88"*/:
    IF tt-access.campo1 = "" THEN NEXT.
    EXPORT DELIMITER " " tt-access.
    /*
    DISP tt-access WITH SIDE-LABELS 1 COLUMN WIDTH 128.
    */
END.
OUTPUT CLOSE.
   
