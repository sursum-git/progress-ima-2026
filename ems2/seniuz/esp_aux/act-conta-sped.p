DEF TEMP-TABLE tt-axi
    FIELD ccr-nova AS CHAR
    FIELD ccr-antiga AS CHAR.

INPUT FROM "m:\spedcontabil\altera‡Æo.csv".
REPEAT:
     CREATE tt-axi.
     IMPORT DELIMITER ";" tt-axi.
END.
INPUT CLOSE. 

FOR EACH tt-axi,
    EACH dwf-cta-ctbl-refer WHERE 
         dwf-cta-ctbl-refer.cod-cta-ctbl-refer = ccr-antiga.

     DISP dwf-cta-ctbl-refer.cod-cta-ctbl-refer " = "
          ccr-nova FORMAT "x(30)" WITH COL 1 WIDTH 600.      

     /*ASSIGN dwf-cta-ctbl-refer.cod-cta-ctbl-refer = ccr-nova.*/
END.
  
