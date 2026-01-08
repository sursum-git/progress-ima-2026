/* Programa: imp-doc-fiscal.p
** Objetivo: Importar acerto em documentos fiscais. 
*/

DEF VAR i-cont-err AS INT.

DEF TEMP-TABLE tt-doc-fiscal
    FIELD cod-estabel  AS CHAR
    FIELD serie        AS CHAR
    FIELD nr-doc-fis   AS CHAR 
    FIELD cod-emitente AS CHAR
    FIELD dt-emissao   AS CHAR
    FIELD it-codigo    AS CHAR
    FIELD narrativa    AS CHAR 
    FIELD cod-novo     AS CHAR.
    
input from "c:/lixo/fiscal.csv".
SET ^.

repeat:
   create tt-doc-fiscal.
   import delimiter ";" tt-doc-fiscal.
end.
input close.

FOR EACH tt-doc-fiscal:
    IF tt-doc-fiscal.cod-estabel = "" THEN NEXT.
    FIND FIRST doc-fiscal WHERE doc-fiscal.cod-estabel  = tt-doc-fiscal.cod-estabel
                            AND doc-fiscal.serie        = tt-doc-fiscal.serie
                            AND doc-fiscal.nr-doc-fis   = STRING(INT(tt-doc-fiscal.nr-doc-fis),"9999999")
                            AND doc-fiscal.cod-emitente = int(tt-doc-fiscal.cod-emitente)
                          NO-LOCK NO-ERROR.
    IF NOT AVAIL doc-fiscal THEN
       DISP tt-doc-fiscal.cod-estabel                       
            tt-doc-fiscal.serie                            
            STRING(int(tt-doc-fiscal.nr-doc-fis),"9999999")
            int(tt-doc-fiscal.cod-emitente).
    ELSE DO:
       FOR EACH it-doc-fisc OF doc-fiscal 
           WHERE it-doc-fisc.it-codigo = "" OR          
                 it-doc-fisc.it-codigo = "," OR         
                 it-doc-fisc.it-codigo = "0000000" OR   
                 it-doc-fisc.it-codigo = "0000006" OR   
                 it-doc-fisc.it-codigo = "4693".
           DISP tt-doc-fiscal.cod-novo
            CAN-FIND(ITEM WHERE ITEM.it-codigo = tt-doc-fiscal.cod-novo).
           /*
           ASSIGN it-doc-fisc.it-codigo = tt-doc-fiscal.cod-novo.
           */
       END.
    END.
END.
