/* Programa: imp-doc-fiscal2.p
** Objetivo: Importar acerto em documentos fiscais. 
*/

DEF VAR i-cont-err AS INT.

DEF TEMP-TABLE tt-doc-fiscal
    FIELD cod-estabel  AS CHAR
    FIELD serie        AS CHAR
    FIELD nr-doc-fis   AS CHAR 
    FIELD cod-emitente AS CHAR
    FIELD nat-operacao AS CHAR
    FIELD dt-emissao   AS CHAR
    FIELD it-codigo    AS CHAR
    FIELD nr-seq-doc   AS CHAR.
    
input from "c:/lixo/doc-fisc_ok.csv".
SET ^.

repeat:
   create tt-doc-fiscal.
   import delimiter ";" tt-doc-fiscal.
end.
input close.

FOR EACH tt-doc-fiscal:
    IF tt-doc-fiscal.cod-estabel = "" THEN NEXT.
    FIND it-doc-fisc WHERE it-doc-fisc.cod-estabel  = tt-doc-fiscal.cod-estabel
                       AND it-doc-fisc.serie        = tt-doc-fiscal.serie
                       AND it-doc-fisc.nr-doc-fis   = tt-doc-fiscal.nr-doc-fis
                       AND it-doc-fisc.cod-emitente = int(tt-doc-fiscal.cod-emitente)
                       AND it-doc-fisc.nat-operacao = tt-doc-fiscal.nat-operacao
                       AND it-doc-fisc.nr-seq-doc   = int(tt-doc-fiscal.nr-seq-doc)
                     NO-ERROR.
    IF NOT AVAIL it-doc-fisc THEN
       MESSAGE tt-doc-fiscal.cod-estabel SKIP                     
               tt-doc-fiscal.serie SKIP                           
               tt-doc-fiscal.nr-doc-fis SKIP
               int(tt-doc-fiscal.cod-emitente) SKIP
               tt-doc-fiscal.nat-operacao SKIP
               int(tt-doc-fiscal.nr-seq-doc)                
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
    ELSE DO:
       IF NOT CAN-FIND(ITEM WHERE ITEM.it-codigo = tt-doc-fiscal.it-codigo) THEN
          DISP tt-doc-fiscal.it-codigo.
       /*
       ASSIGN it-doc-fisc.it-codigo = tt-doc-fiscal.it-codigo.
       */
    END.
END.
