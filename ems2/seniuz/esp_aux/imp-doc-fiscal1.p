/* Programa: imp-doc-fiscal1.p
** Objetivo: Importar acerto em documentos fiscais. 
*/

DEF VAR i-cont-err AS INT.

DEF TEMP-TABLE tt-item
    FIELD it-codigo LIKE ITEM.it-codigo
    INDEX ch-item it-codigo.

DEF TEMP-TABLE tt-doc-fiscal
    FIELD ordem        AS char
    FIELD cod-estabel  AS CHAR
    FIELD serie        AS CHAR
    FIELD nr-doc-fis   AS CHAR 
    FIELD cod-emitente AS CHAR
    FIELD nat-operacao AS CHAR
    FIELD dt-emissao   AS CHAR
    FIELD it-codigo    AS CHAR
    FIELD nr-seq-doc   AS CHAR
    FIELD narrativa    AS CHAR 
    FIELD cod-novo     AS CHAR.
    
input from "C:\lixo\Doc-fis_atualizado/doc-fisc_2006.csv".
SET ^.

repeat:
   create tt-doc-fiscal.
   import delimiter ";" tt-doc-fiscal.
end.
input close.
/*
FOR EACH tt-doc-fiscal:
    DISP tt-doc-fiscal.it-codigo
         tt-doc-fiscal.cod-novo.
END.
*/
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
       IF NOT CAN-FIND(ITEM WHERE ITEM.it-codigo = tt-doc-fiscal.cod-novo) THEN DO.
          FIND tt-item WHERE tt-item.it-codigo = tt-doc-fiscal.cod-novo NO-LOCK NO-ERROR.
          IF NOT AVAIL tt-item THEN DO.
             CREATE tt-item.
             ASSIGN tt-item.it-codigo = tt-doc-fiscal.cod-novo.
          END.
       END.
       /*
       ASSIGN it-doc-fisc.it-codigo = tt-doc-fiscal.cod-novo.
       */
    END.
END.

FOR EACH tt-item.
    DISP tt-item.
END.
