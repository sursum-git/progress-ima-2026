DEFINE VARIABLE ano AS INTEGER     NO-UNDO.
DEFINE VARIABLE mes AS INTEGER     NO-UNDO.
DEFINE VARIABLE nf AS INTEGER     NO-UNDO.
DEFINE VARIABLE serie AS INTEGER     NO-UNDO.
DEFINE VARIABLE cLinha AS CHARACTER   NO-UNDO FORMAT "X(2000)".
DEFINE VARIABLE cArquivo AS CHARACTER   NO-UNDO FORMAT 'x(50)'.

DEFINE VARIABLE iCont AS INTEGER     NO-UNDO.
OUTPUT TO c:\temp\notas_fiscais.txt.
FOR EACH nota-fiscal WHERE
         YEAR(nota-fiscal.dt-emis-nota) = 2015.

         ASSIGN ano = YEAR(nota-fiscal.dt-emis-nota).
         ASSIGN mes = MONTH(nota-fiscal.dt-emis-nota).
         ASSIGN nf  = INT(nota-fiscal.nr-nota-fis).
         ASSIGN serie = INT(nota-fiscal.serie).
/*          MESSAGE ano SKIP mes SKIP nf SKIP serie SKIP */
/*               nota-fiscal.nr-nota-fis                 */
/*              VIEW-AS ALERT-BOX INFO BUTTONS OK.       */
/*                                                       */
     PUT nota-fiscal.nr-nota-fis nota-fiscal.dt-emis-nota cod-emitente nota-fiscal.cgc.
     ASSIGN cArquivo = "z:\" + STRING(ano) + "\" + STRING(mes,"99") + "\3_" + STRING(nf) + ".xml".
     IF SEARCH(cArquivo) <> ? THEN DO:
        INPUT FROM VALUE(cArquivo ).
        REPEAT:
            ASSIGN iCont = iCont + 1.
            IMPORT UNFORM cLinha.
           IF iCont = 1  THEN 
            PUT ENTRY(1,ENTRY(82,cLinha,">"),"<") .

        END.

            
        INPUT CLOSE.
     END.
     ELSE
         PUT 'arquivo: nao encontrado'  cArquivo.
    PUT SKIP.
     
    
END.






 /*

INPUT FROM z:\2014\07\3_28225.xml.

REPEAT:


IMPORT UNFORM cLinha.


MESSAGE  ENTRY(1,ENTRY(86,cLinha,">"),"<")
    VIEW-AS ALERT-BOX INFO BUTTONS OK.


END.

INPUT CLOSE.
*/
