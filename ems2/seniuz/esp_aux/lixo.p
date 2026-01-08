/* Programa: cep_emitente_ace.p
** Objetivo: Acerta dados de endere‡os para emitentes, para posterior importa‡Æo. 
*/

DEF VAR c-numero1 AS CHAR FORMAT "x(20)" LABEL "Atual".
DEF VAR c-numero2 AS CHAR FORMAT "x(20)" LABEL "Anter".
DEF VAR c-numero3 AS CHAR FORMAT "x(20)" LABEL "Cob.Atual".
DEF VAR c-numero4 AS CHAR FORMAT "x(20)" LABEL "Cob.Anter".

DEF TEMP-TABLE tt-emitente
    FIELD cod-emitente LIKE emitente.cod-emitente
    FIELD endereco     LIKE emitente.endereco
    FIELD endereco-cob LIKE emitente.endereco-cob.
    
input from "c:/temp/lixo.csv".
SET ^.

repeat:
   create tt-emitente.
   import delimiter ";" tt-emitente.
end.
input close.

FOR EACH tt-emitente.
    ASSIGN c-numero1 = ""
           c-numero2 = ""
           c-numero3 = ""
           c-numero4 = "".
    FIND emitente WHERE emitente.cod-emitente = tt-emitente.cod-emitente.

    IF emitente.endereco = tt-emitente.endereco AND emitente.endereco-cob = tt-emitente.endereco-cob THEN NEXT.
    
    IF INDEX(emitente.endereco,",") <> 0 THEN
       ASSIGN c-numero1 = SUBSTR(emitente.endereco,INDEX(emitente.endereco,","),LENGTH(emitente.endereco) - INDEX(emitente.endereco,",") + 1)
              c-numero2 = SUBSTR(tt-emitente.endereco,INDEX(tt-emitente.endereco,","),LENGTH(tt-emitente.endereco) - INDEX(tt-emitente.endereco,",") + 1).
    IF INDEX(emitente.endereco-cob,",") <> 0 THEN 
       ASSIGN c-numero3 = SUBSTR(emitente.endereco-cob,INDEX(emitente.endereco-cob,","),LENGTH(emitente.endereco-cob) - INDEX(emitente.endereco-cob,",") + 1).
              c-numero4 = SUBSTR(tt-emitente.endereco-cob,INDEX(tt-emitente.endereco-cob,","),LENGTH(tt-emitente.endereco-cob) - INDEX(tt-emitente.endereco-cob,",") + 1).

    IF c-numero1 <> c-numero2 THEN DO:
       DISP SKIP(1)
            c-numero1
            c-numero2
            tt-emitente.endereco WITH SIDE-LABELS 1 COLUMN.
       UPDATE emitente.endereco.
    END.
       
    IF c-numero3 <> c-numero4 THEN DO:
       DISP c-numero3
            c-numero4
            tt-emitente.endereco-cob WITH SIDE-LABELS 1 COLUMN.
       UPDATE emitente.endereco-cob.
    END.
END.

