DEF VAR c-numero       AS CHAR.
DEF VAR c-compl        AS CHAR.
DEF VAR c-numero-cob   AS CHAR.
DEF VAR c-compl-cob    AS CHAR.
DEF var c-endereco     LIKE emitente.endereco.
DEF var c-endereco-cob LIKE emitente.endereco.
DEF VAR i-cont         AS INT.

OUTPUT TO c:/temp/lixo.csv.
PUT "Cod;Endereco;Logradouro;Numero;Compl;Endereco-Cob;Logradouro-Cob;Numero-Cob;Compl-Cob" SKIP.

FOR EACH emitente WHERE emitente.identific <> 2 NO-LOCK.
       IF INDEX(emitente.endereco,",") <> 0 THEN DO:
          ASSIGN c-compl = "".
          ASSIGN c-numero   = SUBSTR(emitente.endereco,INDEX(emitente.endereco,","),LENGTH(emitente.endereco) - INDEX(emitente.endereco,",") + 1)
                 c-endereco = SUBSTR(emitente.endereco,1,INDEX(emitente.endereco,",") - 1).
          DO i-cont = INDEX(emitente.endereco,",") + 1 TO LENGTH(emitente.endereco):
             IF SUBSTR(emitente.endereco,i-cont,1) = "," THEN
                ASSIGN c-compl = SUBSTR(emitente.endereco,i-cont,LENGTH(emitente.endereco) - i-cont + 1).
          END.
       END.
       ELSE DO:
          ASSIGN c-numero   = ""
                 c-endereco = emitente.endereco.
       END.
       IF INDEX(emitente.endereco-cob,",") <> 0 THEN DO:
          ASSIGN c-compl-cob = "".
          ASSIGN c-numero-cob   = SUBSTR(emitente.endereco-cob,INDEX(emitente.endereco-cob,","),LENGTH(emitente.endereco-cob) - INDEX(emitente.endereco-cob,",") + 1)
                 c-endereco-cob = SUBSTR(emitente.endereco-cob,1,INDEX(emitente.endereco-cob,",") - 1).

          DO i-cont = INDEX(emitente.endereco-cob,",") + 1 TO LENGTH(emitente.endereco-cob):
             IF SUBSTR(emitente.endereco-cob,i-cont,1) = "," THEN
                ASSIGN c-compl-cob = SUBSTR(emitente.endereco-cob,i-cont,LENGTH(emitente.endereco-cob) - i-cont + 1).
          END.
       END.
       ELSE DO:
          ASSIGN c-numero-cob   = ""
                 c-endereco-cob = emitente.endereco-cob.
       END.


       PUT UNFORMAT 
           emitente.cod-emitente ";"
           emitente.endereco ";"
           c-endereco ";"
           c-numero ";"
           c-compl ";"
           emitente.endereco-cob ";"
           c-endereco-cob ";"
           c-numero-cob ";"
           c-compl-cob
           SKIP.
END.
OUTPUT CLOSE.

