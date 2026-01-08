/*
gera‡Æo de arquivo para importa‡Æo no programa fp5915
a partir do arquivo gerado no programa fp4061(com alguns ajustes)

*/


DEFINE VARIABLE cMeses AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iCont AS INTEGER     NO-UNDO.
DEFINE VARIABLE cArquivoCorrente AS CHARACTER   NO-UNDO FORMAT 'x(100)'.
DEFINE VARIABLE cMesCorrente AS CHARACTER   NO-UNDO FORMAT 'x(100)'.
ASSIGN cMeses = '022017,032017,042017,052017,062017,072017,082017,092017,102017,112017,122017'.


DEFINE TEMP-TABLE tt
    FIELD cpf AS CHAR FORMAT '99999999999'
    FIELD nome AS CHAR FORMAT 'x(50)'
    FIELD data AS DATE FORMAT '99/99/9999'
    FIELD valor AS DECIMAL FORMAT '99999999.99'.

OUTPUT TO VALUE("c:\temp\CONSOLIDADO.csv").
REPEAT iCont = 1 TO NUM-ENTRIES(cMeses,",").
    EMPTY TEMP-TABLE tt.
    ASSIGN cArquivoCorrente = "c:\temp\despesas_medicas\" + ENTRY(iCont,cMeses,",") + ".csv"
           cMesCorrente     = ENTRY(iCont,cMeses,",").
    MESSAGE cArquivoCorrente
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    IF SEARCH(cARquivoCorrente) <> ? THEN DO:

       INPUT FROM VALUE(cARquivoCorrente).
            REPEAT:
                CREATE tt.
                IMPORT DELIMITER ";" tt.
            END.
           
       INPUT CLOSE.
       
       FOR EACH tt
           WHERE tt.valor <> 0:
           IF cMesCorrente = '022017' OR cmesCorrente = '032017' THEN
              ASSIGN tt.valor = tt.valor - 1.33.
           ELSE 
              ASSIGN tt.valor = tt.valor - 1.40.

           CASE LENGTH(tt.cpf):
               WHEN 10 THEN
                   ASSIGN tt.cpf = '0' + tt.cpf.
               WHEN 9 THEN
                   ASSIGN tt.cpf = '00' + tt.cpf.
               WHEN 8 THEN
                   ASSIGN tt.cpf = '000' + tt.cpf.

           END CASE.
           PUT "1;21126271000168;" tt.cpf ";" tt.nome ";" tt.data ";" SUBSTRING(cMesCorrente,3,4) FORMAT "x(4)" ";" SUBSTRING(cMesCorrente,1,2) FORMAT "x(2)"
               ";" tt.valor SKIP. 
       END.                                   
       
    END.
END.
OUTPUT CLOSE.






