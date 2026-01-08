OUTPUT TO c:/temp/fornecedores.csv CONVERT SOURCE "ibm850".
PUT "Codigo;Nome;Cidade;UF;Ult-Compra" SKIP.

FOR EACH emitente WHERE emitente.identific <> 1 NO-LOCK
                  BY emitente.cidade:
    FIND LAST recebimento WHERE recebimento.cod-emitente = emitente.cod-emitente NO-LOCK NO-ERROR.
    IF AVAIL recebimento THEN
       PUT emitente.cod-emitente ";"
           emitente.nome-emit ";"
           emitente.cidade ";"
           emitente.estado ";"
           recebimento.data-mov
           SKIP.
END.
OUTPUT CLOSE.
