DEF VAR da-ult-compra LIKE nota-fiscal.dt-emis-nota.
OUTPUT TO c:/temp/emitente.csv.
PUT "UF;Nat;Ult-Compra;Cod;Nome;CNPJ;Contato;Telefone;Ramal;Ramal-Fax;Telex" SKIP.
FOR EACH emitente WHERE emitente.identific <> 2 NO-LOCK.
    FIND LAST nota-fiscal USE-INDEX ch-clinota 
        WHERE nota-fiscal.nome-ab-cli = emitente.nome-abrev NO-LOCK NO-ERROR.
    IF AVAIL nota-fiscal THEN
       ASSIGN da-ult-compra = nota-fiscal.dt-emis-nota.
    ELSE
       ASSIGN da-ult-compra = ?.
    PUT emitente.estado ";"
        emitente.natureza ";"
        da-ult-compra ";"
        emitente.cod-emitente ";"
        emitente.nome-emit ";"
        emitente.cgc ";"
        emitente.contato ";"
        emitente.telefone ";"
        emitente.ramal ";"
        emitente.ramal-fax ";"
        emitente.telex
        SKIP.
END.
OUTPUT CLOSE.
