OUTPUT TO c:/temp/cli-loja.csv CONVERT SOURCE "ibm850".
PUT "Codigo;Nome;Endere‡o;Bairro;Cidade;UF;CPF/CNPJ;Telefone" SKIP.
FOR EACH emitente WHERE emitente.cod-gr-cli = 9 NO-LOCK.
    PUT emitente.cod-emitente ";"
        emitente.nome-emit ";"
        emitente.endereco ";"
        emitente.bairro ";"
        emitente.cidade ";"
        emitente.estado ";"
        emitente.cgc ";"
        emitente.telefone
        SKIP.
END.
OUTPUT CLOSE.
