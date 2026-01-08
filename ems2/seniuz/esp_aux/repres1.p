OUTPUT TO c:/temp/repres.csv CONVERT SOURCE "ibm850".
FOR EACH repres NO-LOCK.
    PUT repres.cod-rep ";"
        repres.nome-abrev ";"
        repres.nome ";"
        repres.endereco ";"
        repres.cep ";"
        repres.cidade ";"
        repres.estado SKIP.
END.
OUTPUT CLOSE.
