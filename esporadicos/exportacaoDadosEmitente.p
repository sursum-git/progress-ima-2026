OUTPUT TO c:\temp\emitente.csv CONVERT TARGET "utf-8" SOURCE SESSION:CPINTERNAL .
PUT "cod_emitente;nome_abrev;nome_emit;cidade;estado;identific;natureza;cgc;cep;pais;atividade;telefone" SKIP.
FOR EACH emitente:
    EXPORT DELIMITER ";" 
    emitente.cod-emitente
    emitente.nome-abrev
    emitente.nome-emit
    emitente.cidade
    emitente.estado
    emitente.identific
    emitente.natureza
    emitente.cgc
    emitente.cep
    emitente.pais
    emitente.atividade
    emitente.telefone
    .

END.


OUTPUT CLOSE.
