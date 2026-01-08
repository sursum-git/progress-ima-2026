OUTPUT TO c:\temp\imobilizados.txt.
FOR EACH natur-oper
    WHERE venda-ativo = YES:
    FOR EACH docum-est OF natur-oper
        WHERE dt-trans > 11.01.2014 USE-INDEX natoper:
        DISP dt-trans cod-estabel nro-docto serie-docto cod-emitente nat-operacao .

    END.
END.
