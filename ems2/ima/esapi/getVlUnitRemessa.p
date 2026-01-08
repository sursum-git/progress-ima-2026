DEFINE INPUT  PARAMETER pRowid  AS ROWID       NO-UNDO.
DEFINE OUTPUT PARAMETER dValor  AS DECIMAL     NO-UNDO.
FOR FIRST saldo-terc NO-LOCK                                                                                                                                                     
    WHERE rowid(saldo-terc) = pRowid .
    FOR FIRST componente OF saldo-terc WHERE componente.componente = 1 NO-LOCK :
        ASSIGN dValor = componente.preco-total[1].
    END.
END.
    
    
