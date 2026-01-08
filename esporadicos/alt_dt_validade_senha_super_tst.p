DISABLE TRIGGERS FOR LOAD OF fndtst.usuar_mestre.
FOR EACH fndtst.usuar_mestre
    WHERE usuar_mestre.cod_usuario = 'super'.
    ASSIGN usuar_mestre.num_dias_valid_senha = 999999
           usuar_mestre.dat_valid_senha = 01.01.2999.
    DISP usuar_mestre WITH 1 COL WIDTH 550.
    
END.
