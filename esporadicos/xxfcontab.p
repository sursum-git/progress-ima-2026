OUTPUT TO c:\temp\func.txt.
FOR EACH funcionario NO-LOCK
    WHERE cod_rh_ccusto = '20200'
    .
    FIND rh_pessoa_fisic OF funcionario NO-LOCK NO-ERROR.

    DISP val_salario_atual funcionario.nom_pessoa_fisic
        dat_desligto_func.
END.
