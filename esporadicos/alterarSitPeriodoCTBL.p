FOR EACH exerc_ctbl WHERE cod_exerc_ctbl ='2014':
    DISP exerc_ctbl WITH 1 COL WIDTH 550.
    FOR EACH sit_period_ctbl OF exerc_ctbl
        WHERE  cod_modul_dtsul = 'fgl' :
        UPDATE sit_period_ctbl WITH 1 COL WIDTH 550.

    END.
END.
