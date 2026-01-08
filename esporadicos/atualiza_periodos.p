
FOR EACH PERIOD_CTBL WHERE:
    DISP PERIOD_CTBL WITH 1 COL WIDTH 550.
    FOR EACH sit_period_ctbl OF PERIOD_CTBL
        WHERE cod_modul_dtsul = 'fas':
        UPDATE sit_period_ctbl WITH 1 COL WIDTH 550.
    END.
END.
