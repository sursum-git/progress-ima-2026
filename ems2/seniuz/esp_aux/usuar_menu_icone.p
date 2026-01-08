OUTPUT TO c:/temp/usuar_menu_icone.csv.
FOR EACH usuar_menu_icone WHERE usuar_menu_icone.cod_usuario = "paulo"
                          NO-LOCK.
    PUT UNFORMAT
        usuar_menu_icone.num_icone ";"
        usuar_menu_icone.nom_icone ";"
        usuar_menu_icone.cod_lin_comando ";"
        usuar_menu_icone.ind_tip_prog
        SKIP.
END.
OUTPUT CLOSE.
