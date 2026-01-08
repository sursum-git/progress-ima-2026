DEF VAR i-seq AS INT.
DEF BUFFER b-usuar_menu_icone FOR usuar_menu_icone.
FOR EACH usuar_menu_icone WHERE usuar_menu_icone.cod_usuario = "janete"
                          NO-LOCK.
    ASSIGN i-seq = i-seq + 1.
    CREATE b-usuar_menu_icone.
    ASSIGN b-usuar_menu_icone.cod_usuar       = 'dalva'
           b-usuar_menu_icone.num_icone       = i-seq      
           b-usuar_menu_icone.nom_icone       = usuar_menu_icone.nom_icone      
           b-usuar_menu_icone.cod_lin_comando = usuar_menu_icone.cod_lin_comando
           b-usuar_menu_icone.ind_tip_prog    = usuar_menu_icone.ind_tip_prog.         
END.
