DEF BUFFER b-usuar_menu_icone FOR usuar_menu_icone.

FOR EACH usuar_menu_icone WHERE usuar_menu_icone.cod_usuario = "viviane".
    /*
    DISP usuar_menu_icone.num_icone       
         usuar_menu_icone.nom_icone       
         usuar_menu_icone.cod_lin_comando 
         usuar_menu_icone.ind_tip_prog
        WITH SIDE-LABELS 1 COLUMN WIDTH 300.
    */
    /*
    IF usuar_menu_icone.num_icone = 18 THEN DO.
       CREATE b-usuar_menu_icone.
       ASSIGN b-usuar_menu_icone.cod_usuario     = "rosana"
              b-usuar_menu_icone.num_icone       = 16      
              b-usuar_menu_icone.nom_icone       = usuar_menu_icone.nom_icone      
              b-usuar_menu_icone.cod_lin_comando = usuar_menu_icone.cod_lin_comando
              b-usuar_menu_icone.ind_tip_prog    = usuar_menu_icone.ind_tip_prog.   
    END.
    */
END.
