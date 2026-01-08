DEF VAR c-comando LIKE usuar_menu_icone.cod_lin_comando.

FOR EACH usuar_menu_icone.
    IF usuar_menu_icone.cod_lin_comando BEGINS "N:\especificos" THEN
       ASSIGN c-comando = SUBSTR(usuar_menu_icone.cod_lin_comando,16,LENGTH(usuar_menu_icone.cod_lin_comando) - 15).
    ELSE 
    IF usuar_menu_icone.cod_lin_comando BEGINS "E:\DATASUL\ems206" THEN
       ASSIGN c-comando = SUBSTR(usuar_menu_icone.cod_lin_comando,19,LENGTH(usuar_menu_icone.cod_lin_comando) - 18).
    ELSE
    IF usuar_menu_icone.cod_lin_comando BEGINS "N:\" THEN
       ASSIGN c-comando = SUBSTR(usuar_menu_icone.cod_lin_comando,4,LENGTH(usuar_menu_icone.cod_lin_comando) - 3).
    ELSE
       ASSIGN c-comando = usuar_menu_icone.cod_lin_comando.

    /*ASSIGN usuar_menu_icone.cod_lin_comando = c-comando.*/
    
    DISP usuar_menu_icone.cod_lin_comando
         c-comando WITH SIDE-LABELS 1 COLUMN WIDTH 300.
    
    /*
    UPDATE usuar_menu_icone.num_icone
           usuar_menu_icone.nom_icone
           usuar_menu_icone.cod_lin_comando
           usuar_menu_icone.ind_tip_prog WITH SIDE-LABELS 1 COLUMN WIDTH 300.
    */
END.
