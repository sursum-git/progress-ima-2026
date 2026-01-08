FOR EACH prog_dtsul_segur WHERE prog_dtsul_segur.cod_grp_usuar = "*".
    DELETE prog_dtsul_segur.
END.

FOR EACH proced_segur WHERE
         proced_segur.cod_grp_usuar = "*".
    DELETE proced_segur.
END. 

FOR EACH modul_dtsul_segur WHERE
         modul_dtsul_segur.cod_grp_usuar = "*".
    DELETE modul_dtsul_segur.
END. 

FOR EACH sist_dtsul_segur WHERE
         sist_dtsul_segur.cod_grp_usuar = "*".
    DELETE sist_dtsul_segur.
END. 

FOR EACH aplicat_dtsul_segur WHERE
         aplicat_dtsul_segur.cod_grp_usuar = "*". 
    DELETE aplicat_dtsul_segur.
END. 

FOR EACH modul_rot_segur WHERE
         modul_rot_segur.cod_grp_usuar = "*". 
    DELETE modul_rot_segur.
END. 

FOR EACH sub_rot_dtsul_segur WHERE
         sub_rot_dtsul_segur.cod_grp_usuar = "*". 
    DELETE sub_rot_dtsul_segur.
END. 
