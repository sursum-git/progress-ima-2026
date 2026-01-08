FIND prog_dtsul WHERE
     prog_dtsul.cod_prog_dtsul = 'essp0108' NO-ERROR.

FIND sub_rot_dtsul_proced WHERE
     sub_rot_dtsul_proced.cod_prog_dtsul = prog_dtsul.cod_prog_dtsul NO-LOCK NO-ERROR.
IF NOT AVAIL sub_rot_dtsul_proced THEN
   FIND sub_rot_dtsul_proced WHERE
        sub_rot_dtsul_proced.cod_proced = prog_dtsul.cod_proced NO-LOCK NO-ERROR.

IF AVAIL sub_rot_dtsul_proced THEN DO.
   FIND sub_rot_dtsul OF sub_rot_dtsul_proced NO-LOCK NO-ERROR.

   FIND modul_rot_proced WHERE
        modul_rot_proced.num_sub_rot_dtsul = sub_rot_dtsul_proced.num_sub_rot_dtsul NO-LOCK NO-ERROR.
END.
ELSE DO.
    FIND modul_rot_proced WHERE
         modul_rot_proced.cod_prog_dtsul = prog_dtsul.cod_prog_dtsul NO-LOCK NO-ERROR.

    IF NOT AVAIL modul_rot_proced THEN
       FIND modul_rot_proced WHERE
            modul_rot_proced.cod_proced = prog_dtsul.cod_proced NO-LOCK NO-ERROR.
END.

FIND modul_rot OF modul_rot_proced NO-LOCK NO-ERROR.

FIND rot_dtsul OF modul_rot NO-LOCK NO-ERROR.

FIND modul_dtsul OF modul_rot_proced NO-LOCK NO-ERROR.
FIND sist_dtsul OF modul_dtsul NO-LOCK NO-ERROR.
FIND aplicat_dtsul OF sist_dtsul NO-LOCK NO-ERROR.

DISP "Aplicativo:" aplicat_dtsul.des_aplicat_dtsul WHEN AVAIL aplicat_dtsul SKIP
     "Sistema:" sist_dtsul.des_sist_dtsul WHEN AVAIL sist_dtsul SKIP
     "Modulo:" modul_dtsul.des_modul_dtsul WHEN AVAIL modul_dtsul SKIP
     "Rotina:" rot_dtsul.des_rot_dtsul WHEN AVAIL rot_dtsul SKIP
     "Sub-Rotina:" sub_rot_dtsul.des_sub_rot_dtsul WHEN AVAIL sub_rot_dtsul SKIP
     "Programa:" prog_dtsul.nom_prog_dtsul
     WITH NO-LABEL.
     
     

     


