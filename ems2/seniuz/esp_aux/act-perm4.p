FOR EACH modul_dtsul NO-LOCK.
    FOR EACH modul_rot OF modul_dtsul.
        FOR EACH modul_rot_proced OF modul_rot.

            IF modul_rot_proced.cod_proced <> "" THEN DO.
               FIND procedimento WHERE
                    procedimento.cod_proced = modul_rot_proced.cod_proced NO-ERROR.

               FOR EACH proced_segur OF procedimento NO-LOCK.
                   FIND modul_rot_segur WHERE
                        modul_rot_segur.cod_modul_dtsul = modul_rot_proced.cod_modul_dtsul AND
                        modul_rot_segur.num_rot_dtsul = modul_rot_proced.num_rot_dtsul AND
                        modul_rot_segur.cod_grp = proced_segur.cod_grp_usuar NO-ERROR.
                   IF NOT AVAIL modul_rot_segur THEN DO.
                      CREATE modul_rot_segur.
                      ASSIGN modul_rot_segur.cod_modul_dtsul = modul_rot_proced.cod_modul_dtsul
                             modul_rot_segur.num_rot_dtsul = modul_rot_proced.num_rot_dtsul
                             modul_rot_segur.cod_grp = proced_segur.cod_grp_usuar.
                   END.
               END.
            END.

            IF modul_rot_proced.cod_prog_dtsul <> "" THEN DO.
                FIND prog_dtsul WHERE
                     prog_dtsul.cod_prog_dtsul = modul_rot_proced.cod_prog_dtsul NO-ERROR.

                FOR EACH prog_dtsul_segur OF prog_dtsul NO-LOCK.
                    FIND modul_rot_segur WHERE
                         modul_rot_segur.cod_modul_dtsul = modul_rot_proced.cod_modul_dtsul AND
                         modul_rot_segur.num_rot_dtsul = modul_rot_proced.num_rot_dtsul AND
                         modul_rot_segur.cod_grp = prog_dtsul_segur.cod_grp_usuar NO-ERROR.
                    IF NOT AVAIL modul_rot_segur THEN DO.
                       CREATE modul_rot_segur.
                       ASSIGN modul_rot_segur.cod_modul_dtsul = modul_rot_proced.cod_modul_dtsul
                              modul_rot_segur.num_rot_dtsul = modul_rot_proced.num_rot_dtsul
                              modul_rot_segur.cod_grp = prog_dtsul_segur.cod_grp_usuar.
                    END.
                END.
            END.
                
            IF modul_rot_proced.num_sub_rot_dtsul <> 0 THEN DO.
               FIND sub_rot_dtsul WHERE
                    sub_rot_dtsul.num_sub_rot_dtsul = modul_rot_proced.num_sub_rot_dtsul NO-ERROR.

                FOR EACH sub_rot_dtsul_segur OF sub_rot_dtsul NO-LOCK.
                    FIND modul_rot_segur WHERE
                         modul_rot_segur.cod_modul_dtsul = modul_rot_proced.cod_modul_dtsul AND
                         modul_rot_segur.num_rot_dtsul = modul_rot_proced.num_rot_dtsul AND
                         modul_rot_segur.cod_grp = sub_rot_dtsul_segur.cod_grp_usuar NO-ERROR.
                    IF NOT AVAIL modul_rot_segur THEN DO.
                       CREATE modul_rot_segur.
                       ASSIGN modul_rot_segur.cod_modul_dtsul = modul_rot_proced.cod_modul_dtsul
                              modul_rot_segur.num_rot_dtsul = modul_rot_proced.num_rot_dtsul
                              modul_rot_segur.cod_grp = sub_rot_dtsul_segur.cod_grp_usuar.
                    END.
                END.
            END.
        END.
    END.
END.
