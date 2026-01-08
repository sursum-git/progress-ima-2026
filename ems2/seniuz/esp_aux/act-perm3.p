    FOR EACH sub_rot_dtsul.
        FOR EACH sub_rot_dtsul_proced OF sub_rot_dtsul.
            IF sub_rot_dtsul_proced.cod_proced <> "" THEN DO.
               FIND procedimento WHERE
                    procedimento.cod_proced = sub_rot_dtsul_proced.cod_proced NO-ERROR.

               FOR EACH proced_segur OF procedimento NO-LOCK.
                   FIND sub_rot_dtsul_segur WHERE
                        sub_rot_dtsul_segur.num_sub_rot_dtsul = sub_rot_dtsul_proced.num_sub_rot_dtsul AND
                        sub_rot_dtsul_segur.cod_grp = proced_segur.cod_grp_usuar NO-ERROR.
                   IF NOT AVAIL sub_rot_dtsul_segur THEN DO.
                      CREATE sub_rot_dtsul_segur.
                      ASSIGN sub_rot_dtsul_segur.num_sub_rot_dtsul = sub_rot_dtsul_proced.num_sub_rot_dtsul
                             sub_rot_dtsul_segur.cod_grp = proced_segur.cod_grp_usuar.
                   END.
               END.
            END.

            IF sub_rot_dtsul_proced.cod_prog_dtsul <> "" THEN DO.
               FIND prog_dtsul WHERE
                    prog_dtsul.cod_prog_dtsul = sub_rot_dtsul_proced.cod_prog_dtsul NO-ERROR.

               FOR EACH prog_dtsul_segur OF prog_dtsul NO-LOCK.
                   FIND sub_rot_dtsul_segur WHERE
                        sub_rot_dtsul_segur.num_sub_rot_dtsul = sub_rot_dtsul_proced.num_sub_rot_dtsul AND
                        sub_rot_dtsul_segur.cod_grp = prog_dtsul_segur.cod_grp_usuar NO-ERROR.
                   IF NOT AVAIL sub_rot_dtsul_segur THEN DO.
                      CREATE sub_rot_dtsul_segur.
                      ASSIGN sub_rot_dtsul_segur.num_sub_rot_dtsul = sub_rot_dtsul_proced.num_sub_rot_dtsul
                             sub_rot_dtsul_segur.cod_grp = prog_dtsul_segur.cod_grp_usuar.
                   END.
               END.
            END.
        END.
    END.

