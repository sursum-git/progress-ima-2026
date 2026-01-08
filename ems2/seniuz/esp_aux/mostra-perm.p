/* Programa: mostra-perm
**           Mostra permiss‰es de um usu†rio.
*/

DEF VAR c-grp_usuar-ori LIKE prog_dtsul_segur.cod_grp_usuar.
DEF VAR c-nom-ori       AS CHAR.
DEF VAR l-ok            AS LOG.

FORM
    c-grp_usuar-ori AT 01
    c-nom-ori       AT 20 NO-LABEL
    l-ok            AT 01
    WITH SIDE-LABELS FRAME f-1.

ON 'leave':U OF c-grp_usuar-ori IN FRAME f-1
DO:
    FIND grp_usuar WHERE grp_usuar.cod_grp_usuar = INPUT FRAME f-1 c-grp_usuar-ori NO-LOCK NO-ERROR.
    IF NOT AVAIL grp_usuar THEN DO:
       MESSAGE "Grupo de usu†rio origem n∆o cadastrado." VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
    END.
    ASSIGN c-nom-ori:SCREEN-VALUE = grp_usuar.des_grp_usuar.
END.


UPDATE c-grp_usuar-ori LABEL "Origem"
       l-ok            LABEL "Ok?"
       WITH FRAME f-1.

IF l-ok = NO THEN RETURN.

FOR EACH prog_dtsul_segur WHERE prog_dtsul_segur.cod_grp_usuar = c-grp_usuar-ori
                          NO-LOCK.
    DISP prog_dtsul_segur.cod_prog_dtsul.
END.

FOR EACH proced_segur WHERE proced_segur.cod_grp_usuar = c-grp_usuar-ori
                      NO-LOCK:

    DISP proced_segur.cod_proced.
END. 

FOR EACH modul_dtsul_segur WHERE modul_dtsul_segur.cod_grp_usuar = c-grp_usuar-ori
                           NO-LOCK:
    DISP modul_dtsul_segur.cod_modul_dtsul.
END. 

FOR EACH sist_dtsul_segur WHERE sist_dtsul_segur.cod_grp_usuar = c-grp_usuar-ori
                          NO-LOCK:
    DISP sist_dtsul_segur.cod_sist_dtsul.
END. 

FOR EACH aplicat_dtsul_segur WHERE aplicat_dtsul_segur.cod_grp_usuar = c-grp_usuar-ori
                             NO-LOCK: 
    DISP aplicat_dtsul_segur.cod_aplicat_dtsul.
END. 

FOR EACH modul_rot_segur WHERE modul_rot_segur.cod_grp_usuar = c-grp_usuar-ori
                         NO-LOCK: 
    DISP modul_rot_segur.cod_modul_dtsul.
END. 

FOR EACH sub_rot_dtsul_segur WHERE sub_rot_dtsul_segur.cod_grp_usuar = c-grp_usuar-ori
                             NO-LOCK: 
    DISP sub_rot_dtsul_segur.num_sub_rot_dtsul.
END. 

