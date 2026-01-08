/* Programa: copia-perm
**           Copia permiss‰es de um usu†rio para outro, sem cancelar as
**           que o usu†rio destino j† tinha.
*/

DEF VAR c-grp_usuar-ori LIKE prog_dtsul_segur.cod_grp_usuar.
DEF VAR c-grp_usuar-dst LIKE prog_dtsul_segur.cod_grp_usuar.
DEF VAR c-nom-ori LIKE grp_usuar.des_grp_usuar.
DEF VAR c-nom-dst LIKE grp_usuar.des_grp_usuar.
DEF VAR l-ok AS LOG FORMAT "Sim/N∆o".
DEF VAR i-cont AS INT.

DEF BUFFER b-prog_dtsul_segur FOR prog_dtsul_segur.
DEF BUFFER b-proced_segur FOR proced_segur.
DEF BUFFER b-modul_dtsul_segur FOR modul_dtsul_segur.
DEF BUFFER b-sist_dtsul_segur FOR sist_dtsul_segur.
DEF BUFFER b-aplicat_dtsul_segur FOR aplicat_dtsul_segur.
DEF BUFFER b-modul_rot_segur FOR modul_rot_segur.
DEF BUFFER b-sub_rot_dtsul_segur FOR sub_rot_dtsul_segur.

FORM
    c-grp_usuar-ori AT 01
    c-nom-ori       AT 20 NO-LABEL
    c-grp_usuar-dst AT 01
    c-nom-dst       AT 20 NO-LABEL
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

ON 'leave':U OF c-grp_usuar-dst IN FRAME f-1
DO:
    FIND grp_usuar WHERE grp_usuar.cod_grp_usuar = INPUT FRAME f-1 c-grp_usuar-dst NO-LOCK NO-ERROR.
    IF NOT AVAIL grp_usuar THEN DO:
       MESSAGE "Grupo de usu†rio destino n∆o cadastrado." VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
    END.
    ASSIGN c-nom-dst:SCREEN-VALUE = grp_usuar.des_grp_usuar.
END.

UPDATE c-grp_usuar-ori LABEL "Origem"
       c-grp_usuar-dst LABEL "Destino"
       l-ok            LABEL "Ok?"
       WITH FRAME f-1.

IF l-ok = NO THEN RETURN.

FOR EACH prog_dtsul_segur WHERE prog_dtsul_segur.cod_grp_usuar = c-grp_usuar-ori
                          NO-LOCK.
    FIND b-prog_dtsul_segur 
         WHERE b-prog_dtsul_segur.cod_prog_dtsul = prog_dtsul_segur.cod_prog_dtsul
           AND b-prog_dtsul_segur.cod_grp_usuar  = c-grp_usuar-dst
         NO-LOCK NO-ERROR.
    IF NOT AVAIL b-prog_dtsul_segur THEN DO:
       ASSIGN i-cont = i-cont + 1.
       CREATE b-prog_dtsul_segur.
       ASSIGN b-prog_dtsul_segur.cod_prog_dtsul = prog_dtsul_segur.cod_prog_dtsul
              b-prog_dtsul_segur.cod_grp_usuar  = c-grp_usuar-dst.
    END.
END.

FOR EACH proced_segur WHERE proced_segur.cod_grp_usuar = c-grp_usuar-ori
                      NO-LOCK:
    FIND b-proced_segur WHERE b-proced_segur.cod_proced    = proced_segur.cod_proced
                          AND b-proced_segur.cod_grp_usuar = c-grp_usuar-dst
                        NO-LOCK NO-ERROR.
    IF NOT AVAIL b-proced_segur THEN DO:
       ASSIGN i-cont = i-cont + 1.
       CREATE b-proced_segur.
       ASSIGN b-proced_segur.cod_proced = PROCED_segur.cod_proced
              b-proced_segur.cod_grp_usuar = c-grp_usuar-dst.
    END.
END. 

FOR EACH modul_dtsul_segur WHERE modul_dtsul_segur.cod_grp_usuar = c-grp_usuar-ori
                           NO-LOCK:
    FIND b-modul_dtsul_segur 
         WHERE b-modul_dtsul_segur.cod_modul_dtsul = modul_dtsul_segur.cod_modul_dtsul
           AND b-modul_dtsul_segur.cod_grp_usuar   = c-grp_usuar-dst
         NO-LOCK NO-ERROR.
    IF NOT AVAIL b-modul_dtsul_segur THEN DO:
       ASSIGN i-cont = i-cont + 1.
       CREATE b-modul_dtsul_segur.
       ASSIGN b-modul_dtsul_segur.cod_modul_dtsul = modul_dtsul_segur.cod_modul_dtsul
              b-modul_dtsul_segur.cod_grp_usuar   = c-grp_usuar-dst.                  
    END.
END. 

FOR EACH sist_dtsul_segur WHERE sist_dtsul_segur.cod_grp_usuar = c-grp_usuar-ori
                          NO-LOCK:
    FIND b-sist_dtsul_segur
         WHERE b-sist_dtsul_segur.cod_sist_dtsul = sist_dtsul_segur.cod_sist_dtsul
           AND b-sist_dtsul_segur.cod_grp_usuar  = c-grp_usuar-dst
         NO-LOCK NO-ERROR.
    IF NOT AVAIL b-sist_dtsul_segur THEN DO:
       ASSIGN i-cont = i-cont + 1.
       CREATE b-sist_dtsul_segur.
       ASSIGN b-sist_dtsul_segur.cod_sist_dtsul = sist_dtsul_segur.cod_sist_dtsul
              b-sist_dtsul_segur.cod_grp_usuar  = c-grp_usuar-dst.               
    END.
END. 

FOR EACH aplicat_dtsul_segur WHERE aplicat_dtsul_segur.cod_grp_usuar = c-grp_usuar-ori
                             NO-LOCK: 
    FIND b-aplicat_dtsul_segur 
         WHERE b-aplicat_dtsul_segur.cod_aplicat_dtsul = aplicat_dtsul_segur.cod_aplicat_dtsul
           AND b-aplicat_dtsul_segur.cod_grp_usuar     = c-grp_usuar-dst
         NO-LOCK NO-ERROR.
    IF NOT AVAIL b-aplicat_dtsul_segur THEN DO:
       ASSIGN i-cont = i-cont + 1.
       CREATE b-aplicat_dtsul_segur.
       ASSIGN b-aplicat_dtsul_segur.cod_aplicat_dtsul = aplicat_dtsul_segur.cod_aplicat_dtsul
              b-aplicat_dtsul_segur.cod_grp_usuar     = c-grp_usuar-dst.                      
    END.                                                                    
END. 

FOR EACH modul_rot_segur WHERE modul_rot_segur.cod_grp_usuar = c-grp_usuar-ori
                         NO-LOCK: 
    FIND b-modul_rot_segur 
         WHERE b-modul_rot_segur.cod_modul_dtsul = modul_rot_segur.cod_modul_dtsul
           AND b-modul_rot_segur.num_rot_dtsul   = modul_rot_segur.num_rot_dtsul
           AND b-modul_rot_segur.cod_grp_usuar   = c-grp_usuar-dst
        NO-LOCK NO-ERROR.
    IF NOT AVAIL b-modul_rot_segur THEN DO:
       ASSIGN i-cont = i-cont + 1.
       CREATE b-modul_rot_segur.
       ASSIGN b-modul_rot_segur.cod_modul_dtsul = modul_rot_segur.cod_modul_dtsul
              b-modul_rot_segur.num_rot_dtsul   = modul_rot_segur.num_rot_dtsul  
              b-modul_rot_segur.cod_grp_usuar   = c-grp_usuar-dst.               
    END.
END. 

FOR EACH sub_rot_dtsul_segur WHERE sub_rot_dtsul_segur.cod_grp_usuar = c-grp_usuar-ori
                             NO-LOCK: 
    FIND b-sub_rot_dtsul_segur 
         WHERE b-sub_rot_dtsul_segur.num_sub_rot_dtsul = sub_rot_dtsul_segur.num_sub_rot_dtsul
           AND b-sub_rot_dtsul_segur.cod_grp_usuar     = c-grp_usuar-dst
         NO-LOCK NO-ERROR.
    IF NOT AVAIL b-sub_rot_dtsul_segur THEN DO:
       ASSIGN i-cont = i-cont + 1.
       CREATE b-sub_rot_dtsul_segur.
       ASSIGN b-sub_rot_dtsul_segur.num_sub_rot_dtsul = sub_rot_dtsul_segur.num_sub_rot_dtsul 
              b-sub_rot_dtsul_segur.cod_grp_usuar     = c-grp_usuar-dst.

    END.
END. 
MESSAGE "Permiss‰es Copiadas: " i-cont VIEW-AS ALERT-BOX.

