/*FOR EACH movto_calcul_normal:
    DISP movto_calcul_normal WITH 1 COL WIDTH 550.
END.*/

/*FOR EACH movto_calcul_func:
    DISP movto_calcul_func WITH 1 COL WIDTH 550 NO-ERROR.
END.*/
DEFINE TEMP-TABLE tt
    FIELD cdn_event LIKE EVENT_fp.cdn_event_fp
    FIELD des_event LIKE EVENT_fp.des_event_fp.

    
DEFINE VARIABLE iCont AS INTEGER     NO-UNDO.
FOR EACH movto_calcul_func BREAK BY movto_calcul_func.cdn_funcionario:
    bloco:
    REPEAT iCont = 1 TO 30:
                  //DISP event_fp.cdn_event_fp des_event_fp.
          FIND FIRST tt
              WHERE tt.cdn_event = movto_calcul_func.cdn_event_fp[iCont]
              NO-ERROR.
          IF NOT AVAIL tt THEN DO:
            CREATE tt.
            ASSIGN tt.cdn_event = movto_calcul_func.cdn_event_fp[iCont].
          END.
    END.
    IF LAST-OF(movto_calcul_func.cdn_funcionario) THEN
       DISP cdn_empresa cdn_funcionario .  PAUSE 0.
END.
OUTPUT TO c:\temp\eventos_utilizados.txt.
FOR EACH tt:
    FIND FIRST EVENT_fp
        WHERE EVENT_fp.cdn_event_fp = tt.cdn_event
        NO-LOCK NO-ERROR.
    IF AVAIL EVENT_fp THEN
       ASSIGN tt.des_event = EVENT_fp.des_event_fp.
    EXPORT DELIMITER "|"  tt.
END.

