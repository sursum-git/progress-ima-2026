
FOR EACH movto_tit_ap
   WHERE cod_estab = '501'
    AND  cod_refer = '150420qhu'
   USE-INDEX mvtttp_refer:
    DISP movto_tit_ap WITH 1 COL WIDTH 550 TITLE "movimento".
    FOR EACH relacto_tit_ap OF movto_tit_ap:
        DISP relacto_tit_ap WITH 1 COL WIDTH 550 TITLE "relacionamento titulos".
    END.
END.

