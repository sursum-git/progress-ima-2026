FOR EACH rat_financ_cmg
    WHERE cod_tip_fluxo_financ = '2.05'.
    DISP rat_financ_cmg WITH 1 COL WIDTH 550.
    PAUSE 0.
    ASSIGN cod_tip_fluxo_financ = '2.8.1'.
END.

FOR EACH rat_financ_cmg
    WHERE cod_tip_fluxo_financ = '2.04'.
    DISP rat_financ_cmg WITH 1 COL WIDTH 550.
    PAUSE 0.
    ASSIGN cod_tip_fluxo_financ = '2.7.6'.
END.

FOR EACH val_movto_ap
WHERE cod_tip_fluxo_financ = '2.05':
    DISP val_movto_ap WITH 1 COL WIDTH 550. 
    PAUSE 0 .
    ASSIGN cod_tip_fluxo_financ = '2.8.1'.
END.


FOR EACH tip_trans_cx 
WHERE cod_tip_fluxo_financ_saida = '2.09':
    ASSIGN cod_tip_fluxo_financ_saida = '2.5.15'.
    DISP tip_trans_cx WITH 1 COL WIDTH 550.


END.


FOR EACH tip_trans_cx 
WHERE cod_tip_fluxo_financ_saida = '2.09':
    ASSIGN cod_tip_fluxo_financ_saida = '2.5.15'.
    DISP tip_trans_cx WITH 1 COL WIDTH 550.


END.
