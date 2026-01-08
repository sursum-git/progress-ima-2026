OUTPUT TO c:\temp\correcao_conta_corrente.txt.
FOR EACH movto_cta_corren 
    WHERE cod_cta_corren  = '71537-6'
    AND  dat_movto_cta_corren > 11/30/2014 NO-LOCK:
    DISP movto_cta_corren WITH 1 COL WIDTH 550.
    FOR EACH aprop_ctbl_cmg OF movto_cta_corren:
        IF cod_cta_ctbl = '11102341' THEN DO:
           ASSIGN cod_cta_ctbl = '21103341'.
           DISP aprop_ctbl_cmg WITH 1 COL WIDTH 550 TITLE "apropria‡Æo".
        END.
    END.
END.         
OUTPUT CLOSE.

