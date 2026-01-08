FOR EACH movto_cta_corren
    WHERE movto_cta_corren.cod_cta_corren               = '602170-0'
    AND   movto_cta_corren.dat_movto_cta_corren         = 04.27.2016
    AND   movto_cta_corren.num_seq_movto_cta_corren = 10.
    DISP movto_cta_corren WITH 1 COL WIDTH 550.
    DELETE movto_cta_corren.
END.
