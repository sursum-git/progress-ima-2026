OUTPUT TO c:\temp\aprop_ctbl_cmg.txt.
FOR EACH aprop_ctbl_cmg
    WHERE cod_cta_ctbl = '41800003'
    AND dat_transacao >= 06.01.2015 :
    ASSIGN cod_plano_ccusto = ''
           cod_ccusto       = '' .
    DISP aprop_ctbl_cmg WITH 1 COL WIDTH 550.

END.
OUTPUT CLOSE.
