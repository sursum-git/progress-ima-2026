DEFINE TEMP-TABLE tt
    FIELD conta AS CHAR .

INPUT FROM u:\retirar_cmg.txt.
REPEAT:
    CREATE tt.
    IMPORT UNFORM tt.conta.
END.
INPUT CLOSE.
    
OUTPUT TO u:\contas_retiradas_cmg.txt.        
FOR EACH cta_ctbl_integr
    WHERE cod_modul_dtsul = 'cmg'
    AND( ind_finalid_ctbl = 'd‚bito padrÆo'
    OR ind_finalid_ctbl = 'cr‚dito padrÆo') :
    FIND FIRST tt
        WHERE tt.conta = cta_ctbl_integr.cod_cta_ctbl NO-ERROR.
    IF AVAIL tt THEN DO:
       DISP tt.conta.
       DELETE cta_ctbl_integr.
    END. 
END.
OUTPUT CLOSE.
