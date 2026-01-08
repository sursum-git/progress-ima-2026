DEFINE VARIABLE d AS DECIMAL     NO-UNDO.
DEFINE VARIABLE c AS DECIMAL     NO-UNDO.
ASSIGN d = 0
       c = 0.
OUTPUT TO c:\temp\corrigeEspec.txt.
FOR EACH movto_tit_ap NO-LOCK
     WHERE( cod_espec_docto = 'ci'
         OR cod_espec_docto = 'pi'
         OR cod_espec_docto = 'FT')
    AND cod_usuario = 'sfaria'
    AND cod_empresa = '500' :
      DISP cod_tit_ap cod_espec_docto  dat_transacao.
    FOR EACH aprop_ctbl_ap OF movto_tit_ap
        WHERE cod_cta_ctbl = '19000005' EXCLUSIVE-LOCK.
        ASSIGN cod_cta_ctbl = '19000002'.
        DISP aprop_ctbl_ap WITH 1 COL WIDTH 550.

        /*WHERE cod_cta_ctbl = '11102341'*/ .
        /*FIND FIRST movto_cta_corren OF movto_tit_acr
            WHERE cod_cta_corren = '71537-6' NO-LOCK NO-ERROR.*/
        
                                                 
    END.
END.       
OUTPUT CLOSE.

