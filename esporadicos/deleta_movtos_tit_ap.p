OUTPUT TO c:\temp\bkp.txt.
FOR EACH tit_ap
    WHERE cdn_fornec = 26715
    AND cod_estab = '501'
    AND cod_tit_ap = '1679/17'
    AND cod_parcela = '01'
    AND cod_espec_docto = 'af'.
    FOR EACH movto_tit_ap OF tit_ap.
       DISP movto_tit_ap WITH 1 COL WIDTH 550.
       MESSAGE 'deseja excluir?' UPDATE lresposta AS LOG
           VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL .
       IF lResposta THEN DO:
           EXPORT movto_tit_ap.
          DELETE movto_tit_ap.
       END.
          

    END.
END.
