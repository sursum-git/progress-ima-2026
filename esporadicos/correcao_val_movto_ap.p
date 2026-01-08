DEFINE VARIABLE cLista AS CHARACTER   NO-UNDO.
{esp/util.i} 
FOR EACH tit_ap NO-LOCK
    WHERE tit_ap.cdn_fornec = 30089
    AND tit_ap.cod_ser = '3'
    AND   tit_ap.cod_tit_ap = '0142372'
    AND   tit_ap.cod_espec_docto = 'sd' 
    AND   tit_ap.cod_parcela = '8'.
    DISP tit_ap WITH 1 COL WIDTH 550.
    ASSIGN cLista = ''.
    FOR EACH movto_tit_ap OF tit_ap NO-LOCK:
        FOR EACH rat_movto_tit_ap OF movto_tit_ap NO-LOCK.
            RUN incrValor(INPUT-OUTPUT cLista, rat_movto_tit_ap.cod_tip_fluxo_financ,",").
        END.
        MESSAGE cLista SKIP 
               NUM-ENTRIES(cLista,",")
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        IF NUM-ENTRIES(cLista,",") <= 1 THEN DO:
           FOR EACH val_movto_ap OF movto_tit_ap EXCLUSIVE-LOCK .
                DISP val_movto_ap WITH 1 COL WIDTH 550.
                ASSIGN val_movto_ap.cod_tip_fluxo_financ = cLista .

            END. 
        END.
        ELSE
            DISP 'mais de uma conta'.
        
    END.
END.
