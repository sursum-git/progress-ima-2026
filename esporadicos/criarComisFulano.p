DEFINE TEMP-TABLE tt LIKE repres_tit_acr .
DEFINE VARIABLE perc AS DECIMAL     NO-UNDO.
UPDATE perc.
FOR EACH tit_acr
    WHERE tit_acr.cod_estab = '501'
    AND   tit_acr.cod_espec_docto  = 'dp'
    AND   tit_acr.cod_ser_docto = '3' 
    AND   tit_acr.cod_tit_acr = '0134772'
    AND   tit_acr.cod_parcela = '01'  .

    FOR EACH repres_tit_acr 
        WHERE repres_tit_Acr.cod_estab          =  tit_acr.cod_Estab
        AND   repres_tit_acr.num_id_tit_acr     = tit_Acr.num_id_tit_acr
        AND   repres_tit_acr.cdn_repres         = tit_Acr.cdn_repres .

        //DISP repres_tit_acr WITH 1 COL WIDTH 550.
        CREATE tt.
        BUFFER-COPY repres_tit_acr TO tt.


    END.
    
    
END.

FOR EACH tt:
    ASSIGN tt.cdn_repres            = 99999
           tt.val_perc_comis_repres =  perc .
    FIND repres_tit_Acr OF tt NO-LOCK NO-ERROR.

    IF NOT AVAIL repres_tit_acr THEN DO:
       CREATE repres_tit_acr.
       BUFFER-COPY tt TO repres_tit_acr . 
    END.

END.
