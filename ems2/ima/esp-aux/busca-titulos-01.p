DEF TEMP-TABLE tt_tit_acr LIKE db-aux.tit_acr.
DEF TEMP-TABLE tt_aprop_ctbl_acr LIKE db-aux.aprop_ctbl_acr.

DEF VAR c-connect AS CHARACTER FORMAT "x(100)" NO-UNDO.
DEF VAR c-service-para AS CHAR.
DEF VAR i-alias AS INT.

DEF VAR de-desconto AS DEC.
DEF VAR de-comissao AS DEC.

DEF INPUT PARAMETER p-data AS DATE FORMAT "99/99/9999".
DEF OUTPUT PARAMETER TABLE FOR tt_tit_acr.
DEF OUTPUT PARAMETER TABLE FOR tt_aprop_ctbl_acr.

OUTPUT TO value('C:\TEMP\LOG_BUSCA-TITULOS' + STRING(TIME)  +  '.TXT').
PUT 'data:' p-data SKIP.

FOR EACH db-aux.tit_acr WHERE
         db-aux.tit_acr.dat_transacao = p-data NO-LOCK.

    PUT "titulo:" db-aux.tit_acr.cod_tit_acr SKIP
        "parcela:" db-aux.tit_acr.cod_tit_acr SKIP
        "valor:" db-aux.tit_acr.val_sdo_tit_acr SKIP.

    FIND FIRST db-aux.repres_tit_acr WHERE
               db-aux.repres_tit_acr.cod_estab = db-aux.tit_acr.cod_estab AND
               db-aux.repres_tit_acr.num_id_tit_acr = db-aux.tit_acr.num_id_tit_acr AND
               db-aux.repres_tit_acr.cdn_repres = 99999 NO-LOCK NO-ERROR.

    IF NOT AVAIL db-aux.repres_tit_acr THEN NEXT.

    PUT 'titulo com repres fulano' SKIP(2).
    
    ASSIGN de-comissao = db-aux.repres_tit_acr.val_perc_comis_repres.

    ASSIGN de-desconto = (db-aux.tit_acr.val_origin_tit_acr * 
                         (100 / (100 - de-comissao))) *
                         (de-comissao  / 100).

    FIND tt_tit_acr WHERE
         tt_tit_acr.cod_estab       = db-aux.tit_acr.cod_estab      AND
         tt_tit_acr.cod_espec_docto = 'DG'                          AND
         tt_tit_acr.cod_ser_docto   = db-aux.tit_acr.cod_ser_docto  AND
         tt_tit_acr.cod_tit_acr     = db-aux.tit_acr.cod_tit_acr    AND
         tt_tit_acr.cod_parcela     = db-aux.tit_acr.cod_parcela 
         NO-ERROR.
    IF NOT AVAIL tt_tit_acr THEN DO.
       CREATE tt_tit_acr.
       BUFFER-COPY db-aux.tit_acr TO tt_tit_acr
            ASSIGN tt_tit_acr.cod_espec_docto = 'DG'
                   tt_tit_acr.cod_portador = '9' 
                   tt_tit_acr.cod_cart_bcia = '05' 
                   tt_tit_acr.val_origin_tit_acr = de-desconto
                   tt_tit_acr.val_liq_tit_acr = de-desconto.
    END.

    FOR EACH db-aux.aprop_ctbl_acr WHERE
             db-aux.aprop_ctbl_acr.cod_estab             = db-aux.tit_acr.cod_estab            AND  
             db-aux.aprop_ctbl_acr.num_id_movto_tit_acr  = db-aux.tit_acr.num_id_movto_tit_acr NO-LOCK.

         CREATE tt_aprop_ctbl_acr.	
         BUFFER-COPY db-aux.aprop_ctbl_acr TO tt_aprop_ctbl_acr
             ASSIGN tt_aprop_ctbl_acr.val_aprop_ctbl = de-desconto.
    END.
END.
OUTPUT CLOSE.


