
DEF VAR c-connect AS CHARACTER FORMAT "x(100)" NO-UNDO.
DEF VAR c-service-para AS CHAR.
DEF VAR i-alias AS INT.


DEF VAR de-desconto AS DEC.
DEF VAR de-comissao AS DEC.

DEF TEMP-TABLE tt_tit_acr LIKE tit_acr.

DEF OUTPUT PARAMETER tt_tit_acr.

RUN esapi/troca-base-ems5.p.

FOR FIRST db-aux.tit_acr WHERE
          db-aux.tit_acr.dat_transacao = 11.03.2014.

    FIND FIRST db-aux.repres_tit_acr WHERE
               db-aux.repres_tit_acr.cod_estab = db-aux.tit_acr.cod_estab AND
               db-aux.repres_tit_acr.num_id_tit_acr = db-aux.tit_acr.num_id_tit_acr AND
               db-aux.repres_tit_acr.cdn_repres = 99999 NO-LOCK NO-ERROR.

    IF NOT AVAIL db-aux.repres_tit_acr THEN NEXT.

    ASSIGN de-comissao = db-aux.repres_tit_acr.val_perc_comis_repres.

    ASSIGN de-desconto = (db-aux.tit_acr.val_origin_tit_acr * 
                         (100 / (100 - de-comissao))) *
                         (de-comissao  / 100).

    CREATE tt_tit_acr.
    BUFFER-COPY db-aux.tit_acr TO tt_tit_acr
        ASSIGN tt_tit_acr.cod_espec_docto = 'DG'
               tt_tit_acr.val_origin_tit_acr = de-desconto.
END.

DISCONNECT db-aux.


RUN esp-aux\importa_tit_acr.p (INPUT TABLE tt_tit_acr).

