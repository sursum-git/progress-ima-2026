DEF TEMP-TABLE tt-movto 
    FIELD data              AS DATE FORMAT "99/99/9999"
    FIELD origem            AS CHAR
    FIELD base              AS CHAR
    FIELD cod_emitente      AS INT
    FIELD desc_emitente     AS CHAR FORMAT "x(15)"
    FIELD cod-tit-acr       LIKE ems5bkp.tit_acr.cod_tit_acr
    FIELD parcela           LIKE ems5bkp.tit_acr.cod_parcela
    FIELD valor-tit         LIKE ems5bkp.tit_acr.val_origin_tit_acr
    FIELD valor-mov         LIKE ems5bkp.tit_acr.val_origin_tit_acr
    FIELD dat-vencto-tit    LIKE ems5bkp.tit_acr.dat_vencto_tit_acr
    FIELD dat-vencto-ori    LIKE ems5bkp.tit_acr.dat_vencto_tit_acr
    FIELD dat-liquid        LIKE ems5bkp.tit_acr.dat_liquidac_tit_acr
    FIELD resultado         AS CHAR
    FIELD num_id_tit        LIKE ems5bkp.tit_acr.num_id_tit_acr 
    FIELD num_renegoc       LIKE ems5bkp.renegoc_acr.num_renegoc_cobr_acr
    FIELD tipo-trans        LIKE ems5bkp.movto_tit_acr.ind_trans_acr_ab
    INDEX indice1 data origem base cod_emitente. 
    
DEF INPUT PARAMETER p-trans AS CHAR.
DEF INPUT PARAMETER p-dt-inicio AS DATE.
DEF INPUT PARAMETER p-dt-final AS DATE.
DEF INPUT-OUTPUT PARAMETER TABLE FOR tt-movto.

FOR EACH ems5bkp.movto_tit_acr WHERE
         ems5bkp.movto_tit_acr.cod_estab = '501' AND
         ems5bkp.movto_tit_acr.ind_trans_acr_ab = p-trans AND
         ems5bkp.movto_tit_acr.dat_trans >= p-dt-inicio AND
         ems5bkp.movto_tit_acr.dat_trans <= p-dt-final 
         USE-INDEX mvtttcr_estab_trans_dtf NO-LOCK .

    FIND ems5bkp.tit_acr OF ems5bkp.movto_tit_acr NO-LOCK NO-ERROR.
    FIND FIRST ems5bkp.cliente WHERE
               ems5bkp.cliente.cdn_cliente = ems5bkp.tit_acr.cdn_cliente NO-LOCK NO-ERROR.

    CREATE tt-movto.
    ASSIGN tt-movto.data           = ems5bkp.movto_tit_acr.dat_trans
           tt-movto.base           = '12'
           tt-movto.cod_emitente   = ems5bkp.tit_acr.cdn_cliente
           tt-movto.desc_emitente  = ems5bkp.cliente.nom_abrev
           tt-movto.cod-tit-acr    = ems5bkp.tit_acr.cod_tit_acr
           tt-movto.parcela        = ems5bkp.tit_acr.cod_parcela
           tt-movto.valor-tit      = ems5bkp.tit_acr.val_origin_tit_acr
           tt-movto.valor-mov      = ems5bkp.movto_tit_acr.val_movto_tit_acr
           tt-movto.num_id_tit     = ems5bkp.tit_acr.num_id_tit_acr
           tt-movto.num_renegoc    = ems5bkp.tit_acr.num_renegoc_cobr_acr
           tt-movto.tipo-trans     = ems5bkp.movto_tit_acr.ind_trans_acr_ab
           tt-movto.dat-vencto-tit = ems5bkp.tit_acr.dat_vencto_tit_acr
           tt-movto.dat-vencto-ori = ems5bkp.tit_acr.dat_vencto_origin_tit_acr
           tt-movto.dat-liquid     = ems5bkp.tit_acr.dat_liquidac_tit_acr.

    IF tt-movto.dat-liquid = 12.31.9999 THEN
       ASSIGN tt-movto.dat-liquid = ?.
END.

