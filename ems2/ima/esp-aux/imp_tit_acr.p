DEF TEMP-TABLE tt_tit_acr LIKE ems5.tit_acr.
DEF TEMP-TABLE tt_aprop_ctbl_acr LIKE ems5.aprop_ctbl_acr.

DEF VAR d-data-ini AS DATE FORMAT "99/99/9999".

ASSIGN d-data-ini = 01.12.2015.

IF CONNECTED("db-aux") THEN
   DISCONNECT db-aux.

CONNECT -db ems5 -ld db-aux -S 10032 -H 192.168.0.44 -N tcp.
RUN esp-aux/busca-titulos.p (INPUT d-data-ini,
                             OUTPUT TABLE tt_tit_acr,
                             OUTPUT TABLE tt_aprop_ctbl_acr). 
DISCONNECT db-aux.


FOR EACH tt_tit_acr NO-LOCK.

    FIND tit_acr WHERE
         tit_acr.cod_estab        = tt_tit_acr.cod_estab      AND
         tit_acr.cod_espec_docto  = tt_tit_acr.cod_espec_docto AND
         tit_acr.cod_ser_docto    = tt_tit_acr.cod_ser_docto AND 
         tit_acr.cod_tit_acr      = tt_tit_acr.cod_tit_acr AND   
         tit_acr.cod_parcela      = tt_tit_acr.cod_parcela 
         NO-LOCK NO-ERROR.
    IF AVAIL tit_acr THEN NEXT.

    DISP tt_tit_acr.cod_empresa
         tt_tit_acr.cod_estab
         tt_tit_acr.cdn_cliente
         tt_tit_acr.cod_espec_docto
         tt_tit_acr.cod_tit_acr
         tt_tit_acr.cod_refer
         tt_tit_acr.dat_transacao
         tt_tit_acr.cod_parcela
         tt_tit_acr.val_origin_tit_acr
         tt_tit_acr.cod_portador
         WITH WIDTH 550.
END.


RUN esp-aux\importa_tit_acr.p (INPUT TABLE tt_tit_acr,
                               INPUT TABLE tt_aprop_ctbl_acr).

