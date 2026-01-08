DEF INPUT PARAMETER p-cod_estab       LIKE tit_acr.cod_estab.
DEF INPUT PARAMETER p-cod_espec_docto LIKE tit_acr.cod_espec_docto.
DEF INPUT PARAMETER p-cod_ser_docto   LIKE tit_acr.cod_ser_docto.
DEF INPUT PARAMETER p-cod_tit_acr     LIKE tit_acr.cod_tit_acr.
DEF INPUT PARAMETER p-cod_parcela     LIKE tit_acr.cod_parcela.

DEF TEMP-TABLE tt_tit_acr LIKE tit_acr.
DEF TEMP-TABLE tt_aprop_ctbl_acr LIKE aprop_ctbl_acr.

/* Defini‡Æo de Variaveis */
DEFINE VARIABLE h-server  AS HANDLE NO-UNDO.
DEFINE VARIABLE c-erros   AS CHAR.
DEFINE VARIABLE i-ct      AS INT.
DEFINE VARIABLE c-arq-log AS CHAR.

DEFINE VARIABLE de-desconto AS DEC.
DEFINE VARIABLE de-comissao AS DEC.

CREATE SERVER h-server.
h-server:CONNECT("-AppService datasul-progress-integra-pro -H 192.168.0.38 -S 5162") NO-ERROR.
/* h-server:CONNECT("-AppService datasul-progress-integra-bases -H 192.168.0.38 -S 5162") NO-ERROR. */

IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO.
   DO i-ct = 0 TO ERROR-STATUS:NUM-MESSAGES:
      IF ERROR-STATUS:GET-MESSAGE(i-ct) <> '' THEN
         ASSIGN c-erros = c-erros + ERROR-STATUS:GET-MESSAGE(i-ct) + '   '. 
   END.
   RETURN 'NOK'.
END.

IF NOT h-server:CONNECTED() THEN RETURN 'NOK'.

FIND tit_acr WHERE
     tit_acr.cod_estab       = p-cod_estab AND     
     tit_acr.cod_espec_docto = p-cod_espec_docto AND
     tit_acr.cod_ser_docto   = p-cod_ser_docto AND 
     tit_acr.cod_tit_acr     = p-cod_tit_acr AND   
     tit_acr.cod_parcela     = p-cod_parcela NO-LOCK NO-ERROR.   

FIND FIRST repres_tit_acr WHERE
           repres_tit_acr.cod_estab = tit_acr.cod_estab AND
           repres_tit_acr.num_id_tit_acr = tit_acr.num_id_tit_acr AND
           repres_tit_acr.cdn_repres = 99999 NO-LOCK NO-ERROR.

IF AVAIL repres_tit_acr THEN DO.
   ASSIGN de-comissao = repres_tit_acr.val_perc_comis_repres.
   ASSIGN de-desconto = (tit_acr.val_origin_tit_acr * 
                        (100 / (100 - de-comissao))) * (de-comissao  / 100).

   CREATE tt_tit_acr.
   BUFFER-COPY tit_acr TO tt_tit_acr
        ASSIGN tt_tit_acr.cod_espec_docto = 'DG'
               tt_tit_acr.cod_portador = '9' 
               tt_tit_acr.cod_cart_bcia = '05' 
               tt_tit_acr.val_origin_tit_acr = de-desconto
               tt_tit_acr.val_liq_tit_acr = de-desconto.

   FOR EACH aprop_ctbl_acr WHERE
            aprop_ctbl_acr.cod_estab             = tit_acr.cod_estab            AND  
            aprop_ctbl_acr.num_id_movto_tit_acr  = tit_acr.num_id_movto_tit_acr NO-LOCK.

       CREATE tt_aprop_ctbl_acr.	
       BUFFER-COPY aprop_ctbl_acr TO tt_aprop_ctbl_acr
           ASSIGN tt_aprop_ctbl_acr.val_aprop_ctbl = de-desconto.
   END.

   RUN esp-aux\importa_tit_acr.p (INPUT TABLE tt_tit_acr,
                                  INPUT TABLE tt_aprop_ctbl_acr).

END.
