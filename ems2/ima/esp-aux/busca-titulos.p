DEF TEMP-TABLE tt_tit_acr LIKE db-aux.tit_acr.
DEF TEMP-TABLE tt_aprop_ctbl_acr LIKE db-aux.aprop_ctbl_acr.

DEF VAR c-connect AS CHARACTER FORMAT "x(100)" NO-UNDO.
DEF VAR c-service-para AS CHAR.
DEF VAR i-alias AS INT.

DEF VAR de-desconto AS DEC.
DEF VAR de-comissao AS DEC.
DEF VAR i-qt-fat AS INT.
DEF VAR de-vl-tit AS DECIMAL.
DEFINE VARIABLE cEstab AS CHARACTER   NO-UNDO.
DEF INPUT PARAMETER p-data AS DATE FORMAT "99/99/9999".
DEF OUTPUT PARAMETER TABLE FOR tt_tit_acr.
DEF OUTPUT PARAMETER TABLE FOR tt_aprop_ctbl_acr.

OUTPUT TO value('C:\TEMP\LOG_BUSCA-TITULOS-' + string(p-data,'99-99-9999') + '-' + STRING(TIME)  +  '.TXT').
FOR EACH db-aux.tit_acr WHERE
         db-aux.tit_acr.dat_transacao = p-data NO-LOCK.

    PUT "estab:" db-aux.tit_acr.cod_estab
        "titulo:" db-aux.tit_acr.cod_tit_acr 
        "parcela:" db-aux.tit_acr.cod_ser_docto 
        "valor:" db-aux.tit_acr.val_sdo_tit_acr .

    FIND FIRST db-aux.repres_tit_acr WHERE
               db-aux.repres_tit_acr.cod_estab = db-aux.tit_acr.cod_estab AND
               db-aux.repres_tit_acr.num_id_tit_acr = db-aux.tit_acr.num_id_tit_acr AND
               db-aux.repres_tit_acr.cdn_repres = 99999 NO-LOCK NO-ERROR.

    IF NOT AVAIL db-aux.repres_tit_acr THEN DO: 
       PUT "|n∆o achou fulano" SKIP.
       NEXT.

    END.
    ELSE 
       PUT "|achou fulano " .



    ASSIGN de-comissao = db-aux.repres_tit_acr.val_perc_comis_repres.
    FIND estabelec  NO-LOCK
        WHERE estabelec.cod-estabel = db-aux.tit_acr.cod_estab
        NO-ERROR.
    IF AVAIL estabelec THEN DO:
       PUT "|Achou o estab. no ems5".
       ASSIGN cEstab = estabelec.cod-Estabel.
    END.
    ELSE DO:
       PUT "|n∆o achou o estab. no ems5".
       FIND ems5.trad_org_ext NO-LOCK
       WHERE trad_org_ext.cod_tip_unid_organ       = '999' 
       AND   trad_org_ext.cod_matriz_trad_org_ext  = 'ems2'
       AND   trad_org_ext.cod_unid_organ           = db-aux.tit_acr.cod_estab
       NO-ERROR.
       IF AVAIL ems5.trad_org_ext  THEN DO:
          PUT ' - Achou matriz de traduá∆o'.
          ASSIGN cEstab = ems5.trad_org_ext.cod_unid_organ_ext.
       END.                                                    
       ELSE
          PUT '|n∆o achou matriz de traduá∆o tambÇm'.
    END.
    

    FIND db-aux2.nota-fiscal WHERE
         db-aux2.nota-fiscal.cod-estabel    = cEstab   AND
         db-aux2.nota-fiscal.serie          = db-aux.tit_acr.cod_ser_docto      AND
         db-aux2.nota-fiscal.nr-nota-fis    = db-aux.tit_acr.cod_tit_acr    
        NO-LOCK NO-ERROR.

   IF NOT AVAIL db-aux2.nota-fiscal THEN DO:
      PUT "|n∆o achou nota fiscal" SKIP.
      NEXT.
   END.
   ELSE DO:
      PUT '|achou nota fiscal'.
   END.

   /* MESSAGE db-aux.tit_acr.cod_estab     SKIP
            db-aux.tit_acr.cod_ser_docto SKIP
            db-aux.tit_acr.cod_tit_acr   SKIP
             AVAIL nota-fiscal
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
  */
   PUT "achou nota?" AVAIL db-aux2.nota-fiscal.

    ASSIGN i-qt-fat = 0.
    FOR EACH db-aux2.fat-duplic WHERE
             db-aux2.fat-duplic.cod-estabel = db-aux2.nota-fiscal.cod-estabel AND
             db-aux2.fat-duplic.serie = db-aux2.nota-fiscal.serie AND
             db-aux2.fat-duplic.nr-fatura = db-aux2.nota-fiscal.nr-nota-fis NO-LOCK.
        ASSIGN i-qt-fat = i-qt-fat + 1.
    END.
    PUT "|qt.duplicada" i-qt-fat.
    // Retira o Frete do valor do T°tulo
    ASSIGN de-vl-tit = db-aux.tit_acr.val_origin_tit_acr - (db-aux2.nota-fiscal.vl-frete / i-qt-fat).
    PUT "valor titulo:" de-vl-tit.

    ASSIGN de-desconto = (de-vl-tit * 
                         (100 / (100 - de-comissao))) *
                         (de-comissao  / 100).

    PUT "|valor desc.:" de-desconto .

    FIND tt_tit_acr WHERE
         tt_tit_acr.cod_estab       = db-aux.tit_acr.cod_estab      AND
         tt_tit_acr.cod_espec_docto = 'DG'                          AND
         tt_tit_acr.cod_ser_docto   = db-aux.tit_acr.cod_ser_docto  AND
         tt_tit_acr.cod_tit_acr     = db-aux.tit_acr.cod_tit_acr    AND
         tt_tit_acr.cod_parcela     = db-aux.tit_acr.cod_parcela 
         NO-ERROR.
    IF NOT AVAIL tt_tit_acr THEN DO.
       PUT "|n∆o achou tt_tit_acr e vai incluir".
       CREATE tt_tit_acr.
       BUFFER-COPY db-aux.tit_acr TO tt_tit_acr
            ASSIGN tt_tit_acr.cod_espec_docto = 'DG'
                   tt_tit_acr.cod_portador = '9' 
                   tt_tit_acr.cod_cart_bcia = '05' 
                   tt_tit_acr.val_origin_tit_acr = de-desconto
                   tt_tit_acr.val_liq_tit_acr = de-desconto.
    END.
    ELSE DO:           
       PUT "|achou tt_tit_acr e NAO vai incluir".
    END.

    FOR EACH db-aux.aprop_ctbl_acr WHERE
             db-aux.aprop_ctbl_acr.cod_estab             = db-aux.tit_acr.cod_estab            AND  
             db-aux.aprop_ctbl_acr.num_id_movto_tit_acr  = db-aux.tit_acr.num_id_movto_tit_acr NO-LOCK.
         PUT "|criou o aprop".
         CREATE tt_aprop_ctbl_acr.	
         BUFFER-COPY db-aux.aprop_ctbl_acr TO tt_aprop_ctbl_acr
             ASSIGN tt_aprop_ctbl_acr.val_aprop_ctbl = de-desconto.
    END.
    PUT SKIP.
END.
OUTPUT CLOSE.


