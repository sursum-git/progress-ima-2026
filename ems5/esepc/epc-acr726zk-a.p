DEF INPUT PARAMETER p-recid-table AS RECID NO-UNDO.

DEFINE TEMP-TABLE tt-docto
       FIELD cod_empresa     LIKE item_lote_liquidac_acr.cod_empresa  
       FIELD cod_estab       LIKE item_lote_liquidac_acr.cod_estab
       FIELD cod_espec_docto LIKE item_lote_liquidac_acr.cod_espec_docto    
       FIELD cod_ser_docto   LIKE item_lote_liquidac_acr.cod_ser_docto      
       FIELD cod_tit_acr     LIKE item_lote_liquidac_acr.cod_tit_acr   
       FIELD cod_parcela     LIKE item_lote_liquidac_acr.cod_parcela
       FIELD vl-baixa        LIKE item_lote_liquidac_acr.val_liquidac_tit_acr
       INDEX chave cod_empresa cod_estab cod_espec_docto cod_ser_docto cod_tit_acr cod_parcela.

DEFINE BUFFER b-lote_liquidac_acr FOR lote_liquidac_acr.
DEFINE BUFFER b-item_lote_liquidac_acr FOR item_lote_liquidac_acr.
DEFINE BUFFER b-relacto_pend_cheq_acr FOR relacto_pend_cheq_acr.

DEFINE NEW GLOBAL SHARED VARIABLE da-dt-debito       AS DATE FORMAT "99/99/9999".
DEFINE NEW GLOBAL SHARED VARIABLE da-dt-credito      AS DATE FORMAT "99/99/9999".
DEFINE NEW GLOBAL SHARED VARIABLE i-diferenca        AS INTEGER.
DEFINE NEW GLOBAL SHARED VARIABLE v-rec-lote         AS RECID.
    
DEFINE VARIABLE de-vl-acum-cheque AS DECIMAL.
DEFINE VARIABLE de-vl-tot-cheque  AS DECIMAL.
DEFINE VARIABLE de-vl-acum-lote   AS DECIMAL.
DEFINE VARIABLE de-vl-tot-lote    AS DECIMAL.
DEFINE VARIABLE int-count-reg     AS INTEGER.
DEFINE VARIABLE i-cod_empresa     LIKE item_lote_liquidac_acr.cod_empresa.
DEFINE VARIABLE c-cod_refer       LIKE item_lote_liquidac_acr.cod_refer.

ASSIGN v-rec-lote = ?.  /* Zera Variavel Global */
FIND FIRST b-item_lote_liquidac_acr WHERE 
     RECID(b-item_lote_liquidac_acr) = p-recid-table NO-ERROR.

IF AVAILABLE b-item_lote_liquidac_acr THEN
   FIND b-lote_liquidac_acr OF b-item_lote_liquidac_acr NO-LOCK NO-ERROR.
ELSE
   FIND b-lote_liquidac_acr WHERE 
        RECID(b-lote_liquidac_acr) = p-recid-table NO-ERROR.

IF AVAIL b-lote_liquidac_acr THEN DO.
   FOR EACH tt-docto:
       DELETE tt-docto.
   END.

   ASSIGN de-vl-acum-cheque = 0      de-vl-tot-cheque = 0
            de-vl-acum-lote = 0        de-vl-tot-lote = 0.
                                     
   FOR EACH item_lote_liquidac_acr OF b-lote_liquidac_acr NO-LOCK.
       FIND tt-docto WHERE 
            tt-docto.cod_empresa     = item_lote_liquidac_acr.cod_empresa      AND
            tt-docto.cod_estab       = item_lote_liquidac_acr.cod_estab        AND
            tt-docto.cod_espec_docto = item_lote_liquidac_acr.cod_espec_docto  AND
            tt-docto.cod_ser_docto   = item_lote_liquidac_acr.cod_ser_docto    AND
            tt-docto.cod_tit_acr     = item_lote_liquidac_acr.cod_tit_acr      AND
            tt-docto.cod_parcela     = item_lote_liquidac_acr.cod_parcela     
            NO-LOCK NO-ERROR.
       IF NOT AVAILABLE tt-docto THEN DO:
          CREATE tt-docto.
          ASSIGN tt-docto.cod_empresa     = item_lote_liquidac_acr.cod_empresa
                 tt-docto.cod_estab       = item_lote_liquidac_acr.cod_estab
                 tt-docto.cod_espec_docto = item_lote_liquidac_acr.cod_espec_docto
                 tt-docto.cod_ser_docto   = item_lote_liquidac_acr.cod_ser_docto
                 tt-docto.cod_tit_acr     = item_lote_liquidac_acr.cod_tit_acr
                 tt-docto.cod_parcela     = item_lote_liquidac_acr.cod_parcela
                 tt-docto.vl-baixa        = item_lote_liquidac_acr.val_liquidac_tit_acr.
       END.
   END.

   FOR EACH tt-docto NO-LOCK.
       FIND tit_acr WHERE 
            tit_acr.cod_empresa     = tt-docto.cod_empresa     AND
            tit_acr.cod_estab       = tt-docto.cod_estab       AND
            tit_acr.cod_espec_docto = tt-docto.cod_espec_docto AND
            tit_acr.cod_ser_docto   = tt-docto.cod_ser_docto   AND
            tit_acr.cod_tit_acr     = tt-docto.cod_tit_acr     AND
            tit_acr.cod_parcela     = tt-docto.cod_parcela NO-LOCK.
        
       ASSIGN de-vl-acum-lote = de-vl-acum-lote + (tt-docto.vl-baixa * INTEGER(tit_acr.dat_vencto_tit_acr))
              de-vl-tot-lote = de-vl-tot-lote + tt-docto.vl-baixa.
   END.
    
   FOR EACH relacto_pend_cheq_acr WHERE
            relacto_pend_cheq_acr.cod_estab = b-lote_liquidac_acr.cod_estab AND 
            relacto_pend_cheq_acr.cod_refer = b-lote_liquidac_acr.cod_refer NO-LOCK
       BREAK BY relacto_pend_cheq_acr.cod_banco
             BY relacto_pend_cheq_acr.cod_agenc_bcia
             BY relacto_pend_cheq_acr.cod_cta_corren_bco
             BY relacto_pend_cheq_acr.num_cheque.

       IF FIRST-OF(relacto_pend_cheq_acr.num_cheque) THEN DO.

          FIND FIRST cheq_acr OF relacto_pend_cheq_acr NO-LOCK NO-ERROR.

          ASSIGN de-vl-acum-cheque = de-vl-acum-cheque + (cheq_acr.val_cheque * INTEGER(cheq_acr.dat_prev_apres_cheq_acr))
                 de-vl-tot-cheque = de-vl-tot-cheque + cheq_acr.val_cheque.
       END.
   END.

   IF de-vl-acum-lote = 0 THEN
      MESSAGE "Aten‡Æo! Nenhum Documento foi encontrado."
            VIEW-AS ALERT-BOX WARNING BUTTONS OK TITLE "Calculo da Data M‚dia".
   ELSE DO:
      IF de-vl-acum-cheque = 0 THEN
         MESSAGE "Aten‡Æo! Nenhum Cheque foi informado."
              VIEW-AS ALERT-BOX WARNING BUTTONS OK TITLE "Calculo da Data M‚dia".
      ELSE DO:
         ASSIGN da-dt-debito  = DATE(INTEGER(de-vl-acum-lote / de-vl-tot-lote))
                da-dt-credito = DATE(INTEGER(de-vl-acum-cheque / de-vl-tot-cheque))
                i-diferenca   = INTEGER(da-dt-credito) - INTEGER(da-dt-debito).

         RUN esepc/epc-acr726zk-b.w.
      END.
   END.
END.

