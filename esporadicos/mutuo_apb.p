DEFINE TEMP-TABLE tt
    FIELD cod_empresa       AS CHAR
    FIELD data_transacao    AS DATE
    FIELD valor             AS DECIMAL
    FIELD cod_modulo        AS CHAR
    FIELD num_documento     AS CHAR FORMAT "x(100)"
    FIELD cod_emitente      AS INT
    FIELD cod_serie         AS CHAR FORMAT "x(100)"
    FIELD cod_parcela       AS CHAR FORMAT "x(100)"
    FIELD cod_especie       AS CHAR FORMAT "x(100)"
    FIELD cod_cta_ctbl      AS CHAR FORMAT "X(20)".

FOR EACH movto_tit_ap_fechado
      WHERE movto_tit_ap_fechado.dat_transacao >= 11.01.2014
      AND   movto_tit_ap_fechado.dat_transacao <= 11.30.2014
      NO-LOCK.
      FIND FIRST tit_ap 
           WHERE tit_ap.cod_estab = movto_tit_ap_fechado.cod_estab 
          AND tit_ap.num_id_tit_ap =  movto_tit_ap_fechado.num_id_tit_ap NO-LOCK NO-ERROR.
      FOR EACH aprop_ctbl_ap OF movto_tit_ap_fechado NO-LOCK.
         CREATE tt.
         ASSIGN tt.cod_empresa      = movto_tit_ap_fechado.cod_empresa
                tt.data_transacao   = movto_tit_ap_fechado.dat_transacao
                tt.valor            = aprop_ctbl_ap_fechado.val_aprop_ctbl
                tt.cod_modulo       = "APB"
                tt.num_documento    = movto_tit_ap_fechado.cod_tit_ap
                tt.cod_emitente     = movto_tit_ap_fechado.cdn_fornecedor
                tt.cod_serie        = IF AVAIL tit_ap THEN tit_ap.cod_ser_docto ELSE ''
                tt.cod_parcela      = IF AVAIL tit_ap THEN tit_ap.cod_parcela ELSE ''
                tt.cod_especie      = IF AVAIL tit_ap THEN tit_ap.cod_espec_docto  ELSE ''
                tt.cod_cta_ctbl     = aprop_ctbl_ap.cod_cta_ctbl.

      END.
  END.



  OUTPUT TO c:\temp\mutuo_apb.txt.
  FOR EACH tt:
      EXPORT DELIMITER "|" tt.
  END.

  OUTPUT CLOSE.
