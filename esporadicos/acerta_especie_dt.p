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

FOR EACH movto_tit_ap
      WHERE movto_tit_ap.dat_transacao >= 10.01.2014
      AND movto_tit_ap.cod_espec_docto = 'AT'
      /*AND   movto_tit_a.dat_transacao <= 11.30.2014*/
      NO-LOCK.
      
      FOR EACH aprop_ctbl_ap OF movto_tit_ap 
          WHERE cod_cta_ctbl = '21202001' EXCLUSIVE-LOCK.
          ASSIGN cod_cta_ctbl = '11306004'.
          
      END.
  END.

