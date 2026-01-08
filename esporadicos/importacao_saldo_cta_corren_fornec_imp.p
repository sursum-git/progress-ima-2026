DEFINE TEMP-TABLE tt 
    FIELD fornecedor_id AS INT
    FIELD nome AS CHAR FORMAT 'X(20)'
    FIELD valor AS DECIMAL.

INPUT FROM c:\temp\saldo_fornec.csv.
REPEAT:
    CREATE tt.
    IMPORT DELIMITER ";" tt.
END.

INPUT CLOSE.

FOR EACH tt:

    CREATE cta_corren_fornec_imp.
    ASSIGN cta_corren_fornec_imp.cta_corren_fornec_imp_id = next-value(seq_cta_corren_fornec_imp)
           cta_corren_fornec_imp.fornecedor_id = tt.fornecedor_id
            cta_corren_fornec_imp.data = TODAY - 1
            cta_corren_fornec_imp.historico = 'Saldo passado pela Sra. Ana Fl via ao fornecedor:' 
       + tt.nome
        cta_corren_fornec_imp.data_hora = NOW
        cta_corren_fornec_imp.vl_lancto = tt.valor
        cta_corren_fornec_imp.cod_tipo_movto = 1
        cta_corren_fornec_imp.cod_acao = 1.
END.


/*Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 cta_corren_fornec_imp_id         inte        i
   20 fornecedor_id                    inte        i
   30 data                             date        i
   60 historico                        char
   80 cod_tipo_movto                   inte        i
   90 vl_lancto                        deci-2
  100 cod_acao                         inte
  110 login                            char
  120 data_hora                        datetm      i
  140 num_id_tit_ap                    inte        i
  150 item_proc_compra_id              inte        i
  160 item_container_id                inte
  170 transacao_id                     inte        i
  */
