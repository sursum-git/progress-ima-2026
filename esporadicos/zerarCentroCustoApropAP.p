FOR EACH aprop_ctbl_ap 
    WHERE cod_cta_ctbl = '41500004'
    AND cod_estab = '501'
    AND dat_transacao >= 01.01.2015
    AND dat_transacao  < 07.01.2015 EXCLUSIVE-LOCK.
    DISP   cod_plano_ccusto           
           cod_ccusto 
        dat_transacao.   
    ASSIGN  cod_plano_ccusto = ''           
            cod_ccusto        = ''.


END.
    /*Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 cod_empresa                      char        m
   20 cod_estab                        char        im
   30 cod_estab_aprop_ctbl             char        im
   40 cod_plano_cta_ctbl               char        im
   50 cod_cta_ctbl                     char        im
   60 cod_unid_negoc                   char        im
   70 cod_plano_ccusto                 char        im
   80 cod_ccusto                       char        im
   90 cod_indic_econ                   char        m
  100 ind_natur_lancto_ctbl            char        im
  110 ind_tip_aprop_ctbl               char        m
  120 ind_gera_val_aprop_ctbl_ap       char        m
  130 val_aprop_ctbl                   deci-2      m
  140 num_id_movto_tit_ap              inte        im
  150 num_id_aprop_ctbl_ap             inte        im
  160 num_livro_fisc                   inte
  170 num_pag_livro_fisc               inte
  180 log_ctbz_aprop_ctbl              logi        m
  190 log_impto_val_agreg              logi        m
  200 dat_transacao                    date        im
  210 cod_livre_1                      char
  220 cod_livre_2                      char
  230 dat_livre_1                      date
  240 dat_livre_2                      date
  250 log_livre_1                      logi
  260 log_livre_2                      logi
  270 num_livre_1                      inte
  280 num_livre_2                      inte
  290 val_livre_1                      deci-4
  300 val_livre_2                      deci-4
*/
