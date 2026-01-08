DEFINE TEMP-TABLE ttPrevisao
    FIELD num_fluxo_cx              LIKE movto_fluxo_cx.num_fluxo_cx
    FIELD cod_estab                 LIKE movto_fluxo_cx.cod_estab
    FIELD cod_empresa               LIKE movto_fluxo_cx.cod_empresa
    FIELD dat_movto_fluxo_cx        LIKE movto_fluxo_cx.dat_movto_fluxo_cx
    FIELD cod_tip_fluxo_financ      LIKE movto_fluxo_cx.cod_tip_fluxo_financ
    FIELD ind_tip_movto_fluxo_cx    LIKE movto_fluxo_cx.ind_tip_movto_fluxo_cx
    FIELD ind_fluxo_movto_cx        LIKE movto_fluxo_cx.ind_fluxo_movto_cx
    FIELD cod_modul_dtsul           LIKE movto_fluxo_cx.cod_modul_dtsul
    FIELD des_histor_movto_fluxo_cx LIKE movto_fluxo_cx.des_histor_movto_fluxo_cx
    FIELD val_movto_fluxo_cx        LIKE movto_fluxo_cx.val_movto_fluxo_cx .
    




/*Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 num_fluxo_cx                     inte        im
   20 dat_movto_fluxo_cx               date        im
   30 num_seq_movto_fluxo_cx           inte        im
   40 cod_estab                        char        im
   50 cod_unid_negoc                   char        im
   60 cod_tip_fluxo_financ             char        im
   70 ind_fluxo_movto_cx               char        m
   80 ind_tip_movto_fluxo_cx           char        im
   90 cod_modul_dtsul                  char        im
  100 val_movto_fluxo_cx               deci-2      m
  110 val_perc_cop_fluxo_cx            deci-2      m
  120 cod_histor_padr                  char        im
  130 des_histor_movto_fluxo_cx        char        im
  140 cod_empresa                      char        m
  150 ind_tip_secao_fluxo_cx           char        m
  160 num_id_movto_fluxo_cx            inte        im
  170 cod_livre_1                      char
  180 dat_prev_orig                    date
  190 cod_estab_orig                   char        m
  200 cod_livre_2                      char
  210 dat_livre_1                      date
  220 dat_livre_2                      date
  230 log_livre_1                      logi
  240 log_livre_2                      logi
  250 num_livre_1                      inte
  260 num_livre_2                      inte
  270 val_livre_1                      deci-4
  280 val_livre_2                      deci-4
*/
