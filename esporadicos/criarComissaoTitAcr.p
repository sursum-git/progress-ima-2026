DEFINE INPUT  PARAMETER pRowid AS ROWID       NO-UNDO.
DEFINE VARIABLE cEstab AS CHARACTER   NO-UNDO.
FIND ems5.tit_acr
    WHERE ROWID(tit_acr) = pRowid NO-LOCK NO-ERROR.
IF NOT AVAIL tit_acr THEN NEXT.
IF LENGTH(cEstab) = 1 THEN
   ASSIGN cEstab = IF substr(tit_acr.cod_estab,1,1) = '5' THEN '5' ELSE '1'.
ELSE
   ASSIGN cEstab = tit_acr.cod_estab.

FOR EACH  fat-repre
    WHERE fat-repre.cod-estabel = cEstab
    AND   fat-repre.serie       = tit_acr.cod_ser_docto
    AND   fat-repre.nr-fatura   = tit_acr.cod_tit_acr     NO-LOCK .
    
    FIND FIRST repres 
        WHERE repres.nome-abrev = fat-repre.nome-ab-rep NO-LOCK NO-ERROR.
    IF NOT AVAIL repres THEN NEXT.
    FIND FIRST repres_tit_acr
        WHERE repres_tit_acr.cod_estab = tit_acr.cod_estab
        AND   repres_tit_acr.num_id_tit_acr = tit_acr.num_id_tit_acr
        AND   repres_tit_acr.cdn_repres = repres.cod-rep NO-LOCK NO-ERROR.

    IF NOT AVAIL repres_tit_acr THEN DO:
         CREATE repres_tit_acr.
         ASSIGN 
         repres_tit_acr.cod_empresa                  = tit_acr.cod_empresa
         repres_tit_acr.cod_estab                    = tit_acr.cod_estab
         repres_tit_acr.cod_comis_vda_estab          = 'padrao'
         repres_tit_acr.cod_espec_docto              = tit_acr.cod_espec_docto
         repres_tit_acr.num_id_tit_acr               = tit_acr.num_id_tit_acr
         repres_tit_acr.cdn_repres                   = repres.cod-rep
         repres_tit_acr.cdn_cliente                  = tit_acr.cdn_cliente
         repres_tit_acr.val_perc_comis_repres        = fat-repre.perc-comis
         repres_tit_acr.val_perc_comis_repres_emis   = 0
         repres_tit_acr.val_perc_comis_abat          = 100
         repres_tit_acr.val_perc_comis_desc          = 100
         repres_tit_acr.val_perc_comis_juros         =  0
         repres_tit_acr.val_perc_comis_multa         =  0
         repres_tit_acr.val_perc_comis_acerto_val    =  100
         repres_tit_acr.log_comis_repres_proporc     =  NO
         repres_tit_acr.ind_sit_comis_vda            = 'implantado'
         repres_tit_acr.ind_tip_comis                = 'valor bruto'
         repres_tit_acr.ind_forma_pagto_comis        = 'pagamento'
         repres_tit_acr.ind_tip_comis_ext            = 'nenhum'
         repres_tit_acr.ind_liber_pagto_comis        = 'nenhum'
         repres_tit_acr.ind_sit_comis_ext            = 'nenhum'
         repres_tit_acr.val_base_calc_comis          = fat-repre.vl-base-calc-comis
         repres_tit_acr.log_sdo_tit_acr              = YES
         repres_tit_acr.dat_ult_liquidac_tit_acr     = 12.31.9999.

    END.
END.


                                            






































/*Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 cod_empresa                      char        im
   20 cod_estab                        char        im
   30 cod_comis_vda_estab              char        im
   40 cod_espec_docto                  char        m
   50 num_id_tit_acr                   inte        im
   60 cdn_repres                       inte        im
   70 cdn_cliente                      inte        im
   80 val_perc_comis_repres            deci-4      m
   90 val_perc_comis_repres_emis       deci-2      m
  100 val_perc_comis_abat              deci-2      m
  110 val_perc_comis_desc              deci-2      m
  120 val_perc_comis_juros             deci-2      m
  130 val_perc_comis_multa             deci-2      m
  140 val_perc_comis_acerto_val        deci-2      m
  150 log_comis_repres_proporc         logi        m
  160 ind_sit_comis_vda                char        m
  170 ind_tip_comis                    char        m
  180 ind_forma_pagto_comis            char        m
  190 cod_livre_1                      char
  200 log_livre_1                      logi
  210 dat_livre_1                      date
  220 val_perc_comis_renegoc           deci-4      m
  230 ind_tip_comis_ext                char
  240 ind_liber_pagto_comis            char
  250 ind_sit_comis_ext                char
  260 val_base_calc_comis              deci-2      m
  270 cod_livre_2                      char
  280 dat_livre_2                      date
  290 log_livre_2                      logi
  300 num_livre_1                      inte
  310 num_livre_2                      inte
  320 val_livre_1                      deci-4
  330 val_livre_2                      deci-4
  340 log_sdo_tit_acr                  logi        i
  350 dat_ult_liquidac_tit_acr         date        i
*/
