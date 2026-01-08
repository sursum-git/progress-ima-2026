
DEFINE BUFFER bf FOR    cta_ctbl_integr.  
DEFINE TEMP-TABLE tt
    FIELD conta AS CHAR.

OUTPUT TO u:\contas_sincronismo_cmg_apb.txt.        
FOR EACH cta_ctbl_integr
    WHERE cod_modul_dtsul = 'apb'
    AND ind_finalid_ctbl = 'conta movimento'
    AND  SUBSTR(cod_cta_ctbl,1,1) = '4' :
    /*DISP cta_ctbl_integr.cod_cta_ctbl ind_finalid_ctbl.*/

    FIND FIRST bf
        WHERE bf.cod_modul_dtsul = 'cmg'
        AND   (bf.ind_finalid_ctbl = 'd‚bito padrÆo'
        OR    bf.ind_finalid_ctbl  = 'cr‚dito padrÆo')
        AND   bf.cod_cta_ctbl      = cta_ctbl_integr.cod_cta_ctbl
        NO-ERROR.
    IF NOT AVAIL bf THEN DO:
       CREATE tt.
       ASSIGN tt.conta = cta_ctbl_integr.cod_cta_ctbl.
    END. 
END.

FOR EACH tt:
    DISP tt.
    CREATE cta_ctbl_integr.
    ASSIGN cta_ctbl_integr.cod_modul_dtsul = 'CMG'
            cod_plano_cta_ctbl             = 'fiscal'
            cod_cta_ctbl                   = tt.conta
            dat_inic_valid                 = 01.01.0001
            dat_fim_valid                  = 12.31.9999
            ind_finalid_ctbl               = 'd‚bito padrÆo'
            dat_livre_1                    = TODAY 
            dat_livre_2                    = TODAY.

    CREATE cta_ctbl_integr.
    ASSIGN cta_ctbl_integr.cod_modul_dtsul = 'CMG'
            cod_plano_cta_ctbl             = 'fiscal'
            cod_cta_ctbl                   = tt.conta
            dat_inic_valid                 = 01.01.0001
            dat_fim_valid                  = 12.31.9999
            ind_finalid_ctbl               = 'cr‚dito padrÆo'
            dat_livre_1                    = TODAY
            dat_livre_2                    = TODAY .

END.

/*
 Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 cod_modul_dtsul                  char        im
   20 cod_plano_cta_ctbl               char        im
   30 cod_cta_ctbl                     char        im
   40 dat_inic_valid                   date        m
   50 dat_fim_valid                    date        m
   60 ind_finalid_ctbl                 char        im
   70 des_anot_tab                     char        m
   80 cod_livre_1                      char
   90 log_livre_1                      logi
  100 num_livre_1                      inte
  110 val_livre_1                      deci-4
  120 dat_livre_1                      date
  130 cod_livre_2                      char
  140 dat_livre_2                      date
  150 log_livre_2                      logi
  160 num_livre_2                      inte
  170 val_livre_2                      deci-4
  180 cdd_version                      deci-0
 
  
  
*/






OUTPUT CLOSE.
