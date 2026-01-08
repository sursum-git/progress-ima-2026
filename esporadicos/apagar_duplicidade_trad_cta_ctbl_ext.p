DEFINE BUFFER bf FOR trad_cta_ctbl_ext.
DEFINE VARIABLE R1 AS CHAR      NO-UNDO FORMAT 'x(50)'.
DEFINE VARIABLE R2 AS CHAR      NO-UNDO FORMAT 'x(50)'.
FOR EACH matriz_trad_cta_ctbl_ext NO-LOCK:
    FOR EACH trad_cta_ctbl_ext OF matriz_trad_cta_ctbl_ext BY cod_estab_ext BY cod_cta_ctbl_ext  .
        DISP cod_cta_ctbl cod_cta_ctbl_ext cod_unid_organ cod_estab  num_seq_trad_cta_ctbl_ext WITH WIDTH 550.
        FIND FIRST bf
            WHERE bf.cod_unid_organ             = trad_cta_ctbl_ext.cod_unid_organ
            AND   bf.cod_matriz_trad_cta_ext    = trad_cta_ctbl_ext.cod_matriz_trad_cta_ext
            AND   bf.cod_cta_ctbl_ext           = trad_cta_ctbl_ext.cod_cta_ctbl_ext
            AND   bf.cod_sub_cta_ctbl_ext       = trad_cta_ctbl_ext.cod_sub_cta_ctbl_ext
            AND   bf.cod_ccusto_ext             = trad_cta_ctbl_ext.cod_ccusto_ext              
            AND   bf.cod_estab_ext              = trad_cta_ctbl_ext.cod_estab_ext             
            AND   bf.cod_unid_negoc_ext         = trad_cta_ctbl_ext.cod_unid_negoc_ext        
            /*AND   bf.cod_plano_cta_ctbl         = trad_cta_ctbl_ext.cod_plano_cta_ctbl        
            AND   bf.cod_cta_ctbl               = trad_cta_ctbl_ext.cod_cta_ctbl              
            AND   bf.cod_plano_ccusto           = trad_cta_ctbl_ext.cod_plano_ccusto          
            AND   bf.cod_ccusto                 = trad_cta_ctbl_ext.cod_ccusto                
            AND   bf.cod_estab                  = trad_cta_ctbl_ext.cod_estab                 
            AND   bf.cod_unid_negoc             = trad_cta_ctbl_ext.cod_unid_negoc */ NO-LOCK NO-ERROR.           
        ASSIGN R1 = string(ROWID(BF)).

        FIND LAST bf
            WHERE bf.cod_unid_organ             = trad_cta_ctbl_ext.cod_unid_organ
            AND   bf.cod_matriz_trad_cta_ext    = trad_cta_ctbl_ext.cod_matriz_trad_cta_ext
            AND   bf.cod_cta_ctbl_ext           = trad_cta_ctbl_ext.cod_cta_ctbl_ext
            AND   bf.cod_sub_cta_ctbl_ext       = trad_cta_ctbl_ext.cod_sub_cta_ctbl_ext
            AND   bf.cod_ccusto_ext             = trad_cta_ctbl_ext.cod_ccusto_ext              
            AND   bf.cod_estab_ext              = trad_cta_ctbl_ext.cod_estab_ext             
            AND   bf.cod_unid_negoc_ext         = trad_cta_ctbl_ext.cod_unid_negoc_ext        
            /*AND   bf.cod_plano_cta_ctbl         = trad_cta_ctbl_ext.cod_plano_cta_ctbl        
            AND   bf.cod_cta_ctbl               = trad_cta_ctbl_ext.cod_cta_ctbl              
            AND   bf.cod_plano_ccusto           = trad_cta_ctbl_ext.cod_plano_ccusto          
            AND   bf.cod_ccusto                 = trad_cta_ctbl_ext.cod_ccusto                
            AND   bf.cod_estab                  = trad_cta_ctbl_ext.cod_estab                 
            AND   bf.cod_unid_negoc             = trad_cta_ctbl_ext.cod_unid_negoc*/ EXCLUSIVE-LOCK NO-ERROR.           
        ASSIGN R2 = string(ROWID(BF)).

        DISP R1 R2 r1 <> r2.
    END.
END.

/*Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 cod_unid_organ                   char        im
   20 cod_matriz_trad_cta_ext          char        im
   30 cod_cta_ctbl_ext                 char        im
   40 cod_sub_cta_ctbl_ext             char        im
   50 cod_ccusto_ext                   char        im
   60 cod_estab_ext                    char        im
   70 cod_unid_negoc_ext               char        im
   80 cod_plano_cta_ctbl               char        im
   90 cod_cta_ctbl                     char        im
  100 cod_plano_ccusto                 char        im
  110 cod_ccusto                       char        im
  120 cod_estab                        char        i
  130 cod_unid_negoc                   char        im
  140 des_cta_ctbl_ext                 char
  150 cod_livre_1                      char
  160 cod_livre_2                      char
  170 dat_livre_1                      date
  180 dat_livre_2                      date
  190 log_livre_1                      logi
  200 log_livre_2                      logi
  210 num_livre_1                      inte
  220 num_livre_2                      inte
  230 val_livre_1                      deci-4
  240 val_livre_2                      deci-4
  250 dat_inic_valid                   date
  260 dat_fim_valid                    date
  270 num_seq_trad_cta_ctbl_ext        inte        im
  280 num_natur_trad_cta_ctbl_ext      inte
  290 cdd_version                      deci-0
*/
