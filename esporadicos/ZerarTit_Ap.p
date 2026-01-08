OUTPUT TO c:\temp\tit_ap.txt.

PUT "Fornecedor | Num. Doc | Esp‚cie | Parcela | Dt. Emis | Saldo" SKIP.

FOR EACH tit_ap WHERE
         tit_ap.val_sdo_tit_ap > 0 AND
         tit_ap.dat_vencto_tit_ap <= 12.31.2015.

    EXPORT DELIMITER "|" tit_ap.cdn_fornecedor
                         tit_ap.cod_tit_ap
                         tit_ap.cod_espec_docto
                         tit_ap.cod_parcela
                         tit_ap.dat_emis_docto
                         tit_ap.val_origin_tit_ap
                         tit_ap.val_sdo_tit_ap.
    ASSIGN tit_ap.LOG_sdo_tit_ap = NO
           tit_ap.val_sdo_tit_ap = 0
           tit_ap.cod_livre_1    = 'Saldo Zerado conforme acordado em reuniÆo do dia 07-04-2017'
           tit_ap.LOG_livre_1 = YES.
                       


END.                     

OS-COMMAND SILENT VALUE ("start excel /t d:\tit_ap.xlsx").
