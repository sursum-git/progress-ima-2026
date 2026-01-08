DEFINE TEMP-TABLE ttTituloAP
    FIELD cod_empresa        LIKE tit_ap.cod_empresa
    FIELD cod_estab          LIKE tit_ap.cod_estab
    FIELD cod_tit_ap         LIKE tit_ap.cod_tit_ap
    FIELD cod_parcela        LIKE tit_ap.cod_parcela
    FIELD cod_ser_docto      LIKE tit_ap.cod_ser_docto 
    FIELD cod_espec_docto    LIKE tit_ap.cod_espec_docto
    FIELD cdn_fornecedor     LIKE tit_ap.cdn_fornecedor
    FIELD nom_abrev          AS CHAR FORMAT 'x(20)'
    FIELD dat_emis_docto     LIKE tit_ap.dat_emis_docto
    FIELD dat_vencto_tit_ap  LIKE tit_ap.dat_vencto_tit_ap
    FIELD val_origin_tit_ap  LIKE tit_ap.val_origin_tit_ap 
    FIELD val_sdo_tit_ap     LIKE tit_ap.val_sdo_tit_ap 
    FIELD situacao           AS  LOGICAL FORMAT 'Vencido\A Vencer'
    FIELD tipo_fluxo         LIKE val_tit_ap.cod_tip_fluxo_financ .






