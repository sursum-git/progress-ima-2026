DEFINE TEMP-TABLE ttTitulo
    FIELD cod_empresa        LIKE tit_acr.cod_empresa
    FIELD cod_estab          LIKE tit_acr.cod_estab
    FIELD cod_tit_acr        LIKE tit_acr.cod_tit_acr
    FIELD cod_parcela        LIKE tit_acr.cod_parcela
    FIELD cod_ser_docto      LIKE tit_acr.cod_ser_docto 
    FIELD cod_espec_docto    LIKE tit_acr.cod_espec_docto
    FIELD cdn_cliente        LIKE tit_acr.cdn_cliente
    FIELD nom_abrev          LIKE tit_acr.nom_abrev
    FIELD dat_emis_docto     LIKE tit_acr.dat_emis_docto
    FIELD dat_vencto_tit_acr LIKE tit_acr.dat_vencto_tit_acr
    FIELD dat_fluxo_tit_acr  LIKE tit_acr.dat_fluxo_tit_acr
    FIELD val_origin_tit_acr LIKE tit_acr.val_origin_tit_acr 
    FIELD val_sdo_tit_acr    LIKE tit_acr.val_sdo_tit_acr 
    FIELD situacao           AS  LOGICAL
    FIELD tipo_fluxo         LIKE val_tit_acr.cod_tip_fluxo_financ.
