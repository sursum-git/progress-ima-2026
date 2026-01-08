DEFINE TEMP-TABLE ttCompCR
       FIELD cod_estab              LIKE tit_acr.cod_estab
       FIELD nome_emit              LIKE emitente.nome-emit
       FIELD cod_espec_docto        LIKE tit_acr.cod_espec_docto
       FIELD cod_ser_docto          LIKE tit_acr.cod_ser_docto
       FIELD cod_parcela            LIKE tit_acr.cod_parcela
       FIELD cdn_cliente            LIKE tit_acr.cdn_cliente
       FIELD cod_tit_acr            LIKE tit_acr.cod_tit_acr
       FIELD dat_transacao          LIKE tit_acr.dat_transacao
       FIELD dat_liquidac_tit_acr   LIKE movto_tit_acr.dat_liquidac_tit_acr
       FIELD val_movto_tit_ar       LIKE movto_tit_acr.val_movto_tit_acr
       FIELD cod_espec_docto_comp   LIKE tit_acr.cod_espec_docto
       FIELD cod_ser_docto_comp     LIKE tit_acr.cod_ser_docto
       FIELD cod_parcela_comp       LIKE tit_acr.cod_parcela
       FIELD cod_tit_acr_comp       LIKE tit_acr.cod_tit_acr .
