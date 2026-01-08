OUTPUT TO p:\titapboficial09112014.txt.
 PUT "Origem|Empresa|Estabelecimento|Especie|Documento|Parcela|Fornecedor|Dt.Emissao|Dt.Vencimento|Vl Saldo" SKIP.
 FOR EACH tit-ap
    WHERE tit-ap.vl-saldo > 0.
    EXPORT DELIMITER "|"  "EMS2" ep-codigo cod-estabel cod-esp nr-docto parcela cod-fornec dt-emissao dt-vencimen vl-saldo.
END.

FOR EACH tit_ap
    WHERE log_sdo_tit_ap = YES.
    EXPORT DELIMITER "|" "EMS5" cod_empresa cod_estab cod_espec_docto cod_tit_ap cod_parcela cdn_fornecedor dat_emis_docto dat_vencto_tit_ap
        val_sdo_tit_ap.
END.
OUTPUT CLOSE.
