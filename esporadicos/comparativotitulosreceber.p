DEFINE VARIABLE cArquivo AS CHARACTER   NO-UNDO FORMAT 'x(100)'.
UPDATE cArquivo WITH WIDTH 350.

OUTPUT TO value(cArquivo).
PUT "origem|Empresa|Estabelecimento|Especie|documento|parcela|Emitente|Dt.EmissÆo|Dt.Vencimento|Saldo" SKIP.
FOR EACH titulo WHERE vl-saldo > 0 :
    EXPORT DELIMITER "|" "ems2" 
        ep-codigo 
        cod-estabel 
        cod-esp
        nr-docto 
        parcela 
        cod-emitente 
        dt-emissao 
        dt-vencimen 
        vl-saldo.
END.
FOR EACH tit_acr NO-LOCK
    WHERE  log_sdo_tit_acr = YES:
    EXPORT DELIMITER "|"  "ems5"
        cod_empresa                    
        cod_estab                      
        cod_espec_docto                
        cod_tit_acr
        cod_parcela                    
        cdn_cliente  
        dat_emis_docto           
        dat_vencto_tit_acr       
        val_sdo_tit_acr.
END.
OUTPUT CLOSE.
/*

=========================================================================
============================= Table: tit_acr ============================

              Table Flags: "f" = frozen, "s" = a SQL table


Table                            Table Field Index Table
Name                             Flags Count Count Label
-------------------------------- ----- ----- ----- ------------------------
tit_acr                                  120    21 T¡tulo Contas a Receber

    Dump Name: fin490
  Description: T¡tulo Contas a Receber
 Storage Area: Schema Area

       Trigger Event Trigger Procedure    Overridable? Check CRC?
       ------------- -------------------- ------------ ----------
       Create        database/tgfin/tcp/t yes          no
       Delete        database/tgfin/tdp/t yes          no
       Write         database/tgfin/twp/t yes          no


============================= FIELD SUMMARY =============================
============================= Table: tit_acr ============================

Flags: <c>ase sensitive, <i>ndex component, <m>andatory, <v>iew component

Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 cod_empresa                      char        im
   20 cod_estab                        char        im
   30 cod_espec_docto                  char        im
   40 cod_ser_docto                    char        im
   50 cod_tit_acr                      char        im
   60 cod_parcela                      char        im
   70 cdn_cliente                      inte        im
   80 cdn_clien_matriz                 inte        m
   90 cdn_repres                       inte        i
  100 nom_abrev                        char        im
  110 nom_abrev_contat                 char
  120 num_pessoa                       inte        im
  130 num_fatur_acr                    inte
  140 num_id_movto_tit_acr_ult         inte
  150 num_id_tit_acr                   inte        im
  160 num_id_movto_cta_corren          inte        im
  170 num_bord_acr                     inte        im
  180 num_renegoc_cobr_acr             inte        m
  190 ind_orig_tit_acr                 char        m
  200 ind_sit_tit_acr                  char        m
  210 ind_tip_espec_docto              char        im
  220 ind_sit_bcia_tit_acr             char        m
  230 ind_tip_cobr_acr                 char
  240 ind_ender_cobr                   char        m
  250 dat_transacao                    date        im
  260 dat_emis_docto                   date        im
  270 dat_vencto_tit_acr               date        im
  280 dat_desconto                     date
  290 dat_prev_liquidac                date        im
  300 dat_fluxo_tit_acr                date        im
  310 dat_ult_liquidac_tit_acr         date
  320 dat_ult_apurac_variac_val        date
  330 dat_liquidac_tit_acr             date        im
  340 dat_abat_tit_acr                 date
  350 dat_vencto_origin_tit_acr        date        m
  360 dat_ult_aprop_despes_financ      date        m
  370 dat_alter_portad                 date        m
  380 dat_indcao_perda_dedut           date        im
  390 val_perc_juros_dia_atraso        deci-6      m
  400 val_perc_multa_atraso            deci-2      m
  410 val_perc_desc                    deci-4      m
  420 val_perc_abat_acr                deci-4      m
  430 val_origin_tit_acr               deci-2      m
  440 val_liq_tit_acr                  deci-2      m
  450 val_abat_negocdo                 deci-2
  460 val_desc_negocdo                 deci-2
  470 val_ajust_val_tit_acr            deci-2      m
  480 val_cm_tit_acr                   deci-2      m
  490 val_juros                        deci-2      m
  500 val_multa_tit_acr                deci-2      m
  510 val_desc_tit_acr                 deci-2      m
  520 val_abat_tit_acr                 deci-2      m
  530 val_transf_estab                 deci-2      m
  540 val_saida_subst_nf_dupl          deci-2
  550 val_sdo_tit_acr                  deci-2      m
  560 val_despes_bcia                  deci-2      m
  570 val_despes_financ                deci-2      m
  580 val_impto_operac_financ          deci-2      m
  590 log_tit_acr_cobr_bcia            logi        m
  600 log_dupl_emitid                  logi        m
  610 log_emis_boleto                  logi        m
  620 log_aviso_db_emitid              logi        m
  630 log_recibo_emitid                logi        m
  640 log_npromis_emitid               logi        m
  650 log_tit_acr_estordo              logi        m
  660 log_tit_acr_destndo              logi        m
  670 log_sdo_tit_acr                  logi        im
  680 log_db_autom                     logi        m
  690 log_tip_cr_perda_dedut_tit       logi        m
  700 cod_estab_bord                   char        i
  710 cod_refer                        char        m
  720 cod_grp_clien                    char
  730 cod_portador                     char        im
  740 cod_cart_bcia                    char        m
  750 cod_indic_econ                   char        m
  760 cod_tit_acr_bco                  char        i
  770 cod_cond_cobr                    char        i
  780 cod_cond_pagto                   char
  790 cod_contrat_vda                  char
  800 cod_banco                        char
  810 cod_agenc_bcia                   char
  820 cod_cta_corren                   char
  830 cod_cta_corren_bco               char
  840 cod_digito_cta_corren            char
  850 cod_agenc_cobr_bcia              char
  860 cod_movto_operac_financ          char        im
  870 cod_usuar_indcao_perda           char        m
  880 cod_indic_econ_juros             char        m
  890 qtd_dias_carenc_juros_acr        deci-0      m
  900 qtd_dias_carenc_multa_acr        deci-0      m
  910 qtd_dias_float_cobr              deci-0      m
  920 qtd_dias_atraso_liquidac         deci-4      m
  930 hra_indcao_perda_dedut           char        m
  940 cod_livre_1                      char
  950 des_obs_cobr                     char        m
  960 des_observacao                   char        m
  970 log_integr_cfl_atlzdo            logi        im
  980 cod_instruc_bcia_1_acr           char        m
  990 cod_instruc_bcia_2_acr           char        m
 1000 val_abat_tit_acr_infor           deci-2      m
 1010 log_tit_agrup_especial           logi        m
 1020 ind_tip_calc_juros               char        m
 1030 cod_proces_export                char        i
 1040 val_cr_pis                       deci-2
 1050 val_cr_cofins                    deci-2
 1060 val_cr_csll                      deci-2
 1070 val_base_calc_impto              deci-2
 1080 log_retenc_impto_impl            logi
 1090 cod_livre_2                      char
 1100 dat_livre_1                      date
 1110 dat_livre_2                      date
 1120 log_livre_1                      logi
 1130 log_livre_2                      logi
 1140 num_livre_1                      inte
 1150 num_livre_2                      inte
 1160 val_livre_1                      deci-4
 1170 val_livre_2                      deci-4
 1180 cod_nota_fisc_faturam            char
 1190 cod_parcela_faturam              char
 1200 val_perc_desc_antecip            deci-4











=========================================================================
============================= Table: titulo =============================

              Table Flags: "f" = frozen, "s" = a SQL table


Table                            Table Field Index Table
Name                             Flags Count Count Label
-------------------------------- ----- ----- ----- ------------------------
titulo                                   137    17 Documento T¡tulos a Rece

    Dump Name: ad264
  Description: Titulos a Receber
 Storage Area: Schema Area

       Trigger Event Trigger Procedure    Overridable? Check CRC?
       ------------- -------------------- ------------ ----------
       Create        database/tgad/tcp/tc yes          no
       Delete        database/tgad/tdp/td yes          no
       Write         database/tgad/twp/tw yes          no


============================= FIELD SUMMARY =============================
============================= Table: titulo =============================

Flags: <c>ase sensitive, <i>ndex component, <m>andatory, <v>iew component

Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 ep-codigo                        char        im
   20 cod-estabel                      char        im
   30 cod-esp                          char        im
   40 nr-docto                         char        im
   50 parcela                          char        im
   80 cod-emitente                     inte        im
  100 cod-port                         inte        im
  110 modalidade                       inte        im
  120 dt-emissao                       date        im
  130 dt-vencimen                      date        im
  150 vl-original                      deci-2      m
  160 vl-liquido                       deci-2      m
  180 vl-saldo                         deci-2      im
  185 dt-desconto                      date
  186 vl-desconto                      deci-2      m
  190 titulo-banco                     char        im
  210 ct-conta-cr                      char        m
  220 sc-conta-cr                      char        m
  230 dt-ult-pagto                     date
  236 flag-contab                      logi        m
  260 pedido-rep                       char        m
  280 nat-operacao                     char        m
  290 bordero                          logi        m
  300 referencia                       char        m
  310 origem                           inte        m
  320 cod-cond-pag                     inte
  350 mo-codigo                        inte        m
  360 emissao-dup                      logi        m
  370 esp-antecip                      char        m
  380 doc-antecip                      char        m
  390 parc-antecip                     char        m
  400 nr-bordero                       inte        im
  410 tipo                             inte        im
  430 dt-pg-prev                       date
  440 perc-desc-an                     deci-5      m
  450 perc-juros                       deci-5      m
  460 emite-oc                         logi        m
  470 emite-aviso                      logi        m
  480 dt-abatimen                      date
  490 vl-abatimen                      deci-2      m
  500 usuario                          char        m
  510 nr-pedcli                        char
  520 situacao                         inte
  530 emite-bloq                       logi
  540 cod-rep                          inte        i
  550 cod-vencto                       inte
  560 vl-abat-fasb                     deci-2      m
  570 vl-desc-fasb                     deci-2      m
  580 vl-liq-fasb                      deci-2      m
  590 vl-orig-fasb                     deci-2      m
  600 vl-sal-fasb                      deci-10     m
  610 nome-abrev                       char        im
  620 vl-fatura                        deci-2      m
  640 dt-destina                       date
  650 dt-confirma                      date
  660 enviado                          inte        m
  670 dt-ult-baixa                     date
  680 agencia                          char        m
  690 ins-banc                         inte[2]
  771 cob-banc-eletr                   logi
  780 port-baixa                       inte        m
  790 mod-baixa                        inte        m
  800 dt-fluxo                         date        im
  810 nr-fluxo                         inte        m
  820 matriz                           inte        im
  850 calcula-fasb                     logi        m
  860 calcula-cmi                      logi        m
  870 taxa-emis-fasb                   deci-4      m
  880 taxa-vencto-fasb                 deci-4      m
  890 taxa-emis-cmi                    deci-8      m
  900 cgc                              char
  960 taxa-emis-anbid                  deci-8      m
  980 e-carta                          logi        m
  990 tp-codigo                        inte        m
 1000 conf-vendor                      logi
 1010 cod-ocorr-banc                   inte
 1020 cond-pagto                       inte
 1030 nr-operacao                      char        m
 1050 dt-prevista                      date
 1060 arq-remessa                      char
 1070 sit-titulo                       inte
 1080 car-juros                        inte
 1100 dt-vecto-orig                    date
 1110 dt-remessa                       date
 1120 cart-cob                         inte
 1130 destinado                        logi
 1140 mercado                          inte
 1150 vl-pago                          deci-2
 1160 dt-liq                           date        i
 1200 serie                            char        im
 1210 conta-credito                    char
 1220 serie-antecip                    char
 1230 vl-desp-banc                     deci-2
 1240 num-id-titulo-cr                 inte        im
 1260 cotacao-dia                      deci-8
 1270 diversos                         deci-2
 1280 frete                            deci-2
 1310 observacao                       char
 1320 ult-seq                          inte
 1360 vl-original-me                   deci-2
 1370 vl-liquido-me                    deci-2
 1380 vl-saldo-me                      deci-2
 1390 vl-desconto-me                   deci-2
 1400 vl-abatimen-me                   deci-2
 1410 diversos-me                      deci-2
 1420 frete-me                         deci-2
 1440 vl-fatura-me                     deci-2
 1450 nr-docto-deposito                char
 1460 cod-impto-ret                    inte
 1470 vl-multa                         deci-2
 1480 vl-multa-me                      deci-2
 1490 dt-multa                         date
 1500 char-1                           char
 1510 char-2                           char
 1520 dec-1                            deci-8
 1530 dec-2                            deci-8
 1540 int-1                            inte
 1550 int-2                            inte
 1560 log-1                            logi
 1570 log-2                            logi
 1580 data-1                           date
 1590 data-2                           date
 1600 perc-multa                       deci-2
 1610 cod-entrega                      char
 1620 check-sum                        char
 1630 enviado-cartorio                 logi        m
 1640 ad-banco                         logi        m
 1650 vl-desp-cart-pagas               deci-2      m
 1660 nr-proc-exp                      char
 1670 l-vincula-contr-cambio           logi
 1680 unid-negocio                     char
 1690 vl-cofins                        deci-2
 1700 vl-pis                           deci-2
 1710 vl-csll                          deci-2
 1720 l-ret-impto                      logi        m
 1730 vl-base-calc-ret                 deci-2      m

Field Name                       Format
-------------------------------- -----------------------------
ep-codigo                        x(3)
cod-estabel                      x(5)
cod-esp                          !!
nr-docto                         x(16)
parcela                          x(2)
cod-emitente                     >>>>>>>>9
cod-port                         >>>>9
modalidade                       9
dt-emissao                       99/99/9999
dt-vencimen                      99/99/9999
vl-original                      >>>>>>>,>>9.99
vl-liquido                       >>>,>>>,>>>,>>9.99
vl-saldo                         >>>>>>>,>>9.99
dt-desconto                      99/99/9999
vl-desconto                      >>>,>>>,>>>,>>9.99
titulo-banco                     x(20)
ct-conta-cr                      x(8)
sc-conta-cr                      x(8)
dt-ult-pagto                     99/99/9999
flag-contab                      Sim/NÆo
pedido-rep                       x(12)
nat-operacao                     x(06)
bordero                          Sim/NÆo
referencia                       X(10)
origem                           >9
cod-cond-pag                     >>>9
mo-codigo                        >9
emissao-dup                      Sim/NÆo
esp-antecip                      !!
doc-antecip                      x(16)
parc-antecip                     x(2)
nr-bordero                       >>>>>>>9
tipo                             >9
dt-pg-prev                       99/99/9999
perc-desc-an                     >>9.99999
perc-juros                       >>9.99999
emite-oc                         Sim/NÆo
emite-aviso                      Sim/NÆo
dt-abatimen                      99/99/9999
vl-abatimen                      >>>>>>>,>>9.99
usuario                          x(12)
nr-pedcli                        x(12)
situacao                         9
emite-bloq                       Sim/NÆo
cod-rep                          >>>>9
cod-vencto                       9
vl-abat-fasb                     >>>>>>>,>>9.99
vl-desc-fasb                     >>>>>>>,>>9.99
vl-liq-fasb                      >>>>>>>,>>9.99
vl-orig-fasb                     >>>>>>>,>>9.99
vl-sal-fasb                      ->>>>>>>,>>9.9999999999
nome-abrev                       X(12)
vl-fatura                        >>>>>>>,>>9.99
dt-destina                       99/99/9999
dt-confirma                      99/99/9999
enviado                          9
dt-ult-baixa                     99/99/9999
agencia                          x(8)
ins-banc                         >>9
cob-banc-eletr                   Sim/NÆo
port-baixa                       >>>>9
mod-baixa                        9
dt-fluxo                         99/99/9999
nr-fluxo                         >>>>9
matriz                           >>>>>>>>9
calcula-fasb                     Sim/NÆo
calcula-cmi                      Sim/NÆo
taxa-emis-fasb                   >>>,>>>,>>9.9999
taxa-vencto-fasb                 >>>,>>>,>>9.9999
taxa-emis-cmi                    >>>,>>>,>>9.99999999
cgc                              x(19)
taxa-emis-anbid                  >>>,>>9.99999999
e-carta                          Sim/NÆo
tp-codigo                        >>9
conf-vendor                      Sim/NÆo
cod-ocorr-banc                   99
cond-pagto                       >>>9
nr-operacao                      x(10)
dt-prevista                      99/99/9999
arq-remessa                      x(20)
sit-titulo                       99
car-juros                        99
dt-vecto-orig                    99/99/9999
dt-remessa                       99/99/9999
cart-cob                         >>9
destinado                        Sim/NÆo
mercado                          >9
vl-pago                          ->>>>>>>,>>9.99
dt-liq                           99/99/9999
serie                            x(5)
conta-credito                    x(17)
serie-antecip                    x(5)
vl-desp-banc                     >>>,>>>,>>>,>>9.99
num-id-titulo-cr                 99999999
cotacao-dia                      >>>,>9.99999999
diversos                         >>>,>>>,>>>,>>9.99
frete                            >>>,>>>,>>>,>>9.99
observacao                       x(2000)
ult-seq                          >>>>>>>>9
vl-original-me                   ->>>>>>>,>>9.99
vl-liquido-me                    ->>>,>>>,>>>,>>9.99
vl-saldo-me                      >>>>>>>,>>9.99
vl-desconto-me                   >>>>>>>,>>9.99
vl-abatimen-me                   >>>>>>>,>>9.99
diversos-me                      >>>,>>>,>>>,>>9.99
frete-me                         >>>,>>>,>>>,>>9.99
vl-fatura-me                     >>>>>>>,>>9.99
nr-docto-deposito                x(16)
cod-impto-ret                    >>9
vl-multa                         ->>>,>>>,>>>,>>9.99
vl-multa-me                      ->>>,>>>,>>>,>>9.99
dt-multa                         99/99/9999
char-1                           x(100)
char-2                           x(100)
dec-1                            ->>>>>>>>>>>9.99999999
dec-2                            ->>>>>>>>>>>9.99999999
int-1                            ->>>>>>>>>9
int-2                            ->>>>>>>>>9
log-1                            Sim/NÆo
log-2                            Sim/NÆo
data-1                           99/99/9999
data-2                           99/99/9999
perc-multa                       >>>9.99
cod-entrega                      x(12)
check-sum                        x(20)
enviado-cartorio                 Sim/NÆo
ad-banco                         Sim/NÆo
vl-desp-cart-pagas               >>>,>>>,>>>,>>9.99
nr-proc-exp                      X(12)
l-vincula-contr-cambio           yes/no
unid-negocio                     X(3)
vl-cofins                        >>>>,>>>,>>9.99
vl-pis                           >>>>,>>>,>>9.99
vl-csll                          >>>,>>>,>>9.99
l-ret-impto                      Sim/NÆo
vl-base-calc-ret                 >>>,>>>,>>9.99





*/
