
OUTPUT TO c:\temp\itens.txt.

FOR EACH ITEM
    WHERE ITEM.ge-codigo = 60:
    FIND item-ext       OF ITEM NO-LOCK NO-ERROR.
    FIND fam-comerc     OF ITEM NO-LOCK NO-ERROR.
    FIND familia        OF ITEM NO-LOCK NO-ERROR.
    

    EXPORT DELIMITER "|" ITEM.it-codigo
        ITEM.desc-item
        ITEM.un
       IF AVAIL familia THEN  ITEM.fm-codigo + "-" + familia.descricao ELSE ''
       IF AVAIL fam-comerc THEN  item.fm-cod-com + "-" + fam-comerc.descricao ELSE ''
       ITEM.data-implant
       IF AVAIL item-ext THEN
          IF item-ext.cod_tipo_item  = 1 THEN 'ESTAMPADO' 
           ELSE IF item-ext.cod_tipo_item  = 2 THEN 'LISO' ELSE ''
        ELSE ''       .




END.


OUTPUT CLOSE.

/*
  Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 it-codigo                        char        im
   20 descricao-1                      char        m
   25 descricao-2                      char        m
   30 un                               char        im
   40 compr-fabric                     inte        im
   50 ge-codigo                        inte        im
   60 fm-codigo                        char        im
   70 data-implant                     date        m
   80 data-liberac                     date        m
  100 path                             char
  110 codigo-refer                     char        im
  120 inform-compl                     char        m
  130 niv-mais-bai                     inte        im
  159 loc-unica                        logi        m
  161 tipo-con-est                     inte        m
  162 deposito-pad                     char        im
  163 ciclo-contag                     inte        m
  164 data-ult-con                     date
  165 variac-acum                      deci-4      m
  166 curva-abc                        logi        m
  170 data-ult-ent                     date
  180 data-ult-sai                     date
  185 data-base                        date
  190 data-ult-rep                     date
  200 cod-obsoleto                     inte        im
  210 consumo-prev                     deci-4      m
  220 consumo-aad                      deci-4      m
  227 cod-comprado                     char        im
  230 demanda                          inte        im
  240 emissao-ord                      inte        m
  250 classif-abc                      inte        m
  260 politica                         inte        im
  270 periodo-fixo                     inte        m
  280 tempo-segur                      inte        m
  285 quant-segur                      deci-4      m
  290 res-int-comp                     inte        m
  295 res-for-comp                     inte        m
  300 res-cq-comp                      inte        m
  305 ressup-fabri                     inte        m
  306 res-cq-fabri                     inte        m
  310 lote-multipl                     deci-4      m
  315 tipo-lote-ec                     inte        m
  320 lote-economi                     deci-4      m
  330 fator-refugo                     deci-2      m
  340 quant-perda                      deci-4      m
  350 classe-repro                     inte        m
  355 contr-qualid                     logi        m
  360 class-fiscal                     char        im
  380 aliquota-ipi                     deci-2      m
  390 codigo-orig                      inte        m
  400 cd-trib-icm                      inte        m
  405 cd-trib-ipi                      inte        m
  410 peso-liquido                     deci-5      m
  420 fator-conver                     deci-5      m
  460 tipo-desc-nt                     inte        m
  465 baixa-estoq                      logi        m
  470 moeda-padrao                     inte        m
  480 preco-base                       deci-4      m
  520 preco-ul-ent                     deci-4      m
  530 preco-repos                      deci-4      m
  540 cd-planejado                     char        im
  550 nr-linha                         inte        im
  560 tipo-requis                      inte        im
  570 cap-est-fabr                     deci-4      m
  580 tipo-contr                       inte        im
  590 tipo-est-seg                     inte
  610 dt-ult-ben                       date
  640 ft-conversao                     deci-0      m
  650 dec-ftcon                        inte        m
  660 pm-ja-calc                       logi        m
  670 fraciona                         logi        m
  680 reporte-mob                      inte        m
  690 ind-item-fat                     logi        m
  700 peso-bruto                       deci-5      m
  710 cd-trib-iss                      inte        m
  720 aliquota-ISS                     deci-2      m
  730 ind-imp-desc                     inte        m
  740 ind-ipi-dife                     logi        m
  760 lote-minimo                      deci-4      m
  770 ind-inf-qtf                      logi
  780 vl-mat-ant                       deci-4      m
  790 vl-mob-ant                       deci-4      m
  800 responsavel                      char        m
  810 usuario-alt                      char
  820 data-obsol                       date
  830 usuario-obsol                    char
  840 criticidade                      inte        m
  870 horiz-fixo                       inte        m
  880 lote-mulven                      deci-4      m
  890 cod-servico                      inte        im
  900 comprim                          deci-8      m
  910 largura                          deci-8      m
  920 altura                           deci-8      m
 1010 qt-max-ordem                     deci-4
 1020 perm-saldo-neg                   inte        m
 1030 dt-pr-fisc                       date
 1040 preco-fiscal                     deci-4      m
 1050 ct-codigo                        char        m
 1060 sc-codigo                        char        m
 1070 pr-sem-tx                        deci-4      m
 1080 div-ordem                        inte        m
 1090 cod-estabel                      char        im
 1100 cod-produto                      char        m
 1110 fm-cod-com                       char        i
 1120 ft-conv-fmcoml                   deci-0      m
 1130 volume                           deci-5      m
 1190 desc-nacional                    char
 1200 desc-inter                       char
 1210 ind-especifico                   logi
 1220 per-min-luc                      deci-2
 1230 ind-backorder                    logi
 1240 rep-prod                         inte        m
 1250 cd-formula                       inte
 1260 cd-origem                        inte
 1270 tp-cons-prev                     deci-0
 1290 tp-aloc-lote                     inte
 1310 tp-desp-padrao                   inte
 1320 id-grade                         logi
 1330 nat-despesa                      inte        i
 1340 tp-adm-lote                      inte
 1350 per-rest-icms                    deci-2      m
 1360 niv-rest-icms                    char
 1370 concentracao                     deci-4      m
 1380 rendimento                       deci-4      m
 1390 nivel                            inte        m
 1400 tipo-insp                        inte        m
 1410 perc-nqa                         deci-3
 1420 per-rest-fora                    deci-2
 1430 niv-rest-fora                    char
 1440 nr-item-dcr                      inte
 1450 tx-importacao                    deci-3
 1460 prioridade                       inte        m
 1470 tipo-sched                       inte        m
 1480 atu-conf                         logi
 1490 isencao-import                   inte
 1500 resumo-mp                        inte        m
 1510 ind-serv-mat                     inte
 1520 cod-refer                        char
 1530 niv-mps                          inte
 1540 calc-cons-prev                   inte        m
 1550 var-transf                       deci-2      m
 1560 var-rep                          deci-2      m
 1570 var-req-maior                    deci-2
 1580 var-req-menor                    deci-2
 1600 de-codigo-prin                   char        m
 1610 contr-plan                       inte        im
 1620 calc-lead-time                   inte
 1630 dec-conv-fmcoml                  inte        m
 1640 fase-medio                       inte
 1660 cod-auxiliar                     char        i
 1670 var-mob-maior                    deci-2
 1680 var-mob-menor                    deci-2
 1690 it-demanda                       char
 1700 perc-demanda                     deci-2
 1710 cd-folh-lote                     char
 1720 cd-folh-item                     char        i
 1730 valor-ipi-beb                    deci-5
 1740 esp-beb                          inte
 1750 enquad-beb                       inte
 1760 tipo-recip-beb                   inte
 1770 capac-recip-beb                  inte
 1780 rot-quant                        logi
 1790 rot-revis                        logi
 1800 rot-refer                        logi
 1820 alt-refer                        logi
 1830 tipo-atp                         inte
 1840 fator-reaj-icms                  deci-8
 1850 incentivado                      logi
 1920 prefixo-lote                     char
 1930 Nr-ult-peca                      inte
 1940 tp-lote-minimo                   logi
 1950 tp-lote-multiplo                 logi
 1960 tp-lote-econom                   logi
 1970 quant-pacote                     deci-4
 1980 conta-aplicacao                  char
 2000 cod-localiz                      char        i
 2010 sit-aloc                         inte
 2020 cd-tag                           char
 2030 cod-tax                          inte
 2040 cod-tax-serv                     inte
 2050 desc-item                        char        i
 2060 narrativa                        char
 2070 log-carac-tec                    logi        m
 2080 cod-lista-destino                char
 2090 log-atualiz-via-mmp              logi
 2110 vl-var-max                       deci-2
 2120 vl-var-min                       deci-2
 2130 qt-var-max                       deci-2
 2140 qt-var-min                       deci-2
 2160 reporte-ggf                      inte        m
 2170 cod-imagem                       char
 2180 cd-referencia                    char
 2190 conv-tempo-seg                   logi
 2200 char-1                           char
 2210 char-2                           char
 2220 dec-1                            deci-8
 2230 dec-2                            deci-8
 2240 INT-1                            inte
 2250 int-2                            inte
 2260 log-1                            logi
 2270 log-2                            logi
 2280 data-1                           date
 2290 data-2                           date
 2300 ind-confprodcom                  logi
 2310 nivel-apr-requis                 inte
 2320 nivel-apr-solic                  inte
 2330 nivel-apr-manut                  inte
 2340 nivel-apr-compra                 inte
 2350 ind-prev-demanda                 inte        m
 2360 ind-calc-meta                    inte        m
 2370 val-fator-custo-dis              deci-2      m
 2380 qtd-refer-custo-dis              deci-2
 2390 qtd-batch-padrao                 deci-4      m
 2400 log-utiliza-batch-padrao         logi        m
 2410 ind-quotas                       logi
 2420 nr-pontos-quotas                 inte
 2430 check-sum                        char
 2440 num-id-item                      inte        im
 2450 ind-refugo                       inte        m
 2460 log-necessita-li                 logi
 2470 dias-estoq-aloc                  char
 2480 pto-repos                        deci-4
 2490 aliquota-ii                      deci-2
 2500 cod-trib-ii                      inte
 2520 geracao-ordem                    inte
 2530 cons-produto                     logi
 2540 cons-saldo                       logi
 2550 mp-restrit                       logi
 2560 qtde-max                         deci-4
 2570 qtde-fixa                        deci-4
 2580 lote-repos                       deci-2
 2590 cons-consumo                     logi
 2600 cod-malha                        char        i
 2610 cod-pulmao                       char
 2620 politica-aps                     inte
 2630 tipo-formula                     inte        m
 2640 per-ppm                          deci-4
 2650 cod-tab-preco-aps                char
 2660 cod-pulmao-proces                char        m
 2670 log-tax-produc                   logi        m
 2680 log-control-estoq-refugo         logi
 2690 log-refugo-preco-fisc            logi
 2700 cod-item-refugo                  char        i
 2710 val-relac-refugo-item            deci-5
 2720 log-multi-malha                  logi
 2730 cod-destaq                       inte        m
 2740 dsl-destaq                       char        m
 2750 idi-classif-item                 inte
 2760 cod-unid-negoc                   char        i
 2770 log-consid-aloc-ativid           logi        m
 2780 val-overlap                      deci-2
 2790 log-programac-sfc                logi
 2800 cod-dcr-item                     char
 2810 log-orig-ext                     logi
 2820 log-altera-valid-lote            logi
 2830 log-inspec-lote                  logi
 2840 cod-workflow                     char        i
 2850 log-benef                        logi
 2860 cdn-emit-benef                   inte
 2870 log-recalc-sdo-terc              logi
*/
