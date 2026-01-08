DEFINE VARIABLE dtIni AS DATE        NO-UNDO.
DEFINE VARIABLE dtFim AS DATE        NO-UNDO.
DEFINE VARIABLE dCalc AS DECIMAL     NO-UNDO.
UPDATE dtini dtfim .
OUTPUT TO value('c:\temp\nf_' + STRING(TIME)  + '.csv').
PUT 'nr.nota;dt.nota;item;refer;qt.faturada;vl.preuni;vl.merc;vl.base icms;aliq.icms;vl.icms;icms calculado;diferenca icms' SKIP.
FOR EACH nota-fiscal
    WHERE nota-fiscal.dt-emis-nota  >= dtIni
    AND   nota-fiscal.dt-emis-nota  <= dtFim
    NO-LOCK.

    FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK.
        FIND natur-oper OF it-nota-fisc NO-LOCK NO-ERROR.
        ASSIGN dCalc = it-nota-fisc.vl-merc-ori * (1 - (natur-oper.perc-red-icm / 100)) * it-nota-fisc.aliquota-icm / 100 .
        EXPORT DELIMITER ";" nota-fiscal.nr-nota-fis
              nota-fiscal.dt-emis-nota
              it-nota-fisc.it-codigo
              it-nota-fisc.cod-refer
              it-nota-fisc.qt-faturada[1]
              it-nota-fisc.vl-preuni
              it-nota-fisc.vl-merc-ori
              it-nota-fisc.vl-bicms-it
              it-nota-fisc.aliquota-icm 
              it-nota-fisc.vl-icms-it 
              round(dCalc,2)
              round(dCalc,2) - round(it-nota-fisc.vl-icms-it,2)
             .


             
    END.
END.

OUTPUT CLOSE.
/*


                         

Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 cod-estabel                      char        im
   20 serie                            char        im
   30 nr-nota-fis                      char        im
   40 nr-seq-fat                       inte        im
   50 it-codigo                        char        im
   60 peso-liq-fat                     deci-5      m
   65 peso-bruto                       deci-5      m
   70 qt-faturada                      deci-4[2]   m
   71 un-fatur                         char[2]     m
   72 vl-pretab                        deci-10     m
   73 vl-preori                        deci-10     m
   74 vl-preuni                        deci-10     m
   75 vl-merc-tab                      deci-10     m
   76 vl-merc-ori                      deci-10     m
   77 vl-merc-liq                      deci-10     m
   78 vl-tot-item                      deci-10
  110 per-des-item                     deci-5      m
  120 nr-ordem                         inte
  130 parcela                          inte
  140 ind-imp-desc                     inte        m
  150 baixa-estoq                      logi        m
  160 tipo-con-est                     inte        m
  170 cod-depos                        char        m
  195 nat-operacao                     char        im
  200 cd-trib-ipi                      inte
  210 aliquota-ipi                     deci-2      m
  215 perc-red-ipi                     deci-2      m
  225 cd-trib-iss                      inte        m
  230 aliquota-ISS                     deci-2
  240 cd-trib-icm                      inte        m
  250 aliquota-icm                     deci-2      m
  260 perc-red-icm                     deci-4
  280 atual-estat                      logi        m
  300 vl-despes-it                     deci-2      m
  320 vl-bicms-it                      deci-2
  330 vl-icms-it                       deci-2
  340 vl-icmsnt-it                     deci-2
  350 vl-icmsou-it                     deci-2
  370 vl-bipi-it                       deci-2
  380 vl-ipi-it                        deci-2      m
  390 vl-ipint-it                      deci-2      m
  400 vl-ipiou-it                      deci-2      m
  420 vl-biss-it                       deci-2      m
  430 vl-iss-it                        deci-2      m
  440 vl-issnt-it                      deci-2      m
  450 vl-issou-it                      deci-2      m
  470 vl-bsubs-it                      deci-2      m
  480 vl-icmsub-it                     deci-2      m
  490 manut-icm-it                     logi        m
  500 manut-ipi-it                     logi        m
  720 nr-pedido                        inte        im
  730 vl-reajuste                      deci-2      m
  740 vl-cuscontab                     deci-10     m
  750 tipo-contr                       inte        m
  760 vl-precon                        deci-10     m
  770 perc-red-iss                     deci-2      m
  780 ind-icm-ret                      logi
  790 ct-cuscon                        char        m
  800 sc-cuscon                        char        m
  810 ct-cusven                        char        m
  820 sc-cusven                        char        m
  830 ind-imprenda                     logi        m
  840 vl-irf-it                        deci-2      m
  850 vl-merc-sicm                     deci-10     m
  860 no-ab-vend                       char        m
  870 nr-ord-produ                     inte        m
  880 dt-retorno                       date
  900 cd-vendedor                      char        m
  910 dt-cancela                       date        i
  920 dt-confirma                      date        i
  930 dt-emis-nota                     date        im
  940 codigo-rejei                     inte        m
  950 desc-devol                       char        m
  960 nr-ficha-cq                      inte        m
  970 per-des-icms                     deci-5      m
  980 tipo-atend                       inte
  990 ind-componen                     inte        m
 1000 vl-ir-adic                       deci-2      m
 1010 class-fiscal                     char        m
 1020 ind-sit-nota                     inte        im
 1110 cod-servico                      inte        m
 1120 vl-finsocial                     deci-2      m
 1130 vl-pis                           deci-2      m
 1140 nr-nota-ant                      char        i
 1150 serie-ant                        char        i
 1160 cod-refer                        char        i
 1170 nr-seq-ped                       inte        i
 1180 nr-pedcli                        char        i
 1190 nome-ab-cli                      char        i
 1200 vl-pretab-e                      deci-10[3]
 1210 vl-preori-e                      deci-10[3]
 1220 vl-preuni-e                      deci-10[3]
 1230 vl-merctab-e                     deci-10[3]
 1240 vl-mercori-e                     deci-10[3]
 1250 vl-mercliq-e                     deci-10[3]
 1260 vl-totitem-e                     deci-10[3]
 1270 vl-despesit-e                    deci-2[3]
 1280 vl-bicmsit-e                     deci-2[3]
 1290 vl-icmsit-e                      deci-2[3]
 1300 vl-icmsntit-e                    deci-2[3]
 1310 vl-icmsouit-e                    deci-2[3]
 1320 vl-bipiit-e                      deci-2[3]
 1330 vl-ipiit-e                       deci-2[3]
 1340 vl-ipintit-e                     deci-2[3]
 1350 vl-ipiouit-e                     deci-2[3]
 1360 vl-bissit-e                      deci-2[3]
 1370 vl-issit-e                       deci-2[3]
 1380 vl-issntit-e                     deci-2[3]
 1390 vl-issouit-e                     deci-2[3]
 1400 vl-bsubsit-e                     deci-2[3]
 1410 vl-icmsubit-e                    deci-2[3]
 1420 vl-reajuste-e                    deci-2[3]
 1430 vl-cuscontab-e                   deci-10[3]
 1440 vl-precon-e                      deci-10[3]
 1450 vl-irfit-e                       deci-2[3]
 1460 vl-mercsimc-e                    deci-10[3]
 1520 vl-ipi-dev                       deci-2      m
 1530 vl-sub-emp                       deci-2
 1540 vl-comp-acum                     deci-5      m
 1550 it-nota-fisc                     inte
 1560 fat-retro                        logi        m
 1570 vl-icmscomp-it                   deci-2
 1580 vl-icmscmp-it-e                  deci-2[3]
 1590 cod-est-ven                      char        m
 1600 nr-entrega                       inte        im
 1610 cd-sit-desp                      inte        m
 1620 cd-emitente                      inte
 1630 nat-docum                        char
 1640 nivel-restituicao                char
 1650 nr-docum                         char        i
 1660 pc-restituicao                   deci-2
 1670 serie-docum                      char        i
 1680 nr-ord-prod                      inte
 1690 ind-fat-qtfam                    logi
 1700 vl-preuni-zfm                    deci-10
 1710 vl-merc-liq-zfm                  deci-10
 1810 conh-frete                       char
 1820 emite-duplic                     logi        m
 1850 aliquota-tax                     deci-2
 1860 cod-vat                          inte
 1870 cod-tax                          inte
 1880 vl-iva-it                        deci-2
 1890 vl-biva-it                       deci-2
 1900 referencia-ct                    char
 1910 cod-localiz                      char
 1920 qt-devolvida                     deci-4[2]
 1930 vl-frete-it                      deci-2
 1940 char-1                           char
 1950 char-2                           char
 1960 dec-1                            deci-8
 1970 dec-2                            deci-8
 1980 int-1                            inte
 1990 int-2                            inte
 2000 log-1                            logi
 2010 log-2                            logi
 2020 data-1                           date
 2030 data-2                           date
 2040 vl-desconto                      deci-10
 2050 nr-remito                        char        i
 2060 check-sum                        char
 2070 log-usa-tabela-desconto          logi        m
 2080 val-pct-desconto-tab-preco       deci-10     m
 2090 des-pct-desconto-inform          char
 2100 val-desconto-inform              deci-10     m
 2120 val-pct-desconto-total           deci-10     m
 2180 val-pct-desconto-periodo         deci-10
 2190 val-pct-desconto-prazo           deci-10
 2200 val-desconto-total               deci-10
 2220 val-desconto                     deci-10[5]
 2230 nr-tabpre                        char        im
 2240 vl-pretab-me                     deci-10     m
 2250 vl-preori-me                     deci-10     m
 2260 vl-preuni-me                     deci-10     m
 2270 vl-merc-tab-me                   deci-10     m
 2280 vl-merc-ori-me                   deci-10     m
 2290 vl-merc-liq-me                   deci-10     m
 2300 vl-tot-item-me                   deci-10
 2310 vl-despes-it-me                  deci-5      m
 2320 vl-reajuste-me                   deci-5      m
 2330 vl-cuscontab-me                  deci-10     m
 2340 vl-precon-me                     deci-10     m
 2360 vl-merc-sicm-me                  deci-10     m
 2380 vl-iva-it-me                     deci-5
 2390 vl-frete-it-me                   deci-5
 2400 vl-biva-it-me                    deci-5
 2410 vl-desconto-me                   deci-10
 2420 ser-remito                       char        i
 2430 nr-seq-it-rmt                    inte        i
 2440 nr-embarque                      inte
 2450 dec-ftconv-unest                 deci-0
 2460 num-casa-dec-unest               inte
 2470 ind-origem-fator                 inte
 2480 val-retenc-csll                  deci-5      m
 2490 val-retenc-pis                   deci-5      m
 2500 val-retenc-cofins                deci-5      m
 2510 idi-forma-calc-pis               inte        m
 2520 idi-forma-calc-cofins            inte        m
 2530 val-unit-pis                     deci-5      m
 2540 val-unit-cofins                  deci-5      m
 2550 log-preco-unid-medid-estoq       logi        m
 2560 vl-preori-un-fat                 deci-5
 2570 cod-unid-negoc                   char        i
 2580 val-sat                          deci-5      m
 2590 val-senar                        deci-5      m
 2610 cod-sit-tributar-ipi             char        m
 2620 cod-sit-tributar-pis             char        m
 2630 cod-sit-tributar-cofins          char        m
 2640 idi-modalid-base-icms            inte        m
 2650 idi-modalid-base-ipi             inte        m
 2660 idi-modalid-base-icms-st         inte        m
 2670 num-romaneio                     inte        i
 2680 val-base-pis-substto             deci-10
 2690 val-base-cofins-substto          deci-10
 2710 num-seq-roman                    inte
 2730 nat-terc                         char
 2740 cod-cta-ctbl-cust-ctbl           char
 2750 cod-cta-ctbl-cust-produt-vendido char        m
 2810 cdd-embarq                       deci-0      m
 2820 val-perc-icms-diferim            deci-2
 2830 cod-ord-compra                   char
*/
