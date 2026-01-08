DISABLE TRIGGERS FOR LOAD OF item-doc-est.
DEFINE VARIABLE dTotal AS DECIMAL     NO-UNDO.
OUTPUT TO c:\temp\item-docum-est02.txt.
FOR EACH docum-est
    WHERE 
    cod-estabel = '5' 
    AND cod-emitente = 25663
    AND  serie-docto = '3'
    AND nro-docto = '0136815'
    AND nat-operacao = '31201' :
    UPDATE tot-valor.
    /*ASSIGN  tot-valor = tot-valor + 4062.36 .*/
    /*
    FOR EACH item-doc-est OF docum-est:


        
       ASSIGN val-aliq-cofins   = 10.65
               val-aliq-ext-cofins = 9.65
               val-base-calc-cofins = base-pis
               val-cofins =   val-aliq-cofins * val-base-calc-cofins   / 100
               despesas[1] =  despesas[1] + val-cofins.
               
        ASSIGN dTotal = val-cofins + dTotal.
        DISP it-codigo cod-refer 
             val-aliq-cofins        
             val-base-calc-cofins   
             val-cofins  WITH  WIDTH 550.
    END.*/
END.
DISP dTotal.

/*  
    1300 acresc-financ                    logi
 1330 aliq-irf                         deci-2      m
  390 aliquota-icm                     deci-2      m
  550 aliquota-iss                     deci-2      m
  570 ap-atual                         logi        m
 1470 atual-ft                         logi
 1890 atualiza-cr                      logi
 2460 bairro                           char
 1980 base-cofins-subs                 deci-2      m
  140 base-icm                         deci-2      m
  730 base-icm-fas                     deci-2[2]   m
  150 base-ipi                         deci-2      m
  740 base-ipi-fas                     deci-2[2]   m
  160 base-iss                         deci-2      m
  750 base-iss-fas                     deci-2[2]   m
 1970 base-pis-subs                    deci-2      m
 1250 base-subs                        deci-2
 1290 base-subs-fasb                   deci-2[2]
 1400 c-nr-invoice                     char
 2220 cc-atual                         logi
 2400 cdn-modalid-transp               inte
 2550 cdn-sit-nfe                      inte
  710 CE-atual                         logi        im
 2470 cep                              char
 1770 char-1                           char
 1780 char-2                           char
 1870 check-sum                        char
 2480 cidade                           char
 2540 cod-chave-aces-nf-eletro         char        im
   50 cod-emitente                     inte        im
 2440 cod-entrega                      char
  300 cod-estabel                      char        im
  370 cod-ibge                         inte
 2050 cod-imp-embal                    inte
 2030 cod-imp-frete                    inte
 1350 cod-imp-irf                      char        m
 2060 cod-imp-outras                   inte
 2040 cod-imp-seguro                   inte
 2530 cod-natur-vincul                 char
  320 cod-observa                      inte        m
 2410 cod-placa                        char[3]
 1360 cod-ret-irf                      inte        m
 2240 cod-rma                          char        i
 2350 cod-servico                      inte
 1910 cod-tax-div                      inte
 1900 cod-tax-fre                      inte
 2420 cod-uf-placa                     char[3]
 1570 conta-transit                    char
 1610 cotacao-dia                      deci-8
  950 cotacao-fasb                     deci-8      m
 2010 cp-atual                         logi        m
 1190 cr-atual                         logi        m
  279 ct-transit                       char        m
 2280 dat-aprovacao                    date
 2250 dat-validade                     date
 1850 data-1                           date
 1860 data-2                           date
 1790 dec-1                            deci-8
 1800 dec-2                            deci-8
 2170 declaracao-import                char        im
 2290 desc-motivo-aprov                char
  760 desp-fasb                        deci-2[2]   m
  450 despesa-nota                     deci-2      m
 1690 despesa-nota-me                  deci-2
 1370 dia-util-irf                     inte
   90 dt-atualiza                      date        m
   70 dt-emissao                       date        m
 1420 dt-entr-merc                     date
   80 dt-trans                         date        im
 1230 dt-venc-icm                      date
 1220 dt-venc-ipi                      date
 1210 dt-venc-iss                      date
  260 embalagem                        char
 2020 embarque                         char
 2450 endereco                         char
   20 esp-docto                        inte        m
 1960 esp-fiscal                       char        m
 1340 esp-irf                          char        m
  700 estab-de-or                      char        m
 1100 estab-fisc                       char        m
  530 estado                           inte        m
 1490 estorn-comis                     logi        m
 2310 Fat-despesa                      logi
 2210 ft-atual                         logi
  100 hr-atualiza                      char        m
 1410 i-sequencia                      inte
  400 icm-complem                      deci-2      m
  770 icm-cpl-fasb                     deci-2[2]   m
  780 icm-dc-fasb                      deci-2[2]   m
  410 icm-deb-cre                      deci-2      m
  790 icm-fnt-fasb                     deci-2[2]   m
  250 icm-fonte                        deci-2      m
  420 icm-nao-trib                     deci-2      m
  800 icm-ntr-fasb                     deci-2[2]   m
  810 icm-out-fasb                     deci-2[2]   m
  430 icm-outras                       deci-2      m
 2580 idi-bxa-devol                    inte
 2370 idi-nf-simples-remes             inte
 2570 idi-sit-nf-eletro                inte
 1440 imp-import                       deci-2
 1380 ind-extrac                       logi
 1520 ind-orig-entrada                 inte
 1750 ind-rateio                       logi
 1740 ind-tipo-rateio                  logi
 1530 ind-via-envio                    inte
 1810 int-1                            inte
 1820 int-2                            inte
  820 ipi-dc-fasb                      deci-2[2]   m
  470 ipi-deb-cre                      deci-2      m
  580 ipi-despesa                      deci-2      m
  480 ipi-nao-trib                     deci-2      m
  830 ipi-ntr-fasb                     deci-2[2]   m
  840 ipi-out-fasb                     deci-2[2]   m
  500 ipi-outras                       deci-2      m
  850 iss-dc-fasb                      deci-2[2]   m
  460 iss-deb-cre                      deci-2      m
  490 iss-nao-trib                     deci-2      m
  860 iss-ntr-fasb                     deci-2[2]   m
  870 iss-out-fasb                     deci-2[2]   m
  510 iss-outras                       deci-2      m
 1830 log-1                            logi
 1840 log-2                            logi
 2590 log-atualiz-proces-export        logi        m
 2190 log-cc-atual                     logi        m
 2500 log-consid-ender-nf-saida        logi        m
 2360 log-draw-atlzdo                  logi        m
 2520 log-entreg-fut                   logi
 2180 log-ft-atual                     logi        m
 2380 log-memorando                    logi        m
 2200 log-pt-atual                     logi        m
 2510 log-recuper-impto-atlzdo         logi        m
 2560 log-tax-siscomex                 logi        m
 2320 log-wms-atual                    logi
 1600 mo-codigo                        inte
 1480 mod-atual                        inte        m
  990 mod-frete                        inte        m
  210 nat-operacao                     char        im
 1560 nf-emitida-est                   logi
  680 nff                              logi        im
 2430 nome-transp                      char
   40 nro-docto                        char        im
 1540 nro-proc-entrada                 inte
 1550 nro-proc-saida                   inte
 2300 num-seq-cont-emit                inte
  720 observacao                       char        m
  690 of-atual                         logi        m
 1120 ok-data                          date
 1010 ok-fornec                        inte        m
 1110 ok-observ                        char
 1130 ok-user                          char        m
 1390 origem                           char
 2490 pais                             char
  970 pais-origem                      char        m
 1930 perc-tax-div                     deci-2
 2160 perc-tax-emb                     deci-2
 1920 perc-tax-fre                     deci-2
 2150 perc-tax-seg                     deci-2
 2330 peso-bruto-tot                   deci-3
 2340 peso-liquido-tot                 deci-3
 2230 pt-atual                         logi
 1460 qt-tot-imp                       deci-2
 1310 rateia-frete                     inte        m
 1500 rec-fisico                       logi
  280 sc-transit                       char        m
   30 serie-docto                      char        im
 1760 sit-atual                        inte
 1720 sit-docum                        inte        i
 1730 sit-edi                          inte
 1430 taxa-conv-imp                    deci-10
  310 tipo-docto                       inte        m
 1880 tipo-nota                        inte        m
  880 tot-des-fasb                     deci-2[2]   m
  640 tot-desconto                     deci-2      m
 1660 tot-desconto-me                  deci-2
  270 tot-peso                         deci-4      m
 1590 tot-taxa                         deci-2
 1700 tot-taxa-me                      deci-2
  200 tot-valor                        deci-2      m
 1670 tot-valor-me                     deci-2
  890 tot-vl-fasb                      deci-2[2]   m
 1710 tp-rat-fret                      inte
  350 uf                               char        m
  980 usuario                          char        m
 2260 val-perc-restocagem              deci-2
 2270 val-restocagem                   deci-4
  590 valor-embal                      deci-2      m
 1640 valor-embal-me                   deci-2
  600 valor-frete                      deci-2      m
 1620 valor-frete-me                   deci-2
 1000 valor-irf                        deci-2      m
  610 valor-mercad                     deci-2      m
 1680 valor-mercad-me                  deci-2
  620 valor-outras                     deci-2      m
 1650 valor-outras-me                  deci-2
  630 valor-seguro                     deci-2      m
 1630 valor-seguro-me                  deci-2
  360 via-transp                       inte        m
 2000 vl-cofins-subs                   deci-2      m
  900 vl-emb-fasb                      deci-2[2]   m
  910 vl-fre-fasb                      deci-2[2]   m
 2090 vl-imp-embalagem                 deci-2
 2130 vl-imp-embalagem-me              deci-2
 2070 vl-imp-frete                     deci-2
 2110 vl-imp-frete-me                  deci-2
  960 vl-imp-impor                     deci-2[2]   m
 2100 vl-imp-outras                    deci-2
 2140 vl-imp-outras-me                 deci-2
 2080 vl-imp-seguro                    deci-2
 2120 vl-imp-seguro-me                 deci-2
  920 vl-merc-fasb                     deci-2[2]   m
  930 vl-out-fasb                      deci-2[2]   m
 1990 vl-pis-subs                      deci-2      m
  940 vl-seg-fasb                      deci-2[2]   m
 1260 vl-subs                          deci-2
 1280 vl-subs-fasb                     deci-2[2]
 1950 vl-tax-diversos                  deci-2
 1940 vl-tax-frete                     deci-2

Field Name                       Format
-------------------------------- -----------------------------
acresc-financ                    Sim/NÆo
aliq-irf                         >>9.99
aliquota-icm                     >>9.99
aliquota-iss                     >>9.99
ap-atual                         Sim/NÆo
atual-ft                         Sim/NÆo
atualiza-cr                      Sim/NÆo
bairro                           X(30)
base-cofins-subs                 >>>>,>>>,>>9.99
base-icm                         >>>>>,>>>,>>9.99
base-icm-fas                     >>>>>,>>>,>>9.99
base-ipi                         >>>>>,>>>,>>9.99
base-ipi-fas                     >>>>>,>>>,>>9.99
base-iss                         >>>>>,>>>,>>9.99
base-iss-fas                     >>>>>,>>>,>>9.99
base-pis-subs                    >>>>,>>>,>>9.99
base-subs                        >>>>>,>>>,>>9.99
base-subs-fasb                   >>>>>,>>>,>>>.99
c-nr-invoice                     x(10)
cc-atual                         Sim/NÆo
cdn-modalid-transp               9
cdn-sit-nfe                      9
CE-atual                         Sim/NÆo
cep                              x(12)
char-1                           x(100)
char-2                           x(100)
check-sum                        x(20)
cidade                           x(25)
cod-chave-aces-nf-eletro         x(60)
cod-emitente                     >>>>>>>>9
cod-entrega                      x(12)
cod-estabel                      x(5)
cod-ibge                         9
cod-imp-embal                    >>9
cod-imp-frete                    >>9
cod-imp-irf                      x(4)
cod-imp-outras                   >>9
cod-imp-seguro                   >>9
cod-natur-vincul                 x(06)
cod-observa                      9
cod-placa                        x(12)
cod-ret-irf                      >>>>9
cod-rma                          X(16)
cod-servico                      >>>>9
cod-tax-div                      >>9
cod-tax-fre                      >>9
cod-uf-placa                     !!
conta-transit                    x(20)
cotacao-dia                      >>>,>9.99999999
cotacao-fasb                     >>>,>>9.99999999
cp-atual                         Sim/NÆo
cr-atual                         Sim/NÆo
ct-transit                       x(20)
dat-aprovacao                    99/99/9999
dat-validade                     99/99/9999
data-1                           99/99/9999
data-2                           99/99/9999
dec-1                            ->>>>>>>>>>>9.99999999
dec-2                            ->>>>>>>>>>>9.99999999
declaracao-import                X(20)
desc-motivo-aprov                X(2000)
desp-fasb                        >>>>>,>>>,>>9.99
despesa-nota                     >>>>>,>>>,>>9.99
despesa-nota-me                  >>>,>>>,>>9.99
dia-util-irf                     >>9
dt-atualiza                      99/99/9999
dt-emissao                       99/99/9999
dt-entr-merc                     99/99/9999
dt-trans                         99/99/9999
dt-venc-icm                      99/99/9999
dt-venc-ipi                      99/99/9999
dt-venc-iss                      99/99/9999
embalagem                        x(10)
embarque                         X(12)
endereco                         X(40)
esp-docto                        >9
esp-fiscal                       X(3)
esp-irf                          !!
estab-de-or                      x(5)

 
 
 
 
 
 
 
 
 
 
 
   
   10 cod-emitente                     inte        im
   20 nro-docto                        char        im
   30 serie-docto                      char        im
   40 nat-operacao                     char        im
   50 aliquota-ipi                     deci-2      m
   60 aliquota-iss                     deci-2      m
   70 cd-trib-icm                      inte        m
   90 cd-trib-iss                      inte        m
  100 class-fiscal                     char        m
  120 it-codigo                        char        im
  130 sequencia                        inte        im
  140 peso-liquido                     deci-5      m
  150 quantidade                       deci-4      m
  160 un                               char        m
  170 base-ipi                         deci-2[2]   m
  180 base-icm                         deci-2[2]   m
  190 base-iss                         deci-2[2]   m
  200 despesas                         deci-2[2]   m
  210 ipi-ntrib                        deci-2[2]   m
  220 icm-ntrib                        deci-2[2]   m
  230 iss-ntrib                        deci-2[2]   m
  240 ipi-outras                       deci-2[2]   m
  250 icm-outras                       deci-2[2]   m
  260 iss-outras                       deci-2[2]   m
  270 preco-total                      deci-2[2]   m
  300 icm-complem                      deci-2[2]   m
  310 cod-depos                        char        m
  330 lote                             char        m
  340 componente                       logi        m
  350 nro-comp                         char        im
  360 serie-comp                       char        im
  370 aliquota-icm                     deci-2      m
  380 codigo-rejei                     inte        m
  390 cotacao-fasb                     deci-8      m
  400 ct-codigo                        char        m
  410 dt-retorno                       date
  420 desconto                         deci-2[2]   m
  440 dt-vali-lote                     date
  450 nr-ficha                         inte        m
  460 atualiza-pa                      logi        m
  470 encerra-pa                       logi        m
  480 reabre-pa                        logi        m
  490 nat-comp                         char        im
  500 nr-ord-produ                     inte        m
  510 num-pedido                       inte        i
  520 numero-ordem                     inte        im
  530 parcela                          inte        im
  540 qt-do-forn                       deci-4      m
  550 sc-codigo                        char        m
  560 vl-imp-impor                     deci-2[2]   m
  570 nr-pedcli                        char        m
  580 nr-pd-seq                        inte        m
  590 narrativa                        char        m
  600 reabre-pd                        logi        m
  610 valor-ipi                        deci-2[2]   m
  620 valor-icm                        deci-2[2]   m
  630 valor-iss                        deci-2[2]   m
  640 preco-unit                       deci-5[2]   m
  650 data-comp                        date
  660 dt-ent-prev                      date
  750 pr-total                         deci-2[3]   m
  760 baixa-ce                         logi        m
  770 dt-nota-comp                     date
  810 cod-refer                        char        m
  820 base-icm-cmi                     deci-2      m
  830 base-ipi-cmi                     deci-2      m
  840 base-iss-cmi                     deci-2      m
  850 desconto-cmi                     deci-2      m
  860 despesas-cmi                     deci-2      m
  870 icm-comp-cmi                     deci-2      m
  880 icm-ntr-cmi                      deci-2      m
  890 icm-out-cmi                      deci-2      m
  900 ipi-ntr-cmi                      deci-2      m
  910 ipi-out-cmi                      deci-2      m
  920 iss-ntr-cmi                      deci-2      m
  930 iss-out-cmi                      deci-2      m
  940 pr-total-cmi                     deci-2      m
  950 vl-icm-cmi                       deci-2      m
  960 vl-ipi-cmi                       deci-2      m
  970 vl-iss-cmi                       deci-2      m
  980 vl-imp-cmi                       deci-2      m
  990 seq-comp                         inte        i
 1020 vl-subs-cmi                      deci-2
 1040 nr-pd-ent                        inte        m
 1060 cod-emit-benef                   inte
 1070 i-nr-gi                          inte        m
 1080 c-nr-invoice                     char
 1090 i-seq-inv                        inte
 1100 pc-restituicao                   deci-2
 1110 nivel-restituicao                char
 1120 emite-comp                       inte        m
 1130 cotacao-cmi                      deci-8      m
 1140 pr-unit-cmi                      deci-5
 1150 pr-mob-cmi                       deci-4
 1160 vl-subs                          deci-2[2]
 1170 base-subs                        deci-2[2]
 1180 pre-unit-mob                     deci-5[2]
 1190 base-subs-cmi                    deci-2      m
 1200 qt-real                          deci-4      m
 1210 num-ord-inv                      inte
 1220 nat-of                           char        m
 1230 cod-localiz                      char
 1240 cd-trib-ipi                      inte        m
 1250 conta-contabil                   char
 1260 item-pai                         char
 1270 cod-roteiro                      char
 1280 op-codigo                        inte
 1290 origem                           inte
 1320 cod-esp                          char
 1330 vl-taxa                          deci-2
 1340 cod-tax                          inte
 1350 vl-isr                           deci-2
 1360 hora                             char
 1370 vl-taxa-me                       deci-2
 1380 fn-docto-ap                      inte
 1390 qt-saldo                         deci-4
 1400 preco-total-alt                  deci-2[2]
 1410 nr-docto-ap                      char
 1420 preco-fatura                     deci-2
 1430 cota-moeda                       deci-4[2]
 1440 perc-vat                         deci-2
 1450 perc-sales-tax                   deci-2
 1460 perc-isr                         deci-2
 1470 cod-tax-isr                      inte
 1480 usuario                          char
 1490 data                             date
 1500 est-cob                          char
 1510 preco-unit-me                    deci-5
 1520 preco-total-me                   deci-4
 1530 desconto-me                      deci-2
 1540 sit-item                         inte
 1550 cotacao-alt                      deci-4[2]
 1560 vl-unit-mob                      deci-2
 1570 etiquetas                        inte
 1580 flag-atu                         inte
 1590 qt-etiquetas                     inte
 1600 char-1                           char
 1610 char-2                           char
 1620 dec-1                            deci-8
 1630 dec-2                            deci-8
 1640 int-1                            inte
 1650 int-2                            inte
 1660 log-1                            logi
 1670 log-2                            logi
 1680 data-1                           date
 1690 data-2                           date
 1700 check-sum                        char
 1710 vl-isr-me                        deci-2
 1720 despesas-me                      deci-2[2]
 1730 quant-conf                       deci-4      m
 1740 base-pis-subs                    deci-2      m
 1750 base-cofins-subs                 deci-2      m
 1760 vl-pis-subs                      deci-2      m
 1770 vl-cofins-subs                   deci-2      m
 1780 log-fifo-oc                      logi        m
 1790 seq-evento                       inte        m
 1800 log-icm-retido                   logi        m
 1810 cod-emit-terc                    inte        i
 1820 nro-docto-terc                   char        i
 1830 serie-terc                       char        i
 1840 nat-terc                         char        i
 1850 seq-terc                         inte        i
 1860 nr-ato-concessorio               char        i
 1870 valor-frete                      deci-2
 1880 valor-frete-me                   deci-2
 1890 nr-proc-imp                      char
 1900 declaracao-import                char        m
 1910 num-seq-rma                      inte
 1920 log-geracao-ncr-ap               logi
 1930 val-perc-restocagem              deci-2
 1940 val-restocagem                   deci-4
 1950 cdn-devolucao                    inte
 1960 cod-motivo                       char        i
 1970 cod-disposicao                   char        i
 1980 cod-despesa                      char
 1990 fornec-comp                      inte
 2000 val-perc-rep-ipi                 deci-4      m
 2010 val-perc-red-icms                deci-4      m
 2020 val-sdo-alocad-oc                deci-2
 2030 val-sdo-alocad-terc              deci-2
 2040 num-event-contrat                inte
 2050 num-ret-om                       inte
 2060 peso-bruto-item                  deci-5
 2070 peso-liquido-item                deci-5
 2080 cod-estab-compon                 char
 2090 idi-tributac-cofins              inte        m
 2100 val-aliq-cofins                  deci-2      m
 2110 val-base-calc-cofins             deci-2      m
 2120 val-cofins                       deci-2      m
 2130 val-cofins-fasb                  deci-2      m
 2140 val-cofins-cmi                   deci-2      m
 2150 idi-tributac-pis                 inte        m
 2160 val-aliq-pis                     deci-2      m
 2170 base-pis                         deci-2      m
 2180 valor-pis                        deci-2      m
 2190 valor-pis-fasb                   deci-2      m
 2200 valor-pis-cmi                    deci-2      m
 2210 num-ord-import                   inte
 2220 num-parc-import                  inte
 2230 cdn-impto-retid                  inte        m
 2240 val-aliq-impto-retid             deci-2      m
 2250 val-impto-retid                  deci-2      m
 2260 val-impto-retid-me               deci-2      m
 2270 val-aduana-pis                   deci-5
 2280 val-despes-aduana-pis            deci-5
 2290 val-aliq-impto-import            deci-2
 2300 val-aliq-ext-ipi                 deci-2
 2310 val-aliq-ext-pis                 deci-2      m
 2320 val-aliq-ext-cofins              deci-2      m
 2330 val-aliq-ext-icms                deci-2      m
 2340 val-aduana-cofins                deci-5
 2350 val-despes-aduana-cofins         deci-5
 2360 val-unit-pis-import              deci-2
 2370 val-unit-vol-pis-import          deci-2
 2380 val-unit-cofins-import           deci-2
 2390 val-unit-vol-cofins-import       deci-2
 2400 val-reduc-cofins-import          deci-2
 2410 val-reduc-pis-import             deci-2
 2420 cod-unid-negoc                   char
 2430 log-usa-unid-negoc-fifo          logi        m
 2440 id-bem                           deci-0      m
 2450 log-gerou-ncredito               logi        m
 2460 cod-parc-devol                   char
 2480 num-sit-trib-icms                inte        m
 2490 val-base-st-antec                deci-2
 2500 val-st-antec                     deci-2
 2510 cod-lote-fabrican                char
 2520 dat-valid-lote-fabrican          date
 2530 cod-barras                       char
 2570 dat-fabricc-lote                 date
 2580 nom-fabrican                     char
*/
