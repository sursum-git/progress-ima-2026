OUTPUT TO c:\temp\imp.txt.
   FOR EACH docum-est
    WHERE cod-estabel ='5'
    AND nat-operacao = '31201'
    AND cod-emitente = 26715
    AND nro-docto = '0063301':
    PUT "valor total :" docum-est.tot-valor SKIP
        "valor mercadoria:" valor-mercad   SKIP
"valor frete:" valor-frete SKIP
"valor embal:" valor-embal   SKIP
"valor outras:" valor-outras   SKIP
"valor seguro:" valor-seguro    SKIP
"valor imp frete:" vl-imp-frete SKIP
"valor imp import:" vl-imp-impor SKIP
"valor imp emb:" vl-imp-embalagem SKIP
"valor imp outras:" vl-imp-outras SKIP
"valor imp seguro" vl-imp-seguro SKIP .
PUT
   " it-codigo | cod-refer |  quantidade
            despesas[1]      | 
            despesas[2]|
            despesa-me[1] |
            despesa-me[2] |
            preco-total[1]     | 
            preco-total[2] |
            vl-imp-impor[1]     | 
            vl-imp-impor[2] |
            preco-unit-me     | 
            preco-total-me     | 
            vl-unit-mob        | 
            valor-frete         | 
            valor-pis            | 
            val-cofins           | 
            val-impto-retid       |         
            val-impto-retid-me     |        
            val-aduana-pis          |       
            val-despes-aduana-pis   |       
            val-aliq-impto-import   |       
            val-aliq-ext-ipi         |      
            val-aliq-ext-pis         |      
            val-aliq-ext-cofins       |     
            val-aliq-ext-icms         |     
            val-aduana-cofins         |     
            val-despes-aduana-cofins   |    
            val-unit-pis-import        |    
            val-unit-vol-pis-import    |    
            val-unit-cofins-import     |    
            val-unit-vol-cofins-import |
            preco-unit[1]|
            preco-unit[2] " 
           skip .
    FOR EACH item-doc-est OF docum-est:
        EXPORT DELIMITER "|" it-codigo cod-refer quantidade
            despesas[1]
             despesas[2]
            despesas-me[1]
            despesas-me[2]
            preco-total[1]
            preco-total[2]
            vl-imp-impor[1]
            vl-imp-impor[2]
            preco-unit-me    
            preco-total-me
            vl-unit-mob
            valor-frete
            valor-pis
            val-cofins
            val-impto-retid              
            val-impto-retid-me           
            val-aduana-pis               
            val-despes-aduana-pis        
            val-aliq-impto-import        
            val-aliq-ext-ipi             
            val-aliq-ext-pis             
            val-aliq-ext-cofins          
            val-aliq-ext-icms            
            val-aduana-cofins            
            val-despes-aduana-cofins     
            val-unit-pis-import          
            val-unit-vol-pis-import      
            val-unit-cofins-import       
            val-unit-vol-cofins-import
            preco-unit[1]
            preco-unit[2].
                             







    END.
END.
/*

=========================================================================
============================= Table: item-doc-est =======================

              Table Flags: "f" = frozen, "s" = a SQL table


Table                            Table Field Index Table
Name                             Flags Count Count Label
-------------------------------- ----- ----- ----- ------------------------
item-doc-est                             231     8 Itens NF Movimentadas p/

    Dump Name: in176
  Description: Itens das Notas Fiscais Movimentadas pelo Estoque
 Storage Area: Schema Area

       Trigger Event Trigger Procedure    Overridable? Check CRC?
       ------------- -------------------- ------------ ----------
       Delete        database/tgin/tdp/td yes          no


============================= FIELD SUMMARY =============================
============================= Table: item-doc-est =======================

Flags: <c>ase sensitive, <i>ndex component, <m>andatory, <v>iew component

Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
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

Field Name                       Format
-------------------------------- -----------------------------
cod-emitente                     >>>>>>>>9
nro-docto                        x(16)
serie-docto                      x(5)
nat-operacao                     x(06)
aliquota-ipi                     >>9.99
aliquota-iss                     >>9.99
cd-trib-icm                      >9
cd-trib-iss                      >9
class-fiscal                     9999.99.99
it-codigo                        x(16)
sequencia                        >>>>9
peso-liquido                     >>>>,>>9.99999
quantidade                       >>>>,>>>,>>9.9999
un                               xx
base-ipi                         >>>>,>>>,>>9.99
base-icm                         >>>>,>>>,>>9.99
base-iss                         >>>>,>>>,>>9.99
despesas                         >>>>,>>>,>>9.99
ipi-ntrib                        >>>>,>>>,>>9.99
icm-ntrib                        >>>>,>>>,>>9.99
iss-ntrib                        >>>>,>>>,>>9.99
ipi-outras                       >>>>,>>>,>>9.99
icm-outras                       >>>>,>>>,>>9.99
iss-outras                       >>>>,>>>,>>9.99
preco-total                      >>>>,>>>,>>9.99
icm-complem                      >>>>,>>>,>>9.99
cod-depos                        x(3)
lote                             x(40)
componente                       Sim/N∆o
nro-comp                         x(16)
serie-comp                       x(5)
aliquota-icm                     >>9.99
codigo-rejei                     >>9
cotacao-fasb                     >>>,>>9.99999999
ct-codigo                        x(20)
dt-retorno                       99/99/9999
desconto                         >>>>,>>>,>>9.99
dt-vali-lote                     99/99/9999
nr-ficha                         >>>>,>>9
atualiza-pa                      Sim/N∆o
encerra-pa                       Sim/N∆o
reabre-pa                        Sim/N∆o
nat-comp                         x(06)
nr-ord-produ                     >>>,>>>,>>9
num-pedido                       >>>>>,>>9
numero-ordem                     zzzzz9,99
parcela                          >>>>9
qt-do-forn                       >>>>,>>>,>>9.9999
sc-codigo                        x(20)
vl-imp-impor                     >>>>,>>>,>>9.99
nr-pedcli                        x(12)
nr-pd-seq                        >>,>>9
narrativa                        x(2000)
reabre-pd                        Sim/N∆o
valor-ipi                        >>>>,>>>,>>9.99
valor-icm                        >>>>,>>>,>>9.99
valor-iss                        >>>>,>>>,>>9.99
preco-unit                       >>>,>>>,>>9.99999
data-comp                        99/99/9999
dt-ent-prev                      99/99/9999
pr-total                         >>>>,>>>,>>9.99
baixa-ce                         Sim/N∆o
dt-nota-comp                     99/99/9999
cod-refer                        x(8)
base-icm-cmi                     >>>>,>>>,>>9.99
base-ipi-cmi                     >>>>,>>>,>>9.99
base-iss-cmi                     >>>>,>>>,>>9.99
desconto-cmi                     >>>>,>>>,>>9.99
despesas-cmi                     >>>>,>>>,>>9.99
icm-comp-cmi                     >>>>,>>>,>>9.99
icm-ntr-cmi                      >>>>,>>>,>>9.99
icm-out-cmi                      >>>>,>>>,>>9.99
ipi-ntr-cmi                      >>>>,>>>,>>9.99
ipi-out-cmi                      >>>>,>>>,>>9.99
iss-ntr-cmi                      >>>>,>>>,>>9.99
iss-out-cmi                      >>>>,>>>,>>9.99
pr-total-cmi                     >>>>,>>>,>>9.99
vl-icm-cmi                       >>>>,>>>,>>9.99
vl-ipi-cmi                       >>>>,>>>,>>9.99
vl-iss-cmi                       >>>>,>>>,>>9.99
vl-imp-cmi                       >>>>,>>>,>>9.99
seq-comp                         >>>>9
vl-subs-cmi                      >>>>,>>>,>>9.99
nr-pd-ent                        >>>>9
cod-emit-benef                   >>>>>>>>9
i-nr-gi                          99999999
c-nr-invoice                     x(10)
i-seq-inv                        999
pc-restituicao                   >>9.99
nivel-restituicao                !
emite-comp                       >>>>>>>>9
cotacao-cmi                      >>>,>>9.99999999
pr-unit-cmi                      >>>,>>>,>>9.99999
pr-mob-cmi                       >>>,>>>,>>9.9999
vl-subs                          >>>>>,>>>,>>9.99
base-subs                        >>>>>,>>>,>>9.99
pre-unit-mob                     >>>,>>>,>>9.99999
base-subs-cmi                    >>>>,>>>,>>9.99
qt-real                          >>>>>,>>9.9999
num-ord-inv                      >>>,>>9
nat-of                           x(06)
cod-localiz                      x(20)
cd-trib-ipi                      >9
conta-contabil                   x(20)
item-pai                         x(16)
cod-roteiro                      x(16)
op-codigo                        >>>>9
origem                           >9
cod-esp                          !!
vl-taxa                          >>>,>>>,>>>,>>9.99
cod-tax                          >>>9
vl-isr                           >>>,>>>,>>9.99
hora                             x(08)
vl-taxa-me                       >>>,>>>,>>>,>>9.99
fn-docto-ap                      >>>>9
qt-saldo                         >>>,>>9.9999
preco-total-alt                  >>>>,>>>,>>9.99
nr-docto-ap                      x(16)
preco-fatura                     >>>,>>>,>>9.99
cota-moeda                       ->>,>>9.9999
perc-vat                         >>9.99
perc-sales-tax                   >>9.99
perc-isr                         >>9.99
cod-tax-isr                      >>9
usuario                          x(12)
data                             99/99/9999
est-cob                          x(5)
preco-unit-me                    >>>>>,>>>,>>9.99999
preco-total-me                   >>>>>,>>>,>>9.9999
desconto-me                      >>>>>,>>>,>>9.99
sit-item                         >9
cotacao-alt                      >>>,>>>,>>9.9999
vl-unit-mob                      ->>>,>>>,>>9.99
etiquetas                        zz9
flag-atu                         9
qt-etiquetas                     zz9
char-1                           x(2000)
char-2                           x(2000)
dec-1                            ->>>>>>>>>>>9.99999999
dec-2                            ->>>>>>>>>>>9.99999999
int-1                            ->>>>>>>>>9
int-2                            ->>>>>>>>>9
log-1                            Sim/N∆o
log-2                            Sim/N∆o
data-1                           99/99/9999
data-2                           99/99/9999
check-sum                        x(20)
vl-isr-me                        >>>,>>>,>>9.99
despesas-me                      >>>>,>>>,>>9.99
quant-conf                       >>>>>,>>9.9999
base-pis-subs                    >>>>,>>>,>>9.99
base-cofins-subs                 >>>>,>>>,>>9.99
vl-pis-subs                      >>>>,>>>,>>9.99
vl-cofins-subs                   >>>>,>>>,>>9.99
log-fifo-oc                      Sim/N∆o
seq-evento                       >>>>9
log-icm-retido                   Sim/N∆o
cod-emit-terc                    >>>>>>>>9
nro-docto-terc                   X(16)
serie-terc                       X(5)
nat-terc                         x(06)
seq-terc                         >>,>>9
nr-ato-concessorio               X(20)
valor-frete                      >>>>,>>>,>>9.99
valor-frete-me                   >>>>,>>>,>>9.99
nr-proc-imp                      X(12)
declaracao-import                X(20)
num-seq-rma                      >>>>9
log-geracao-ncr-ap               Sim/N∆o
val-perc-restocagem              >9.99
val-restocagem                   >>>,>>9.9999
cdn-devolucao                    9
cod-motivo                       X(3)
cod-disposicao                   X(3)
cod-despesa                      X(12)
fornec-comp                      >>>>>>>>9
val-perc-rep-ipi                 >>9.9999
val-perc-red-icms                >>9.9999
val-sdo-alocad-oc                ->>,>>>,>>>,>>9.99
val-sdo-alocad-terc              ->>,>>>,>>>,>>9.99
num-event-contrat                >>>>,>>9
num-ret-om                       >>>>,>>9
peso-bruto-item                  >>>,>>>,>>9.99999
peso-liquido-item                >>>,>>>,>>9.99999
cod-estab-compon                 x(5)
idi-tributac-cofins              9
val-aliq-cofins                  >>9.99
val-base-calc-cofins             >>>,>>>,>>>,>>9.99
val-cofins                       >>>,>>>,>>>,>>9.99
val-cofins-fasb                  >>>,>>>,>>>,>>9.99
val-cofins-cmi                   >>>,>>>,>>>,>>9.99
idi-tributac-pis                 9
val-aliq-pis                     >>9.99
base-pis                         >>>,>>>,>>>,>>9.99
valor-pis                        >>>,>>>,>>>,>>9.99
valor-pis-fasb                   >>>,>>>,>>>,>>9.99
valor-pis-cmi                    >>>,>>>,>>>,>>9.99
num-ord-import                   zzzzz9,99
num-parc-import                  >>>>9
cdn-impto-retid                  >>>9
val-aliq-impto-retid             >>9.99
val-impto-retid                  >>>,>>>,>>>,>>9.99
val-impto-retid-me               >>>,>>>,>>>,>>9.99
val-aduana-pis                   >>>>,>>>,>>>,>>9.99999
val-despes-aduana-pis            >>>>,>>>,>>>,>>9.99999
val-aliq-impto-import            >>9.99
val-aliq-ext-ipi                 >>9.99
val-aliq-ext-pis                 >>9.99
val-aliq-ext-cofins              >>9.99
val-aliq-ext-icms                >>9.99
val-aduana-cofins                >>>>,>>>,>>>,>>9.99999
val-despes-aduana-cofins         >>>>,>>>,>>>,>>9.99999
val-unit-pis-import              >>,>>>,>>>,>>9.99
val-unit-vol-pis-import          >>,>>>,>>>,>>9.99
val-unit-cofins-import           >>,>>>,>>>,>>9.99
val-unit-vol-cofins-import       >>,>>>,>>>,>>9.99
val-reduc-cofins-import          >>9.99
val-reduc-pis-import             >>9.99
cod-unid-negoc                   X(3)
log-usa-unid-negoc-fifo          Sim/N∆o
id-bem                           >>>,>>>,>>9
log-gerou-ncredito               Sim/N∆o
cod-parc-devol                   x(2)
num-sit-trib-icms                999
val-base-st-antec                ->>,>>>,>>>,>>9.99
val-st-antec                     ->>,>>>,>>>,>>9.99
cod-lote-fabrican                x(40)
dat-valid-lote-fabrican          99/99/9999
cod-barras                       x(50)
dat-fabricc-lote                 99/99/9999
nom-fabrican                     x(60)

Field Name                       Initial
-------------------------------- -----------------------------
cod-emitente                     0
nro-docto
serie-docto
nat-operacao
aliquota-ipi                     0
aliquota-iss                     0
cd-trib-icm                      1
cd-trib-iss                      1
class-fiscal
it-codigo
sequencia                        0
peso-liquido                     0
quantidade                       0
un
base-ipi                         0
base-icm                         0
base-iss                         0
despesas                         0
ipi-ntrib                        0
icm-ntrib                        0
iss-ntrib                        0
ipi-outras                       0
icm-outras                       0
iss-outras                       0
preco-total                      0
icm-complem                      0
cod-depos
lote
componente                       no
nro-comp
serie-comp
aliquota-icm                     0
codigo-rejei                     0
cotacao-fasb                     0
ct-codigo
dt-retorno                       today
desconto                         0
dt-vali-lote                     ?
nr-ficha                         0
atualiza-pa                      yes
encerra-pa                       no
reabre-pa                        no
nat-comp
nr-ord-produ                     0
num-pedido                       0
numero-ordem                     0
parcela                          0
qt-do-forn                       0
sc-codigo
vl-imp-impor                     0
nr-pedcli
nr-pd-seq                        0
narrativa
reabre-pd                        no
valor-ipi                        0
valor-icm                        0
valor-iss                        0
preco-unit                       0
data-comp                        ?
dt-ent-prev                      ?
pr-total                         0
baixa-ce                         yes
dt-nota-comp                     ?
cod-refer
base-icm-cmi                     0
base-ipi-cmi                     0
base-iss-cmi                     0
desconto-cmi                     0
despesas-cmi                     0
icm-comp-cmi                     0
icm-ntr-cmi                      0
icm-out-cmi                      0
ipi-ntr-cmi                      0
ipi-out-cmi                      0
iss-ntr-cmi                      0
iss-out-cmi                      0
pr-total-cmi                     0
vl-icm-cmi                       0
vl-ipi-cmi                       0
vl-iss-cmi                       0
vl-imp-cmi                       0
seq-comp                         0
vl-subs-cmi                      0
nr-pd-ent                        0
cod-emit-benef                   0
i-nr-gi                          0
c-nr-invoice
i-seq-inv                        0
pc-restituicao                   0
nivel-restituicao
emite-comp                       0
cotacao-cmi                      0
pr-unit-cmi                      0
pr-mob-cmi                       0
vl-subs                          0
base-subs                        0
pre-unit-mob                     0
base-subs-cmi                    0
qt-real                          0
num-ord-inv                      0
nat-of
cod-localiz
cd-trib-ipi                      1
conta-contabil
item-pai
cod-roteiro
op-codigo                        0
origem                           9
cod-esp
vl-taxa                          0
cod-tax                          0
vl-isr                           0
hora
vl-taxa-me                       0
fn-docto-ap                      0
qt-saldo                         0
preco-total-alt                  0
nr-docto-ap
preco-fatura                     0
cota-moeda                       0
perc-vat                         0
perc-sales-tax                   0
perc-isr                         0
cod-tax-isr                      0
usuario
data                             today
est-cob
preco-unit-me                    0
preco-total-me                   0
desconto-me                      0
sit-item                         1
cotacao-alt                      0
vl-unit-mob                      0
etiquetas                        0
flag-atu                         2
qt-etiquetas                     1
char-1
char-2
dec-1                            0
dec-2                            0
int-1                            0
int-2                            0
log-1                            no
log-2                            no
data-1                           ?
data-2                           ?
check-sum
vl-isr-me                        0
despesas-me                      0
quant-conf                       0
base-pis-subs                    0
base-cofins-subs                 0
vl-pis-subs                      0
vl-cofins-subs                   0
log-fifo-oc                      No
seq-evento                       0
log-icm-retido                   No
cod-emit-terc                    0
nro-docto-terc
serie-terc
nat-terc
seq-terc                         0
nr-ato-concessorio
valor-frete                      0
valor-frete-me                   0
nr-proc-imp
declaracao-import
num-seq-rma                      0
log-geracao-ncr-ap               no
val-perc-restocagem              0
val-restocagem                   0
cdn-devolucao                    0
cod-motivo
cod-disposicao
cod-despesa
fornec-comp                      0
val-perc-rep-ipi                 0
val-perc-red-icms                0
val-sdo-alocad-oc                0
val-sdo-alocad-terc              0
num-event-contrat                0
num-ret-om                       0
peso-bruto-item                  0
peso-liquido-item                0
cod-estab-compon
idi-tributac-cofins              2
val-aliq-cofins                  0
val-base-calc-cofins             0
val-cofins                       0
val-cofins-fasb                  0
val-cofins-cmi                   0
idi-tributac-pis                 2
val-aliq-pis                     0
base-pis                         0
valor-pis                        0
valor-pis-fasb                   0
valor-pis-cmi                    0
num-ord-import                   0
num-parc-import                  0
cdn-impto-retid                  0
val-aliq-impto-retid             0
val-impto-retid                  0
val-impto-retid-me               0
val-aduana-pis                   0
val-despes-aduana-pis            0
val-aliq-impto-import            0
val-aliq-ext-ipi                 0
val-aliq-ext-pis                 0
val-aliq-ext-cofins              0
val-aliq-ext-icms                0
val-aduana-cofins                0
val-despes-aduana-cofins         0
val-unit-pis-import              0
val-unit-vol-pis-import          0
val-unit-cofins-import           0
val-unit-vol-cofins-import       0
val-reduc-cofins-import          0
val-reduc-pis-import             0
cod-unid-negoc
log-usa-unid-negoc-fifo          no
id-bem                           0
log-gerou-ncredito               no
cod-parc-devol
num-sit-trib-icms                0
val-base-st-antec                0
val-st-antec                     0
cod-lote-fabrican
dat-valid-lote-fabrican          ?
cod-barras
dat-fabricc-lote                 ?
nom-fabrican

Field Name                     Label                  Column Label
------------------------------ ---------------------- ----------------------
cod-emitente                   Emitente               Emit
nro-docto                      Documento              Documento
serie-docto                    SÇrie                  Ser
nat-operacao                   Nat Operaá∆o           Nat Oper
aliquota-ipi                   Aliquota IPI           % IPI
aliquota-iss                   Aliquota ISS           % ISS
cd-trib-icm                    C¢digo Tributaá∆o ICMS Cod Trib ICMS
cd-trib-iss                    C¢digo Tributaá∆o ISS  Cod Trib ISS
class-fiscal                   Classificaá∆o Fiscal   Class Fisc
it-codigo                      Item                   Item
sequencia                      Seq                    Seq
peso-liquido                   Peso Liq               Peso Liq
quantidade                     Nossa Qtde             Nossa Qtde
un                             Unid                   Un
base-ipi                       Base C†lculo IPI       Base Calc IPI
base-icm                       Base C†lculo ICMS      Base Calc ICMS
base-iss                       Base C†lculo ISS       Base Calc ISS
despesas                       Despesas               Despesas
ipi-ntrib                      IPI N∆o Tributado      IPI N∆o Tributado
icm-ntrib                      ICMS N∆o Tributado     ICMS N∆o Tributado
iss-ntrib                      ISS N∆o Tributado      ISS N∆o Tributado
ipi-outras                     IPI Outras             IPI Outras
icm-outras                     ICMS Outras            ICMS Outras
iss-outras                     ISS Outras             ISS Outras
preco-total                    Preáo Total            Preáo Total
icm-complem                    ICMS Complementar      ICMS Complementar
cod-depos                      Dep¢sito               Dep
lote                           Lote/Serie             Lote/SÇrie
componente                     Componente             Compon
nro-comp                       Numero Comp            Num Comp
serie-comp                     SÇrie                  Ser Comp
aliquota-icm                   Aliquota ICMS          % ICMS
codigo-rejei                   C¢digo Devoluá∆o       Dev
cotacao-fasb                   Cotaá∆o Moeda FASB     Cot Moeda FASB
ct-codigo                      Conta                  Cta
dt-retorno                     Data Retorno           Retorno
desconto                       Desconto               Desconto
dt-vali-lote                   Validade Lote          Valid Lote
nr-ficha                       Ficha                  Ficha
atualiza-pa                    Atualiza Parcela       Atu Pa
encerra-pa                     Encerra Parcela        En Pa
reabre-pa                      Reabre Parc            Reabre Pa
nat-comp                       Natureza Complementar  Nat Comp
nr-ord-produ                   Ordem Produá∆o         Ord Prod
num-pedido                     Pedido                 Pedido
numero-ordem                   Ordem Compra           Ordem
parcela                        Parcela                Parc
qt-do-forn                     Qtde Emitente          Qtde Emitente
sc-codigo                      Sub-Conta              SC
vl-imp-impor                   Imposto Import         Imposto de Importacao
nr-pedcli                      Pedido Cliente         Ped Cliente
nr-pd-seq                      SeqÅància              Seq Ped
narrativa                      Narrativa              Narrativa
reabre-pd                      Reabre Ped             Reabre PD
valor-ipi                      Valor IPI              Vlr IPI
valor-icm                      Valor ICMS             Vlr ICMS
valor-iss                      Valor ISS              Vlr ISS
preco-unit                     Preáo Unit             Preáo Unit†rio
data-comp                      Data Nota Comp         Nota Comp
dt-ent-prev                    Data Entrega Prevista  Entr Prev
pr-total                       Preáo Total            Preáo Total
baixa-ce                       Baixa Estoque          Bx Estoque
dt-nota-comp                   Data Nota Complementar Nota Complem
cod-refer                      Referància             Ref
base-icm-cmi                   Base C†lculo ICMS      Base Calc ICMS
base-ipi-cmi                   Base C†lculo IPI       Base Calc IPI
base-iss-cmi                   Base C†lculo ISS       Base Calc ISS
desconto-cmi                   Desconto               Desconto
despesas-cmi                   Despesas               Despesas
icm-comp-cmi                   ICMS Complementar      ICMS Complementar
icm-ntr-cmi                    ICMS N∆o Tributado     ICMS N∆o Tributado
icm-out-cmi                    ICMS Outros            ICMS Outros
ipi-ntr-cmi                    IPI N∆o Tributado      IPI N∆o Tributado
ipi-out-cmi                    IPI Outros             IPI Outros
iss-ntr-cmi                    ISS N∆o Tributado      ISS N∆o Tributado
iss-out-cmi                    ISS Outros             ISS Outros
pr-total-cmi                   Preáo Total            Preáo Total
vl-icm-cmi                     Valor ICMS             Vlr ICMS
vl-ipi-cmi                     Valor IPI              Vlr IPI
vl-iss-cmi                     Valor ISS              Vlr ISS
vl-imp-cmi                     Imposto Import         Impos Importacao
seq-comp                       Seq Complem            Seq Comp
vl-subs-cmi                    Valor Subst Trib CMCAC Vl Subst Trib CMI
nr-pd-ent                      Numero Entrega         Entrega
cod-emit-benef                 Emitente Beneficiament Emitente Benefic
i-nr-gi                        N£mero GI              Num GI
c-nr-invoice                   N£mero Invoice         Invoice
i-seq-inv                      Seq Invoice            Seq
pc-restituicao                 % Restituiá∆o          % Rest
nivel-restituicao              N°vel Restituiá∆o      Niv Restit
emite-comp                     Emitente Comp          Emit Comp
cotacao-cmi                    Cotaá∆o Moeda CMI      Cot Moeda CMI
pr-unit-cmi                    Preáo Unit CMI         Preáo Unit CMI
pr-mob-cmi                     Preco Mob CMI          Preco Mob CMI
vl-subs                        Valor Subst Trib       Vl Subst Trib
base-subs                      Base Substituiá∆o Trib Base Subst Trib
pre-unit-mob                   Preáo Unit Mob         Preáo Unit Mob
base-subs-cmi                  Base Substituiá∆o CMI  Base Subst CMI
qt-real                        Qtde Real              Qtde Real
num-ord-inv                    Ordem Invest           Ordem Inv
nat-of                         Natureza               Natureza
cod-localiz                    Localizaá∆o            Localiz
cd-trib-ipi                    C¢digo Tributaá∆o IPI  Cod Trib IPI
conta-contabil                 Conta Cont†bil         Conta Cont†bil
item-pai                       Item Pai               Item Pai
cod-roteiro                    Roteiro                Roteiro
op-codigo                      Operaá∆o               Oper
origem                         Origem                 Origem
cod-esp                        EspÇcie                Esp
vl-taxa                        Valor Imposto          Vl Imposto
cod-tax                        C¢digo Imposto         Cod Imposto
vl-isr                         Valor ISR              Valor ISR
hora                           Hora                   Hora
vl-taxa-me                     Valor Imposto          Valor Imposto
fn-docto-ap                    Fornecedor             Fornecedor
qt-saldo                       Qtd Saldo              Qtd Saldo
preco-total-alt                Preáo Altern           Preáo Altern
nr-docto-ap                    Documento              Documento
preco-fatura                   Preáo Fatura           Preáo Fatura
cota-moeda                     Cotaá∆o                Cotaá∆o
perc-vat                       Perc Taxa              % Taxa
perc-sales-tax                 Perc Venda             % Venda
perc-isr                       Taxa Serviáo           % Taxa
cod-tax-isr                    Cod Taxa Serv          Tx Serv
usuario                        Usu†rio                Usu†rio
data                           Data Movto             Data Movto
est-cob                        Estab Cobranáa         Est Cob
preco-unit-me                  Preáo Unit ME          Preáo Unit ME
preco-total-me                 Preáo Total ME         Preáo Total ME
desconto-me                    Desconto ME            Desconto ME
sit-item                       Situaá∆o ÷tem          Sit ÷tem
cotacao-alt                    Cotaá∆o Moeda Alt      Cot Altern
vl-unit-mob                    Valor Unit†rio M∆o-de- Vl Unit MOB
etiquetas                      Qt. Etiquetas          Qt. Etiq.
flag-atu                       Indicador Atualizaá∆o  Atualiz
qt-etiquetas                   Quantidade Etiqueta    Qtd Etq
char-1                         ?                      ?
char-2                         ?                      ?
dec-1                          ?                      ?
dec-2                          ?                      ?
int-1                          ?                      ?
int-2                          ?                      ?
log-1                          ?                      ?
log-2                          ?                      ?
data-1                         ?                      ?
data-2                         ?                      ?
check-sum                      Check-sum              ?
vl-isr-me                      Valor ISR              Valor ISR
despesas-me                    Despesas ME            Despesas ME
quant-conf                     Qtde Conferida         Qtde Conferida
base-pis-subs                  Base PIS Subs          Base PIS Subs
base-cofins-subs               Base COFINS Subs       Base COFINS Subs
vl-pis-subs                    Valor PIS Subs         Valor PIS Subs
vl-cofins-subs                 Valor COFINS Subs      Valor COFINS Subs
log-fifo-oc                    FIFO Ordem Compra      FIFO OC
seq-evento                     Seq                    Seq
log-icm-retido                 ICMS Retido            ICMS Retido
cod-emit-terc                  Emitente Entrega       Emit Entrega
nro-docto-terc                 Nota Entrega           Nota Entrega
serie-terc                     SÇrie Entrega          SÇrie Ent
nat-terc                       Natureza Entrega       Natureza Entrega
seq-terc                       SeqÅància Entrega      Seq Ent
nr-ato-concessorio             Nr. Ato Concess¢rio    ?
valor-frete                    Valor Frete            Vl Frete
valor-frete-me                 Valor Frete ME         Vl Frete ME
nr-proc-imp                    Processo Imp           Proc Imp
declaracao-import              Declaraá∆o Importaá∆o  Declaraá∆o Importaá∆o
num-seq-rma                    Seq RMA                Seq RMA
log-geracao-ncr-ap             Geraá∆o Nota CrÇdito A Geraá∆o NC AP
val-perc-restocagem            Perc Restocagem        % Restocagem
val-restocagem                 Val Restocagem         Vl Restocagem
cdn-devolucao                  Tipo Devoluá∆o         Tp Devoluá∆o
cod-motivo                     C¢digo Motivo          C¢d Motivo
cod-disposicao                 C¢digo Disposiá∆o      C¢d Disposiá∆o
cod-despesa                    Despesa                Despesa
fornec-comp                    Fornecedor Remito      Forn Rem
val-perc-rep-ipi               % Red IPI              % Red IPI
val-perc-red-icms              % Red ICMS             % Red ICMS
val-sdo-alocad-oc              Saldo Alocado Ordem co Saldo Alocado Ordem
val-sdo-alocad-terc            Saldo Alocado Poder Te Saldo Alocado Poder
num-event-contrat              Evento Contrato        Evento Contrato
num-ret-om                     Retorno Ordem manut    Retorno Ordem manut
peso-bruto-item                Peso Bruto             Peso Bruto
peso-liquido-item              Peso Liquido           Peso Liq
cod-estab-compon               Estabel Comp           Estab Comp
idi-tributac-cofins            Trib COFINS            Trib COFINS
val-aliq-cofins                Al°quota COFINS        Aliquota Cof
val-base-calc-cofins           Base C†lculo COFINS    ?
val-cofins                     Valor COFINS           ?
val-cofins-fasb                Valor COFINS FASB      COFINS FASB
val-cofins-cmi                 Valor COFINS CMI       COFINS CMI
idi-tributac-pis               Trib PIS               Trib PIS
val-aliq-pis                   Al°quota PIS           % PIS
base-pis                       Base Calculo PIS       Base PIS
valor-pis                      Valor PIS              ?
valor-pis-fasb                 Valor PIS FASB         PIS FASB
valor-pis-cmi                  PIS CMI                ?
num-ord-import                 Num Ordem Import       Num Ord Imp
num-parc-import                Parcela Importacao     Parcela Imp
cdn-impto-retid                Imposto Retido         Impto Ret
val-aliq-impto-retid           Taxa Retido            Taxa Ret
val-impto-retid                Valor Imposto Retido   Vl Imp Ret
val-impto-retid-me             Valor Imposto Retido   Vl Imposto Ret
val-aduana-pis                 Val Aduana Pis         Val Adu Pis
val-despes-aduana-pis          Val DA Pis             Val DA Pis
val-aliq-impto-import          Al°quota II            % II
val-aliq-ext-ipi               Aliq Ext IPI           IPI Ext
val-aliq-ext-pis               % Externo PIS          % Ext PIS
val-aliq-ext-cofins            % Externo COFINS       % Ext COFINS
val-aliq-ext-icms              Aliq Ext ICMS          ICMS Ext
val-aduana-cofins              Val Adu COFINS         Val COFINS
val-despes-aduana-cofins       Desp Adua COFINS       Desp COFINS
val-unit-pis-import            Val Unit PIS Imp       Val Unit PIS Imp
val-unit-vol-pis-import        Val Unit Vol PIS       Unit Vol PIS
val-unit-cofins-import         Val Unit COFINS Imp    Val Unit COFINS
val-unit-vol-cofins-import     Val Unit Vol COFINS    Val Unit Vol COFINS
val-reduc-cofins-import        Val Red COFINS Imp     Val Red COFINS Imp
val-reduc-pis-import           Val Red PIS Imp        Val Red PIS Imp
cod-unid-negoc                 Unidade Neg¢cio        Unid Negoc
log-usa-unid-negoc-fifo        Usa Unidade Negocio FI Unid Negoc FIFO
id-bem                         Id Bem                 Id Bem
log-gerou-ncredito             Gerou Nota de CrÇdito  Gerou NCredito
cod-parc-devol                 Parc Dev               Parc  Dev
num-sit-trib-icms              Situaá∆o Tribut†ria IC Sit Trib ICMS
val-base-st-antec              Base St Antecipado     Base St Antecipado
val-st-antec                   St Antecipado          St Antecipado
cod-lote-fabrican              Lote Fabricante        Lote Fabricante
dat-valid-lote-fabrican        Validade Lote Fabrican Validade Lote Fabric
cod-barras                     C¢digo Barras          C¢digo Barras
dat-fabricc-lote               Data Fabricaá∆o        Dt Fabric
nom-fabrican                   Fabricante             Fabricante


============================= INDEX SUMMARY =============================
============================= Table: item-doc-est =======================

Flags: <p>rimary, <u>nique, <w>ord, <a>bbreviated, <i>nactive, + asc, - desc

Flags Index Name                       Cnt Field Name
----- -------------------------------- --- ---------------------------------
      ch-ato                             1 + nr-ato-concessorio

      componente                         6 + cod-emitente
                                           + serie-comp
                                           + nro-comp
                                           + nat-comp
                                           + seq-comp
                                           + it-codigo

      disposicao                         1 + cod-disposicao

pu    documento                          5 + serie-docto
                                           + nro-docto
                                           + cod-emitente
                                           + nat-operacao
                                           + sequencia

      item                               1 + it-codigo

      itmdctst-09                        3 + num-pedido
                                           + numero-ordem
                                           + parcela

      motivo                             1 + cod-motivo

      terceiro                           6 + cod-emit-terc
                                           + serie-terc
                                           + nro-docto-terc
                                           + nat-terc
                                           + seq-terc
                                           + it-codigo

** Index Name: ch-ato
 Storage Area: Schema Area
** Index Name: componente
 Storage Area: Schema Area
** Index Name: disposicao
 Storage Area: Schema Area
** Index Name: documento
 Storage Area: Schema Area
** Index Name: item
 Storage Area: Schema Area
** Index Name: itmdctst-09
 Storage Area: Schema Area
** Index Name: motivo
 Storage Area: Schema Area
** Index Name: terceiro
 Storage Area: Schema Area


============================= FIELD DETAILS =============================
============================= Table: item-doc-est =======================

** Field Name: nro-docto
         Help: N£mero do Documento

** Field Name: class-fiscal
      Val-Msg: Classificaá∆o fiscal inexistente.
      Val-Exp: can-find(classif-fisc where classif-fisc.class-fiscal =
               item-doc-est.class-fiscal)

** Field Name: it-codigo
      Val-Msg: Item nao cadastrado
      Val-Exp: can-find(item where item.it-codigo = item-doc-est.it-codigo)

** Field Name: sequencia
  Description: sequencia
         Help: Permite informar a seqÅància do componente

** Field Name: quantidade
         Help: Quantidade na nossa unidade de medida

** Field Name: preco-total
         Help: Preáo total do item, sem desconto, sem IPI e com ICMS

** Field Name: cod-depos
      Val-Msg: Dep¢sito inexistente.
      Val-Exp: can-find(deposito of item-doc-est)

** Field Name: lote
         Help: Lote ou numero de serie

** Field Name: componente
  Description: Indica, no beneficiamento, se o item e' componente ou nao

** Field Name: codigo-rejei
      Val-Msg: Devoluá∆o inexistente.
      Val-Exp: can-find(cod-rejeicao of item-doc-est)

** Field Name: dt-retorno
      Val-Exp: dt-retorno >= docum-est.dt-emissao

** Field Name: nr-ficha
         Help: Informe o numero do Roteiro de Inspeá∆o

** Field Name: atualiza-pa
         Help: Indica se o saldo da parcela da ordem ser† atualizado

** Field Name: encerra-pa
         Help: Indica se a parcela da ordem de compra ser† encerrada

** Field Name: reabre-pa
         Help: Indica, na devoluá∆o, se a parcela de compra ser†' reaberta

** Field Name: nat-comp
         Help: Possui a natureza do documento de origem

** Field Name: nr-ord-produ
         Help: Informe o numero da ordem de produá∆o
      Val-Msg: Ordem de produá∆o n∆o cadastrada
      Val-Exp: item-doc-est.nr-ord-prod = 0 or

can-find(ord-prod where
               ord-prod.nr-ord-prod = 


               item-doc-est.nr-ord-prod)

** Field Name: num-pedido
         Help: Numero do pedido de compra que esta' sendo recebido
      Val-Msg: Pedido n∆o cadastrado
      Val-Exp: item-doc-est.num-pedido = ? or

item-doc-est.num-pedido = 0
               or

can-find(pedido-compr where pedido-compr.num-pedido =
               item-doc-est.num-pedido)

** Field Name: numero-ordem
         Help: Ordem de compra do pedido que esta' sendo recebido
      Val-Msg: Ordem de Compra n∆o cadastrada
      Val-Exp: item-doc-est.numero-ordem = ? or

item-doc-est.numero-ordem
               = 0 or

can-find(ordem-compra where
               ordem-compra.numero-ordem =
               item-doc-est.numero-ordem)

** Field Name: parcela
         Help: Parcela da Ordem de Compra

** Field Name: qt-do-forn
         Help: Quantidade na unidade de medida do emitente

** Field Name: vl-imp-impor
         Help: Valor dos impostos de importaá∆o

** Field Name: nr-pedcli
         Help: N£mero do Pedido do Cliente

** Field Name: nr-pd-seq
         Help: Numero sequencial do item no pedido do cliente

** Field Name: narrativa
         Help: Narrativa

** Field Name: reabre-pd
         Help: Indica se o pedido do cliente deve ou n∆o ser reaberto

** Field Name: pr-total
         Help: Preáo total do item, sem desconto, sem IPI e com ICMS

** Field Name: baixa-ce
         Help: Indica se o item dever† ser baixado do estoque

** Field Name: cod-refer
  Description: Informe o codigo da Referencia - prod acabado controlado
               por referencia

** Field Name: base-icm-cmi
  Description: Valor da base de ICMS convertido para CMI

** Field Name: base-ipi-cmi
  Description: Valor base do IPI convertido para CMI

** Field Name: base-iss-cmi
  Description: Valor da base de ISS convertido para CMI

** Field Name: desconto-cmi
  Description: Desconro convertido para CMI

** Field Name: despesas-cmi
  Description: Valor de despesas convertido para CMI

** Field Name: icm-comp-cmi
  Description: Icms complementar convertido para CMI

** Field Name: icm-ntr-cmi
  Description: ICMS nao tributado convertido para CMI

** Field Name: icm-out-cmi
  Description: ICMS Outros convertido para CMI

** Field Name: ipi-ntr-cmi
  Description: IPI nao tributado convertido para CMI

** Field Name: ipi-out-cmi
  Description: IPI outros convertido para CMI

** Field Name: iss-ntr-cmi
  Description: ISS nao tributado convertido para CMI

** Field Name: iss-out-cmi
  Description: ISS outros convertido para CMI

** Field Name: pr-total-cmi
  Description: Preco total para CMI

** Field Name: vl-icm-cmi
  Description: Valor ICMS convertido para CMI

** Field Name: vl-ipi-cmi
  Description: Valor IPI convertido para CMI

** Field Name: vl-iss-cmi
  Description: Valor ISS convertido para CMI

** Field Name: vl-imp-cmi
  Description: Imposto de importacao convertido para CMI

** Field Name: seq-comp
         Help: SeqÅància Complementar

** Field Name: vl-subs-cmi
         Help: Valor para substituiá∆o tributaria do CMI

** Field Name: nr-pd-ent
         Help: Numero da seqÅància da entrega do item do pedido de venda

** Field Name: cod-emit-benef
  Description: Codigo do emitente de beneficiamento

** Field Name: i-nr-gi
         Help: Informe o Nro da Guia de Importaá∆o

** Field Name: pc-restituicao
         Help: Percentual de Restituiá∆o

** Field Name: nivel-restituicao
         Help: N°vel de Restituiá∆o para a Zona Franca de Manaus

** Field Name: emite-comp
         Help: Emitente Comp

** Field Name: vl-subs
         Help: Valor substituiá∆o tributaria

** Field Name: pre-unit-mob
         Help: Preáo unit†rio da m∆o-de-obra

** Field Name: base-subs-cmi
         Help: Base substituiá∆o tribut†ria em CMI

** Field Name: qt-real
         Help: Quantidade Real Recebida

** Field Name: num-ord-inv
         Help: Numero da Ordem de Investimento (Numero Magnus)

** Field Name: cod-localiz
  Description: C¢digo da localizaá∆o do material

** Field Name: item-pai
  Description: Item pai do item desta reserva na estrutura.
         Help: Item pai do item desta reserva na estrutura

** Field Name: cod-roteiro
  Description: C¢digo do roteiro

** Field Name: op-codigo
         Help: C¢digo da operaá∆o

** Field Name: origem
         Help: Origem da Remessa para Beneficiamento

** Field Name: cod-esp
         Help: C¢digo da espÇcie do documento

** Field Name: vl-taxa
         Help: Valor do Imposto

** Field Name: cod-tax
         Help: C¢digo do Imposto

** Field Name: hora
         Help: Hora

** Field Name: vl-taxa-me
         Help: Valor do imposto

** Field Name: preco-total-alt
  Description: Preáo alternativo
         Help: Preáo total em moeda alternativa

** Field Name: cota-moeda
         Help: Cotaá∆o da Moeda

** Field Name: perc-vat
         Help: Percentual da Taxa

** Field Name: perc-sales-tax
         Help: Percentual de taxa de venda

** Field Name: perc-isr
         Help: Percentual taxa de serviáo

** Field Name: cod-tax-isr
         Help: C¢digo da taxa de serviáo

** Field Name: usuario
         Help: F5 para zoom

** Field Name: data
         Help: Data movimento

** Field Name: est-cob
         Help: Estabelecimento de cobranáa

** Field Name: preco-unit-me
         Help: Preáo unit†rio em moeda estrangeira

** Field Name: preco-total-me
         Help: Preáo total em moeda estrangeira

** Field Name: desconto-me
         Help: Desconto em moeda estrangeira

** Field Name: sit-item
  Description: Indica a situaá∆o do item em relaá∆o Ö entrada da fatura no
               contas a pagar
         Help: Situaá∆o do °tem em relaá∆o a entrada da fatura no Ctas
               Pagar

** Field Name: cotacao-alt
         Help: Informe o valor da cotaá∆o na moeda alternativa do Estoque

** Field Name: vl-unit-mob
         Help: Valor unit†rio de m∆o-de-obra

** Field Name: etiquetas
  Description: Quantidade de Etiquetas
         Help: Quantidade de Etiquetas

** Field Name: flag-atu
         Help: Indicador de Atualizaá∆o

** Field Name: qt-etiquetas
         Help: Quantidade na etiqueta.

** Field Name: despesas-me
         Help: Informe o Valor das Despesas na Moeda Estrangeira

** Field Name: quant-conf
         Help: Quantidade na nossa unidade de medida

** Field Name: base-pis-subs
         Help: Base PIS Substituto

** Field Name: base-cofins-subs
         Help: Base COFINS Substituto

** Field Name: vl-pis-subs
         Help: Valor PIS Substituto

** Field Name: vl-cofins-subs
         Help: Valor COFINS Substituto

** Field Name: log-fifo-oc
         Help: FIFO Ordem de Compra

** Field Name: cod-emit-terc
         Help: Emitente de Entrega das Mercadorias

** Field Name: nro-docto-terc
         Help: N£mero da Nota Fiscal de Entrega das Mercadorias

** Field Name: serie-terc
         Help: SÇrie da Nota Fiscal de Entrega das Mercadorias

** Field Name: nat-terc
         Help: Natureza de Operaá∆o da Nota Fiscal de Entrega das
               Mercadorias

** Field Name: seq-terc
         Help: SeqÅància do Item na Nota Fiscal de Entrega das Mercadorias

** Field Name: nr-ato-concessorio
         Help: Informar o Nr. Ato Concess¢rio

** Field Name: valor-frete
         Help: Informe o valor do frete

** Field Name: valor-frete-me
         Help: Informe valor do frete na moeda estrangeira

** Field Name: nr-proc-imp
         Help: N£mero do Processo de Importaá∆o

** Field Name: declaracao-import
         Help: Declaraá∆o de Importaá∆o

** Field Name: num-seq-rma
  Description: SeqÅància da RMA
         Help: SeqÅància da RMA

** Field Name: log-geracao-ncr-ap
  Description: Indica a geraá∆o de nota de crÇdito no contas a pagar
         Help: Indica a geraá∆o de nota de crÇdito no contas a pagar

** Field Name: val-perc-restocagem
  Description: Percentual de restocagem praticado pelo fornecedor
         Help: Percentual de restocagem praticado pelo fornecedor

** Field Name: val-restocagem
  Description: Valor de restocagem praticado pelo fornecedor
         Help: Valor de restocagem praticado pelo fornecedor

** Field Name: cdn-devolucao
  Description: Tipo de devoluá∆o do item da RGA
         Help: Tipo de devoluá∆o do item da RGA

** Field Name: cod-motivo
  Description: C¢digo do motivo de devoluá∆o do item da RGA
         Help: C¢digo do motivo de devoluá∆o do item da RGA

** Field Name: cod-disposicao
  Description: C¢digo de disposiá∆o do item da RGA
         Help: C¢digo de disposiá∆o do item da RGA

** Field Name: cod-despesa
  Description: C¢digo da despesa
         Help: Informe o c¢digo da despesa

** Field Name: fornec-comp
  Description: Fornecedor do Remito da Fatura de Despesas
         Help: Fornecedor do Remito da Fatura de Despesas

** Field Name: val-perc-rep-ipi
  Description: % Reduá∆o de IPI
         Help: Informe o % de Reduá∆o de IPI

** Field Name: val-perc-red-icms
  Description: % Reduá∆o de ICMS
         Help: Informe o % de Reduá∆o de ICMS

** Field Name: val-sdo-alocad-oc
         Help: Valor Saldo Alocado Ordem compra

** Field Name: val-sdo-alocad-terc
         Help: Valor Saldo Alocado Poder Terceiro

** Field Name: num-event-contrat
         Help: N£mero Evento Contrato

** Field Name: num-ret-om
         Help: N£mero Retorno Ordem manut

** Field Name: peso-bruto-item
         Help: Peso Bruto

** Field Name: peso-liquido-item
         Help: Peso liquido do item

** Field Name: cod-estab-compon
  Description: C¢digo do estabelecimento
         Help: C¢digo do estabelecimento

** Field Name: idi-tributac-cofins
  Description: C¢digo de Tributaá∆o COFINS
         Help: C¢digo de Tributaá∆o COFINS

** Field Name: val-aliq-cofins
  Description: Al°quota de COFINS
         Help: Al°quota de COFINS

** Field Name: val-base-calc-cofins
         Help: Valor Base C†lculo COFINS

** Field Name: val-cofins
         Help: Valor da Cofins

** Field Name: val-cofins-fasb
         Help: Valor COFINS FASB

** Field Name: val-cofins-cmi
         Help: Valor COFINS CMI

** Field Name: idi-tributac-pis
  Description: C¢digo de Tributaá∆o PIS
         Help: C¢digo de Tributaá∆o PIS

** Field Name: val-aliq-pis
  Description: Al°quota PIS
         Help: Al°quota PIS

** Field Name: base-pis
         Help: Valor Base C†lculo PIS

** Field Name: valor-pis
         Help: Valor de PIS

** Field Name: valor-pis-fasb
         Help: Valor Programa integracao social Fasb

** Field Name: valor-pis-cmi
         Help: Valor PIS CMI

** Field Name: num-ord-import
         Help: N£mero Ordem Importacao

** Field Name: num-parc-import
         Help: N£mero Parcela Importacao

** Field Name: cdn-impto-retid
  Description: Imposto Retido
         Help: Imposto Retido

** Field Name: val-aliq-impto-retid
  Description: Taxa Imposto Retido
         Help: Taxa Imposto Retido

** Field Name: val-impto-retid
  Description: Valor Imposto Retido
         Help: Valor Imposto Retido

** Field Name: val-impto-retid-me
  Description: Valor Imposto Retido Moeda estrangeira
         Help: Valor Imposto Retido Moeda estrangeira

** Field Name: val-aduana-pis
  Description: Valor Aduaneiro Programa integracao social
         Help: Valor Aduaneiro Programa integracao social

** Field Name: val-despes-aduana-pis
  Description: Valor Despesa Aduaneiras PIS
         Help: Valor Despesa Aduana PIS

** Field Name: val-aliq-impto-import
         Help: Informar a al°quota do Imposto de Importaá∆o

** Field Name: val-aliq-ext-ipi
  Description: Valor Aliquota Externo IPI
         Help: Valor Aliquota Externo IPI

** Field Name: val-aliq-ext-pis
  Description: Al°quota do PIS para o mercado Externo
         Help: Al°quota do PIS para o mercado Externo

** Field Name: val-aliq-ext-cofins
  Description: Al°quota COFINS para o mercado Externo
         Help: Al°quota COFINS para o mercado Externo

** Field Name: val-aliq-ext-icms
  Description: Valor Aliquota Externo ICMS
         Help: Valor Aliquota Externo ICMS

** Field Name: val-aduana-cofins
  Description: Valor Aduaneiro COFINS
         Help: Valor Aduaneiro COFINS

** Field Name: val-despes-aduana-cofins
  Description: Valor Despesa Aduaneiras COFINS
         Help: Valor Despesa Aduana COFINS

** Field Name: val-unit-pis-import
  Description: Valor Unit†rio PIS Importaá∆o
         Help: Valor Unit†rio PIS Importaá∆o

** Field Name: val-unit-vol-pis-import
  Description: Valor Unit†rio Volume PIS Importaá∆o
         Help: Valor Unit†rio Volume PIS Importaá∆o

** Field Name: val-unit-cofins-import
  Description: Valor Unit†rio COFINS Importaá∆o
         Help: Valor Unit†rio COFINS Importaá∆o

** Field Name: val-unit-vol-cofins-import
  Description: Valor Unit†rio Volume COFINS Importaá∆o
         Help: Valor Unit†rio Volume COFINS Importaá∆o

** Field Name: val-reduc-cofins-import
  Description: Valor Reduá∆o COFINS Importaá∆o
         Help: Valor Reduá∆o COFINS Importaá∆o

** Field Name: val-reduc-pis-import
  Description: Valor Reduá∆o PIS Importaá∆o
         Help: Valor Reduá∆o PIS Importaá∆o

** Field Name: cod-unid-negoc
         Help: C¢digo Unidade Neg¢cio

** Field Name: log-usa-unid-negoc-fifo
         Help: Usa Unidade Negocio FIFO

** Field Name: id-bem
  Description: Identificador do Bem
         Help: Identificador do Bem

** Field Name: log-gerou-ncredito
  Description: Gerou Nota de CrÇdito
         Help: Gerou Nota de CrÇdito

** Field Name: cod-parc-devol
         Help: Codigo Parcela Devolucao

** Field Name: num-sit-trib-icms
         Help: C¢digo Situaá∆o Tribut†ria ICMS

** Field Name: val-base-st-antec
         Help: Valor Base St Antecipado

** Field Name: val-st-antec
         Help: Valor St Antecipado

** Field Name: cod-lote-fabrican
  Description: C¢digo Lote Fabricante
         Help: C¢digo Lote Fabricante

** Field Name: dat-valid-lote-fabrican
         Help: Data Validade Lote Fabricante

** Field Name: cod-barras
         Help: C¢digo Barras

** Field Name: dat-fabricc-lote
         Help: Data de fabricaá∆o

** Field Name: nom-fabrican
         Help: Nome Fabricante


*/
