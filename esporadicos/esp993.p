DEFINE VARIABLE cDescConta          AS CHARACTER   NO-UNDO FORMAT 'X(400)'.
DEFINE VARIABLE cDescContaSaldo     AS CHARACTER   NO-UNDO FORMAT 'X(400)'.
DEFINE VARIABLE i                   AS INTEGER     NO-UNDO.
DEFINE VARIABLE cContaCorrente      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDescContaCorrente  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE tipomov             AS CHARACTER   NO-UNDO.

OUTPUT TO c:\temp\movto-estoq.txt.
FOR EACH movto-estoq NO-LOCK
    WHERE movto-estoq.dt-trans >= 11.01.2014
    AND   movto-estoq.dt-trans <= 11.30.2014 
    USE-INDEX data-conta :
    FIND FIRST ITEM OF movto-estoq NO-LOCK NO-ERROR.
    FIND FIRST grup-estoque OF ITEM NO-LOCK NO-ERROR.
    RUN buscarDescricaoConta(movto-estoq.ct-codigo, OUTPUT cDescConta).    
    RUN buscarDescricaoConta(movto-estoq.ct-saldo, OUTPUT cDescContaSaldo).

    REPEAT i = 1 TO  2:
        IF i = 1 THEN DO:
            IF tipo-trans = 1  THEN
               ASSIGN cContaCorrente     = movto-estoq.ct-saldo
                      cDescContaCorrente = cDescContaSaldo
                      tipomov            = 'debito'.
           ELSE
               ASSIGN cContaCorrente      = movto-estoq.ct-codigo
                      cDescContaCorrente  = cDescConta
                      tipomov             = 'credito'.
        END.
        ELSE DO:
          IF tipo-trans = 2  THEN
               ASSIGN cContaCorrente     = movto-estoq.ct-saldo
                      cDescContaCorrente = cDescContaSaldo
                      tipomov            = 'debito'.
           ELSE
               ASSIGN cContaCorrente      = movto-estoq.ct-codigo
                      cDescContaCorrente  = cDescConta
                      tipomov             = 'credito'.
        END.

        
        EXPORT DELIMITER "|" 
        tipomov
        movto-estoq.cod-estabel 
        movto-estoq.esp-docto 
        movto-estoq.it-codigo 
        IF movto-estoq.tipo-trans = 1 THEN "entrada" ELSE "saida"
        cContaCorrente
        cDescContaCorrente 
        movto-estoq.dt-trans 
        movto-estoq.nro-docto
        movto-estoq.cod-emitente 
        movto-estoq.valor-mat-m[1]
        movto-estoq.refer-contab  
        item.ge-codigo 
        grup-estoque.descricao. 

    END.
END.
OUTPUT  CLOSE.


PROCEDURE buscarDescricaoConta:
    DEFINE INPUT  PARAMETER pConta     AS CHARACTER   NO-UNDO FORMAT 'x(20)'.
    DEFINE OUTPUT PARAMETER cDescricao AS CHARACTER   NO-UNDO FORMAT 'x(200)'.

    FIND FIRST cta_ctbl
        WHERE cod_cta_Ctbl = pConta NO-LOCK NO-ERROR.
    IF AVAIL cta_ctbl THEN
       ASSIGN cDescricao = cta_ctbl.des_tit_ctbl.


END.
/*

=========================================================================
============================= Table: movto-estoq ========================

              Table Flags: "f" = frozen, "s" = a SQL table


Table                            Table Field Index Table
Name                             Flags Count Count Label
-------------------------------- ----- ----- ----- ------------------------
movto-estoq                              100    21 Movimento de Estoque

    Dump Name: in218
  Description: Movimento de Estoque
 Storage Area: Schema Area

       Trigger Event Trigger Procedure    Overridable? Check CRC?
       ------------- -------------------- ------------ ----------
       Create        database/tgin/tcp/tc yes          no
       Delete        database/tgin/tdp/td yes          no
       Write         database/tgin/twp/tw yes          no


============================= FIELD SUMMARY =============================
============================= Table: movto-estoq ========================

Flags: <c>ase sensitive, <i>ndex component, <m>andatory, <v>iew component

Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 cod-estabel                      char        im
   20 cod-depos                        char        im
   30 it-codigo                        char        im
   41 lote                             char        i
   42 tipo-trans                       inte        m
   61 ct-codigo                        char        m
   71 sc-codigo                        char        m
   73 dt-trans                         date        im
   79 esp-docto                        inte        im
   80 num-sequen                       inte        im
   95 serie-docto                      char        im
  110 quantidade                       deci-4      m
  120 un                               char        m
  170 numero-ordem                     inte
  260 referencia                       char
  270 nro-docto                        char        im
  280 cod-emitente                     inte        i
  320 peso-liquido                     deci-5
  330 valor-nota                       deci-2      m
  370 valor-icm                        deci-2      m
  380 valor-ipi                        deci-2      m
  430 nat-operacao                     char        im
  450 descricao-db                     char
  500 tipo-valor                       inte        m
  520 valor-iss                        deci-2      m
  590 vl-icm-fasb                      deci-2[2]   m
  600 vl-ipi-fasb                      deci-2[2]   m
  610 vl-iss-fasb                      deci-2[2]   m
  630 vl-nota-fasb                     deci-2[2]   m
  640 tipo-preco                       inte[3]     m
  650 cod-refer                        char        im
  710 dt-nf-saida                      date
  720 op-seq                           inte        im
  730 usuario                          char
  740 nr-trans                         inte        im
  750 cod-estabel-des                  char        m
  790 sequen-nf                        inte        m
  800 origem-valor                     char
  810 num-ord-des                      inte        im
  820 num-seq-des                      inte        i
  830 num-ord-inv                      inte        i
  860 nr-ord-refer                     inte        i
  870 nr-req-sum                       inte
  880 cod-roteiro                      char
  890 nr-reporte                       inte        i
  900 item-pai                         char
  910 op-codigo                        inte
  940 cod-localiz                      char        i
  950 cod-usu-ult-alter                char
  960 hr-trans                         char
  970 conta-contabil                   char        i
  980 nr-trans-deb                     inte
  990 cod-orig-trans                   char
 1000 ct-saldo                         char
 1010 sc-saldo                         char
 1020 conta-saldo                      char        i
 1030 valor-mat-m                      deci-4[3]
 1040 valor-mob-m                      deci-4[3]
 1050 valor-ggf-m                      deci-4[3]
 1060 valor-mat-o                      deci-4[3]
 1070 valor-mob-o                      deci-4[3]
 1080 valor-ggf-o                      deci-4[3]
 1090 valor-mat-p                      deci-4[3]
 1100 valor-mob-p                      deci-4[3]
 1110 valor-ggf-p                      deci-4[3]
 1120 cod-prog-orig                    char
 1130 refer-contab                     char
 1140 dt-contab                        date
 1150 hr-contab                        char
 1160 contabilizado                    logi        i
 1170 nr-ord-produ                     inte        i
 1180 vl-taxa                          deci-2
 1190 char-1                           char
 1200 char-2                           char
 1210 dec-1                            deci-8
 1220 dec-2                            deci-8
 1230 int-1                            inte
 1240 int-2                            inte
 1250 log-1                            logi
 1260 log-2                            logi
 1270 data-1                           date
 1280 data-2                           date
 1290 check-sum                        char
 1300 dt-criacao                       date
 1310 base-calculo                     inte
 1320 saldo-req                        deci-4      m
 1330 per-ppm                          deci-4
 1340 val-cofins                       deci-2      m
 1350 val-cofins-fasb                  deci-2      m
 1360 val-cofins-cmi                   deci-2      m
 1370 valor-pis                        deci-2      m
 1380 vl-pis-fasb                      deci-2      m
 1390 vl-pis-cmi                       deci-2      m
 1400 cod-unid-negoc                   char        i
 1410 cod-unid-negoc-sdo               char        i
 1420 cod-barras                       char
 1430 cod-lote-fabrican                char
 1440 dat-valid-lote-fabrican          date
 1450 dat-fabricc-lote                 date
 1460 nom-fabrican                     char

Field Name                       Format
-------------------------------- -----------------------------
cod-estabel                      x(5)
cod-depos                        x(3)
it-codigo                        x(16)
lote                             x(40)
tipo-trans                       >9
ct-codigo                        x(20)
sc-codigo                        x(20)
dt-trans                         99/99/9999
esp-docto                        >9
num-sequen                       >>>,>>9
serie-docto                      x(5)
quantidade                       ->>>>,>>>,>>9.9999
un                               xx
numero-ordem                     >>>,>>>,>>9
referencia                       x(40)
nro-docto                        x(16)
cod-emitente                     >>>>>>>>9
peso-liquido                     >>>,>>9.99999
valor-nota                       >>>>,>>>,>>>,>>9.99
valor-icm                        >>>>,>>>,>>9.99
valor-ipi                        >>>>,>>>,>>9.99
nat-operacao                     x(06)
descricao-db                     x(2000)
tipo-valor                       >9
valor-iss                        >>>>,>>>,>>9.99
vl-icm-fasb                      >>>>>,>>>,>>9.99
vl-ipi-fasb                      >>>>>,>>>,>>9.99
vl-iss-fasb                      >>>>>,>>>,>>9.99
vl-nota-fasb                     >>>>>,>>>,>>9.99
tipo-preco                       >9
cod-refer                        x(8)
dt-nf-saida                      99/99/9999
op-seq                           >>>9
usuario                          x(12)
nr-trans                         ->>>,>>>,>>9
cod-estabel-des                  x(5)
sequen-nf                        >>>>9
origem-valor                     x(2)
num-ord-des                      >>>,>>>,>>9
num-seq-des                      >>>,>>9
num-ord-inv                      >>>,>>9
nr-ord-refer                     >>>,>>>,>>9
nr-req-sum                       >>>,>>>,>>9
cod-roteiro                      x(16)
nr-reporte                       >>>>>>>>9
item-pai                         x(16)
op-codigo                        >>>>9
cod-localiz                      x(20)
cod-usu-ult-alter                x(12)
hr-trans                         x(8)
conta-contabil                   x(20)
nr-trans-deb                     >>>>>>>>9
cod-orig-trans                   x(10)
ct-saldo                         x(20)
sc-saldo                         x(20)
conta-saldo                      x(20)
valor-mat-m                      >>>>,>>>,>>9.9999
valor-mob-m                      >>>>,>>>,>>9.9999
valor-ggf-m                      >>>>,>>>,>>9.9999
valor-mat-o                      >>>>,>>>,>>9.9999
valor-mob-o                      >>>>,>>>,>>9.9999
valor-ggf-o                      >>>>,>>>,>>9.9999
valor-mat-p                      >>>>,>>>,>>9.9999
valor-mob-p                      >>>>,>>>,>>9.9999
valor-ggf-p                      >>>>,>>>,>>9.9999
cod-prog-orig                    x(10)
refer-contab                     x(10)
dt-contab                        99/99/9999
hr-contab                        x(8)
contabilizado                    Sim/N’o
nr-ord-produ                     >>>,>>>,>>9
vl-taxa                          >>>,>>>,>>>,>>9.99
char-1                           x(100)
char-2                           x(100)
dec-1                            ->>>>>>>>>>>9.99999999
dec-2                            ->>>>>>>>>>>9.99999999
int-1                            ->>>>>>>>>9
int-2                            ->>>>>>>>>9
log-1                            Sim/N’o
log-2                            Sim/N’o
data-1                           99/99/9999
data-2                           99/99/9999
check-sum                        x(20)
dt-criacao                       99/99/9999
base-calculo                     >9
saldo-req                        ->>>>,>>>,>>9.9999
per-ppm                          >>>>,>>9.9999
val-cofins                       >>>>,>>>,>>9.99
val-cofins-fasb                  >>>,>>>,>>>,>>9.99
val-cofins-cmi                   >>>,>>>,>>>,>>9.99
valor-pis                        >>>,>>>,>>>,>>9.99
vl-pis-fasb                      >>>,>>>,>>>,>>9.99
vl-pis-cmi                       >>>,>>>,>>>,>>9.99
cod-unid-negoc                   X(3)
cod-unid-negoc-sdo               x(3)
cod-barras                       x(50)
cod-lote-fabrican                x(40)
dat-valid-lote-fabrican          99/99/9999
dat-fabricc-lote                 99/99/9999
nom-fabrican                     x(60)

Field Name                       Initial
-------------------------------- -----------------------------
cod-estabel
cod-depos
it-codigo
lote
tipo-trans                       1
ct-codigo
sc-codigo
dt-trans                         today
esp-docto                        1
num-sequen                       0
serie-docto
quantidade                       0
un
numero-ordem                     0
referencia
nro-docto
cod-emitente                     0
peso-liquido                     0
valor-nota                       0
valor-icm                        0
valor-ipi                        0
nat-operacao
descricao-db
tipo-valor                       1
valor-iss                        0
vl-icm-fasb                      0
vl-ipi-fasb                      0
vl-iss-fasb                      0
vl-nota-fasb                     0
tipo-preco                       0
cod-refer
dt-nf-saida                      ?
op-seq                           0
usuario
nr-trans                         0
cod-estabel-des
sequen-nf                        0
origem-valor
num-ord-des                      0
num-seq-des                      0
num-ord-inv                      0
nr-ord-refer                     0
nr-req-sum                       0
cod-roteiro
nr-reporte                       0
item-pai
op-codigo                        0
cod-localiz
cod-usu-ult-alter
hr-trans
conta-contabil
nr-trans-deb                     0
cod-orig-trans
ct-saldo
sc-saldo
conta-saldo
valor-mat-m                      0
valor-mob-m                      0
valor-ggf-m                      0
valor-mat-o                      0
valor-mob-o                      0
valor-ggf-o                      0
valor-mat-p                      0
valor-mob-p                      0
valor-ggf-p                      0
cod-prog-orig
refer-contab
dt-contab                        TODAY
hr-contab
contabilizado                    no
nr-ord-produ                     0
vl-taxa                          0
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
dt-criacao                       today
base-calculo                     1
saldo-req                        0
per-ppm                          0
val-cofins                       0
val-cofins-fasb                  0
val-cofins-cmi                   0
valor-pis                        0
vl-pis-fasb                      0
vl-pis-cmi                       0
cod-unid-negoc
cod-unid-negoc-sdo
cod-barras
cod-lote-fabrican
dat-valid-lote-fabrican          ?
dat-fabricc-lote                 ?
nom-fabrican

Field Name                     Label                  Column Label
------------------------------ ---------------------- ----------------------
cod-estabel                    Estabel                Est
cod-depos                      Dep½sito               Dep
it-codigo                      Item                   Item
lote                           Lote/S²rie             Lote/S²rie
tipo-trans                     Tipo Transa»’o         Tp
ct-codigo                      Conta                  Cta
sc-codigo                      Sub-Conta              SC
dt-trans                       Data Transa»’o         Transa»’o
esp-docto                      Esp²cie Documento      Esp Docto
num-sequen                     Seq±¼ncia              Seq
serie-docto                    S²rie Documento        Ser
quantidade                     Qtde                   Qtde
un                             Unid Medid             Un
numero-ordem                   Ordem                  Ordem
referencia                     Referencia             Referencia
nro-docto                      Documento              Documento
cod-emitente                   Fornecedor             Fornec
peso-liquido                   Peso Liq               Peso Liq
valor-nota                     Valor Nota             Vlr da Nota
valor-icm                      ICMS Recup             ICMS Recuperavel
valor-ipi                      IPI Recup              IPI Recuperavel
nat-operacao                   Nat Opera»’o           Nat Oper
descricao-db                   Descri»’o D²bito Diret Descri»’o D²bito Diret
tipo-valor                     Tipo Valor             Tp Val
valor-iss                      ISS Recup              ISS Recuperavel
vl-icm-fasb                    ICMS Recup             ICMS Recup
vl-ipi-fasb                    IPI Recup              IPI Recup
vl-iss-fasb                    ISS Recup              ISS Recup
vl-nota-fasb                   Valor Nota             Vlr Nota
tipo-preco                     Tipo Pre»o             Tp Pre»o
cod-refer                      Refer¼ncia             Ref
dt-nf-saida                    Data NF Sa­da          NF Sa­da
op-seq                         Op Sequencia           Op Seq
usuario                        Usuar                  Usuario
nr-trans                       Trans                  Trans
cod-estabel-des                Estabelecimento Destin Est Dest
sequen-nf                      Seq                    Seq
origem-valor                   Origem                 Origem
num-ord-des                    Ordem Destino          OP Dest
num-seq-des                    Num Seq Destino        Num Seq Destino
num-ord-inv                    Ordem Invest           Ordem Inv
nr-ord-refer                   Ordem Refer¼ncia       Ord Refer
nr-req-sum                     Requisi»’o Sumariada   Req Sum
cod-roteiro                    Roteiro                Roteiro
nr-reporte                     Reporte                Reporte
item-pai                       Item Pai               Item Pai
op-codigo                      Opera»’o               Oper
cod-localiz                    Localiza»’o            Localiz
cod-usu-ult-alter              Usuÿrio                Usuÿrio
hr-trans                       Hora                   Hora
conta-contabil                 Conta Contÿbil         Conta Contÿbil
nr-trans-deb                   Trans Deb              Trans Deb
cod-orig-trans                 Origem Trans           Origem
ct-saldo                       Conta Saldo            Conta Saldo
sc-saldo                       Sub Conta Saldo        Sub Conta Saldo
conta-saldo                    Cta Saldo              Saldo
valor-mat-m                    Valor Mat Batch        Mat Batch
valor-mob-m                    Valor Mob Batch        Mob Batch
valor-ggf-m                    Valor GGF Batch        GGF Batch
valor-mat-o                    Valor Mat On-line      Mat On-line
valor-mob-o                    Valor Mob On-line      Mob On-line
valor-ggf-o                    Valor GGF On-line      GGF On-line
valor-mat-p                    Valor Mat Padr’o       Mat Padr’o
valor-mob-p                    Valor Mob Padr’o       Mob Padr’o
valor-ggf-p                    Valor GGF Padr’o       GGF Padr’o
cod-prog-orig                  Cod Prog Orig          Cod Prog Orig
refer-contab                   Refer¼ncia Contÿbil    Refer Contÿbil
dt-contab                      Data Contabiliza»’o    Dt Contab
hr-contab                      Hora Contabiliza»’o    Hr Contab
contabilizado                  Movimento Contabilizad Contab
nr-ord-produ                   Ordem Produ»’o         Ord Prod
vl-taxa                        Valor Imposto          Vl Imposto
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
dt-criacao                     Data Cria»’o           Dt Cria»’o
base-calculo                   Base Cÿlculo M²dio     Base Calc
saldo-req                      Saldo Requisi»’o       Sld Req
per-ppm                        ?                      ?
val-cofins                     Valor COFINS           ?
val-cofins-fasb                Vl COFINS FASB         ?
val-cofins-cmi                 Vl COFINS CMI          ?
valor-pis                      Valor PIS              ?
vl-pis-fasb                    Valor PIS FASB         ?
vl-pis-cmi                     Vl PIS CMI             Vl PIS CMI
cod-unid-negoc                 Unidade Neg½cio        Unid Negoc
cod-unid-negoc-sdo             Unid Neg½cio Saldo     Unid Neg Saldo
cod-barras                     Barras                 Barras
cod-lote-fabrican              Lote Fabricante        Lote Fabricante
dat-valid-lote-fabrican        Validade Lote Fabrican Validade Lote Fabric
dat-fabricc-lote               Data Fabrica»’o        Dt Fabric
nom-fabrican                   Fabricante             Fabricante


============================= INDEX SUMMARY =============================
============================= Table: movto-estoq ========================

Flags: <p>rimary, <u>nique, <w>ord, <a>bbreviated, <i>nactive, + asc, - desc

Flags Index Name                       Cnt Field Name
----- -------------------------------- --- ---------------------------------
      contab-trans                       3 + contabilizado
                                           + cod-estabel
                                           + dt-trans

      data-conta                         2 + dt-trans
                                           + conta-contabil

      data-item                          2 + dt-trans
                                           + it-codigo

      data-saldo                         2 + dt-trans
                                           + conta-saldo

      dep-estab                          6 + cod-depos
                                           + cod-estabel
                                           - dt-trans
                                           + it-codigo
                                           + lote
                                           + cod-refer

      documento                          4 + serie-docto
                                           + nro-docto
                                           + cod-emitente
                                           + nat-operacao

      esp-data                           3 + esp-docto
                                           + dt-trans
                                           + it-codigo

      estab-dep                          3 + cod-estabel
                                           + dt-trans
                                           + cod-depos

p     item-data                          3 + it-codigo
                                           + cod-estabel
                                           + dt-trans

      item-est-dep                       5 + it-codigo
                                           + cod-estabel
                                           + cod-depos
                                           + lote
                                           + cod-localiz

      item-estab                         6 + it-codigo
                                           + cod-refer
                                           + cod-estabel
                                           + cod-depos
                                           + lote
                                           + cod-localiz

      mvtstq-18                          4 + num-ord-inv
                                           + esp-docto
                                           + serie-docto
                                           + nro-docto

      mvtstq-19                          2 + cod-unid-negoc
                                           + dt-trans

      mvtstq-20                          2 + cod-unid-negoc-sdo
                                           + dt-trans

      nr-ord-refer                       1 + nr-ord-refer

      nr-reporte                         1 + nr-reporte

u     nr-trans                           1 + nr-trans

      operacao                           2 + nr-ord-produ
                                           + op-seq

      ord-des                            2 + num-ord-des
                                           + num-seq-des

      ord-seq                            2 + nr-ord-produ
                                           + num-sequen

      reporte                            3 + nr-reporte
                                           + esp-docto
                                           + it-codigo

** Index Name: contab-trans
 Storage Area: Schema Area
** Index Name: data-conta
 Storage Area: Schema Area
** Index Name: data-item
 Storage Area: Schema Area
** Index Name: data-saldo
 Storage Area: Schema Area
** Index Name: dep-estab
 Storage Area: Schema Area
** Index Name: documento
 Storage Area: Schema Area
** Index Name: esp-data
 Storage Area: Schema Area
** Index Name: estab-dep
 Storage Area: Schema Area
** Index Name: item-data
 Storage Area: Schema Area
** Index Name: item-est-dep
 Storage Area: Schema Area
** Index Name: item-estab
 Storage Area: Schema Area
** Index Name: mvtstq-18
 Storage Area: Schema Area
** Index Name: mvtstq-19
 Storage Area: Schema Area
** Index Name: mvtstq-20
 Storage Area: Schema Area
** Index Name: nr-ord-refer
 Storage Area: Schema Area
** Index Name: nr-reporte
 Storage Area: Schema Area
** Index Name: nr-trans
 Storage Area: Schema Area
** Index Name: operacao
 Storage Area: Schema Area
** Index Name: ord-des
 Storage Area: Schema Area
** Index Name: ord-seq
 Storage Area: Schema Area
** Index Name: reporte
 Storage Area: Schema Area


============================= FIELD DETAILS =============================
============================= Table: movto-estoq ========================

** Field Name: cod-estabel
         Help: C½digo do Estabelecimento
      Val-Msg: Estabelecimento inexistente.
      Val-Exp: can-find(estabelec where estabelec.cod-estabel =


               movto-estoq.cod-estabel)

** Field Name: cod-depos
      Val-Msg: Dep½sito inexistente.
      Val-Exp: can-find(deposito of movto-estoq)

** Field Name: it-codigo
      Val-Msg: Item n’o cadastrado
      Val-Exp: can-find(item where item.it-codigo=movto-estoq.it-codigo)

** Field Name: tipo-trans
         Help: Tipo de Transa»’o do Estoque

** Field Name: sc-codigo
         Help: Sub-Conta da Conta Contÿbil

** Field Name: esp-docto
         Help: Esp²cie do Documento

** Field Name: un
      Val-Msg: Unidade de Medida n’o cadastrada
      Val-Exp: can-find(tab-unidade of movto-estoq)

** Field Name: nro-docto
         Help: Nœmero do Documento

** Field Name: cod-emitente
      Val-Msg: Fornecedor inexistente
      Val-Exp: movto-estoq.cod-emitente = 0 or can-find (emitente where
               emitente.cod-emitente = movto-estoq.cod-emitente)

** Field Name: nat-operacao
      Val-Msg: Natureza de Opera»’o n’o cadastrada
      Val-Exp: can-find(natur-oper of movto-estoq)

** Field Name: tipo-valor
         Help: Tipo de Valor

** Field Name: tipo-preco
  Description: Tipo de preco para cada moeda

** Field Name: cod-refer
  Description: Informe o codigo da Referencia - prod acabado controlado
               por referencia

** Field Name: op-seq
  Description: Numero Sequencial da Operacao da Ordem
         Help: Numero Sequencial da Operacao da Ordem

** Field Name: cod-estabel-des
  Description: Estabelecimento de Destino
      Val-Msg: Estabelecimento Destino inexistente.
      Val-Exp: can-find(estabel where estabel.cod-estabel =
               movto-estoq.cod-estabel-des)

** Field Name: num-ord-des
         Help: Numero da Ordem de Produ»’o Destino

** Field Name: num-ord-inv
         Help: Numero da Ordem de Investimento (Numero Magnus)

** Field Name: nr-ord-refer
  Description: Nœmero da ordem de refer¼ncia
         Help: Nœmero da ordem de refer¼ncia

** Field Name: nr-req-sum
         Help: Nœmero da Requisi»’o Sumariada

** Field Name: cod-roteiro
  Description: C½digo do roteiro

** Field Name: nr-reporte
  Description: Nœmero do Reporte
         Help: Nœmero do Reporte

** Field Name: item-pai
  Description: Item pai do item desta reserva na estrutura.
         Help: Item pai do item desta reserva na estrutura

** Field Name: op-codigo
         Help: C½digo da opera»’o

** Field Name: cod-localiz
  Description: C½digo da localiza»’o do material

** Field Name: cod-usu-ult-alter
  Description: C½digo do usuÿrio responsÿvel pela altera»’o

** Field Name: hr-trans
  Description: Hora de efetiva»’o da transa»’o
         Help: Informe a hora da transa»’o

** Field Name: nr-trans-deb
  Description: Armazena o atributo Nr-trans (Sequencia do registro) de
               contrapartida da transacao (transacao de debito)
         Help: Informe a seq±¼ncia de contrapartida

** Field Name: cod-orig-trans
  Description: Atributo utilizado para identificar a origem da transa»’o,
               isto ² , qual programa deu origem a traansa»’o
         Help: Identificador de Origem da Transa»’o

** Field Name: ct-saldo
  Description: Conta de saldo
         Help: Conta de saldo

** Field Name: sc-saldo
         Help: Sub Conta de Saldo

** Field Name: conta-saldo
         Help: Conta saldo

** Field Name: valor-mat-m
         Help: Valor Material Batch

** Field Name: valor-mob-m
         Help: Valor M’o-de-obra Batch

** Field Name: valor-ggf-m
         Help: Valor do GGF Batch

** Field Name: valor-mat-o
         Help: Valor Material On-line

** Field Name: valor-mob-o
         Help: Valor M’o-de-obra On-line

** Field Name: valor-ggf-o
         Help: Valor do GGF On-line

** Field Name: valor-mat-p
         Help: Valor Material Padr’o

** Field Name: valor-mob-p
         Help: Valor M’o-de-obra Padr’o

** Field Name: valor-ggf-p
         Help: Valor do GGF Padr’o

** Field Name: cod-prog-orig
  Description: C½digo do programa de origem que gerou a transa»’o
         Help: Programa de Origem

** Field Name: refer-contab
  Description: Refer¼ncia contÿbil usada na contabiliza»’o deste movimento
         Help: Refer¼ncia contÿbil usada na contabiliza»’o deste movimento

** Field Name: dt-contab
         Help: Data em que o movimento foi contabilizado

** Field Name: hr-contab
         Help: Hora em que o movimento foi contabilizado

** Field Name: contabilizado
         Help: Movimento contabilizado ?

** Field Name: nr-ord-produ
         Help: Nœmero da Ordem de Produ»’o

** Field Name: vl-taxa
         Help: Valor do Imposto

** Field Name: dt-criacao
         Help: Data da cria»’o do movimento de estoque

** Field Name: base-calculo
         Help: Indica se o movimento faz parte da base do m²dio

** Field Name: saldo-req
  Description: Mant²m o saldo das requisi»„es (req) para efeitos de
               devolu»’o.  o Saldo da requisi»’o conterÿ em principio o
               total da requisi»’o efetuada. Quando ocorrer uma devolu»’o
               de material, esta devolu»’o deverÿ se de, no mÿximo, o
               total do saldo da requisi»’o.
         Help: Saldo da requisi»’o.

** Field Name: val-cofins
         Help: Valor do Cofins

** Field Name: val-cofins-fasb
         Help: Valor COFINS na moeda FASB

** Field Name: val-cofins-cmi
         Help: Valor COFINS na moeda CMI

** Field Name: valor-pis
         Help: Valor de PIS

** Field Name: vl-pis-fasb
         Help: Valor de PIS na moeda FASB

** Field Name: vl-pis-cmi
         Help: Valor PIS na moeda CMI

** Field Name: cod-unid-negoc
         Help: C½digo Unidade Neg½cio

** Field Name: cod-unid-negoc-sdo
         Help: C½digo Unidade Neg½cio Saldo

** Field Name: cod-barras
         Help: C½digo Barras

** Field Name: cod-lote-fabrican
  Description: C½digo Lote Fabricante
         Help: C½digo Lote Fabricante

** Field Name: dat-valid-lote-fabrican
         Help: Data Validade Lote Fabricante

** Field Name: dat-fabricc-lote
         Help: Data de fabrica»’o

** Field Name: nom-fabrican
         Help: Nome Fabricante


*/
