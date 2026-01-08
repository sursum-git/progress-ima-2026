DEFINE VARIABLE iPedido    AS INTEGER     NO-UNDO.
DEFINE VARIABLE iContainer AS INTEGER     NO-UNDO.
DEFINE VARIABLE iParcela   AS INTEGER     NO-UNDO INIT 1.
DEFINE VARIABLE deTotalQT  AS DECIMAL     NO-UNDO INIT 0.
DEFINE BUFFER bfPrazoCompra FOR prazo-compra.
DEFINE BUFFER bfOrdemCOmpra FOR ordem-compra.
UPDATE iPedido iContainer .
FIND FIRST pedido-compr WHERE num-pedido = iPedido NO-LOCK.
OUTPUT  TO  c:\temp\LOG.txt.
PUT "Pedido de compra encontrado?"  AVAIL pedido-compr SKIP.

FOR EACH pp-container NO-LOCK
    WHERE pp-container.nr-container = iContainer:    
    PUT "container:" pp-container.nr-container SKIP.
    FOR EACH pp-it-container OF pp-container NO-LOCK:
        PUT "item container:" pp-it-container.it-comprado " ref.:" pp-it-container.ref-comprada SKIP.
        FIND FIRST ordem-compra
            WHERE ordem-compra.num-pedido = pedido-compr.num-pedido
            AND   ordem-compra.it-codigo = pp-it-container.it-comprado NO-LOCK NO-ERROR.
        IF AVAIL ordem-compra THEN DO:
           FIND FIRST ITEM OF ordem-compra NO-LOCK NO-ERROR.
           PUT "encontrada ordem de compra:" ordem-compra.numero-ordem SKIP.
           FIND FIRST prazo-compra OF ordem-compra
               WHERE prazo-compra.cod-refer = pp-it-container.ref-comprada EXCLUSIVE-LOCK NO-ERROR .
           IF NOT AVAIL prazo-compra THEN DO:
              PUT "vou criar o prazo de compra para a referencia:" pp-it-container.ref-comprada SKIP.
             /* FIND LAST bfPrazoCompra OF ordem-compra NO-LOCK NO-ERROR.
             IF AVAIL bfPrazoCompra THEN DO:
                 ASSIGN iParcela = bfPrazoCompra.parcela + 1 .
              END.
              ELSE*/ 

              ASSIGN iParcela = iParcela + 1.
              CREATE prazo-compra.
              ASSIGN prazo-compra.numero-ordem = ordem-compra.numero-ordem
                     prazo-compra.it-codigo = ordem-compra.it-codigo
                     prazo-compra.cod-refer = pp-it-container.ref-comprada
                     prazo-compra.parcela   = iParcela
                     prazo-compra.data-entrega = TODAY
                     prazo-compra.data-alter    = TODAY
                     prazo-compra.quantidade    = pp-it-container.qt-pedida
                     prazo-compra.quantid-orig  = pp-it-container.qt-pedida
                     prazo-compra.quant-saldo   = pp-it-container.qt-pedida
                     prazo-compra.qtd-sal-forn   = pp-it-container.qt-pedida
                     prazo-compra.sequencia     = ordem-compra.sequencia   
                     prazo-compra.situacao      = 2
                     prazo-compra.quant-a-ped   = pp-it-container.qt-pedida
                     prazo-compra.natureza      = 1
                     prazo-compra.cod-alter     = TRUE
                     prazo-compra.un            = ITEM.un . 

           END.
        END.
    END.
    FOR EACH  bfPrazoCompra
        WHERE bfPrazoCompra.numero-ordem = ordem-compra.numero-ordem NO-LOCK .
        ASSIGN deTotalQT = deTotalQT + bfPrazoCompra.qtd-sal-forn.
    END.
    PUT "total quantidades parcela:" deTotalQT SKIP.
   ASSIGN  ordem-compra.qt-acum-nec = deTotalQt
           ordem-compra.qt-solic    = deTotalQt.
                                  
END.
OUTPUT CLOSE.

/*



=========================================================================
============================= Table: prazo-compra =======================

              Table Flags: "f" = frozen, "s" = a SQL table


Table                            Table Field Index Table
Name                             Flags Count Count Label
-------------------------------- ----- ----- ----- ------------------------
prazo-compra                              58    14 Parcelas da Ordem de Com

    Dump Name: in356
  Description: Prazos de Entrega de Compras
 Storage Area: Schema Area

       Trigger Event Trigger Procedure    Overridable? Check CRC?
       ------------- -------------------- ------------ ----------
       Create        database/tgin/tcp/tc yes          no
       Delete        database/tgin/tdp/td yes          no
       Write         database/tgin/twp/tw yes          no


============================= FIELD SUMMARY =============================
============================= Table: prazo-compra =======================

Flags: <c>ase sensitive, <i>ndex component, <m>andatory, <v>iew component

Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 numero-ordem                     inte        im
   20 parcela                          inte        im
   30 it-codigo                        char        im
   40 un                               char        m
   50 quantid-orig                     deci-4      m
   60 quantidade                       deci-4      m
   70 quant-saldo                      deci-4      im
   80 quant-rejeit                     deci-4      m
   90 quant-receb                      deci-4      m
  100 qtd-do-forn                      deci-4      m
  110 qtd-sal-forn                     deci-4      m
  120 qtd-rej-forn                     deci-4      m
  130 qtd-rec-forn                     deci-4      m
  140 data-orig                        date
  150 data-entrega                     date        im
  160 pedido-clien                     char        im
  170 cod-alter                        logi        m
  180 data-alter                       date
  190 usuario-alt                      char        m
  200 nr-alt-data                      inte        m
  210 nr-alt-quant                     inte        m
  220 nome-abrev                       char        im
  230 situacao                         inte        im
  290 natureza                         inte        m
  300 cod-refer                        char        im
  310 quant-a-ped                      deci-4
  320 qtd-a-ped-forn                   deci-4
  330 nr-contrato                      inte        i
  340 hora                             inte
  350 concentracao                     deci-4      m
  360 rendimento                       deci-4      m
  390 nr-sequencia                     inte        im
  400 cons-mrp                         logi
  410 cons-pmp                         logi
  420 cc-codigo                        char        m
  430 nr-entrega                       inte
  440 MRP                              inte
  450 ordem-emitida                    logi
  460 expectativa                      logi
  470 nr-estrut                        inte
  480 item-cotacao                     char
  490 sequencia                        inte
  500 es-codigo                        char
  510 nr-estrut-filha                  inte
  520 char-1                           char
  530 char-2                           char
  540 dec-1                            deci-8
  550 dec-2                            deci-8
  560 int-1                            inte
  570 int-2                            inte
  580 log-1                            logi
  590 log-2                            logi
  600 data-1                           date
  610 data-2                           date
  620 check-sum                        char
  630 qtd-aloc-forn                    deci-4      m
  640 quant-alocada                    deci-4      m
  650 data-entrega-ant                 date

Field Name                       Format
-------------------------------- -----------------------------
numero-ordem                     zzzzz9,99
parcela                          >>>>9
it-codigo                        X(16)
un                               xx
quantid-orig                     >>>>,>>9.9999
quantidade                       >>>>,>>9.9999
quant-saldo                      ->>>>,>>9.9999
quant-rejeit                     >>>>,>>9.9999
quant-receb                      >>>>,>>9.9999
qtd-do-forn                      >>>>,>>9.9999
qtd-sal-forn                     >>>>,>>9.9999
qtd-rej-forn                     >>>>,>>9.9999
qtd-rec-forn                     >>>>,>>9.9999
data-orig                        99/99/9999
data-entrega                     99/99/9999
pedido-clien                     X(12)
cod-alter                        Sim/N∆o
data-alter                       99/99/9999
usuario-alt                      x(12)
nr-alt-data                      >9
nr-alt-quant                     >9
nome-abrev                       x(12)
situacao                         >9
natureza                         9
cod-refer                        x(8)
quant-a-ped                      >>>>,>>9.9999
qtd-a-ped-forn                   >>>>,>>9.9999
nr-contrato                      >>>>>>>>9
hora                             99
concentracao                     >>9.9999
rendimento                       >>9.9999
nr-sequencia                     >>,>>9
cons-mrp                         Sim/N∆o
cons-pmp                         Sim/N∆o
cc-codigo                        x(8)
nr-entrega                       >>>>9
MRP                              9
ordem-emitida                    Sim/N∆o
expectativa                      Sim/N∆o
nr-estrut                        >>>>>>9
item-cotacao                     x(16)
sequencia                        >>>>9
es-codigo                        x(16)
nr-estrut-filha                  >>>>>>9
char-1                           x(100)
char-2                           x(100)
dec-1                            ->>>>>>>>>>>9.99999999
dec-2                            ->>>>>>>>>>>9.99999999
int-1                            ->>>>>>>>>9
int-2                            ->>>>>>>>>9
log-1                            Sim/N∆o
log-2                            Sim/N∆o
data-1                           99/99/9999
data-2                           99/99/9999
check-sum                        x(20)
qtd-aloc-forn                    >>>>,>>9.9999
quant-alocada                    >>>>,>>9.9999
data-entrega-ant                 99/99/9999

Field Name                       Initial
-------------------------------- -----------------------------
numero-ordem                     0
parcela                          1
it-codigo
un
quantid-orig                     0
quantidade                       0
quant-saldo                      0
quant-rejeit                     0
quant-receb                      0
qtd-do-forn                      0
qtd-sal-forn                     0
qtd-rej-forn                     0
qtd-rec-forn                     0
data-orig                        ?
data-entrega                     today
pedido-clien
cod-alter                        no
data-alter                       today
usuario-alt
nr-alt-data                      0
nr-alt-quant                     0
nome-abrev
situacao                         1
natureza                         1
cod-refer
quant-a-ped                      0
qtd-a-ped-forn                   0
nr-contrato                      0
hora                             0
concentracao                     0
rendimento                       0
nr-sequencia                     0
cons-mrp                         yes
cons-pmp                         yes
cc-codigo
nr-entrega                       0
MRP                              0
ordem-emitida                    no
expectativa                      no
nr-estrut                        1
item-cotacao
sequencia                        0
es-codigo
nr-estrut-filha                  0
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
qtd-aloc-forn                    0
quant-alocada                    0
data-entrega-ant                 ?

Field Name                     Label                  Column Label
------------------------------ ---------------------- ----------------------
numero-ordem                   Ordem                  Ordem
parcela                        Parcela                Parc
it-codigo                      Item                   Item
un                             Unid Medid             Un
quantid-orig                   Qtde Original          Qtde Orig
quantidade                     Qtde                   Qtde
quant-saldo                    Qtde Saldo             Qtde Saldo
quant-rejeit                   Qtde Dev               Qtde Devol
quant-receb                    Qtde Recebida          Qtde Receb
qtd-do-forn                    Qtde Fornec            Qtde Fornec
qtd-sal-forn                   Qtde Saldo Fornec      Qtde Sal Forn
qtd-rej-forn                   Qtde Devol Fornec      Qtde Dev Forn
qtd-rec-forn                   Qtde Receb Fornec      Qtde Rec Forn
data-orig                      Data Entrega Original  Data Ent Orig
data-entrega                   Data Entrega           Entrega
pedido-clien                   Ped Cliente            Ped Cli
cod-alter                      Alteraá∆o              Alt
data-alter                     Data Èltima Alteraá∆o  Ult Alt
usuario-alt                    Usuar Ult Alt          Usu†r Ult alt
nr-alt-data                    Num Alt Data           Alt Data
nr-alt-quant                   Num Alt Quant          Alt Qtd
nome-abrev                     Cliente                Cliente
situacao                       Situaá∆o               Sit
natureza                       Natureza               Nat
cod-refer                      Referància             Ref
quant-a-ped                    Qtde Pedir             Qtde Pedir
qtd-a-ped-forn                 Qtde ∑ Pedir           Qtde Pedir
nr-contrato                    Contrato               Contrato
hora                           Hora Entrega           Hora
concentracao                   Concentraá∆o           Conc
rendimento                     Rendimento             Rendimento
nr-sequencia                   SeqÅància              Seq
cons-mrp                       Considera para MRP     MRP
cons-pmp                       Considera para PMP     PMP
cc-codigo                      Centro Custo           Centro Custo
nr-entrega                     Entrega                Entrega
MRP                            Considera MRP          MRP
ordem-emitida                  Ordem Emitida          Ord Emit
expectativa                    Expectativa            Expect
nr-estrut                      Estrutura              Estrut
item-cotacao                   Item                   Item
sequencia                      Seq                    Seq
es-codigo                      Componente             Componente
nr-estrut-filha                Estrutura Filha        Estr Filha
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
qtd-aloc-forn                  Qtde Alocada Fornec    Qtde Aloc Forn
quant-alocada                  Qtde Alocada           Qtde Alocada
data-entrega-ant               Dt Entrega Ant         Dt Entr Ant


============================= INDEX SUMMARY =============================
============================= Table: prazo-compra =======================

Flags: <p>rimary, <u>nique, <w>ord, <a>bbreviated, <i>nactive, + asc, - desc

Flags Index Name                       Cnt Field Name
----- -------------------------------- --- ---------------------------------
      ch-saldo                           4 + it-codigo
                                           + quant-saldo
                                           + data-entrega
                                           + situacao

      cli-ped                            5 + nome-abrev
                                           + pedido-clien
                                           + nr-sequencia
                                           + it-codigo
                                           + cod-refer

      cliente-item                       2 + pedido-clien
                                           + it-codigo

      cliente-pedido                     5 + nome-abrev
                                           + pedido-clien
                                           + nr-sequencia
                                           + numero-ordem
                                           + parcela

      data                               1 + data-entrega

      entrega-pl                         4 + it-codigo
                                           + data-entrega
                                           + situacao
                                           + quant-saldo

      it-contrato                        2 + it-codigo
                                           + nr-contrato

      item                               4 + it-codigo
                                           + data-entrega
                                           + numero-ordem
                                           + parcela

      item-ordem                         3 + it-codigo
                                           + numero-ordem
                                           + parcela

pu    ordem                              2 + numero-ordem
                                           + parcela

      ordem-sit                          3 + numero-ordem
                                           + data-entrega
                                           + situacao

      planejamento                       3 + it-codigo
                                           + situacao
                                           + data-entrega

      przcmpr_ix14                       1 + numero-ordem

      sit-ordem                          2 + situacao
                                           + numero-ordem

** Index Name: ch-saldo
 Storage Area: Schema Area
** Index Name: cli-ped
 Storage Area: Schema Area
** Index Name: cliente-item
 Storage Area: Schema Area
** Index Name: cliente-pedido
 Storage Area: Schema Area
** Index Name: data
 Storage Area: Schema Area
** Index Name: entrega-pl
 Storage Area: Schema Area
** Index Name: it-contrato
 Storage Area: Schema Area
** Index Name: item
 Storage Area: Schema Area
** Index Name: item-ordem
 Storage Area: Schema Area
** Index Name: ordem
 Storage Area: Schema Area
** Index Name: ordem-sit
 Storage Area: Schema Area
** Index Name: planejamento
 Storage Area: Schema Area
** Index Name: przcmpr_ix14
 Storage Area: Schema Area
** Index Name: sit-ordem
 Storage Area: Schema Area


============================= FIELD DETAILS =============================
============================= Table: prazo-compra =======================

** Field Name: numero-ordem
         Help: N£mero da Ordem do Pedido

** Field Name: parcela
         Help: Parcela da Ordem de Compra
      Val-Msg: Parcela deve ser diferente de zeros
      Val-Exp: parcela > 0

** Field Name: it-codigo
         Help: Informe o C¢digo do Item
      Val-Msg: Item nao cadastrado
      Val-Exp: can-find (item where 

                 item.it-codigo =
               prazo-compra.it-codigo)

** Field Name: un
         Help: Unidade de Medida
      Val-Msg: Unidade de medida deve ser diferente de brancos
      Val-Exp: un <> ""

** Field Name: quantid-orig
         Help: Quantidade Original

** Field Name: quantidade
         Help: Quantidade
      Val-Msg: Quantidade deve ser maior que zero
      Val-Exp: quantidade > 0

** Field Name: quant-saldo
         Help: Quantidade de Saldo
      Val-Msg: Quantidade de saldo deve ser maior ou igual a zero
      Val-Exp: quant-saldo >= 0

** Field Name: quant-rejeit
         Help: Quantidade Devolvida

** Field Name: quant-receb
         Help: Quantidade Recebida

** Field Name: qtd-do-forn
         Help: Quantidade do Fornecedor

** Field Name: qtd-sal-forn
         Help: Quantidade de Saldo do Fornecedor

** Field Name: qtd-rej-forn
         Help: Quantidade Devolvida p/ o Fornecedor

** Field Name: qtd-rec-forn
         Help: Quantidade Recebida do Fornecedor

** Field Name: pedido-clien
         Help: N£mero do Pedido do Cliente

** Field Name: cod-alter
  Description: Sim 
 Nao

** Field Name: usuario-alt
         Help: Usu†rio da Ultima Atualizaá∆o

** Field Name: nome-abrev
         Help: Informe o Nome Abreviado do Cliente

** Field Name: situacao
         Help: Situaá∆o do Pedido de Compra

** Field Name: cod-refer
  Description: Codigo da Referencia do item

** Field Name: quant-a-ped
         Help: Quantidade a pedir

** Field Name: qtd-a-ped-forn
         Help: Quantidade a pedir

** Field Name: nr-contrato
         Help: Informe o N£mero do Contrato

** Field Name: hora
         Help: Informe a Hora p/ Entrega

** Field Name: concentracao
      Val-Msg: Concentraá∆o deve ser maior do que zeros.
      Val-Exp: concentracao > 0

** Field Name: rendimento
         Help: Digite o rendimento
      Val-Msg: Rendimento deve ser maior que zero
      Val-Exp: rendimento > 0

** Field Name: nr-sequencia
  Description: numero sequencial do item para o pedido
         Help: N£mero da seqÅància do item no pedido
      Val-Msg: Numero da SeqÅància Invalido
      Val-Exp: nr-sequencia > 0

** Field Name: nr-entrega
         Help: N£mero de seqÅància da entrega do item do pedido

** Field Name: ordem-emitida
  Description: Identificador de emiss∆o de ordens
         Help: Informe se a ordem foi emitida ou n∆o

** Field Name: expectativa
  Description: Expectativa de
         Help: Informe a expectativa de compra da ordem

** Field Name: nr-estrut
         Help: Numero da estrutura no desenvolvimento de produtos
      Val-Msg: N∆o pode ser Zero
      Val-Exp: nr-estrut > 0

** Field Name: item-cotacao
      Val-Msg: Prot¢tipo n∆o cadastrado
      Val-Exp: can-find (cot-item of cot-estrut)

** Field Name: sequencia
         Help: SeqÅància de montagem do componente (numerar de 100 em 100)

** Field Name: nr-estrut-filha
         Help: N£mero da Estrutura Filha

** Field Name: qtd-aloc-forn
         Help: Quantidade Alocada do Fornecedor

** Field Name: quant-alocada
         Help: Quantidade Alocada
      Val-Msg: Quantidade de saldo deve ser maior ou igual a zero
      Val-Exp: quant-saldo >= 0

** Field Name: data-entrega-ant
  Description: Data Entrega Anterior a Vinculaá∆o
         Help: Data Entrega Anterior a Vinculaá∆o da Parcela ao Embarque


*/
