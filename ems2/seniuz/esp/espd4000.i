DEF TEMP-TABLE tt-itens-ped NO-UNDO LIKE ped-item
    FIELD tp-acao       AS   CHAR
    FIELD qtidade-atu   LIKE saldo-estoq.qtidade-atu
    FIELD qt-dsp-venda  LIKE saldo-estoq.qtidade-atu
    FIELD qt-reserva    LIKE ped-item.qt-pedida
    FIELD nr-nota-fis   LIKE nota-fiscal.nr-nota-fis
    FIELD dt-emis-nf    AS   DATE FORMAT "99/99/9999" 
    FIELD dt-saida-nf   LIKE nota-fiscal.dt-saida
    FIELD retirar-corte AS LOG
    FIELD bloqueio-fat  AS LOG 
    FIELD motivo        AS CHAR
    FIELD cod-estabel   LIKE ped-venda.cod-estabel
    FIELD vl-pre-calc   LIKE ped-item.vl-pretab
    FIELD vl-pre-out    LIKE ped-item.vl-pretab
    FIELD outlet        AS LOG
    FIELD num-id-preco  LIKE ped-item-ext.num-id-liquida-ima
    INDEX indice-1 nr-sequencia 
    INDEX ch-item-ped   IS PRIMARY UNIQUE nome-abrev nr-pedcli nr-sequencia it-codigo cod-refer.

DEF TEMP-TABLE tt-ped-venda NO-UNDO LIKE ped-venda 
     FIELD r-rowid AS ROWID.

DEF TEMP-TABLE tt-ped-item NO-UNDO LIKE ped-item
     FIELD r-rowid AS ROWID.

DEF TEMP-TABLE tt-ped-repre NO-UNDO LIKE ped-repre
    FIELD r-rowid AS ROWID.
DEF TEMP-TABLE wt-ped-repre  NO-UNDO LIKE ped-repre
    FIELD r-rowid AS ROWID.

DEF TEMP-TABLE tt-cond-ped NO-UNDO LIKE cond-ped
    FIELD r-rowid AS ROWID.
DEF TEMP-TABLE wt-cond-ped NO-UNDO LIKE cond-ped
    FIELD r-rowid AS ROWID.

DEFINE TEMP-TABLE tt-ref-item
       FIELD cod-refer   LIKE saldo-estoq.cod-refer
       FIELD qtidade-atu LIKE saldo-estoq.qtidade-atu
       FIELD preco-un    LIKE preco-item.preco-venda
       FIELD qt-pedida   LIKE ped-item.qt-pedida
       FIELD vl-tot-ref  AS   DEC FORMAT ">>>,>>>,>>9.9999"
       FIELD cod-depos   LIKE saldo-estoq.cod-depos
       INDEX indice1 cod-refer.

DEFINE TEMP-TABLE RowErrors NO-UNDO
       FIELD ErrorSequence    AS INTEGER
       FIELD ErrorNumber      AS INTEGER
       FIELD ErrorDescription AS CHARACTER
       FIELD ErrorParameters  AS CHARACTER
       FIELD ErrorType        AS CHARACTER
       FIELD ErrorHelp        AS CHARACTER
       FIELD ErrorSubType     AS CHARACTER.

DEF TEMP-TABLE tt-estoque
    FIELD it-codigo      LIKE ob-etiqueta.it-codigo
    FIELD cod-refer      LIKE ob-etiqueta.cod-refer
    FIELD corte-comerc   LIKE ob-etiqueta.corte-comerc LABEL "Corte Comercial"
    FIELD qt-estoque     LIKE ob-etiqueta.quantidade   LABEL "Estoque"
    FIELD qt-res-antc    LIKE ob-etiqueta.quantidade   LABEL "Res.Antec."
    FIELD qt-ped-reserva LIKE ob-etiqueta.quantidade   LABEL "Ped.Reserva"
    FIELD qt-trf         AS   DEC FORMAT "->>>,>>9.99" LABEL "Transform"
    FIELD qt-benefic     LIKE ob-etiqueta.quantidade   LABEL "Benefic"
    FIELD qt-carteira    LIKE ped-item.qt-pedida       LABEL "Carteira"
    FIELD qt-res-cart    LIKE ob-etiqueta.quantidade   LABEL "Cart.Forn."
    FIELD qt-saldo       LIKE saldo-estoq.qtidade-atu  LABEL "SALDO"
    FIELD visualiza      AS   LOG INIT NO
    INDEX indice1 IS PRIMARY it-codigo cod-refer corte-comerc.

DEF TEMP-TABLE tt-saldo-estoq
    FIELD  cod-estabel   LIKE movadm.saldo-estoq.cod-estabel 
    FIELD  empresa       LIKE mgadm.empresa.nome
    FIELD  it-codigo     LIKE movadm.saldo-estoq.it-codigo
    field  cod-depos     like movadm.saldo-estoq.cod-depos
    field  cod-refer     like movadm.saldo-estoq.cod-refer 
    FIELD  qt-disponivel like movadm.saldo-estoq.qtidade-atu
    field  qt-aloc-ped   like movadm.saldo-estoq.qt-aloc-ped 
    field  qt-alocada    like movadm.saldo-estoq.qt-alocada 
    field  qtidade-atu   like movadm.saldo-estoq.qtidade-atu
    field  qt-aloc-prod  LIKE movadm.saldo-estoq.qt-aloc-prod
    FIELD  qt-aloc-pi    LIKE movadm.saldo-estoq.qtidade-atu.
