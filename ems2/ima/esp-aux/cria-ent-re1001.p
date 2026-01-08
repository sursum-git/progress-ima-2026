    /* Variaveis do Recebimento Fiscal ---                                  */
DEF TEMP-TABLE tt-docum-est NO-UNDO LIKE docum-est 
    FIELD r-rowid AS ROWID.
DEF TEMP-TABLE tt-item-doc-est NO-UNDO LIKE item-doc-est
    FIELD r-rowid AS ROWID.

DEF BUFFER b-estabelec FOR estabelec.

FIND estabelec WHERE
     estabelec.cod-estab = '5' NO-LOCK NO-ERROR.

FIND b-estabelec WHERE
     b-estabelec.cod-estab = '504' NO-LOCK NO-ERROR.


DEF VAR de-tot-valor AS DEC.
DEF VAR de-tot-qtde AS DEC.
DEF VAR c-nro-docto AS CHAR.


ASSIGN c-nro-docto = '0141197'.


FIND FIRST nota-fiscal WHERE
           nota-fiscal.dt-emis >= 03.01.2022 AND
           nota-fiscal.nr-nota-fis = c-nro-docto NO-ERROR.

/* Cria Temp-Table para o Recebimento*/
CREATE tt-docum-est.
ASSIGN tt-docum-est.cod-emit = estabelec.cod-emit
       tt-docum-est.serie-docto = nota-fiscal.serie
       tt-docum-est.nro-docto = nota-fiscal.nr-nota-fis
       tt-docum-est.cod-estabel = b-estabelec.cod-estabel
       tt-docum-est.nat-operacao = '11207m'
       tt-docum-est.dt-emissao = nota-fiscal.dt-emis
       tt-docum-est.dt-trans = TODAY
       tt-docum-est.cod-chave-aces-nf-eletro = nota-fiscal.cod-chave-aces-nf-eletro.

FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK
         BREAK BY it-nota-fisc.it-codigo.

    FIND item WHERE
         item.it-codigo = it-nota-fisc.it-codigo NO-LOCK NO-ERROR.

    /* Cria os Itens do Recebimento */
    CREATE tt-item-doc-est.
    ASSIGN tt-item-doc-est.nro-docto      = tt-docum-est.nro-docto
           tt-item-doc-est.serie-docto    = tt-docum-est.serie-docto
           tt-item-doc-est.cod-emitente   = tt-docum-est.cod-emit
           tt-item-doc-est.nat-operacao   = tt-docum-est.nat-operacao
           tt-item-doc-est.sequencia      = it-nota-fisc.nr-seq-fat
           tt-item-doc-est.cod-depos      = 'DEF'
           tt-item-doc-est.it-codigo      = it-nota-fisc.it-codigo
           tt-item-doc-est.cod-refer      = it-nota-fisc.cod-refer
           tt-item-doc-est.lote           = it-nota-fisc.cod-refer
           tt-item-doc-est.qt-do-forn     = it-nota-fisc.qt-faturada[1]
           tt-item-doc-est.preco-total[1] = it-nota-fisc.vl-tot-item
           tt-item-doc-est.preco-unit[1]  = it-nota-fisc.vl-preuni
           tt-item-doc-est.peso-bruto     = it-nota-fisc.qt-faturada[1]
           tt-item-doc-est.base-icm       = it-nota-fisc.vl-tot-item
           tt-item-doc-est.base-ipi       = it-nota-fisc.vl-tot-item
           tt-item-doc-est.dt-vali-lote   = 12.31.9999
           tt-item-doc-est.narrativa      = "Recebimento Autom tico".

    ASSIGN de-tot-qtde = de-tot-qtde + it-nota-fisc.qt-faturada[1]
           de-tot-valor = de-tot-valor + it-nota-fisc.vl-tot-item.
END.
ASSIGN tt-docum-est.tot-valor = de-tot-valor 
       tt-docum-est.tot-peso = de-tot-qtde
       tt-docum-est.valor-mercad = de-tot-valor
       tt-docum-est.base-icm = de-tot-valor  
       tt-docum-est.base-ipi = de-tot-valor 
       tt-docum-est.despesa-nota = 0.


// Cria docum-est
RUN esapi/cria-nota-re1001.p (INPUT TABLE tt-docum-est,
                              INPUT TABLE tt-item-doc-est).

MESSAGE RETURN-VALUE
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

