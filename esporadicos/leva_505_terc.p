
{esp/util.i}
DEFINE VARIABLE h-acomp AS HANDLE      NO-UNDO.

{esapi/ttRemessaTerc.i}
    
DEFINE VARIABLE iContMovto AS INTEGER     NO-UNDO.
DEFINE VARIABLE iCont      AS INTEGER     NO-UNDO.
DEFINE VARIABLE notaERP    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE logmsg     AS LOGICAL     NO-UNDO.
DEFINE TEMP-TABLE ttRemessa NO-UNDO
    FIELD id        AS INT
    FIELD itCodigo  AS CHAR
    FIELD codRefer  AS CHAR
    FIELD nrNota    AS CHAR
    FIELD dtSaida   AS DATE
    FIELD quantidade AS DECIMAL FORMAT '->>>,>>>,>>9.9999'
    FIELD qtSaldo    AS DECIMAL FORMAT '->>>,>>>,>>9.9999'
    FIELD valorUnit  AS DECIMAL FORMAT '->>>,>>>,>>9.9999'
    FIELD valorTotal AS DECIMAL FORMAT '->>>,>>>,>>9.9999'
    FIELD vlSaldo    AS DECIMAL FORMAT '->>>,>>>,>>9.9999'
    INDEX ind-dt-saida IS PRIMARY itCodigo codRefer dtSaida  nrNota
    INDEX ind-saldo   itCodigo codRefer dtSaida qtSaldo.



DEFINE TEMP-TABLE ttMovto  NO-UNDO
    FIELD id        AS INT
    FIELD itCodigo  AS CHAR
    FIELD codRefer  AS CHAR
    FIELD nrNota    AS CHAR
    FIELD nrNotaRem AS CHAR
    FIELD nfBaixada AS CHAR
    FIELD nfERP     AS CHAR
    FIELD logLancERP AS LOGICAL
    FIELD dataMovto AS DATE
    FIELD qtMovto   AS DECIMAL FORMAT '->>>,>>>,>>9.9999'
    FIELD vlMovto   AS DECIMAL FORMAT '->>>,>>>,>>9.9999'
    FIELD qtSaldo   AS DECIMAL FORMAT '->>>,>>>,>>9.9999'
    FIELD vlSaldo   AS DECIMAL FORMAT '->>>,>>>,>>9.9999'
    FIELD descricao AS CHAR FORMAT 'x(100)'
    FIELD nrPedido  AS CHAR
    INDEX ind-item-ref-nota IS PRIMARY itCodigo codRefer nrNota dataMovto
    INDEX ind-id id
    INDEX ind-item-ref-nota-id itCodigo codRefer nrNota id
    .
DEFINE BUFFER bf FOR ttRemessa.
DEFINE BUFFER bfMovto FOR ttMovto.

RUN utp/ut-acomp PERSIST SET h-acomp.
RUN pi-inicializar IN h-acomp('CONCILIACAO LISA').

FOR EACH nota-fiscal NO-LOCK
    WHERE nota-fiscal.nat-operacao = '59207i'
    AND nota-fiscal.cod-estabel = '505'
    AND nota-fiscal.dt-cancela  = ?.
    RUN pi-acompanhar IN h-acomp("NF REM:" + nota-fiscal.nr-nota-fis + "-Dt:" + string(nota-fiscal.dt-emis-nota)).
    FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK:
       ASSIGN iCont = iCont + 1.
        CREATE ttRemessa.
        ASSIGN ttRemessa.id = iCont
               ttRemessa.itCodigo   = it-nota-fisc.it-codigo
               ttRemessa.codRefer   = it-nota-fisc.cod-refer
               ttRemessa.nrNota     = nota-fiscal.nr-nota-fis
               ttRemessa.dtSaida    = nota-fiscal.dt-emis-nota
               ttRemessa.quantidade = it-nota-fisc.qt-faturada[1]
               ttRemessa.qtSaldo    = it-nota-fisc.qt-faturada[1]
               ttRemessa.valorUnit  = it-nota-fisc.vl-preuni
               ttRemessa.valorTotal = it-nota-fisc.vl-tot-item
               ttRemessa.vlSaldo    = it-nota-fisc.vl-tot-item
               .                                              
        RUN pi-acompanhar IN h-acomp("MOVTO NF REM:" + nota-fiscal.nr-nota-fis + "-Dt:" + string(nota-fiscal.dt-emis-nota)).
        RUN criarMovto(
         it-nota-fisc.it-codigo,
         it-nota-fisc.cod-refer,
         it-nota-fisc.nr-nota-fis,
         '',
         ttRemessa.quantidade,
         ttRemessa.valorTotal,
         "Saldo referente Remessa",
         ttRemessa.dtSaida,
         '',
         '',
         YES,
         ''
         ).
    END.   

    

END.

ASSIGN icont = 0.
//NFs DE RETORNO LAN€ADAS NO ERP
FOR EACH docum-est NO-LOCK
    WHERE docum-est.nat-operacao = '19207i'
    AND docum-est.cod-estabel = '505'.
    RUN pi-acompanhar IN h-acomp("NF RET:" + docum-est.nro-docto + "-Dt:" + string(docum-est.dt-trans)).
    FOR EACH item-doc-est OF docum-est NO-LOCK:
        RUN getNfERP(ROWID(ITEM-DOC-EST), OUTPUT notaErp).

        

        //FIND LAST bf NO-ERROR.
        ASSIGN iCont = iCont + 1.
        CREATE ttRetorno.
        ASSIGN ttRetorno.id = iCont
               ttRetorno.itCodigo   = item-doc-est.it-codigo
               ttRetorno.codRefer   = item-doc-est.cod-refer
               ttRetorno.nrNota     = docum-est.nro-docto
               ttRetorno.dtEntrada  = docum-est.dt-trans
               ttRetorno.quantidade = item-doc-est.quantidade
               ttRetorno.valorUnit  = item-doc-est.preco-unit[1]
               ttRetorno.valorTotal = item-doc-est.preco-total[1]
               ttRetorno.nfErp      = notaErp
               ttRetorno.logLancErp = YES
               .

        FIND itens_pedido_lisa NO-LOCK
            WHERE int(itens_pedido_lisa.nf_retorno)  = INT(item-doc-est.nro-docto)
            AND   itens_pedido_lisa.serie_nf_retorno = ITEM-doc-est.serie-docto
            AND   itens_pedido_lisa.it_codigo       = ITem-doc-est.it-codigo
            AND   itens_pedido_lisa.cod_refer       = item-doc-est.cod-refer
            NO-ERROR.
        FIND pedidos_lisa
            WHERE pedidos_lisa.pedido_lisa_id = itens_pedido_lisa.pedido_lisa_id
            NO-LOCK NO-ERROR.
        IF AVAIL itens_pedido_lisa THEN DO:
           ASSIGN ttRetorno.nfBaixada = itens_pedido_lisa.nf_origem
                  ttRetorno.nrPedido  = string(pedidos_lisa.nr_pedido).
        END.
        RUN pi-acompanhar IN h-acomp("BAIXA NF RET:" + docum-est.nro-docto + "-Dt:" + string(docum-est.dt-trans)).
        RUN baixarRetorno(ROWID(ttRetorno)).


    END.            
END.



//NFs DE RETORNO EMITIDAS PELA LISA E NÇO LAN€ADAS NO ERP
RUN esapi/getItemPedidoLisaPendLanctoErp.p(INPUT-OUTPUT TABLE ttRetorno).
FOR EACH ttRetorno
    WHERE ttRetorno.logLancErp = NO:
     RUN pi-acompanhar IN h-acomp("BAIXA PROJETADA NF RET:" + ttRetorno.nrNota ).
        RUN baixarRetorno(ROWID(ttRetorno)).


END.


{esp/exportarTabelacsv3.i ttRemessa " " " " "ttRemessa" }
{esp/exportarTabelacsv3.i ttRetorno " " " " "ttRetorno" }
{esp/exportarTabelacsv3.i ttMovto   " " " " "ttMovto" }

RUN pi-finalizar IN h-acomp.

PROCEDURE baixarRetorno:

    DEFINE INPUT  PARAMETER pRowid AS ROWID     NO-UNDO.
    DEFINE VARIABLE dSaldo AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE dAbat  AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE iCont  AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cHist  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE mem    AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE nfRem  AS CHARACTER   NO-UNDO.


    
    FIND ttRetorno
        WHERE rowid(ttRetorno)  = pRowid NO-ERROR.
    IF AVAIL ttRetorno THEN DO:
       ASSIGN dSaldo = ttRetorno.quantidade.
       RUN incrValor(INPUT-OUTPUT mem, 'Qt.Retorno a Baixar:' + STRING(dSaldo),"|").
    END.
    ELSE RETURN 'nok'.

    /*IF int(ttRetorno.nrNota) = 95374 
       AND ttRetorno.itCodigo = '535492'
       AND ttRetorno.codRefer = 'i46'  THEN
       ASSIGN logmsg = TRUE.
    ELSE 
       ASSIGN logmsg = FALSE.*/

    //RUN msg('saldo a baixar:' + STRING(dSaldo)).

    REPEAT iCont = 1 TO 50:
        RUN incrValor(INPUT-OUTPUT mem, 'Contador:' + STRING(iCont),"|").
        ASSIGN iCont = iCont + 1.
        FIND FIRST ttRemessa
            WHERE ttRemessa.itCodigo = ttRetorno.itCodigo
            AND   ttRemessa.codrefer = ttRetorno.codRefer
            AND   ttRemessa.qtSaldo  > 0 USE-INDEX ind-saldo NO-ERROR.
        IF  AVAIL ttRemessa THEN DO:
            ASSIGN nfRem = ttRemessa.nrNota.
            RUN incrValor(INPUT-OUTPUT mem, 'NF Remessa Encontrada:' + STRING(ttRemessa.nrNota),"|").
            RUN incrValor(INPUT-OUTPUT mem, 'Saldo NF Remessa Encontrada:' + STRING(ttRemessa.qtSaldo),"|").
            IF ttRemessa.qtSaldo >= dSaldo THEN DO:
               ASSIGN ttRemessa.qtSaldo = ttRemessa.qtSaldo - dSaldo
                      ttRemessa.vlSaldo = ttRemessa.vlSaldo - (dSaldo * ttRetorno.valorUnit)
                      dAbat  = dSaldo 
                      dSaldo = 0
                      cHist =  "Baixa de Retorno TOTAL" .
               RUN incrValor(INPUT-OUTPUT mem, 'NF Remessa Saldo Ap¢s baixa:' + STRING(ttRemessa.qtSaldo),"|").   
               RUN incrValor(INPUT-OUTPUT mem, 'Saldo a Compensar:' + STRING(dSaldo),"|").   
            END.
            ELSE DO:
             /* RUN msg('saldo da remessa:' + STRING(ttRemessa.qtSaldo) + CHR(13) + 
                      'saldo do retorno:' + STRING(dSaldo) + CHR(13) 
                      ).*/
              ASSIGN dAbat =  ttRemessa.qtSaldo 
                     dSaldo = dSaldo - dAbat.        
              ASSIGN ttRemessa.qtSaldo  = ttRemessa.qtSaldo - dAbat
                     ttRemessa.vlSaldo = ttRemessa.vlSaldo - dAbat * ttRetorno.valorUnit 
                     cHist =  "Baixa de Retorno PARCIAL" .
              /*RUN msg('saldo abatido na remessa:' + STRING(dAbat) + CHR(13) + 
                      'novo saldo da remessa:' + STRING(ttRemessa.qtsaldo) + CHR(13) 
                      ).*/
              RUN incrValor(INPUT-OUTPUT mem, 'NF Remessa Saldo Ap¢s baixa:' + STRING(ttRemessa.qtSaldo),"|").   
              RUN incrValor(INPUT-OUTPUT mem, 'Saldo Retorno a Compensar:' + STRING(dSaldo),"|").   

            END.
        END.
        ELSE DO:
            RUN incrValor(INPUT-OUTPUT mem, 'NF Remessa NÆo Encontrada- Saldo Retorno a Compensar' + STRING(dSaldo),"|").   
            ASSIGN ttRetorno.qtNaoBaixada = dSaldo. 
            LEAVE.
        END.
        ASSIGN ttRetorno.memoriaCalc = mem.

        RUN criarMovto(
        ttRetorno.itCodigo,
        ttRetorno.codRefer,
        ttRetorno.nrNota,
        nfRem,
        dAbat * -1,
        dAbat * ttRetorno.valorUnit * -1,
        IF AVAIL ttRemessa THEN cHist ELSE "Retorno sem Possibilidade de Baixa",
        ttRetorno.dtEntrada,
        ttRetorno.nfBaixada,
        ttRetorno.nfErp,
        ttRetorno.LOGLancErp,
        ttRetorno.nrPedido     
        ).
        IF dSaldo = 0 OR iCont = 50 THEN LEAVE.
    END.

END PROCEDURE.

PROCEDURE msg:

    DEFINE INPUT  PARAMETER pTexto AS CHARACTER   NO-UNDO.

    IF logmsg THEN
       MESSAGE pTexto
           VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

END PROCEDURE.


PROCEDURE criarMovto:
    
    DEFINE INPUT  PARAMETER pItCodigo   AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pCodRefer   AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pNrNota     AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pNrNotaRem  AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pqtMovto    AS DECIMAL     NO-UNDO.
    DEFINE INPUT  PARAMETER pvlMovto    AS DECIMAL     NO-UNDO.
    DEFINE INPUT  PARAMETER pDescricao  AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pData       AS DATE        NO-UNDO.
    DEFINE INPUT  PARAMETER pNFBaixada  AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pNfErp      AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pLogLancErp AS LOGICAL     NO-UNDO.
    DEFINE INPUT  PARAMETER pNrPedido   AS CHARACTER   NO-UNDO.

    CREATE ttMovto.
    ASSIGN iContMovto = iContMovto + 1.
    ASSIGN ttMovto.id                      = iContMovto
           ttMovto.ItCodigo                = pItCodigo
           ttMovto.CodRefer                = pCodRefer  
           ttMovto.NrNota                  = pNrNota    
           ttMovto.NrNotaRem               = pNrNotaRem 
           ttMovto.qtMovto                 = pqtMovto    
           ttMovto.vlMovto                 = pvlMovto    
           ttMovto.Descricao               = pDescricao 
           ttMovto.dataMovto               = pData
           ttMovto.nfBaixada               = pNFBaixada
           ttMovto.nfErp                   = pNfErp
           ttMovto.logLancErp              = pLogLancErp
           ttMovto.nrPedido                = pnrPedido.
     
     FIND LAST bfMovto 
         WHERE bfMovto.itCodigo = pItCodigo
         AND   bfMovto.codRefer = pCodRefer
         AND   bfMovto.NrNota   = pNrNota
         AND   bfMovto.id       <  ttMovto.id
         USE-INDEX ind-item-ref-nota-id NO-ERROR.
     IF AVAIL bfMovto THEN DO:
        ASSIGN ttMovto.qtsaldo                 = bfMovto.qtSaldo  + pQtMovto
               ttMovto.vlSaldo                 = bfMovto.vlSaldo  + pVlMovto.
     END.
     ELSE DO:
        ASSIGN ttMovto.qtsaldo                 =  pQtMovto
               ttMovto.vlSaldo                 =  pVlMovto.
     END.
END PROCEDURE.


PROCEDURE getNFERP:

    DEFINE INPUT  PARAMETER pRowid  AS ROWID       NO-UNDO.
    DEFINE OUTPUT PARAMETER nrDocto AS CHARACTER   NO-UNDO.

    FIND item-doc-est NO-LOCK
        WHERE rowid(item-doc-est) = pRowid
        NO-ERROR.
    IF AVAIL item-doc-est THEN DO:
       ASSIGN nrDocto = item-doc-est.nro-comp.
    END.




END PROCEDURE.


