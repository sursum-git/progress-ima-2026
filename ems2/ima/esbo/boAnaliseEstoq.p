/**************************************************************************
PADRAO DE BO DE CONSULTA
Programa: esbo/boAnaliseEstoq.p 
Autor: Tadeu Silva Parreiras
Objetivo: Extrair dados para analise de estoque
Data: 
Modificacoes:
*****************************************************************************/


&SCOPED-DEFINE ttParam  ttParam
&SCOPED-DEFINE boMsg    HBoMsg
&SCOPED-DEFINE ttResult ttResult
&SCOPED-DEFINE tabela   movto-estoq
DEFINE TEMP-TABLE {&ttparam}
    FIELD codEstabel       AS CHAR EXTENT 2
    FIELD itCodigo         AS CHAR EXTENT 2
    FIELD codRefer         AS CHAR EXTENT 2
    FIELD geCodigo         AS  INT EXTENT 2
    FIELD dtTrans          AS DATE EXTENT 2 FORMAT '99/99/9999'
    FIELD logApenasFat     AS LOGICAL    
    .

DEFINE VARIABLE {&boMsg}   AS HANDLE      NO-UNDO.
//DEFINE VARIABLE cTpRet     AS CHARACTER   NO-UNDO.

 

{esbo/bomsg.i}
{esp/util.i}
{esp/setProp.i  {&ttparam} }
{esbo/boAnaliseEstoq.i}

DEFINE TEMP-TABLE ttDetMovto NO-UNDO LIKE ttMovto.

//DEFINE TEMP-TABLE ttMovtoEtq NO-UNDO  LIKE ttMovto.

{esbo/boMovtoEstoqEtq.i}
DEFINE VARIABLE h-acomp AS HANDLE      NO-UNDO.

PROCEDURE iniciar:

RUN esbo/boMsg.p PERSIST SET {&boMsg}.
RUN esbo/boAcomp.p PERSIST SET h-acomp.
CREATE {&ttparam}.
    
END PROCEDURE.

PROCEDURE finalizar:
    IF VALID-HANDLE({&boMsg}) THEN
       DELETE PROCEDURE {&boMsg}.
    IF VALID-HANDLE(h-acomp) THEN
       RUN finalizar IN h-acomp.
    DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

PROCEDURE setValsIni:

    FIND FIRST {&ttParam} NO-ERROR.
    ASSIGN 
    {&ttParam}.codEstabel[1] = ''
    {&ttParam}.codEstabel[2] = 'zzzz'
    {&ttParam}.itCodigo[1]   = ''  
    {&ttParam}.itCodigo[2]   = 'zzzzzzzzzzzzzz'  
    {&ttParam}.codRefer[1]   = ''   
    {&ttParam}.codRefer[2]   = 'zzzzz'
    {&ttParam}.geCodigo[1]   = 0   
    {&ttParam}.geCodigo[2]   = 999999
    {&ttParam}.dtTrans[1]    = 01.01.2001
    {&ttParam}.dtTrans[2]    = 01.01.2999
   
    .

            

END PROCEDURE.

/*PROCEDURE setTipoRetorno:

    DEFINE INPUT  PARAMETER pRet AS CHARACTER   NO-UNDO.
    
    ASSIGN cTpRet = pRet.


END PROCEDURE.          */

PROCEDURE setTTParam:

    DEFINE INPUT PARAMETER TABLE FOR {&ttParam}.

END PROCEDURE.

PROCEDURE setAcomp:

    DEFINE INPUT  PARAMETER logHabilita AS LOGICAL     NO-UNDO.
    DEFINE INPUT  PARAMETER pHAComp     AS HANDLE      NO-UNDO.

    RUN setHabilita IN h-acomp(logHabilita).
    IF valid-handle(phAcomp) THEN DO:
       RUN setHandle IN h-acomp(phAComp).
    END.
    ELSE DO:
       RUN setTitulo IN h-acomp('Extra‡Æo Dados Movimento Estoque').
    END.

    

END PROCEDURE.


PROCEDURE setBoMsg:

    DEFINE INPUT  PARAMETER pHBoMsg AS HANDLE      NO-UNDO.
    ASSIGN {&boMsg} = pHBoMsg.

END PROCEDURE.

PROCEDURE _gerarTtSaldo:

    FIND FIRST {&ttparam} NO-ERROR.
    IF NOT AVAIL {&ttParam} THEN DO:
      RUN setMsg IN {&bomsg}(10,'NÆo foram passados parametros','erro').
      RETURN 'nok'.
    END.
    EMPTY TEMP-TABLE ttSaldo.
    FOR EACH ttMovto
        BREAK BY ttMovto.itCodigo BY ttMovto.codRefer :
        IF  FIRST-OF(ttMovto.codRefer) THEN DO:
           
           FOR EACH saldo-estoq NO-LOCK                                                                                        
               WHERE saldo-estoq.it-codigo     = ttMovto.itCodigo
               AND   saldo-estoq.cod-refer     = ttMovto.codRefer.
               RUN acomp IN h-acomp('Saldo Estoq. Item:'  + saldo-estoq.it-codigo + " - Ref:" + saldo-estoq.cod-refer).        
               FIND ttSaldo                                                                                                    
                   WHERE ttSaldo.itCodigo      = saldo-estoq.it-codigo                                                         
                   AND   ttSaldo.codRefer      = saldo-estoq.cod-refer                                                         
                   NO-ERROR.                                                                                                   
               IF NOT AVAIL ttSaldo THEN DO:                                                                                   
                  CREATE ttSaldo.                                                                                              
                  ASSIGN ttSaldo.itCodigo         = saldo-estoq.it-codigo                                                      
                         ttSaldo.codRefer         = saldo-estoq.cod-refer .                                                    
               END.                                                                                                            
               ASSIGN ttSaldo.qtSaldoAtual        = ttSaldo.qtSaldoAtual + saldo-estoq.qtidade-atu .                           
           END.
           FIND ttSaldo
           WHERE ttSaldo.itCodigo         = ttMovto.itCodigo
           AND   ttSaldo.codRefer         = ttMovto.codRefer NO-ERROR.
           IF NOT AVAIL ttSaldo THEN DO:
              CREATE ttSaldo.                                                                                              
              ASSIGN ttSaldo.itCodigo         = saldo-estoq.it-codigo                                                      
                     ttSaldo.codRefer         = saldo-estoq.cod-refer.
           END.
        END. 
        FIND ttSaldo
           WHERE ttSaldo.itCodigo         = ttMovto.itCodigo
           AND   ttSaldo.codRefer         = ttMovto.codRefer NO-ERROR.
        IF AVAIL ttSaldo THEN DO:
           IF ttMovto.tipoTrans = 1 THEN //entrada
              ASSIGN ttSaldo.qtEntrada = ttSaldo.qtEntrada + ttMovto.qtMovto.
           ELSE
              ASSIGN ttSaldo.qtSaida    = ttSaldo.qtSaida   + ttMovto.qtMovto * -1.
        END.
    END.       
END PROCEDURE.




PROCEDURE exec:
    
    DEFINE VARIABLE iSinal     AS INTEGER     NO-UNDO.
    
    RUN limparTTMsg IN {&boMsg}.
    FIND FIRST {&ttparam} NO-ERROR.
    IF NOT AVAIL {&ttParam} THEN DO:
       RUN setMsg IN {&bomsg}(1,'NÆo foram passados parametros','erro').
       RETURN 'nok'.
    END.
    EMPTY TEMP-TABLE ttMovto.
    
    
    FOR EACH movto-estoq NO-LOCK
        WHERE movto-estoq.cod-estabel   >= {&ttParam}.codEstabel[1]
        AND   movto-estoq.cod-estabel   <= {&ttParam}.codEstabel[2]
        AND   movto-estoq.it-codigo     >= {&ttParam}.itCodigo[1]
        AND   movto-estoq.it-codigo     <= {&ttParam}.itCodigo[2]
        AND   movto-estoq.cod-refer     >= {&ttParam}.codRefer[1]
        AND   movto-estoq.cod-refer     <= {&ttParam}.codRefer[2]
        AND   movto-estoq.dt-trans      >= {&ttParam}.dtTrans[1] 
        AND   movto-estoq.dt-trans      <= {&ttParam}.dtTrans[2]
        USE-INDEX data-item,
        ITEM NO-LOCK OF movto-estoq
        WHERE ITEM.ge-codigo            >= {&ttParam}.geCodigo[1]
        AND   ITEM.ge-codigo            <= {&ttParam}.geCodigo[2]
         .
        RUN acomp IN h-acomp('Mov-Data: ' + STRING(movto-estoq.dt-trans) + "- Prod:"  + movto-estoq.it-codigo + " - Ref:" + movto-estoq.cod-refer).
        IF {&ttParam}.logApenasFat AND item.ind-item-fat = NO  THEN NEXT.
        ASSIGN iSinal = IF movto-estoq.tipo-Trans = 1 THEN 1 ELSE -1 .
        CREATE ttMovto.
        ASSIGN ttMovto.rRowid            =  rowid(movto-estoq)
               ttMovto.itCodigo          =  movto-estoq.it-codigo
               ttMovto.codRefer          =  movto-estoq.cod-refer
               ttMovto.codEstabel        =  movto-estoq.cod-estabel
               ttMovto.descItem          =  IF AVAIL ITEM THEN ITEM.desc-item ELSE ''
               ttMovto.un                =  IF AVAIL ITEM THEN ITEM.un ELSE ''
               ttMovto.especDocto        =  movto-estoq.esp-docto
               ttMovto.desEspecDocto     =  {ininc/i03in218.i 4 ttMovto.especDocto }
               ttMovto.nrDocto           =  movto-estoq.nro-docto
               ttMovto.serie             =  movto-estoq.serie-docto
               ttMovto.codEmitente       =  movto-estoq.cod-emitente
               ttMovto.tipoTrans         =  movto-estoq.tipo-trans
               ttMovto.desTipoTrans      =  {ininc/i01in218.i 4 ttMovto.tipoTrans}
               ttMovto.natOperacao       =  movto-estoq.nat-operacao
               ttMovto.codDepos          =  movto-estoq.cod-depos
               ttMovto.codLocaliz        =  movto-estoq.cod-localiz
               ttMovto.qtMovto           =  movto-estoq.quantidade * iSinal
               ttMovto.numSequen         =  movto-estoq.num-sequen
               ttMovto.dtTrans           =  movto-estoq.dt-trans
               ttMovto.ano               =  YEAR(movto-estoq.dt-trans)
               ttMovto.mes               =  MONTH(movto-estoq.dt-trans)
               ttMovto.dia               =  DAY(movto-estoq.dt-trans)
               ttMovto.ordem             = 1
               ttMovto.vlMovto           = IF substr(ttMovto.natOperacao,1,1) = '3' AND especDocto = 21 THEN movto-estoq.valor-mat-m[1] + movto-estoq.valor-icm
                                           ELSE
                                           (movto-estoq.valor-mat-m[1] + movto-estoq.valor-mat-m[2] + movto-estoq.valor-mat-m[3] +
                                            movto-estoq.valor-mat-o[1] + movto-estoq.valor-mat-o[2] + movto-estoq.valor-mat-o[3] +
                                            movto-estoq.valor-mat-p[1] + movto-estoq.valor-mat-p[2] + movto-estoq.valor-mat-p[3] +
                                            movto-estoq.valor-mob-m[1] + movto-estoq.valor-mob-m[2] + movto-estoq.valor-mob-m[3] +
                                            movto-estoq.valor-mob-o[1] + movto-estoq.valor-mob-o[2] + movto-estoq.valor-mob-o[3] +
                                            movto-estoq.valor-mob-p[1] + movto-estoq.valor-mob-p[2] + movto-estoq.valor-mob-p[3] + 
                                            movto-estoq.valor-nota     + movto-estoq.val-cofins     + movto-estoq.valor-icm      + 
                                            movto-estoq.valor-ipi ) * iSinal
               .
               
        RUN setMsg IN {&bomsg}(1,'produto:' + ttmovto.itCodigo + '-refer:' + ttmovto.codRefer + '-nrDocto:' + ttMovto.nrDocto + '-qt:' + string(ttMovto.qtMovto) ,'log').       
        RUN classificarMovtoEstoq(INPUT  ttMovto.codEmitente,
                                  INPUT  ttMovto.especDocto,
                                  INPUT  ttMovto.desEspecDocto,  
                                  INPUT  ttMovto.tipoTrans,     
                                  INPUT  ttMovto.natOperacao,   
                                  OUTPUT ttMovto.desClassifMovto,
                                  OUTPUT ttMovto.codClassifMovto).
        

        RUN _getAgrup(ttMovto.codClassifMovto, OUTPUT ttMovto.agrup).
        
        //verifica se ‚ uma nota fiscal de importa‡Æo e busca o numero do container
        IF substring(ttMovto.natOperacao,1,1) = '3' THEN  DO:            
           RUN esapi/getNrContainerNFImp.p(ttMovto.codEstabel,ttMovto.serie,ttMovto.nrDocto,ttMovto.natOperacao,OUTPUT ttMovto.nrContainer).
           IF ttMovto.nrContainer > 0 THEN DO:
            /*  FOR FIRST :
              END.
              ASSIGN ttMovto.cubagem =  */
               
           END.
        END.
        
        

    END.
    //nao mudar ordem de chamada das procedures, senÆo podem ocorrer efeitos colaterais
    RUN _gerarTtSaldo.
    RUN _gerarMovtoSaldoAnterior.
    RUN _gerarSaldoAcum.
    RUN _gerarSaldoData.
    RUN _gerarSaldoComp.
    RUN _gerarEstatisticaProdutoRef.
    RUN _gerarEstatisticaProduto.
    
    FOR EACH ttMovto:
        RUN setMsg IN {&bomsg}(1,'DEPOIS-produto:' + ttmovto.itCodigo + '-refer:' + ttmovto.codRefer + '-nrDocto:' + ttMovto.nrDocto + '-qt:' + string(ttMovto.qtMovto) ,'log').    
    END.

END PROCEDURE.


PROCEDURE getTTMovtoContainer:

    DEFINE INPUT  PARAMETER  pTipoRet AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE hBoMovtoEstoqEtq  AS HANDLE      NO-UNDO.   
    //DEFINE VARIABLE dQuant AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE lAchou AS LOGICAL     NO-UNDO.
    
    FIND FIRST {&ttparam} NO-ERROR.
    IF NOT AVAIL {&ttparam}  THEN    DO:
       RETURN 'nok'.       
    END.
    RUN esbo/boMovtoEstoqEtq.p PERSIST SET hBoMovtoEStoqEtq.
    FOR EACH ttMovto
        WHERE ttMovto.especDocto = 20 
        OR    ttMovto.especDocto = 22:  
        RUN acomp IN h-acomp('Etq|Chave:'  + ttMovto.nrDocto + "|"  + ttMovto.itCodigo + "|" + ttMovto.codRefer).
        RUN setTipoRetorno     IN hBoMovtoEstoqEtq(pTipoRet).
        RUN setRowidMovtoEstoq IN hBoMovtoEstoqEtq(ttMovto.rROWID).
        RUN setMsg IN {&bomsg}(10,'Buscando Etqs movto estoq = rowid(' + STRING(ttMovto.rRowid) + ') :' + '- tipo:' + ptipoRet ,'log').        
        RUN getDadosNfVenda   IN hBoMovtoEstoqEtq.        
        CASE pTipoRet:
            WHEN 'container' THEN DO:
                RUN getTTContainer IN hBoMovtoEstoqEtq(OUTPUT TABLE ttContainer).
                {esp/exportarTabelaCsv3.i ttContainer " " " " "ttContainerAnalise"  }         
                IF CAN-FIND(FIRST ttContainer) THEN DO:
                   RUN setMsg IN {&bomsg}(20,'Buscando por container. Encontrou registros ETQ' ,'log').
                   ASSIGN lAchou = YES.     
                   FOR EACH ttContainer:
                        CREATE ttDetMovto.
                        BUFFER-COPY ttMovto TO ttDetMovto.  
                        ASSIGN ttDetMovto.qtMovto        = ttContainer.quantidade  * -1
                               ttDetmovto.nrContainer    = ttContainer.nr-Container                               
                               ttDetmovto.vlMovto        = ttDetmovto.vlMovto / ttDetMovto.qtMovto * ttContainer.quantidade * -1
                               .
                       RUN setMsg IN {&bomsg}(20,'item:' + ttMovto.itCodigo + '- ref:' + ttMovto.codRefer + '- container:' + string(ttContainer.nr-container) + '- quantidade:' + STRING(ttContainer.quantidade) ,'log').        
                    END.
                END.
                ELSE DO:
                   RUN setMsg IN {&bomsg}(30,'Buscando por container. NAO Encontrou registros ETQ' ,'log'). 
                   ASSIGN lAchou = NO.                 
                END.
                   
                
            END.
            WHEN 'etiqueta' THEN DO:
                RUN getTtEtq IN hBoMovtoEstoqEtq(OUTPUT TABLE ttEtq).
                IF CAN-FIND(FIRST ttEtq) THEN DO:
                   ASSIGN lAchou = YES.     
                   FOR EACH ttEtq:
                        CREATE ttDetMovto.
                        BUFFER-COPY ttMovto TO ttDetMovto.  
                        ASSIGN ttDetMovto.qtMovto         = ttEtq.quantidade   * -1
                               ttDetmovto.nrContainer     =  ttEtq.nr-Container                   
                               ttDetMovto.numEtq          = ttEtq.num-etiqueta
                               ttDetmovto.vlMovto         = ttDetmovto.vlMovto / ttMovto.qtMovto * ttEtq.quantidade * -1
                               .
                         RUN setMsg IN {&bomsg}(40,'item:' + ttMovto.itCodigo + '- ref:' + ttMovto.codRefer + '- container:' + STRING(ttEtq.nr-container) + '- quantidade:' + STRING(ttEtq.quantidade) ,'log').               
                    END.
                END.
                ELSE DO:
                   RUN setMsg IN {&bomsg}(50,'Buscando por etiqueta. NAO Encontrou registros ETQ' ,'log'). 
                   ASSIGN lAchou = NO.                                 
                END.
                   
            END.
            
        END CASE.
        
        IF lAchou  THEN  DO:
           DELETE ttmovto.              
        END.       
    END.
    RUN acomp IN h-acomp('Copiando movimentos Detalhados').
    //criar registros detalhados no movimento principal.
    FOR EACH ttDetMovto:
        CREATE ttMovto.
        BUFFER-COPY ttDetMovto TO ttMovto.
    END.
    
    IF VALID-HANDLE(hBoMovtoEstoqEtq) THEN DO:
       DELETE PROCEDURE hBoMovtoEstoqEtq.        
    END.

END PROCEDURE.


PROCEDURE exportarTTParam:

OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY +  'PARAMETROS_BO_ANALISE_ESTOQ_' +    STRING(TIME) + '.txt').

    FOR EACH {&ttParam}.
        DISP {&ttParam} WITH 1 COL WIDTH 550.
    END.
OUTPUT CLOSE.


END PROCEDURE.

PROCEDURE getErro:

    DEFINE OUTPUT PARAMETER pErro AS CHARACTER   NO-UNDO.
    RUN getErro IN {&boMsg}.

END PROCEDURE.

PROCEDURE getTtMsg:   
    DEFINE OUTPUT PARAMETER TABLE FOR ttmsg.
    RUN getTtMsg IN {&boMsg}(OUTPUT TABLE ttMsg).

END PROCEDURE.

PROCEDURE expttMsg:
    DEFINE INPUT  PARAMETER pArquivo AS CHARACTER   NO-UNDO.
    RUN expttMsg IN {&bomsg}(INPUT pArquivo).

END PROCEDURE.

PROCEDURE getTTMovto:

    DEFINE OUTPUT PARAMETER TABLE FOR ttMovto.

END PROCEDURE.

PROCEDURE getTTSaldo:

    DEFINE OUTPUT PARAMETER TABLE FOR ttSaldo.

END PROCEDURE.
                                                                                                                            

PROCEDURE exportarTTResult:

      DEFINE INPUT  PARAMETER pArquivo      AS CHARACTER   NO-UNDO.
      DEFINE INPUT  PARAMETER pDelimitador  AS CHARACTER   NO-UNDO.
      OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY + pArquivo) NO-CONVERT.
      PUT UNFORM "Produto"              pDelimitador 
                 "Referˆncia"           pDelimitador 
                 "Descri‡Æo"            pDelimitador 
                 "U.M."                 pDelimitador 
                 "Esp‚cie"              pDelimitador 
                 "Descr.Esp‚cie"        pDelimitador
                 "Nr.Docto"             pDelimitador
                 "S‚rie"                pDelimitador
                 "Cod.Emitente"         pDelimitador
                 "Tp.Trans"             pDelimitador
                 "Descr.Tp.Trans"       pDelimitador
                 "Nat.Opera‡Æo"         pDelimitador
                 "Dep¢sito"             pDelimitador
                 "Localiz."             pDelimitador   
                 "Quantidade"           pDelimitador   
                 "Saldo Acumulado"      pDelimitador   
                 "Dt.Transa‡Æo"         pDelimitador   
                 "Ano"                  pDelimitador   
                 "Mˆs"                  pDelimitador   
                 "Dia"                  pDelimitador   
                 "Vl.Movto"             pDelimitador   
                 "Agrupamento"          pDelimitador   
                 "Classif.Movto"        pDelimitador
                 "ID"                   pDelimitador
                 "Container"            pDelimitador
                 "Etiqueta" 
                 SKIP .
      FOR EACH ttMovto USE-INDEX ind-data:
          RUN acomp IN h-acomp('EXP-Data:'  + string(ttMovto.dtTrans) + " - Produto:" + ttMovto.itCodigo +  " - Refer:" +  ttMovto.codRefer).
          PUT UNFORM   
              ttMovto.itCodigo              pDelimitador
              ttMovto.codRefer              pDelimitador
              ttMovto.descItem              pDelimitador
              ttMovto.un                    pDelimitador
              ttMovto.especDocto            pDelimitador 
              ttMovto.desEspecDocto         pDelimitador
              ttMovto.nrDocto               pDelimitador
              ttMovto.serie                 pDelimitador
              ttMovto.codEmitente           pDelimitador
              ttMovto.tipoTrans             pDelimitador
              ttMovto.desTipoTrans          pDelimitador
              ttMovto.natOperacao           pDelimitador
              ttMovto.codDepos              pDelimitador
              ttMovto.codLocaliz            pDelimitador
              ttMovto.qtMovto               pDelimitador
              ttMovto.qtSaldoAcum           pDelimitador
              ttMovto.dtTrans               pDelimitador
              ttMovto.ano                   pDelimitador
              ttMovto.mes                   pDelimitador
              ttMovto.dia                   pDelimitador
              ttMovto.vlMovto               pDelimitador
              ttMovto.agrup                 pDelimitador
              ttMovto.desClassifMovto       pDelimitador
              ttMovto.id                    pDelimitador
              ttMovto.nrcontainer           pDelimitador
              ttMovto.numEtq                           
              SKIP .
           


      END.

      OUTPUT CLOSE.

END PROCEDURE.


PROCEDURE exportarEstatistica:

      DEFINE INPUT  PARAMETER pArquivo      AS CHARACTER   NO-UNDO.
      DEFINE INPUT  PARAMETER pDelimitador  AS CHARACTER   NO-UNDO.
      OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY + pArquivo) NO-CONVERT.
      PUT UNFORM "Produto"              pDelimitador 
                 "Referˆncia"           pDelimitador 
                 "Descri‡Æo"            pDelimitador 
                 "Qt.Importa‡äes"       pDelimitador 
                 "Qt.Importada"         pDelimitador 
                 "Dt.Ult.Importa‡Æo"    pDelimitador 
                 "Qt.Vendas"            pDelimitador 
                 "Qt.Vendida"           pDelimitador 
                 "Dt.Ult.Venda"         pDelimitador 
                 "Qt.Dias Sem Venda"    pDelimitador 
                 "Qt.Saldo Anterior"    pDelimitador 
                 "Qt.Saldo Atual"       pDelimitador
                 "% Redu‡Æo Saldo"      
                 SKIP .
      FOR EACH ttProdRef
          WHERE ttProdRef.qtSaldoAnterior <> 0 OR   ttprodRef.qtSaldoAtual <> 0
          USE-INDEX vl :
          RUN acomp IN h-acomp('EXP-EST-Produto:'  + ttProdRef.itCodigo +  " - Refer:" +  ttProdRef.codRefer).
          PUT UNFORM   
                 ttProdRef.itCodigo          pDelimitador  
                 ttProdRef.descitem          pDelimitador 
                 ttProdRef.codRefer          pDelimitador 
                 ttProdRef.qtImportacoes     pDelimitador 
                 ttProdRef.qtImportada       pDelimitador 
                 ttProdRef.dtUltImportacao   pDelimitador 
                 ttProdRef.qtVendas          pDelimitador 
                 ttProdRef.qtVendida         pDelimitador 
                 ttProdRef.dtUltimaVenda     pDelimitador 
                 ttProdRef.qtDiasSemVenda    pDelimitador 
                 ttProdRef.qtSaldoAnterior   pDelimitador 
                 ttProdRef.qtSaldoAtual      pDelimitador
                 ttProdRef.percRedSaldo      
                 SKIP .
           


      END.

      OUTPUT CLOSE.





END PROCEDURE.

PROCEDURE exportarEstatisticaProduto:

      DEFINE INPUT  PARAMETER pArquivo      AS CHARACTER   NO-UNDO.
      DEFINE INPUT  PARAMETER pDelimitador  AS CHARACTER   NO-UNDO.
      OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY + pArquivo) NO-CONVERT.
      PUT UNFORM "Produto"              pDelimitador 
                 "Descri‡Æo"            pDelimitador 
                 "Qt.Importa‡äes"       pDelimitador 
                 "Qt.Importada"         pDelimitador 
                 "Dt.Ult.Importa‡Æo"    pDelimitador 
                 "Qt.Vendas"            pDelimitador 
                 "Qt.Vendida"           pDelimitador 
                 "Dt.Ult.Venda"         pDelimitador 
                 "Qt.Dias Sem Venda"    pDelimitador 
                 "Qt.Saldo Anterior"    pDelimitador 
                 "Qt.Saldo Atual"       pDelimitador
                 "% Redu‡Æo Saldo"      
                 SKIP .
      FOR EACH ttProd
          WHERE ttProd.qtSaldoAnterior <> 0 OR   ttprod.qtSaldoAtual <> 0
          USE-INDEX vl :
          RUN acomp IN h-acomp('EXP-EST-Produto:'  + ttProd.itCodigo ).
          PUT UNFORM   
                 ttProd.itCodigo          pDelimitador  
                 ttProd.descitem          pDelimitador 
                 ttProd.qtImportacoes     pDelimitador 
                 ttProd.qtImportada       pDelimitador 
                 ttProd.dtUltImportacao   pDelimitador 
                 ttProd.qtVendas          pDelimitador 
                 ttProd.qtVendida         pDelimitador 
                 ttProd.dtUltimaVenda     pDelimitador 
                 ttProd.qtDiasSemVenda    pDelimitador 
                 ttProd.qtSaldoAnterior   pDelimitador 
                 ttProd.qtSaldoAtual      pDelimitador
                 ttProd.percRedSaldo      
                 SKIP .
      END.

      OUTPUT CLOSE.





END PROCEDURE.


PROCEDURE exportarTTSaldo:

      DEFINE INPUT  PARAMETER pArquivo      AS CHARACTER   NO-UNDO.
      DEFINE INPUT  PARAMETER pDelimitador  AS CHARACTER   NO-UNDO.
      OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY + pArquivo) NO-CONVERT.
      PUT UNFORM "Produto"              pDelimitador 
                 "Referencia"           pDelimitador 
                 "Dt.Inicial"           pDelimitador 
                 "QtSaldo Anterior"     pDelimitador 
                 "Qt.Entrada"           pDelimitador 
                 "Qt.Saida"             pDelimitador 
                 "Qt.Saldo Atual"       pDelimitador 
                  SKIP .
      FOR EACH ttSaldo:
          RUN acomp IN h-acomp('EXP-SALDO-Produto:'  + ttSaldo.itCodigo ).
          PUT UNFORM   
                 ttSaldo.itCodigo          pDelimitador  
                 ttSaldo.codRefer          pDelimitador  
                 ttSaldo.dtInicial         pDelimitador 
                 ttSaldo.qtSaldoAnterior   pDelimitador
                 ttSaldo.qtEntrada         pDelimitador 
                 ttSaldo.qtSaida           pDelimitador  
                 ttSaldo.qtSaldoAtual        
                 SKIP .
      END.         
      OUTPUT CLOSE.

END PROCEDURE.



PROCEDURE exportarTTComp:

      DEFINE INPUT  PARAMETER pArquivo      AS CHARACTER   NO-UNDO.
      DEFINE INPUT  PARAMETER pDelimitador  AS CHARACTER   NO-UNDO.
      OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY + pArquivo) NO-CONVERT.
      PUT UNFORM "Produto"              pDelimitador 
                 "Descr.Item"           pDelimitador 
                 "Referencia"           pDelimitador 
                 "Ano"                  pDelimitador 
                 "Mˆs"                  pDelimitador 
                 "Qt.Saldo"             SKIP .
      FOR EACH ttComp:
          RUN acomp IN h-acomp('EXP-SALDO-COMP-Produto:'  + ttComp.itCodigo + "-" + ttComp.codRefer).
          PUT UNFORM   
                 ttComp.itCodigo           pDelimitador  
                 ttComp.descItem           pDelimitador       
                 ttComp.codRefer           pDelimitador  
                 ttComp.ano                pDelimitador 
                 ttComp.Mes                pDelimitador
                 ttComp.qtSaldo            
                 SKIP .
      END.         
      OUTPUT CLOSE.

END PROCEDURE.


PROCEDURE exportarTTData:

      DEFINE INPUT  PARAMETER pArquivo      AS CHARACTER   NO-UNDO.
      DEFINE INPUT  PARAMETER pDelimitador  AS CHARACTER   NO-UNDO.
      OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY + pArquivo) NO-CONVERT.
      PUT UNFORM "Produto"              pDelimitador 
                 "Descr.Item"           pDelimitador 
                 "Referencia"           pDelimitador 
                 "Data"                 pDelimitador 
                 "Qt.Saldo"             SKIP .
      FOR EACH ttData:
          RUN acomp IN h-acomp('EXP-SALDO-COMP-Produto:'  + ttData.itCodigo + "-" + ttData.codRefer ).
          PUT UNFORM   
                 ttData.itCodigo           pDelimitador  
                 ttData.descItem           pDelimitador       
                 ttData.codRefer           pDelimitador  
                 ttData.data               pDelimitador
                 ttData.qtSaldo            
                 SKIP .
      END.         
      OUTPUT CLOSE.

END PROCEDURE.


PROCEDURE classificarMovtoEstoq:

    DEFINE INPUT  PARAMETER pCodEmitente    AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER pEspDocto       AS INT   NO-UNDO.
    DEFINE INPUT  PARAMETER pDesEspecDocto  AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pTipoTrans      AS INT   NO-UNDO.
    DEFINE INPUT  PARAMETER pNatOperacao    AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER cClassif        AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER iClassif        AS INTEGER     NO-UNDO.

    DEFINE VARIABLE lGerarDp     AS LOGICAL     NO-UNDO.
    DEFINE VARIABLE lTerceiros   AS LOGICAL     NO-UNDO.
    DEFINE VARIABLE descEspDocto AS CHARACTER   NO-UNDO.

    

    FIND natur-oper NO-LOCK
        WHERE natur-oper.nat-operacao   = pNatOperacao NO-ERROR.
    IF AVAIL natur-oper THEN DO:
       ASSIGN lGerarDp    = natur-oper.emite-duplic
              lTerceiros  = natur-oper.terceiros
              .     
    END.

    CASE pEspDocto:
        WHEN 2 OR  //act
        WHEN 6     //div
        THEN
          ASSIGN cClassif = "Acerto"
                 iClassif = 1.
        WHEN 15  THEN //inv
          ASSIGN cClassif = "Invent rio"
                 iClassif = 2.
        WHEN 20 THEN  //NFD
          ASSIGN cClassif = "Devolu‡Æo de Venda"
                 iClassif = 3.
        WHEN 21 THEN DO: //NFE
          IF lTerceiros THEN
             ASSIGN cClassif = "Movto Terceiros-Entrada"
                    iClassif = 4.
          IF AVAIL natur-oper AND SUBSTR(natur-oper.nat-operacao,1,1) = '3' THEN //importacao
             ASSIGN cClassif = "NF Importa‡Æo"
                    iClassif = 5.
        END.
        WHEN 22 THEN DO: //NFS
          IF lGerarDP THEN
             ASSIGN cClassif = "Venda"
                    iClassif = 6.
          ELSE DO:
             IF lTerceiros THEN
                ASSIGN cClassif = "Movto Terceiros-Saida"
                       iClassif = 7.
          END.
        END.
        WHEN 23 OR //NFT - transferencia externa 
        WHEN 33    //TRA - transferencia interna
        THEN
          ASSIGN cClassif = "Transferencia"
                 iClassif = 8.
        OTHERWISE DO:
          IF pTipoTrans = 1 THEN
            ASSIGN cClassif = "Outros-Entrada / " + pDesEspecDocto
                   iClassif = pEspDocto * 10  + 9.

         IF pTipoTRans = 2 THEN
            ASSIGN cClassif = "Outros-Saida-" + pDesEspecDocto
                   iClassif = pEspDocto * 10 + 10.
        END.
    END CASE.
    
    IF cClassif = '' THEN
       ASSIGN cClassif = "Nao Classificado / " + pDesEspecDocto.

    FIND estabelec  NO-LOCK
        WHERE estabelec.cod-emitente = pCodEmitente NO-ERROR.
    IF AVAIL estabelec THEN DO:
       ASSIGN cClassif = cClassif + " / Empresas Grupo"
              iClassif = iClassif + 1000
              .
    END.
    


END PROCEDURE.

PROCEDURE _gerarMovtoSaldoAnterior:

  DEFINE VARIABLE dtCustoMedio AS DATE        NO-UNDO.
  DEFINE VARIABLE vlCustoMedio AS DECIMAL     NO-UNDO.

  FIND FIRST ttParam NO-ERROR.

  FOR EACH ttSaldo:
      RUN acomp IN h-acomp('Calc.Sld.Ant.Item:'  + ttSaldo.itCodigo + " - Ref:" + ttSaldo.codRefer).
      ASSIGN ttSaldo.qtSaldoAnterior = (ttSaldo.qtSaldoAtual +  ttSaldo.qtSaida - ttSaldo.qtEntrada  ).
      FIND FIRST ttMovto
          WHERE ttMovto.itCodigo    = ttSaldo.itCodigo
          AND   ttMovto.codRefer    = ttSaldo.codRefer
          USE-INDEX ind-data NO-ERROR.
      IF AVAIL ttMovto THEN
         ASSIGN ttSaldo.dtInicial = ttMovto.dtTrans.
      ELSE
         ASSIGN ttSaldo.dtInicial = ttParam.dtTrans[1].
      IF ttSaldo.qtSaldoAnterior <> 0 THEN DO:
          FIND ITEM NO-LOCK
              WHERE ITEM.it-codigo = ttSaldo.itCodigo NO-ERROR.
          
          CREATE ttMovto.
          ASSIGN ttMovto.itCodigo          = ttSaldo.itCodigo
                 ttMovto.codRefer          = ttSaldo.codRefer
                 ttMovto.descItem          = IF AVAIL ITEM THEN ITEM.desc-item ELSE ''
                 ttMovto.un                = IF AVAIL ITEM THEN ITEM.un ELSE ''
                 ttMovto.dtTrans           = ttSaldo.dtInicial - 1
                 ttMovto.nrDocto           = "SALDO ANTERIOR"
                 ttMovto.ano               =  YEAR(ttMovto.dtTrans)
                 ttMovto.mes               =  MONTH(ttMovto.dtTrans)
                 ttMovto.dia               =  DAY(ttMovto.dtTrans)
                 ttMovto.desClassifMovto   =  "SALDO ANTERIOR"
                 ttMovto.codClassifMovto   =  99
                 ttMovto.especDocto        =  0
                 ttMovto.desEspecDocto     = "SLD"
                 ttMovto.tipoTrans         = IF ttSaldo.qtSaldoAnterior < 0 THEN  2 ELSE 1
                 ttMovto.desTipoTrans      = {ininc/i01in218.i 4 ttMovto.tipoTrans}
                 ttMovto.qtMovto           = IF ttMovto.TipoTrans = 1 THEN ttSaldo.qtSaldoAnterior ELSE ttSaldo.qtSaldoAnterior * -1
                 ttMovto.agrup             = "SALDO ANTERIOR"
                 .

           RUN esapi/getCustoMedioItemData.p(ttMovto.dtTrans, ttMovto.itCodigo, OUTPUT vlCustoMedio, OUTPUT dtCustoMedio).
           ASSIGN ttMovto.vlMovto          = ttMovto.qtMovto * vlCustoMedio.

      END.
  END.


END PROCEDURE.


PROCEDURE _getAgrup:

     DEFINE INPUT  PARAMETER pClassif AS INTEGER     NO-UNDO.
     DEFINE OUTPUT PARAMETER cAgrup   AS CHARACTER   NO-UNDO.

     IF pClassif = 1 OR (pClassif > 100 AND pClassif < 1000) THEN
        ASSIGN cAgrup = "Acertos".

     IF pClassif > 100 THEN
        ASSIGN cAgrup = "Movto Grupo Empresa".

     IF pClassif = 2 THEN
        ASSIGN cAgrup = "Inventario".

     IF pClassif = 3 THEN
        ASSIGN cAgrup = "Devolucao".


     IF pClassif = 4 OR pClassif = 7 THEN
        ASSIGN cAgrup = "Movto Terceiros".
   
     IF pClassif = 5 THEN
        ASSIGN cAgrup = "Importacao".

     IF pClassif = 6 THEN
        ASSIGN cAgrup = "Venda".

     IF pClassif = 8 THEN
        ASSIGN cAgrup = "Transferencia".

     IF pClassif = 99 THEN
        ASSIGN cAgrup = "Saldo Anterior".


END PROCEDURE.

PROCEDURE _gerarEstatisticaProdutoRef:

DEFINE VARIABLE dtUltImp    AS DATE        NO-UNDO.
DEFINE VARIABLE dtUltVenda  AS DATE        NO-UNDO.
RUN acomp IN h-acomp('Gerando Estatisticas do Produto e da Referˆncia').
FOR EACH ttMovto:
    FIND ttProdRef
        WHERE ttProdRef.itCodigo = ttMovto.itCodigo
        AND   ttProdRef.codRefer = ttMovto.codRefer
        NO-ERROR.
    IF NOT AVAIL ttProdRef THEN DO:
        CREATE ttProdRef.
        ASSIGN ttProdRef.itCodigo = ttMovto.itCodigo 
               ttProdRef.codRefer = ttMovto.codRefer 
               ttProdRef.descItem = ttMovto.descItem .
    END.
END.

FIND FIRST {&ttParam} NO-ERROR.

FOR EACH ttProdRef:
    //RUN acomp IN h-acomp('Estatistica Prod:'  + ttProdRef.itCodigo + " - Ref:" + ttProdRef.codRefer).
    ASSIGN dtUltImp = 01.01.2001.
    FOR EACH ttMovto
        WHERE ttMovto.agrup     = 'importacao'
        AND   ttMovto.itCodigo  = ttProdRef.itCodigo 
        AND   ttMovto.codRefer  = ttProdRef.codRefer
        USE-INDEX ind-agrup :
        ASSIGN ttProdRef.qtImportacoes = ttProdRef.qtImportacoes + 1.
               ttProdRef.qtImportada    = qtImportada + ttMovto.qtMovto.
        IF ttMovto.dtTrans > dtUltImp THEN
           ASSIGN dtUltImp = ttMovto.DtTrans.
    END.  
    ASSIGN ttProdRef.dtUltImportacao = dtUltImp.

    ASSIGN dtUltVenda = 01.01.2001.
    FOR EACH ttMovto
        WHERE ttMovto.agrup = 'venda'
        AND   ttMovto.itCodigo  = ttProdRef.itCodigo 
        AND   ttMovto.codRefer  = ttProdRef.codRefer
        USE-INDEX ind-agrup  :
        assign ttProdRef.qtVendas     = ttProdRef.qtVendas   + 1
               ttProdRef.qtVendida    = ttProdRef.qtVendida  + ttMovto.qtMovto * -1 
               .
        IF ttMovto.dtTrans > dtUltVenda THEN
           ASSIGN  dtUltVenda = ttMovto.dtTrans.
    END.  
    ASSIGN ttProdRef.dtUltimaVenda  = dtUltVenda
           ttProdRef.qtDiasSemVenda =  {&ttparam}.dtTrans[2] - dtUltVenda 
           .
    FIND ttSaldo
        WHERE ttSaldo.itCodigo = ttProdRef.itCodigo
        AND   ttSaldo.codRefer = ttprodREf.codRefer
        NO-ERROR.
    IF AVAIL ttSaldo THEN
       ASSIGN ttProdRef.qtSaldoAtual    = ttSaldo.qtSaldoAtual
              ttProdRef.qtSaldoAnterior = ttSaldo.qtSaldoAnterior
              ttProdRef.percRedSaldo    = IF ttSaldo.qtSaldoAnterior <> 0 THEN (1 - ttSaldo.qtSaldoAtual / ttSaldo.qtSaldoAnterior) * 100  ELSE 0.

END.

END PROCEDURE.

PROCEDURE _gerarEstatisticaProduto:

    RUN acomp IN h-acomp('Gerando Estatisticas do Produto').
    FOR EACH ttProdRef:
        FIND ttprod
            WHERE ttProd.itCodigo = ttprodRef.itCodigo
            NO-ERROR.
        IF NOT AVAIL ttProd THEN DO:
           CREATE ttProd.
           ASSIGN ttProd.itCodigo   = ttprodRef.itCodigo
                  ttProd.descItem   = ttProdRef.descitem
                  ttProd.dtUltImportacao = 01.01.2001
                  ttProd.dtUltimaVenda   = 01.01.2001
                  .
        END.
        ASSIGN ttProd.qtImportacoes     = ttProd.qtImportacoes      + ttprodRef.qtImportacoes
               ttProd.qtImportada       = ttProd.qtImportada        + ttprodRef.qtImportada
               ttProd.qtVendas          = ttProd.qtVendas           + ttprodRef.qtVendas
               ttProd.qtVendida         = ttProd.qtVendida          + ttprodRef.qtVendida
               ttProd.qtSaldoAnterior   = ttProd.qtSaldoAnterior    + ttProdRef.qtSaldoAnterior
               ttProd.qtSaldoAtual      = ttProd.qtSaldoAtual       + ttProdRef.qtSaldoAtual
              .

        IF ttProdRef.dtUltImportacao > ttProd.dtUltImportacao THEN
           ASSIGN ttProd.dtUltImportacao = ttProdRef.dtUltImportacao.


        IF ttProdRef.dtUltimaVenda > ttProd.dtUltimaVenda THEN
           ASSIGN ttProd.dtUltimaVenda = ttProdRef.dtUltimaVenda.
    END.

    FOR EACH ttProd:
        ASSIGN ttProd.qtDiasSemVenda = TODAY  - ttProd.dtUltimaVenda
               ttProd.percRedSaldo   = IF ttProd.qtSaldoAnterior <> 0 THEN (1 - ttprod.qtSaldoAtual / ttProd.qtSaldoAnterior) * 100 ELSE 0
               .
    END.

END PROCEDURE.


PROCEDURE _gerarSaldoAcum:

    DEFINE VARIABLE dSaldo LIKE ttMovto.qtSaldoAcum     NO-UNDO.
    DEFINE VARIABLE iCont AS INTEGER     NO-UNDO.
    DEFINE VARIABLE lContinuarMsg AS LOGICAL     NO-UNDO.
    RUN acomp IN h-acomp('Gerando Saldo Acumulado').
    FOR EACH ttMovto 
        USE-INDEX ind-data 
        BREAK 
        BY ttMovto.itCodigo 
        BY ttMovto.codRefer :
        ASSIGN iCont = iCont + 1
               ttMovto.Id = iCont.
        IF FIRST-OF(ttMovto.codRefer) THEN DO:
           ASSIGN dSaldo = 0.
/*            IF lContinuarMsg THEN                         */
/*            MESSAGE 'dentro do break' SKIP                */
/*                "ID:" ttmovto.id SKIP                     */
/*                "SALDO CALC:" dSaldo                      */
/*                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK. */
        END.
        
        ASSIGN dSaldo = dSaldo + ttMovto.qtMovto
               ttMovto.qtSaldoAcum = dSaldo.
/*         IF lContinuarMsg THEN                         */
/*         MESSAGE 'fora do break' SKIP                  */
/*                 'linha:' ttMovto.Id SKIP              */
/*                 'movto:' ttMovto.qtMovto SKIP         */
/*                 'saldo acum:' dSaldo                  */
/*             VIEW-AS ALERT-BOX INFORMATION BUTTONS OK. */

/*         IF lContinuarMsg THEN                          */
/*         MESSAGE "Continuar msg?" UPDATE lContinuarMsg  */
/*             VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO. */
        
    END.


END PROCEDURE.

PROCEDURE _gerarSaldoComp:

   FOR EACH ttMovto
       USE-INDEX ind-comp
       BREAK BY ttMovto.itCodigo
       BY ttMovto.codRefer
       BY ttmovto.ano
       BY ttMovto.mes:

       IF LAST-OF(ttMovto.Mes) THEN DO:
          CREATE ttComp.
          ASSIGN ttComp.itCodigo    = ttMovto.itCodigo
                 ttComp.descItem    = ttMovto.descItem
                 ttComp.codRefer    = ttMovto.codRefer
                 ttComp.ano         = ttMovto.ano
                 ttComp.mes         = ttMovto.mes
                 ttComp.qtSaldo     = ttMovto.qtSaldoAcum .
       END.

   END.

END PROCEDURE.


PROCEDURE _gerarSaldoData:

   FOR EACH ttMovto
       USE-INDEX ind-data
       BREAK BY ttMovto.itCodigo
       BY ttMovto.codRefer
       BY ttmovto.dtTrans:

       IF LAST-OF(ttMovto.dtTrans) THEN DO:
          CREATE ttData.
          ASSIGN ttData.itCodigo    = ttMovto.itCodigo
                 ttData.descItem    = ttMovto.descItem
                 ttData.codRefer    = ttMovto.codRefer
                 ttData.data        = ttMovto.dtTrans
                 ttData.qtSaldo     = ttMovto.qtSaldoAcum .
       END.

   END.

END PROCEDURE.
