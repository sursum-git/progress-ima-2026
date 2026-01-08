/*
Programa: esbo/boOrdemCompra.p
Autor: Tadeu Silva Parreiras
Altera‡Æo:17/05/2021 - InclusÆo do filtro de estabelecimento

*/

DEFINE VARIABLE datOrdemIni      AS DATE        NO-UNDO INIT 01.01.2001.
DEFINE VARIABLE datOrdemFim      AS DATE        NO-UNDO INIT 12.31.2999.
DEFINE VARIABLE dataEntregaIni   AS DATE        NO-UNDO INIT 01.01.2001.
DEFINE VARIABLE dataEntregaFim   AS DATE        NO-UNDO INIT 12.31.2999.
DEFINE VARIABLE dataVenctoIni    AS DATE        NO-UNDO INIT 01.01.2001.
DEFINE VARIABLE dataVenctoFim    AS DATE        NO-UNDO INIT 12.31.2999.
DEFINE VARIABLE sitOrdemIni      AS INTEGER     NO-UNDO INIT 0.
DEFINE VARIABLE sitOrdemFim      AS INTEGER     NO-UNDO INIT 9.
DEFINE VARIABLE numOrdemIni      AS INTEGER     NO-UNDO INIT 00000.
DEFINE VARIABLE numOrdemFim      AS INTEGER     NO-UNDO INIT 99999.
DEFINE VARIABLE numPedIni        AS INTEGER     NO-UNDO INIT 00000.
DEFINE VARIABLE numPedFim        AS INTEGER     NO-UNDO INIT 99999.
DEFINE VARIABLE codItemIni       AS CHAR        NO-UNDO INIT 0000000.
DEFINE VARIABLE codItemFim       AS CHAR        NO-UNDO INIT 9999999.
DEFINE VARIABLE codFornecIni     AS INTEGER     NO-UNDO INIT 00000.
DEFINE VARIABLE codFornecFim     AS INTEGER     NO-UNDO INIT 99999.
DEFINE VARIABLE lExportarDados   AS LOGICAL     NO-UNDO INIT NO.
DEFINE VARIABLE cEstabIni        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cEstabFim        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE i AS INTEGER     NO-UNDO.

{esbo\boOrdemCompra.i}
    
PROCEDURE retornarttPrazo:
DEFINE OUTPUT PARAMETER TABLE FOR ttPrazo.
END PROCEDURE.


PROCEDURE limparDados.
    EMPTY TEMP-TABLE ttDados.     
    EMPTY TEMP-TABLE ttPrazo. 

     

END PROCEDURE.

PROCEDURE definirEstab:
    DEFINE INPUT  PARAMETER pEstabIni AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pEstabFim AS CHARACTER   NO-UNDO.
    ASSIGN cEstabIni = pEstabIni
           cEstabFim = pEstabFim.

END PROCEDURE.



PROCEDURE definirSituacaoOrdem.
    DEFINE INPUT PARAMETER sitIni AS INT NO-UNDO.
    DEFINE INPUT PARAMETER sitFim AS INT NO-UNDO.
    ASSIGN sitOrdemIni = sitIni
           sitOrdemFim = sitFim.

END PROCEDURE.

PROCEDURE definirNumOrdem.
    DEFINE INPUT PARAMETER ordemIni AS INT NO-UNDO.
    DEFINE INPUT PARAMETER ordemFim AS INT NO-UNDO.
    ASSIGN numOrdemIni = ordemIni
           numOrdemFim = ordemFim.

END PROCEDURE.

PROCEDURE definirCodItem.
    DEFINE INPUT PARAMETER itIni AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER itFim AS CHAR NO-UNDO.
    ASSIGN codItemIni = itIni
           codItemFim = itFim.

END PROCEDURE.

PROCEDURE definirCodFornec.
    DEFINE INPUT PARAMETER fornecIni AS INT NO-UNDO.
    DEFINE INPUT PARAMETER fornecFim AS INT NO-UNDO.
    ASSIGN codFornecIni = fornecIni
           codFornecFim = fornecFim.

END PROCEDURE.


PROCEDURE definirNumPed.
    DEFINE INPUT PARAMETER pedIni AS INT NO-UNDO.
    DEFINE INPUT PARAMETER pedFim AS INT NO-UNDO.
    ASSIGN numPedIni = pedIni
           numPedFim = pedFim.

END PROCEDURE.

PROCEDURE definirDataOrdem:
   DEFINE INPUT  PARAMETER dtIni AS DATE        NO-UNDO.
   DEFINE INPUT  PARAMETER dtFim AS DATE        NO-UNDO.
   ASSIGN datOrdemIni = dtIni
          datOrdemFim = dtFim.


END PROCEDURE.

PROCEDURE definirDataEntrega:
   DEFINE INPUT  PARAMETER dtEnIni AS DATE        NO-UNDO.
   DEFINE INPUT  PARAMETER dtEnFim AS DATE        NO-UNDO.
   ASSIGN dataEntregaIni = dtEnIni
          dataEntregaFim = dtEnFim.


END PROCEDURE.     

PROCEDURE definirDataVencto:
   DEFINE INPUT  PARAMETER dtVEnIni AS DATE        NO-UNDO.
   DEFINE INPUT  PARAMETER dtVEnFim AS DATE        NO-UNDO.
   ASSIGN dataVenctoIni = dtVEnIni
          dataVenctoFim = dtVEnFim.


END PROCEDURE.     




PROCEDURE buscarOrdensCompra:
    DEFINE INPUT  PARAMETER lCalcCondPagto AS LOGICAL     NO-UNDO INIT NO.
    DEFINE VARIABLE dPreco AS DEC     NO-UNDO.
    FOR EACH ordem-compra WHERE
             ordem-compra.dat-ordem >= datOrdemIni AND
             ordem-compra.dat-ordem <= datOrdemFim AND
             ordem-compra.situacao >= sitOrdemIni AND
             ordem-compra.situacao <= sitOrdemFim AND
             ordem-compra.numero-ordem >= numOrdemIni AND
             ordem-compra.numero-ordem <= numOrdemFim AND
             ordem-compra.num-pedido >= numPedIni AND
             ordem-compra.num-pedido <= numPedFim AND
             ordem-compra.it-codigo >= codItemIni AND
             ordem-compra.it-codigo <= codItemFim AND
             ordem-compra.cod-emitente >= codFornecIni AND
             ordem-compra.cod-emitente <= codFornecFim AND
             ordem-compra.cod-estabel >= cEstabIni AND
             ordem-compra.cod-estabel <= cEstabFim NO-LOCK.
        
        IF lCalcCondPagto = YES THEN
           RUN calcCondPagto(ROWID(ordem-compra)).
        FOR EACH prazo-compr WHERE
                 prazo-compr.numero-ordem = ordem-compra.numero-ordem AND
                 prazo-compr.data-entrega >= dataEntregaIni AND
                 prazo-compr.data-entrega <= dataEntregaFim.
            FIND ITEM WHERE
             ITEM.it-codigo = ordem-compra.it-codigo NO-LOCK NO-ERROR.

            FIND emitente WHERE
                 emitente.cod-emitente = ordem-compra.cod-emitente NO-LOCK NO-ERROR.

            
            ASSIGN dPreco = ordem-compra.preco-unit * DEC (prazo-compr.quantidade).
            CREATE ttDados.
            ASSIGN ttDados.codEstab  = ordem-compra.cod-estabel
                   ttDados.numPedido = ordem-compra.num-pedido
                   ttDados.numOrdem  = ordem-compra.numero-ordem
                   ttDados.codigo = ordem-compra.it-codigo
                   ttDados.descItem = ITEM.desc-item
                   ttDados.fornec = ordem-compra.cod-emitente
                   ttDados.descFornec = emitente.nome-abrev
                   ttDados.situacao = {ininc/i02in274.i 4 ordem-compra.situacao}
                   ttDados.dataOrdem = ordem-compra.dat-ordem
                   ttDados.dataEntrega = prazo-compr.data-entrega
                   ttDados.precoUnit = ordem-compra.preco-unit
                   ttDados.quantidade = prazo-compr.quantidade
                   ttDados.preco = dPreco
                   ttDados.narrativa = ordem-compra.narrativa.
                   ttDados.narrativa = REPLACE(ttDados.narrativa, CHR(10),";").
           

        END.
    END. 
END PROCEDURE.

PROCEDURE calcCondPagto:
    DEFINE INPUT PARAMETER rOrdemCompra AS ROWID       NO-UNDO.
    DEFINE VARIABLE i AS INTEGER     NO-UNDO.
    DEFINE VARIABLE dPreco AS DEC     NO-UNDO.
    FIND ordem-compra
        WHERE ROWID(ordem-compra) = rOrdemCompra NO-LOCK NO-ERROR.
    IF AVAIL ordem-compra THEN DO:
        FIND FIRST pedido-compr OF ordem-compra NO-LOCK NO-ERROR.
        FIND FIRST cond-pagto OF pedido-compr NO-LOCK NO-ERROR.  
        IF AVAIL cond-pagto THEN DO:
           FOR EACH prazo-compr OF ordem-compra NO-LOCK.
               FIND ITEM WHERE
                    ITEM.it-codigo = ordem-compra.it-codigo NO-LOCK NO-ERROR.

               FIND emitente WHERE
                    emitente.cod-emitente = ordem-compra.cod-emitente NO-LOCK NO-ERROR.

               FIND tipo-rec-desp WHERE
                    tipo-rec-desp.tp-codigo = ordem-compra.tp-despesa NO-LOCK NO-ERROR.

               REPEAT i = 1 TO cond-pagto.num-parcelas:
                    ASSIGN dPreco = ordem-compra.preco-unit * DEC (prazo-compr.quantidade).
                    CREATE ttPrazo.
                    ASSIGN
                       ttPrazo.codEstab  = ordem-compra.cod-estabel  
                       ttPrazo.numPedido = ordem-compra.num-pedido
                       ttPrazo.numOrdem  = ordem-compra.numero-ordem
                       ttPrazo.codigo = ordem-compra.it-codigo
                       ttPrazo.descItem = ITEM.desc-item
                       ttPrazo.fornec = ordem-compra.cod-emitente
                       ttPrazo.descFornec = emitente.nome-abrev
                       ttPrazo.situacao = {ininc/i02in274.i 4 ordem-compra.situacao}
                       ttPrazo.dataOrdem = ordem-compra.dat-ordem
                       ttPrazo.dataEntrega = prazo-compr.data-entrega
                       ttPrazo.precoUnit = ordem-compra.preco-unit
                       ttPrazo.quantidade = prazo-compr.quantidade
                       ttPrazo.preco = dPreco * per-pg-dup[i] / 100
                       ttPrazo.narrativa = ordem-compra.narrativa
                       ttPrazo.narrativa = REPLACE(ttPrazo.narrativa, CHR(10),";")
                       ttPrazo.parcelas = i
                       ttPrazo.dtVenc = ttPrazo.dataEntrega + cond-pagto.prazos[i]
                       ttPrazo.codDespesa = ordem-compra.tp-despesa
                       ttPrazo.descDespesa = tipo-rec-desp.descricao.
              END.
           END.
        END.  
    END.
END.

PROCEDURE exportarDados.
    ASSIGN lExportarDados = YES.
    OUTPUT TO c:\temp\ordens-compra.txt.
    PUT "Num Pedido | Num Ordem | Cod Item | Desc Item | Fornecedor | Desc Fornec | Situa‡Æo | Data Ordem | Data Entrega | Pre‡o Unit | Quantidade | Valor Tot | Narrativa " SKIP.
    FOR EACH ttDados:
        EXPORT DELIMITER "|" ttDados.
    END. 
    OUTPUT CLOSE.

    OUTPUT TO c:\temp\ordem-compra-prazos.txt.
       PUT "Num Pedido | Num Ordem | Cod Item | Desc Item | Fornecedor | Desc Fornec | Situa‡Æo | Data Ordem | Data Entrega | Pre‡o Unit | Quantidade | Valor Tot | Narrativa | Parcela | Dt Venc | Cod. Despesa | Desc. Despesa. " SKIP.
       FOR EACH ttPrazo:
        EXPORT DELIMITER "|" ttPrazo.
    END. 
    OUTPUT CLOSE.
END PROCEDURE.

PROCEDURE exportarExcel.
    
    IF lExportarDados = NO THEN 
       RUN exportarDados.
    OS-COMMAND SILENT VALUE ("start excel /t t:\especificos\excel\ordens-compra.xlsx").
    OS-COMMAND SILENT VALUE ("start excel /t t:\especificos\excel\ordem-compra-prazos.xlsx").

END PROCEDURE.

PROCEDURE filtrarDtVencto:
    /*MESSAGE dataVenctoIni SKIP
            dataVenctoFim SKIP
        VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
    FOR EACH ttPrazo
        WHERE ttPrazo.dtVenc < dataVenctoIni
        OR   ttPrazo.dtVenc > dataVenctoFim:
        DELETE ttPrazo.
    END.
END PROCEDURE.







/*DEFINE VARIABLE iPreco AS INTEGER     NO-UNDO.



FOR EACH ordem-compra WHERE
         ordem-compra.dat-ordem >= 01.01.2016 AND
         ordem-compra.dat-ordem <= 01.01.2017 AND
         ordem-compra.situacao = 6.

    FOR EACH prazo-compr WHERE
             prazo-compra.numero-ordem = ordem-compra.numero-ordem.

ASSIGN iPreco = ordem-compra.preco-unit * prazo-compr.quantidade.
  

    EXPORT DELIMITER "|" ordem-compra.num-pedido
                         ordem-compra.it-codigo                       
                         ordem-compra.situacao
                         ordem-compra.dat-ordem
                         prazo-compr.quantidade
                         ordem-compra.preco-unit
                         iPreco.
         
         
         
    END.

END.    */                       
