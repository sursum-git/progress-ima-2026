DEFINE VARIABLE datOrdemIni      AS DATE        NO-UNDO INIT 01.01.2001.
DEFINE VARIABLE datOrdemFim      AS DATE        NO-UNDO INIT 12.31.2999.
DEFINE VARIABLE dataEntregaIni   AS DATE        NO-UNDO INIT 01.01.2001.
DEFINE VARIABLE dataEntregaFim   AS DATE        NO-UNDO INIT 12.31.2999.

DEFINE TEMP-TABLE ttDados
    FIELD numPedido  LIKE ordem-compra.num-pedido
    FIELD codigo     LIKE ordem-compra.it-codigo
    FIELD situacao   LIKE ordem-compra.situacao
    FIELD dataOrdem  LIKE ordem-compra.dat-ordem
    FIELD quantidade LIKE prazo-compr.quantidade
    FIELD preco      AS   DEC.


PROCEDURE limparDados.
    EMPTY TEMP-TABLE ttDados.


END PROCEDURE.

PROCEDURE definirDataOrdem:
   DEFINE INPUT  PARAMETER dtIni AS DATE        NO-UNDO.
   DEFINE INPUT  PARAMETER dtFim AS DATE        NO-UNDO.
   ASSIGN datOrdemIni = dtIni
          datOrdemFim = dtFim.


END PROCEDURE.     


PROCEDURE buscarOrdensCompra:
    DEFINE VARIABLE iPreco AS INTEGER     NO-UNDO.
    FOR EACH ordem-compra WHERE
             ordem-compra.dat-ordem >= datOrdemIni AND
             ordem-compra.dat-ordem <= datOrdemFim.

        FOR EACH prazo-compr WHERE
                 prazo-compra.numero-ordem = ordem-compra.numero-ordem.
            ASSIGN iPreco = ordem-compra.preco-unit * prazo-compr.quantidade.
            CREATE ttDados.
            ASSIGN ttDados.numPedido = ordem-compra.num-pedido.

        END.
    END. 
END PROCEDURE.

PROCEDURE exportarDados.
OUTPUT TO c:\temp\ped-compra.txt.
PUT "Num Pedido | Cod Item | Situa‡Æo | Data Ordem | Pre‡o Unit rio | Quantidade | Valor Tot " SKIP.
FOR EACH ttDados:
    EXPORT DELIMITER "|" ttDados.
END. 
OUTPUT CLOSE.

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
