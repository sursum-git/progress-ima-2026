
//LOG-MANAGER:LOGGING-LEVEL = 5.
//LOG-MANAGER:LOGFILE-NAME = 'C:\temp\nota' + STRING(TIME) + '.txt'.
{esapi/envia-nfs-venda-lisa.i}



//DEF VAR h-handle  AS HANDLE.
DEFINE VARIABLE i-id      AS INT.
DEFINE VARIABLE c-arq-xml AS CHAR.
DEFINE VARIABLE c-arq-pdf AS CHAR.
/*DEF VAR lc-xml    AS LONGCHAR.
DEF VAR lc-pdf    AS LONGCHAR.*/

DEFINE INPUT PARAMETER  pRowidPedVenda   AS ROWID.
DEFINE INPUT PARAMETER  pObservacao      AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER cErro AS CHARACTER   NO-UNDO.

RUN criarInfNotaVenda(INPUT pRowidPedVenda,
                      INPUT 'reprovar',
                      INPUT pObservacao,
                      INPUT NO, //n∆o Ç troca 
                      OUTPUT i-id
                      ).

DEF DATASET dsNFe SERIALIZE-HIDDEN FOR infNotaVenda.

RUN pi-chama-api.
IF RETURN-VALUE = 'ADM-ERROR' THEN
   RETURN 'ADM-ERROR'.

RETURN 'ADM-OK'.


//-------------- Procedures
PROCEDURE pi-chama-api.

    DEF VAR h-dataset               AS HANDLE.
    DEF VAR iSitRet                 AS INTEGER.

    ASSIGN h-dataset = DATASET dsNFe:HANDLE.
    FIND FIRST infNotaVenda NO-ERROR.
    
    RUN lisa/enviarAvalSepPedVenda.p(INPUT h-dataset,
                                     IF infNotaVenda.nota = '' THEN infNotaVenda.nota ELSE infNotaVenda.pedidoCliente,
                                     OUTPUT cErro).
    IF cErro <> '' THEN DO:
       ASSIGN cErro = 'ERRO ao Enviar Reprovaá∆o do Pedido para Armazem Geral Comunique Ö TI os erros a seguir...'
                      + chr(13) + cErro.
       RETURN 'ADM-ERROR'.
    END.
    
    
END PROCEDURE.
