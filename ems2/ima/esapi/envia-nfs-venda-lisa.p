//LOG-MANAGER:LOGGING-LEVEL = 5.
//LOG-MANAGER:LOGFILE-NAME = 'C:\temp\nota' + STRING(TIME) + '.txt'.
{esapi/envia-nfs-venda-lisa.i}



DEF BUFFER b-nota-fiscal  FOR nota-fiscal.




//DEF VAR h-handle  AS HANDLE.
DEFINE VARIABLE i-id      AS INT.
DEFINE VARIABLE c-arq-xml AS CHAR.
DEFINE VARIABLE c-arq-pdf AS CHAR.
/*DEF VAR lc-xml    AS LONGCHAR.
DEF VAR lc-pdf    AS LONGCHAR.*/

DEFINE INPUT PARAMETER p-row-di135  AS ROWID.
DEFINE INPUT PARAMETER pTrocaNF     AS LOGICAL  NO-UNDO.









RUN criarInfNotaVenda(INPUT p-row-di135,
                      INPUT 'aprovar',
                      '', //observacoes
                      pTrocaNF,
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
    DEFINE VARIABLE cErro           AS CHARACTER   NO-UNDO.
    

    ASSIGN h-dataset = DATASET dsNFe:HANDLE.

    /*RUN esbo/boAPIsLisa.p PERSIST SET h-handle.

    RUN iniciar IN h-handle.
    RUN retirarNoRoot IN h-handle(YES).
    RUN avaliarSeparacaoPedido IN h-handle (INPUT h-dataset,
                                            INPUT 'dataset',
                                            INPUT infNotaVenda.nota).

    RUN getStatusRetorno IN h-handle (OUTPUT iSitRet). 
    RUN getTTRetorno IN h-handle (OUTPUT TABLE ttJson).
    RUN getErro IN h-handle(OUTPUT cErro).
    RUN finalizar IN h-handle.
    */

    
    RUN lisa/enviarAvalSepPedVenda.p(INPUT h-dataset,
                                     INPUT infNotaVenda.nota,
                                     OUTPUT cErro).
    IF cErro <> '' THEN DO:
       MESSAGE 'ERRO ao Enviar NF para Armazem Geral' SKIP 
               'Comunique … TI os erros a seguir...' SKIP(2)
               cErro
           VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

       RETURN 'ADM-ERROR'.
    END.

    //enviar romaneio
    RUN lisa/enviarRomaneioPDF.p(ROWID(nota-fiscal),OUTPUT cErro).

    IF cErro <> '' THEN DO:
       MESSAGE 'ERRO ao Enviar Romaneio para Armazem Geral' SKIP 
               'Comunique … TI os erros a seguir...' SKIP(2)
               cErro
           VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

       RETURN 'ADM-ERROR'.
    END.                             
    
    // Localizar nota remessa
    FIND FIRST b-nota-fiscal WHERE
               b-nota-fiscal.nr-nota-fis >= nota-fiscal.nr-nota-fis AND
               b-nota-fiscal.cod-estabel = nota-fiscal.cod-estabel AND
               b-nota-fiscal.serie = nota-fiscal.serie AND
               b-nota-fiscal.nome-ab-cli = ped-venda.nome-abrev-tri AND
               b-nota-fiscal.vl-tot-nota = nota-fiscal.vl-tot-nota
               NO-ERROR. 

    IF AVAIL b-nota-fiscal THEN DO.
       CREATE infNotaRemessa.
       ASSIGN infNotaRemessa.ccodFilial =  cFilial
              infNotaRemessa.cpedido = ped-venda.nr-pedcli
              infNotaRemessa.ccgc = estabelec.cgc
              infNotaRemessa.ctipo = "Nota Triangular".

       ASSIGN c-arq-pdf = SESSION:TEMP-DIRECTORY + "FT0518" + b-nota-fiscal.nr-nota-fis + ".PDF".

       RUN pi-gera-danfe (INPUT ROWID(b-nota-fiscal) ).

       FILE-INFO:FILE-NAME = c-arq-pdf.
       IF FILE-INFO:FILE-NAME <> ? THEN DO.
          COPY-LOB FROM FILE c-arq-pdf TO infNotaRemessa.cdocumentoPDFbase64.
       END.

       RUN lisa/enviarNfTriangular.p (INPUT TEMP-TABLE infNotaRemessa:HANDLE,
                                      INPUT b-nota-fiscal.nr-nota-fis,
                                      OUTPUT cErro).
    END.
    
END PROCEDURE.


