/*****************************************************************************
Programa: esapi/envia-nfs-venda-lisa.p
objetivo: Enviar a nota fiscal de venda, remessa triangular e romameio
conforme rowid da nota fiscal de venda passo por parametro.
autor: Antonio Sousa
Data: 2023
Altera‡äes
Tadeu - nova versÆo - para casos onde a venda ‚ triangular tornar  dinamico
conforme o parametro pTpNfAprov qual nota fiscal ser  enviada como documento
e qual ser  como xml na aprova‡Æo do pedido.
pTpNfAprov = 'venda' -> Aprova com a nota fiscal de venda e a triangular fica
como documento.
pTpNfAprov = 'triangular' -> Aprova com a nota fiscal triangular e a 
de venda fica como documento.

***************************************************************************/


//LOG-MANAGER:LOGGING-LEVEL = 5.
//LOG-MANAGER:LOGFILE-NAME = 'C:\temp\nota' + STRING(TIME) + '.txt'.




DEF BUFFER b-nota-fiscal  FOR nota-fiscal.




//DEF VAR h-handle  AS HANDLE.
DEFINE VARIABLE i-id      AS INT.
DEFINE VARIABLE c-arq-xml AS CHAR.
DEFINE VARIABLE c-arq-pdf AS CHAR.
/*DEF VAR lc-xml    AS LONGCHAR.
DEF VAR lc-pdf    AS LONGCHAR.*/

DEFINE INPUT PARAMETER p-row-di135  AS ROWID.
DEFINE INPUT PARAMETER pTrocaNF     AS LOGICAL  NO-UNDO.
DEFINE INPUT PARAMETER pTpNFAprov   AS CHARACTER   NO-UNDO.


DEFINE VARIABLE rowidTriang AS ROWID       NO-UNDO.

DEFINE VARIABLE rowidAprova AS ROWID      NO-UNDO.
DEFINE VARIABLE rowidDocum  AS ROWID      NO-UNDO.

{esapi/envia-nfs-venda-lisa2.i}

RUN esapi/getNfTriang.p(INPUT p-row-di135,OUTPUT rowidTriang).

IF rowidTriang <> ? THEN DO:
   CASE pTpNfAProv:
       WHEN 'venda' THEN DO:
           ASSIGN rowidAprova = p-row-di135
                  rowidDocum = rowidTriang.
       END.
       WHEN 'triangular'  THEN DO:
           ASSIGN rowidDocum  = p-row-di135
                  rowidAprova = rowidTriang .
       END.
   END CASE.

END.
ELSE DO:
    ASSIGN rowidAprova = p-row-di135
           rowidDocum = rowidTriang .
END.
 
//a partir daqui modificar a logica para bodecer os rowids

RUN criarInfNotaVenda(INPUT rowidAprova,
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
    RUN lisa/enviarRomaneioPDF.p(p-row-di135,OUTPUT cErro).

    IF cErro <> '' THEN DO:
       MESSAGE 'ERRO ao Enviar Romaneio para Armazem Geral' SKIP 
               'Comunique … TI os erros a seguir...' SKIP(2)
               cErro
           VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

       RETURN 'ADM-ERROR'.
    END.                             
    
    // Localizar nota que ser  enviada como documento
    FIND FIRST b-nota-fiscal WHERE
               rowid(b-nota-fiscal) = rowidDocum NO-ERROR. 

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
      //com a mudan‡a a nota pode ser triangular ou a nota de venda, conforme o parametro tpNfAprov  
       RUN lisa/enviarNfTriangular.p (INPUT TEMP-TABLE infNotaRemessa:HANDLE,
                                      INPUT b-nota-fiscal.nr-nota-fis,
                                      OUTPUT cErro).
    END.
    
END PROCEDURE.


