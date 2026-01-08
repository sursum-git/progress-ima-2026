DEFINE VARIABLE iCont AS INTEGER     NO-UNDO.
DEFINE TEMP-TABLE ttImagens
      FIELD it-codigo AS CHAR FORMAT 'x(20)'
      FIELD cod-refer AS CHAR
      FIELD imagem    AS CHAR FORMAT 'X(100)'. 

DEFINE TEMP-TABLE ttEstoque
     FIELD it-codigo AS CHAR FORMAT 'x(20)'
     FIELD cod-refer AS CHAR
     FIELD saldo     AS DECIMAL
     FIELD cod-estab AS CHAR
     INDEX item-ref
      it-codigo cod-refer. 
DEFINE TEMP-TABLE ttLog
    FIELD dtHr   AS DATETIME
    FIELD imagem AS CHAR FORMAT 'x(200)'
    FIELD itCodigo AS CHAR FORMAT 'x(20)'
    FIELD codRefer AS CHAR 
    FIELD soma     AS DECIMAL
    FIELD moveu    AS LOGICAL.

DEFINE TEMP-TABLE ttSemImagem
        FIELD itCodigo AS CHAR FORMAT 'x(20)'
        FIELD codrefer AS CHAR
        FIELD qtEstoque AS DECIMAL.

DEFINE VARIABLE cDirSemSaldo AS CHARACTER NO-UNDO INITIAL '\\192.168.0.70\estampas\itens_sem_saldo'.
DEFINE VARIABLE cDirNaoLiberadas AS CHARACTER NO-UNDO INITIAL '\\192.168.0.70\estampas\itens_nao_liberados'.
DEFINE VARIABLE cDir        AS CHARACTER NO-UNDO INITIAL '\\192.168.0.70\estampas'.
DEFINE VARIABLE cArquivoCSV AS CHARACTER   NO-UNDO .
ASSIGN cArquivoCSV =  'c:\temp\logImagens_' + STRING(TIME) + '.csv'.

PROCEDURE buscarSaldo.
DEFINE INPUT  PARAMETER pItemIni AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pItemFim AS CHARACTER   NO-UNDO.
OUTPUT TO c:\temp\saldoestoqueima.txt.
FOR EACH ems2ima.saldo-estoq WHERE 
         ems2ima.saldo-estoq.it-codigo >= pItemIni AND 
         ems2ima.saldo-estoq.it-codigo <= pItemFim NO-LOCK.
    FIND FIRST ems2ima.ITEM OF ems2ima.saldo-estoq NO-LOCK
        WHERE ems2ima.ITEM.ind-item-fat = YES NO-ERROR.
    IF NOT AVAIL ems2ima.ITEM THEN NEXT.

    DISP ems2ima.saldo-estoq.it-codigo ems2ima.saldo-estoq.cod-refer ems2ima.saldo-estoq.qtidade-atu ems2ima.saldo-estoq.cod-estabel .

    CREATE ttEstoque.
    ASSIGN ttEstoque.it-codigo = ems2ima.saldo-estoq.it-codigo
           ttEstoque.cod-refer = ems2ima.saldo-estoq.cod-refer
           ttEstoque.saldo = ems2ima.saldo-estoq.qtidade-atu
           ttEstoque.cod-estab = ems2ima.saldo-estoq.cod-estab.

END.
OUTPUT CLOSE.
    
OUTPUT TO c:\temp\saldoestoquemed.txt.
FOR EACH dbaux.saldo-estoq WHERE 
         dbaux.saldo-estoq.it-codigo >= pItemIni AND 
         dbaux.saldo-estoq.it-codigo <= pItemFim NO-LOCK.
    FIND FIRST ems2med.ITEM OF ems2med.saldo-estoq NO-LOCK
        WHERE ems2med.ITEM.ind-item-fat = YES NO-ERROR.
    IF NOT AVAIL ems2med.ITEM THEN NEXT.

       DISP dbaux.saldo-estoq.it-codigo dbaux.saldo-estoq.cod-refer dbaux.saldo-estoq.qtidade-atu dbaux.saldo-estoq.cod-estabel. 

       CREATE ttEstoque.
       ASSIGN ttEstoque.it-codigo    = dbaux.saldo-estoq.it-codigo
              ttEstoque.cod-refer    = dbaux.saldo-estoq.cod-refer
              ttEstoque.saldo        = dbaux.saldo-estoq.qtidade-atu
              ttEstoque.cod-estab    = dbaux.saldo-estoq.cod-estab.
   
END.
OUTPUT CLOSE.

OUTPUT TO c:\temp\saldopi.txt.
FOR EACH pp-container  NO-LOCK WHERE 
         pp-container.situacao = 1: 
         FOR EACH pp-it-container OF pp-container
             WHERE pp-it-container.it-codigo >= pItemIni
             AND   pp-it-container.it-codigo <= pItemFim
             /*AND   pp-it-container.qt-vendida < pp-it-container.qt-pedida*/
             AND pp-it-container.qt-pedida > 0 NO-LOCK.

             CREATE ttEstoque.
             ASSIGN ttEstoque.it-codigo = pp-it-container.it-codigo
                    ttEstoque.cod-refer = pp-it-container.cod-refer
                    ttEstoque.saldo     = pp-it-container.qt-pedida /* - pp-it-container.qt-vendida*/
                    ttEstoque.cod-estab = pp-container.cod-estabel.
            DISP pp-it-container.it-codigo pp-it-container.cod-refer ttEstoque.saldo ttEstoque.cod-estab. 
         END.
END.
OUTPUT CLOSE.



END PROCEDURE.

PROCEDURE retornarSaldo.



END PROCEDURE.


PROCEDURE buscarImagens.

DEFINE INPUT  PARAMETER pDiretorio AS CHARACTER   NO-UNDO FORMAT 'X(200)'.
DEFINE VARIABLE cFileStream AS CHARACTER NO-UNDO FORMAT 'x(100)'.
INPUT FROM OS-DIR (pDiretorio) ECHO.
REPEAT:
    IMPORT cFileStream.
    CREATE ttImagens.
    ASSIGN ttImagens.it-codigo = SUBSTR(cFileStream,1,6)
           ttImagens.cod-refer = SUBSTR(cFileStream,9,4)
           ttImagens.cod-refer = TRIM(ttImagens.cod-refer)
           ttImagens.cod-refer = REPLACE(ttImagens.cod-refer,".","")
           ttImagens.imagem    = pDiretorio + "\" + cFileStream. 
END.

END PROCEDURE.

PROCEDURE inserirLog:

DEFINE INPUT  PARAMETER pImagem AS CHARACTER   NO-UNDO FORMAT 'x(200)'.
DEFINE INPUT  PARAMETER pItem   AS CHARACTER   NO-UNDO FORMAT 'x(20)'.
DEFINE INPUT  PARAMETER pRef    AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pSoma   AS DECIMAL     NO-UNDO.
DEFINE INPUT  PARAMETER pMoveu  AS LOGICAL     NO-UNDO.

CREATE ttLog.
ASSIGN ttLog.dtHr     = NOW
       ttLog.imagem   = pImagem
       ttLog.itCodigo = pItem
       ttLog.codRefer = pRef
       ttLog.soma     = pSoma
       ttLog.moveu    = pMoveu.

END.

PROCEDURE imprimirLog:
OUTPUT TO VALUE(cArquivoCSV).
PUT "Hora;Imagem;Item;Referencia;Saldo;Moveu?" SKIP.
FOR EACH ttLog.
    EXPORT DELIMITER ";" ttLog.
END.
OUTPUT CLOSE.

END PROCEDURE.

PROCEDURE buscarItensComSaldoSemImagem:
DEFINE VARIABLE cRef AS CHARACTER   NO-UNDO.

FOR EACH ttEstoque
    WHERE ttEstoque.saldo > 0
    AND   substr(ttEstoque.it-Codigo,3,1)  <> '5' // desconsiderar liso
           AND   ttEstoque.cod-Refer <> 'LED'
           AND   ttEstoque.cod-Refer <> 'LEG'
           AND   ttEstoque.cod-Refer <> '888'
           AND   ttEstoque.cod-Refer <> '999':
    FIND FIRST ttImagens
        WHERE ttImagens.it-codigo = ttEstoque.it-codigo
        AND   (   ttImagens.cod-refer = ttEstoque.cod-refer 
               OR "0" + ttImagens.cod-refer = ttEstoque.cod-refer) NO-LOCK NO-ERROR.
    IF NOT AVAIL ttImagens THEN DO:
       FIND FIRST ttSemImagem
           WHERE ttSemImagem.itCodigo = ttEstoque.it-codigo
           AND   ttSemImagem.codRefer = ttEstoque.cod-Refer
           
           NO-LOCK NO-ERROR.
       IF NOT AVAIL ttSemImagem THEN DO:
          CREATE ttSemImagem.
          ASSIGN ttSemImagem.itCodigo = ttEstoque.it-codigo
                 ttSemImagem.codRefer = ttEstoque.cod-refer.
       END. 
       ASSIGN ttSemImagem.qtEstoque = ttSemImagem.qtEstoque + ttEstoque.saldo.
    END.    
END.

END PROCEDURE.

PROCEDURE imprimirItensComSaldoSemImagem:

OUTPUT TO value(cArquivoCSV).
PUT "Item;Refer;Qt.Estoque" SKIP.
FOR EACH ttSemImagem:
    EXPORT DELIMITER ";" ttSemImagem.
END.
OUTPUT CLOSE.

END PROCEDURE.


PROCEDURE moverImagens.
DEFINE INPUT  PARAMETER pDiretorio AS CHARACTER   NO-UNDO FORMAT 'x(200)'.
DEFINE INPUT  PARAMETER lSoma  AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lMoveu AS LOGICAL     NO-UNDO.
DEFINE VARIABLE dSoma AS DECIMAL     NO-UNDO.
DEFINE VARIABLE cRef AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cListaExcecao AS CHARACTER   NO-UNDO.
ASSIGN dSoma = 0.


FOR EACH ttImagens.
    IF ttImagens.imagem = cDir + "\itens_sem_saldo"     THEN NEXT.
    IF ttImagens.imagem = cDir + "\branco.jpg"          THEN NEXT.
    IF ttImagens.imagem = cDir + "\Itens_nao_liberados" THEN NEXT.
    IF ttImagens.imagem = cDir + "\etiqueta_cores"      THEN NEXT.
    IF ttImagens.imagem = cDir + "\books"               THEN NEXT.


    IF ttImagens.imagem = cDir + "\." THEN NEXT.
    IF ttImagens.imagem = cDir + "\.." THEN NEXT.
    IF LENGTH (trim(ttImagens.cod-refer)) = 2 THEN
        ASSIGN cRef = "0" + ttImagens.cod-refer. 
    ELSE
       ASSIGN cRef = ttImagens.cod-refer.
    //PUT ttImagens.imagem ttImagens.it-codigo ttImagens.cod-refer cRef SKIP "Saldos:" SKIP.
    FOR EACH ttEstoque
        WHERE ttEstoque.it-codigo = ttImagens.it-codigo
        AND   ttEstoque.cod-refer = cRef
        USE-INDEX item-ref.
        //PUT ttEstoque.cod-estab ttEstoque.saldo SKIP.
        ASSIGN dSoma = dSoma + ttEstoque.saldo.
    END.

    /*EXPORT DELIMITER ";"  string(TIME,'hh:mm:ss') ttImagens.imagem ttImagens.it-codigo ttImagens.cod-refer dSoma .*/

    ASSIGN lMoveu = NO.
    IF dSoma = 0  AND lSoma = YES THEN DO: 
       ASSIGN lMoveu = YES.
       RUN moveImagem(ttImagens.imagem, pDiretorio).
    END.
       
    IF dSoma > 0  AND lSoma = NO THEN DO:
       ASSIGN lMoveu = YES.
       RUN moveImagem(ttImagens.imagem, pDiretorio).
    END.
    RUN inserirLog(ttImagens.imagem, ttImagens.it-codigo, ttImagens.cod-refer, dSoma, lMoveu).

    ASSIGN dSoma = 0.
END.
//OUTPUT CLOSE.
END PROCEDURE.


PROCEDURE buscarImagensEstampas:

RUN buscarImagens(cDir).

END PROCEDURE.

PROCEDURE buscarImagensSemSaldo:

RUN buscarImagens(cDirSemSaldo).

END PROCEDURE.

PROCEDURE buscarImagensNaoLiberadas:

RUN buscarImagens(cDirNaoLiberadas).

END PROCEDURE.

PROCEDURE getDiretorioEstampas:
DEFINE OUTPUT PARAMETER pDiretorio AS CHARACTER   NO-UNDO.
ASSIGN pDiretorio = cDir.
END.

PROCEDURE getDiretorioItensSemSaldo:
DEFINE OUTPUT PARAMETER pDiretorio AS CHARACTER   NO-UNDO.
ASSIGN pDiretorio = cDirSemSaldo.
END.

PROCEDURE getDiretorioItensNaoLiberados:

DEFINE OUTPUT PARAMETER pDiretorio AS CHARACTER   NO-UNDO.
ASSIGN pDiretorio = cDirNaoLiberadas.

END.



PROCEDURE moverImagensSemSaldo.

RUN moverImagens(cDirSemSaldo,YES).

END PROCEDURE.


PROCEDURE moverImagensComSaldo.

RUN moverImagens(cDir,NO).

END PROCEDURE.


PROCEDURE moverImagensNaoLiberadas:

RUN moverImagens(cDir,NO).

END PROCEDURE.


PROCEDURE exibirLog:

OS-COMMAND SILENT VALUE('start excel ' + cArquivoCSV).

END PROCEDURE.


PROCEDURE MOVEImagem:

DEFINE INPUT  PARAMETER pImagem LIKE ttImagens.imagem   NO-UNDO.
DEFINE INPUT  PARAMETER pCaminho AS CHARACTER FORMAT "x(200)"  NO-UNDO.
DEFINE VARIABLE cComando AS CHARACTER   NO-UNDO.
ASSIGN cComando = 'move "' + pImagem + '" ' + pCaminho.
OS-COMMAND SILENT VALUE(cComando).

END.

PROCEDURE limparTTs:

EMPTY TEMP-TABLE ttLog.
EMPTY TEMP-TABLE ttImagens.
EMPTY TEMP-TABLE ttEstoque.
EMPTY TEMP-TABLE ttSemImagem.


END PROCEDURE.
