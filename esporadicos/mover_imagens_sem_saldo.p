DEFINE VARIABLE iCont AS INTEGER     NO-UNDO.
DEFINE TEMP-TABLE ttImagens
      FIELD it-codigo AS CHAR FORMAT 'x(20)'
      FIELD cod-refer AS CHAR
      FIELD imagem    AS CHAR FORMAT 'X(100)'. 

DEFINE TEMP-TABLE ttEstoque
     FIELD it-codigo AS CHAR FORMAT 'x(20)'
     FIELD cod-refer AS CHAR
     FIELD saldo     AS DECIMAL
     FIELD cod-estab AS CHAR. 

DEFINE VARIABLE cDir        AS CHARACTER NO-UNDO INITIAL '\\192.168.0.70\estampas'.
DEFINE VARIABLE cFileStream AS CHARACTER NO-UNDO FORMAT 'x(100)'.
DEFINE VARIABLE dSoma AS DECIMAL     NO-UNDO.

INPUT FROM OS-DIR (cDir) ECHO.

OUTPUT TO c:\temp\saldoestoqueima.txt.
FOR EACH ems2ima.saldo-estoq NO-LOCK.

    DISP ems2ima.saldo-estoq.it-codigo ems2ima.saldo-estoq.cod-refer.

    CREATE ttEstoque.
    ASSIGN ttEstoque.it-codigo = ems2ima.saldo-estoq.it-codigo
           ttEstoque.cod-refer = ems2ima.saldo-estoq.cod-refer
           ttEstoque.saldo = ems2ima.saldo-estoq.qtidade-atu
           ttEstoque.cod-estab = ems2ima.saldo-estoq.cod-estab.

END.
OUTPUT CLOSE.
    
OUTPUT TO c:\temp\saldoestoquemed.txt.
FOR EACH dbaux.saldo-estoq NO-LOCK.

       DISP dbaux.saldo-estoq.it-codigo dbaux.saldo-estoq.cod-refer. 

       CREATE ttEstoque.
       ASSIGN ttEstoque.it-codigo    = dbaux.saldo-estoq.it-codigo
              ttEstoque.cod-refer    = dbaux.saldo-estoq.cod-refer
              ttEstoque.saldo        = dbaux.saldo-estoq.qtidade-atu
              ttEstoque.cod-estab    = dbaux.saldo-estoq.cod-estab.
   
END.
OUTPUT CLOSE.

REPEAT:
    IMPORT cFileStream.
    CREATE ttImagens.
    ASSIGN ttImagens.it-codigo = SUBSTR(cFileStream,1,6)
           ttImagens.cod-refer = SUBSTR(cFileStream,10,3)
           ttImagens.imagem    = cDir + "\" + cFileStream. 
END.
ASSIGN dSoma = 0.
OUTPUT TO c:\temp\saldo.txt.
FOR EACH ttImagens.
    
    FOR EACH ttEstoque
        WHERE ttEstoque.it-codigo = ttImagens.it-codigo
        AND   ttEstoque.cod-refer = ttImagens.cod-refer.
        ASSIGN dSoma = dSoma + ttEstoque.saldo.
    END.

    DISP ttImagens.it-codigo ttImagens.cod-refer dSoma WITH  WIDTH 550.

    IF dSoma = 0 THEN 
       RUN moveImagem(ttImagens.imagem).
    ASSIGN dSoma = 0.
END.
OUTPUT CLOSE.


PROCEDURE MOVEImagem:
    DEFINE INPUT  PARAMETER pImagem LIKE ttImagens.imagem   NO-UNDO.
    DEFINE VARIABLE cComando AS CHARACTER   NO-UNDO.
    ASSIGN cComando = 'move "' + pImagem + '" \\192.168.0.70\estampas\itens_sem_saldo'.
    OS-COMMAND SILENT VALUE(cComando).
END.
