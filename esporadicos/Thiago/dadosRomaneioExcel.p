DEFINE VARIABLE chExcelApp      AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorkBook      AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorkSheet     AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE i-linha         AS INTEGER    NO-UNDO.
DEFINE VARIABLE nomeEmpresa     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE h-acomp         AS HANDLE      NO-UNDO.
DEFINE VARIABLE qtLinhasItem    AS INTEGER     NO-UNDO.
DEFINE VARIABLE qtLinhaIni      AS INTEGER     NO-UNDO INIT 10.
DEFINE VARIABLE qtLinhaPg       AS INTEGER     NO-UNDO.
DEFINE VARIABLE qtMaxPg         AS INTEGER     NO-UNDO INIT 49.
DEFINE VARIABLE cArquivo        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cArqPDF         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE qtPecasItem     AS INT         NO-UNDO.
DEFINE VARIABLE descPecasItem   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE totLinhasItens  AS INTEGER     NO-UNDO.
DEFINE VARIABLE totGeralLinhas  AS INTEGER     NO-UNDO.
//DEFINE INPUT  PARAMETER pCodEstabel AS CHARACTER   NO-UNDO.
//DEFINE INPUT  PARAMETER pSerie      AS CHARACTER   NO-UNDO.
//DEFINE INPUT  PARAMETER pNF         AS CHARACTER   NO-UNDO.
//DEFINE OUTPUT PARAMETER cArqPDF     AS CHARACTER   NO-UNDO.


{esapi/extrairDadosRomaneio.i}

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
RUN pi-inicializar IN h-acomp (INPUT "Processando..").


//RUN esapi/extrairDadosRomaneio.p(pCodEstabel,int(pSerie),pNF,OUTPUT TABLE tt-romaneio,OUTPUT TABLE tt-itens-romaneio,OUTPUT TABLE tt-dados-itens-romaneio).
RUN esapi/extrairDadosRomaneio.p('505',2,'0001706',OUTPUT TABLE tt-romaneio,OUTPUT TABLE tt-itens-romaneio,OUTPUT TABLE tt-dados-itens-romaneio).


RUN pi-acompanhar IN h-acomp (INPUT "Gerando PDF:").
FIND FIRST tt-romaneio NO-LOCK NO-ERROR.
IF AVAIL tt-romaneio THEN DO:
    CREATE "excel.application" chExcelApp.
    chExcelApp:VISIBLE = FALSE. 

    //chWorkBook = chExcelApp:Workbooks:ADD().
    ASSIGN cArquivo = SEARCH("modelo-xlt/romaneio.xltx").
    IF cArquivo = ? THEN DO:
       MESSAGE "Arquivo De Modelo modelo-xlt/romaneio.xltx nÆo encontrado"
           VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
       RETURN 'nok'.
    END.
    chWorkBook = chExcelApp:Workbooks:OPEN(cArquivo).
    chWorkSheet = chExcelApp:Sheets:Item(1).

    chWorkbook:Worksheets(1):activate.
    chExcelApp:ActiveWindow:Zoom = 100.

    chWorkSheet:PageSetup:PrintTitleRows = "$1:$9" .

    /* Configura a Linha do Titulo da Planilha */
/*     ASSIGN chWorkSheet:Rows("1:1"):RowHeight = 30     */
/*            chWorkSheet:Rows("1:1"):FONT:SIZE = 13     */
/*            chWorkSheet:Rows("1:1"):FONT:bold = FALSE. */

    IF tt-romaneio.codEstabel = '505'THEN DO: 
        ASSIGN nomeEmpresa = 'MEDTEXTIL ITAJAÖ'.
    END.
    ELSE 
        ASSIGN nomeEmpresa = 'MEDTEXTIL VILA VELHA'.

    ASSIGN chworksheet:range("A1"):VALUE = "R O M A N E I O    D E    E M B A R Q U E  - " + nomeEmpresa + FILL(" ",20).

/*     /* Configura Alinhamento Horizontal do Titulo da Planilha */           */
/*     ChWorkSheet:range("A1:D1"):SELECT().                                   */
/*     ChWorksheet:range("A1:D1"):Merge.                                      */
/*     Chworksheet:Range("A1:D1"):HorizontalAlignment = 3. /* Centralizado */ */
/*     Chworksheet:Range("A1:D1"):VerticalAlignment   = 2. /* Centralizado */ */
/*                                                                            */
/*     /* Colorir Titulo da Planilha */                                       */
/*     chWorkSheet:Range("A1:D1"):FONT:ColorIndex     = 18. /* Avermelhado */ */
/*     chWorkSheet:Range("A1:D1"):Interior:ColorIndex = 2.  /* Branco */      */
/*                                                                            */
/*     /* Titulo das Colunas */                                               */
/*     ASSIGN chWorkSheet:Range("A3"):VALUE = "PEDIDO"                        */
/*            chWorkSheet:Range("B3"):VALUE = "NOTA FISCAL"                   */
/*            chWorkSheet:Range("C3"):VALUE = "CLIENTE"                       */
/*            chWorkSheet:Range("D3"):VALUE = "TRANSPORTADORA"                */
/*            chWorkSheet:Range("A5"):VALUE = "QTDE VOLUMES"                  */
/*            chWorkSheet:Range("B5"):VALUE = "QTDE PE€AS"                    */
/*            chWorkSheet:Range("C5"):VALUE = "PESO TOTAL"                    */
/*            chWorkSheet:Range("D5"):VALUE = "QTDE TOTAL".                   */
/*                                                                            */
/*     ASSIGN chworksheet:range("J:J"):ShrinkToFit = TRUE.                    */

    
    /* Ajustar o Tamanho Dentro da Celula */     
    ASSIGN chWorkSheet:COLUMNS("A"):ColumnWidth           = 10.
    ASSIGN chWorkSheet:COLUMNS("B"):ColumnWidth           = 15.
    ASSIGN chWorkSheet:COLUMNS("C"):ColumnWidth           = 25.
    ASSIGN chWorkSheet:COLUMNS("D"):ColumnWidth           = 19.
    /*ASSIGN chWorkSheet:COLUMNS("E"):ColumnWidth           = 17.
    ASSIGN chWorkSheet:COLUMNS("F"):ColumnWidth           = 14.
    ASSIGN chWorkSheet:COLUMNS("G"):ColumnWidth           = 14.
    ASSIGN chWorkSheet:COLUMNS("H"):ColumnWidth           = 14.*/
    
    

    /* Configura as Colunas da Planilha */
/*     ASSIGN chworksheet:range("A:C"):NumberFormat        = "@"               */
/*            chworksheet:range("E:I"):NumberFormat        = "@"               */
/*            chworksheet:range("J:K"):NumberFormat        = "###.###.##0,00". */
/*                                                                             */

    ASSIGN Chworksheet:range("H:H"):HorizontalAlignment = 4
           Chworksheet:range("I:K"):HorizontalAlignment = 4.

    /* Configura Cabe»alho das Colunas */
/*     chWorkSheet:Range("A3:D3"):SELECT().                             */
/*     ASSIGN chExcelApp:SELECTION:FONT:NAME               = "Consolas" */
/*            chExcelApp:SELECTION:FONT:SIZE               = 12         */
/*            chExcelApp:SELECTION:FONT:Bold               = FALSE      */
/*            chExcelApp:SELECTION:Interior:ColorIndex     = 37         */
/*            chExcelApp:SELECTION:FONT:ColorIndex         = 11.        */
/*                                                                      */
/*     chWorkSheet:Range("A5:D5"):SELECT().                             */
/*     ASSIGN chExcelApp:SELECTION:FONT:NAME               = "Consolas" */
/*            chExcelApp:SELECTION:FONT:SIZE               = 12         */
/*            chExcelApp:SELECTION:FONT:Bold               = FALSE      */
/*            chExcelApp:SELECTION:Interior:ColorIndex     = 37         */
/*            chExcelApp:SELECTION:FONT:ColorIndex         = 11.        */

    ASSIGN i-linha = 3.
    FOR EACH tt-romaneio NO-LOCK  
             BREAK BY tt-romaneio.notaFiscal.
        
       ASSIGN i-linha = i-linha + 1.

       ASSIGN chWorkSheet:Range("A" + STRING(i-linha)):VALUE = tt-romaneio.cPedido
              chWorkSheet:Range("B" + STRING(i-linha)):VALUE = tt-romaneio.notaFiscal
              chWorkSheet:Range("C" + STRING(i-linha)):VALUE = tt-romaneio.cNomeAbrev
              chWorkSheet:Range("D" + STRING(i-linha)):VALUE = tt-romaneio.cTransp.

      ASSIGN  i-linha = 6.

      ASSIGN  chWorkSheet:Range("A" + STRING(i-linha)):VALUE = tt-romaneio.totVolume
              chWorkSheet:Range("B" + STRING(i-linha)):VALUE = tt-romaneio.totEtq
              chWorkSheet:Range("C" + STRING(i-linha)):VALUE = tt-romaneio.totPeso
              chWorkSheet:Range("D" + STRING(i-linha)):VALUE = tt-romaneio.totQtd.
     
    END.

   

    ASSIGN chworksheet:range("A8"):VALUE = "D E S C R I € Ç O    D O S    V O L U M E S" + FILL(" ",20).

    /* Configura Alinhamento Horizontal do Titulo da Planilha */
    ChWorkSheet:range("A8:D8"):SELECT().
    ChWorksheet:range("A8:D8"):Merge.
    Chworksheet:Range("A8:D8"):HorizontalAlignment = 3. /* Centralizado */
    Chworksheet:Range("A8:D8"):VerticalAlignment   = 2. /* Centralizado */

    /* Colorir Titulo da Planilha */
    chWorkSheet:Range("A8:D8"):FONT:ColorIndex     = 18. /* Avermelhado */
    chWorkSheet:Range("A8:D8"):Interior:ColorIndex = 2.  /* Branco */

    ASSIGN i-linha = qtLinhaIni
           qtLinhaPg = qtLinhaIni.
    //ASSIGN qtLinhasItem = 10.
    
    FOR EACH tt-dados-itens-romaneio NO-LOCK  .
    //////////
       ASSIGN qtLinhasItem = 0.
       ASSIGN qtPecasItem  = 0.
       FOR EACH tt-itens-romaneio
           WHERE tt-itens-romaneio.iditem = tt-dados-itens-romaneio.id .
           ASSIGN qtLinhasItem = qtLinhasItem + 1 .
           ASSIGN qtPecasItem  = qtPecasItem + 1.
       END.
       IF qtPecasItem > 1 THEN ASSIGN descPecasItem = "Pe‡as". ELSE ASSIGN descPecasItem = "Pe‡a".



/*        ASSIGN totGeralLinhas = qtMaxPg - qtLinhaPg -  qtLinhasItem - 2.                       */
/*        ASSIGN totLinhasItens = qtLinhasItem .                                                 */
/*        IF totGeralLinhas < 0 THEN DO:                                                         */
/*           IF totLinhasItens < 49 THEN ChWorkSheet:Range("A" + STRING(i-linha)):pageBreak = 1. */
/*           ASSIGN qtLinhaPg = qtLinhaIni.                                                      */
/*                                                                                               */
/*        END.                                                                                   */
       ///////////////
       //aplicar formata‡Æo da linha 10 e 11.excelappl:worksheets:item(2):SELECT.
       RUN copiarFormatacao('item',i-linha).
       RUN impCabecalho('item',i-linha).
       RUN incrLinha.
       RUN copiarFormatacao('cab',i-linha).
       RUN impCabecalho('cab',i-linha). 
       RUN incrLinha.
       
       FOR EACH tt-itens-romaneio 
           WHERE tt-itens-romaneio.iditem = tt-dados-itens-romaneio.id NO-LOCK.
           
/*            MESSAGE "num.etq:" tt-itens-romaneio.numEtq   */
/*                    "linha:" i-linha                      */
/*                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK. */
           ASSIGN 
               chWorkSheet:Range("A" + STRING(i-linha)):VALUE = tt-itens-romaneio.numEtq
               chWorkSheet:Range("B" + STRING(i-linha)):VALUE = tt-itens-romaneio.qtidade
               chWorkSheet:Range("C" + STRING(i-linha)):VALUE = tt-itens-romaneio.peso.
          RUN incrLinha.                                                               
       END.                                                                            
       RUN incrLinha. 
    END.

    i-linha  = i-linha   + 2.
    ASSIGN chWorkSheet:Range("A" + STRING(i-linha)):VALUE = "IMPORTANTE!!".
    i-linha  = i-linha   + 1.
    ASSIGN chWorkSheet:Range("A" + STRING(i-linha)):VALUE = "AO RECEBER A MERCADORIA, FAVOR CONFERIR O TOTAL DE VOLUMES E A QUANTIDADE DE PE€AS.".
    i-linha  = i-linha   + 1.
           chWorkSheet:Range("A" + STRING(i-linha)):VALUE = "CASO HAJA A VIOLA€ÇO DO VOLUME, COM FALTA DE PE€AS3".
    i-linha  = i-linha   + 1.
            chWorkSheet:Range("A" + STRING(i-linha)):VALUE = "ENTRE CONTATO COM A IMATEXTIL (031)32383100.".
    i-linha  = i-linha   + 1.
           chWorkSheet:Range("A" + STRING(i-linha)):VALUE = "*** EM CASO DE RECLAMA€ÇO, FAVOR DEVOLVER A ETIQUETA DO VOLUME ***".
    //chExcelApp:VISIBLE = TRUE.
    ASSIGN cArqPDF = "c:\temp\romaneio_" + tt-romaneio.cPedido + '_' + STRING(TIME) + ".pdf" .
    chExcelapp:ActiveSheet:ExportAsFixedFormat(0,cArqPDF,0,true,,,,,).
    OS-COMMAND SILENT VALUE("start " + cArqPDF).

    RELEASE OBJECT chWorkSheet.
    RELEASE OBJECT chWorkBook.
    RELEASE OBJECT chExcelApp.
END.
ELSE DO:
    MESSAGE "NÆo h  registros nesse periodo"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

    
RUN pi-finalizar in h-acomp.

PROCEDURE incrLinha:

 ASSIGN i-linha  = i-linha   + 1
       qtLinhaPg = qtLinhaPg + 1.

END PROCEDURE.



PROCEDURE impCabecalho:

    DEFINE INPUT  PARAMETER pTipo  AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER iLinha AS INTEGER     NO-UNDO.

    DEFINE VARIABLE iLinhaCp AS INTEGER     NO-UNDO.
    
  
    CASE pTipo:
        WHEN 'item' THEN DO:
              ASSIGN   chworksheet:range("A"+ STRING(i-linha)):VALUE = tt-dados-itens-romaneio.descItem + " - " + tt-dados-itens-romaneio.itCodigo +
              " - " + tt-dados-itens-romaneio.codRefer + " - " + string(qtPecasItem) + " " +  descPecasItem.
        END.
        
           
        WHEN 'cab' THEN
          ASSIGN  chWorkSheet:Range("A" + STRING(i-linha)):VALUE = "NUM ROLO"
                  chWorkSheet:Range("B" + STRING(i-linha)):VALUE = "QUANTIDADE"
                  chWorkSheet:Range("C" + STRING(i-linha)):VALUE = "PESO".
    END CASE.


    

END PROCEDURE.



PROCEDURE copiarFormatacao:

    DEFINE INPUT  PARAMETER pTipo  AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER iLinha AS INTEGER     NO-UNDO.

    DEFINE VARIABLE iLinhaCp AS INTEGER     NO-UNDO.
    
    CASE pTipo:
        WHEN 'item' THEN 
             chExcelApp:Range("A10:A10"):Select.
        WHEN 'cab' THEN
            chExcelApp:Range("A11:C11"):Select.
    END CASE.

   
    chExcelApp:Selection:Copy.
    chExcelApp:worksheets:item(1):SELECT.
    chExcelApp:range("A" + string(iLinha, "99999")):SELECT.
    chExcelApp:ActiveSheet:Paste. 

END PROCEDURE.


