DEFINE VARIABLE chExcelApp      AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorkBook      AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorkSheet     AS COM-HANDLE NO-UNDO.

DEFINE INPUT  PARAMETER pCodEstabel AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pSerie      AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pNF         AS CHARACTER   NO-UNDO.

//for‡a a gera‡Æo de novo romaneio ao inves de tentar buscar um j  existente
DEFINE INPUT  PARAMETER logNovo         AS LOGICAL     NO-UNDO.
DEFINE INPUT  PARAMETER logConvertPDF   AS LOGICAL     NO-UNDO.

DEFINE OUTPUT PARAMETER cArq     AS CHARACTER   NO-UNDO.

//variaveis locais
DEFINE VARIABLE i-linha         AS INTEGER     NO-UNDO.
DEFINE VARIABLE nomeEmpresa     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE h-acomp         AS HANDLE      NO-UNDO.
DEFINE VARIABLE qtLinhasItem    AS INTEGER     NO-UNDO.
DEFINE VARIABLE qtLinhaIni      AS INTEGER     NO-UNDO INIT 10.
DEFINE VARIABLE qtLinhaPg       AS INTEGER     NO-UNDO.
DEFINE VARIABLE qtMaxPg         AS INTEGER     NO-UNDO INIT 49.
DEFINE VARIABLE cArquivo        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE qtPecasItem     AS INT         NO-UNDO.
DEFINE VARIABLE descPecasItem   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE totLinhasItens  AS INTEGER     NO-UNDO.
DEFINE VARIABLE totGeralLinhas  AS INTEGER     NO-UNDO.
DEFINE VARIABLE hBoParam        AS HANDLE      NO-UNDO.
DEFINE VARIABLE cDirRomaneio    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cExtensaoArq   AS CHARACTER   NO-UNDO.
{esapi/extrairDadosRomaneio.i}
RUN esbo/boConsParam.p PERSIST SET hBoParam NO-ERROR.
RUN utp/ut-acomp.p PERSISTENT SET h-acomp NO-ERROR.
RUN pi-inicializar IN h-acomp (INPUT "Processando..") NO-ERROR.
RUN getDirRomaneioPDF IN hBoParam (pCodEstabel,OUTPUT cDirRomaneio) NO-ERROR.

FIND nota-fiscal NO-LOCK
    WHERE nota-fiscal.cod-estabel = pCodEstabel
    AND   nota-fiscal.serie       = pSerie
    AND   nota-fiscal.nr-nota-fis = pNF
    NO-ERROR.
IF NOT AVAIL nota-fiscal THEN 
   RETURN 'nok'.
IF logConvertPDF THEN
   ASSIGN cExtensaoArq = '.pdf'.
ELSE
   ASSIGN cExtensaoArq = '.xlsx'.


ASSIGN cArq = cDirRomaneio + "\" + nota-fiscal.nr-pedcli + cExtensaoArq  NO-ERROR.

IF SEARCH(cArq) <> ? AND logNovo = NO THEN DO:
   RUN pi-finalizar IN h-acomp NO-ERROR.
   RETURN 'ok'.
END.
   

RUN esapi/extrairDadosRomaneio.p(pCodEstabel,int(pSerie),pNF,OUTPUT TABLE tt-romaneio,OUTPUT TABLE tt-itens-romaneio,OUTPUT TABLE tt-dados-itens-romaneio).
//RUN esapi/extrairDadosRomaneio.p('5',3,'0167456',OUTPUT TABLE tt-romaneio,OUTPUT TABLE tt-itens-romaneio,OUTPUT TABLE tt-dados-itens-romaneio).



RUN pi-acompanhar IN h-acomp (INPUT "Gerando PDF:") NO-ERROR.
FIND FIRST tt-romaneio NO-LOCK NO-ERROR.
IF AVAIL tt-romaneio THEN DO:
    CREATE "excel.application" chExcelApp NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
       RETURN 'nok'.
    chExcelApp:VISIBLE = FALSE NO-ERROR. 
    //chExcelApp:VISIBLE = TRUE.
    //chWorkBook = chExcelApp:Workbooks:ADD().
    ASSIGN cArquivo = SEARCH("modelo-xlt/romaneio.xltx") NO-ERROR.
    IF cArquivo = ? THEN DO:
       MESSAGE "Arquivo De Modelo modelo-xlt/romaneio.xltx nÆo encontrado"
           VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
       RETURN 'nok'.
    END.
    
    chWorkBook = chExcelApp:Workbooks:OPEN(cArquivo) NO-ERROR.
    chWorkSheet = chExcelApp:Sheets:Item(1) NO-ERROR.

    chWorkbook:Worksheets(1):activate NO-ERROR.
    chExcelApp:ActiveWindow:Zoom = 100 NO-ERROR.

    chWorkSheet:PageSetup:PrintTitleRows = "$1:$9"  NO-ERROR.

    /* Configura a Linha do Titulo da Planilha */
/*     ASSIGN chWorkSheet:Rows("1:1"):RowHeight = 30     */
/*            chWorkSheet:Rows("1:1"):FONT:SIZE = 13     */
/*            chWorkSheet:Rows("1:1"):FONT:bold = FALSE. */

    IF tt-romaneio.codEstabel = '505'THEN DO: 
        ASSIGN nomeEmpresa = 'MEDTEXTIL ITAJAÖ'.
    END.
    ELSE 
        ASSIGN nomeEmpresa = 'MEDTEXTIL VILA VELHA'.

    ASSIGN chworksheet:range("A1"):VALUE = "R O M A N E I O    D E    E M B A R Q U E  - " + nomeEmpresa + FILL(" ",20) NO-ERROR.

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
    ASSIGN chWorkSheet:COLUMNS("A"):ColumnWidth           = 10 
           chWorkSheet:COLUMNS("B"):ColumnWidth           = 15 
           chWorkSheet:COLUMNS("C"):ColumnWidth           = 25 
           chWorkSheet:COLUMNS("D"):ColumnWidth           = 19 NO-ERROR.
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
           Chworksheet:range("I:K"):HorizontalAlignment = 4 NO-ERROR.

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
              chWorkSheet:Range("D" + STRING(i-linha)):VALUE = tt-romaneio.cTransp NO-ERROR.

      ASSIGN  i-linha = 6.

      ASSIGN  chWorkSheet:Range("A" + STRING(i-linha)):VALUE = tt-romaneio.totVolume
              chWorkSheet:Range("B" + STRING(i-linha)):VALUE = tt-romaneio.totEtq
              chWorkSheet:Range("C" + STRING(i-linha)):VALUE = tt-romaneio.totPeso
              chWorkSheet:Range("D" + STRING(i-linha)):VALUE = tt-romaneio.totQtd NO-ERROR.
     
    END.

   

    ASSIGN chworksheet:range("A8"):VALUE = "D E S C R I € Ç O    D O S    V O L U M E S" + FILL(" ",20) NO-ERROR.

    /* Configura Alinhamento Horizontal do Titulo da Planilha */
    ChWorkSheet:range("A8:D8"):SELECT() NO-ERROR.
    ChWorksheet:range("A8:D8"):Merge NO-ERROR.
    ASSIGN 
    Chworksheet:Range("A8:D8"):HorizontalAlignment = 3 /* Centralizado */
    Chworksheet:Range("A8:D8"):VerticalAlignment   = 2 NO-ERROR. /* Centralizado */

    /* Colorir Titulo da Planilha */
    ASSIGN 
    chWorkSheet:Range("A8:D8"):FONT:ColorIndex     = 18 /* Avermelhado */
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
       RUN copiarFormatacao('item',i-linha) NO-ERROR.
       RUN impCabecalho('item',i-linha) NO-ERROR.
       RUN incrLinha NO-ERROR.
       RUN copiarFormatacao('cab',i-linha) NO-ERROR.
       RUN impCabecalho('cab',i-linha) NO-ERROR. 
       RUN incrLinha NO-ERROR.
       
       FOR EACH tt-itens-romaneio 
           WHERE tt-itens-romaneio.iditem = tt-dados-itens-romaneio.id NO-LOCK.
           
/*            MESSAGE "num.etq:" tt-itens-romaneio.numEtq   */
/*                    "linha:" i-linha                      */
/*                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK. */
           ASSIGN 
               chWorkSheet:Range("A" + STRING(i-linha)):VALUE = tt-itens-romaneio.numEtq
               chWorkSheet:Range("B" + STRING(i-linha)):VALUE = tt-itens-romaneio.qtidade
               chWorkSheet:Range("C" + STRING(i-linha)):VALUE = tt-itens-romaneio.peso NO-ERROR.
          RUN incrLinha NO-ERROR.                                                               
       END.                                                                            
       RUN incrLinha NO-ERROR. 
    END.

    i-linha  = i-linha   + 2.
    ASSIGN chWorkSheet:Range("A" + STRING(i-linha)):VALUE = "IMPORTANTE!!" NO-ERROR.
    i-linha  = i-linha   + 1.
    ASSIGN chWorkSheet:Range("A" + STRING(i-linha)):VALUE = "AO RECEBER A MERCADORIA, FAVOR CONFERIR O TOTAL DE VOLUMES E A QUANTIDADE DE PE€AS." NO-ERROR.
    i-linha  = i-linha   + 1.
           chWorkSheet:Range("A" + STRING(i-linha)):VALUE = "CASO HAJA A VIOLA€ÇO DO VOLUME, COM FALTA DE PE€AS3" NO-ERROR.
    i-linha  = i-linha   + 1.
            chWorkSheet:Range("A" + STRING(i-linha)):VALUE = "ENTRE CONTATO COM A IMATEXTIL (031)32383100." NO-ERROR.
    i-linha  = i-linha   + 1.
           chWorkSheet:Range("A" + STRING(i-linha)):VALUE = "*** EM CASO DE RECLAMA€ÇO, FAVOR DEVOLVER A ETIQUETA DO VOLUME ***" NO-ERROR.
    //chExcelApp:VISIBLE = TRUE.
    IF logConvertPDF THEN
       chExcelapp:ActiveSheet:ExportAsFixedFormat(0,cArq,0,true,,,,,) NO-ERROR.
    ELSE DO:
        //chExcelapp:ActiveWorkbook:SaveAs(cArq,43,,,,,).
        chExcelapp:ActiveSheet:SaveAs(cArq).
    END.
        
    //chWorksheet:PrintOut(1,1,1,FALSE,cArq,FALSE, FALSE).
    //OS-COMMAND SILENT VALUE("start " + cArq).
    chExcelapp:QUIT.
    RELEASE OBJECT chWorkSheet NO-ERROR.
    RELEASE OBJECT chWorkBook NO-ERROR.
    RELEASE OBJECT chExcelApp NO-ERROR.
END.
ELSE DO:
    MESSAGE "NÆo h  registros nesse periodo"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

RUN pi-finalizar in h-acomp.

{esp/ERROProgress.i}

IF cErroProgress <> '' THEN
   MESSAGE cErroProgress
       VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.


/*CATCH oError AS Progress.Lang.Error :    
MESSAGE "Ocorreu erro na tentiva de Gerar o Excel" SKIP

    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
RETURN 'nok'.
END CATCH.*/




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


