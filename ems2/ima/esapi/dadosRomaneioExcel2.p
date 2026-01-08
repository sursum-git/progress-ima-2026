DEFINE VARIABLE chExcelApp      AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorkBook      AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorkSheet     AS COM-HANDLE NO-UNDO.

DEFINE VARIABLE vchActiveSheet AS COM-HANDLE NO-UNDO.

DEFINE INPUT  PARAMETER pCodEstabel AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pSerie      AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pNF         AS CHARACTER   NO-UNDO.

//foráa a geraá∆o de novo romaneio ao inves de tentar buscar um j† existente
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
   ASSIGN cExtensaoArq = '.xls'.

ASSIGN cDirRomaneio = 'c:\temp'.
ASSIGN cArq = cDirRomaneio + "\" + nota-fiscal.nr-pedcli + cExtensaoArq  NO-ERROR.

IF SEARCH(cArq) <> ? AND logNovo = NO THEN DO:
   RUN pi-finalizar IN h-acomp NO-ERROR.
   RETURN 'ok'.
END.
   

RUN esapi/extrairDadosRomaneio.p(pCodEstabel,int(pSerie),pNF,OUTPUT TABLE tt-romaneio,OUTPUT TABLE tt-itens-romaneio,OUTPUT TABLE tt-dados-itens-romaneio).
//RUN esapi/extrairDadosRomaneio.p('5',3,'0167456',OUTPUT TABLE tt-romaneio,OUTPUT TABLE tt-itens-romaneio,OUTPUT TABLE tt-dados-itens-romaneio).

//OUTPUT TO VALUE('\\pv1\pv2\romaneio\testepermissao.txt').
OUTPUT TO VALUE('c:\temp\testepermissao.txt').

PUT 'deu certo' SKIP.
OUTPUT CLOSE.

RUN pi-acompanhar IN h-acomp (INPUT "Gerando PDF:") NO-ERROR.
FIND FIRST tt-romaneio NO-LOCK NO-ERROR.
IF AVAIL tt-romaneio THEN DO:
    CREATE "excel.application" chExcelApp NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
       RETURN 'nok'.
    chExcelApp:VISIBLE = FALSE NO-ERROR. 
    //chExcelApp:VISIBLE = TRUE.
    //chWorkBook = chExcelApp:Workbooks:ADD().
    ASSIGN cArquivo = SEARCH("modelo-xlt/romaneio2.xlt") NO-ERROR.
    IF cArquivo = ? THEN DO:
       MESSAGE "Arquivo De Modelo modelo-xlt/romaneio.xltx n∆o encontrado"
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
        ASSIGN nomeEmpresa = 'MEDTEXTIL ITAJA÷'.
    END.
    ELSE 
        ASSIGN nomeEmpresa = 'MEDTEXTIL VILA VELHA'.

    ASSIGN chworksheet:range("A1"):VALUE = "R O M A N E I O    D E    E M B A R Q U E  - " + nomeEmpresa + FILL(" ",20) NO-ERROR.

    IF logConvertPDF THEN
       chExcelapp:ActiveSheet:ExportAsFixedFormat(0,cArq,0,true,,,,,) NO-ERROR.
    ELSE DO:
        //chExcelapp:ActiveWorkbook:SaveAs(cArq,43,,,,,).
        vchActiveSheet = chExcelApp:ActiveSheet.
        vchActiveSheet:Saveas(cArq).
        //vchActiveSheet:SAVE().
        //chExcelApp:ActiveSheet:Saveas(cArq).
    END.
        
    //chWorksheet:PrintOut(1,1,1,FALSE,cArq,FALSE, FALSE).
    //OS-COMMAND SILENT VALUE("start " + cArq).
    chExcelapp:QUIT.
    RELEASE OBJECT chWorkSheet NO-ERROR.
    RELEASE OBJECT chWorkBook NO-ERROR.
    RELEASE OBJECT chExcelApp NO-ERROR.
END.
ELSE DO:
    MESSAGE "N∆o h† registros nesse periodo"
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


