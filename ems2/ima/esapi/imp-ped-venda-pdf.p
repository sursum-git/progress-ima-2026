/*****[ Includes ]*****/
{include/tt-edit.i}
{include/pi-edit.i}
{PDFinclude/pdf_inc.i "THIS-PROCEDURE"}

DEF VAR h-acomp          AS HANDLE NO-UNDO.
DEF VAR h-prog           AS HANDLE NO-UNDO.
DEF VAR i-lin            AS INT.
DEF VAR i-salto          AS INT INITIAL  6.
DEF VAR c-desc-condpag   AS CHAR FORMAT "x(50)".
DEF VAR de-qtd-tot       AS DEC FORMAT ">>>,>>>,>>9.9999". 
DEF VAR de-tot-ped       AS DEC FORMAT ">>>,>>>,>>9.9999". 
DEF VAR c-cgc            AS CHAR.
DEF VAR c-un             LIKE item.un.
DEF VAR c-arq-modelo     AS CHAR.  
DEF VAR c-arq-gerado-pdf AS CHAR.
DEF VAR c-arq-logo       AS CHAR.
DEF VAR c-acrobat        AS CHAR.
DEF VAR c-destinatario   AS CHAR.
DEF VAR c-Imp            AS CHAR.
DEF VAR c-observ         AS CHAR.
DEF VAR c-tp-pedido      AS CHAR.
DEF VAR c-entrega        AS CHAR.
DEF VAR c-meses          AS CHAR INIT "JAN,FEV,MAR,ABR,MAI,JUN,JUL,AGO,SET,OUT,NOV,DEZ".

DEFINE INPUT PARAMETER p-nr-pedido LIKE ped-venda.nr-pedido.
DEFINE INPUT PARAMETER p-saida AS INTEGER.

/* Verifica a Existencia do Utilitario ADOBE READER */
LOAD "AcroExch.Document" BASE-KEY "HKEY_CLASSES_ROOT".
USE "AcroExch.Document".

GET-KEY-VALUE SECTION "shell\open\command" KEY DEFAULT VALUE c-acrobat.
UNLOAD "AcroExch.Document".

IF c-acrobat = ? THEN DO:
   MESSAGE "O Utilitario ADOBE READER n∆o foi encontrado." SKIP
           "N∆o Ç possivel a execuá∆o do programa."
       VIEW-AS ALERT-BOX INFO BUTTONS OK.
   RETURN.
END.

/* Abertura e Criaá∆o de Arquivo do PDF */
ASSIGN c-arq-modelo = SEARCH("modelos\modelo2.pdf")
       c-arq-logo   = SEARCH("ima\image\ima\logoima.jpg")
       c-Imp        = STRING(CAPS(SESSION:PRINTER-NAME))
       c-acrobat    = ENTRY(2,c-acrobat,'"').


RUN utp/ut-utils.p PERSISTENT SET h-prog.
RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
{utp/ut-liter.i Imprimindo *}
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

FIND ped-venda WHERE
     ped-venda.nr-pedido = p-nr-pedido NO-LOCK NO-ERROR.

FIND ped-venda-ext WHERE
     ped-venda-ext.cod-estabel = ped-venda.cod-estabel AND
     ped-venda-ext.nr-pedido = ped-venda.nr-pedido NO-LOCK NO-ERROR.

FIND estabelec WHERE
     estabelec.cod-estabel = ped-venda.cod-estabel NO-LOCK NO-ERROR.

FIND estab-distrib WHERE
     estab-distrib.cod-estab = estabelec.cod-estabel NO-LOCK NO-ERROR.

FIND emitente WHERE
     emitente.cod-emit = ped-venda.cod-emit NO-LOCK NO-ERROR.

ASSIGN c-arq-gerado-pdf = SESSION:TEMP-DIRECTORY + STRING(ped-venda.nr-pedcli) + ".pdf".
RUN pi-cria-pdf.

FIND cond-pagto WHERE
     cond-pagto.cod-cond-pag = ped-venda.cod-cond-pag NO-LOCK NO-ERROR.

IF AVAIL cond-pagto THEN
   ASSIGN c-desc-condpag = cond-pagto.descricao.
ELSE DO.
   FOR EACH cond-ped OF ped-venda NO-LOCK.
       IF cond-ped.nr-dias <> 0 THEN
          ASSIGN c-desc-condpag = IF c-desc-condpag = ''
                                  THEN STRING(cond-ped.nr-dias)
                                  ELSE c-desc-condpag + "," + STRING(cond-ped.nr-dias). 
       ELSE
          ASSIGN c-desc-condpag = IF c-desc-condpag = ''
                                  THEN STRING(cond-ped.data-pagto)
                                  ELSE c-desc-condpag + "," + STRING(cond-ped.data-pagto). 
   END.
   ASSIGN c-desc-condpag = c-desc-condpag + " DD".
END.

ASSIGN c-entrega = '1a Quinzena / ' + ENTRY(MONTH(ped-venda.dt-entrega),c-meses).
IF DAY(ped-venda.dt-entrega) > 15 THEN
   ASSIGN c-entrega = REPLACE(c-entrega,"1a","2a").

ASSIGN c-tp-pedido = IF ped-venda.tp-pedido = 'PI' 
                     THEN "PEDIDO IMPORTAÄ«O"
                     ELSE "PRONTA ENTREGA".

RUN pi-cabec-form.
 
ASSIGN de-tot-ped = 0.
FOR EACH ped-item OF ped-venda WHERE 
         ped-item.cod-sit-item = 1 NO-LOCK
         BY ped-item.it-codigo 
         BY ped-item.cod-refer.

    RUN pi-acompanhar IN h-acomp (INPUT "Pedido: " + ped-venda.nr-pedcli + " Item: " + ped-item.it-codigo).

    FIND ITEM WHERE
         ITEM.it-codigo = ped-item.it-codigo NO-LOCK NO-ERROR.

    RUN pi-imp-item.

    ASSIGN de-tot-ped = de-tot-ped + (ped-item.qt-pedida * ped-item.vl-preori).

END.

ASSIGN i-lin = i-lin - 20.
RUN pdf_set_font IN h_PDFinc ("Spdf","Helvetica-bold",12).
RUN pdf_text_xy  IN h_PDFinc ("Spdf","VALOR TOTAL DO PEDIDO:",300,i-lin).
RUN pdf_text_xy  IN h_PDFinc ("Spdf",STRING(de-tot-ped,">>>,>>>,>>9.99"),480,i-lin).
RUN pdf_text_Color("Spdf",0,0,0).

IF i-lin < 100 THEN 
   RUN pi-nova-pagina.
ELSE
  ASSIGN i-lin = 100.

RUN pdf_text_Color("Spdf",.3,0.4,.5).
RUN pdf_rect ("Spdf",15,95,565,20,1). 

RUN pdf_text_Color("Spdf",1,1,1).
RUN pdf_set_font IN h_PDFinc ("Spdf","Helvetica-bold",12).
RUN pdf_text_xy  IN h_PDFinc ("Spdf","Observaá∆o",20,100).

RUN pdf_text_Color("Spdf",0,0,0).

ASSIGN i-lin = i-lin - 20.

ASSIGN c-observ = 'PEDIDO SUJEITO ∑ APROVAÄ«O'.

RUN pi-print-editor(INPUT REPLACE(REPLACE(c-observ, CHR(13), " "), CHR(10), " "), INPUT 105).
FOR EACH tt-editor:
    RUN pdf_set_font IN h_PDFinc ("Spdf","Helvetica-bold",9).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf",tt-editor.conteudo,20,i-lin).
    ASSIGN i-lin = i-lin - 10 .
END.

RUN pdf_close ("Spdf"). /* Fechar Arquivo pdf */
RUN pi-finalizar in h-acomp.

CASE p-saida.
    WHEN 1 THEN DO.   /* Impress∆o do Documento */
        RUN WinExec (c-acrobat, INPUT 2). 
        RUN WinExec (c-acrobat + " " + "/n /t /h" + " " + c-arq-gerado-pdf + " " + c-Imp, INPUT 2). 
        PAUSE 15 NO-MESSAGE.
        RUN WinExec ("taskkill /im AcroRd32.exe /f", INPUT 2). 
    END.
    WHEN 3 THEN DO.  // Terminal
        RUN EXECUTE IN h-prog(INPUT c-acrobat,
                              INPUT c-arq-gerado-pdf).
        DELETE PROCEDURE h-prog.
    END.
END CASE.

/**************** P R O C E D I M E N T O S *********************/

PROCEDURE pi-cabec-form.
    /* Criaá∆o dos Retangulos do Cabeáalho */
    RUN pdf_rect2 ("Spdf",15,730,565,100,1). /* ColIni LinIni ColFin Altura Espessura */
    RUN pdf_place_image IN h_PDFinc ("Spdf","logo",25,105,150,86). /* Logotipo */
    RUN pi-desenha-linha (180,830,180,730,1). /* Traáo Vertical que separa o Logo */

    RUN pdf_set_font IN h_PDFinc ("Spdf","Helvetica-bold",14).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf","IMA TEXTIL",200,805).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf","Rua dos Timbiras, 2072/15a - Lourdes",200,785).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf","Belo Horizonte - MG",200,765).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf","Telefone: (31) 3238-3100",200,745).

    RUN pdf_set_font IN h_PDFinc ("Spdf","Helvetica-bold",25).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf","PEDIDO DE VENDA",200,700).

    RUN pdf_rect2 ("Spdf",15,15,565,675,1). /* ColIni LinIni ColFin Altura Espessura */ 
    
    RUN pdf_set_font IN h_PDFinc ("Spdf","Helvetica-Bold", 12).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf","PEDIDO: " + ped-venda.nr-pedcli,30,665).       
    RUN pdf_text_xy  IN h_PDFinc ("Spdf","DT IMPLANT: " + STRING(ped-venda.dt-implant,"99/99/9999"),350,665).       

    RUN pdf_text_xy  IN h_PDFinc ("Spdf","CLIENTE: " + emitente.nome-emit,30,645).       
    RUN pdf_text_xy  IN h_PDFinc ("Spdf","CIDADE: " + ped-venda.cidade + " / " + ped-venda.estado,350,645).       

    RUN pdf_text_xy  IN h_PDFinc ("Spdf","REPRES: " + ped-venda.no-ab-reppri,30,625).       
    RUN pdf_text_xy  IN h_PDFinc ("Spdf","COND.PAGTO: " + c-desc-condpag,350,625).       

    RUN pdf_text_xy  IN h_PDFinc ("Spdf","TRANSP: " + ped-venda.nome-transp,30,605).       
    RUN pdf_text_xy  IN h_PDFinc ("Spdf","TIPO FRETE: " + ped-venda-ext.tp-frete,350,605).       

    RUN pdf_text_xy  IN h_PDFinc ("Spdf","TIPO PEDIDO: " + c-tp-pedido,30,585).       
    RUN pdf_text_xy  IN h_PDFinc ("Spdf","ENTREGA: " + c-entrega,350,585).       

    RUN pi-desenha-linha (15,575,575,575,1). /* Traáo Horizontal que Divide 0 2ß Retangulo */

    /* Montagem do Cabeáalho do Item */
    RUN pdf_text_Color("Spdf",.3,0.4,.5).
    RUN pdf_rect ("Spdf",15,550,560,20,1). 

    RUN pdf_text_Color("Spdf",1,1,1).
    RUN pdf_set_font IN h_PDFinc ("Spdf","Helvetica-bold",12).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf","Item",20,555).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf","Descriá∆o",70,555).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf","Ref",305,555).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf","Qt Pedida",335,555).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf","Un",400,555).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf","Preáo UN",425,555).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf","Preáo Total",485,555).

    RUN pdf_text_Color("Spdf",0,0,0).

    ASSIGN i-lin = 530.
END PROCEDURE.


PROCEDURE pi-imp-item.
     IF i-lin < 30 THEN RUN pi-nova-pagina.

     RUN pdf_set_font IN h_PDFinc ("Spdf","Helvetica",10).
     RUN pdf_text_xy  IN h_PDFinc ("Spdf",ped-item.it-codigo,20,i-lin).
     RUN pdf_text_xy  IN h_PDFinc ("Spdf",SUBSTR(item.desc-item,1,36),70,i-lin).
     RUN pdf_text_xy  IN h_PDFinc ("Spdf",UPPER(ped-item.cod-refer),307,i-lin).
     RUN pdf_text_xy  IN h_PDFinc ("Spdf",STRING(ped-item.qt-pedida,">>>,>>9.99"),350,i-lin).
     RUN pdf_text_xy  IN h_PDFinc ("Spdf",item.un,405,i-lin).
     RUN pdf_text_xy  IN h_PDFinc ("Spdf",STRING(ped-item.vl-preori,">>>,>>9.99"),433,i-lin).
     RUN pdf_text_xy  IN h_PDFinc ("Spdf",STRING(ped-item.qt-pedida * ped-item.vl-preori,">>>,>>>,>>9.99"),490,i-lin). 

     ASSIGN i-lin = i-lin - 7.

     IF i-lin - 15 >= 30 THEN
        RUN pi-desenha-linha (15,i-lin,580,i-lin,1). /* Traáo Horizontal Apos Impress∆o do ITEM */

     ASSIGN i-lin = i-lin - 15.
END PROCEDURE.


PROCEDURE pi-desenha-linha.
   DEF INPUT PARAMETER i-col-ini AS INT.
   DEF INPUT PARAMETER i-lin-ini AS INT.
   DEF INPUT PARAMETER i-col-fin AS INT.
   DEF INPUT PARAMETER i-lin-fin AS INT.
   DEF INPUT PARAMETER i-denso   AS INT.

   RUN pdf_line ("Spdf", i-col-ini, /* Coluna Inicial */
                         i-lin-ini, /* Linha Inicial */
                         i-col-fin, /* Coluna Final */
                         i-lin-fin, /* Linha Final */
                         i-denso).  /* Espessura da Linha */
END PROCEDURE.


PROCEDURE pi-nova-pagina.
    RUN pdf_new_page IN h_PDFinc ("Spdf"). /* Cria Uma Nova Pagina */
    RUN pdf_rect2 ("Spdf",15,15,565,815,1). /* ColIni LinIni ColFin Altura Espessura */

    /* Montagem do Cabeáalho do Item */
    RUN pdf_text_Color("Spdf",.3,0.4,.5).
    RUN pdf_rect ("Spdf",15,815,565,20,1). 

    RUN pdf_text_Color("Spdf",1,1,1).
    RUN pdf_set_font IN h_PDFinc ("Spdf","Helvetica-bold",12).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf","Item",20,600).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf","Descriá∆o",70,600).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf","Ref",305,600).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf","Qt Pedida",335,600).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf","Un",400,600).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf","Preáo UN",425,600).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf","Preáo Total",485,600).

    RUN pdf_text_Color("Spdf",0,0,0).

    ASSIGN i-lin = 800.
END PROCEDURE.

PROCEDURE pi-cria-pdf.
   RUN pdf_new             IN h_PDFinc ("Spdf", c-arq-gerado-pdf).
   RUN pdf_open_pdf        IN h_PDFinc ("Spdf", c-arq-modelo, "Modelo"). 
   RUN pdf_load_font       IN h_PDFinc ("Spdf","TimesBD","C:\WINDOWS\Fonts\timesbd.ttf","modelos\timesbd.afm",""). /* Importando Fonte */
   RUN pdf_load_font       IN h_PDFinc ("Spdf","Arial","C:\WINDOWS\Fonts\arial.ttf","modelos\arial.afm","").   /* Importando Fonte */
   RUN pdf_set_papertype   IN h_PDFinc ("Spdf","A4").
   RUN pdf_new_page        IN h_PDFinc ("Spdf").                     /* Cria Uma Nova Pagina */
   RUN pdf_load_image      IN h_PDFinc ("Spdf","logo",c-arq-logo).   /* Importando logotipo  */
END PROCEDURE.

PROCEDURE WinExec EXTERNAL "kernel32.dll":
  DEF INPUT PARAMETER prog_name    AS CHAR.
  DEF INPUT PARAMETER visual_style AS SHORT.
END PROCEDURE.

