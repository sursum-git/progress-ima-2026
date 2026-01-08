/*****[ Includes ]*****/
{include/tt-edit.i}
{include/pi-edit.i}
{PDFinclude/pdf_inc.i "THIS-PROCEDURE"}

DEF VAR h-acomp          AS HANDLE NO-UNDO.
DEF VAR h-prog           AS HANDLE NO-UNDO.
DEF VAR i-lin            AS INT.
DEF VAR i-salto          AS INT INITIAL  6.
DEF VAR c-docas          AS CHAR.
DEF VAR c-desc-condpag   AS CHAR FORMAT "x(50)".
DEF VAR de-total         AS DEC FORMAT ">>>,>>>,>>9.9999".
DEF VAR de-qtd-tot       AS DEC FORMAT ">>>,>>>,>>9.9999". 
DEF VAR c-cgc            AS CHAR.
DEF VAR c-un             LIKE item.un.
DEF VAR c-arq-modelo     AS CHAR.  
DEF VAR c-arq-gerado-pdf AS CHAR.
DEF VAR c-arq-logo       AS CHAR.
DEF VAR c-acrobat        AS CHAR.
DEF VAR c-destinatario   AS CHAR.
DEF VAR c-Imp            AS CHAR.
DEF VAR c-observ         AS CHAR.

DEFINE INPUT PARAMETER p-nr-pedido LIKE ped-venda.nr-pedido.
DEFINE INPUT PARAMETER p-saida AS INTEGER.


DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.


/* Verifica a Existencia do Utilitario ADOBE READER */
LOAD "AcroExch.Document" BASE-KEY "HKEY_CLASSES_ROOT".
USE "AcroExch.Document".

GET-KEY-VALUE SECTION "shell\open\command"
KEY DEFAULT 
VALUE c-acrobat.
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

FIND FIRST ped-venda NO-LOCK NO-ERROR.

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

RUN pi-cabec-form.

ASSIGN de-total = 0.
FOR EACH ped-item OF ped-venda where
         ped-item.cod-sit-item = 1 NO-LOCK
         BY ped-item.it-codigo 
         BY ped-item.cod-refer.

    RUN pi-acompanhar IN h-acomp (INPUT "Pedido: " + ped-venda.nr-pedcli + " Item: " + ped-item.it-codigo).

    FIND ITEM WHERE
         ITEM.it-codigo = ped-item.it-codigo NO-LOCK NO-ERROR.

    FIND ped-item-ext WHERE
         ped-item-ext.cod-estabel = ped-venda.cod-estabel AND
         ped-item-ext.nr-pedcli = ped-item.nr-pedcli AND
         ped-item-ext.nome-abrev = ped-item.nome-abrev AND
         ped-item-ext.nr-sequencia = ped-item.nr-sequencia NO-LOCK.


    ASSIGN c-observ = 'ESTOQUE'.
    IF ped-item-ext.retirar-corte THEN
       ASSIGN c-observ = "RETIRAR CORTE".

    ASSIGN de-qtd-tot = 0
           c-docas = ''.
    FOR EACH ob-etiqueta WHERE
             ob-etiqueta.it-codigo = ped-item.it-codigo AND
             ob-etiqueta.cod-refer = ped-item.cod-refer AND 
             ob-etiqueta.situacao = 3 AND
             ob-etiqueta.cod-estabel = estabelec.cod-estabel AND
             ob-etiqueta.quantidade > 0 NO-LOCK USE-INDEX indice6.

        ASSIGN de-qtd-tot = de-qtd-tot + ob-etiqueta.quantidade.
 
        IF ob-etiqueta.localiz <> '' AND
           LOOKUP(STRING(ob-etiqueta.localiz,"999/999"),c-docas) = 0 THEN
           ASSIGN c-docas = IF c-docas = ''
                            THEN STRING(ob-etiqueta.localiz,"999/999")
                            ELSE c-docas + ',' + STRING(ob-etiqueta.localiz,"999/999").
    END.

    RUN pi-imp-item.
END.

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

ASSIGN c-observ = ped-venda.observacoes + ped-venda-ext.compl-observ.

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
    WHEN 3 THEN DO.
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
    RUN pdf_rect2 ("Spdf",15,15,565,700,1). /* ColIni LinIni ColFin Altura Espessura */ 
    RUN pdf_set_font IN h_PDFinc ("Spdf","Helvetica-bold",15).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf","INSTRUÄ«O PARA SEPARAÄ«O E FATURAMENTO",190,790).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf","EMISS«O: " + STRING(TODAY,"99/99/9999") + " " + STRING(TIME,"HH:MM:SS"),250,760).       

    RUN pdf_set_font IN h_PDFinc ("Spdf","Helvetica-Bold", 14).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf","PEDIDO...: " + ped-venda.nr-pedcli,30,690).       
    RUN pdf_text_xy  IN h_PDFinc ("Spdf","DT IMPLANT: " + STRING(ped-venda.dt-implant,"99/99/9999"),350,690).       
    RUN pdf_text_xy  IN h_PDFinc ("Spdf","CLIENTE..: " + ped-venda.nome-abrev,30,670).       
    RUN pdf_text_xy  IN h_PDFinc ("Spdf","CIDADE....: ",350,670).       
    RUN pdf_text_xy  IN h_PDFinc ("Spdf","REPRES..: " + ped-venda.no-ab-reppri,30,650).       

    RUN pdf_text_xy  IN h_PDFinc ("Spdf","TRANSP..: " + ped-venda.nome-transp,30,630).       

    RUN pdf_set_font IN h_PDFinc ("Spdf","Helvetica-bold",12).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf",ped-venda.cidade + " / " + ped-venda.estado,430,670).       

    RUN pdf_set_font IN h_PDFinc ("Spdf","Helvetica-bold",10).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf","COND. PAGTO: " + c-desc-condpag,350,650).       
    RUN pdf_text_xy  IN h_PDFinc ("Spdf","Qtde Peáas......:_______       Volume:_______",350,630).       

    RUN pi-desenha-linha (15,620,580,620,1). /* Traáo Horizontal que Divide 0 2ß Retangulo */

    /* Montagem do Cabeáalho do Item */
    RUN pdf_text_Color("Spdf",.3,0.4,.5).
    RUN pdf_rect ("Spdf",15,595,565,20,1). 

    RUN pdf_text_Color("Spdf",1,1,1).
    RUN pdf_set_font IN h_PDFinc ("Spdf","Helvetica-bold",12).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf","Item",20,600).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf","Descriá∆o/Docas",70,600).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf","Ref",305,600).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf","Qt Pedida",335,600).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf","Un",400,600).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf","Qt Estoq",425,600).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf","Observaá∆o",485,600).

    RUN pdf_text_Color("Spdf",0,0,0).

    ASSIGN i-lin = 580.
END PROCEDURE.


PROCEDURE pi-imp-item.
     IF i-lin < 30 THEN RUN pi-nova-pagina.

     RUN pdf_set_font IN h_PDFinc ("Spdf","Helvetica",10).
     RUN pdf_text_xy  IN h_PDFinc ("Spdf",ped-item.it-codigo,20,i-lin).
     RUN pdf_text_xy  IN h_PDFinc ("Spdf",SUBSTR(item.desc-item,1,36),70,i-lin).
     RUN pdf_text_xy  IN h_PDFinc ("Spdf",UPPER(ped-item.cod-refer),307,i-lin).
     RUN pdf_text_xy  IN h_PDFinc ("Spdf",STRING(ped-item.qt-pedida,">>>,>>9.99"),350,i-lin).
     RUN pdf_text_xy  IN h_PDFinc ("Spdf",item.un,405,i-lin).
     RUN pdf_text_xy  IN h_PDFinc ("Spdf",STRING(de-qtd-tot,">>>,>>9.99"),433,i-lin).
     RUN pdf_text_xy  IN h_PDFinc ("Spdf",c-observ,485,i-lin). 

     ASSIGN i-lin = i-lin - 7.
     
     IF c-docas <> '' THEN DO.
        ASSIGN i-lin = i-lin - 7.
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Docas:",70,i-lin).
        RUN pdf_text_xy IN h_PDFinc ("Spdf",c-docas,105,i-lin). 
        ASSIGN i-lin = i-lin - 8.
     END.

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
    RUN pdf_text_xy  IN h_PDFinc ("Spdf","Item",20,820).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf","Descriá∆o/Docas",70,820).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf","Ref",305,820).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf","Qt Pedida",335,820).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf","Un",400,820).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf","Qt Estoq",425,820).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf","Observaá∆o",485,820).

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

