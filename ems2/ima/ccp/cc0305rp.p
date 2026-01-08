DEFINE TEMP-TABLE tt-param NO-UNDO
   FIELD destino          AS INTEGER
   FIELD arquivo          AS CHAR FORMAT "x(35)"
   FIELD usuario          AS CHAR FORMAT "x(12)"
   FIELD data-exec        AS DATE
   FIELD hora-exec        AS INTEGER
   FIELD pedido-ini       LIKE pedido-compr.num-pedido
   FIELD pedido-fin       LIKE pedido-compr.num-pedido
   FIELD comprador-ini    LIKE pedido-compr.responsavel
   FIELD comprador-fim    LIKE pedido-compr.responsavel
   FIELD num-copias       AS INT
   FIELD e-mail           AS LOG
   FIELD subject          AS CHAR FORMAT "x(40)"
   FIELD texto            AS CHAR FORMAT "x(2000)"
   FIELD tg-descricao     AS LOGICAL
   FIELD tg-nar-itordem   AS LOGICAL
   FIELD tg-narrat-ordem  AS LOGICAL.

DEFINE TEMP-TABLE tt-raw-digita 
       FIELD raw-digita AS RAW.

DEF INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEF INPUT PARAMETER TABLE FOR tt-raw-digita.

DEF VAR l-impressao-ok AS LOG.

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

/*****[ Includes ]*****/
{include/tt-edit.i}
{include/pi-edit.i}
{PDFinclude/pdf_inc.i "THIS-PROCEDURE"}

DEF VAR h-acomp          AS HANDLE NO-UNDO.
DEF VAR h-prog           AS HANDLE NO-UNDO.
DEF VAR i-lin            AS INT.
DEF VAR i-salto          AS INT INITIAL  6.
DEF VAR i-lin-item       AS INT INITIAL 400.
DEF VAR de-total         AS DEC FORMAT ">>>,>>>,>>9.9999".
DEF VAR c-cgc            AS CHAR.
DEF VAR c-un             LIKE prazo-compra.un.
DEF VAR c-arq-modelo     AS CHAR.  
DEF VAR c-arq-gerado-pdf AS CHAR.
DEF VAR c-arq-logo       AS CHAR.
DEF VAR c-acrobat        AS CHAR.
DEF VAR c-destinatario   AS CHAR.
DEF VAR c-Imp            AS CHAR.
DEF VAR l-ranhura        AS LOG INITIAL NO.

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
 ASSIGN c-arq-modelo = SEARCH("modelos\modelo.pdf")
        c-arq-logo   = SEARCH("ima\image\ima\logoima.jpg")
        c-Imp        = (STRING(CAPS(SESSION:PRINTER-NAME)))
        c-acrobat    = ENTRY(2,c-acrobat,'"').

 RUN utp/ut-utils.p PERSISTENT SET h-prog.
 RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
 {utp/ut-liter.i Imprimindo *}
 RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

 FIND FIRST pedido-compr WHERE  
            pedido-compr.num-pedido >= tt-param.pedido-ini AND
            pedido-compr.num-pedido <= tt-param.pedido-fin NO-LOCK NO-ERROR.
 IF NOT AVAIL pedido-compr THEN DO.
    MESSAGE "N∆o Encontrado Pedidos na Faixa Selecionada..." SKIP
            "N∆o Ç possivel a execuá∆o do programa."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
 END.

 FOR EACH pedido-compr WHERE  
          pedido-compr.num-pedido >= tt-param.pedido-ini AND
          pedido-compr.num-pedido <= tt-param.pedido-fin EXCLUSIVE-LOCK.

     FIND FIRST ordem-compra WHERE 
                ordem-compra.num-pedido = pedido-compr.num-pedido NO-LOCK. 
     FIND comprador WHERE
          comprador.cod-comprado = ordem-compra.cod-comprado NO-LOCK NO-ERROR.
     FIND estabelec WHERE
          estabelec.cod-estabel = pedido-compr.cod-estabel NO-LOCK NO-ERROR.
     FIND estab-distrib WHERE
          estab-distrib.cod-estab = estabelec.cod-estabel NO-LOCK NO-ERROR.
     FIND emitente WHERE 
          emitente.cod-emitente = pedido-compr.cod-emitente NO-LOCK.

     FIND cont-emit OF emitente WHERE
          cont-emit.area = 'COMERCIAL' NO-LOCK NO-ERROR.
     IF AVAIL cont-emit THEN
        ASSIGN c-destinatario = cont-emit.e-mail.
     ELSE
        ASSIGN c-destinatario = emitente.e-mail.

     IF tt-param.e-mail AND c-destinatario = "" THEN DO.
        MESSAGE "Fornecedor sem e-mail Cadastrado, Imposs°vel enviar e-mail" SKIP
                "Veja se Fornecedor tem um Contato Cadastrado com a µreal COMERCIAL"
                VIEW-AS ALERT-BOX.
        RETURN.
     END.

     c-arq-gerado-pdf = SESSION:TEMP-DIRECTORY + STRING(pedido-compr.num-pedido) + ".pdf".
     RUN pi-cria-pdf.

     RUN pi-cabec-form.
     
     ASSIGN de-total = 0.
     FOR EACH ordem-compra WHERE
              ordem-compra.num-pedido =  pedido-compr.num-pedido AND
              ordem-compra.situacao <> 4 NO-LOCK,
         EACH prazo-compra WHERE
              prazo-compra.numero-ordem = ordem-compra.numero-ordem AND
              prazo-compra.situacao <> 4 NO-LOCK
           BY prazo-compra.numero-ordem BY prazo-compra.parcela.

         RUN pi-acompanhar IN h-acomp (INPUT "Pedido de Compra: " + STRING(ordem-compra.num-pedido) + " Item: " + prazo-compra.it-codigo).

         FIND item WHERE
              item.it-codigo = prazo-compra.it-codigo NO-LOCK NO-ERROR.

         ASSIGN c-un = prazo-compra.un.
         FIND item-fornec WHERE
              item-fornec.cod-emit = ordem-compra.cod-emit AND
              item-fornec.it-codigo = prazo-compra.it-codigo 
              NO-LOCK NO-ERROR.
         IF AVAIL item-fornec THEN
            ASSIGN c-un = item-fornec.un.

         RUN pi-imp-linha.

         ASSIGN de-total =  de-total + ((prazo-compra.qtd-do-forn * ordem-compra.preco-fornec) +                                       
                                       ((prazo-compra.qtd-do-forn * ordem-compra.preco-fornec) * ordem-compra.aliquota-ipi * 0.01) +  
                                       ((prazo-compra.qtd-do-forn * ordem-compra.preco-fornec) * ordem-compra.valor-taxa   * 0.01) -    
                                       ((prazo-compra.qtd-do-forn * ordem-compra.preco-fornec) * ordem-compra.perc-descto  * 0.01)).   
     END.
     
     RUN pi-rodape.
     
     RUN pdf_close ("Spdf"). /* Fechar Arquivo pdf */

     ASSIGN pedido-compr.situacao = 1
            l-impressao-ok = YES.

     IF tt-param.destino = 1 THEN DO: /* Impress∆o do Documento */
        RUN WinExec (c-acrobat, INPUT 2). 
        RUN WinExec (c-acrobat + " " + "/n /t /h" + " " + c-arq-gerado-pdf + " " + c-Imp, INPUT 2). 
        PAUSE 15 NO-MESSAGE.
        RUN WinExec ("taskkill /im AcroRd32.exe /f", INPUT 2). 
     END.

     IF tt-param.e-mail THEN DO:
        ASSIGN tt-param.texto = REPLACE(tt-param.texto,"#PED",STRING(pedido-compr.num-pedido)).
        ASSIGN tt-param.texto = REPLACE(tt-param.texto,"#COMP",comprador.nome).
        ASSIGN tt-param.texto = REPLACE(tt-param.texto,"#FONE","Fone: " + comprador.telefone[1] + "   E-mail: " + comprador.e-mail).

        RUN esapi/esapi001.p (INPUT c-destinatario, /* e-mail */
                              INPUT tt-param.subject, /* Assunto */
                              INPUT tt-param.texto,   /* Mensagem */
                              INPUT c-arq-gerado-pdf, /* Arquivo anexo*/
                              INPUT YES).             /* Mostra Erros */
     END.
 END. /* pedido-compra */
 
 RUN pi-finalizar in h-acomp.
 
 IF NOT l-impressao-ok THEN DO.
    MESSAGE 'Erro na Impress∆o, favor Verificar...'
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN 'NOK'.
 END.

 IF tt-param.destino = 3 AND NOT tt-param.e-mail THEN DO.
     RUN EXECUTE IN h-prog(INPUT c-acrobat,
                           INPUT c-arq-gerado-pdf).
     DELETE PROCEDURE h-prog.
 END.
 
 PAUSE 15 NO-MESSAGE.
 OS-DELETE VALUE(c-arq-gerado-pdf). 


/*****[ P  R  O  C  E  D  I  M  E  N  T  O  S ]*****/
PROCEDURE pi-desenha-linha.
   /* Em Um Formulario A4 Com Orientaá∆o PAISAGEM:
         Coluna Inicial =  15
         Coluna Final   = 830
         Linha  Inicial = 595
         Linha  Final   =  10 */

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

PROCEDURE pi-cria-pdf.
   RUN pdf_new             IN h_PDFinc ("Spdf", c-arq-gerado-pdf).
   RUN pdf_open_pdf        IN h_PDFinc ("Spdf", c-arq-modelo, "Modelo"). 
   RUN pdf_load_font       IN h_PDFinc ("Spdf","TimesBD","c:\WINDOWS\Fonts\timesbd.ttf","modelos\timesbd.afm",""). /* Importando Fonte */
   RUN pdf_load_font       IN h_PDFinc ("Spdf","Arial",  "c:\WINDOWS\Fonts\arial.ttf",  "modelos\arial.afm","").   /* Importando Fonte */
   RUN pdf_set_papertype   IN h_PDFinc ("Spdf","A4").
   RUN pdf_set_orientation IN h_PDFinc ("Spdf","Landscape").
   RUN pdf_new_page        IN h_PDFinc ("Spdf").                     /* Cria Uma Nova Pagina */
   RUN pdf_load_image      IN h_PDFinc ("Spdf","logo",c-arq-logo).   /* Importando logotipo  */
END PROCEDURE.

PROCEDURE pi-imp-linha.
     ASSIGN i-lin = i-lin - 2.
     IF i-lin-item < 30 THEN DO:
        RUN pdf_new_page IN h_PDFinc ("Spdf"). /* Cria Uma Nova Pagina */
        RUN pi-cabec-form.
     END.
     /*  ton
     IF l-ranhura THEN DO:
        RUN pdf_text_Color("Spdf",3,2,0).
        RUN pdf_rect ("Spdf",15,(i-lin - 6),815,20,1).
        RUN pdf_text_Color("Spdf",0,0,0).
     END.
     */

     RUN pdf_set_font IN h_PDFinc ("Spdf","Helvetica",10).
     RUN pdf_text_xy  IN h_PDFinc ("Spdf",prazo-compra.it-codigo,25,i-lin).
     RUN pdf_text_xy  IN h_PDFinc ("Spdf",item.desc-item,95,i-lin)  .
     RUN pdf_text_xy  IN h_PDFinc ("Spdf",STRING(prazo-compra.qtd-do-forn,">,>>9.99"),375,i-lin).
     RUN pdf_text_xy  IN h_PDFinc ("Spdf",c-un,420,i-lin).
     RUN pdf_text_xy  IN h_PDFinc ("Spdf",STRING(ordem-compra.preco-fornec,">>>,>>9.9999"),470,i-lin).
     RUN pdf_text_xy  IN h_PDFinc ("Spdf",STRING(ordem-compra.aliquota-ipi, ">9.99") + "%",540,i-lin).
     RUN pdf_text_xy  IN h_PDFinc ("Spdf",(STRING(((prazo-compra.qtd-do-forn * ordem-compra.preco-fornec) +                                           
                                                  ((prazo-compra.qtd-do-forn * ordem-compra.preco-fornec) * ordem-compra.aliquota-ipi * 0.01) +              
                                                  ((prazo-compra.qtd-do-forn * ordem-compra.preco-fornec) * ordem-compra.valor-taxa   * 0.01) -                
                                                  ((prazo-compra.qtd-do-forn * ordem-compra.preco-fornec) * ordem-compra.perc-descto  * 0.01)),">>>>,>>9.99")),575,i-lin).
     RUN pdf_text_xy  IN h_PDFinc ("Spdf",STRING(prazo-compra.data-entrega,"99/99/9999"),650,i-lin)  .
     RUN pdf_text_xy  IN h_PDFinc ("Spdf",ordem-compra.requisitante,730,i-lin)  .
     
     RUN pi-print-editor(INPUT TRIM(ordem-compra.narrativa), INPUT 45).
     FIND FIRST tt-editor NO-LOCK NO-ERROR.
     IF AVAIL tt-editor AND tt-editor.conteudo <> "" THEN DO:
        FOR EACH tt-editor.
            IF tt-editor.conteudo <> "" THEN DO:
               ASSIGN i-lin = i-lin - 10.

               IF i-lin-item < 30 THEN DO:
                  RUN pdf_new_page IN h_PDFinc ("Spdf"). /* Cria Uma Nova Pagina */
                  RUN pi-cabec-form.
               END.

               IF i-lin > 0 THEN
                  RUN pdf_text_xy IN h_PDFinc ("Spdf",tt-editor.conteudo,95,i-lin).
            END.
        END.
        RUN pi-linha-item.
     END.
     ELSE DO:
        RUN pi-linha-item.
        ASSIGN i-lin = i-lin - 7.
        IF i-lin-item < 30 THEN DO:
            RUN pdf_new_page IN h_PDFinc ("Spdf"). /* Cria Uma Nova Pagina */
            RUN pi-cabec-form.
         END.

     END.
END PROCEDURE.

PROCEDURE pi-linha-item.
    IF i-lin-item < i-salto THEN DO:
       RUN pdf_new_page IN h_PDFinc ("Spdf"). /* Cria Uma Nova Pagina */
       RUN pi-cabec-form.
    END.

   RUN pi-desenha-linha (15,(i-lin - i-salto),830,(i-lin - i-salto),1). /* Traáo Horizontal Apos Impress∆o do ITEM */
   ASSIGN i-lin = i-lin - i-salto.

   RUN pi-desenha-linha ( 15,i-lin-item, 15,i-lin,1). /*  1ß Traáo do Cabeáalho de Item */
   RUN pi-desenha-linha ( 90,i-lin-item, 90,i-lin,1). /*  2ß Traáo do Cabeáalho de Item */
   RUN pi-desenha-linha (370,i-lin-item,370,i-lin,1). /*  3ß Traáo do Cabeáalho de Item */
   RUN pi-desenha-linha (410,i-lin-item,410,i-lin,1). /*  4ß Traáo do Cabeáalho de Item */
   RUN pi-desenha-linha (455,i-lin-item,455,i-lin,1). /*  5ß Traáo do Cabeáalho de Item */
   RUN pi-desenha-linha (525,i-lin-item,525,i-lin,1). /*  6ß Traáo do Cabeáalho de Item */
   RUN pi-desenha-linha (570,i-lin-item,570,i-lin,1). /*  7ß Traáo do Cabeáalho de Item */
   RUN pi-desenha-linha (640,i-lin-item,640,i-lin,1). /*  8ß Traáo do Cabeáalho de Item */
   RUN pi-desenha-linha (725,i-lin-item,725,i-lin,1). /*  9ß Traáo do Cabeáalho de Item */
   RUN pi-desenha-linha (830,i-lin-item,830,i-lin,1). /* 10ß Traáo do Cabeáalho de Item */

   ASSIGN i-lin-item = i-lin
          i-lin      = i-lin - 7
          l-ranhura  = IF l-ranhura THEN NO ELSE YES.

   IF i-lin-item < 30 THEN DO:
      RUN pdf_new_page IN h_PDFinc ("Spdf"). /* Cria Uma Nova Pagina */
      RUN pi-cabec-form.
   END.
   
END PROCEDURE.

PROCEDURE pi-cabec-form.
   ASSIGN i-lin-item  = 400
          i-lin       = 365
          l-ranhura   = NO.

   /* Criaá∆o dos Retangulos do Cabeáalho */
   RUN pdf_rect2 ("Spdf",15,500,815, 90,1). /* 15=Col Ini 500=Lin Ini 815=Col Fin 90=Altura 1=Espessura da Linha */
   RUN pdf_rect2 ("Spdf",15,400,815,100,1). 
   RUN pdf_rect2 ("Spdf",15,380,815, 20,1). 

   /* Criaá∆o das Linhas que dividem os Retangulos */
   RUN pi-desenha-linha (445,590,445,400,1). /* Traáo Vertical que Divide ao meio 0 1ß e 2ß Retangulo */
   RUN pi-desenha-linha (445,450,830,450,1). /* Traáo Horizontal que Divide 0 2ß Retangulo */

   /* Logotipo e Dados da Empresa */
   RUN pi-formata-cgc (INPUT estabelec.cgc, OUTPUT c-cgc).
   RUN pdf_place_image IN h_PDFinc ("Spdf","logo",18,93,150,86). /* Logotipo */
   RUN pdf_set_font    IN h_PDFinc ("Spdf","Helvetica-Bold",12).
   RUN pdf_text_xy     IN h_PDFinc ("Spdf",estabelec.nome,170,570).
   RUN pdf_text_xy     IN h_PDFinc ("Spdf","CNPJ: " + c-cgc ,170,550).
   RUN pdf_text_xy     IN h_PDFinc ("Spdf","INSCRIÄ«O ESTADUAL: " + estabelec.ins-estadual,170,530).
   RUN pdf_text_xy     IN h_PDFinc ("Spdf","www.imatextil.com.br",170,510).

   /* Endereáo da Empresa e Dados do Comprador */
   RUN pdf_set_font IN h_PDFinc ("Spdf","Helvetica",10).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf","Endereáo: " + TRIM(estabelec.endereco) + " - " + TRIM(estabelec.bairro),450,580).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf","Cidade: " + TRIM(estabelec.cidade) + " / " + TRIM(estabelec.estado) + "  -  CEP: " + STRING(estabelec.cep, "99999,999"),450,565).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf","Tel./Fax: " + estab-distrib.cod-telef + " / " + estab-distrib.cod-fax,450,550).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf","Comprador: " + comprador.nome,450,535).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf","Telefone: " + comprador.telefone[1],450,520).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf","e-mail: " + comprador.e-mail,450,505).
   
   /* Endereáo e Dados do Fornecedor */
   RUN pi-formata-cgc (INPUT emitente.cgc, OUTPUT c-cgc).
   RUN pdf_set_font IN h_PDFinc ("Spdf","Helvetica",10).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf","Fornecedor: " + STRING(emitente.cod-emitente, ">>9,999") + " - " + emitente.nome-emit,20,490).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf","Endereáo: " + emitente.endereco + " - " + emitente.bairro,20,476).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf","Cidade: " + emitente.cidade + " - " + emitente.estado + "  -  CEP: " + STRING(emitente.cep,"99999,999"),20,462).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf","Tel./Fax: " + emitente.telefone[1] + " / " + emitente.telefax,20,448).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf","CNPJ: " + c-cgc + "  -  INSC.EST: " + emitente.ins-estadual,20,434).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf","Contato: " + (IF AVAIL cont-emit THEN cont-emit.nome ELSE ""),20,420).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf","e-mail: " + emitente.e-mail,20,406).

   /* Dados do Pedido de Compra */
   RUN pdf_set_font IN h_PDFinc ("Spdf","Helvetica-Bold", 14).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf","PEDIDO Nß: " + STRING(pedido-compr.num-pedido, ">>>9,999"),450,480).       
   RUN pdf_text_xy  IN h_PDFinc ("Spdf","DATA: " + string(pedido-compr.data-pedido, "99/99/9999"),450,457).       
   
   /* Titulo confirmaá∆o de compra */
   RUN pdf_text_Color("Spdf",.3,0.4,.5).
/*   RUN pdf_rect ("Spdf",445,400,385, 50,1).
   RUN pdf_text_Color("Spdf",1,1,1). */
   RUN pdf_set_font IN h_PDFinc ("Spdf","Helvetica-bold",27).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf","CONFIRMAÄ«O DE COMPRA",450,415).
   RUN pdf_text_Color("Spdf",0,0,0).

   /* Montagem do Cabeáalho do Item */
   RUN pdf_text_Color("Spdf",.3,0.4,.5).
   RUN pdf_rect ("Spdf",15,380,815,20,1). 
   
   RUN pdf_text_Color("Spdf",1,1,1).
   RUN pdf_set_font IN h_PDFinc ("Spdf","Helvetica-bold",12).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf","C¢digo",        25,385).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf","Descriá∆o",     95,385).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf","Qtde",         375,385).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf","Und",          420,385).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf","Preáo",        475,385).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf","IPI",          540,385).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf","Valor Item",   575,385).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf","Data Entrega", 645,385).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf","Requisitante" ,730,385).
   RUN pdf_text_Color("Spdf",0,0,0).
/*
   RUN pi-desenha-linha ( 90,400, 90,380,1). /* 1ß Traáo do Cabeáalho de Item */
   RUN pi-desenha-linha (370,400,370,380,1). /* 2ß Traáo do Cabeáalho de Item */
   RUN pi-desenha-linha (410,400,410,380,1). /* 3ß Traáo do Cabeáalho de Item */
   RUN pi-desenha-linha (455,400,455,380,1). /* 4ß Traáo do Cabeáalho de Item */
   RUN pi-desenha-linha (525,400,525,380,1). /* 5ß Traáo do Cabeáalho de Item */
   RUN pi-desenha-linha (570,400,570,380,1). /* 6ß Traáo do Cabeáalho de Item */
   RUN pi-desenha-linha (640,400,640,380,1). /* 7ß Traáo do Cabeáalho de Item */
   RUN pi-desenha-linha (725,400,725,380,1). /* 8ß Traáo do Cabeáalho de Item */ */
END PROCEDURE.

PROCEDURE pi-rodape.
   DEF VAR i-lin-obs AS INT.

   FIND cond-pagto OF pedido-compr NO-LOCK NO-ERROR.
   FIND transporte OF pedido-compr NO-LOCK NO-ERROR.
   
   /* Criaá∆o dos Retangulos do Cabeáalho */
   RUN pdf_rect2 ("Spdf",15,i-lin-item - 100,815,100,1). /* Col Ini, Lin Ini, Col Fin, Altura, Espessura da Linha */

   RUN pi-desenha-linha (455,i-lin-item     ,455,i-lin-item - 100,1).  /* Traáo Vertical */ 
   RUN pi-desenha-linha (455,i-lin-item - 20,830,i-lin-item -  20,1).  /* 1¶ Linha Horizontal */
   RUN pi-desenha-linha (455,i-lin-item - 40,830,i-lin-item -  40,1).  /* 2¶ Linha Horizontal */
   RUN pi-desenha-linha (455,i-lin-item - 60,830,i-lin-item -  60,1).  /* 3¶ Linha Horizontal */
   RUN pi-desenha-linha (455,i-lin-item - 80,830,i-lin-item -  80,1).  /* 4¶ Linha Horizontal */
   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold", 12).
   RUN pdf_text_xy IN h_PDFinc ("Spdf", "OBS.:", 25,i-lin-item - 20).       
   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).

   ASSIGN i-lin-obs = i-lin-item - 20.
   RUN pi-print-editor(INPUT pedido-compr.comentarios, INPUT 65).
   FIND FIRST tt-editor NO-LOCK NO-ERROR.
   IF AVAIL tt-editor AND tt-editor.conteudo <> ""  THEN DO:
      FOR EACH tt-editor.
          IF tt-editor.conteudo <> "" THEN DO:
             RUN pdf_text_xy IN h_PDFinc ("Spdf", tt-editor.conteudo,65,i-lin-obs).
             ASSIGN i-lin-obs = i-lin-obs - 10.
          END.
      END.
   END.

   /* Criar Hachuras */
   RUN pdf_text_Color("Spdf",.3,0.4,.5).
   RUN pdf_rect ("Spdf",455,i-lin-item - 20,375, 20,1). 
   RUN pdf_text_Color("Spdf",1,1,1).

   RUN pdf_text_xy  IN h_PDFinc ("Spdf","TOTAL  R$",460,i-lin-item - 15).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf",STRING(de-total,">>>,>>>,>>9.99"),565,i-lin-item - 15).       
   RUN pdf_text_Color("Spdf",0,0,0).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf","PAGAMENTO",460,i-lin-item - 33).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf",IF AVAIL cond-pagto THEN cond-pagto.descricao ELSE "",533,i-lin-item - 33).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf","ENTREGA",460,i-lin-item - 52).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf",pedido-compr.compl-entrega,533,i-lin-item - 52).
   RUN pi-desenha-linha (530,i-lin-item - 20,530,i-lin-item - 60,1).    /* Traáo Vertical */ 
   RUN pdf_text_xy  IN h_PDFinc ("Spdf","TRANSPORTE/FRETE",460,i-lin-item - 73).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf",IF pedido-compr.frete = 1 THEN "CIF" ELSE "FOB",600,i-lin-item - 73).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf","TRANSPORTADORA/FONE",460,i-lin-item - 93).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf",IF AVAIL transporte THEN TRIM(transporte.nome) + " / " + transporte.telefone ELSE "" ,600,i-lin-item - 93).
   RUN pi-desenha-linha (595,i-lin-item - 60,595,i-lin-item - 100,1).   /* Traáo Vertical */ 
   RUN pi-desenha-linha (16,i-lin-item - 85,250,i-lin-item - 85,1).   /* Traáo Horizontal */
   RUN pdf_set_font IN h_PDFinc ("Spdf","Helvetica",10).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf","Gerància de Compras",75,i-lin-item - 95).
END PROCEDURE.


PROCEDURE pi-formata-cgc.
   DEF INPUT  PARAMETER c-cgc-ent AS CHAR.
   DEF OUTPUT PARAMETER c-cgc     AS CHAR.

   IF LENGTH(c-cgc-ent) = 14 THEN
      ASSIGN c-cgc = SUBSTR(c-cgc-ent,  1, 2) + "." + SUBSTR(c-cgc-ent, 3, 3) + "." +
                     SUBSTR(c-cgc-ent,  6, 3) + "/" + SUBSTR(c-cgc-ent, 9, 4) + "-" +
                     SUBSTR(c-cgc-ent, 13, 2).
   ELSE
      ASSIGN c-cgc = SUBSTR(c-cgc-ent,  1, 3) + "." + SUBSTR(c-cgc-ent, 4, 3) + "." +
                     SUBSTR(c-cgc-ent,  7, 3) + "-" + SUBSTR(c-cgc-ent, 10, 2).
END PROCEDURE.

PROCEDURE WinExec EXTERNAL "kernel32.dll":
  DEF INPUT PARAMETER prog_name    AS CHAR.
  DEF INPUT PARAMETER visual_style AS SHORT.
END PROCEDURE.
