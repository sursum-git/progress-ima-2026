/**************/
/** Includes **/
/**************/
{include/tt-edit.i}
{include/pi-edit.i}
{PDFinclude/pdf_inc.i "THIS-PROCEDURE"}

DEF VAR h-acomp     AS HANDLE NO-UNDO.
DEF VAR h-prog      AS HANDLE NO-UNDO.
DEF VAR c-cgc       AS CHAR.
DEF VAR i-lin       AS INT.
DEF VAR i-salto     AS INT INITIAL  6.
DEF VAR i-lin-item  AS INT INITIAL 400.
DEF VAR de-total    AS DEC FORMAT ">>,>>>,>>9.9999".

/* Variaveis da rotina do WORD */
 DEF VAR c-arq-modelo     AS CHAR.  
 DEF VAR c-arq-gerado-pdf AS CHAR.
 DEF VAR c-arq-logo       AS CHAR.

/* Abertura e Cria‡Æo de Arquivo PDF */
 ASSIGN c-arq-modelo     = SEARCH("modelos/modelo.pdf")
        c-arq-gerado-pdf = SESSION:TEMP-DIRECTORY + "ped-compras.pdf"
        c-arq-logo       = SEARCH("image/tear.jpg").

 /* Configura‡äes do Arquivo PDF */
 RUN pdf_new             IN h_PDFinc ("Spdf", c-arq-gerado-pdf).
 RUN pdf_open_pdf        IN h_PDFinc ("Spdf", c-arq-modelo, "Modelo"). 
 RUN pdf_load_font       IN h_PDFinc ("Spdf","TimesBD","c:\WINDOWS\Fonts\timesbd.ttf","modelos\timesbd.afm",""). /* Importando Fonte */
 RUN pdf_load_font       IN h_PDFinc ("Spdf","Arial",  "c:\WINDOWS\Fonts\arial.ttf",  "modelos\arial.afm","").   /* Importando Fonte */
 RUN pdf_set_papertype   IN h_PDFinc ("Spdf","A4").
 RUN pdf_set_orientation IN h_PDFinc ("Spdf","Landscape").
 RUN pdf_new_page        IN h_PDFinc ("Spdf").                     /* Cria Uma Nova Pagina */
 RUN pdf_load_image      IN h_PDFinc ("Spdf","logo",c-arq-logo).   /* Importando logotipo  */

 FOR EACH pedido-compr WHERE  
          pedido-compr.num-pedido >= 36411 AND
          pedido-compr.num-pedido <= 36411 NO-LOCK.

     FIND FIRST ordem-compra WHERE 
                ordem-compra.num-pedido = pedido-compr.num-pedido NO-LOCK. 
     FIND comprador WHERE
          comprador.cod-comprado = ordem-compra.cod-comprado NO-LOCK NO-ERROR.

     FIND estabelec WHERE
          estabelec.cod-estabel = pedido-compr.cod-estabel NO-LOCK NO-ERROR.
     FIND estab-distrib WHERE
          estab-distrib.cod-estab = estabelec.cod-estabel NO-LOCK NO-ERROR.

     FIND emitente WHERE 
          emitente.cod-emitente = pedido.cod-emitente NO-LOCK.

     FIND FIRST cont-emit WHERE
                cont-emit.cod-emitente =  emitente.cod-emitente NO-LOCK NO-ERROR.

     RUN pi-cabec-form.

     ASSIGN de-total    = 0.
     FOR EACH ordem-compra WHERE
              ordem-compra.num-pedido =  pedido.num-pedido AND
              ordem-compra.situacao   <> 4 NO-LOCK,
         EACH prazo-compra WHERE
              prazo-compra.numero-ordem = ordem-compra.numero-ordem AND
              prazo-compra.situacao <> 4 NO-LOCK
           BY (prazo-compra.it-codigo +
               STRING(prazo-compra.parcela)).

         FIND item WHERE
              item.it-codigo = prazo-compra.it-codigo NO-LOCK NO-ERROR.

         RUN pi-imp-linha.

         ASSIGN de-total =  de-total + ((prazo-compra.quantidade * ordem-compra.preco-fornec) +                                       
                                        ((prazo-compra.quantidade * ordem-compra.preco-fornec) * ordem-compra.aliquota-ipi * 0.01) +  
                                        ((prazo-compra.quantidade * ordem-compra.preco-fornec) * ordem-compra.valor-taxa * 0.01) -    
                                        ((prazo-compra.quantidade * ordem-compra.preco-fornec) * ordem-compra.perc-descto * 0.01)).   

     END.
     RUN pi-rodape.
 END.

 RUN pdf_close("Spdf"). /* Fechar Arquivo pdf */


 /* P  R  O  C  E  D  I  M  E  N  T  O  S */
 /* ------------------------------------- */

 PROCEDURE pi-desenha-linha.

   /* Em Um Formulario A4 Com Orienta‡Æo PAISAGEM:
         Coluna Inicial =  15
         Coluna Final   = 830
         Linha  Inicial = 595
         Linha  Final   =  10 */

   DEF INPUT PARAMETER i-col-ini AS INT.
   DEF INPUT PARAMETER i-lin-ini AS INT.
   DEF INPUT PARAMETER i-col-fin AS INT.
   DEF INPUT PARAMETER i-lin-fin AS INT.
   DEF INPUT PARAMETER i-denso   AS INT.

   RUN pdf_line ("Spdf", i-col-ini,  /* Coluna Inicial */
                         i-lin-ini,  /* Linha Inicial */
                         i-col-fin,  /* Coluna Final */
                         i-lin-fin,  /* Linha Final */
                         i-denso). /* Espessura da Linha */

 END PROCEDURE.

 PROCEDURE pi-imp-linha.

     IF i-lin <= 0 THEN
        RETURN.
     RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
     RUN pdf_text_xy  IN h_PDFinc ("Spdf", prazo-compra.it-codigo,28,i-lin).
     RUN pdf_text_xy  IN h_PDFinc ("Spdf", item.desc-item,95,i-lin)  .
     RUN pdf_text_xy  IN h_PDFinc ("Spdf", prazo-compra.quantidade,387,i-lin)  .
     RUN pdf_text_xy  IN h_PDFinc ("Spdf", prazo-compra.un,422,i-lin)  .
     RUN pdf_text_xy  IN h_PDFinc ("Spdf", STRING(ordem-compra.preco-fornec,">>>,>>9.9999"),475,i-lin)  .
     RUN pdf_text_xy  IN h_PDFinc ("Spdf", STRING(ordem-compra.aliquota-ipi, ">9.99") + "%",540,i-lin)  .
     RUN pdf_text_xy  IN h_PDFinc ("Spdf", (STRING(((prazo-compra.quantidade * ordem-compra.preco-fornec) +                                           
                                           ((prazo-compra.quantidade * ordem-compra.preco-fornec) * ordem-compra.aliquota-ipi * 0.01) +              
                                           ((prazo-compra.quantidade * ordem-compra.preco-fornec) * ordem-compra.valor-taxa * 0.01) -                
                                           ((prazo-compra.quantidade * ordem-compra.preco-fornec) * ordem-compra.perc-descto * 0.01)),">>>>,>>9.99")),600,i-lin).
     RUN pdf_text_xy  IN h_PDFinc ("Spdf", STRING(prazo-compra.data-entrega, "99/99/9999") + "%",655,i-lin)  .
     RUN pdf_text_xy  IN h_PDFinc ("Spdf", ordem-compra.requisitante,730,i-lin)  .
     
     
     RUN pi-print-editor(INPUT ordem-compra.narrativa, INPUT 65).
     FIND FIRST tt-editor NO-LOCK NO-ERROR.
     IF AVAIL tt-editor AND tt-editor.conteudo <> ""  THEN DO:
        FOR EACH tt-editor.
            IF tt-editor.conteudo <> "" THEN DO:
               ASSIGN i-lin = i-lin - 10.
               IF i-lin > 0 THEN
                  RUN pdf_text_xy IN h_PDFinc ("Spdf", tt-editor.conteudo,95,i-lin).
            END.
        END.
        RUN pi-linha-item.
     END.
     ELSE DO:
        RUN pi-linha-item.
        ASSIGN i-lin = i-lin - 10.
     END.
     
     

 END PROCEDURE.

 PROCEDURE pi-linha-item.


   IF i-lin <= 0 THEN
      RETURN.


   RUN pi-desenha-linha (15,(i-lin - i-salto),830,(i-lin - i-salto),1). /* Tra‡o Horizontal Apos ImpressÆo do ITEM */
   ASSIGN i-lin = i-lin - i-salto.
   RUN pi-desenha-linha ( 15,i-lin-item, 15,i-lin,1). /*  1§ Tra‡o do Cabe‡alho de Item */
   RUN pi-desenha-linha ( 90,i-lin-item, 90,i-lin,1). /*  2§ Tra‡o do Cabe‡alho de Item */
   RUN pi-desenha-linha (370,i-lin-item,370,i-lin,1). /*  3§ Tra‡o do Cabe‡alho de Item */
   RUN pi-desenha-linha (410,i-lin-item,410,i-lin,1). /*  4§ Tra‡o do Cabe‡alho de Item */
   RUN pi-desenha-linha (455,i-lin-item,455,i-lin,1). /*  5§ Tra‡o do Cabe‡alho de Item */
   RUN pi-desenha-linha (525,i-lin-item,525,i-lin,1). /*  6§ Tra‡o do Cabe‡alho de Item */
   RUN pi-desenha-linha (570,i-lin-item,570,i-lin,1). /*  7§ Tra‡o do Cabe‡alho de Item */
   RUN pi-desenha-linha (640,i-lin-item,640,i-lin,1). /*  8§ Tra‡o do Cabe‡alho de Item */
   RUN pi-desenha-linha (725,i-lin-item,725,i-lin,1). /*  9§ Tra‡o do Cabe‡alho de Item */
   RUN pi-desenha-linha (830,i-lin-item,830,i-lin,1). /* 10§ Tra‡o do Cabe‡alho de Item */

   ASSIGN i-lin-item = i-lin
          i-lin      = i-lin - 10.

   IF i-lin-item < 30 THEN DO:
      RUN pdf_new_page IN h_PDFinc ("Spdf"). /* Cria Uma Nova Pagina */
      RUN pi-cabec-form.
   END.

 END PROCEDURE.


 PROCEDURE pi-cabec-form.

   ASSIGN i-lin-item  = 400
          i-lin       = 370.

   /* Cria‡Æo dos Retangulos do Cabe‡alho */
   RUN pdf_rect2 ("Spdf",15,500,815, 90,1). /* 15=Col Ini 500=Lin Ini 815=Col Fin 90=Altura 1=Espessura da Linha */
   RUN pdf_rect2 ("Spdf",15,400,815,100,1). 
   RUN pdf_rect2 ("Spdf",15,380,815, 20,1). 

   /* Cria‡Æo das Linhas que dividem os Retangulos */
   RUN pi-desenha-linha (445,590,445,400,1). /* Tra‡o Vertical que Divide ao meio 0 1§ e 2§ Retangulo */
   RUN pi-desenha-linha (445,450,830,450,1). /* Tra‡o Horizontal que Divide 0 2§ Retangulo */

   /* Criar Hachuras */
   RUN pdf_text_Color("Spdf",.3,0.4,.5).
   RUN pdf_rect ("Spdf",445,400,385, 50,1). 
   RUN pdf_text_Color("Spdf",0,0,0).

   /* Logotipo e Dados da Empresa */
   RUN pdf_place_image IN h_PDFinc ("Spdf","logo",18,93,150,86). /* Logotipo */
   RUN pi-formata-cgc (INPUT estabelec.cgc, OUTPUT c-cgc).
   RUN pdf_set_font IN h_PDFinc ("Spdf", "Times-Bold", 12).
   RUN pdf_text_xy IN h_PDFinc ("Spdf", estabelec.nome   ,165,570).
   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold", 12).
   RUN pdf_text_xy IN h_PDFinc ("Spdf", "CNPJ: " + c-cgc ,165,550).
   RUN pdf_text_xy IN h_PDFinc ("Spdf", "INSCRI€ÇO ESTADUAL: " + estabelec.ins-estadual ,165,530).
   RUN pdf_text_xy IN h_PDFinc ("Spdf", "www.teartextil.com.br",165,510).

   /* Endere‡o da Empresa e Dados do Comprador */
   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica", 8).
   RUN pdf_text_xy IN h_PDFinc ("Spdf", TRIM(estabelec.endereco) + " - " +
                                        TRIM(estabelec.bairro)  +
                                        " CEP: " + STRING(estabelec.cep, "99999,999")  + " - " + 
                                        TRIM(estabelec.cidade) + "/" +
                                        TRIM(estabelec.estado),450,580).
   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica", 9).
   RUN pdf_text_xy IN h_PDFinc ("Spdf", "Tel./Fax: " + comprador.telefone[2] + " / " +
                                        comprador.telefax,450,560).
   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold", 10).
   RUN pdf_text_xy IN h_PDFinc ("Spdf", "Comprador: " + comprador.nome,450,530).
   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica", 9).
   RUN pdf_text_xy IN h_PDFinc ("Spdf", "Fone: " + comprador.telefone[1] + " - " +
                                        "E-mail: " + comprador.e-mail,450,510).
   /* Endere‡o e Dados do Fornecedor */
   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica", 10).
   RUN pdf_text_xy IN h_PDFinc ("Spdf", "Fornecedor: ",20,485).
   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold", 12).
   RUN pdf_text_xy IN h_PDFinc ("Spdf", STRING(emitente.cod-emitente, ">>9,999") + " - " + emitente.nome-emit,70,485).
   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).
   RUN pdf_text_xy IN h_PDFinc ("Spdf", "Endere‡o: ",28,468).
   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica", 9).
   RUN pdf_text_xy IN h_PDFinc ("Spdf", emitente.endereco + " - " + emitente.bairro + " - " +                 
                                        emitente.cidade + " - " + emitente.estado + " - " + "Cep: " + STRING(emitente.cep),77,468).
   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).
   RUN pdf_text_xy IN h_PDFinc ("Spdf", "Tel./Fax: ",35,451).
   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold", 10).
   RUN pdf_text_xy IN h_PDFinc ("Spdf", emitente.telefone[1] + " / " + emitente.telefax,77,451).
   RUN pi-formata-cgc (INPUT emitente.cgc, OUTPUT c-cgc).
   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).
   RUN pdf_text_xy IN h_PDFinc ("Spdf", "CNPJ: ",265,451).
   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold", 10).
   RUN pdf_text_xy IN h_PDFinc ("Spdf", c-cgc,298,451).
   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).
   RUN pdf_text_xy IN h_PDFinc ("Spdf", "Contato: ",37,434).
   RUN pdf_text_xy IN h_PDFinc ("Spdf", "INSC.EST: ",245,434).
   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold", 10).
   RUN pdf_text_xy IN h_PDFinc ("Spdf", IF AVAIL cont-emit       
                                        THEN cont-emit.nome ELSE "",79,434).
   RUN pdf_text_xy IN h_PDFinc ("Spdf", emitente.ins-estadual, 298,434).       
   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).
   RUN pdf_text_xy IN h_PDFinc ("Spdf", "E-mail: ",45,412).
   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold", 11).
   RUN pdf_text_xy IN h_PDFinc ("Spdf", emitente.e-mail, 79,412).       
   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",14).

   /* Dados do Pedido de Compra */
   RUN pdf_text_xy IN h_PDFinc ("Spdf", "PEDIDO N§: ",450,480).
   RUN pdf_text_xy IN h_PDFinc ("Spdf", "DATA: ",486,457).
   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold", 14).
   RUN pdf_text_xy IN h_PDFinc ("Spdf", STRING(pedido-compr.num-pedido, ">>>9,999"), 525,480).       
   RUN pdf_text_xy IN h_PDFinc ("Spdf", pedido-compr.data-pedido, 533,457).       
   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica", 27).
   RUN pdf_text_Color("Spdf",1,1,1).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf", "CONFIRMA€ÇO DE COMPRA" ,450,415).
   RUN pdf_text_Color("Spdf",0,0,0).

   /* Montagem do Cabe‡alho do Item */
   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica", 12).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf", "C¢digo"       , 25,385).
   RUN pi-desenha-linha ( 90,400, 90,380,1). /* 1§ Tra‡o do Cabe‡alho de Item */
   RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Descri‡Æo"    , 95,385).
   RUN pi-desenha-linha (370,400,370,380,1). /* 2§ Tra‡o do Cabe‡alho de Item */
   RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Qtde"   ,375,385).
   RUN pi-desenha-linha (410,400,410,380,1). /* 3§ Tra‡o do Cabe‡alho de Item */
   RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Und",420,385).
   RUN pi-desenha-linha (455,400,455,380,1). /* 4§ Tra‡o do Cabe‡alho de Item */
   RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Pre‡o",475,385).
   RUN pi-desenha-linha (525,400,525,380,1). /* 5§ Tra‡o do Cabe‡alho de Item */
   RUN pdf_text_xy  IN h_PDFinc ("Spdf", "IPI",540,385).
   RUN pi-desenha-linha (570,400,570,380,1). /* 6§ Tra‡o do Cabe‡alho de Item */
   RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Valor Item",575,385).
   RUN pi-desenha-linha (640,400,640,380,1). /* 7§ Tra‡o do Cabe‡alho de Item */
   RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Data Entrega",645,385).
   RUN pi-desenha-linha (725,400,725,380,1). /* 8§ Tra‡o do Cabe‡alho de Item */
   RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Requisitante" ,730,385).

 END PROCEDURE.

 PROCEDURE pi-rodape.

   /* Cria‡Æo dos Retangulos do Cabe‡alho */
   RUN pdf_rect2 ("Spdf",15,i-lin-item - 100,815,100,1). /* Col Ini, Lin Ini, Col Fin, Altura, Espessura da Linha */

   RUN pi-desenha-linha (455,i-lin-item     ,455,i-lin-item - 100,1).  /* Tra‡o Vertical */ 
   RUN pi-desenha-linha (455,i-lin-item - 20,830,i-lin-item -  20,1).  /* 1¦ Linha Horizontal */
   RUN pi-desenha-linha (455,i-lin-item - 40,830,i-lin-item -  40,1).  /* 2¦ Linha Horizontal */
   RUN pi-desenha-linha (455,i-lin-item - 60,830,i-lin-item -  60,1).  /* 3¦ Linha Horizontal */
   RUN pi-desenha-linha (455,i-lin-item - 80,830,i-lin-item -  80,1).  /* 4¦ Linha Horizontal */
   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold", 12).
   RUN pdf_text_xy IN h_PDFinc ("Spdf", "OBS.:", 25,i-lin-item - 20).       
   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).
   RUN pdf_text_xy IN h_PDFinc  ("Spdf", pedido-compr.comentarios, 65,i-lin-item - 20).       

   /* Criar Hachuras */
   RUN pdf_text_Color("Spdf",.3,0.4,.5).
   RUN pdf_rect ("Spdf",455,i-lin-item - 20,375, 20,1). 
   RUN pdf_text_Color("Spdf",1,1,1).
   RUN pdf_text_xy IN h_PDFinc ("Spdf", "TOTAL  R$",460,i-lin-item - 15).
   RUN pdf_text_xy IN h_PDFinc  ("Spdf", STRING(de-total,">>>,>>>,>>9.99"),500,i-lin-item - 15).       
   RUN pdf_text_Color("Spdf",0,0,0).
   RUN pdf_text_xy IN h_PDFinc ("Spdf", "PAGAMENTO",460,i-lin-item - 33).
   RUN pdf_text_xy IN h_PDFinc  ("Spdf", IF AVAIL cond-pagto      
                                         THEN cond-pagto.descricao ELSE "",500,i-lin-item - 33).
   RUN pdf_text_xy IN h_PDFinc ("Spdf", "ENTREGA",460,i-lin-item - 52).
   RUN pdf_text_xy IN h_PDFinc  ("Spdf", pedido-compr.compl-entrega,500,i-lin-item - 52).
   RUN pi-desenha-linha (530,i-lin-item - 20,530,i-lin-item - 60,1).  /* Tra‡o Vertical */ 
   RUN pdf_text_xy IN h_PDFinc ("Spdf", "TRANSPORTE/FRETE",460,i-lin-item - 73).
   RUN pdf_text_xy IN h_PDFinc  ("Spdf", IF pedido-compr.frete = 1 
                                         THEN "CIF" ELSE "FOB",600,i-lin-item - 73).
   RUN pdf_text_xy IN h_PDFinc ("Spdf", "TRANSPORTADORA/FONE",460,i-lin-item - 93).
   RUN pdf_text_xy IN h_PDFinc  ("Spdf", IF AVAIL transporte          
                                         THEN transporte.nome + " / " + transporte.telefone ELSE "" ,600,i-lin-item - 93).
   RUN pi-desenha-linha (595,i-lin-item - 60,595,i-lin-item - 100,1). /* Tra‡o Vertical */ 
   RUN pi-desenha-linha ( 15, 13,250,13,1). /* Tra‡o Horizontal */
   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).
   RUN pdf_text_xy IN h_PDFinc ("Spdf", "Gerˆncia de Compras",75,3).

 END PROCEDURE.


 PROCEDURE pi-copia-rodape.

     /* Cria‡Æo dos Retangulos do Cabe‡alho */
     RUN pdf_rect2 ("Spdf",15,60,815,100,1). /* 15=Col Ini 60=Lin Ini 815=Col Fin 80=Altura 1=Espessura da Linha */
     RUN pi-desenha-linha (455,160,455, 60,1).  /* Tra‡o Vertical */ 
     RUN pi-desenha-linha (455,140,830,140,1).  /* 1¦ Linha Horizontal */
     RUN pi-desenha-linha (455,120,830,120,1).  /* 2¦ Linha Horizontal */
     RUN pi-desenha-linha (455,100,830,100,1).  /* 3¦ Linha Horizontal */
     RUN pi-desenha-linha (455, 80,830, 80,1).  /* 4¦ Linha Horizontal */
     RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold", 12).
     RUN pdf_text_xy IN h_PDFinc ("Spdf", "OBS.:", 25,140).       

     /* Criar Hachuras */
     RUN pdf_text_Color("Spdf",.3,0.4,.5).
     RUN pdf_rect ("Spdf",455,140,375, 20,1). 
     RUN pdf_text_Color("Spdf",1,1,1).

     RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).
     RUN pdf_text_xy IN h_PDFinc ("Spdf", "TOTAL  R$",460,145).
     RUN pdf_text_Color("Spdf",0,0,0).
     RUN pdf_text_xy IN h_PDFinc ("Spdf", "PAGAMENTO",460,125).
     RUN pdf_text_xy IN h_PDFinc ("Spdf", "ENTREGA",460,105).
     RUN pi-desenha-linha (530,140,530,100,1).  /* Tra‡o Vertical */ 
     RUN pdf_text_xy IN h_PDFinc ("Spdf", "TRANSPORTE/FRETE",460,85).
     RUN pdf_text_xy IN h_PDFinc ("Spdf", "TRANSPORTADORA/FONE",460,65).
     RUN pi-desenha-linha (595,100,595,60,1). /* Tra‡o Vertical */ 
     RUN pi-desenha-linha ( 15, 13,250,13,1). /* Tra‡o Horizontal */
     RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).
     RUN pdf_text_xy IN h_PDFinc ("Spdf", "Gerˆncia de Compras",75,3).


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
