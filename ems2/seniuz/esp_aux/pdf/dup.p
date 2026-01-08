/**************/
/** Includes **/
/**************/
{include/tt-edit.i}
{include/pi-edit.i}
{PDFinclude/pdf_inc.i "THIS-PROCEDURE"}

 DEF VAR c-reconhecimento AS CHAR EXTENT 3.
 DEF VAR c-cgc            AS CHAR.
 DEF VAR c-arq-modelo     AS CHAR.  
 DEF VAR c-arq-gerado-pdf AS CHAR.
 DEF VAR c-arq-logo       AS CHAR.
 DEF VAR c-cod-barra      AS CHAR INITIAL "1543742".
 DEF VAR i-num-bar        AS INT.

/* Abertura e Criaá∆o de Arquivo PDF */
 ASSIGN c-arq-modelo     = SEARCH("modelos/modelo.pdf")
        c-arq-gerado-pdf = SESSION:TEMP-DIRECTORY + "duplicata.pdf"
        c-arq-logo       = SEARCH("image/tear.jpg").

 ASSIGN c-reconhecimento[1] = "Reconheáo(emos) a exatid∆o desta DUPLICATA DE VENDA MERCANTIL, na importÉncia acima, que pagarei Ö TEAR TEXTIL IND. E COM. LTDA., ou Ö sua" 
        c-reconhecimento[2] = "ordem na praáa e vencimento indicados e por qualquer atraso que ocorrer, pagarei(emos) juros legais e correá∆o monet†ria no caso de cobranáa judiciais"
        c-reconhecimento[3] = "adicionado de custas judiciais e honor†rios advocat°cios".

 /* Configuraá‰es do Arquivo PDF */
 RUN pdf_new             IN h_PDFinc ("Spdf", c-arq-gerado-pdf).
 RUN pdf_open_pdf        IN h_PDFinc ("Spdf", c-arq-modelo, "Modelo"). 
 RUN pdf_load_font       IN h_PDFinc ("Spdf","TimesBD","c:\WINDOWS\Fonts\timesbd.ttf","modelos\timesbd.afm",""). /* Importando Fonte */
 RUN pdf_load_font       IN h_PDFinc ("Spdf","Arial",  "c:\WINDOWS\Fonts\arial.ttf",  "modelos\arial.afm","").   /* Importando Fonte */
 RUN pdf_load_font       IN h_PDFinc ("Spdf","cbarra25",  "c:\WINDOWS\Fonts\cbarra25.ttf",  "modelos\cbarra25.afm","").   /* Importando Fonte */

 RUN pdf_set_papertype   IN h_PDFinc ("Spdf","A4").
/* RUN pdf_set_orientation IN h_PDFinc ("Spdf","Portrait"). */
 RUN pdf_new_page        IN h_PDFinc ("Spdf").                     /* Cria Uma Nova Pagina */
 RUN pdf_load_image      IN h_PDFinc ("Spdf","logo",c-arq-logo).   /* Importando logotipo  */

 RUN pi-gera-duplic (INPUT 0).
 RUN pi-gera-duplic (INPUT 450).



 RUN pdf_close("Spdf"). /* Fechar Arquivo pdf */ 


 /* P  R  O  C  E  D  I  M  E  N  T  O  S */
 /* ------------------------------------- */

 PROCEDURE pi-desenha-linha.

   /* Em Um Formulario A4 Com Orientaá∆o RETRATO(Portrait):
         Coluna Inicial =  15
         Coluna Final   = 582
         Linha  Inicial = 772
         Linha  Final   =  10 */

   DEF INPUT PARAMETER p-col-ini AS INT.
   DEF INPUT PARAMETER p-lin-ini AS INT.
   DEF INPUT PARAMETER p-col-fin AS INT.
   DEF INPUT PARAMETER p-lin-fin AS INT.
   DEF INPUT PARAMETER p-denso   AS INT.

   RUN pdf_line ("Spdf", p-col-ini,  /* Coluna Inicial */
                         p-lin-ini,  /* Linha Inicial */
                         p-col-fin,  /* Coluna Final */
                         p-lin-fin,  /* Linha Final */
                         p-denso).   /* Espessura da Linha */

 END PROCEDURE.

 PROCEDURE pi-gera-duplic.

   DEF INPUT PARAMETER p-lin AS INT.

   DEF VAR i-pula-lin AS INT.

   IF p-lin = 0 THEN
      ASSIGN i-pula-lin = 0.
   ELSE
       ASSIGN i-pula-lin = 33.

   /* Criaá∆o do Retangulo Com os Dados da Empresa */
   RUN pdf_rect2       IN h_PDFinc ("Spdf",5,755 - p-lin,585,80,1). /* 1ß Retangulo */
   RUN pdf_place_image IN h_PDFinc ("Spdf","logo",6,81 + p-lin,178,73). /* Logotipo */
   RUN pi-desenha-linha (180,755 - p-lin,180,835 - p-lin,1). /* Traáo Vertical SEPARADOR do Logotipo */
   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica", 10).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Endereáo: "            ,187,825 - p-lin).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf", "CEP: "                 ,187,812 - p-lin).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Mun°cipio: "           ,300,812 - p-lin).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf", "UF:: "                 ,487,812 - p-lin).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf", "CNPJ: "                ,187,799 - p-lin).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Inscr.Est.: "          ,380,799 - p-lin).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Natureza de Operaá∆o: ",187,786 - p-lin).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Via de Transporte: "   ,187,773 - p-lin).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Data de Emiss∆o: "     ,187,760 - p-lin).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Nß do Pedido: "        ,380,760 - p-lin).

   /* Criaá∆o do Retangulo com os dados da Fatura */
   RUN pdf_rect2    IN h_PDFinc ("Spdf",80,675 - p-lin,390,65,1).  /* 2ß Retangulo (Dados Fatura) */
   RUN pi-desenha-linha (80,708 - p-lin,470,708 - p-lin,1). /* Traáo Horizontal dos Dados Fatura */
   RUN pdf_text_xy  IN h_PDFinc ("Spdf", "FATURA Nß",100,720 - p-lin).
   RUN pi-desenha-linha (180,740 - p-lin,180,675 - p-lin,1). /* 1ß Traáo Vertical Dados Fatura */
   RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Fatura - Duplicata",189,728 - p-lin).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf", "VALOR",214,714 - p-lin).
   RUN pi-desenha-linha (280,740 - p-lin,280,675 - p-lin,1). /* 2ß Traáo Vertical Dados Fatura */
   RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Duplicata",310,728 - p-lin).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Nß de Ordem",300,714 - p-lin).
   RUN pi-desenha-linha (380,740 - p-lin,380,675 - p-lin,1). /* 3ß Traáo Vertical Dados Fatura */
   RUN pdf_text_xy  IN h_PDFinc ("Spdf", "VENCIMENTO",390,720 - p-lin).

   /* Criaá∆o do Retangulo para uso da Instituiá∆o Financeira */
   RUN pdf_rect2 ("Spdf",481,650 - p-lin,108,90,1). /* 3ß Retangulo */
   RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Para Uso da Instituiá∆o",483,730 - p-lin).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Financeira",510,720 - p-lin).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Desconto de:",85,665 - p-lin).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Condiá‰es Especiais:",85,653 - p-lin).

   /* Criaá∆o do Retangulo com os dados do Cliente */
   RUN pdf_rect2 ("Spdf",80,538 - p-lin,508,107,1).  /* 4ß Retangulo (Dados do Cliente) */
   RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Nome do Sacado: ",85,633 - p-lin).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Endereáo: ",85,620 - p-lin).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Munic°pio: ",85,607 - p-lin).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Praáa de Pagto: ",85,594 - p-lin).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf", "CNPJ: ",85,581 - p-lin).








   /* Retangulo do Extenso */
   RUN pi-desenha-linha (80,576 - p-lin,588,576 - p-lin,1). /* Traáo Horizontal (EXTENSO) */
   RUN pdf_text_xy  IN h_PDFinc ("Spdf", "VALOR"  , 91,563 - p-lin).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf", "POR"    , 97,553 - p-lin).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf", "EXTENSO", 85,543 - p-lin).
   /* Criar Hachuras para Extenso */
   RUN pdf_text_Color IN h_PDFinc ("Spdf",.3,0.4,.5).
   RUN pdf_rect       IN h_PDFinc ("Spdf",140,539.15 - p-lin,447, 35.60,1). 
   RUN pdf_text_Color IN h_PDFinc ("Spdf",0,0,0).
   RUN pdf_text_Color IN h_PDFinc ("Spdf",1,1,1).
   RUN pdf_text_xy    IN h_PDFinc ("Spdf", "Vinte e Oito Mil, Oitocentos e Cinquenta e Cinco Reais e Setenta Centavos *************************" ,145,563 - p-lin).
   RUN pdf_text_xy    IN h_PDFinc ("Spdf", "Vinte e Oito Mil, Oitocentos e Cinquenta e Cinco Reais e Setenta Centavos *************************" ,145,553 - p-lin).
   RUN pdf_text_xy    IN h_PDFinc ("Spdf", "Vinte e Oito Mil, Oitocentos e Cinquenta e Cinco Reais e Setenta Centavos *************************" ,145,543 - p-lin).
   RUN pdf_text_Color IN h_PDFinc ("Spdf",0,0,0).

   /* Reconhecimento Mercantil */
   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica", 7).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf", c-reconhecimento[1],80,530 - p-lin).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf", c-reconhecimento[2],80,522 - p-lin).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf", c-reconhecimento[3],80,515 - p-lin).
   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica", 10).

   /* Ultimo Retangulo */
   RUN pdf_rect2    IN h_PDFinc ("Spdf",5,460 - p-lin,585,45,1). /* Ultimo Retangulo */
   RUN pi-desenha-linha (250,476 - p-lin,550,476 - p-lin,1). /* Traáo Horizontal dos Dados Fatura */
   RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Assinatura do Sacado",345,467 - p-lin).
   RUN pi-desenha-linha (30,476 - p-lin,200,476 - p-lin,1). /* Traáo Horizontal dos Dados Fatura */
   RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Data de Aceite",80,467 - p-lin).
   RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Em ",10,475 - p-lin).

   /* Impress∆o de Dados Verticais */
   RUN pdf_skipn       IN h_PDFinc ("Spdf",27 + i-pula-lin).
   RUN pdf_text_rotate IN h_PDFinc ("Spdf",0).
   RUN pdf_text        IN h_PDFinc ("Spdf", FILL(" ",5)).
   RUN pdf_text_rotate IN h_PDFinc ("Spdf",270).
   RUN pdf_text        IN h_PDFinc ("Spdf", "TEAR TEXTIL INDUSTRIA E COMERCIO LTDA").
   RUN pi-desenha-linha (60,745 - p-lin,60,510 - p-lin,1). /* Traáo Vertical da Assinatura */
   RUN pdf_set_font    IN h_PDFinc ("Spdf", "Helvetica", 8).
   RUN pdf_skipn       IN h_PDFinc ("Spdf",19).
   RUN pdf_text_rotate IN h_PDFinc ("Spdf",0).
   RUN pdf_text        IN h_PDFinc ("Spdf", FILL(" ",27)).
   RUN pdf_text_rotate IN h_PDFinc ("Spdf",270).
   RUN pdf_text        IN h_PDFinc ("Spdf", "Assinatura do Emitente").

   /* Linha Final na 1¶ Duplicata */
   IF p-lin = 0 THEN
      RUN pdf_text_xy  IN h_PDFinc ("Spdf", FILL(".",264),5,418 - p-lin).
   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica", 10).
   
 END PROCEDURE.

 PROCEDURE pi-rodape.

     /* Criaá∆o do Retangulo Com os Dados da Empresa */
     RUN pdf_rect2 ("Spdf",5,755,585,80,1). /* 1ß Retangulo */
     RUN pdf_place_image IN h_PDFinc ("Spdf","logo",6,81,178,73). /* Logotipo */
     RUN pi-desenha-linha (180,755,180,835,1). /* Traáo Vertical SEPARADOR do Logotipo */
     RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica", 10).
     RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Endereáo: "            ,187,825).
     RUN pdf_text_xy  IN h_PDFinc ("Spdf", "CEP: "                 ,187,812).
     RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Mun°cipio: "           ,300,812).
     RUN pdf_text_xy  IN h_PDFinc ("Spdf", "UF:: "                 ,487,812).
     RUN pdf_text_xy  IN h_PDFinc ("Spdf", "CNPJ: "                ,187,799).
     RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Inscr.Est.: "          ,380,799).
     RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Natureza de Operaá∆o: ",187,786).
     RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Via de Transporte: "   ,187,773).
     RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Data de Emiss∆o: "     ,187,760).
     RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Nß do Pedido: "        ,380,760).
     RUN pdf_rect2 ("Spdf",80,675,390,65,1).  /* 2ß Retangulo (Dados Fatura) */
     RUN pi-desenha-linha (80,708,470,708,1). /* Traáo Horizontal dos Dados Fatura */
     RUN pdf_text_xy  IN h_PDFinc ("Spdf", "FATURA Nß",100,720).
     RUN pi-desenha-linha (180,740,180,675,1). /* 1ß Traáo Vertical Dados Fatura */
     RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Fatura - Duplicata",189,728).
     RUN pdf_text_xy  IN h_PDFinc ("Spdf", "VALOR",214,714).
     RUN pi-desenha-linha (280,740,280,675,1). /* 2ß Traáo Vertical Dados Fatura */
     RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Duplicata",310,728).
     RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Nß de Ordem",300,714).
     RUN pi-desenha-linha (380,740,380,675,1). /* 3ß Traáo Vertical Dados Fatura */
     RUN pdf_text_xy  IN h_PDFinc ("Spdf", "VENCIMENTO",390,720).
     RUN pdf_rect2 ("Spdf",481,650,108,90,1). /* 3ß Retangulo */
     RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Para Uso da Instituiá∆o",483,730).
     RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Financeira",510,720).
     RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Desconto de:",85,665).
     RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Condiá‰es Especiais:",85,653).
     RUN pdf_rect2 ("Spdf",80,538,508,107,1).  /* 4ß Retangulo (Dados do Cliente) */
     RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Nome do Sacado: ",85,633).
     RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Endereáo: ",85,620).
     RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Munic°pio: ",85,607).
     RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Praáa de Pagto: ",85,594).
     RUN pdf_text_xy  IN h_PDFinc ("Spdf", "CNPJ: ",85,581).
     RUN pi-desenha-linha (80,576,588,576,1). /* Traáo Horizontal (EXTENSO) */
     RUN pdf_text_xy  IN h_PDFinc ("Spdf", "VALOR"  , 91,563).
     RUN pdf_text_xy  IN h_PDFinc ("Spdf", "POR"    , 97,553).
     RUN pdf_text_xy  IN h_PDFinc ("Spdf", "EXTENSO", 85,543).
     /* Criar Hachuras */
     RUN pdf_text_Color("Spdf",.3,0.4,.5).
     RUN pdf_rect ("Spdf",140,539.15,447, 35.60,1). 
     RUN pdf_text_Color("Spdf",0,0,0).
     RUN pdf_text_Color("Spdf",1,1,1).
     RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Vinte e Oito Mil, Oitocentos e Cinquenta e Cinco Reais e Setenta Centavos *************************" ,145,563).
     RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Vinte e Oito Mil, Oitocentos e Cinquenta e Cinco Reais e Setenta Centavos *************************" ,145,553).
     RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Vinte e Oito Mil, Oitocentos e Cinquenta e Cinco Reais e Setenta Centavos *************************" ,145,543).
     RUN pdf_text_Color("Spdf",0,0,0).
     RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica", 7).
     RUN pdf_text_xy  IN h_PDFinc ("Spdf", c-reconhecimento[1],80,530).
     RUN pdf_text_xy  IN h_PDFinc ("Spdf", c-reconhecimento[2],80,522).
     RUN pdf_text_xy  IN h_PDFinc ("Spdf", c-reconhecimento[3],80,515).
     RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica", 10).
     RUN pdf_rect2 ("Spdf",5,460,585,45,1). /* Ultimo Retangulo */
     RUN pi-desenha-linha (250,476,550,476,1). /* Traáo Horizontal dos Dados Fatura */
     RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Assinatura do Sacado",345,467).
     RUN pi-desenha-linha (30,476,200,476,1). /* Traáo Horizontal dos Dados Fatura */
     RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Data de Aceite",80,467).
     RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Em ",10,475).
     RUN pdf_skipn("Spdf",27).
     RUN pdf_text_rotate ("Spdf",0).
     RUN pdf_text ("Spdf", FILL(" ",5)).
     RUN pdf_text_rotate IN h_PDFinc ("Spdf",270).
     RUN pdf_text        IN h_PDFinc ("Spdf", "TEAR TEXTIL INDUSTRIA E COMERCIO LTDA").
     RUN pi-desenha-linha (60,745,60,510,1). /* Traáo Vertical da Assinatura */
     RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica", 8).
     RUN pdf_skipn("Spdf",19).
     RUN pdf_text_rotate ("Spdf",0).
     RUN pdf_text ("Spdf", FILL(" ",27)).
     RUN pdf_text_rotate IN h_PDFinc ("Spdf",270).
     RUN pdf_text        IN h_PDFinc ("Spdf", "Assinatura do Emitente").
     RUN pdf_text_xy  IN h_PDFinc ("Spdf", FILL(".",264),5,445).
     RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica", 10).

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

/* Funá∆o para Imprimir Barras em C¢digo ITF (Intercalado 2 de 5) */
FUNCTION fn-code25 RETURNS CHARACTER
   (INPUT posicaoX AS INTEGER, input posicaoY AS INTEGER, INPUT texto AS CHARACTER, INPUT sentido AS CHAR, INPUT altura AS DECIMAL, INPUT largura AS DECIMAL):
    define variable heigth    as decimal.
    define variable small-bar as decimal.
    define variable wide-bar  as decimal.
    define variable dpl       as decimal.
    define variable i-ct         as integer.
    define variable j         as integer.

    define variable inicio    as character.
    define variable fim       as character.
    define variable pp        as character.
    define variable pg        as character.
    define variable bp        as character.
    define variable bg        as character.
    define variable chars     as character.
    define variable cbarra    as character extent 10.
    define variable barras    as character.

    DEF VAR num-1 AS CHAR.
    DEF VAR num-2 AS CHAR.

    ASSIGN chars  = '1234567890'
           inicio = '~033*p-50Y'
           fim    = '~033*p+50Y'.

    ASSIGN heigth    = altura                     /* Altura das linhas */
           small-bar = largura                    /* Numero de pontos da linha */
           wide-bar  = ROUND(small-bar * 3, 0)    /* Numero de pontos da barra */
           dpl       = 50.                        /* Pontos por linha 300 dpi/6lpi = 50dpl */

    ASSIGN pp = '~033*c'  +  STRING(small-bar, "99") + 'a' + STRING(heigth * dpl) + 'b0p~033*p+' + STRING(small-bar, "99") + "X"
           pg = '~033*c'  +  STRING(wide-bar,  "99") + 'a' + STRING(heigth * dpl) + 'b0p~033*p+' + STRING(wide-bar,  "99") + "X"
           bp = '~033*p+' +  STRING(small-bar, "99") + "X"
           bg = '~033*p+' +  STRING(wide-bar,  "99") + "X".

    ASSIGN cbarra[01] = "GPPPG" /* 1 */ 
           cbarra[02] = "PGPPG" /* 2 */
           cbarra[03] = "GGPPP" /* 3 */
           cbarra[04] = "PPGPG" /* 4 */
           cbarra[05] = "GPGPP" /* 5 */
           cbarra[06] = "PGGPP" /* 6 */
           cbarra[07] = "PPPGG" /* 7 */
           cbarra[08] = "GPPGP" /* 8 */
           cbarra[09] = "PGPGP" /* 9 */
           cbarra[10] = "PPGGP" /* 0 */

    barras = '~033*p' + STRING(posicaoX) + 'x' + STRING(posicaoY) + 'Y'.
    barras = barras + (IF sentido  = "V" THEN "~033&a90P" ELSE "~033&a0P"). 

    barras = barras + pp + bp + pp + bp.   /* Inicio */ 
    DO i-ct = 1 TO LENGTH(texto) BY 2:
       ASSIGN num-1 = SUBSTRING(texto, i-ct,1)
              num-2 = SUBSTRING(texto, i-ct + 1,1).

       DO j = 1 TO 5.
          ASSIGN barras = barras + IF SUBSTR(cbarra[INDEX(chars,num-1)],j,1) = "G" 
                                   THEN pg ELSE pp.

          ASSIGN barras = barras + IF SUBSTR(cbarra[INDEX(chars,num-2)],j,1) = "G" 
                                   THEN bg ELSE bp.
       END.
    END.
    barras = barras + pg + bp + pp.   /* Fim */
    barras = barras + "~033&a0P".

    RETURN barras.
END FUNCTION.


/* Funá∆o para Calcular D°gito Verificador (formula do EAN13) */
FUNCTION fn-calc-digito RETURNS CHAR
    ( INPUT c-num-calc AS CHAR):

    DEF VAR i-soma AS INT.
    DEF VAR i-ct AS INT.
    DEF VAR i-dig AS INT.

    DO i-ct = 1 TO LENGTH(STRING(c-num-calc)).
       IF i-ct MODULO 2 = 0 THEN
          ASSIGN i-soma = i-soma + INT(SUBSTR(c-num-calc,i-ct,1)) * 3. 
       ELSE
          ASSIGN i-soma = i-soma + INT(SUBSTR(c-num-calc,i-ct,1)). 
    END.
    ASSIGN i-dig = IF i-soma MODULO 10 <> 0 
                   THEN 10 - (i-soma MODULO 10)
                   ELSE i-soma MODULO 10.
    RETURN STRING(i-dig).
END FUNCTION.

