/*  Include : sz-pcl.i
   Objetivo : Escrever diversas funcäes em codigo PCL5
      Autor : SeniuZ - Antonio Souza (Toninho)  
*/   


/* Fun‡Æo para gravar imagens na mem¢ria da impressora */
FUNCTION fn-grava-macro RETURNS CHARACTER
  ( INPUT arq-image AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose: Gravar Imagens na Mem¢ria da Impressora 
    Notes: Esta funcao Grava as imagens em macros, lembrando que a defini‡Æo das
           macros estÆo inclusas nas imagens e foram feitas com um editor bin rio 
------------------------------------------------------------------------------*/
    DEF VAR c-comando AS CHAR.
    ASSIGN c-comando = "net use lpt2: " + IF SESSION:PRINTER-NAME BEGINS "\\" 
                                          THEN SESSION:PRINTER-NAME
                                          ELSE SESSION:PRINTER-PORT.
    OS-COMMAND SILENT VALUE(c-comando).  

    ASSIGN c-comando = "copy /Y /b " + arq-image + " lpt2". 
    OS-COMMAND SILENT VALUE(c-comando). 

    ASSIGN c-comando = "net use lpt2: /DELETE".
    OS-COMMAND SILENT VALUE(c-comando).  

END FUNCTION.

   
/* Fun‡Æo para imprimir imagens gravadas na mem¢ria da impressora */
FUNCTION fn-imp-macro RETURNS CHARACTER
 ( INPUT pos-i-X AS INTEGER, INPUT pos-i-Y AS INTEGER,INPUT macroid AS INTEGER):

   DEFINE VARIABLE macro AS CHARACTER.
   ASSIGN macro = '~033*p' + STRING(pos-i-X) + 'x' + STRING(pos-i-Y) + 'Y' +
                  '~033&f' + STRING(macroid) + 'y2X'.
   RETURN macro.
END FUNCTION.

                           
/* Fun‡Æo para fazer uma linha */
FUNCTION fn-linha RETURNS CHARACTER
  ( INPUT posicaoX AS INTEGER, INPUT posicaoY AS INTEGER, INPUT tamanho AS INTEGER, INPUT espessura AS INTEGER, INPUT orientacao AS CHARACTER):
    DEFINE VARIABLE linha as CHARACTER.

    ASSIGN linha = '~033*p' + STRING(posicaoX) + 'x' + STRING(posicaoY) + 'Y'.

    IF orientacao = "H" THEN
       ASSIGN linha = linha + '~033*c' + STRING(tamanho) + 'a' + STRING(espessura) + 'b' + '0P'.
    ELSE
       ASSIGN linha = linha + '~033*c' + STRING(espessura) + 'a' + STRING(tamanho) + 'b' + '0P'.

    RETURN linha.
END FUNCTION.


/* Fun‡Æo para Montar um Retƒngulo */
FUNCTION fn-retangulo RETURNS CHARACTER
  ( INPUT pos-i-X AS INTEGER, INPUT pos-i-Y AS INTEGER, INPUT pos-f-X AS INTEGER, INPUT pos-f-Y AS INTEGER, INPUT espessura AS INTEGER):

    DEFINE VARIABLE retangulo as CHARACTER.
    ASSIGN retangulo = '~033*p' + STRING(pos-i-X) + 'x' + STRING(pos-i-Y) + 'Y' +
                       '~033*c' + STRING(pos-f-X - pos-i-X) + 'a' + STRING(espessura) + 'b' + '0P' + 
                       '~033*c' + STRING(espessura) + 'a' + STRING(pos-f-Y - pos-i-Y) + 'b' + '0P' + 
                       '~033*p' + STRING(pos-i-X) + 'x' + string(pos-f-Y) + 'Y' +
                       '~033*c' + STRING(pos-f-X - pos-i-X + espessura) + 'a' + string(espessura) + 'b' + '0P' +
                       '~033*p' + STRING(pos-f-X) + 'x' + string(pos-i-Y) + 'Y' +
                       '~033*c' + STRING(espessura) + 'a' + string(pos-f-Y - pos-i-Y) + 'b' + '0P'.
    RETURN retangulo.
END FUNCTION.


/* Fun‡Æo para Escrever textos */
FUNCTION fn-texto RETURNS CHARACTER
  ( INPUT pos-i-X AS INTEGER, INPUT pos-i-Y as integer, INPUT texto AS CHARACTER,INPUT fonte AS INTEGER, INPUT tamanho AS INTEGER, INPUT negrito AS INTEGER, INPUT italico AS INTEGER, INPUT fixo AS INTEGER, INPUT sentido AS CHAR):

    DEFINE VARIABLE c-texto as CHARACTER.

    ASSIGN c-texto = '~033*p' + STRING(pos-i-X) + 'x' + STRING(pos-i-Y) + 'Y' + 
                     (IF sentido  = "V" THEN "~033&a90P" ELSE "~033&a0P") +
                     '~033(s' + STRING(fixo) + 'p' + 
                                STRING(tamanho) + (IF fixo = 1 THEN 'v' ELSE 'h') +
                                STRING(italico) + 's' +
                                STRING(negrito) + 'b' +
                                STRING(fonte)   + 'T' +
                     texto + (IF sentido = "V" THEN "~033&a0P" ELSE "").

    RETURN c-texto.
END FUNCTION.


/* Fun‡Æo para Imprimir um C¡rculo */
FUNCTION fn-circulo RETURNS CHARACTER
  ( INPUT posicaoX AS INTEGER, INPUT posicaoY AS INTEGER, INPUT raio AS INTEGER):
    DEFINE VARIABLE circulo as CHARACTER.
    DEF VAR i-limite AS INT INIT 11300.

    ASSIGN circulo = '~033%0BINSP1PA' + STRING(posicaoY) + ',' + STRING(i-limite - posicaoX) + 'CI' + STRING(raio) + '~033%0A'.

    RETURN circulo.
END FUNCTION.


/* Fun‡Æo para Imprimir Barras em C¢digo ITF (Intercalado 2 de 5) */
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

/* Fun‡Æo para Calcular D¡gito Verificador (formula do EAN13) */
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


/* Fun‡Æo preencher rachurar uma  rea  */
FUNCTION fn-area RETURNS CHARACTER
  (INPUT pos-X AS INTEGER, INPUT pos-Y AS INTEGER, INPUT comprimento AS INTEGER, INPUT area AS INTEGER,INPUT intensidade AS INTEGER):
    DEFINE VARIABLE conteudo AS CHARACTER.
    ASSIGN conteudo = '~033*p' + STRING(pos-X) + 'x' + STRING(pos-Y) + 'Y' +
                      '~033*c' + STRING(comprimento) + 'a' + STRING(area) + 'b' + string(intensidade) + 'G' + 
                      '~033*c2P'. 
    RETURN conteudo.
END FUNCTION.


/* Fun‡Æo para Imprimir Barras em C¢digo EAN13 */
FUNCTION fn-ean13 RETURNS CHARACTER
  ( INPUT posicaoX AS INTEGER, INPUT posicaoY AS INTEGER, INPUT num-ean13 AS CHARACTER):
    DEFINE VARIABLE b-ean13   AS CHAR.        /* Composi‡Æo das Barras */
    DEFINE VARIABLE b0        AS CHARACTER.   /* Barra Branca */
    DEFINE VARIABLE b1        AS CHARACTER.   /* Barra Preta */
    DEFINE VARIABLE bg1       AS CHARACTER.   /* Barra Preta Grande para os delimitadores */
    DEFINE VARIABLE i         AS INTEGER.
    DEFINE VARIABLE j         AS INTEGER.
    DEFINE VARIABLE i-num     AS INTEGER.
    DEFINE VARIABLE barra-c   AS CHAR.
    DEFINE VARIABLE num-c     AS CHAR.
    DEFINE VARIABLE i-ct      AS INT.
    DEFINE VARIABLE tot-e     AS INT.
    DEFINE VARIABLE i-dv      AS INT.   /* Digito Verificador do EAN-13*/
    DEFINE VARIABLE tab-ean13 AS CHAR EXTENT 10 INIT
                   ["1110010","1100110","1101100","1000010","1011100",
                    "1001110","1010000","1000100","1001000","1110100"].

    ASSIGN bg1 = '~033*c2a120b0p' + '~033*p+3X'
            b1 = '~033*c2a100b0p' + '~033*p+3X'
            b0 = '~033*p+3X'.

    /* Delimitador Inicial */
    ASSIGN b-ean13 = '~033*p' + STRING(posicaoX) + 'x' + STRING(posicaoY) + 'Y' +
                      bg1 + b0 + bg1.

    /* Escreve as Barras do 7890413 - Codigo do Brasil (789) + Codigo da Tear  (0413) */
    ASSIGN b-ean13 = b-ean13 + 
                     b0 + b1 + b1 + b0 + b1 + b1 + b1 + b0 + b0 + b1 + b0 + b1 +
                     b1 + b1 + b0 + b0 + b0 + b1 + b1 + b0 + b1 + b0 + b0 + b1 +
                     b1 + b1 + b0 + b1 + b0 + b0 + b1 + b1 + b0 + b0 + b1 + b0 +
                     b1 + b0 + b0 + b0 + b0 + b1.

    /* Delimitardor Central */
    ASSIGN b-ean13 = b-ean13 + b0 + bg1 + b0 + bg1 + b0.

    /* Calcula digito verificador ean 13 */
    DO i-ct = 1 TO 12.
       IF i-ct MODULO 2 = 0 THEN
          ASSIGN tot-e = tot-e + INT(SUBSTR(num-ean13,i-ct,1)) * 3. 
       ELSE
          ASSIGN tot-e = tot-e + INT(SUBSTR(num-ean13,i-ct,1)). 
    END.
    ASSIGN i-dv = IF tot-e MODULO 10 <> 0 
                  THEN 10 - (tot-e MODULO 10)
                  ELSE tot-e MODULO 10
           num-c = SUBSTR(num-ean13,8,6) + STRING(i-dv,"9").

    /* Escreve Barras do resto do c¢digo + digito (Parte C) */

    DO i = 1 TO LENGTH(num-c).
       ASSIGN i-num = INT(SUBSTR(num-c,i,1))
              barra-c = tab-ean13[i-num + 1].

       DO j = 1 TO LENGTH(barra-c).
          ASSIGN b-ean13 = b-ean13 + IF SUBSTR(barra-c,j,1) = '0'
                                       THEN b0 ELSE b1.
       END.
    END.

    /* Delimitador Final */
    ASSIGN b-ean13 = b-ean13 + bg1 + b0 + bg1.

    /* Escreve Texto abaixo das Barras */
    DO i = 1 TO LENGTH(num-c) BY 2.
       ASSIGN num-c = SUBSTR(num-c,1,i) + " " + SUBSTR(num-c,i + 1).
    END.
    ASSIGN b-ean13 = b-ean13 + 
                     fn-texto(INPUT posicaoX - 18, INPUT posicaoY + 120, INPUT "7",INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H") +
                     fn-texto(INPUT posicaoX + 13, INPUT posicaoY + 120, INPUT "8 9 0 4 1 3" ,INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H") +
                     fn-texto(INPUT posicaoX + 153, INPUT posicaoY + 120, INPUT num-c ,INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").

    RETURN b-ean13.
END FUNCTION.

/* Fun‡Æo para Imprimir Barras em C¢digo 39 */
FUNCTION fn-code39 RETURNS CHARACTER(INPUT posicaoX AS INTEGER, INPUT posicaoY AS INTEGER, INPUT texto AS CHARACTER):
    define variable heigth    as decimal.
    define variable small-bar as decimal.
    define variable wide-bar  as decimal.
    define variable dpl       as decimal.
    define variable i         as integer.
    define variable j         as integer.

    define variable inicio    as character.
    define variable fim       as character.
    define variable pp        as character.
    define variable pg        as character.
    define variable bp        as character.
    define variable bg        as character.
    define variable chars     as character.
    define variable cbarra    as character extent 100.
    define variable barras    as character.

    assign chars  = '1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ-. *$/+%!#&~'(),:;<=>?@]\[^_`abcdefghijklmnopqrstuvwxyz~~'
           inicio = '~033*p-50Y'
           fim    = '~033*p+50Y'.

    assign heigth    = 2                          /* Altura das linhas */
           small-bar = 2                          /* Numero de pontos da linha */
           wide-bar  = round(small-bar * 2.8, 0)  /* Numero de pontos da barra */
           dpl       = 50.                        /* Pontos por linha 300 dpi/6lpi = 50dpl */

    ASSIGN pp = '~033*c'  +  string(small-bar, "99") + 'a' + string(heigth * dpl) + 'b0p~033*p+' + string(small-bar, "99") + "X"
           pg = '~033*c'  +  string(wide-bar,  "99") + 'a' + string(heigth * dpl) + 'b0p~033*p+' + string(wide-bar,  "99") + "X"
           bp = '~033*p+' +  string(small-bar, "99") + "X"
           bg = '~033*p+' +  string(wide-bar,  "99") + "X".

    ASSIGN cbarra[01] = pg + bp + pp + bg + pp + bp + pp + bp + pg /* 1 */
           cbarra[02] = pp + bp + pg + bg + pp + bp + pp + bp + pg /* 2 */ 
           cbarra[03] = pg + bp + pg + bg + pp + bp + pp + bp + pp /* 3 */ 
           cbarra[04] = pp + bp + pp + bg + pg + bp + pp + bp + pg /* 4 */ 
           cbarra[05] = pg + bp + pp + bg + pg + bp + pp + bp + pp /* 5 */ 
           cbarra[06] = pp + bp + pg + bg + pg + bp + pp + bp + pp /* 6 */ 
           cbarra[07] = pp + bp + pp + bg + pp + bp + pg + bp + pg /* 7 */ 
           cbarra[08] = pg + bp + pp + bg + pp + bp + pg + bp + pp /* 8 */ 
           cbarra[09] = pp + bp + pg + bg + pp + bp + pg + bp + pp /* 9 */ 
           cbarra[10] = pp + bp + pp + bg + pg + bp + pg + bp + pp /* 0 */ 
           cbarra[11] = pg + bp + pp + bp + pp + bg + pp + bp + pg /* a */ 
           cbarra[12] = pp + bp + pg + bp + pp + bg + pp + bp + pg /* b */ 
           cbarra[13] = pg + bp + pg + bp + pp + bg + pp + bp + pp /* c */ 
           cbarra[14] = pp + bp + pp + bp + pg + bg + pp + bp + pg /* d */ 
           cbarra[15] = pg + bp + pp + bp + pg + bg + pp + bp + pp /* e */ 
           cbarra[16] = pp + bp + pg + bp + pg + bg + pp + bp + pp /* f */ 
           cbarra[17] = pp + bp + pp + bp + pp + bg + pg + bp + pg /* g */ 
           cbarra[18] = pg + bp + pp + bp + pp + bg + pg + bp + pp /* h */ 
           cbarra[19] = pp + bp + pg + bp + pp + bg + pg + bp + pp /* i */ 
           cbarra[20] = pp + bp + pp + bp + pg + bg + pg + bp + pp /* j */ 
           cbarra[21] = pg + bp + pp + bp + pp + bp + pp + bg + pg /* k */ 
           cbarra[22] = pp + bp + pg + bp + pp + bp + pp + bg + pg /* l */ 
           cbarra[23] = pg + bp + pg + bp + pp + bp + pp + bg + pp /* m */ 
           cbarra[24] = pp + bp + pp + bp + pg + bp + pp + bg + pg /* n */ 
           cbarra[25] = pg + bp + pp + bp + pg + bp + pp + bg + pp /* o */ 
           cbarra[26] = pp + bp + pg + bp + pg + bp + pp + bg + pp /* p */ 
           cbarra[27] = pp + bp + pp + bp + pp + bp + pg + bg + pg /* q */ 
           cbarra[28] = pg + bp + pp + bp + pp + bp + pg + bg + pp /* r */ 
           cbarra[29] = pp + bp + pg + bp + pp + bp + pg + bg + pp /* s */ 
           cbarra[30] = pp + bp + pp + bp + pg + bp + pg + bg + pp /* t */ 
           cbarra[31] = pg + bg + pp + bp + pp + bp + pp + bp + pg /* u */ 
           cbarra[32] = pp + bg + pg + bp + pp + bp + pp + bp + pg /* v */ 
           cbarra[33] = pg + bg + pg + bp + pp + bp + pp + bp + pp /* w */ 
           cbarra[34] = pp + bg + pp + bp + pg + bp + pp + bp + pg /* x */ 
           cbarra[35] = pg + bg + pp + bp + pg + bp + pp + bp + pp /* y */ 
           cbarra[36] = pp + bg + pg + bp + pg + bp + pp + bp + pp /* z */ 
           cbarra[37] = pp + bg + pp + bp + pp + bp + pg + bp + pg /* - */ 
           cbarra[38] = pg + bg + pp + bp + pp + bp + pg + bp + pp /* . */ 
           cbarra[39] = pp + bg + pg + bp + pp + bp + pg + bp + pp /*   */.
    
    ASSIGN cbarra[40] = pp + bg + pp + bp + pg + bp + pg + bp + pp /* * */
           cbarra[41] = pp + bg + pp + bg + pp + bg + pp + bp + pp /* $ */
           cbarra[42] = pp + bg + pp + bg + pp + bp + pp + bg + pp /* / */
           cbarra[43] = pp + bg + pp + bp + pp + bg + pp + bg + pp /* + */
           cbarra[44] = pp + bp + pp + bg + pp + bg + pp + bg + pp /* % */
           cbarra[45] = pp + bg + pp + bg + pp + bp + pp + bg + pp + bp + pg + bp + pp + bp + pp + bg + pg + bp + pp /* ( */    
           cbarra[46] = pp + bg + pp + bg + pp + bp + pp + bg + pp + bp + pp + bp + pg + bp + pp + bg + pg + bp + pp /* ) */    
           cbarra[45] = pp + bg + pp + bg + pp + bp + pp + bg + pp /* ! */
           cbarra[46] = pp + bg + pp + bg + pp + bp + pp + bg + pp /* # */
           cbarra[47] = pp + bg + pp + bg + pp + bp + pp + bg + pp /* & */
           cbarra[48] = pp + bg + pp + bg + pp + bp + pp + bg + pp /* ' */
           cbarra[49] = pp + bg + pp + bg + pp + bp + pp + bg + pp /* ( */
           cbarra[50] = pp + bg + pp + bg + pp + bp + pp + bg + pp /* ) */
           cbarra[51] = pp + bg + pp + bg + pp + bp + pp + bg + pp /* , */
           cbarra[52] = pp + bg + pp + bg + pp + bp + pp + bg + pp /* : */
           cbarra[53] = pp + bp + pp + bg + pp + bg + pp + bg + pp /* ; */
           cbarra[54] = pp + bp + pp + bg + pp + bg + pp + bg + pp /* < */
           cbarra[55] = pp + bp + pp + bg + pp + bg + pp + bg + pp /* = */
           cbarra[56] = pp + bp + pp + bg + pp + bg + pp + bg + pp /* > */
           cbarra[57] = pp + bp + pp + bg + pp + bg + pp + bg + pp /* ? */
           cbarra[58] = pp + bp + pp + bg + pp + bg + pp + bg + pp /* @ */
           cbarra[59] = pp + bp + pp + bg + pp + bg + pp + bg + pp /* ] */
           cbarra[60] = pp + bp + pp + bg + pp + bg + pp + bg + pp /* \ */
           cbarra[61] = pp + bp + pp + bg + pp + bg + pp + bg + pp /* [ */
           cbarra[62] = pp + bp + pp + bg + pp + bg + pp + bg + pp /* ^ */
           cbarra[63] = pp + bp + pp + bg + pp + bg + pp + bg + pp /* _ */
           cbarra[64] = pp + bp + pp + bg + pp + bg + pp + bg + pp /* ` */
           cbarra[65] = pp + bg + pp + bp + pp + bg + pp + bg + pp /* a */
           cbarra[66] = pp + bg + pp + bp + pp + bg + pp + bg + pp /* b */
           cbarra[67] = pp + bg + pp + bp + pp + bg + pp + bg + pp /* c */
           cbarra[68] = pp + bg + pp + bp + pp + bg + pp + bg + pp /* d */
           cbarra[69] = pp + bg + pp + bp + pp + bg + pp + bg + pp /* e */
           cbarra[70] = pp + bg + pp + bp + pp + bg + pp + bg + pp /* f */
           cbarra[71] = pp + bg + pp + bp + pp + bg + pp + bg + pp /* g */
           cbarra[72] = pp + bg + pp + bp + pp + bg + pp + bg + pp /* h */
           cbarra[73] = pp + bg + pp + bp + pp + bg + pp + bg + pp /* i */
           cbarra[74] = pp + bg + pp + bp + pp + bg + pp + bg + pp /* j */
           cbarra[75] = pp + bg + pp + bp + pp + bg + pp + bg + pp /* k */
           cbarra[76] = pp + bg + pp + bp + pp + bg + pp + bg + pp /* l */
           cbarra[77] = pp + bg + pp + bp + pp + bg + pp + bg + pp /* m */
           cbarra[78] = pp + bg + pp + bp + pp + bg + pp + bg + pp /* n */
           cbarra[79] = pp + bg + pp + bp + pp + bg + pp + bg + pp /* o */
           cbarra[80] = pp + bg + pp + bp + pp + bg + pp + bg + pp /* p */.                                                    
    
    ASSIGN cbarra[81] = pp + bg + pp + bp + pp + bg + pp + bg + pp /* q */
           cbarra[82] = pp + bg + pp + bp + pp + bg + pp + bg + pp /* r */
           cbarra[83] = pp + bg + pp + bp + pp + bg + pp + bg + pp /* s */
           cbarra[84] = pp + bg + pp + bp + pp + bg + pp + bg + pp /* t */
           cbarra[85] = pp + bg + pp + bp + pp + bg + pp + bg + pp /* u */
           cbarra[86] = pp + bg + pp + bp + pp + bg + pp + bg + pp /* v */
           cbarra[87] = pp + bg + pp + bp + pp + bg + pp + bg + pp /* w */
           cbarra[88] = pp + bg + pp + bp + pp + bg + pp + bg + pp /* x */
           cbarra[89] = pp + bg + pp + bp + pp + bg + pp + bg + pp /* y */
           cbarra[90] = pp + bg + pp + bp + pp + bg + pp + bg + pp /* z */
           cbarra[91] = pp + bp + pp + bg + pp + bg + pp + bg + pp /* { */
           cbarra[92] = pp + bp + pp + bg + pp + bg + pp + bg + pp /* | */
           cbarra[93] = pp + bp + pp + bg + pp + bg + pp + bg + pp /* } */
           cbarra[94] = pp + bp + pp + bg + pp + bg + pp + bg + pp /* ~ */.

    barras = '~033*p' + STRING(posicaoX) + 'x' + STRING(posicaoY) + 'Y'.
    barras = barras + pp + bg + pp + bp + pg + bp + pg + bp + pp + bp. /* inicializador do codigo de barras code 39 */
    DO i = 1 TO LENGTH(texto):
        barras = barras + cbarra[INDEX(chars, SUBSTRING(texto, i, 1))] + bp.
    END.
    barras = barras + pp + bg + pp + bp + pg + bp + pg + bp + pp.  /* Terminador do codigo de barras code 39 */

    RETURN barras.
END FUNCTION.


/* Fun‡Æo para Imprimir Barras em C¢digo 128 */
FUNCTION codigo-barras128 RETURNS character(input posicaoX as integer,
                                            input posicaoY as integer,
                                            input texto as CHARACTER,
                                            INPUT sentido AS CHAR):
    define variable heigth    as decimal.
    define variable small-bar as decimal.
    define variable wide-bar  as decimal.
    define variable dpl       as decimal.
    define variable i         as integer.
    define variable j         as integer.

    define variable inicio    as character.
    define variable fim       as character.
    define variable pp        as character.
    define variable pg        as character.
    define variable bp        as character.
    define variable bg        as character.
    define variable chars     as character.
    define variable cbarra    as character extent 100.
    define variable barras    as character.

    define variable b0        as character.
    define variable b1        as character.

    ASSIGN chars  = '1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ'
           inicio = '~033*p-50Y'
           fim    = '~033*p+50Y'.

    ASSIGN heigth    = 5                          /* Altura das linhas */
           small-bar = 2                          /* Numero de pontos da linha */
           wide-bar  = ROUND(small-bar * 3, 0)    /* Numero de pontos da barra */
           dpl       = 50.                        /* Pontos por linha 300 dpi/6lpi = 50dpl */


    ASSIGN b1 = '~033*c2a100b0p' + '~033*p+3X'
           b0 = '~033*p+3X'.

    ASSIGN cbarra[01] = b1 + b0 + b0 + b1 + b1 + b1 + b0 + b0 + b1 + b1 + b0 /* 1 */
           cbarra[02] = b1 + b1 + b0 + b0 + b1 + b1 + b1 + b0 + b0 + b1 + b0 /* 2 */ 
           cbarra[03] = b1 + b1 + b0 + b0 + b1 + b0 + b1 + b1 + b1 + b0 + b0 /* 3 */.

    barras = '~033*p' + STRING(posicaoX) + 'x' + STRING(posicaoY) + 'Y'.
    barras = barras + (IF sentido  = "V" THEN "~033&a90P" ELSE "~033&a0P"). 

    barras = barras + b1 + b1 + b0 + b1 + b0 + b0 + b1 + b0 + b0 + b0 + b0.  /* Start 104 */

    DO i = 1 TO LENGTH(texto):
        barras = barras + cbarra[INDEX(chars, SUBSTRING(texto, i, 1))].
    END.

    barras = barras + b1 + b1 + b0 + b0 + b0 + b1 + b1 + b1 + b0 + b1 + b0 + b1 + b1.  /* stop  106 */
    barras = barras + "~033&a0P".

    RETURN barras.
END FUNCTION.


/* Fun‡Æo preencher rachurar uma  rea  */
FUNCTION fn-duplex RETURNS CHARACTER
  (INPUT ligar AS LOGICAL):
    DEFINE VARIABLE conteudo AS CHARACTER.
    /* PUT CONTROL "~033&l2S~033(s16H". /* DUPLEX BORDA CURTA */ */

    ASSIGN conteudo = '~033&l' + IF ligar 
                                 THEN "2S"
                                 ELSE "0S".
    RETURN conteudo.
END FUNCTION.

