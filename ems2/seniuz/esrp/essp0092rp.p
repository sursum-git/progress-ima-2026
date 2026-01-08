/* Programa: ESSP0092.P
** Objetivo: imprimir Etiquetas codigo de Barras
**           As fun‡äes deste programa foi escrito utilizando linguagem
**           de impressora PCL-5
** Autor...: PRODB-Antonio G. Souza (Jun 2004)
*/

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    FIELD it-codigo        AS CHAR FORMAT "x(16)"
    FIELD cod-refer-ini    AS CHAR FORMAT "x(8)"
    FIELD cod-refer-fim    AS CHAR FORMAT "x(8)"
    FIELD seq-ini          AS INT  FORMAT ">>9"
    FIELD seq-fim          AS INT  FORMAT ">>9"
    FIELD partida          AS INT  FORMAT ">>>>>9"
    FIELD nuance           AS CHAR FORMAT "x(1)"
    FIELD qualidade        AS CHAR FORMAT "x(2)"
    FIELD metro            AS DEC  FORMAT ">>,>>9.99"
    FIELD embalagem        AS CHAR FORMAT "x(2)".

DEFINE TEMP-TABLE tt-digita NO-UNDO 
    FIELD it-codigo        AS CHAR FORMAT "x(16)"
    FIELD cod-refer        AS CHAR FORMAT "x(8)"
    FIELD seq-ini          AS INT  FORMAT ">>9" 
    FIELD seq-fim          AS INT  FORMAT ">>9" 
    FIELD partida          AS INT  FORMAT ">>>>>9"
    FIELD nuance           AS CHAR FORMAT "x(1)" 
    FIELD qualidade        AS CHAR FORMAT "x(2)"
    FIELD metro            AS DEC  FORMAT ">>,>>9.99"
    FIELD embalagem        AS CHAR FORMAT "x(2)"
    INDEX id-dig it-codigo.

define temp-table tt-raw-digita 
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

FOR EACH tt-raw-digita.
    CREATE tt-digita.
    RAW-TRANSFER tt-raw-digita.raw-digita TO tt-digita.
END.

/* Variaveis */
define temp-table tt-itens no-undo
    FIELD it-codigo        AS CHAR FORMAT "x(16)"
    FIELD cod-refer        AS CHAR FORMAT "x(8)" 
    FIELD seq-ini          AS INT  FORMAT ">>9"
    FIELD seq-fim          AS INT  FORMAT ">>9"
    FIELD partida          AS INT  FORMAT ">>>>>9"
    FIELD nuance           AS CHAR FORMAT "x(1)" 
    FIELD qualidade        AS CHAR FORMAT "x(2)"
    FIELD metro            AS DEC  FORMAT ">>,>>9.99"
    FIELD embalagem        AS CHAR FORMAT "x(2)"
    index id-it it-codigo.

DEF VAR i-sequencia AS INT.
DEF VAR i-posicao   AS INT.
DEF VAR i-ct        AS INT.
DEF VAR c-desc-item AS CHAR FORMAT "x(50)".
DEF VAR c-composicao1 LIKE composi.descricao.
DEF VAR c-composicao2 LIKE composi.descricao.

/* Funcäes */

FUNCTION f-imp-macro RETURNS CHARACTER(INPUT pos-i-X AS INTEGER, INPUT pos-i-Y AS INTEGER,INPUT macroid AS INTEGER).
    DEFINE VARIABLE macro AS CHARACTER.
    ASSIGN macro = '~033*p' + STRING(pos-i-X) + 'x' + STRING(pos-i-Y) + 'Y' +
                   '~033&f' + STRING(macroid) + 'y2X'.
    RETURN macro.
END FUNCTION.

FUNCTION f-retangulo RETURNS character(input pos-i-X as integer, input pos-i-Y as integer, input pos-f-X as INTEGER, input pos-f-Y as INTEGER, INPUT espessura AS INTEGER):
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

FUNCTION f-texto RETURNS CHARACTER(INPUT pos-i-X AS INTEGER, INPUT pos-i-Y as integer, INPUT texto AS CHARACTER,INPUT fonte AS INTEGER, INPUT tamanho AS INTEGER, INPUT negrito AS INTEGER, INPUT italico AS INTEGER, INPUT fixo AS INTEGER):
    DEFINE VARIABLE c-texto as CHARACTER.
    
    ASSIGN c-texto = '~033*p' + STRING(pos-i-X) + 'x' + STRING(pos-i-Y) + 'Y' + 
                     '~033(s' + STRING(fixo) + 'p' + 
                                STRING(tamanho) + (IF fixo = 1 THEN 'v' ELSE 'h') +
                                STRING(italico) + 's' +
                                STRING(negrito) + 'b' +
                                STRING(fonte)   + 'T' +
                      texto.

    RETURN c-texto.
END FUNCTION.

FUNCTION f-linha RETURNS CHARACTER(INPUT posicaoX AS INTEGER, INPUT posicaoY as integer, input tamanho AS INTEGER, INPUT espessura AS INTEGER, INPUT orientacao AS CHARACTER):
    DEFINE VARIABLE linha as CHARACTER.

    ASSIGN linha = '~033*p' + STRING(posicaoX) + 'x' + STRING(posicaoY) + 'Y'.

    IF orientacao = "H" THEN
       ASSIGN linha = linha + '~033*c' + STRING(tamanho) + 'a' + STRING(espessura) + 'b' + '0P'.
    ELSE
       ASSIGN linha = linha + '~033*c' + STRING(espessura) + 'a' + STRING(tamanho) + 'b' + '0P'.
    RETURN linha.
END FUNCTION.

FUNCTION codigo-barras13 RETURNS character(input posicaoX as integer, input posicaoY as integer, input num-ean13 as character):
    DEFINE VARIABLE barras13  AS CHAR.
    define variable b0        as character.
    define variable b1        as character.
    define variable bg1       as character.
    DEFINE VARIABLE i         AS INTEGER.
    DEFINE VARIABLE j         AS INTEGER.
    DEFINE VARIABLE i-num     AS INTEGER.
    DEFINE VARIABLE barra-c   AS CHAR.
    DEFINE VARIABLE num-c     AS CHAR.
    DEFINE VARIABLE tab-c     AS CHAR EXTENT 10 INIT
           ["1110010","1100110","1101100","1000010","1011100",
            "1001110","1010000","1000100","1001000","1110100"].

    DEF VAR i-ct AS INT.
    DEF VAR tot-e AS INT.
    DEF VAR i-dig AS INT.

    ASSIGN bg1 = '~033*c2a120b0p' + '~033*p+3X'
            b1 = '~033*c2a100b0p' + '~033*p+3X'
            b0 = '~033*p+3X'.

    /* Delimitador Inicial */
    ASSIGN barras13 = '~033*p' + STRING(posicaoX) + 'x' + STRING(posicaoY) + 'Y' +
                      bg1 + b0 + bg1.

    /* Escreve as Barras do 7890413 - Codigo do Brasil (789) + Codigo da Tear  (0413) */
    ASSIGN barras13 = barras13 + 
                      b0 + b1 + b1 + b0 + b1 + b1 + b1 + b0 + b0 + b1 + b0 + b1 +
                      b1 + b1 + b0 + b0 + b0 + b1 + b1 + b0 + b1 + b0 + b0 + b1 +
                      b1 + b1 + b0 + b1 + b0 + b0 + b1 + b1 + b0 + b0 + b1 + b0 +
                      b1 + b0 + b0 + b0 + b0 + b1.

    /* Delimitardor Central */
    ASSIGN barras13 = barras13 + b0 + bg1 + b0 + bg1 + b0.

    /* Calcula digito verificador ean 13 */
    DO i-ct = 1 TO 12.
       IF i-ct MODULO 2 = 0 THEN
          ASSIGN tot-e = tot-e + INT(SUBSTR(num-ean13,i-ct,1)) * 3. 
       ELSE
          ASSIGN tot-e = tot-e + INT(SUBSTR(num-ean13,i-ct,1)). 
    END.
    ASSIGN i-dig = IF tot-e MODULO 10 <> 0 
                   THEN 10 - (tot-e MODULO 10)
                   ELSE tot-e MODULO 10
           num-c = SUBSTR(num-ean13,8,6) + STRING(i-dig,"9").

    /* Escreve Barras do resto do c¢digo + digito (Parte C) */

    DO i = 1 TO LENGTH(num-c).
       ASSIGN i-num = INT(SUBSTR(num-c,i,1))
              barra-c = tab-c[i-num + 1].

       DO j = 1 TO LENGTH(barra-c).
          ASSIGN barras13 = barras13 + IF SUBSTR(barra-c,j,1) = '0'
                                       THEN b0
                                       ELSE b1.
       END.
    END.

    /* Delimitador Final */
    ASSIGN barras13 = barras13 + bg1 + b0 + bg1.

    /* Escreve Texto abaixo das Barras */
    DO i = 1 TO LENGTH(num-c) BY 2.
       ASSIGN num-c = SUBSTR(num-c,1,i) + " " + SUBSTR(num-c,i + 1).
    END.
    ASSIGN barras13 = barras13 + 
                      f-texto(INPUT posicaoX - 18, INPUT posicaoY + 120, INPUT "7",INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1) +
                      f-texto(INPUT posicaoX + 13, INPUT posicaoY + 120, INPUT "8 9 0 4 1 3" ,INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1) +
                      f-texto(INPUT posicaoX + 153, INPUT posicaoY + 120, INPUT num-c ,INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1).

    RETURN barras13.
END FUNCTION.

FUNCTION codigo-barras39 RETURNS character(input posicaoX as integer, input posicaoY as integer, input texto as character):
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

    barras = barras + f-texto(INPUT posicaoX + 260, INPUT posicaoY + 120, INPUT texto,INPUT 16602, INPUT 5, INPUT 3, INPUT 0, INPUT 1).
    RETURN barras.
END FUNCTION.

FIND FIRST tt-digita NO-LOCK NO-ERROR.
IF AVAIL tt-digita THEN DO.
   FOR EACH tt-digita.
       CREATE tt-itens.
       ASSIGN tt-itens.it-codigo = tt-digita.it-codigo  
              tt-itens.cod-refer = tt-digita.cod-refer
              tt-itens.seq-ini   = tt-digita.seq-ini
              tt-itens.seq-fim   = tt-digita.seq-fim
              tt-itens.partida   = tt-digita.partida    
              tt-itens.nuance    = tt-digita.nuance     
              tt-itens.qualidade = tt-digita.qualidade  
              tt-itens.metro     = tt-digita.metro      
              tt-itens.embalagem = tt-digita.embalagem.  
   END.
END.
ELSE DO.
   FOR EACH mgind.ref-item WHERE
            mgind.ref-item.it-codigo  = tt-param.it-codigo AND
            mgind.ref-item.cod-refer >= tt-param.cod-refer-ini AND
            mgind.ref-item.cod-refer <= tt-param.cod-refer-fim NO-LOCK.

       CREATE tt-itens.
       ASSIGN tt-itens.it-codigo = mgind.ref-item.it-codigo  
              tt-itens.cod-refer = mgind.ref-item.cod-refer
              tt-itens.seq-ini   = tt-param.seq-ini
              tt-itens.seq-fim   = tt-param.seq-fim
              tt-itens.partida   = tt-param.partida    
              tt-itens.nuance    = tt-param.nuance     
              tt-itens.qualidade = tt-param.qualidade  
              tt-itens.metro     = tt-param.metro      
              tt-itens.embalagem = tt-param.embalagem.  
   END.
END.

PUT UNFORMATTED 
    "~033&l1O"     /* Seta a impressora para Paisagem (Landscape) */
    "~033&l26A".


ASSIGN i-posicao = 250.
FOR EACH tt-itens NO-LOCK.
    FIND mgind.item WHERE
         mgind.item.it-codigo = tt-itens.it-codigo NO-LOCK NO-ERROR.

    FIND mgind.referencia WHERE
         mgind.referencia.cod-refer = tt-itens.cod-refer NO-LOCK NO-ERROR.

    /*
    IF NOT AVAIL mgind.referencia THEN DO.
       MESSAGE "Referencia " tt-itens.cod-refer " nÆo Cadastrada..." VIEW-AS ALERT-BOX.
       NEXT.
    END.
    */

    FIND FIRST item-ext NO-LOCK WHERE
               item-ext.it-codigo = ITEM.it-codigo NO-ERROR.

    IF NOT AVAIL item-ext THEN DO.
       MESSAGE "ExtensÆo do Item nÆo Cadastrada..." VIEW-AS ALERT-BOX.
       NEXT.
    END.

    FIND FIRST ref-item-ext WHERE 
               ref-item-ext.it-codigo = tt-itens.it-codigo AND 
               ref-item-ext.cod-refer = tt-itens.cod-refer NO-LOCK NO-ERROR.

    IF NOT AVAIL ref-item-ext THEN DO.
       MESSAGE "ExtensÆo da Referencia nÆo Cadastrada..." VIEW-AS ALERT-BOX.
       NEXT.
    END.
    ASSIGN c-desc-item = ITEM.descricao-1 + ITEM.descricao-2 + " " + 
                         IF AVAIL referencia 
                         THEN referencia.descricao
                         ELSE "" . 

    FIND FIRST composi NO-LOCK WHERE
               composi.cod-composi = item-ext.cod-composi NO-ERROR.

    ASSIGN c-composicao1 = ""
           c-composicao2 = "".

    IF AVAIL COMPOSI THEN DO.
       DO i-ct = 1 TO NUM-ENTRIES(composi.descricao).
          IF i-ct <= 3 THEN 
             ASSIGN c-composicao1 = c-composicao1 + ENTRY(i-ct,composi.descricao).
          ELSE 
             ASSIGN c-composicao2 = c-composicao2 + ENTRY(i-ct,composi.descricao).
       END.
    END.

    DO i-sequencia = tt-itens.seq-ini TO tt-itens.seq-fim.
       RUN pi-etiqueta (INPUT i-posicao).

       IF i-posicao = 2450 THEN DO.
          PUT UNFORMATTED "~033&l0H".   /* Salta P gina */
          ASSIGN i-posicao = 250.
       END.
       ELSE
          ASSIGN i-posicao = i-posicao + 1100.
    END.
END.

PROCEDURE pi-etiqueta.
  DEFINE INPUT PARAMETER i-col AS INT.

  PUT UNFORMATTED 
      f-retangulo(input i-col + 10, input 0, input i-col + 875, INPUT 430, INPUT 6). 

  PUT UNFORMATTED 
      f-texto(INPUT i-col + 50, INPUT 40, INPUT "PRODUTO",INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1)
      f-texto(INPUT i-col + 25, INPUT 100, INPUT tt-itens.it-codigo,INPUT 16602, INPUT 13, INPUT 3, INPUT 0, INPUT 1)
      f-linha(input i-col + 210, input 0, INPUT 110, INPUT 6, INPUT "V")
      f-texto(INPUT i-col + 225, INPUT 40, INPUT "ACB/DES/COR",INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1)
      f-texto(INPUT i-col + 220, INPUT 100, INPUT SUBSTR(tt-itens.cod-refer,1,2),INPUT 16602, INPUT 13, INPUT 3, INPUT 0, INPUT 1)
      f-texto(INPUT i-col + 295, INPUT 100, INPUT SUBSTR(tt-itens.cod-refer,3,4),INPUT 16602, INPUT 13, INPUT 3, INPUT 0, INPUT 1)
      f-linha(input i-col + 420, input 0, input 110, INPUT 6, INPUT "V")
      f-texto(INPUT i-col + 473, INPUT 40, INPUT "VAR",INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1)
      f-texto(INPUT i-col + 480, INPUT 100, INPUT SUBSTR(tt-itens.cod-refer,7,1),INPUT 16602, INPUT 13, INPUT 3, INPUT 0, INPUT 1)
      f-linha(input i-col + 570, input 0, input 110, INPUT 6, INPUT "V")
      f-texto(INPUT i-col + 605, INPUT 40, INPUT "DV",INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1)
      f-texto(INPUT i-col + 605, INPUT 100, INPUT ref-item-ext.dv,INPUT 16602, INPUT 13, INPUT 3, INPUT 0, INPUT 1)
      f-linha(input i-col + 670, input 0, input 110, INPUT 6, INPUT "V")
      f-texto(INPUT i-col + 725, INPUT 40, INPUT "NUANCE",INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1)
      f-texto(INPUT i-col + 750, INPUT 100, INPUT tt-itens.nuance,INPUT 16602, INPUT 13, INPUT 3, INPUT 0, INPUT 1)
      f-linha(input i-col + 10, input 105, input 870, INPUT 6, INPUT "H").

  PUT UNFORMATTED
      f-texto(INPUT i-col + 35, INPUT 150, INPUT c-desc-item, INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1)
      f-linha(input i-col + 10, input 160, input 870, INPUT 6, INPUT "H").

  PUT UNFORMATTED
      f-texto(INPUT i-col + 40, INPUT 200, INPUT "Seq",INPUT 16602, INPUT 8, INPUT 3, INPUT 0, INPUT 1)
      f-texto(INPUT i-col + 50, INPUT 255, INPUT STRING(i-sequencia,"999"),INPUT 16602, INPUT 6, INPUT 0, INPUT 0, INPUT 1)
      f-linha(input i-col + 130, input 160, input 120, INPUT 6, INPUT "V")
      f-texto(INPUT i-col + 145, INPUT 200, INPUT "Partida",INPUT 16602, INPUT 8, INPUT 3, INPUT 0, INPUT 1)
      f-texto(INPUT i-col + 170, INPUT 255, INPUT STRING(tt-itens.partida),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1)
      f-linha(input i-col + 275, input 160, input 120, INPUT 6, INPUT "V")
      f-texto(INPUT i-col + 295, INPUT 200, INPUT "Largura",INPUT 16602, INPUT 8, INPUT 3, INPUT 0, INPUT 1)
      f-texto(INPUT i-col + 330, INPUT 255, INPUT STRING(item-ext.largura,"9.99"),INPUT 16602, INPUT 6, INPUT 0, INPUT 0, INPUT 1)
      f-linha(input i-col + 435, input 160, input 120, INPUT 6, INPUT "V")
      f-texto(INPUT i-col + 460, INPUT 200, INPUT "Metros",INPUT 16602, INPUT 8, INPUT 3, INPUT 0, INPUT 1)
      f-texto(INPUT i-col + 455, INPUT 255, INPUT STRING(tt-itens.metro,">>>9.99"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1)
      f-linha(input i-col + 585, input 160, input 120, INPUT 6, INPUT "V")
      f-texto(INPUT i-col + 605, INPUT 200, INPUT "Qualid",INPUT 16602, INPUT 8, INPUT 3, INPUT 0, INPUT 1)
      f-texto(INPUT i-col + 630, INPUT 255, INPUT tt-itens.qualidade,INPUT 16602, INPUT 6, INPUT 0, INPUT 0, INPUT 1)
      f-linha(input i-col + 720, input 160, input 120, INPUT 6, INPUT "V")
      f-texto(INPUT i-col + 740, INPUT 200, INPUT "Embal.",INPUT 16602, INPUT 8, INPUT 3, INPUT 0, INPUT 1)
      f-texto(INPUT i-col + 765, INPUT 255, INPUT tt-itens.embalagem,INPUT 16602, INPUT 6, INPUT 0, INPUT 0, INPUT 1)
      f-linha(input i-col + 10, input 280, input 870, INPUT 6, INPUT "H").
  
  PUT UNFORMATTED
      codigo-barras39 (input i-col + 30,
                       input 300,
                       input TRIM(tt-itens.it-codigo) +
                             TRIM(tt-itens.cod-refer) +
                             STRING(ref-item-ext.dv) +
                             STRING(tt-itens.metro,"9999.99") +
                             STRING(tt-itens.embalagem)).

  PUT UNFORMATTED
      f-retangulo(input i-col + 10, input 600, input i-col + 875, INPUT 1595, INPUT 6)
      f-imp-macro(INPUT i-col + 25, INPUT 680, INPUT 10)  
      f-texto(INPUT i-col + 30, INPUT 645, INPUT "TEAR TEXTIL INDUSTRIA E COMERCIO LTDA",INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1)
      f-texto(INPUT i-col + 250, INPUT 680, INPUT "Av. General David Sarnoff, 5005 - D",INPUT 16602, INPUT 8, INPUT 3, INPUT 0, INPUT 1)
      f-texto(INPUT i-col + 250, INPUT 715, INPUT "32210-110 - CONTAGEM - MG - BRASIL",INPUT 16602, INPUT 8, INPUT 3, INPUT 0, INPUT 1)
      f-texto(INPUT i-col + 250, INPUT 750, INPUT "CNPJ  03.123.987/0002-00",INPUT 16602, INPUT 8, INPUT 3, INPUT 0, INPUT 1)
      f-texto(INPUT i-col + 250, INPUT 785, INPUT "Insc. Est.  186020807.0187",INPUT 16602, INPUT 8, INPUT 3, INPUT 0, INPUT 1)
      f-linha(input i-col + 10, input 790, input 870, INPUT 6, INPUT "H").
  
  PUT UNFORMATTED
      f-texto(INPUT i-col + 50, INPUT 830, INPUT "PRODUTO",INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1)
      f-texto(INPUT i-col + 25, INPUT 890, INPUT SUBSTR(tt-itens.it-codigo,1,6),INPUT 16602, INPUT 13, INPUT 3, INPUT 0, INPUT 1)
      f-linha(input i-col + 210, input 790, input 110, INPUT 6, INPUT "V")
      f-texto(INPUT i-col + 225, INPUT 830, INPUT "ACB/DES/COR",INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1)
      f-texto(INPUT i-col + 220, INPUT 890, INPUT SUBSTR(tt-itens.cod-refer,1,2),INPUT 16602, INPUT 13, INPUT 3, INPUT 0, INPUT 1)
      f-texto(INPUT i-col + 295, INPUT 890, INPUT SUBSTR(tt-itens.cod-refer,3,4),INPUT 16602, INPUT 13, INPUT 3, INPUT 0, INPUT 1)
      f-linha(input i-col + 420, input 790, input 110, INPUT 6, INPUT "V")
      f-texto(INPUT i-col + 473, INPUT 830, INPUT "VAR",INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1)
      f-texto(INPUT i-col + 480, INPUT 890, INPUT SUBSTR(tt-itens.cod-refer,7,1),INPUT 16602, INPUT 13, INPUT 3, INPUT 0, INPUT 1)
      f-linha(input i-col + 570, input 790, input 110, INPUT 6, INPUT "V")
      f-texto(INPUT i-col + 605, INPUT 830, INPUT "DV",INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1)
      f-texto(INPUT i-col + 605, INPUT 890, INPUT ref-item-ext.dv,INPUT 16602, INPUT 13, INPUT 3, INPUT 0, INPUT 1)
      f-linha(input i-col + 670, input 790, input 110, INPUT 6, INPUT "V")
      f-texto(INPUT i-col + 725, INPUT 830, INPUT "NUANCE",INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1)
      f-texto(INPUT i-col + 750, INPUT 890, INPUT tt-itens.nuance,INPUT 16602, INPUT 13, INPUT 3, INPUT 0, INPUT 1)
      f-linha(input i-col + 10, input 895, input 870, INPUT 6, INPUT "H").

  PUT UNFORMATTED
      f-texto(INPUT i-col + 35, INPUT 940, INPUT c-desc-item, INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1)
      f-linha(input i-col + 10, input 950, input 870, INPUT 6, INPUT "H").

  PUT UNFORMATTED
      f-texto(INPUT i-col + 40, INPUT 990, INPUT "Seq",INPUT 16602, INPUT 8, INPUT 3, INPUT 0, INPUT 1)
      f-texto(INPUT i-col + 50, INPUT 1045, INPUT STRING(i-sequencia,"999"),INPUT 16602, INPUT 6, INPUT 0, INPUT 0, INPUT 1)
      f-linha(input i-col + 130, input 950, input 120, INPUT 6, INPUT "V")
      f-texto(INPUT i-col + 145, INPUT 990, INPUT "Partida",INPUT 16602, INPUT 8, INPUT 3, INPUT 0, INPUT 1)
      f-texto(INPUT i-col + 170, INPUT 1045, INPUT STRING(tt-itens.partida),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1)
      f-linha(input i-col + 275, input 950, input 120, INPUT 6, INPUT "V")
      f-texto(INPUT i-col + 295, INPUT 990, INPUT "Largura",INPUT 16602, INPUT 8, INPUT 3, INPUT 0, INPUT 1)
      f-texto(INPUT i-col + 330, INPUT 1045, INPUT STRING(item-ext.largura,"9.99"),INPUT 16602, INPUT 6, INPUT 0, INPUT 0, INPUT 1)
      f-linha(input i-col + 435, input 950, input 120, INPUT 6, INPUT "V")
      f-texto(INPUT i-col + 460, INPUT 990, INPUT "Metros",INPUT 16602, INPUT 8, INPUT 3, INPUT 0, INPUT 1)
      f-texto(INPUT i-col + 455, INPUT 1045, INPUT STRING(tt-itens.metro,">>>9.99"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1)
      f-linha(input i-col + 585, input 950, input 120, INPUT 6, INPUT "V")
      f-texto(INPUT i-col + 605, INPUT 990, INPUT "Qualid",INPUT 16602, INPUT 8, INPUT 3, INPUT 0, INPUT 1)
      f-texto(INPUT i-col + 630, INPUT 1045, INPUT tt-itens.qualidade,INPUT 16602, INPUT 6, INPUT 0, INPUT 0, INPUT 1)
      f-linha(input i-col + 720, input 950, input 120, INPUT 6, INPUT "V")
      f-texto(INPUT i-col + 740, INPUT 990, INPUT "Embal.",INPUT 16602, INPUT 8, INPUT 3, INPUT 0, INPUT 1)
      f-texto(INPUT i-col + 765, INPUT 1045, INPUT tt-itens.embalagem,INPUT 16602, INPUT 6, INPUT 0, INPUT 0, INPUT 1)
      f-linha(input i-col + 10, input 1070, input 870, INPUT 6, INPUT "H").

  PUT UNFORMATTED
      f-texto(INPUT i-col + 50, INPUT 1110, INPUT "Em caso de Reclamacao favor devolver esta",INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1)
      f-texto(INPUT i-col + 380, INPUT 1150, INPUT "etiqueta",INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1)
      f-linha(input i-col + 10, input 1160, input 870, INPUT 6, INPUT "H").

  PUT UNFORMATTED
      f-texto(INPUT i-col + 30, INPUT 1195, INPUT "Composicao",INPUT 16602, INPUT 8, INPUT 3, INPUT 0, INPUT 1)
      f-texto(INPUT i-col + 30, INPUT 1230, INPUT c-composicao1,INPUT 16602, INPUT 6, INPUT 0, INPUT 0, INPUT 1)
      f-texto(INPUT i-col + 30, INPUT 1255, INPUT c-composicao2,INPUT 16602, INPUT 6, INPUT 0, INPUT 0, INPUT 1)
      f-linha(input i-col + 10, input 1280, input 870, INPUT 6, INPUT "H").

  PUT UNFORMATTED
      f-linha(input i-col + 10, input 1440, input 870, INPUT 6, INPUT "H")
      codigo-barras39 (input i-col + 30,
                       input 1300,
                       input TRIM(tt-itens.it-codigo) +
                             TRIM(tt-itens.cod-refer) +
                             STRING(ref-item-ext.dv) +
                             STRING(tt-itens.metro,"9999.99") +
                             STRING(tt-itens.embalagem))
      f-linha(input i-col + 370, input 1440, input 155, INPUT 6, INPUT "V")
      codigo-barras13 (input i-col + 50,
                       input 1460, 
                       input STRING(ref-item-ext.cod-ean,"999999999999")).


  IF AVAIL item-ext THEN DO. /*Escolhendo a imagem para jogar na etiqueta*/
     CASE item-ext.cod-rlgp:
        WHEN 1 THEN
            PUT UNFORMATTED
                f-imp-macro(INPUT i-col + 375, INPUT 1490, INPUT 11) 
                f-imp-macro(INPUT i-col + 480, INPUT 1490, INPUT 12)
                f-imp-macro(INPUT i-col + 580, INPUT 1490, INPUT 13)
                f-imp-macro(INPUT i-col + 680, INPUT 1510, INPUT 14)
                f-imp-macro(INPUT i-col + 780, INPUT 1480, INPUT 15).
        WHEN 2 THEN
            PUT UNFORMATTED
                f-imp-macro(INPUT i-col + 375, INPUT 1490, INPUT 11) 
                f-imp-macro(INPUT i-col + 480, INPUT 1490, INPUT 12)
                f-imp-macro(INPUT i-col + 580, INPUT 1490, INPUT 16)
                f-imp-macro(INPUT i-col + 680, INPUT 1510, INPUT 14)
                f-imp-macro(INPUT i-col + 780, INPUT 1480, INPUT 15).
        WHEN 3 THEN 
            PUT UNFORMATTED
                f-imp-macro(INPUT i-col + 377, INPUT 1490, INPUT 17) 
                f-imp-macro(INPUT i-col + 480, INPUT 1490, INPUT 12)
                f-imp-macro(INPUT i-col + 587, INPUT 1490, INPUT 16)
                f-imp-macro(INPUT i-col + 680, INPUT 1510, INPUT 14)
                f-imp-macro(INPUT i-col + 780, INPUT 1480, INPUT 15).
        WHEN 4 THEN 
            PUT UNFORMATTED
                f-imp-macro(INPUT i-col + 375, INPUT 1490, INPUT 18) 
                f-imp-macro(INPUT i-col + 480, INPUT 1490, INPUT 12)
                f-imp-macro(INPUT i-col + 580, INPUT 1490, INPUT 19)
                f-imp-macro(INPUT i-col + 680, INPUT 1510, INPUT 20)
                f-imp-macro(INPUT i-col + 780, INPUT 1480, INPUT 15).
     END CASE.
  END.

  PUT UNFORMATTED
      f-retangulo(input i-col + 10, input 1760, input i-col + 875, INPUT 2190, INPUT 6).

  PUT UNFORMATTED
      codigo-barras39 (input i-col + 30,
                       input 1780,
                       input TRIM(tt-itens.it-codigo) +
                             TRIM(tt-itens.cod-refer) +
                             STRING(ref-item-ext.dv) +
                             STRING(tt-itens.metro,"9999.99") +
                             STRING(tt-itens.embalagem))
      f-linha(input i-col + 10, input 1910, input 870, INPUT 6, INPUT "H").

  PUT UNFORMATTED
      f-texto(INPUT i-col + 50, INPUT 1950, INPUT "PRODUTO",INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1)
      f-texto(INPUT i-col + 25, INPUT 2010, INPUT SUBSTR(tt-itens.it-codigo,1,6),INPUT 16602, INPUT 13, INPUT 3, INPUT 0, INPUT 1)
      f-linha(input i-col + 210, input 1910, input 110, INPUT 6, INPUT "V")
      f-texto(INPUT i-col + 225, INPUT 1950, INPUT "ACB   DES/COR",INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1)
      f-texto(INPUT i-col + 220, INPUT 2010, INPUT SUBSTR(tt-itens.cod-refer,1,2),INPUT 16602, INPUT 13, INPUT 3, INPUT 0, INPUT 1)
      f-texto(INPUT i-col + 295, INPUT 2010, INPUT SUBSTR(tt-itens.cod-refer,3,4),INPUT 16602, INPUT 13, INPUT 3, INPUT 0, INPUT 1)
      f-linha(input i-col + 420, input 1910, input 110, INPUT 6, INPUT "V")
      f-texto(INPUT i-col + 473, INPUT 1950, INPUT "VAR",INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1)
      f-texto(INPUT i-col + 480, INPUT 2010, INPUT SUBSTR(tt-itens.cod-refer,7,1),INPUT 16602, INPUT 13, INPUT 3, INPUT 0, INPUT 1)
      f-linha(input i-col + 570, input 1910, input 110, INPUT 6, INPUT "V")
      f-texto(INPUT i-col + 605, INPUT 1950, INPUT "DV",INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1)
      f-texto(INPUT i-col + 605, INPUT 2010, INPUT ref-item-ext.dv,INPUT 16602, INPUT 13, INPUT 3, INPUT 0, INPUT 1)
      f-linha(input i-col + 670, input 1910, input 110, INPUT 6, INPUT "V")
      f-texto(INPUT i-col + 725, INPUT 1950, INPUT "NUANCE",INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1)
      f-texto(INPUT i-col + 750, INPUT 2010, INPUT tt-itens.nuance,INPUT 16602, INPUT 13, INPUT 3, INPUT 0, INPUT 1)
      f-linha(input i-col + 10, input 2015, input 870, INPUT 6, INPUT "H").

  PUT UNFORMATTED
      f-texto(INPUT i-col + 35, INPUT 2060, INPUT c-desc-item, INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1)
      f-linha(input i-col + 10, input 2070, input 870, INPUT 6, INPUT "H").

  PUT UNFORMATTED
      f-texto(INPUT i-col + 40, INPUT 2110, INPUT "Seq",INPUT 16602, INPUT 8, INPUT 3, INPUT 0, INPUT 1)
      f-texto(INPUT i-col + 50, INPUT 2165, INPUT STRING(i-sequencia,"999"),INPUT 16602, INPUT 6, INPUT 0, INPUT 0, INPUT 1)
      f-linha(input i-col + 130, input 2070, input 120, INPUT 6, INPUT "V")
      f-texto(INPUT i-col + 145, INPUT 2110, INPUT "Partida",INPUT 16602, INPUT 8, INPUT 3, INPUT 0, INPUT 1)
      f-texto(INPUT i-col + 170, INPUT 2165, INPUT STRING(tt-itens.partida),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1)
      f-linha(input i-col + 275, input 2070, input 120, INPUT 6, INPUT "V")
      f-texto(INPUT i-col + 295, INPUT 2110, INPUT "Largura",INPUT 16602, INPUT 8, INPUT 3, INPUT 0, INPUT 1)
      f-texto(INPUT i-col + 330, INPUT 2165, INPUT STRING(item-ext.largura,"9.99"),INPUT 16602, INPUT 6, INPUT 0, INPUT 0, INPUT 1)
      f-linha(input i-col + 435, input 2070, input 120, INPUT 6, INPUT "V")
      f-texto(INPUT i-col + 460, INPUT 2110, INPUT "Metros",INPUT 16602, INPUT 8, INPUT 3, INPUT 0, INPUT 1)
      f-texto(INPUT i-col + 455, INPUT 2165, INPUT STRING(tt-itens.metro,">>>9.99"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1)
      f-linha(input i-col + 585, input 2070, input 120, INPUT 6, INPUT "V")
      f-texto(INPUT i-col + 605, INPUT 2110, INPUT "Qualid",INPUT 16602, INPUT 8, INPUT 3, INPUT 0, INPUT 1)
      f-texto(INPUT i-col + 630, INPUT 2165, INPUT tt-itens.qualidade,INPUT 16602, INPUT 6, INPUT 0, INPUT 0, INPUT 1)
      f-linha(input i-col + 720, input 2070, input 120, INPUT 6, INPUT "V")
      f-texto(INPUT i-col + 740, INPUT 2110, INPUT "Embal.",INPUT 16602, INPUT 8, INPUT 3, INPUT 0, INPUT 1)
      f-texto(INPUT i-col + 765, INPUT 2165, INPUT tt-itens.embalagem,INPUT 16602, INPUT 6, INPUT 0, INPUT 0, INPUT 1).

END PROCEDURE.

