/******************************************************************************
* Programa..:   Etiqueta Embalagem
* Autor.....:   Marcelo Fulan
* Data......:   03/07/2009
* Finalidade:   Impress∆o da Etiqueta de Embalagem dos Produtos Com C¢digo de
*               Barras no padr∆o Cod-39.
******************************************************************************/
{PDFinclude/pdf_inc.i "THIS-PROCEDURE"}


DEF VAR Vold_Y      AS INTEGER NO-UNDO.
DEF VAR c-arquivo   AS CHAR FORMAT "x(17)" NO-UNDO.
DEF VAR c-cod-barra AS CHAR FORMAT 'x(16)'.
DEF VAR c-it-codigo AS CHAR FORMAT 'x(16)'.
DEF VAR i-lote      AS INT FORMAT '999999999'.

ASSIGN c-cod-barra = '1234567890123'
       c-it-codigo = 'XYKZ-123'
       i-lote      = 2009123456.

ASSIGN c-arquivo = "C:\temp\teste.pdf". 

/* Create stream for new PDF file */
RUN pdf_new  IN h_PDFinc ("Spdf",c-arquivo).

/* Parametrizar o local onde est† a fonte do c¢digo de barras */
/* RUN pdf_load_font  IN h_PDFinc ("Spdf","Code39","y:\ems2\_custom\glp\pdf\support\code39.ttf","y:\ems2\_custom\glp\pdf\support\code39.afm",""). */
 RUN pdf_load_font       IN h_PDFinc ("Spdf","code39",  "c:\WINDOWS\Fonts\BarcodeFont.ttf",  "modelos\BarCode39.afm","").   /* Importando Fonte */

RUN pdf_set_parameter("Spdf","PageLayout","OneColumn").

/* Formato Paisagem */
RUN pdf_new_page2("Spdf","Landscape").

RUN pdf_set_font IN h_PDFinc ("Spdf","Courier-Bold",18.0).
RUN pdf_text_xy("Spdf", STRING("Teste de Etiqueta de Codigo de Barras") , 1, 580).
RUN pdf_skip("Spdf").

RUN pdf_set_font IN h_PDFinc ("Spdf","Courier-Bold",90.0).
RUN pdf_text_xy("Spdf", c-it-codigo, 1, 510).
RUN pdf_skip("Spdf").
RUN pdf_skip("Spdf").
RUN pdf_text_rotate ("Spdf",0).
RUN pdf_set_font IN h_PDFinc ("Spdf","Code39",80.0).
/* Utilizar os inicializados e finalizadores '*' ou '<' e '>' */
RUN pdf_text IN h_PDFinc ("Spdf", c-it-codigo).
RUN pdf_skip("Spdf").
RUN pdf_skip("Spdf").

RUN pdf_set_font IN h_PDFinc ("Spdf","Courier",60.0).
RUN pdf_text_xy("Spdf", string(i-lote, "9999,999999"), 1, 460).
RUN pdf_skip("Spdf").
RUN pdf_text_rotate ("Spdf",0).
RUN pdf_set_font IN h_PDFinc ("Spdf","Code39",80.0).
/* Utilizar os inicializados e finalizadores '*' ou '<' e '>' */
RUN pdf_text IN h_PDFinc ("Spdf", STRING(i-lote)).
RUN pdf_skip("Spdf").

RUN pdf_text_rotate ("Spdf",0).
RUN pdf_set_font IN h_PDFinc ("Spdf","Code39",25.0).
/* Utilizar os inicializados e finalizadores '*' ou '<' e '>' */
RUN pdf_text_xy IN h_PDFinc ("Spdf", string('VI*') + STRING(c-cod-barra) + STRING('TOR*'), 1, 300).
        
RUN pdf_set_font IN h_PDFinc ("Spdf","Courier",14.0).
RUN pdf_text_xy IN h_PDFinc ("Spdf", STRING(c-cod-barra), 100, 290).

RUN pdf_text_rotate ("Spdf",45).
RUN pdf_set_font IN h_PDFinc ("Spdf","Code39",40.0).
/* Utilizar os inicializados e finalizadores '*' ou '<' e '>' */
RUN pdf_text IN h_PDFinc ("Spdf", string('FAB') + STRING(c-cod-barra) + STRING('IO')).

RUN pdf_close("Spdf").

/*Verifica se o arquivo existe e o abre*/
IF SEARCH (c-arquivo) <> ? THEN
    OS-COMMAND NO-WAIT VALUE(c-arquivo).
ELSE DO:
    RUN glp/MESSAGE.p('Arquivo n∆o encontado.','Arquivo n∆o encontrado.').
END.
