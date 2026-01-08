/******************************************************************************
* Programa..:   Etiqueta Embalagem
* Autor.....:   Marcelo Fulan
* Data......:   03/07/2009
* Finalidade:   Impress∆o da Etiqueta de Embalagem dos Produtos Com C¢digo de
*               Barras no padr∆o 2of5.
******************************************************************************/
{PDFinclude/pdf_inc.i "THIS-PROCEDURE"}


DEF VAR c-cod-barra AS CHAR FORMAT 'x(16)'.
DEF VAR c-arquivo   AS CHAR.

ASSIGN c-cod-barra = '1234567890123'.

/* Create stream for new PDF file */
ASSIGN c-arquivo = "C:\temp\teste.pdf". 
RUN pdf_new  IN h_PDFinc ("Spdf",c-arquivo).

/* Parametrizar o local onde est† a fonte do c¢digo de barras */
RUN pdf_load_font       IN h_PDFinc ("Spdf","cbarra25","c:\WINDOWS\Fonts\cbarra2of5.ttf",  "modelos\cbarra2of5.afm",""). /* Importando Fonte */

/* Formato Paisagem */
RUN pdf_new_page2("Spdf","Landscape").


/* Impress∆o do Codigo de Barra 2of5 */
RUN pdf_text_rotate ("Spdf",0).
RUN pdf_set_font IN h_PDFinc ("Spdf","cbarra25",30.0).
RUN pdf_text IN h_PDFinc ("Spdf", c-cod-barra).

RUN pdf_text        IN h_PDFinc ("Spdf", FILL(" ",5)).
RUN pdf_text IN h_PDFinc ("Spdf", c-cod-barra).
RUN pdf_text        IN h_PDFinc ("Spdf", FILL(" ",5)).
RUN pdf_text IN h_PDFinc ("Spdf", c-cod-barra).
RUN pdf_text        IN h_PDFinc ("Spdf", FILL(" ",5)).
RUN pdf_text IN h_PDFinc ("Spdf", c-cod-barra).

/*
RUN pdf_skipn       IN h_PDFinc ("Spdf",2).
RUN pdf_set_font IN h_PDFinc ("Spdf","cbarra25",20.0).
RUN pdf_text IN h_PDFinc ("Spdf", c-cod-barra).
RUN pdf_text        IN h_PDFinc ("Spdf", FILL(" ",5)).
RUN pdf_text IN h_PDFinc ("Spdf", c-cod-barra).
RUN pdf_text        IN h_PDFinc ("Spdf", FILL(" ",5)).
RUN pdf_text IN h_PDFinc ("Spdf", c-cod-barra).
RUN pdf_text        IN h_PDFinc ("Spdf", FILL(" ",5)).
RUN pdf_text IN h_PDFinc ("Spdf", c-cod-barra).

RUN pdf_skipn       IN h_PDFinc ("Spdf",2).
RUN pdf_set_font IN h_PDFinc ("Spdf","cbarra25",10.0).
RUN pdf_text IN h_PDFinc ("Spdf", c-cod-barra).
RUN pdf_text        IN h_PDFinc ("Spdf", FILL(" ",5)).
RUN pdf_text IN h_PDFinc ("Spdf", c-cod-barra).
RUN pdf_text        IN h_PDFinc ("Spdf", FILL(" ",5)).
RUN pdf_text IN h_PDFinc ("Spdf", c-cod-barra).
RUN pdf_text        IN h_PDFinc ("Spdf", FILL(" ",5)).
RUN pdf_text IN h_PDFinc ("Spdf", c-cod-barra).

RUN pdf_skipn       IN h_PDFinc ("Spdf",5).
RUN pdf_set_font IN h_PDFinc ("Spdf","cbarra25",40.0).
RUN pdf_text IN h_PDFinc ("Spdf", c-cod-barra).
RUN pdf_text        IN h_PDFinc ("Spdf", FILL(" ",5)).
RUN pdf_text IN h_PDFinc ("Spdf", c-cod-barra).
RUN pdf_text        IN h_PDFinc ("Spdf", FILL(" ",5)).
RUN pdf_text IN h_PDFinc ("Spdf", c-cod-barra).
RUN pdf_text        IN h_PDFinc ("Spdf", FILL(" ",5)).
RUN pdf_text IN h_PDFinc ("Spdf", c-cod-barra).

RUN pdf_skipn       IN h_PDFinc ("Spdf",2).
RUN pdf_set_font IN h_PDFinc ("Spdf","cbarra25",50.0).
RUN pdf_text IN h_PDFinc ("Spdf", c-cod-barra).
RUN pdf_text        IN h_PDFinc ("Spdf", FILL(" ",5)).
RUN pdf_text IN h_PDFinc ("Spdf", c-cod-barra).
RUN pdf_text        IN h_PDFinc ("Spdf", FILL(" ",5)).
RUN pdf_text IN h_PDFinc ("Spdf", c-cod-barra).
RUN pdf_text        IN h_PDFinc ("Spdf", FILL(" ",5)).
RUN pdf_text IN h_PDFinc ("Spdf", c-cod-barra).
*/
RUN pdf_close("Spdf").

/*Verifica se o arquivo existe e o abre*/
IF SEARCH (c-arquivo) <> ? THEN
    OS-COMMAND NO-WAIT VALUE(c-arquivo).
ELSE DO:
    RUN glp/MESSAGE.p('Arquivo n∆o encontado.','Arquivo n∆o encontrado.').
END.
