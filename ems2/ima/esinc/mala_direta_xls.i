DEFINE VARIABLE chExcelApp  AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorkBook  AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorkSheet AS COM-HANDLE NO-UNDO.
DEF VAR coluna    AS INT.
DEF VAR i-linha AS INTEGER NO-UNDO.

def var h-acomp as handle no-undo.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

IF var-tg-partir THEN DO:
   FOR EACH tt-conteudo BREAK BY tt-conteudo.nom-rep-ven. /* {1} = breakby + " BY tt-conteudo." + tt-disp-colunas.coluna.*/
       RUN  pi-acompanhar IN  h-acomp (INPUT "Gerando xls - " + STRING(tt-conteudo.nom-rep-ven) + '-' + STRING(tt-conteudo.cod-emitente)).
       If FIRST-OF(tt-conteudo.nom-rep-ven) then  DO: /* {3} = "If first-of(tt-conteudo." + tt-disp-colunas.coluna + ") then " */
           ASSIGN i-linha = 1.
           CREATE "Excel.Application" chExcelApp.
           chWorkBook = chExcelApp:Workbooks:Add("M:\Ems206\especificos\modelo-xlt\mala_direta.xlt").
           /*chWorkBook = chExcelApp:Workbooks:Add().*/
           chWorkSheet = chExcelApp:Sheets:Item(1).
           chexcelapp:VISIBLE = FALSE.
           chWorkSheet:Range(fn-letra(1) + STRING(i-linha)):VALUE = trim(tt-conteudo.nom-rep-ven).
           ASSIGN i-linha = i-linha + 1.
           FOR EACH tt-disp-colunas NO-LOCK BREAK BY tt-disp-colunas.ordem.
               ASSIGN coluna = coluna + 1. chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = trim(tt-disp-colunas.coluna).
           END.
           ASSIGN i-linha = 3
                  coluna  = 0.
       END.
       {2} /* {2} = var-put + ' ASSIGN coluna = coluna + 1. chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = tt-conteudo.' + TRIM(tt-disp-colunas.coluna) + '.'.*/
       ASSIGN i-linha = i-linha + 1
              coluna  = 0.
       If last-of(tt-conteudo.nom-rep-ven) then  DO:
           ASSIGN salvar-como = caminho + nome-arquivo + "-" + tt-conteudo.nom-rep-ven + ".xls".
           RELEASE OBJECT chWorkSheet.
           chWorkBook:SaveAs(salvar-como,-4143,,,,,).
           chWorkBook:Save().        
                                     
           chWorkBook:Close().       
           chExcelApp:Quit().        
                                     
           RELEASE OBJECT chWorkBook.
           RELEASE OBJECT chExcelApp.
       END.
   END.
END.
ELSE DO:
     ASSIGN i-linha = 1.
     CREATE "Excel.Application" chExcelApp.
     chWorkBook = chExcelApp:Workbooks:Add("M:\Ems206\especificos\modelo-xlt\mala_direta.xlt").
     /*chWorkBook = chExcelApp:Workbooks:Add().*/
     chWorkSheet = chExcelApp:Sheets:Item(1).
     chexcelapp:VISIBLE = FALSE.
     chWorkSheet:Range(fn-letra(1) + STRING(i-linha)):VALUE = STRING(TODAY).
     ASSIGN i-linha = i-linha + 1.
     FOR EACH tt-disp-colunas NO-LOCK BREAK BY tt-disp-colunas.ordem.
         ASSIGN coluna = coluna + 1. chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = trim(tt-disp-colunas.coluna).
     END.
     ASSIGN i-linha = 3
            coluna  = 0.
   
     FOR EACH tt-conteudo BREAK {1} . /* {1} = breakby + " BY tt-conteudo." + tt-disp-colunas.coluna.*/
     {2} /* {2} = var-put + ' ASSIGN coluna = coluna + 1. chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = tt-conteudo.' + TRIM(tt-disp-colunas.coluna) + '.'.*/
     RUN  pi-acompanhar IN  h-acomp (INPUT "Gerando xls - " + STRING(i-linha) + '-' + STRING(tt-conteudo.cod-emitente)).
     ASSIGN i-linha = i-linha + 1
            coluna  = 0.
     END.
     chexcelapp:VISIBLE = TRUE.
     /*ASSIGN salvar-como = caminho + nome-arquivo + ".xls".*/
     RELEASE OBJECT chWorkSheet.
     /*chWorkBook:SaveAs(salvar-como,-4143,,,,,).
     chWorkBook:Save().        
     */
     /*chWorkBook:Close().  
     chExcelApp:Quit().*/
     RELEASE OBJECT chWorkBook.
     RELEASE OBJECT chExcelApp.
END.
                       
/* ************************  Function Implementations ***************** */

/*
&IF DEFINED(EXCLUDE-fn-letra) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-letra Procedure 
FUNCTION fn-letra RETURNS CHARACTER
  ( coluna AS INT ) :
DEF VAR col-letra AS CHAR.
CASE coluna:
    WHEN 1  THEN ASSIGN col-letra = 'A'.
    WHEN 2  THEN ASSIGN col-letra = 'B'.
    WHEN 3  THEN ASSIGN col-letra = 'C'.
    WHEN 4  THEN ASSIGN col-letra = 'D'.
    WHEN 5  THEN ASSIGN col-letra = 'E'.
    WHEN 6  THEN ASSIGN col-letra = 'F'. 
    WHEN 7  THEN ASSIGN col-letra = 'G'. 
    WHEN 8  THEN ASSIGN col-letra = 'H'. 
    WHEN 9  THEN ASSIGN col-letra = 'I'. 
    WHEN 10 THEN ASSIGN col-letra = 'J'. 
    WHEN 11 THEN ASSIGN col-letra = 'K'. 
    WHEN 12 THEN ASSIGN col-letra = 'L'. 
    WHEN 13 THEN ASSIGN col-letra = 'M'. 
    WHEN 14 THEN ASSIGN col-letra = 'N'. 
    WHEN 15 THEN ASSIGN col-letra = 'O'. 
    WHEN 16 THEN ASSIGN col-letra = 'P'. 
    WHEN 17 THEN ASSIGN col-letra = 'Q'. 
    WHEN 18 THEN ASSIGN col-letra = 'R'. 
    WHEN 19 THEN ASSIGN col-letra = 'S'. 
    WHEN 20 THEN ASSIGN col-letra = 'T'. 
    WHEN 21 THEN ASSIGN col-letra = 'U'.
    WHEN 22 THEN ASSIGN col-letra = 'V'. 
    WHEN 23 THEN ASSIGN col-letra = 'W'. 
    WHEN 24 THEN ASSIGN col-letra = 'X'. 
    WHEN 25 THEN ASSIGN col-letra = 'Y'. 

END CASE.

  RETURN col-letra.   

END FUNCTION.


&ANALYZE-RESUME

&ENDIF
*/
run pi-finalizar in h-acomp.
