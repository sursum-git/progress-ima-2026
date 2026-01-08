DEF SHARED TEMP-TABLE tt-disp-colunas
    FIELD ordem  AS INT
    FIELD coluna AS CHAR.

DEF SHARED TEMP-TABLE tt-conteudo
    FIELD cod-emitente   AS INT FORMAT ">>>>>9"
    FIELD nome-ab-cli    AS CHAR FORMAT "x(25)"          
    FIELD nome-emit      AS CHAR FORMAT "x(45)"          
    FIELD endereco       AS CHAR FORMAT "x(45)"          
    FIELD cidade         AS CHAR FORMAT "x(45)"          
    FIELD bairro         AS CHAR FORMAT "x(30)"          
    FIELD estado         AS CHAR FORMAT "X(2)" 
    FIELD CEP            AS CHAR FORMAT "x(20)"
    FIELD endereco-cob   AS CHAR FORMAT "x(45)"          
    FIELD cidade-cob     AS CHAR FORMAT "x(45)"          
    FIELD bairro-cob     AS CHAR FORMAT "x(30)"          
    FIELD estado-cob     AS CHAR FORMAT "X(2)"           
    FIELD CEP-cob        AS CHAR FORMAT "x(20)"
    FIELD email          AS CHAR FORMAT "x(60)" 
    FIELD ind-cre-cli    AS INT FORMAT "9"
    FIELD cod-ramo-ativ  AS INT FORMAT ">>>>>9"
    FIELD desc-ramo-ativ AS CHAR FORMAT "x(45)"
    FIELD telefone       AS CHAR FORMAT "x(40)"
    FIELD dt-ult-compra  AS DATE
    FIELD tot-venda      AS DEC
    FIELD cod-rep-ven    AS INT
    FIELD nom-rep-ven    AS CHAR FORMAT "x(45)"
    FIELD cod-rep-cad    AS INT
    FIELD nom-rep-cad    AS CHAR FORMAT "x(45)"
    FIELD cgc            AS CHAR FORMAT "x(45)".

DEFINE NEW GLOBAL SHARED VARIABLE caminho       AS CHARACTER  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE nome-arquivo  AS CHARACTER  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE var-tg-partir AS LOG  NO-UNDO.

DEFINE NEW GLOBAL SHARED VARIABLE salvar-como AS CHARACTER  NO-UNDO.


DEFINE VARIABLE chExcelApp  AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorkBook  AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorkSheet AS COM-HANDLE NO-UNDO.

DEF VAR coluna     AS INT.
DEF VAR i-linha    AS INTEGER NO-UNDO.
DEF VAR h-acomp    AS HANDLE NO-UNDO.
DEF VAR c-colunas  AS CHAR.
DEF VAR i-ct       AS INT.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
{utp/ut-liter.i Imprimindo *}
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

&IF DEFINED(EXCLUDE-fn-letra) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-letra Procedure 
FUNCTION fn-letra RETURNS CHARACTER
  ( coluna AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


FOR EACH tt-disp-colunas NO-LOCK BREAK BY tt-disp-colunas.ordem.
    ASSIGN c-colunas = IF c-colunas = "" 
                       THEN TRIM(tt-disp-colunas.coluna)
                       ELSE c-colunas + "#" + TRIM(tt-disp-colunas.coluna).
END.

IF var-tg-partir THEN DO:
   FOR EACH tt-conteudo BREAK BY tt-conteudo.nom-rep-ven. 
       RUN  pi-acompanhar IN  h-acomp (INPUT "Gerando xls - " + STRING(tt-conteudo.nom-rep-ven) + '-' + STRING(tt-conteudo.cod-emitente)).
       IF FIRST-OF(tt-conteudo.nom-rep-ven) THEN DO: 
           ASSIGN i-linha = 1.
           CREATE "Excel.Application" chExcelApp.
           chWorkBook = chExcelApp:Workbooks:ADD(SEARCH("modelo-xlt\mala_direta.xlt")).
           chWorkSheet = chExcelApp:Sheets:Item(1).
           chexcelapp:VISIBLE = FALSE.
           chWorkSheet:Range(fn-letra(1) + STRING(i-linha)):VALUE = TRIM(tt-conteudo.nom-rep-ven).
           ASSIGN i-linha = i-linha + 1.
           FOR EACH tt-disp-colunas NO-LOCK BREAK BY tt-disp-colunas.ordem.
               ASSIGN coluna = coluna + 1. 
               chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = TRIM(tt-disp-colunas.coluna).
           END.
           ASSIGN i-linha = 3
                  coluna  = 0.
       END.

       DO i-ct = 1 TO NUM-ENTRIES(c-colunas,"#").
          CASE ENTRY(i-ct,c-colunas,"#").
              WHEN  "cod-emitente" THEN DO.
                  ASSIGN coluna = coluna + 1. 
                  chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = tt-conteudo.cod-emit.
              END.
              WHEN  "nome-ab-cli" THEN DO.
                  ASSIGN coluna = coluna + 1. 
                  chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = tt-conteudo.nome-ab-cli.
              END.
              WHEN  "nome-emit" THEN DO. 
                  ASSIGN coluna = coluna + 1. 
                  chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = tt-conteudo.nome-emit.              
              END.
              WHEN  "endereco" THEN DO. 
                  ASSIGN coluna = coluna + 1. 
                  chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = tt-conteudo.endereco.
              END.
              WHEN  "bairro" THEN DO. 
                  ASSIGN coluna = coluna + 1. 
                  chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = tt-conteudo.bairro.
              END.
              WHEN  "cidade" THEN DO. 
                  ASSIGN coluna = coluna + 1. 
                  chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = tt-conteudo.cidade.
              END.
              WHEN  "estado" THEN DO. 
                  ASSIGN coluna = coluna + 1. 
                  chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = tt-conteudo.estado.
              END.
              WHEN  "CEP" THEN DO. 
                  ASSIGN coluna = coluna + 1. 
                  chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = tt-conteudo.cep.
              END.
              WHEN  "endereco-cob" THEN DO. 
                  ASSIGN coluna = coluna + 1. 
                  chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = tt-conteudo.endereco-cob.
              END.
              WHEN  "bairro-cob" THEN DO. 
                  ASSIGN coluna = coluna + 1. 
                  chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = tt-conteudo.bairro-cob.
              END.
              WHEN  "cidade-cob" THEN DO. 
                  ASSIGN coluna = coluna + 1. 
                  chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = tt-conteudo.cidade-cob.
              END.
              WHEN  "estado-cob" THEN DO. 
                  ASSIGN coluna = coluna + 1. 
                  chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = tt-conteudo.estado-cob.
              END.
              WHEN  "CEP-cob" THEN DO.
                  ASSIGN coluna = coluna + 1. 
                   chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = tt-conteudo.cep-cob.
              END.
              WHEN  "email" THEN DO. 
                  ASSIGN coluna = coluna + 1. 
                  chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = tt-conteudo.email.
              END.
              WHEN  "cod-ramo-ativ" THEN DO.
                   ASSIGN coluna = coluna + 1. 
                   chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = tt-conteudo.cod-ramo-ativ.
              END.
              WHEN  "desc-ramo-ativ" THEN DO.
                   ASSIGN coluna = coluna + 1. 
                   chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = tt-conteudo.desc-ramo-ativ.
              END.
              WHEN  "telefone" THEN DO.
                   ASSIGN coluna = coluna + 1. 
                   chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = tt-conteudo.telefone.
              END.
              WHEN  "dt-ult-compra" THEN DO.
                   ASSIGN coluna = coluna + 1. 
                   chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = tt-conteudo.dt-ult-compra.
              END.
              WHEN  "tot-venda" THEN DO. 
                  ASSIGN coluna = coluna + 1. 
                  chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = tt-conteudo.tot-venda.
              END.
              WHEN  "cod-rep-ven" THEN DO. 
                  ASSIGN coluna = coluna + 1. 
                  chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = tt-conteudo.cod-rep-ven.
              END.
              WHEN  "nom-rep-ven" THEN DO.
                   ASSIGN coluna = coluna + 1. 
                   chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = tt-conteudo.nom-rep-ven.
              END.
              WHEN  "cod-rep-cad" THEN DO.
                   ASSIGN coluna = coluna + 1. 
                   chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = tt-conteudo.cod-rep-cad.
              END.
              WHEN  "nom-rep-cad" THEN DO. 
                  ASSIGN coluna = coluna + 1. 
                  chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = tt-conteudo.nom-rep-cad.
              END.
              WHEN  "cgc" THEN DO.
                   ASSIGN coluna = coluna + 1. 
                   chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = tt-conteudo.cgc.
              END.
          END CASE.
       END.

       ASSIGN i-linha = i-linha + 1
              coluna  = 0.
       If LAST-OF(tt-conteudo.nom-rep-ven) THEN  DO:
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
     chWorkBook = chExcelApp:Workbooks:Add(search("modelo-xlt\mala_direta.xlt")).
     chWorkSheet = chExcelApp:Sheets:Item(1).
     chexcelapp:VISIBLE = FALSE.
     chWorkSheet:Range(fn-letra(1) + STRING(i-linha)):VALUE = STRING(TODAY).
     ASSIGN i-linha = i-linha + 1.
     FOR EACH tt-disp-colunas NO-LOCK BREAK BY tt-disp-colunas.ordem.
         ASSIGN coluna = coluna + 1.
         chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = trim(tt-disp-colunas.coluna).
     END.
     ASSIGN i-linha = 3
            coluna  = 0.
   
     FOR EACH tt-conteudo. 
         RUN  pi-acompanhar IN  h-acomp (INPUT "Gerando xls - " + STRING(i-linha) + '-' + STRING(tt-conteudo.cod-emitente)).

         DO i-ct = 1 TO NUM-ENTRIES(c-colunas,"#").
            CASE ENTRY(i-ct,c-colunas,"#").
                WHEN  "cod-emitente" THEN DO.
                    ASSIGN coluna = coluna + 1. 
                    chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = tt-conteudo.cod-emit.
                END.
                WHEN  "nome-ab-cli" THEN DO.
                    ASSIGN coluna = coluna + 1. 
                    chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = tt-conteudo.nome-ab-cli.
                END.
                WHEN  "nome-emit" THEN DO. 
                    ASSIGN coluna = coluna + 1. 
                    chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = tt-conteudo.nome-emit.              
                END.
                WHEN  "endereco" THEN DO. 
                    ASSIGN coluna = coluna + 1. 
                    chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = tt-conteudo.endereco.
                END.
                WHEN  "bairro" THEN DO. 
                    ASSIGN coluna = coluna + 1. 
                    chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = tt-conteudo.bairro.
                END.
                WHEN  "cidade" THEN DO. 
                    ASSIGN coluna = coluna + 1. 
                    chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = tt-conteudo.cidade.
                END.
                WHEN  "estado" THEN DO. 
                    ASSIGN coluna = coluna + 1. 
                    chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = tt-conteudo.estado.
                END.
                WHEN  "CEP" THEN DO. 
                    ASSIGN coluna = coluna + 1. 
                    chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = tt-conteudo.cep.
                END.
                WHEN  "endereco-cob" THEN DO. 
                    ASSIGN coluna = coluna + 1. 
                    chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = tt-conteudo.endereco-cob.
                END.
                WHEN  "bairro-cob" THEN DO. 
                    ASSIGN coluna = coluna + 1. 
                    chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = tt-conteudo.bairro-cob.
                END.
                WHEN  "cidade-cob" THEN DO. 
                    ASSIGN coluna = coluna + 1. 
                    chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = tt-conteudo.cidade-cob.
                END.
                WHEN  "estado-cob" THEN DO. 
                    ASSIGN coluna = coluna + 1. 
                    chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = tt-conteudo.estado-cob.
                END.
                WHEN  "CEP-cob" THEN DO.
                    ASSIGN coluna = coluna + 1. 
                     chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = tt-conteudo.cep-cob.
                END.
                WHEN  "email" THEN DO. 
                    ASSIGN coluna = coluna + 1. 
                    chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = tt-conteudo.email.
                END.
                WHEN  "cod-ramo-ativ" THEN DO.
                     ASSIGN coluna = coluna + 1. 
                     chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = tt-conteudo.cod-ramo-ativ.
                END.
                WHEN  "desc-ramo-ativ" THEN DO.
                     ASSIGN coluna = coluna + 1. 
                     chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = tt-conteudo.desc-ramo-ativ.
                END.
                WHEN  "telefone" THEN DO.
                     ASSIGN coluna = coluna + 1. 
                     chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = tt-conteudo.telefone.
                END.
                WHEN  "dt-ult-compra" THEN DO.
                     ASSIGN coluna = coluna + 1. 
                     chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = tt-conteudo.dt-ult-compra.
                END.
                WHEN  "tot-venda" THEN DO. 
                    ASSIGN coluna = coluna + 1. 
                    chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = tt-conteudo.tot-venda.
                END.
                WHEN  "cod-rep-ven" THEN DO. 
                    ASSIGN coluna = coluna + 1. 
                    chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = tt-conteudo.cod-rep-ven.
                END.
                WHEN  "nom-rep-ven" THEN DO.
                     ASSIGN coluna = coluna + 1. 
                     chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = tt-conteudo.nom-rep-ven.
                END.
                WHEN  "cod-rep-cad" THEN DO.
                     ASSIGN coluna = coluna + 1. 
                     chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = tt-conteudo.cod-rep-cad.
                END.
                WHEN  "nom-rep-cad" THEN DO. 
                    ASSIGN coluna = coluna + 1. 
                    chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = tt-conteudo.nom-rep-cad.
                END.
                WHEN  "cgc" THEN DO.
                     ASSIGN coluna = coluna + 1. 
                     chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = tt-conteudo.cgc.
                END.
            END CASE.
         END.
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
run pi-finalizar in h-acomp.
