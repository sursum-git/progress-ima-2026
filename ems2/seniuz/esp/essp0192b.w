&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*:T*******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESSP0192B 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE TEMP-TABLE tt-work NO-UNDO 
       FIELD cod-estabel  LIKE ob-etiqueta.cod-estabel
       FIELD num-etiqueta LIKE ob-etiqueta.num-etiqueta
       FIELD dt-emissao   LIKE ob-etiqueta.dt-emissao
       FIELD hr-emissao   AS CHAR
       FIELD nr-lote      LIKE ob-etiqueta.nr-lote
       FIELD quantidade   LIKE ob-etiqueta.quantidade
       FIELD situacao     AS CHAR
       FIELD localiz      AS CHAR
       FIELD nr-reporte   LIKE ob-etiqueta.nr-reporte
       FIELD resp-revisao LIKE ob-etiqueta.resp-revisao
       FIELD nome-ab-cli  LIKE nota-fiscal.nome-ab-cli
       FIELD nr-pedcli    LIKE nota-fiscal.nr-pedcli
       FIELD nr-nota-fis  AS CHAR
       FIELD classif      AS CHAR FORMAT "x(4)".

DEFINE BUFFER b-tt-work FOR tt-work.

/* Variaveis de Parametros */
DEFINE INPUT PARAMETER p-cod-estabel LIKE ordem-benefic.cod-estabel.
DEFINE INPUT PARAMETER p-nr-ob       LIKE ordem-benefic.nr-ob.
DEFINE INPUT PARAMETER p-dt-ob       LIKE ordem-benefic.dt-ob.
DEFINE INPUT PARAMETER p-nr-carro    LIKE ordem-benefic.nr-carro.

/* Local Variable Definitions ---                                       */
DEF NEW GLOBAL SHARED VAR gr-ped-venda    AS ROWID NO-UNDO.
DEF NEW GLOBAL SHARED VAR gr-nota-fiscal  AS ROWID NO-UNDO.
DEF NEW GLOBAL SHARED VAR gr-ob-etiqueta  AS ROWID NO-UNDO.

DEF VAR h-acomp       AS HANDLE NO-UNDO.
DEF VAR h-query       AS HANDLE NO-UNDO.
DEF VAR c-situacao    AS CHAR FORMAT "x(12)".
DEF VAR c-nr-pedcli   AS CHAR.
DEF VAR i-nr-nota-fis AS INT.
DEF VAR c-nome-abrev  AS CHAR.
DEF VAR de-total      AS DEC.
DEF VAR de-tot-m      AS DEC.
DEF VAR de-tot-kg     AS DEC.
DEF VAR c-empresa     AS CHAR.

 /* Variaveis da Rotina de ImpressÆo */
DEFINE VAR l-ok         AS LOG.
DEFINE VAR i-saida      AS INT.
DEFINE VAR c-saida      AS CHAR.
DEFINE VAR i-num-copias AS INT.
DEFINE VAR i-ct         AS INT.
DEFINE VAR i-lin        AS INT.
DEFINE VAR i-pag        AS INT.

/* Variaveis para o Excel */
DEF VAR chExcelApp  AS COM-HANDLE NO-UNDO.
DEF VAR chWorkbook  AS COM-HANDLE NO-UNDO.
DEF VAR chworksheet AS COM-HANDLE NO-UNDO.
DEF VAR c-lin       AS CHAR FORMAT "x(500)".
DEF VAR arq-saida   AS CHAR FORMAT "x(50)".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog
&Scoped-define BROWSE-NAME br-work

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-work

/* Definitions for BROWSE br-work                                       */
&Scoped-define FIELDS-IN-QUERY-br-work tt-work.num-etiqueta tt-work.dt-emissao tt-work.hr-emissao tt-work.nr-lote tt-work.quantidade tt-work.situacao tt-work.localiz tt-work.nr-reporte tt-work.resp-revisao tt-work.nome-ab-cli tt-work.nr-pedcli tt-work.nr-nota-fis   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-work   
&Scoped-define SELF-NAME br-work
&Scoped-define QUERY-STRING-br-work FOR EACH tt-work NO-LOCK                               BY tt-work.nr-lote                               BY tt-work.dt-emissao
&Scoped-define OPEN-QUERY-br-work OPEN QUERY {&SELF-NAME} FOR EACH tt-work NO-LOCK                               BY tt-work.nr-lote                               BY tt-work.dt-emissao.
&Scoped-define TABLES-IN-QUERY-br-work tt-work
&Scoped-define FIRST-TABLE-IN-QUERY-br-work tt-work


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-br-work}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 rt-buttom br-work bt-vapara ~
bt-consulta bt-imprime bt-excel bt-exit bt-ok bt-cancela bt-ajuda 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-6 bt-excel 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     LABEL "&Ajuda" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-cancela AUTO-END-KEY 
     LABEL "&Cancelar" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-consulta 
     IMAGE-UP FILE "image/im-det.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Detalhar Etiqueta"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-excel 
     IMAGE-UP FILE "image/excel.bmp":U
     LABEL "Button 2" 
     SIZE 5 BY 1.29 TOOLTIP "Gerar Planilha".

DEFINE BUTTON bt-exit AUTO-END-KEY 
     IMAGE-UP FILE "image/im-exi.bmp":U
     LABEL "" 
     SIZE 4.86 BY 1.29 TOOLTIP "Fechar Programa"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-imprime 
     IMAGE-UP FILE "image/im-printer.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Imprimir as Ordem de Beneficiamento".

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "&OK" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-vapara 
     IMAGE-UP FILE "image/im-vapra.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Posicionar na Etiqueta"
     BGCOLOR 8 FONT 10.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 7 BY 14.92.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 93.29 BY 1.42
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-work FOR 
      tt-work SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-work
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-work D-Dialog _FREEFORM
  QUERY br-work NO-LOCK DISPLAY
      tt-work.num-etiqueta COLUMN-LABEL "Etiqueta"                  WIDTH  7.5   
      tt-work.dt-emissao   COLUMN-LABEL "Dt.Emissao"                WIDTH  8.2 
      tt-work.hr-emissao   COLUMN-LABEL "Hora"     FORMAT "x(7)"    WIDTH  6.3     
      tt-work.nr-lote      COLUMN-LABEL "Lote"                      WIDTH  3.5
      tt-work.quantidade   COLUMN-LABEL "Qtde"                      WIDTH  8
      tt-work.situacao     COLUMN-LABEL "Situacao" FORMAT "x(15)"   WIDTH  9.5
      tt-work.localiz      COLUMN-LABEL "Local"    FORMAT "999/999" WIDTH  6   
      tt-work.nr-reporte   COLUMN-LABEL "Reporte"                   WIDTH  5.8  
      tt-work.resp-revisao COLUMN-LABEL "Revisor"                   WIDTH  5.2   
      tt-work.nome-ab-cli  COLUMN-LABEL "Cliente"                   WIDTH 12         
      tt-work.nr-pedcli    COLUMN-LABEL "Pedido"                    WIDTH  5.5  
      tt-work.nr-nota-fis  COLUMN-LABEL "N.Fiscal"                  WIDTH  6
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 93.29 BY 13.5
         FONT 1
         TITLE "ETIQUETAS" ROW-HEIGHT-CHARS .46.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     br-work AT ROW 1 COL 1.72
     bt-vapara AT ROW 1.29 COL 96
     bt-consulta AT ROW 2.67 COL 96.14
     bt-imprime AT ROW 6.33 COL 96.14
     bt-excel AT ROW 7.63 COL 96.14
     bt-exit AT ROW 14.25 COL 96
     bt-ok AT ROW 14.79 COL 2.57
     bt-cancela AT ROW 14.79 COL 13.57
     bt-ajuda AT ROW 14.79 COL 82.14
     RECT-1 AT ROW 1 COL 95.14
     rt-buttom AT ROW 14.54 COL 1.86
     SPACE(7.56) SKIP(0.28)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 1
         TITLE "Etiquetas - ESSP0192B"
         DEFAULT-BUTTON bt-ok.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/d-dialog.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   NOT-VISIBLE FRAME-NAME L-To-R                                        */
/* BROWSE-TAB br-work rt-buttom D-Dialog */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON bt-excel IN FRAME D-Dialog
   6                                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-work
/* Query rebuild information for BROWSE br-work
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-work NO-LOCK
                              BY tt-work.nr-lote
                              BY tt-work.dt-emissao.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-work */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Etiquetas - ESSP0192B */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-work
&Scoped-define SELF-NAME br-work
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-work D-Dialog
ON ROW-DISPLAY OF br-work IN FRAME D-Dialog /* ETIQUETAS */
DO:
IF tt-work.dt-emissao = ? THEN
   ASSIGN tt-work.num-etiqueta:FGCOLOR IN BROWSE br-work = 15
          tt-work.nr-reporte:FGCOLOR IN BROWSE br-work   = 15
          tt-work.hr-emissao:FGCOLOR IN BROWSE br-work   = 12
          tt-work.hr-emissao:FONT IN BROWSE br-work      =  6
          tt-work.nr-lote:FGCOLOR IN BROWSE br-work      = 12
          tt-work.nr-lote:FONT IN BROWSE br-work         =  6
          tt-work.quantidade:FGCOLOR IN BROWSE br-work   = 12
          tt-work.quantidade:FONT IN BROWSE br-work      =  6. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-work D-Dialog
ON VALUE-CHANGED OF br-work IN FRAME D-Dialog /* ETIQUETAS */
DO:
    /*
  IF tt-work.dt-emissao = ? THEN
     ASSIGN tt-work.num-etiqueta:FGCOLOR IN BROWSE br-work = 1
            tt-work.nr-reporte:FGCOLOR IN BROWSE br-work   = 1.
   ELSE
       ASSIGN tt-work.num-etiqueta:FGCOLOR IN BROWSE br-work = 15
              tt-work.nr-reporte:FGCOLOR IN BROWSE br-work   = 15.
  */            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda D-Dialog
ON CHOOSE OF bt-ajuda IN FRAME D-Dialog /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-consulta D-Dialog
ON CHOOSE OF bt-consulta IN FRAME D-Dialog
DO:
   FIND ob-etiqueta WHERE
        ob-etiqueta.num-etiqueta = tt-work.num-etiqueta NO-LOCK NO-ERROR.
   ASSIGN gr-ob-etiqueta = ROWID(ob-etiqueta).

   ASSIGN FRAME {&FRAME-NAME}:VISIBLE = NO.
   RUN esp\essp0146.w.
   ASSIGN FRAME {&FRAME-NAME}:VISIBLE = YES.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-excel D-Dialog
ON CHOOSE OF bt-excel IN FRAME D-Dialog /* Button 2 */
DO:

    ASSIGN arq-saida = SESSION:TEMP-DIRECTORY + "Etiquetas da OB " +  STRING(p-nr-ob, "999999") + ".XLS".
    RUN esdlg/d02essp0192.w (INPUT-OUTPUT arq-saida).
    IF arq-saida <> "" AND arq-saida <> "NOK" THEN DO:
       RUN pi-gera-excel.
       MESSAGE "O arquivo foi gerado em " TRIM(arq-saida) + "." SKIP 
               "Para acess -lo,  abra-o atrav‚s do Excel."
           VIEW-AS ALERT-BOX INFO BUTTONS OK. 
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-exit D-Dialog
ON CHOOSE OF bt-exit IN FRAME D-Dialog
DO:
   APPLY 'CLOSE':U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-imprime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-imprime D-Dialog
ON CHOOSE OF bt-imprime IN FRAME D-Dialog
DO:
   ASSIGN FRAME {&FRAME-NAME}:VISIBLE = NO.
   RUN pi-imprime.
   ASSIGN FRAME {&FRAME-NAME}:VISIBLE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-vapara
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-vapara D-Dialog
ON CHOOSE OF bt-vapara IN FRAME D-Dialog
DO:
    DEF VAR i-num-etiqueta LIKE ob-etiqueta.num-etiqueta.

    RUN esdlg/d03essp0192.w (OUTPUT i-num-etiqueta).

    IF i-num-etiqueta <> 0 THEN DO:
       FIND tt-work WHERE
            tt-work.num-etiqueta = i-num-etiqueta NO-LOCK NO-ERROR.
       IF NOT AVAIL tt-work THEN DO.
          MESSAGE "Etiqueta nÆo est  contida na sele‡Æo!"
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN NO-APPLY.
       END.

       h-query:REPOSITION-TO-ROWID(ROWID(tt-work)) NO-ERROR. 
       APPLY 'VALUE-CHANGED' TO br-work.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */
ASSIGN h-query = br-work:QUERY.

{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  ENABLE RECT-1 rt-buttom br-work bt-vapara bt-consulta bt-imprime bt-excel 
         bt-exit bt-ok bt-cancela bt-ajuda 
      WITH FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy D-Dialog 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .
  {include/i-logfin.i}

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize D-Dialog 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* {utp/ut9000.i "ESSP0174A" "2.04.00.000"} */ 

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  /* Busca Nome da Empresa */
  FIND FIRST param-global NO-LOCK NO-ERROR.
  FIND FIRST empresa WHERE
             empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR. 
  ASSIGN c-empresa = (IF AVAIL empresa THEN empresa.razao-social ELSE "").

  RUN pi-processa.

  {&OPEN-QUERY-br-work}
  APPLY 'entry' TO br-work IN FRAME {&FRAME-NAME}.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-abre-excel D-Dialog 
PROCEDURE pi-abre-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 CREATE "Excel.Application" chExcelApp NO-ERROR.
 IF chExcelApp <> ? THEN /* Cria a Planilha */
    ASSIGN chExcelApp:VISIBLE     = TRUE  /* A Planilha Ficar  Visivel */
           chExcelApp:SheetsInNewWorkbook = 1 /* Nõ PLANILHAS A SEREM CRIADAS */
           chWorkbook             = chExcelApp:Workbooks:ADD() /* Cria Planilha */
           chworksheet            = chExcelapp:sheets:ITEM(1).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-gera-excel D-Dialog 
PROCEDURE pi-gera-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN pi-abre-excel.
  IF chExcelApp = ? THEN DO:
     MESSAGE "O Aplicativo EXCEL nÆo foi encontrado. NÆo ‚ possivel a execu‡Æo do programa."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     ASSIGN arq-saida = "".
     RETURN.
  END.

  RUN pi-monta-planilha.

  /* Posiciona o Foco no Inicio da Planilha */
  chExcelApp:Range("A1"):SELECT.
  chExcelApp:Range("A:A"):EntireColumn:AutoFit.

  /* Posiciona na Planilha 1, Salva e Fecha */
  chWorkSheet = chExcelapp:Sheets:ITEM(1).
  chWorkbook:Worksheets(1):activate.

  /* Salva e Fecha Planilha */
  OS-DELETE VALUE(arq-saida).

  IF chExcelApp:Version BEGINS "8":U THEN 
     chWorkBook:SaveAs(arq-saida,39,,,,,,,TRUE). /* Salva em formato mais Antigo EXCEL97 */ 
  ELSE 
     chWorkBook:SaveAs(arq-saida,,,,,,,).  /* Salva na VersÆo da planilha da ESTA€ÇO */

  chWorkBook:CLOSE().
  chExcelApp:QUIT().
  RELEASE OBJECT chExcelApp. 
  RELEASE OBJECT chworkBook.
  RELEASE OBJECT chworksheet.
    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imp-cabec D-Dialog 
PROCEDURE pi-imp-cabec :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 PUT c-empresa  FORMAT "X(40)"                 AT   1
     "DATA: "                                  AT  58
     STRING(TODAY,"99/99/9999") FORMAT "X(10)" AT  64
     "HORA: "                                  AT  85
     STRING(TIME,"hh:mm:ss")                   AT  91
     "PAG:"                                    AT 125
     i-pag FORMAT ">>"                         AT 130
     SKIP(1).

 PUT "RELA€ÇO DAS ETIQUETAS DA ORDEM DE BENEFICIAMENTO: " +  STRING(p-nr-ob, "999,999") FORMAT "x(100)"  AT 40 SKIP(1).

 PUT "N§ ETIQUETA   DT.EMISSÇO    HORA   LOTE   QUANTIDADE   SITUACAO         LOCAL     REPORTE   REVISOR   CLIENTE        PEDIDO   NOTA FISCAL" AT 1. 
 PUT "-----------   ----------   -----   ----   ----------   ---------------  -------   -------   -------   ------------   ------   -----------" AT 1. 
 ASSIGN i-pag = i-pag + 1.
                                                                            
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imprime D-Dialog 
PROCEDURE pi-imprime :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF VAR h-prog AS HANDLE NO-UNDO.
  DEF VAR i-ct   AS INT.

  RUN utp/ut-utils.p PERSISTENT SET h-prog.


  RUN esapi/saida-imp.p (OUTPUT i-saida,
                         OUTPUT c-saida,
                         OUTPUT i-num-copias,
                         OUTPUT l-ok).

  CASE i-saida:
      WHEN 1 THEN DO.
          OUTPUT TO VALUE(c-saida) CONVERT TARGET "ibm850" PAGED PAGE-SIZE 39.
          /* PUT CONTROL "~033&l1O~033(s16H". */  /* ORIENTA€ÇO PAISAGEM & COMPACTA */ 
          /* PUT CONTROL "~033&l2S~033(s16H". */  /* DUPLEX BORDA CURTA */ 
          PUT CONTROL "~033E~033(s21H".  /* ORIENTA€ÇO RETRATO & COMPACTA */
      END.
      WHEN 2 THEN
          OUTPUT TO VALUE(c-saida) CONVERT SOURCE "ibm850".
      WHEN 3 THEN DO.
          ASSIGN c-saida = SESSION:TEMP-DIRECTORY + "essp0192b.tmp".
          OUTPUT TO VALUE(c-saida) CONVERT SOURCE "ibm850".
      END.
  END CASE.

  DO i-ct = 1 TO i-num-copias.
     ASSIGN i-pag      =  1
            i-lin      = 99.

     FOR EACH b-tt-work NO-LOCK        
           BY b-tt-work.nr-lote
           BY b-tt-work.dt-emissao.

         IF i-lin > 39 THEN DO:
            RUN pi-imp-cabec.
            ASSIGN i-lin = 7.
         END.
          
         IF b-tt-work.num-etiqueta > 0 THEN
            PUT b-tt-work.num-etiqueta   FORMAT ">>>,>>>,>>9"   AT 1.
         PUT b-tt-work.dt-emissao     FORMAT "99/99/9999"    AT  15
             b-tt-work.hr-emissao     FORMAT "x(7)"          AT  28
             b-tt-work.nr-lote        FORMAT "X(4)"          AT  36
             b-tt-work.quantidade     FORMAT ">>>,>>9.99"    AT  43
             b-tt-work.situacao       FORMAT "x(15)"         AT  56
             b-tt-work.localiz        FORMAT "999/999"       AT  73.
         IF  b-tt-work.nr-reporte > 0  THEN 
             PUT b-tt-work.nr-reporte     FORMAT ">>>>>>9" AT  83.
         PUT b-tt-work.resp-revisao   FORMAT "X(7)"          AT  93
             b-tt-work.nome-ab-cli    FORMAT "x(12)"         AT 103
             b-tt-work.nr-pedcli      FORMAT "x(6)"          AT 118
             b-tt-work.nr-nota-fis    FORMAT "x(12)"         AT 127.
         ASSIGN i-lin = i-lin + 1.

     END.
     /*
     IF i-lin > 39 THEN DO:
        RUN pi-imp-cabec.
        ASSIGN i-lin = 7.
     END.
     PUT "------------- ---------- ---------- ---------- ---------- ----------" AT 43.
     PUT "TOTAL GERAL.......:" AT 10.
     PUT fi-planejado   FORMAT ">>,>>>,>>9.99" AT  43 
         fi-estoque-rp  FORMAT ">>>,>>9.99"    AT  57 
         fi-estoque-rd  FORMAT ">>>,>>9.99"    AT  68 
         fi-estoque-ca  FORMAT ">>>,>>9.99"    AT  79 
         fi-retalho-m   FORMAT ">>>,>>9.99"    AT  90 
         fi-retalho-kg  FORMAT ">>>,>>9.99"    AT 101. 
     */
     IF i-saida = 1 THEN DO:
        PAGE.
        PUT "" AT 1.
     END.
  END.

  IF i-saida = 3 THEN DO.
     RUN EXECUTE IN h-prog(INPUT "notepad.exe",
                           INPUT c-saida).
     DELETE PROCEDURE h-prog.
  END.
  OUTPUT CLOSE.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-monta-planilha D-Dialog 
PROCEDURE pi-monta-planilha :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 /* Nomear Aba da Planilha */
 chWorkSheet = chExcelapp:Sheets:ITEM(1).
 chWorkSheet:NAME = "OB " + STRING(p-nr-ob, "999,999").
 chWorkSheet:TAB:ColorIndex = 19.

 /* Ativar a Planilha */
 chWorkSheet = chExcelapp:Sheets:ITEM(1).
 chWorkbook:Worksheets(1):activate.
 chExcelApp:ActiveWindow:Zoom = 100.

 ASSIGN chworksheet:range("A1"):VALUE = "ETIQUETAS DA ORDEM DE BENEFICIAMENTO " + STRING(p-nr-ob, "999,999").

 /* Configura Alinhamento Horizontal do Titulo da Planilha */
 ChWorkSheet:range("A1:O1"):SELECT().
 ChWorksheet:range("A1:O1"):Merge.
 Chworksheet:Range("A1:O1"):HorizontalAlignment =  3.
 /* Colorir Titulo da Planilha */
 chWorkSheet:Range("A1:O1"):FONT:ColorIndex     = 18. /* Avermelhado */
 chWorkSheet:Range("A1:O1"):Interior:ColorIndex = 2. /* Branco */

 /* Configura a Linha do Titulo da Planilha */
 ASSIGN chWorkSheet:Rows("1:1"):RowHeight = 25
        chWorkSheet:Rows("2:2"):RowHeight =  4
        chWorkSheet:Rows("1:1"):FONT:SIZE = 13
        chWorkSheet:Rows("1:1"):FONT:bold = TRUE.

 /* Titulo das Colunas */
 ASSIGN chworksheet:range("A3"):VALUE = "N§ ETIQUETA"
        chworksheet:range("B3"):VALUE = "DT EMISSAO"    
        chworksheet:range("C3"):VALUE = "HORA"  
        chworksheet:range("D3"):VALUE = "LOTE"
        chworksheet:range("E3"):VALUE = "QUANTIDADE" 
        chworksheet:range("F3"):VALUE = "SITUA€ÇO" 
        chworksheet:range("G3"):VALUE = "LOCAL" 
        chworksheet:range("H3"):VALUE = "REPORTE" 
        chworksheet:range("I3"):VALUE = "REVISOR"
        chworksheet:range("J3"):VALUE = "CLIENTE"
        chworksheet:range("K3"):VALUE = "PEDIDO"     
        chworksheet:range("L3"):VALUE = "NOTA FISCAL".

 /* Tamanho das Colunas */
 ASSIGN chWorkSheet:Columns("A"):ColumnWidth = 10
        chWorkSheet:Columns("B"):ColumnWidth = 10
        chWorkSheet:Columns("C"):ColumnWidth =  6
        chWorkSheet:Columns("D"):ColumnWidth =  6
        chWorkSheet:Columns("E"):ColumnWidth = 10
        chWorkSheet:Columns("F"):ColumnWidth = 12
        chWorkSheet:Columns("G"):ColumnWidth =  8
        chWorkSheet:Columns("H"):ColumnWidth =  8
        chWorkSheet:Columns("I"):ColumnWidth = 12
        chWorkSheet:Columns("J"):ColumnWidth = 12
        chWorkSheet:Columns("K"):ColumnWidth =  8
        chWorkSheet:Columns("L"):ColumnWidth = 12. 

 /* Configura as Colunas da Planilha */
 ASSIGN chworksheet:range("A:A"):NumberFormat = "###.###.##0"
        chworksheet:range("B:D"):NumberFormat = "@" 
        chworksheet:range("E:E"):NumberFormat = "###.###.##0,00"
        chworksheet:range("F:G"):NumberFormat = "@"
        chworksheet:range("H:H"):NumberFormat = "###.###.##0"
        chworksheet:range("I:L"):NumberFormat = "@"
        Chworksheet:range("A:A"):HorizontalAlignment = 4
        Chworksheet:range("E:E"):HorizontalAlignment = 4. /* Alinhamento a Direita */

 /* Configura Cabe‡alho das Colunas */
 chWorkSheet:Range("A3:L3"):SELECT().
 ASSIGN chExcelApp:SELECTION:FONT:NAME               = "Consolas"
        chExcelApp:SELECTION:FONT:SIZE               = 09
        chExcelApp:SELECTION:FONT:Bold               = TRUE 
        chExcelApp:SELECTION:Interior:ColorIndex     = 19
        chExcelApp:SELECTION:FONT:ColorIndex         = 11.

 ASSIGN i-Lin    = 4.
 FOR EACH b-tt-work NO-LOCK.
     ASSIGN chworksheet:range("B" + STRING(i-lin)):VALUE = b-tt-work.dt-emissao
            chworksheet:range("C" + STRING(i-lin)):VALUE = b-tt-work.hr-emissao
            chworksheet:range("D" + STRING(i-lin)):VALUE = b-tt-work.nr-lote
            chworksheet:range("E" + STRING(i-lin)):VALUE = b-tt-work.quantidade
            chworksheet:range("F" + STRING(i-lin)):VALUE = b-tt-work.situacao
            chworksheet:range("I" + STRING(i-lin)):VALUE = b-tt-work.resp-revisao
            chworksheet:range("J" + STRING(i-lin)):VALUE = b-tt-work.nome-ab-cli
            chworksheet:range("K" + STRING(i-lin)):VALUE = b-tt-work.nr-pedcli
            chworksheet:range("L" + STRING(i-lin)):VALUE = b-tt-work.nr-nota-fis.
     IF b-tt-work.num-etiqueta <> 0 THEN
        ASSIGN chworksheet:range("A" + STRING(i-lin)):VALUE = b-tt-work.num-etiqueta
               chworksheet:range("G" + STRING(i-lin)):VALUE = SUBSTR(b-tt-work.localiz, 1, 3) + "/" +
                                                              SUBSTR(b-tt-work.localiz, 4, 3)
               chworksheet:range("H" + STRING(i-lin)):VALUE = b-tt-work.nr-reporte.

     /*  Configura Tamanho da Fonte */
     ASSIGN c-lin = STRING(i-lin) + ":" + STRING(i-lin)
            chworksheet:Rows(c-lin):FONT:SIZE = 9.

     ASSIGN i-lin = i-lin + 1.
 
 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-processa D-Dialog 
PROCEDURE pi-processa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 br-work:TITLE IN FRAME {&FRAME-NAME} = "Etiquetas da Ordem Beneficiamento: " +
                                        STRING(p-nr-ob, "999,999").

 ASSIGN FRAME {&FRAME-NAME}:VISIBLE = NO.
 RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
 {utp/ut-liter.i Calculando_Valores *}
 RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

 FIND ordem-benefic WHERE
      ordem-benefic.cod-estabel = p-cod-estabel AND
      ordem-benefic.nr-ob       = p-nr-ob AND 
      ordem-benefic.dt-ob       = p-dt-ob AND
      ordem-benefic.nr-carro    = p-nr-carro NO-LOCK NO-ERROR.
 IF AVAIL ordem-benefic THEN DO:
    FOR EACH ob-etiqueta WHERE
             ob-etiqueta.cod-estabel = ordem-benefic.cod-estabel AND
             ob-etiqueta.nr-ob       = ordem-benefic.nr-ob       AND
             ob-etiqueta.dt-ob       = ordem-benefic.dt-ob       AND
             ob-etiqueta.nr-carro    = ordem-benefic.nr-carro NO-LOCK:

        IF ob-etiqueta.quantidade <= 0  THEN NEXT.

        RUN pi-acompanhar IN h-acomp (INPUT "OB: " + STRING(ordem-benefic.nr-ob) +
                                            " ETIQUETA " + STRING(ob-etiqueta.num-etiqueta)).

        {esinc/i-dsrb.i ob-etiqueta.situacao ob-etiqueta.situacao c-situacao} 

        FIND ped-item-rom WHERE 
             ped-item-rom.num-etiqueta = ob-etiqueta.num-etiqueta AND 
             ped-item-rom.cod-estabel  = ob-etiqueta.cod-estabel AND
             ped-item-rom.nr-ob = ob-etiqueta.nr-ob NO-LOCK NO-ERROR.                    
        
        ASSIGN c-nr-pedcli = "".          
        IF AVAIL ped-item-rom THEN  /*  pecas 20 - 30 - 70 separadas      */                             
           ASSIGN c-nr-pedcli  = ped-item-rom.nr-pedcli
                  c-nome-abrev = ped-item-rom.nome-abrev.                

        FIND ped-item-res WHERE   /*  somatoria das pecas   120     */
             ped-item-res.nome-abrev   = ped-item-rom.nome-abrev AND
             ped-item-res.nr-pedcli    = ped-item-rom.nr-pedcli  AND
             ped-item-res.nr-sequencia = ped-item-rom.nr-sequencia AND 
             ped-item-res.cod-estabel  = ped-item-rom.cod-estabel NO-LOCK NO-ERROR.
         ASSIGN i-nr-nota-fis = 0.
         IF AVAIL ped-item-res THEN                                        
            ASSIGN i-nr-nota-fis = ped-item-res.nr-nota-fis.
        
        FIND tt-work WHERE
             tt-work.cod-estabel  = ob-etiqueta.cod-estabel AND
             tt-work.num-etiqueta = ob-etiqueta.num-etiqueta NO-ERROR.
        IF NOT AVAIL tt-work THEN DO:
           CREATE tt-work.
           ASSIGN tt-work.cod-estabel  = ob-etiqueta.cod-estabel
                  tt-work.num-etiqueta = ob-etiqueta.num-etiqueta
                  tt-work.dt-emissao   = ob-etiqueta.dt-emissao
                  tt-work.hr-emissao   = string(ob-etiqueta.hr-emissao, "99999")
                  tt-work.nr-lote      = ob-etiqueta.nr-lote
                  tt-work.quantidade   = ob-etiqueta.quantidade
                  tt-work.situacao     = c-situacao
                  tt-work.localiz      = ob-etiqueta.localiz
                  tt-work.nr-reporte   = ob-etiqueta.nr-reporte
                  tt-work.resp-revisao = ob-etiqueta.resp-revisao
                  tt-work.nome-ab-cli  = c-nome-abrev
                  tt-work.nr-pedcli    = c-nr-pedcli.
           IF i-nr-nota-fis > 0 THEN
              ASSIGN tt-work.nr-nota-fis  = STRING(i-nr-nota-fis, "9999999").
        END.
    END.
 END. 

 ASSIGN de-total  = 0
        de-tot-m  = 0
        de-tot-kg = 0.
 FOR EACH b-tt-work  NO-LOCK
     BREAK BY b-tt-work.nr-lote
           BY b-tt-work.dt-emissao.

     ASSIGN de-total = de-total + b-tt-work.quantidade.

     /*
     /* Retalhos em M e KG */
     FIND ob-etiqueta WHERE
          ob-etiqueta.cod-estabel  = b-tt-work.cod-estabel AND
          ob-etiqueta.num-etiqueta = b-tt-work.num-etiqueta AND
          ob-etiqueta.nr-ob = p-nr-ob NO-LOCK NO-ERROR.
     FOR EACH mov-est-acbd WHERE
              mov-est-acbd.cod-estabel  = ob-etiqueta.cod-estabel  AND
              mov-est-acbd.data-mov     = ob-etiqueta.dt-emissao   AND
              mov-est-acbd.num-lote     = ob-etiqueta.nr-ob        AND
              mov-est-acbd.nr-carro     = ob-etiqueta.nr-carro     AND
              mov-est-acbd.acondic      = ob-etiqueta.acondic      AND
              mov-est-acbd.nr-sequencia = ob-etiqueta.nr-sequencia AND
              mov-est-acbd.classif = "RT" NO-LOCK.
         ASSIGN de-tot-m = de-tot-m + mov-est-acbd.qtd-defeit.

         FIND item WHERE
              item.it-codigo = mov-est-acbd.it-codigo NO-LOCK NO-ERROR.
         IF AVAIL item THEN
            ASSIGN de-tot-kg = de-tot-kg + (item.peso-liquido * mov-est-acbd.qtd-defeit).


     MESSAGE ob-etiqueta.nr-lote
         VIEW-AS ALERT-BOX INFO BUTTONS OK.

     END. */

     IF LAST-OF(b-tt-work.nr-lote) THEN DO:
        CREATE tt-work.
        ASSIGN tt-work.nr-lote    = b-tt-work.nr-lote +  "ÿ" /* O Branco ‚ ALT 164 */
               tt-work.quantidade = de-total
               tt-work.hr-emissao = "TOTAL".
        IF de-tot-m <> 0 THEN DO:
           CREATE tt-work.
           ASSIGN tt-work.nr-lote    = "ÿ M" /* O Branco ‚ ALT 164 */
                  tt-work.quantidade = de-tot-m
                  tt-work.hr-emissao = "Retalho".
        END.

        IF de-tot-kg <> 0 THEN DO:
           CREATE tt-work.
           ASSIGN tt-work.nr-lote    = "ÿ KG" /* O Branco ‚ ALT 164 */
                  tt-work.quantidade = de-tot-kg
                  tt-work.hr-emissao = "Retalho".
        END.

        ASSIGN de-total  = 0
               de-tot-m  = 0
               de-tot-kg = 0.
     END.
 END.

 RUN pi-finalizar in h-acomp.
 ASSIGN FRAME {&FRAME-NAME}:VISIBLE = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-work"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
  
  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

