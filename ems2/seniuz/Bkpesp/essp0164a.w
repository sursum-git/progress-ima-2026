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
{include/i-prgvrs.i ESSP0164A 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

{utp/utapi011.i} /* Geraá∆o de Graficos */

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEFINE TEMP-TABLE tt-itens NO-UNDO 
       FIELD it-codigo     LIKE it-nota-fisc.it-codigo
       FIELD qtd           LIKE it-nota-fisc.qt-faturada[1]
       FIELD valor         LIKE it-nota-fisc.vl-tot-item  
       INDEX indice1 it-codigo.

DEF BUFFER b-tt-itens FOR tt-itens.

DEFINE INPUT PARAMETER TABLE FOR tt-itens.  
DEFINE INPUT PARAMETER p-dt-entrega-ini AS DATE.
DEFINE INPUT PARAMETER p-dt-entrega-fin AS DATE.

/* Variaveis Usadas Na Geraá∆o da Planilha Excel */
DEF VAR c-empresa LIKE empresa.razao-social.
DEFINE VAR i-canal     AS INTEGER.
DEFINE VAR sys         AS INTEGER.
DEFINE VAR c-lin       AS CHARACTER FORMAT "x(500)".
DEFINE VAR aux-command AS CHAR FORMAT "x(100)".


/* Local Variable Definitions ---                                       */
DEF VAR c-desc-item           LIKE ITEM.desc-item.
DEF VAR c-desc-tipo           AS CHAR FORMAT "x(10)".
DEF VAR arq-saida             AS CHAR.
DEF VAR de-tot-qtd-indigo     AS DEC.
DEF VAR de-tot-vlr-indigo     AS DEC.
DEF VAR de-tot-qtd-nao-indigo AS DEC.
DEF VAR de-tot-vlr-nao-indigo AS DEC.
DEF VAR l-ok                  AS LOG.
DEF VAR i-lin                 AS INT.
DEF VAR i-pag                 AS INT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog
&Scoped-define BROWSE-NAME br-itens

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-itens

/* Definitions for BROWSE br-itens                                      */
&Scoped-define FIELDS-IN-QUERY-br-itens tt-itens.it-codigo fn-desc-item() @ c-desc-item fn-tipo-artigo() @ c-desc-tipo tt-itens.qtd tt-itens.valor   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-itens   
&Scoped-define SELF-NAME br-itens
&Scoped-define OPEN-QUERY-br-itens RUN pi-total. OPEN QUERY {&SELF-NAME} FOR EACH tt-itens NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-itens tt-itens
&Scoped-define FIRST-TABLE-IN-QUERY-br-itens tt-itens


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-br-itens}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-buttom br-itens bt-imprime bt-excel ~
bt-grafico-qtd bt-grafico-vlr bt-ok bt-cancela bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS fi-tot-qtd-geral fi-tot-vlr-geral ~
fi-tot-qtd-ind fi-tot-vlr-ind fi-tot-qtd-n-ind fi-tot-vlr-n-ind 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-desc-item D-Dialog 
FUNCTION fn-desc-item RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-tipo-artigo D-Dialog 
FUNCTION fn-tipo-artigo RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
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

DEFINE BUTTON bt-excel 
     IMAGE-UP FILE "image/img-excel.bmp":U
     LABEL "Impress∆o" 
     SIZE 13 BY 1.25 TOOLTIP "Gera Planilha do Resumo das Devoluá‰es".

DEFINE BUTTON bt-grafico-qtd 
     IMAGE-UP FILE "image/img-graf.bmp":U
     LABEL "Impress∆o" 
     SIZE 13 BY 1.25 TOOLTIP "Gera Grafico das Devoluá‰es em Quantidade".

DEFINE BUTTON bt-grafico-vlr 
     IMAGE-UP FILE "image/img-graf1.bmp":U
     LABEL "Impress∆o" 
     SIZE 13 BY 1.25 TOOLTIP "Gera Grafico das Devoluá‰es em Valores".

DEFINE BUTTON bt-imprime 
     IMAGE-UP FILE "image/img-pri.bmp":U
     LABEL "Impress∆o" 
     SIZE 13 BY 1.25 TOOLTIP "Impress∆o Resumo das Devoluá‰es".

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "&OK" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE fi-tot-qtd-geral AS DECIMAL FORMAT "      ZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88
     FGCOLOR 12  NO-UNDO.

DEFINE VARIABLE fi-tot-qtd-ind AS DECIMAL FORMAT "      ZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fi-tot-qtd-n-ind AS DECIMAL FORMAT "      ZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88
     FGCOLOR 2  NO-UNDO.

DEFINE VARIABLE fi-tot-vlr-geral AS DECIMAL FORMAT "   ZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88
     FGCOLOR 12  NO-UNDO.

DEFINE VARIABLE fi-tot-vlr-ind AS DECIMAL FORMAT "    ZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fi-tot-vlr-n-ind AS DECIMAL FORMAT "   ZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88
     FGCOLOR 2  NO-UNDO.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 80.43 BY 1.42
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-itens FOR 
      tt-itens SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-itens
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-itens D-Dialog _FREEFORM
  QUERY br-itens NO-LOCK DISPLAY
      tt-itens.it-codigo              COLUMN-LABEL "Item"  WIDTH 6
      fn-desc-item()   @ c-desc-item  COLUMN-LABEL "Descriá∆o" WIDTH 31
      fn-tipo-artigo() @ c-desc-tipo  COLUMN-LABEL "Tipo Artigo"  WIDTH  10   
      tt-itens.qtd                    COLUMN-LABEL "Metros Devolvidos"  FORMAT ">,>>>,>>9.99"   WIDTH 13.6
      tt-itens.valor                  COLUMN-LABEL "Valores Devolvidos" FORMAT ">>>,>>>,>>9.99" WIDTH 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 80.29 BY 11.92
         FONT 1
         TITLE "ITENS" ROW-HEIGHT-CHARS .46.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     br-itens AT ROW 1 COL 1.72
     bt-imprime AT ROW 13.08 COL 1.72
     bt-excel AT ROW 13.08 COL 15
     fi-tot-qtd-geral AT ROW 13.08 COL 48.72 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     fi-tot-vlr-geral AT ROW 13.13 COL 63.29 COLON-ALIGNED NO-LABEL
     bt-grafico-qtd AT ROW 14.5 COL 1.72
     bt-grafico-vlr AT ROW 14.5 COL 15
     fi-tot-qtd-ind AT ROW 15.08 COL 48.72 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     fi-tot-vlr-ind AT ROW 15.08 COL 63.29 COLON-ALIGNED NO-LABEL
     fi-tot-qtd-n-ind AT ROW 16.08 COL 48.72 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     fi-tot-vlr-n-ind AT ROW 16.08 COL 63.29 COLON-ALIGNED NO-LABEL
     bt-ok AT ROW 18.38 COL 2.57
     bt-cancela AT ROW 18.38 COL 13.57
     bt-ajuda AT ROW 18.38 COL 70
     "Total N∆o Ind°go:" VIEW-AS TEXT
          SIZE 12 BY .88 AT ROW 15.75 COL 37
     "Total Geral:" VIEW-AS TEXT
          SIZE 8 BY .88 AT ROW 13 COL 42
     "Total Indigo:" VIEW-AS TEXT
          SIZE 9 BY .88 AT ROW 14.75 COL 41
     rt-buttom AT ROW 18.13 COL 1.57
     SPACE(0.28) SKIP(0.11)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 1
         TITLE "Resumo das Devoluá‰es por Itens - ESSP0164A.W"
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
/* BROWSE-TAB br-itens rt-buttom D-Dialog */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fi-tot-qtd-geral IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-qtd-ind IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-qtd-n-ind IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-vlr-geral IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-vlr-ind IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-vlr-n-ind IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-itens
/* Query rebuild information for BROWSE br-itens
     _START_FREEFORM
RUN pi-total.
OPEN QUERY {&SELF-NAME} FOR EACH tt-itens NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-itens */
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
ON WINDOW-CLOSE OF FRAME D-Dialog /* Resumo das Devoluá‰es por Itens - ESSP0164A.W */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
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


&Scoped-define SELF-NAME bt-excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-excel D-Dialog
ON CHOOSE OF bt-excel IN FRAME D-Dialog /* Impress∆o */
DO:
   RUN esdlg/d02essp0164.w (INPUT 4, OUTPUT arq-saida).
   IF arq-saida <> "" THEN DO:
      RUN pi-gera-excel (INPUT arq-saida).
      MESSAGE "O arquivo foi gerado em " TRIM(arq-saida) + "." SKIP 
              "Para acess†-lo,  abra-o atravÇs do Excel."
          VIEW-AS ALERT-BOX INFO BUTTONS OK. 
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-grafico-qtd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-grafico-qtd D-Dialog
ON CHOOSE OF bt-grafico-qtd IN FRAME D-Dialog /* Impress∆o */
DO:
   ASSIGN FRAME {&FRAME-NAME}:VISIBLE = NO.
   RUN pi-grafico-qtd.
   ASSIGN FRAME {&FRAME-NAME}:VISIBLE = YES.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-grafico-vlr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-grafico-vlr D-Dialog
ON CHOOSE OF bt-grafico-vlr IN FRAME D-Dialog /* Impress∆o */
DO:
   ASSIGN FRAME {&FRAME-NAME}:VISIBLE = NO.
   RUN pi-grafico-vlr.
   ASSIGN FRAME {&FRAME-NAME}:VISIBLE = YES.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-imprime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-imprime D-Dialog
ON CHOOSE OF bt-imprime IN FRAME D-Dialog /* Impress∆o */
DO:
    CLOSE QUERY br-itens.
    RUN pi-imprime.
    {&OPEN-QUERY-br-itens}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-itens
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

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
  DISPLAY fi-tot-qtd-geral fi-tot-vlr-geral fi-tot-qtd-ind fi-tot-vlr-ind 
          fi-tot-qtd-n-ind fi-tot-vlr-n-ind 
      WITH FRAME D-Dialog.
  ENABLE rt-buttom br-itens bt-imprime bt-excel bt-grafico-qtd bt-grafico-vlr 
         bt-ok bt-cancela bt-ajuda 
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

  APPLY 'entry' TO br-itens IN FRAME {&FRAME-NAME}.

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
 DEF INPUT PARAMETER p-arquivo AS CHAR.

 DEF VAR h-prog AS HANDLE NO-UNDO.
 RUN utp/ut-utils.p PERSISTENT SET h-prog.

 RUN EXECUTE IN h-prog(INPUT "EXCEL.EXE", INPUT p-arquivo).

 DELETE PROCEDURE h-prog.
 PAUSE 5 NO-MESSAGE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cor D-Dialog 
PROCEDURE pi-cor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*
 DEF INPUT PARAMETER p-cor AS INT.

 tt-vda.tipo:FGCOLOR IN BROWSE         {&browse-name} = p-cor.
 tt-vda.nr-pedcli:FGCOLOR IN BROWSE    {&browse-name} = p-cor.
 tt-vda.nr-pedrep:FGCOLOR IN BROWSE    {&browse-name} = p-cor.
 tt-vda.nome-abrev:FGCOLOR IN BROWSE   {&browse-name} = p-cor.
 tt-vda.no-ab-reppri:FGCOLOR IN BROWSE {&browse-name} = p-cor.
 tt-vda.dt-implant:FGCOLOR IN BROWSE   {&browse-name} = p-cor.
 tt-vda.dt-entrega:FGCOLOR IN BROWSE   {&browse-name} = p-cor.
 tt-vda.sit:FGCOLOR IN BROWSE          {&browse-name} = p-cor.
 tt-vda.qtd:FGCOLOR IN BROWSE          {&browse-name} = p-cor.
 tt-vda.valor:FGCOLOR IN BROWSE        {&browse-name} = p-cor. 
*/
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
 DEF INPUT PARAMETER p-arq-saida AS CHAR FORMAT "x(45)".
    
 DEFINE FRAME frm_excel WITH SIZE 2 BY 2 ROW 1 COLUMN 1.
 ENABLE ALL WITH FRAME frm_excel.
    
 RUN pi-abre-excel (INPUT "").
 PAUSE 3 NO-MESSAGE.

 DDE INITIATE sys FRAME FRAME frm_excel:HANDLE APPLICATION "EXCEL" TOPIC "System".
 DDE INITIATE i-canal FRAME FRAME frm_excel:HANDLE APPLICATION "EXCEL" TOPIC "Pasta1".

 RUN pi-monta-planilha.

 OS-DELETE VALUE(p-arq-saida).
 DDE EXECUTE   sys COMMAND '[save.as("' + p-arq-saida + '")]'.
    
 DDE EXECUTE   sys COMMAND "[close(0)]". 
 DDE EXECUTE   sys COMMAND "[quit()]". 
   
 DDE TERMINATE sys.
    
 HIDE FRAME frm_excel.
 CLEAR FRAME frm_excel.
 DISABLE ALL WITH FRAME frm_excel.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-grafico-qtd D-Dialog 
PROCEDURE pi-grafico-qtd :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR i-point   AS INT INITIAL 1.
 DEF VAR i-numsets AS INT.

 EMPTY TEMP-TABLE tt-atributos.
 EMPTY TEMP-TABLE tt-sets.
 EMPTY TEMP-TABLE tt-points-2.
 EMPTY TEMP-TABLE tt-dados.
 EMPTY TEMP-TABLE tt-erros.

 /* Configuraá∆o Geral do Grafico */
 CREATE tt-atributos.
 ASSIGN tt-atributos.cod-versao-integracao = 3
        tt-atributos.graphtype             = 4
        tt-atributos.graphtitle            = ' ITENS DAS DEVOLUÄÂES NO PERIODO: ' + STRING(p-dt-entrega-ini, "99/99/9999") + ' A ' + STRING(p-dt-entrega-fin, "99/99/9999") 
        tt-atributos.lefttitle             = 'Quantidade das DEVOLUÄÂES.'
        tt-atributos.lefttitlestyle        = 2
        tt-atributos.bottomtitle           = 'ITENS DEVOLVIDOS'
        tt-atributos.numgraph              = 1.

 /* Configuraá∆o das Variantes do Grafico */
 ASSIGN i-numsets = 1.
 CREATE tt-sets.
 ASSIGN tt-sets.NumSet     = i-numsets 
        tt-sets.NumGraph   = 1
        tt-sets.ColorSet   = 1
        tt-sets.legendText = 'DEVOLUÄÂES'.

 FOR EACH b-tt-itens NO-LOCK 
    BREAK BY b-tt-itens.it-codigo.

    /* Valores do EIXO X (ITENS) */
    CREATE tt-points-2.
    ASSIGN tt-points-2.NumPoint  = i-point
           tt-points-2.NumGraph  = 1
           tt-points-2.labeltext = b-tt-itens.it-codigo.

    /* Valores do EIXO Y (QUANTIDADE) */
    ASSIGN i-numsets = 1.
    CREATE tt-dados.
    ASSIGN tt-dados.NumPoint   = i-point
           tt-dados.NumSet     = i-numsets
           tt-dados.NumGraph   = 1
           tt-dados.graphdata  = b-tt-itens.qtd.

    ASSIGN i-numsets = i-numsets + 1 
           i-point   = i-point   + 1.
 END.

 DEF VAR h-utapi011 AS HANDLE NO-UNDO.

 RUN utp/utapi011.p PERSISTENT SET h-utapi011.

 RUN pi-execute IN h-utapi011 (INPUT  TABLE tt-atributos,
                               INPUT  TABLE tt-points-2,
                               INPUT  TABLE tt-sets,
                               INPUT  TABLE tt-dados,
                               INPUT  TABLE tt-ylabels,
                               OUTPUT TABLE tt-erros).

 IF RETURN-VALUE = "NOK" THEN DO: 
    FOR EACH tt-erros: 
        DISP cod-erro desc-erro FORMAT "x(100)" WITH 1 COL WIDTH 500. 
    END.
 END.                  

 DELETE PROCEDURE h-utapi011.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-grafico-vlr D-Dialog 
PROCEDURE pi-grafico-vlr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 DEF VAR i-point   AS INT INITIAL 1.
 DEF VAR i-numsets AS INT.

 EMPTY TEMP-TABLE tt-atributos.
 EMPTY TEMP-TABLE tt-sets.
 EMPTY TEMP-TABLE tt-points-2.
 EMPTY TEMP-TABLE tt-dados.
 EMPTY TEMP-TABLE tt-erros.

 /* Configuraá∆o Geral do Grafico */
 CREATE tt-atributos.
 ASSIGN tt-atributos.cod-versao-integracao = 3
        tt-atributos.graphtype             = 4
        tt-atributos.graphtitle            = ' ITENS DAS DEVOLUÄÂES NO PERIODO: ' + STRING(p-dt-entrega-ini, "99/99/9999") + ' A ' + STRING(p-dt-entrega-fin, "99/99/9999") 
        tt-atributos.lefttitle             = 'Valores das DEVOLUÄÂES.'
        tt-atributos.lefttitlestyle        = 2
        tt-atributos.bottomtitle           = 'ITENS DEVOLVIDOS'
        tt-atributos.numgraph              = 1.

 /* Configuraá∆o das Variantes do Grafico */
 ASSIGN i-numsets = 1.
 CREATE tt-sets.
 ASSIGN tt-sets.NumSet     = i-numsets 
        tt-sets.NumGraph   = 1
        tt-sets.ColorSet   = 1
        tt-sets.legendText = 'DEVOLUÄÂES'.

 FOR EACH b-tt-itens NO-LOCK 
    BREAK BY b-tt-itens.it-codigo.

    /* Valores do EIXO X (ITENS) */
    CREATE tt-points-2.
    ASSIGN tt-points-2.NumPoint  = i-point
           tt-points-2.NumGraph  = 1
           tt-points-2.labeltext = b-tt-itens.it-codigo.

    /* Valores do EIXO Y (QUANTIDADE) */
    ASSIGN i-numsets = 1.
    CREATE tt-dados.
    ASSIGN tt-dados.NumPoint   = i-point
           tt-dados.NumSet     = i-numsets
           tt-dados.NumGraph   = 1
           tt-dados.graphdata  = b-tt-itens.valor.

    ASSIGN i-numsets = i-numsets + 1 
           i-point   = i-point   + 1.
 END.

 DEF VAR h-utapi011 AS HANDLE NO-UNDO.

 RUN utp/utapi011.p PERSISTENT SET h-utapi011.

 RUN pi-execute IN h-utapi011 (INPUT  TABLE tt-atributos,
                               INPUT  TABLE tt-points-2,
                               INPUT  TABLE tt-sets,
                               INPUT  TABLE tt-dados,
                               INPUT  TABLE tt-ylabels,
                               OUTPUT TABLE tt-erros).

 IF RETURN-VALUE = "NOK" THEN DO: 
    FOR EACH tt-erros: 
        DISP cod-erro desc-erro FORMAT "x(100)" WITH 1 COL WIDTH 500. 
    END.
 END.                  

 DELETE PROCEDURE h-utapi011.

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
        "DATA: "                                  AT  63
        STRING(TODAY,"99/99/9999") FORMAT "X(10)" AT  69
        "HORA: "                                  AT  97
        STRING(TIME,"hh:mm:ss")                   AT 103
        "PAG:"                                    AT 119
        i-pag FORMAT ">>>"                        AT 124
        SKIP(1).

    PUT "RELATORIO RESUMO DAS DEVOLUÄÂES DO PERIODO: " AT 31.
    PUT STRING(p-dt-entrega-ini, "99/99/9999") FORMAT "x(10)" AT 75.
    PUT "A" AT 87.
    PUT STRING(p-dt-entrega-fin, "99/99/9999") FORMAT "x(10)" AT 89 SKIP(1).
 
    PUT "ITEM     DESCRIÄ«O                         TIPO ARTIGO   METROS DEVOLVIDO    VALOR DEVOLVIDO" AT 1.
    PUT "------   --------------------------------  -----------   ----------------    ---------------" AT 1. 
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

 DEF VAR de-n-ind-qtd AS DEC.
 DEF VAR de-n-ind-vlr AS DEC.
 DEF VAR de-ind-qtd   AS DEC.
 DEF VAR de-ind-vlr   AS DEC.

 SYSTEM-DIALOG PRINTER-SETUP UPDATE l-ok.
 IF l-ok THEN DO:
    OUTPUT TO PRINTER CONVERT TARGET "ibm850" PAGED PAGE-SIZE 62.
    PUT CONTROL "~033E~033(s18H".    
    
    ASSIGN i-pag      =  1
           i-lin      = 99.
    FOR EACH tt-itens NO-LOCK.
        IF i-lin > 62 THEN DO:
           RUN pi-imp-cabec.
           ASSIGN i-lin = 7.
        END.
        PUT tt-itens.it-codigo FORMAT "x(6)"           AT  1
            fn-desc-item() FORMAT "x(31)"              AT 10
            fn-tipo-artigo() FORMAT "x(10)"            AT 44
            tt-itens.qtd       FORMAT ">>>,>>>,>>9.99" AT 60
            tt-itens.valor     FORMAT ">>>,>>>,>>9.99" AT 79.
        ASSIGN i-lin = i-lin + 1.
        ACCUMULATE tt-itens.qtd   (TOTAL).
        ACCUMULATE tt-itens.valor (TOTAL).
        CASE fn-tipo-artigo():
            WHEN "Indigo" THEN
                ASSIGN de-ind-qtd = de-ind-qtd +  tt-itens.qtd
                       de-ind-vlr = de-ind-vlr +  tt-itens.valor.
            WHEN "Nao Indigo" THEN
                ASSIGN de-n-ind-qtd = de-n-ind-qtd + tt-itens.qtd
                       de-n-ind-vlr = de-n-ind-vlr + tt-itens.valor.
        END CASE.
   END.
   IF (ACCUM TOTAL tt-itens.qtd)  <> 0 OR
      (ACCUM TOTAL tt-itens.valor) <> 0 THEN DO:
      IF i-lin > 59 THEN DO:
         RUN pi-imp-cabec.
         ASSIGN i-lin = 7.
      END.
      PUT "----------------    ---------------"              AT 58 SKIP.
      PUT "TOTAL GERAL     :"                                AT 40.
      PUT ACCUM TOTAL tt-itens.qtd   FORMAT ">>>,>>>,>>9.99" AT 60.
      PUT ACCUM TOTAL tt-itens.valor FORMAT ">>>,>>>,>>9.99" AT 79 SKIP(1).
      PUT "TOTAL IND÷GO    :"                                AT 40.
      PUT de-ind-qtd    FORMAT ">>>,>>>,>>9.99"              AT 60.
      PUT de-ind-vlr    FORMAT ">>>,>>>,>>9.99"              AT 79.
      PUT "TOTAL N«O IND÷GO:"                                AT 40.
      PUT de-n-ind-qtd  FORMAT ">>>,>>>,>>9.99"              AT 60.
      PUT de-n-ind-vlr  FORMAT ">>>,>>>,>>9.99"              AT 79.
   END.
   OUTPUT CLOSE.
 END.

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

    /* Cabeáalho  da Planilha */
    ASSIGN c-Lin = c-empresa + "             " + " RESUMO POR ITENS DA DEVOLUÄ«O EM " + STRING(p-dt-entrega-ini, "99/99/9999") + " A " + STRING(p-dt-entrega-fin, "99/99/9999"). 
    DDE SEND i-canal SOURCE c-Lin ITEM "L1C1".
    DDE EXECUTE i-canal COMMAND '[select("L1C1:L1C5")]'.
    /* Fonte, Tamanho, Negrito, It†lico, Sublinhado, Atachado, Cor(0=Autom†tico, 1=Preto, 2=Branco 3=Vermelho...) */
    DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",12,True,False,False,False,3)]".

    /* Cabeáalho dos Dados */
    DDE SEND i-canal SOURCE "ITEM"               ITEM "L3C1".
    DDE SEND i-canal SOURCE "DESCRIÄ«O"          ITEM "L3C2".
    DDE SEND i-canal SOURCE "TIPO ARTIGO"        ITEM "L3C3".
    DDE SEND i-canal SOURCE "METROS DEVOLVIDOS"  ITEM "L3C4".
    DDE SEND i-canal SOURCE "VALORES DEVOLVIDOS" ITEM "L3C5".

    /* Formataá∆o das Celulas do Cabeáalho de Dados */
    DDE EXECUTE i-canal COMMAND '[select("L3C1:L3C5")]'.
    /* Fonte, Tamanho, Negrito, It†lico, Sublinhado, Atachado, Cor(0=Autom†tico, 1=Preto, 2=Branco 3=Vermelho...) */
    DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",8,True,False,False,False,5)]".
    
    DDE EXECUTE i-canal COMMAND "[select(~"C1~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(7.00)]".
    
    DDE EXECUTE i-canal COMMAND "[select(~"C2~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(33.00)]".
    
    DDE EXECUTE i-canal COMMAND "[select(~"C3~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(15.00)]". 

    DDE EXECUTE i-canal COMMAND "[select(~"C4~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(14.00)]".
    DDE EXECUTE sys     COMMAND "[format.number(~"###.###.##0,00~")]".
    DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]".

    DDE EXECUTE i-canal COMMAND "[select(~"C5~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(14.00)]".
    DDE EXECUTE sys     COMMAND "[format.number(~"###.###.##0,00~")]".
    DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]".

    
    /* Montagem das Celulas de Dados */
    ASSIGN i-Lin = 4.
    FOR EACH tt-itens NO-LOCK.

        DDE SEND i-canal SOURCE STRING(tt-itens.it-codigo) ITEM "L" + TRIM(STRING(i-Lin)) + "C1". 
        DDE SEND i-canal SOURCE STRING(fn-desc-item())     ITEM "L" + TRIM(STRING(i-Lin)) + "C2".
        DDE SEND i-canal SOURCE STRING(fn-tipo-artigo())   ITEM "L" + TRIM(STRING(i-Lin)) + "C3".
        DDE SEND i-canal SOURCE STRING(tt-itens.qtd)       ITEM "L" + TRIM(STRING(i-Lin)) + "C4".
        DDE SEND i-canal SOURCE STRING(tt-itens.valor)     ITEM "L" + TRIM(STRING(i-Lin)) + "C5".
        ASSIGN aux-command = '[select("L' + TRIM(STRING(i-Lin)) + 'C1:L' + TRIM(STRING(i-Lin)) + 'C5")]'.
        DDE EXECUTE i-canal COMMAND aux-command.
        /* Fonte, Tamanho, Negrito, It†lico, Sublinhado, Atachado, Cor(0=Autom†tico, 1=Preto, 2=Branco 3=Vermelho...) */
        DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",8,False,False,False,False,0)]".
        
        ASSIGN i-Lin = i-Lin + 1.

        ACCUMULATE tt-itens.qtd   (TOTAL).
        ACCUMULATE tt-itens.valor (TOTAL).

        IF fn-tipo-artigo() = "Ind°go" THEN
           ASSIGN de-tot-qtd-indigo = de-tot-qtd-indigo + tt-itens.qtd
                  de-tot-vlr-indigo = de-tot-vlr-indigo + tt-itens.valor.
        ELSE
           ASSIGN de-tot-qtd-nao-indigo = de-tot-qtd-nao-indigo + tt-itens.qtd
                  de-tot-vlr-nao-indigo = de-tot-vlr-nao-indigo + tt-itens.valor.

    END.
    DDE SEND i-canal SOURCE "TOTAL GERAL" ITEM "L" + TRIM(STRING(i-Lin)) + "C2".
    DDE SEND i-canal SOURCE STRING((ACCUM TOTAL tt-itens.qtd))   ITEM "L" + TRIM(STRING(i-Lin)) + "C4".
    DDE SEND i-canal SOURCE STRING((ACCUM TOTAL tt-itens.valor)) ITEM "L" + TRIM(STRING(i-Lin)) + "C5".
    ASSIGN aux-command = '[select("L' + TRIM(STRING(i-Lin)) + 'C1:L' + TRIM(STRING(i-Lin)) + 'C5")]'.
    DDE EXECUTE i-canal COMMAND aux-command.
    /* Fonte, Tamanho, Negrito, It†lico, Sublinhado, Atachado, Cor(0=Autom†tico, 1=Preto, 2=Branco 3=Vermelho...) */
    DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",8,True,False,False,False,0)]".
    ASSIGN i-Lin = i-Lin + 2.
  
    DDE SEND i-canal SOURCE "Total Ind°go"                ITEM "L" + TRIM(STRING(i-Lin)) + "C2".
    DDE SEND i-canal SOURCE STRING(de-tot-qtd-indigo)     ITEM "L" + TRIM(STRING(i-Lin)) + "C4".
    DDE SEND i-canal SOURCE STRING(de-tot-vlr-indigo)     ITEM "L" + TRIM(STRING(i-Lin)) + "C5".
    ASSIGN aux-command = '[select("L' + TRIM(STRING(i-Lin)) + 'C1:L' + TRIM(STRING(i-Lin)) + 'C5")]'.
    DDE EXECUTE i-canal COMMAND aux-command.
    /* Fonte, Tamanho, Negrito, It†lico, Sublinhado, Atachado, Cor(0=Autom†tico, 1=Preto, 2=Branco 3=Vermelho...) */
    DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",8,True,False,False,False,0)]".
    ASSIGN i-Lin = i-Lin + 1.


    DDE SEND i-canal SOURCE "Total N∆o Ind°go"            ITEM "L" + TRIM(STRING(i-Lin)) + "C2".
    DDE SEND i-canal SOURCE STRING(de-tot-qtd-nao-indigo) ITEM "L" + TRIM(STRING(i-Lin)) + "C4".
    DDE SEND i-canal SOURCE STRING(de-tot-vlr-nao-indigo) ITEM "L" + TRIM(STRING(i-Lin)) + "C5".
    ASSIGN aux-command = '[select("L' + TRIM(STRING(i-Lin)) + 'C1:L' + TRIM(STRING(i-Lin)) + 'C5")]'.
    DDE EXECUTE i-canal COMMAND aux-command.
    /* Fonte, Tamanho, Negrito, It†lico, Sublinhado, Atachado, Cor(0=Autom†tico, 1=Preto, 2=Branco 3=Vermelho...) */
    DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",8,True,False,False,False,0)]".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-total D-Dialog 
PROCEDURE pi-total :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    ASSIGN fi-tot-qtd-geral = 0
           fi-tot-vlr-geral = 0
           fi-tot-qtd-ind   = 0 
           fi-tot-vlr-ind   = 0
           fi-tot-qtd-n-ind = 0 
           fi-tot-vlr-n-ind = 0.
    FOR EACH tt-itens NO-LOCK.
        CASE fn-tipo-artigo():
            WHEN "Ind°go" THEN
                ASSIGN fi-tot-qtd-ind = fi-tot-qtd-ind + tt-itens.qtd
                       fi-tot-vlr-ind = fi-tot-vlr-ind + tt-itens.valor.
            WHEN "N∆o Ind°go" THEN
                ASSIGN fi-tot-qtd-n-ind = fi-tot-qtd-n-ind + tt-itens.qtd  
                       fi-tot-vlr-n-ind = fi-tot-vlr-n-ind + tt-itens.valor.
        END CASE.
        ASSIGN fi-tot-qtd-geral = fi-tot-qtd-geral + tt-itens.qtd  
               fi-tot-vlr-geral = fi-tot-vlr-geral + tt-itens.valor.
    END.


    DISP fi-tot-qtd-geral 
         fi-tot-vlr-geral 
         fi-tot-qtd-ind 
         fi-tot-vlr-ind 
         fi-tot-qtd-n-ind 
         fi-tot-vlr-n-ind 
         WITH FRAME {&FRAME-NAME}.

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
  {src/adm/template/snd-list.i "tt-itens"}

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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-desc-item D-Dialog 
FUNCTION fn-desc-item RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  FIND ITEM WHERE
       ITEM.it-codigo = tt-itens.it-codigo NO-LOCK NO-ERROR.

  RETURN item.desc-item.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-tipo-artigo D-Dialog 
FUNCTION fn-tipo-artigo RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    /*
    FIND item-ext WHERE
         item-ext.it-codigo = tt-itens.it-codigo NO-LOCK NO-ERROR.
    IF AVAIL item-ext AND item-ext.indigo = YES THEN
       ASSIGN c-desc-tipo = "Ind°go".
    ELSE
       ASSIGN c-desc-tipo = "N∆o Ind°go".
    */
    RETURN c-desc-tipo.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

