&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-window 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
**
** Altera‡äes: DMJGD001 - Dataminas - JoÆo Gabriel Costa Rocha - 09/01/2004
**             Implanta‡Æo tratamento das informa‡äes por estabelecimento, sendo
**             permitida a consolida‡Æo dos dados de estabelecimentos distintos. 
*******************************************************************************/
{include/i-prgvrs.i IMPD030I 1.00.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
DEFINE SHARED VAR i-cod-emitente LIKE emitente.cod-emitente NO-UNDO.  
DEF INPUT PARAMETER c-query AS CHAR NO-UNDO.
DEF INPUT PARAMETER c-total AS CHAR NO-UNDO.
                                                                                /***DMJGD001.sn***/
DEF NEW GLOBAL SHARED TEMP-TABLE tt-estabelec-dest LIKE estabelec.
                                                                                /***DMJGD001.en***/
DEF VAR hquery   AS HANDLE NO-UNDO.
DEF VAR hBuffer  AS HANDLE NO-UNDO.
DEF VAR hColumn  AS HANDLE NO-UNDO.
DEF VAR i AS INTEGER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE JanelaDetalhe
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME BROWSE-3

/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-3 RECT-1 bt-ok 
&Scoped-Define DISPLAYED-OBJECTS d-total-duplicatas 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-window AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "OK" 
     SIZE 10 BY 1.

DEFINE VARIABLE d-total-duplicatas AS DECIMAL FORMAT ">>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Total (R$)" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 87 BY 1.38
     BGCOLOR 7 .


/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-3 w-window _STRUCTURED
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 87 BY 11 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BROWSE-3 AT ROW 1 COL 1
     d-total-duplicatas AT ROW 12 COL 75 COLON-ALIGNED
     bt-ok AT ROW 13.21 COL 1.86
     RECT-1 AT ROW 13 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 123 BY 13.46.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: JanelaDetalhe
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-window ASSIGN
         HIDDEN             = YES
         TITLE              = "Detalhe Duplicatas"
         HEIGHT             = 13.46
         WIDTH              = 87.43
         MAX-HEIGHT         = 22
         MAX-WIDTH          = 123
         VIRTUAL-HEIGHT     = 22
         VIRTUAL-WIDTH      = 123
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-window 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-window.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-window
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-3 1 F-Main */
/* SETTINGS FOR FILL-IN d-total-duplicatas IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
THEN w-window:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON END-ERROR OF w-window /* Detalhe Duplicatas */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON WINDOW-CLOSE OF w-window /* Detalhe Duplicatas */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-window
ON CHOOSE OF bt-ok IN FRAME F-Main /* OK */
DO:
    APPLY 'close' TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-3
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-window 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-window  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-window  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-window  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
  THEN DELETE WIDGET w-window.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-window  _DEFAULT-ENABLE
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
  DISPLAY d-total-duplicatas 
      WITH FRAME F-Main IN WINDOW w-window.
  ENABLE RECT-1 bt-ok 
      WITH FRAME F-Main IN WINDOW w-window.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW w-window.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-window 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
                                                                                /***DMJGD001.sn***/
  hquery:QUERY-CLOSE().
  DELETE OBJECT hquery.
                                                                                /***DMJGD001.en***/

                     
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .
  {include/i-logfin.i}

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-window 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-window 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    {include/win-size.i}
    
    {utp/ut9000.i "IMPD030I" "1.00.00.000"}
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .
    
    CREATE QUERY  hQuery.
    
    /* Inicializa o Browser */
                                                                                /***DMJGD001.so***
    CREATE BUFFER hBuffer  FOR TABLE "titulo".
    hquery:SET-BUFFERS(hBuffer).
                                                                                 ***DMJGD001.eo***/
                                                                                /***DMJGD001.sn***/
    hquery:SET-BUFFERS(BUFFER titulo:HANDLE, BUFFER tt-estabelec-dest:HANDLE).
                                                                                /***DMJGD001.en***/  

    hquery:QUERY-PREPARE(c-query).
    hquery:QUERY-OPEN.
    hquery:GET-FIRST().
    browse-3:QUERY = hquery.
    
    /* Adiciona Campos */
                                                                                /***DMJGD001.sn***/
    browse-3:ADD-LIKE-COLUMN("titulo.cod-estabel").
                                                                                /***DMJGD001.en***/
    browse-3:ADD-LIKE-COLUMN("titulo.cod-esp").
    browse-3:ADD-LIKE-COLUMN("titulo.serie").
    browse-3:ADD-LIKE-COLUMN("titulo.nr-docto").
    browse-3:ADD-LIKE-COLUMN("titulo.parcela").
    browse-3:ADD-LIKE-COLUMN("titulo.cod-port").
    browse-3:ADD-LIKE-COLUMN("titulo.modalidade").
    browse-3:ADD-LIKE-COLUMN("titulo.mo-codigo").
    browse-3:ADD-LIKE-COLUMN("titulo.vl-saldo").
    browse-3:ADD-LIKE-COLUMN("titulo.vl-original").
    browse-3:ADD-LIKE-COLUMN("titulo.dt-emissao").
    browse-3:ADD-LIKE-COLUMN("titulo.dt-vencimen").

    /* Altera Largura das Colunas */
                                                                                /***DMJGD001.so***
    hcolumn = browse-3:GET-BROWSE-COLUMN(1).
    hcolumn:WIDTH = 9.
    hcolumn = browse-3:GET-BROWSE-COLUMN(2).
    hcolumn:WIDTH = 3.
    hcolumn = browse-3:GET-BROWSE-COLUMN(3).
    hcolumn:WIDTH = 8.
    hcolumn = browse-3:GET-BROWSE-COLUMN(4).
    hcolumn:WIDTH = 20.
    hcolumn = browse-3:GET-BROWSE-COLUMN(5).
    hcolumn:WIDTH = 13.
    hcolumn = browse-3:GET-BROWSE-COLUMN(6).
    hcolumn:WIDTH = 10.
    hcolumn = browse-3:GET-BROWSE-COLUMN(7).
    hcolumn:WIDTH = 2.
    hcolumn = browse-3:GET-BROWSE-COLUMN(8).
    hcolumn:WIDTH = 10.
    hcolumn = browse-3:GET-BROWSE-COLUMN(9).
    hcolumn:WIDTH = 10.
    hcolumn = browse-3:GET-BROWSE-COLUMN(10).
    hcolumn:WIDTH = 10.
    hcolumn = browse-3:GET-BROWSE-COLUMN(11).
    hcolumn:WIDTH = 10.
                                                                                 ***DMJGD001.eo***/
                                                                                /***DMJGD001.sn***/
    hcolumn = browse-3:GET-BROWSE-COLUMN(1).
    hcolumn:WIDTH = 3.
    hcolumn = browse-3:GET-BROWSE-COLUMN(2).
    hcolumn:WIDTH = 9.
    hcolumn = browse-3:GET-BROWSE-COLUMN(3).
    hcolumn:WIDTH = 5.
    hcolumn = browse-3:GET-BROWSE-COLUMN(4).
    hcolumn:WIDTH = 8.
    hcolumn = browse-3:GET-BROWSE-COLUMN(5).
    hcolumn:WIDTH = 20.                   
    hcolumn = browse-3:GET-BROWSE-COLUMN(6).
    hcolumn:WIDTH = 13.
    hcolumn = browse-3:GET-BROWSE-COLUMN(7).
    hcolumn:WIDTH = 10.
    hcolumn = browse-3:GET-BROWSE-COLUMN(8).
    hcolumn:WIDTH = 5.
    hcolumn = browse-3:GET-BROWSE-COLUMN(9).
    hcolumn:WIDTH = 10.
    hcolumn = browse-3:GET-BROWSE-COLUMN(10).
    hcolumn:WIDTH = 10.
    hcolumn = browse-3:GET-BROWSE-COLUMN(11).
    hcolumn:WIDTH = 10.
    hcolumn = browse-3:GET-BROWSE-COLUMN(12).
    hcolumn:WIDTH = 10.
                                                                                /***DMJGD001.en***/
    
    d-total-duplicatas:SCREEN-VALUE IN FRAME {&FRAME-NAME} = c-total.
    
    browse-3:SENSITIVE = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-window  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this JanelaDetalhe, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-window 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

