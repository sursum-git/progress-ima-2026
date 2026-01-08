&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mgcad            PROGRESS
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
{include/i-prgvrs.i ESSP0174F 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE TEMP-TABLE tt-lojas  NO-UNDO 
       FIELD loja    AS CHAR FORMAT "x(12)"
       FIELD qtd-vda AS DEC
       FIELD vlr-vda AS DEC
       FIELD qtd-can AS DEC
       FIELD vlr-can AS DEC
       FIELD qtd-fat AS DEC
       FIELD vlr-fat AS DEC.

DEFINE INPUT PARAMETER TABLE FOR tt-lojas.
DEFINE INPUT PARAMETER p-periodo        AS CHAR.
DEFINE INPUT PARAMETER p-fi-qtd-vda-ind AS DEC.
DEFINE INPUT PARAMETER p-fi-vlr-vda-ind AS DEC.
DEFINE INPUT PARAMETER p-fi-qtd-can-ind AS DEC.
DEFINE INPUT PARAMETER p-fi-vlr-can-ind AS DEC.
DEFINE INPUT PARAMETER p-fi-qtd-fat-ind AS DEC.
DEFINE INPUT PARAMETER p-fi-vlr-fat-ind AS DEC.
DEFINE INPUT PARAMETER p-fi-qtd-vda-out AS DEC.
DEFINE INPUT PARAMETER p-fi-vlr-vda-out AS DEC.
DEFINE INPUT PARAMETER p-fi-qtd-can-out AS DEC.
DEFINE INPUT PARAMETER p-fi-vlr-can-out AS DEC.
DEFINE INPUT PARAMETER p-fi-qtd-fat-out AS DEC.
DEFINE INPUT PARAMETER p-fi-vlr-fat-out AS DEC.
DEFINE INPUT PARAMETER p-tp-relat       AS INT.


/* Local Variable Definitions ---                                       */
DEF NEW GLOBAL SHARED VAR gr-nota-fiscal AS ROWID NO-UNDO.

DEF VAR i-lin AS INT.
DEF VAR i-pag AS INT.
DEF VAR c-empresa AS CHAR.

 /* Variaveis da Rotina de Impress∆o */
DEFINE VAR l-ok                AS LOG.
DEFINE VAR i-saida             AS INT.
DEFINE VAR c-saida             AS CHAR.
DEFINE VAR i-num-copias        AS INT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME D-Dialog
&Scoped-define BROWSE-NAME br-lojas

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-lojas

/* Definitions for BROWSE br-lojas                                      */
&Scoped-define FIELDS-IN-QUERY-br-lojas tt-lojas.loja tt-lojas.qtd-vda tt-lojas.vlr-vda tt-lojas.qtd-can tt-lojas.vlr-can tt-lojas.qtd-fat tt-lojas.vlr-fat   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-lojas   
&Scoped-define SELF-NAME br-lojas
&Scoped-define OPEN-QUERY-br-lojas RUN pi-total. OPEN QUERY {&SELF-NAME} FOR EACH tt-lojas NO-LOCK                               BY tt-lojas.loja.
&Scoped-define TABLES-IN-QUERY-br-lojas tt-lojas
&Scoped-define FIRST-TABLE-IN-QUERY-br-lojas tt-lojas


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-br-lojas}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-lojas bt-imprime bt-ok bt-cancela ~
bt-ajuda rt-buttom 
&Scoped-Define DISPLAYED-OBJECTS fi-qtd-vda fi-vlr-vda fi-qtd-can ~
fi-vlr-can fi-qtd-fat fi-vlr-fat fi-qtd-vda-ind fi-vlr-vda-ind ~
fi-qtd-can-ind fi-vlr-can-ind fi-qtd-fat-ind fi-vlr-fat-ind fi-qtd-vda-out ~
fi-vlr-vda-out fi-qtd-can-out fi-vlr-can-out fi-qtd-fat-out fi-vlr-fat-out ~
fi-qtd-vda-total fi-vlr-vda-total fi-qtd-can-total fi-vlr-can-total ~
fi-qtd-fat-total fi-vlr-fat-total 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

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

DEFINE BUTTON bt-imprime 
     IMAGE-UP FILE "image/img-pri.bmp":U
     LABEL "Impress∆o" 
     SIZE 13 BY 1.25 TOOLTIP "Imprime os Pedidos Faturados".

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "&OK" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE fi-qtd-can AS DECIMAL FORMAT "       -ZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     FGCOLOR 6  NO-UNDO.

DEFINE VARIABLE fi-qtd-can-ind AS DECIMAL FORMAT "       -ZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     FGCOLOR 6  NO-UNDO.

DEFINE VARIABLE fi-qtd-can-out AS DECIMAL FORMAT "       -ZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     FGCOLOR 6  NO-UNDO.

DEFINE VARIABLE fi-qtd-can-total AS DECIMAL FORMAT "       -ZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     FGCOLOR 2  NO-UNDO.

DEFINE VARIABLE fi-qtd-fat AS DECIMAL FORMAT "    -ZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fi-qtd-fat-ind AS DECIMAL FORMAT "    -ZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fi-qtd-fat-out AS DECIMAL FORMAT "    -ZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fi-qtd-fat-total AS DECIMAL FORMAT "    -ZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1
     FGCOLOR 2  NO-UNDO.

DEFINE VARIABLE fi-qtd-vda AS DECIMAL FORMAT "    -ZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY 1
     FGCOLOR 4  NO-UNDO.

DEFINE VARIABLE fi-qtd-vda-ind AS DECIMAL FORMAT "    -ZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY 1
     FGCOLOR 4  NO-UNDO.

DEFINE VARIABLE fi-qtd-vda-out AS DECIMAL FORMAT "    -ZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY 1
     FGCOLOR 4  NO-UNDO.

DEFINE VARIABLE fi-qtd-vda-total AS DECIMAL FORMAT "    -ZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY 1
     FGCOLOR 2  NO-UNDO.

DEFINE VARIABLE fi-vlr-can AS DECIMAL FORMAT "       -ZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     FGCOLOR 6  NO-UNDO.

DEFINE VARIABLE fi-vlr-can-ind AS DECIMAL FORMAT "       -ZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     FGCOLOR 6  NO-UNDO.

DEFINE VARIABLE fi-vlr-can-out AS DECIMAL FORMAT "       -ZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     FGCOLOR 6  NO-UNDO.

DEFINE VARIABLE fi-vlr-can-total AS DECIMAL FORMAT "       -ZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     FGCOLOR 2  NO-UNDO.

DEFINE VARIABLE fi-vlr-fat AS DECIMAL FORMAT "      -ZZZZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fi-vlr-fat-ind AS DECIMAL FORMAT "      -ZZZZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fi-vlr-fat-out AS DECIMAL FORMAT "       -ZZZZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fi-vlr-fat-total AS DECIMAL FORMAT "      -ZZZZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     FGCOLOR 2  NO-UNDO.

DEFINE VARIABLE fi-vlr-vda AS DECIMAL FORMAT "       -ZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     FGCOLOR 4  NO-UNDO.

DEFINE VARIABLE fi-vlr-vda-ind AS DECIMAL FORMAT "       -ZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     FGCOLOR 4  NO-UNDO.

DEFINE VARIABLE fi-vlr-vda-out AS DECIMAL FORMAT "       -ZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     FGCOLOR 4  NO-UNDO.

DEFINE VARIABLE fi-vlr-vda-total AS DECIMAL FORMAT "       -ZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     FGCOLOR 2  NO-UNDO.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 93 BY 1.42
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-lojas FOR 
      tt-lojas SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-lojas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-lojas D-Dialog _FREEFORM
  QUERY br-lojas NO-LOCK DISPLAY
      tt-lojas.loja     COLUMN-LABEL "Lojas" WIDTH 10 
      tt-lojas.qtd-vda  COLUMN-LABEL "Metros Vendidos"    FORMAT ">,>>>,>>9.99"   WIDTH 12  COLUMN-FGCOLOR 4
      tt-lojas.vlr-vda  COLUMN-LABEL "Valores Vendidos"   FORMAT ">>>,>>>,>>9.99" WIDTH 14  COLUMN-FGCOLOR 4
      tt-lojas.qtd-can  COLUMN-LABEL "Metros Cancelados"  FORMAT ">,>>>,>>9.99"   WIDTH 13  COLUMN-FGCOLOR 6
      tt-lojas.vlr-can  COLUMN-LABEL "Valores Cancelados" FORMAT ">>>,>>>,>>9.99" WIDTH 14  COLUMN-FGCOLOR 6
      tt-lojas.qtd-fat  COLUMN-LABEL "Metros Faturados"   FORMAT ">,>>>,>>9.99"   WIDTH 12  COLUMN-FGCOLOR 9
      tt-lojas.vlr-fat  COLUMN-LABEL "Valores Faturados"  FORMAT ">>>,>>>,>>9.99" WIDTH 15.5  COLUMN-FGCOLOR 9
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 97.43 BY 10.42
         FONT 1
         TITLE "" ROW-HEIGHT-CHARS .83.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     br-lojas AT ROW 1.08 COL 1.57
     fi-qtd-vda AT ROW 11.67 COL 10.14 COLON-ALIGNED NO-LABEL
     fi-vlr-vda AT ROW 11.67 COL 23.14 COLON-ALIGNED NO-LABEL
     fi-qtd-can AT ROW 11.67 COL 37.86 COLON-ALIGNED NO-LABEL
     fi-vlr-can AT ROW 11.67 COL 51.43 COLON-ALIGNED NO-LABEL
     fi-qtd-fat AT ROW 11.67 COL 65.72 COLON-ALIGNED NO-LABEL
     fi-vlr-fat AT ROW 11.67 COL 78.14 COLON-ALIGNED NO-LABEL
     fi-qtd-vda-ind AT ROW 12.67 COL 10.14 COLON-ALIGNED NO-LABEL
     fi-vlr-vda-ind AT ROW 12.67 COL 23.14 COLON-ALIGNED NO-LABEL
     fi-qtd-can-ind AT ROW 12.67 COL 37.86 COLON-ALIGNED NO-LABEL
     fi-vlr-can-ind AT ROW 12.67 COL 51.43 COLON-ALIGNED NO-LABEL
     fi-qtd-fat-ind AT ROW 12.67 COL 65.72 COLON-ALIGNED NO-LABEL
     fi-vlr-fat-ind AT ROW 12.67 COL 78.14 COLON-ALIGNED NO-LABEL
     fi-qtd-vda-out AT ROW 13.67 COL 10.14 COLON-ALIGNED NO-LABEL
     fi-vlr-vda-out AT ROW 13.67 COL 23.14 COLON-ALIGNED NO-LABEL
     fi-qtd-can-out AT ROW 13.67 COL 37.86 COLON-ALIGNED NO-LABEL
     fi-vlr-can-out AT ROW 13.67 COL 51.43 COLON-ALIGNED NO-LABEL
     fi-qtd-fat-out AT ROW 13.67 COL 65.72 COLON-ALIGNED NO-LABEL
     fi-vlr-fat-out AT ROW 13.67 COL 78.14 COLON-ALIGNED NO-LABEL
     fi-qtd-vda-total AT ROW 14.67 COL 10.14 COLON-ALIGNED NO-LABEL
     fi-vlr-vda-total AT ROW 14.67 COL 23.14 COLON-ALIGNED NO-LABEL
     fi-qtd-can-total AT ROW 14.67 COL 37.86 COLON-ALIGNED NO-LABEL
     fi-vlr-can-total AT ROW 14.67 COL 51.43 COLON-ALIGNED NO-LABEL
     fi-qtd-fat-total AT ROW 14.67 COL 65.72 COLON-ALIGNED NO-LABEL
     fi-vlr-fat-total AT ROW 14.67 COL 78.14 COLON-ALIGNED NO-LABEL
     bt-imprime AT ROW 15.83 COL 2
     bt-ok AT ROW 17.46 COL 3
     bt-cancela AT ROW 17.46 COL 14
     bt-ajuda AT ROW 17.46 COL 82.57
     rt-buttom AT ROW 17.21 COL 2
     "Total Lojas:" VIEW-AS TEXT
          SIZE 8 BY .54 AT ROW 11.88 COL 4
     "Total Geral:" VIEW-AS TEXT
          SIZE 8 BY .54 AT ROW 14.88 COL 3.72
     "Outros:" VIEW-AS TEXT
          SIZE 5.14 BY .54 AT ROW 13.88 COL 6.86
     "Industrializacao:" VIEW-AS TEXT
          SIZE 10.86 BY .54 AT ROW 12.88 COL 1.14
     SPACE(87.99) SKIP(5.45)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 1
         TITLE "Detalha Vendas/ Faturamento/Cancelamentos das Lojas - ESSP0174G"
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
   NOT-VISIBLE L-To-R                                                   */
/* BROWSE-TAB br-lojas 1 D-Dialog */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fi-qtd-can IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-can-ind IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-can-out IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-can-total IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-fat IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-fat-ind IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-fat-out IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-fat-total IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-vda IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-vda-ind IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-vda-out IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-vda-total IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-vlr-can IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-vlr-can-ind IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-vlr-can-out IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-vlr-can-total IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-vlr-fat IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-vlr-fat-ind IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-vlr-fat-out IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-vlr-fat-total IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-vlr-vda IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-vlr-vda-ind IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-vlr-vda-out IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-vlr-vda-total IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-lojas
/* Query rebuild information for BROWSE br-lojas
     _START_FREEFORM
RUN pi-total.
OPEN QUERY {&SELF-NAME} FOR EACH tt-lojas NO-LOCK
                              BY tt-lojas.loja.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-lojas */
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
ON WINDOW-CLOSE OF FRAME D-Dialog /* Detalha Vendas/ Faturamento/Cancelamentos das Lojas - ESSP0174G */
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


&Scoped-define SELF-NAME bt-imprime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-imprime D-Dialog
ON CHOOSE OF bt-imprime IN FRAME D-Dialog /* Impress∆o */
DO:
    CLOSE QUERY br-lojas.
    RUN pi-imprime.
    {&OPEN-QUERY-br-lojas}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-lojas
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
  DISPLAY fi-qtd-vda fi-vlr-vda fi-qtd-can fi-vlr-can fi-qtd-fat fi-vlr-fat 
          fi-qtd-vda-ind fi-vlr-vda-ind fi-qtd-can-ind fi-vlr-can-ind 
          fi-qtd-fat-ind fi-vlr-fat-ind fi-qtd-vda-out fi-vlr-vda-out 
          fi-qtd-can-out fi-vlr-can-out fi-qtd-fat-out fi-vlr-fat-out 
          fi-qtd-vda-total fi-vlr-vda-total fi-qtd-can-total fi-vlr-can-total 
          fi-qtd-fat-total fi-vlr-fat-total 
      WITH FRAME D-Dialog.
  ENABLE br-lojas bt-imprime bt-ok bt-cancela bt-ajuda rt-buttom 
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

  /* {utp/ut9000.i "ESSP0174F "2.04.00.000"} */ 

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  /* Busca Nome da Empresa */
  FIND FIRST param-global NO-LOCK NO-ERROR.
  FIND FIRST empresa WHERE
             empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR. 
  ASSIGN c-empresa = (IF AVAIL empresa THEN empresa.razao-social ELSE "").

  IF p-tp-relat = 1 THEN
     ASSIGN  br-lojas:TITLE IN FRAME {&FRAME-NAME} = "DETALHAMENTO DAS VENDAS & CANCELAMENTOS & FATURAMENTO DAS LOJAS NO PERIODO: " + 
                                                       SUBSTR(p-periodo,1,2) + "/" + SUBSTR(p-periodo,3,4).
  ELSE
      ASSIGN  br-lojas:TITLE IN FRAME {&FRAME-NAME} = "DETALHAMENTO DAS VENDAS & CANCELAMENTOS & FATURAMENTO DAS LOJAS NO EXERCICIO: " + 
                                                      SUBSTR(p-periodo,3,4).

  APPLY 'entry' TO br-lojas IN FRAME {&FRAME-NAME}.

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
        "DATA: "                                  AT  52
        STRING(TODAY,"99/99/9999") FORMAT "X(10)" AT  58
        "HORA: "                                  AT  80
        STRING(TIME,"hh:mm:ss")                   AT  86
        "PAG:"                                    AT 115
        i-pag FORMAT ">>>"                        AT 120
        SKIP(1).

    IF p-tp-relat = 1 THEN DO:
       PUT "RELATORIO DAS VENDAS E CANCELAMENTOS E FATURAMENTO DAS LOJAS NO PERIODO:" AT 21.
       PUT  SUBSTR(p-periodo,1,2) + "/" + SUBSTR(p-periodo,3,4) AT 94 SKIP(1).
    END.
    ELSE DO:
       PUT "RELATORIO DAS VENDAS E CANCELAMENTOS E FATURAMENTO DAS LOJAS NO EXERCICIO:" AT 21.
       PUT  SUBSTR(p-periodo,3,4) AT 96 SKIP(1).
    END.

    PUT "DATA        Metros Vendidos  Valores Vendidos  Metros Cancelados  Valores Cancelados  Metros Faturados   Valores Faturados" AT 1.  
    PUT "----------  ---------------  ----------------  -----------------  ------------------  ----------------   -----------------" AT 1.  
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
         OUTPUT TO PRINTER CONVERT TARGET "ibm850" PAGED PAGE-SIZE 62.
         PUT CONTROL "~033E~033(s16H".    
     END.
     WHEN 2 THEN
         OUTPUT TO VALUE(c-saida).
     WHEN 3 THEN DO.
         ASSIGN c-saida = SESSION:TEMP-DIRECTORY + "essp0174g.tmp".
         OUTPUT TO VALUE(c-saida).
     END.
 END CASE.

 DO i-ct = 1 TO i-num-copias.
    ASSIGN i-pag      =  1
           i-lin      = 99.
    FOR EACH tt-lojas WHERE NO-LOCK.
        
        IF i-lin > 62 THEN DO:
           RUN pi-imp-cabec.
           ASSIGN i-lin = 7.
        END.

        PUT tt-lojas.loja     FORMAT "X(12)"          AT   1
            tt-lojas.qtd-vda  FORMAT ">>>,>>>,>>9.99" AT  14
            tt-lojas.vlr-vda  FORMAT ">>>,>>>,>>9.99" AT  32
            tt-lojas.qtd-can  FORMAT ">>>,>>>,>>9.99" AT  51
            tt-lojas.vlr-can  FORMAT ">>>,>>>,>>9.99" AT  71
            tt-lojas.qtd-fat  FORMAT ">>>,>>>,>>9.99" AT  89
            tt-lojas.vlr-fat  FORMAT ">>>,>>>,>>9.99" AT 109.
        ASSIGN i-lin = i-lin + 1.
        ACCUMULATE tt-lojas.qtd-vda (TOTAL).
        ACCUMULATE tt-lojas.vlr-vda (TOTAL).
        ACCUMULATE tt-lojas.qtd-can (TOTAL).
        ACCUMULATE tt-lojas.vlr-can (TOTAL).
        ACCUMULATE tt-lojas.qtd-fat (TOTAL).
        ACCUMULATE tt-lojas.vlr-fat (TOTAL).
    END.
    IF (ACCUM TOTAL tt-lojas.qtd-vda) <> 0 OR
       (ACCUM TOTAL tt-lojas.vlr-vda) <> 0 OR
       (ACCUM TOTAL tt-lojas.qtd-can) <> 0 OR 
       (ACCUM TOTAL tt-lojas.vlr-can) <> 0 OR 
       (ACCUM TOTAL tt-lojas.qtd-fat) <> 0 OR 
       (ACCUM TOTAL tt-lojas.vlr-fat) <> 0 THEN DO:
       IF i-lin > 59 THEN DO:
          RUN pi-imp-cabec.
          ASSIGN i-lin = 7.
       END.
       PUT "---------------  ----------------  -----------------  ------------------  ----------------   -----------------" AT 13 SKIP.
       PUT "TOTAIS...." AT 1.
       PUT ACCUM TOTAL tt-lojas.qtd-vda FORMAT ">>>>,>>>,>>9.99" AT  13.
       PUT ACCUM TOTAL tt-lojas.vlr-vda FORMAT ">>>>,>>>,>>9.99" AT  31.
       PUT ACCUM TOTAL tt-lojas.qtd-can FORMAT ">>>>,>>>,>>9.99" AT  50.
       PUT ACCUM TOTAL tt-lojas.vlr-can FORMAT ">>>>,>>>,>>9.99" AT  70.
       PUT ACCUM TOTAL tt-lojas.qtd-fat FORMAT ">>>>,>>>,>>9.99" AT  88.
       PUT ACCUM TOTAL tt-lojas.vlr-fat FORMAT ">>>>,>>>,>>9.99" AT 108 SKIP(2).
    END.

    IF p-fi-qtd-vda-ind <> 0 OR
       p-fi-vlr-vda-ind <> 0 OR
       p-fi-qtd-can-ind <> 0 OR 
       p-fi-vlr-can-ind <> 0 OR 
       p-fi-qtd-fat-ind <> 0 OR 
       p-fi-vlr-fat-ind <> 0 THEN DO:
       IF i-lin > 59 THEN DO:
          RUN pi-imp-cabec.
          ASSIGN i-lin = 7.
       END.
       PUT "INDUSTRIA." AT 1.
       PUT p-fi-qtd-vda-ind  FORMAT ">>>>,>>>,>>9.99" AT  13.
       PUT p-fi-vlr-vda-ind  FORMAT ">>>>,>>>,>>9.99" AT  31.
       PUT p-fi-qtd-can-ind  FORMAT ">>>>,>>>,>>9.99" AT  50.
       PUT p-fi-vlr-can-ind  FORMAT ">>>>,>>>,>>9.99" AT  70.
       PUT p-fi-qtd-fat-ind  FORMAT ">>>>,>>>,>>9.99" AT  88.
       PUT p-fi-vlr-fat-ind  FORMAT ">>>>,>>>,>>9.99" AT 108 SKIP.
    END.

    IF p-fi-qtd-vda-out <> 0 OR
       p-fi-vlr-vda-out <> 0 OR
       p-fi-qtd-can-out <> 0 OR 
       p-fi-vlr-can-out <> 0 OR 
       p-fi-qtd-fat-out <> 0 OR 
       p-fi-vlr-fat-out <> 0 THEN DO:
       IF i-lin > 59 THEN DO:
          RUN pi-imp-cabec.
          ASSIGN i-lin = 7.
       END.
       PUT "OUTROS...." AT 1.
       PUT p-fi-qtd-vda-out  FORMAT ">>>>,>>>,>>9.99" AT  13.
       PUT p-fi-vlr-vda-out  FORMAT ">>>>,>>>,>>9.99" AT  31.
       PUT p-fi-qtd-can-out  FORMAT ">>>>,>>>,>>9.99" AT  50.
       PUT p-fi-vlr-can-out  FORMAT ">>>>,>>>,>>9.99" AT  70.
       PUT p-fi-qtd-fat-out  FORMAT ">>>>,>>>,>>9.99" AT  88.
       PUT p-fi-vlr-fat-out  FORMAT ">>>>,>>>,>>9.99" AT 108 SKIP.
    END.

    IF (ACCUM TOTAL tt-lojas.qtd-vda) <> 0 OR
       (ACCUM TOTAL tt-lojas.vlr-vda) <> 0 OR
       (ACCUM TOTAL tt-lojas.qtd-can) <> 0 OR 
       (ACCUM TOTAL tt-lojas.vlr-can) <> 0 OR 
       (ACCUM TOTAL tt-lojas.qtd-fat) <> 0 OR 
       (ACCUM TOTAL tt-lojas.vlr-fat) <> 0 THEN DO:
       IF i-lin > 59 THEN DO:
          RUN pi-imp-cabec.
          ASSIGN i-lin = 7.
       END.
       PUT "---------------  ----------------  -----------------  ------------------  ----------------   -----------------" AT 13 SKIP.
       PUT "TOTAL GERAL" AT 1.
       PUT p-fi-qtd-vda-ind + p-fi-qtd-vda-out + ACCUM TOTAL tt-lojas.qtd-vda FORMAT ">>>>,>>>,>>9.99" AT  13.
       PUT p-fi-vlr-vda-ind + p-fi-vlr-vda-out + ACCUM TOTAL tt-lojas.vlr-vda FORMAT ">>>>,>>>,>>9.99" AT  31.
       PUT p-fi-qtd-can-ind + p-fi-qtd-can-out + ACCUM TOTAL tt-lojas.qtd-can FORMAT ">>>>,>>>,>>9.99" AT  50.
       PUT p-fi-vlr-can-ind + p-fi-vlr-can-out + ACCUM TOTAL tt-lojas.vlr-can FORMAT ">>>>,>>>,>>9.99" AT  70.
       PUT p-fi-qtd-fat-ind + p-fi-qtd-fat-out + ACCUM TOTAL tt-lojas.qtd-fat FORMAT ">>>>,>>>,>>9.99" AT  88.
       PUT p-fi-vlr-fat-ind + p-fi-vlr-fat-out + ACCUM TOTAL tt-lojas.vlr-fat FORMAT ">>>>,>>>,>>9.99" AT 108 SKIP(2).
    END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-total D-Dialog 
PROCEDURE pi-total :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    ASSIGN fi-qtd-vda     = 0
           fi-vlr-vda     = 0
           fi-qtd-can     = 0 
           fi-vlr-can     = 0
           fi-qtd-fat     = 0 
           fi-vlr-fat     = 0
           fi-qtd-vda-ind = p-fi-qtd-vda-ind 
           fi-vlr-vda-ind = p-fi-vlr-vda-ind 
           fi-qtd-can-ind = p-fi-qtd-can-ind 
           fi-vlr-can-ind = p-fi-vlr-can-ind 
           fi-qtd-fat-ind = p-fi-qtd-fat-ind 
           fi-vlr-fat-ind = p-fi-vlr-fat-ind
           fi-qtd-vda-out = p-fi-qtd-vda-out 
           fi-vlr-vda-out = p-fi-vlr-vda-out 
           fi-qtd-can-out = p-fi-qtd-can-out 
           fi-vlr-can-out = p-fi-vlr-can-out 
           fi-qtd-fat-out = p-fi-qtd-fat-out
           fi-vlr-fat-out = p-fi-vlr-fat-out.

    FOR EACH tt-lojas NO-LOCK.
         ASSIGN fi-qtd-vda = fi-qtd-vda + tt-lojas.qtd-vda
                fi-vlr-vda = fi-vlr-vda + tt-lojas.vlr-vda
                fi-qtd-can = fi-qtd-can + tt-lojas.qtd-can  
                fi-vlr-can = fi-vlr-can + tt-lojas.vlr-can 
                fi-qtd-fat = fi-qtd-fat + tt-lojas.qtd-fat  
                fi-vlr-fat = fi-vlr-fat + tt-lojas.vlr-fat .
    END.

    ASSIGN fi-qtd-vda-total = fi-qtd-vda + fi-qtd-vda-ind + fi-qtd-vda-out
           fi-vlr-vda-total = fi-vlr-vda + fi-vlr-vda-ind + fi-vlr-vda-out 
           fi-qtd-can-total = fi-qtd-can + fi-qtd-can-ind + fi-qtd-can-out 
           fi-vlr-can-total = fi-vlr-can + fi-vlr-can-ind + fi-vlr-can-out 
           fi-qtd-fat-total = fi-qtd-fat + fi-qtd-fat-ind + fi-qtd-fat-out
           fi-vlr-fat-total = fi-vlr-fat + fi-vlr-fat-ind + fi-vlr-fat-out.




    DISP fi-qtd-vda 
         fi-vlr-vda 
         fi-qtd-can 
         fi-vlr-can 
         fi-qtd-fat 
         fi-vlr-fat 
         fi-qtd-vda-ind 
         fi-vlr-vda-ind 
         fi-qtd-can-ind 
         fi-vlr-can-ind 
         fi-qtd-fat-ind 
         fi-vlr-fat-ind 
         fi-qtd-vda-out 
         fi-vlr-vda-out 
         fi-qtd-can-out 
         fi-vlr-can-out 
         fi-qtd-fat-out 
         fi-vlr-fat-out 
         fi-qtd-vda-total  
         fi-vlr-vda-total
         fi-qtd-can-total
         fi-vlr-can-total
         fi-qtd-fat-total
         fi-vlr-fat-total
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
  {src/adm/template/snd-list.i "tt-lojas"}

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

