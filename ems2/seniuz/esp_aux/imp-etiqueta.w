&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          espec            PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF BUFFER b-etiqueta FOR ob-etiqueta.

/* Local Variable Definitions ---                                       */

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.
DEF NEW GLOBAL SHARED VAR g-tp-dados AS CHAR.

DEF VAR c-cod-estabel AS CHAR NO-UNDO.
DEF VAR i-ct AS INT.
DEF VAR c-desc-item AS CHAR FORMAT "x(36)".
DEF VAR c-composicao LIKE composi.descricao EXTENT 3.
DEF VAR v-defeito AS CHAR EXTENT 3.
DEF VAR i-lote AS INT.
DEF VAR c-comando AS CHAR.
DEF VAR c-code-ant AS CHAR.
DEF VAR i-sit-ant AS INT.
DEF VAR c-desc-situacao AS CHAR FORMAT "x(20)".
DEF VAR i-num-bar AS INT.
DEF VAR c-lote AS CHAR.
DEF VAR i-tp-embal AS INT.
DEF VAR c-corte-comerc AS CHAR.
DEF VAR de-quantidade AS DEC.

DEF VAR h-prog AS HANDLE.

DEF VAR i-tempo-ini    AS INT.
DEF VAR c-prog-balanca AS CHAR.
DEF VAR c-peso-balanca AS CHAR.
DEF VAR c-peso AS CHAR FORMAT "x(10)".
DEF VAR de-peso-lido   AS DEC FORMAT ">>>,>>9.9".

DEF VAR de-peso-calc   AS DEC FORMAT ">>>,>>9.9".
DEF VAR de-media-peso  AS DEC.

{esinc/sz-pcl.i}

DEF STREAM str-rp.
DEF STREAM s-etq.
DEF VAR c-form-epl    AS CHAR FORMAT "x(30)".
DEF VAR c-prog-epl    AS CHAR FORMAT "x(50)".
DEF VAR de-peso-liquido LIKE ITEM.peso-liquido.
DEF VAR i-nr-seq      LIKE ob-etiqueta.nr-sequencia.

DEF VAR i-num-etiqueta LIKE ob-etiqueta.num-etiqueta.
DEF VAR i-nr-cortes AS INT.
DEF VAR c-nuance AS CHAR INIT ''.
DEF VAR c-acondic AS CHAR INIT "ROLO".

DEF VAR i-nr-ob LIKE ob-etiqueta.nr-ob.
DEF VAR c-nr-lote AS CHAR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ob-etiqueta

/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define QUERY-STRING-DEFAULT-FRAME FOR EACH ob-etiqueta SHARE-LOCK
&Scoped-define OPEN-QUERY-DEFAULT-FRAME OPEN QUERY DEFAULT-FRAME FOR EACH ob-etiqueta SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-DEFAULT-FRAME ob-etiqueta
&Scoped-define FIRST-TABLE-IN-QUERY-DEFAULT-FRAME ob-etiqueta


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS bt-imprime bt-sair fi-it-codigo fi-desc-item ~
fi-cod-refer fi-peso-bruto fi-corte-comerc RECT-54 
&Scoped-Define DISPLAYED-OBJECTS fi-it-codigo fi-desc-item fi-cod-refer ~
fi-desc-refer fi-peso-bruto fi-corte-comerc 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-imprime AUTO-GO 
     IMAGE-UP FILE "image/im-f-dv.bmp":U
     LABEL "Btn 2" 
     SIZE 6 BY 1.5 TOOLTIP "Imprime Etiqueta".

DEFINE BUTTON bt-sair AUTO-END-KEY 
     IMAGE-UP FILE "image/im-exi.bmp":U
     LABEL "Btn 2" 
     SIZE 6 BY 1.5 TOOLTIP "Sair".

DEFINE VARIABLE fi-cod-refer AS CHARACTER FORMAT "X(8)" 
     LABEL "Referància" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88.

DEFINE VARIABLE fi-corte-comerc AS CHARACTER FORMAT "!" 
     LABEL "Corte-Comercial" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88.

DEFINE VARIABLE fi-desc-item AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 36.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-refer AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 32.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-it-codigo AS CHARACTER FORMAT "X(16)" 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88.

DEFINE VARIABLE fi-peso-bruto AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "Peso Bruto (KG)" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-54
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 29 BY 1.75
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY DEFAULT-FRAME FOR 
      ob-etiqueta SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     bt-imprime AT ROW 11.67 COL 19.72
     bt-sair AT ROW 11.67 COL 41.43
     fi-it-codigo AT ROW 3.25 COL 17 COLON-ALIGNED WIDGET-ID 24
     fi-desc-item AT ROW 3.25 COL 28.43 COLON-ALIGNED NO-LABEL WIDGET-ID 48
     fi-cod-refer AT ROW 4.25 COL 17 COLON-ALIGNED WIDGET-ID 10
     fi-desc-refer AT ROW 4.25 COL 25.57 COLON-ALIGNED NO-LABEL WIDGET-ID 50
     fi-peso-bruto AT ROW 6.25 COL 17 COLON-ALIGNED WIDGET-ID 20
     fi-corte-comerc AT ROW 8.25 COL 17 COLON-ALIGNED WIDGET-ID 14
     RECT-54 AT ROW 11.5 COL 19 WIDGET-ID 52
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 12.88
         FONT 1
         CANCEL-BUTTON bt-sair.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Emiss∆o de Etiqueta Final - ESSP0134"
         HEIGHT             = 12.88
         WIDTH              = 80
         MAX-HEIGHT         = 17.29
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 17.29
         VIRTUAL-WIDTH      = 80
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR FILL-IN fi-desc-refer IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _TblList          = "espec.ob-etiqueta"
     _Query            is OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Emiss∆o de Etiqueta Final - ESSP0134 */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Emiss∆o de Etiqueta Final - ESSP0134 */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-imprime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-imprime C-Win
ON CHOOSE OF bt-imprime IN FRAME DEFAULT-FRAME /* Btn 2 */
DO:
    ASSIGN INPUT FRAME {&FRAME-NAME} fi-it-codigo
           INPUT FRAME {&FRAME-NAME} fi-desc-item
           INPUT FRAME {&FRAME-NAME} fi-cod-refer
           INPUT FRAME {&FRAME-NAME} fi-peso-bruto
           INPUT FRAME {&FRAME-NAME} fi-corte-comerc.


    FIND estabelec WHERE
         estabelec.cod-estab = '2' NO-LOCK NO-ERROR.

    ASSIGN c-prog-epl = SESSION:TEMP-DIRECTORY + c-seg-usuario + ".epl"
           c-form-epl = "n:\especificos\etiqueta\form-etq.epl".

    FIND FIRST item-ext WHERE
               item-ext.it-codigo = ITEM.it-codigo NO-LOCK NO-ERROR.

    IF AVAIL item-ext THEN DO.
       FIND FIRST composi WHERE
                  composi.cod-composi = item-ext.cod-composi NO-LOCK NO-ERROR.

       ASSIGN c-composicao = "".
       IF AVAIL composi THEN DO.
          ASSIGN c-composicao[1] = composi.descricao
                 c-composicao[2] = composi.descricao1.
       END.
    END.

    FIND qualid-tecido WHERE
         qualid-tecido.codigo = 'D' NO-LOCK NO-ERROR.

    ASSIGN i-nr-ob = 237555
           c-nr-lote = 'SC'.

    ASSIGN i-num-etiqueta = NEXT-VALUE(seq-etq-estoq).

    ASSIGN i-num-bar = INT(STRING(i-num-etiqueta) + fn-calc-digito(INPUT STRING(i-num-etiqueta,"999999999"))).

    ASSIGN de-peso-liquido = ITEM.peso-liquido.

    OS-COPY VALUE(c-form-epl) VALUE(c-prog-epl).

    OUTPUT STREAM s-etq TO VALUE(c-prog-epl) APPEND.

       PUT STREAM s-etq UNFORMATTED 
           "A110,160,0,1,3,4,N," '"' TRIM(fi-it-codigo) '"' SKIP
           "A300,160,0,1,2,4,N," '"' TRIM(SUBSTR(fi-cod-refer,1,2)) TRIM(SUBSTR(fi-cod-refer,3,4)) '"' SKIP
           "A450,160,0,1,2,4,N," '"' TRIM(SUBSTR(fi-cod-refer,7,1)) '"' SKIP
           "A490,160,0,1,2,4,N," '"' STRING((de-peso-liquido * 1000),"999.99") '"' SKIP
           IF AVAIL ordem-benefic AND ordem-benefic.cor-etiqueta = 99 THEN "LE106,150,505,58" ELSE "" SKIP 
           "A210,236,0,1,1,2,N," '"' TRIM(fi-desc-item) '"' SKIP
           IF qualid-tecido.impr-tarja THEN "LE208,217,490,40" ELSE "" SKIP
           "A220,310,0,2,1,1,N," '"' STRING(i-nr-ob,">>>>>9") '"' SKIP
           "A340,310,0,2,1,1,N," '"' IF AVAIL item-ext THEN STRING(item-ext.largura,"9.99") ELSE "" '"' SKIP
           "A450,270,0,2,1,1,N," '"' IF SUBSTR(c-nr-lote,1,2) = "SC" THEN "(kg)" ELSE "(M)" '"' SKIP
           "A415,298,0,3,1,2,N," '"' STRING(fi-peso-bruto,">>9.99") '"' SKIP
           "A555,310,0,2,1,1,N," '"' STRING(i-nr-cortes,">9") '"' SKIP
           "A640,300,0,1,3,3,N," '"' TRIM(c-nuance) '"' SKIP
           IF qualid-tecido.class-qualid = 2 THEN "LE620,130,80,75" ELSE "" SKIP /* Tamanho */
           IF qualid-tecido.class-qualid = 2 THEN "LE610,270,90,65" ELSE "" SKIP /* Nuance */
           "A215,367,0,1,1,3,N," '"' TRIM(c-composicao[1]) '"' SKIP
           "A215,400,0,1,1,3,N," '"' TRIM(c-composicao[2]) '"' SKIP.

      PUT STREAM s-etq UNFORMATTED 
          "A620,170,0,2,1,1,N," '"' TRIM(c-acondic) '"' SKIP.

       PUT STREAM s-etq UNFORMATTED 
           "A235,450,0,4,3,4,N," '"' STRING(i-num-etiqueta,"999999999") '"' SKIP
           "B305,550,0,1,3,7,60,N," '"' STRING(i-num-bar,"9999999999") '"' SKIP.

       PUT STREAM s-etq UNFORMATTED 
           "A630,550,0,2,1,1,N," '"' TRIM(v-defeito[1]) '"' SKIP
           "A630,570,0,2,1,1,N," '"' TRIM(v-defeito[2]) '"' SKIP
           "A630,590,0,2,1,1,N," '"' TRIM(v-defeito[3]) '"' SKIP.

       IF AVAIL item-ext THEN DO. /*Escolhendo a imagem para jogar na etiqueta*/
          CASE item-ext.cod-rlgp:
               WHEN 1 THEN
                  PUT STREAM s-etq UNFORMATTED
                      "GG130,280," '"imag0204"' SKIP
                      "GG130,330," '"imag0103"' SKIP 
                      "GG130,390," '"imag0302"' SKIP 
                      "GG130,450," '"imag0402"' SKIP 
                      "GG130,510," '"imag0606"' SKIP. 
               WHEN 2 THEN
                  PUT STREAM s-etq UNFORMATTED
                      "GG130,280," '"imag0206"' SKIP
                      "GG130,330," '"imag0103"' SKIP 
                      "GG130,390," '"imag0303"' SKIP 
                      "GG130,450," '"imag0402"' SKIP 
                      "GG130,510," '"imag0606"' SKIP. 
               WHEN 3 THEN 
                  PUT STREAM s-etq UNFORMATTED
                      "GG130,280," '"imag0201"' SKIP
                      "GG130,330," '"imag0103"' SKIP 
                      "GG130,390," '"imag0302"' SKIP 
                      "GG130,450," '"imag0404"' SKIP 
                      "GG130,510," '"imag0606"' SKIP. 
              WHEN 4 THEN 
                 PUT STREAM s-etq UNFORMATTED
                     "GG130,280," '"imag0204"' SKIP
                     "GG130,330," '"imag0103"' SKIP 
                     "GG130,390," '"imag0302"' SKIP 
                     "GG130,450," '"imag0402"' SKIP 
                     "GG130,510," '"imag0603"' SKIP. 
          END CASE.
       END.

       PUT STREAM s-etq UNFORMATTED
           "P1" SKIP.
    OUTPUT STREAM s-etq CLOSE.
    ASSIGN c-comando = "copy /Y /b " + c-prog-epl + " lpt1". 
    OS-COMMAND SILENT VALUE(c-comando). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-sair C-Win
ON CHOOSE OF bt-sair IN FRAME DEFAULT-FRAME /* Btn 2 */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-corte-comerc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-corte-comerc C-Win
ON VALUE-CHANGED OF fi-corte-comerc IN FRAME DEFAULT-FRAME /* Corte-Comercial */
DO:
   IF INPUT FRAME {&FRAME-NAME} fi-corte-comerc <> ob-etiqueta.corte-comerc THEN DO:
      FIND corte-comerc WHERE corte-comerc.codigo = INPUT FRAME {&FRAME-NAME} fi-corte-comerc
                        NO-ERROR.
      IF NOT AVAIL corte-comerc THEN
         MESSAGE "Corte comercial n∆o encontrado."
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-it-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo C-Win
ON LEAVE OF fi-it-codigo IN FRAME DEFAULT-FRAME /* Item */
DO:
   FIND ITEM WHERE
        ITEM.it-codigo = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.

   IF NOT AVAIL ITEM THEN DO.
      MESSAGE 'item n∆o cadastradao..'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'ENTRY' TO SELF.
      RETURN NO-APPLY.
   END.
   ASSIGN fi-desc-item:SCREEN-VALUE = ITEM.desc-item.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

ON 'return':U ANYWHERE DO.
   APPLY 'TAB' TO SELF.
   RETURN NO-APPLY.
END.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.

  FIND FIRST ob-param NO-LOCK NO-ERROR.
  FIND FIRST mp-param NO-LOCK NO-ERROR.

  ASSIGN fi-corte-comerc:SCREEN-VALUE = 'C'.

  APPLY 'ENTRY' TO fi-it-codigo.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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

  {&OPEN-QUERY-DEFAULT-FRAME}
  GET FIRST DEFAULT-FRAME.
  DISPLAY fi-it-codigo fi-desc-item fi-cod-refer fi-desc-refer fi-peso-bruto 
          fi-corte-comerc 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE bt-imprime bt-sair fi-it-codigo fi-desc-item fi-cod-refer 
         fi-peso-bruto fi-corte-comerc RECT-54 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

