&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          espec            PROGRESS
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
{include/i-prgvrs.i D99XX999 9.99.99.999}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF TEMP-TABLE tt-work
    FIELD periodo      AS CHAR  FORMAT "x(6)"
    FIELD corte-comerc AS CHAR 
    FIELD nr-lote      LIKE ob-etiqueta.nr-lote
    FIELD qtd          LIKE ob-etiqueta.quantidade
    INDEX indice1 periodo corte-comerc.

DEF TEMP-TABLE tt-pedidos LIKE ped-venda
    FIELD it-codigo       LIKE ped-item.it-codigo
    FIELD cod-refer       LIKE ped-item.cod-refer
    FIELD qt-pedida       LIKE ped-item.qt-pedida
    FIELD visualiza       AS   LOG.

DEF TEMP-TABLE tt-carteira
    FIELD corte-comerc LIKE corte-comerc.codigo
    FIELD nr-lote      LIKE ob-etiqueta.nr-lote
    FIELD it-codigo    LIKE ob-etiqueta.it-codigo
    FIELD cod-refer    LIKE ob-etiqueta.cod-refer
    FIELD periodo      AS CHAR  FORMAT "x(6)"
    FIELD qt-estoque   LIKE ob-etiqueta.quantidade  COLUMN-LABEL "Estoque"
    FIELD qt-carteira  LIKE ped-item.qt-pedida      COLUMN-LABEL "Carteira"
    FIELD row-pedidos  AS  CHAR
    INDEX indice1 periodo
                  corte-comerc
                  nr-lote
                  it-codigo
                  cod-refer.

DEF INPUT-OUTPUT PARAMETER TABLE FOR tt-carteira.
DEF INPUT-OUTPUT PARAMETER TABLE FOR tt-work.
DEF INPUT        PARAMETER p-nr-ob     LIKE ob-etiqueta.nr-ob.
DEF INPUT        PARAMETER p-it-codigo LIKE ITEM.it-codigo.
DEF INPUT        PARAMETER p-desc-item LIKE ITEM.desc-item.
DEF INPUT        PARAMETER p-cod-refer LIKE ob-etiqueta.cod-refer.
DEF INPUT        PARAMETER p-quantidade LIKE ob-etiqueta.quantidade.
DEF OUTPUT       PARAMETER l-ok AS LOG.


DEF VAR i-row AS INT.
DEF VAR de-produzir AS DEC.
DEF VAR i-ct AS INT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog
&Scoped-define BROWSE-NAME br-carteira

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-carteira corte-comerc tt-work ~
ordem-benefic

/* Definitions for BROWSE br-carteira                                   */
&Scoped-define FIELDS-IN-QUERY-br-carteira SUBSTR(tt-carteira.periodo,5,2) + "/" + SUBSTR(tt-carteira.periodo,1,4) corte-comerc.descricao tt-carteira.nr-lote tt-carteira.qt-carteira   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-carteira   
&Scoped-define SELF-NAME br-carteira
&Scoped-define OPEN-QUERY-br-carteira RUN pi-total. OPEN QUERY {&SELF-NAME} FOR EACH tt-carteira where                                  tt-carteira.qt-carteira > 0 NO-LOCK, ~
                                   FIRST corte-comerc WHERE                                   corte-comerc.codigo = tt-carteira.corte-comerc NO-LOCK                                   BY tt-carteira.periodo                                   BY tt-carteira.corte-comerc DESCENDING                                   BY tt-carteira.qt-carteira                                   INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-carteira tt-carteira corte-comerc
&Scoped-define FIRST-TABLE-IN-QUERY-br-carteira tt-carteira
&Scoped-define SECOND-TABLE-IN-QUERY-br-carteira corte-comerc


/* Definitions for BROWSE br-work                                       */
&Scoped-define FIELDS-IN-QUERY-br-work SUBSTR(tt-work.periodo,5,2) + "/" + SUBSTR(tt-work.periodo,1,4) corte-comerc.descricao tt-work.qtd   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-work   
&Scoped-define SELF-NAME br-work
&Scoped-define QUERY-STRING-br-work FOR EACH tt-work NO-LOCK, ~
                                   FIRST corte-comerc WHERE                                   corte-comerc.codigo = tt-work.corte-comerc NO-LOCK                                   INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-work OPEN QUERY {&SELF-NAME} FOR EACH tt-work NO-LOCK, ~
                                   FIRST corte-comerc WHERE                                   corte-comerc.codigo = tt-work.corte-comerc NO-LOCK                                   INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-work tt-work corte-comerc
&Scoped-define FIRST-TABLE-IN-QUERY-br-work tt-work
&Scoped-define SECOND-TABLE-IN-QUERY-br-work corte-comerc


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-br-carteira}~
    ~{&OPEN-QUERY-br-work}
&Scoped-define QUERY-STRING-D-Dialog FOR EACH ordem-benefic SHARE-LOCK
&Scoped-define OPEN-QUERY-D-Dialog OPEN QUERY D-Dialog FOR EACH ordem-benefic SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-D-Dialog ordem-benefic
&Scoped-define FIRST-TABLE-IN-QUERY-D-Dialog ordem-benefic


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 rt-buttom rt-key-parent br-carteira ~
br-work bt-add bt-del bt-det bt-ok bt-cancela bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS fi-nr-ob fi-it-codigo fi-desc-item ~
fi-cod-refer fi-qt-rev fi-tot-carteira fi-tot-prod 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-add 
     IMAGE-UP FILE "adeicon\next-au":U
     IMAGE-INSENSITIVE FILE "adeicon\next-ai":U
     LABEL "" 
     SIZE 7 BY 1.

DEFINE BUTTON bt-ajuda 
     LABEL "&Ajuda" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-cancela AUTO-END-KEY 
     LABEL "&Cancelar" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-del 
     IMAGE-UP FILE "adeicon\prev-au":U
     IMAGE-INSENSITIVE FILE "adeicon\prev-ai":U
     LABEL "" 
     SIZE 7 BY 1.

DEFINE BUTTON bt-det 
     IMAGE-UP FILE "image/im-open.bmp":U
     LABEL "Button 1" 
     SIZE 7 BY 1 TOOLTIP "Detalha Pedidos".

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "&OK" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE fi-cod-refer AS CHARACTER FORMAT "X(256)":U 
     LABEL "Referˆncia" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-item AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 44.43 BY .88 NO-UNDO.

DEFINE VARIABLE fi-it-codigo AS CHARACTER FORMAT "X(8)":U 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nr-ob AS INTEGER FORMAT ">>>,>>9" INITIAL 0 
     LABEL "N£mero da OB" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE VARIABLE fi-qt-rev AS DECIMAL FORMAT "Z,ZZZ,ZZ9.99":U INITIAL 0 
     LABEL "Quantidade" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE VARIABLE fi-tot-carteira AS INTEGER FORMAT "-ZZ,ZZZ,ZZ9.99":U INITIAL 0 
     LABEL "Total Carteira" 
     VIEW-AS FILL-IN 
     SIZE 10.43 BY .88
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fi-tot-prod AS DECIMAL FORMAT "ZZ,ZZZ,ZZ9.99":U INITIAL 0 
     LABEL "Total a Produzir" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 72 BY 10.25.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 72 BY 1.42
     BGCOLOR 7 .

DEFINE RECTANGLE rt-key-parent
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 72 BY 3.33.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-carteira FOR 
      tt-carteira, 
      corte-comerc SCROLLING.

DEFINE QUERY br-work FOR 
      tt-work, 
      corte-comerc SCROLLING.

DEFINE QUERY D-Dialog FOR 
      ordem-benefic SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-carteira
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-carteira D-Dialog _FREEFORM
  QUERY br-carteira NO-LOCK DISPLAY
      SUBSTR(tt-carteira.periodo,5,2) + "/" + 
SUBSTR(tt-carteira.periodo,1,4) COLUMN-LABEL "Periodo"
corte-comerc.descricao FORMAT "x(9)"  COLUMN-LABEL "Corte"
tt-carteira.nr-lote       COLUMN-LABEL "Lote"
tt-carteira.qt-carteira  FORMAT ">>,>>>,>>9.99" COLUMN-LABEL "Quantidade"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 32.86 BY 8
         FONT 1
         TITLE "Carteira Negativa".

DEFINE BROWSE br-work
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-work D-Dialog _FREEFORM
  QUERY br-work DISPLAY
      SUBSTR(tt-work.periodo,5,2) + "/" + 
SUBSTR(tt-work.periodo,1,4) COLUMN-LABEL " Periodo "
corte-comerc.descricao FORMAT "x(10)"  COLUMN-LABEL "Corte"
tt-work.qtd   COLUMN-LABEL "Qtd"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 25.43 BY 8
         FONT 1
         TITLE "Produzir".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     fi-nr-ob AT ROW 1.5 COL 12 COLON-ALIGNED
     fi-it-codigo AT ROW 2.5 COL 12 COLON-ALIGNED
     fi-desc-item AT ROW 2.5 COL 22.57 COLON-ALIGNED NO-LABEL
     fi-cod-refer AT ROW 3.5 COL 12 COLON-ALIGNED
     fi-qt-rev AT ROW 3.5 COL 58 COLON-ALIGNED
     br-carteira AT ROW 5.25 COL 3.14
     br-work AT ROW 5.25 COL 47
     bt-add AT ROW 7.75 COL 37.86
     bt-del AT ROW 8.96 COL 37.86
     bt-det AT ROW 10.83 COL 38 WIDGET-ID 2
     fi-tot-carteira AT ROW 13.5 COL 21.14 COLON-ALIGNED
     fi-tot-prod AT ROW 13.5 COL 58.86 COLON-ALIGNED
     bt-ok AT ROW 15.5 COL 3
     bt-cancela AT ROW 15.5 COL 14
     bt-ajuda AT ROW 15.5 COL 63.14
     RECT-1 AT ROW 4.75 COL 2
     rt-buttom AT ROW 15.25 COL 2
     rt-key-parent AT ROW 1.25 COL 2
     SPACE(1.28) SKIP(12.33)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 1
         TITLE "Cortes para OB's da RevisÆo"
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
   FRAME-NAME L-To-R                                                    */
/* BROWSE-TAB br-carteira fi-qt-rev D-Dialog */
/* BROWSE-TAB br-work br-carteira D-Dialog */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fi-cod-refer IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-item IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-it-codigo IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nr-ob IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qt-rev IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-carteira IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-prod IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-carteira
/* Query rebuild information for BROWSE br-carteira
     _START_FREEFORM
RUN pi-total.
OPEN QUERY {&SELF-NAME} FOR EACH tt-carteira where
                                 tt-carteira.qt-carteira > 0 NO-LOCK,
                            FIRST corte-comerc WHERE
                                  corte-comerc.codigo = tt-carteira.corte-comerc NO-LOCK
                                  BY tt-carteira.periodo
                                  BY tt-carteira.corte-comerc DESCENDING
                                  BY tt-carteira.qt-carteira
                                  INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-carteira */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-work
/* Query rebuild information for BROWSE br-work
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-work NO-LOCK,
                            FIRST corte-comerc WHERE
                                  corte-comerc.codigo = tt-work.corte-comerc NO-LOCK
                                  INDEXED-REPOSITION.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-work */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _TblList          = "espec.ordem-benefic"
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Cortes para OB's da RevisÆo */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-add D-Dialog
ON CHOOSE OF bt-add IN FRAME D-Dialog
DO:
   DO i-row = 1 TO br-carteira:NUM-SELECTED-ROWS:
      IF br-carteira:FETCH-SELECTED-ROW(i-row) THEN DO.
         ASSIGN de-produzir  = tt-carteira.qt-carteira.
         IF fi-tot-prod + tt-carteira.qt-carteira > 
            DEC(fi-qt-rev:SCREEN-VALUE IN FRAME {&FRAME-NAME}) THEN DO:
            ASSIGN de-produzir = DEC(fi-qt-rev:SCREEN-VALUE IN FRAME {&FRAME-NAME}) -
                                 fi-tot-prod.
         END.
         CREATE tt-work.
         ASSIGN tt-work.periodo      = tt-carteira.periodo
                tt-work.corte-comerc = tt-carteira.corte-comerc
                tt-work.nr-lote      = tt-carteira.nr-lote
                tt-work.qtd          = de-produzir.

         ASSIGN fi-tot-prod = fi-tot-prod + de-produzir.
         IF fi-tot-prod = DEC(fi-qt-rev:SCREEN-VALUE IN FRAME {&FRAME-NAME}) THEN DO.
            ASSIGN tt-carteira.qt-carteira = tt-carteira.qt-carteira - de-produzir
                   bt-add:SENSITIVE = NO
                   fi-tot-prod:FGCOLOR = 12.
         END.
         ELSE
            DELETE tt-carteira.
      END.
   END.

   {&OPEN-QUERY-br-carteira}
   {&OPEN-QUERY-br-work}

   ASSIGN fi-tot-prod:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(fi-tot-prod). 

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


&Scoped-define SELF-NAME bt-cancela
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancela D-Dialog
ON CHOOSE OF bt-cancela IN FRAME D-Dialog /* Cancelar */
DO:
  ASSIGN l-ok = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-del
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-del D-Dialog
ON CHOOSE OF bt-del IN FRAME D-Dialog
DO:
   
   DO i-row = 1 TO br-work:NUM-SELECTED-ROWS:
      IF br-work:FETCH-SELECTED-ROW(i-row) THEN DO.
         FIND tt-carteira WHERE
              tt-carteira.periodo      = tt-work.periodo      AND
              tt-carteira.corte-comerc = tt-work.corte-comerc AND
              tt-carteira.nr-lote      = tt-work.nr-lote      NO-ERROR.
         IF NOT AVAIL tt-carteira THEN DO.
            CREATE tt-carteira.
            ASSIGN tt-carteira.periodo      = tt-work.periodo
                   tt-carteira.corte-comerc = tt-work.corte-comerc
                   tt-carteira.nr-lote      = tt-work.nr-lote
                   tt-carteira.qt-carteira  = tt-work.qtd.
         END.
         ELSE
             ASSIGN tt-carteira.qt-carteira = tt-carteira.qt-carteira + tt-work.qtd.
         ASSIGN fi-tot-prod = fi-tot-prod - tt-work.qtd
                bt-add:SENSITIVE = YES
                fi-tot-prod:FGCOLOR = ?.

         DELETE tt-work.
      END.
   END.

   {&OPEN-QUERY-br-carteira}
   {&OPEN-QUERY-br-work}

   ASSIGN fi-tot-prod:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(fi-tot-prod).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-det
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-det D-Dialog
ON CHOOSE OF bt-det IN FRAME D-Dialog /* Button 1 */
DO:
   FOR EACH tt-pedidos.
       DELETE tt-pedidos.
   END.
   DO i-ct = 1 TO NUM-ENTRIES(tt-carteira.row-pedidos,";").
      FIND ped-venda WHERE
           ROWID(ped-venda) = TO-ROWID(ENTRY(i-ct,tt-carteira.row-pedidos,";"))
           NO-LOCK NO-ERROR.

      FIND tt-pedidos WHERE
           tt-pedidos.nr-pedcli = ped-venda.nr-pedcli NO-LOCK NO-ERROR.
      IF NOT AVAIL tt-pedidos THEN DO.
         CREATE tt-pedidos.
         BUFFER-COPY ped-venda TO tt-pedidos
                ASSIGN tt-pedidos.it-codigo = tt-carteira.it-codigo
                       tt-pedidos.cod-refer = tt-carteira.cod-refer.
      END.
      ASSIGN tt-pedidos.visualiza = YES.    

      FOR EACH ped-item OF ped-venda WHERE
               ped-item.it-codigo = tt-carteira.it-codigo AND
               ped-item.cod-refer = tt-carteira.cod-refer AND
               LOOKUP(STRING(ped-item.cod-sit-item),"1,2,5") > 0 NO-LOCK.

          FIND ped-item-res OF ped-item NO-LOCK NO-ERROR.

          IF AVAIL ped-item-res THEN NEXT.

          ASSIGN tt-pedidos.qt-pedida = tt-pedidos.qt-pedida + ped-item.qt-pedida.
      END.
   END.
   RUN esp/essp0100c.p (INPUT "A",
                        INPUT TABLE tt-pedidos).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok D-Dialog
ON CHOOSE OF bt-ok IN FRAME D-Dialog /* OK */
DO:
  ASSIGN l-ok = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-carteira
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
  DISPLAY fi-nr-ob fi-it-codigo fi-desc-item fi-cod-refer fi-qt-rev 
          fi-tot-carteira fi-tot-prod 
      WITH FRAME D-Dialog.
  ENABLE RECT-1 rt-buttom rt-key-parent br-carteira br-work bt-add bt-del 
         bt-det bt-ok bt-cancela bt-ajuda 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN fi-nr-ob:SCREEN-VALUE IN FRAME {&FRAME-NAME}     = STRING(p-nr-ob)
         fi-it-codigo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(p-it-codigo)
         fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(p-desc-item)
         fi-cod-refer:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(p-cod-refer)
         fi-qt-rev:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = STRING(p-quantidade).

 /* RUN pi-popula-browse. */
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
  ASSIGN fi-tot-carteira = 0
         fi-tot-prod = 0.
  FOR EACH tt-carteira WHERE
           tt-carteira.qt-carteira > 0
           NO-LOCK.
      ASSIGN fi-tot-carteira = fi-tot-carteira + tt-carteira.qt-carteira.                   
  END.

  FOR EACH tt-work.
      ASSIGN fi-tot-prod = fi-tot-prod + tt-work.qtd.
  END.


  DISP fi-tot-carteira
       fi-tot-prod
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
  {src/adm/template/snd-list.i "tt-work"}
  {src/adm/template/snd-list.i "corte-comerc"}
  {src/adm/template/snd-list.i "tt-carteira"}
  {src/adm/template/snd-list.i "ordem-benefic"}

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

