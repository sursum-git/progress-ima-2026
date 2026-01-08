&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          espec            PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i V02ES072 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
&Scop adm-attribute-dlg support/viewerd.w

/* global variable definitions */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
def var v-row-parent as rowid no-undo.

DEF NEW GLOBAL SHARED VAR c-cod-estabel AS CHAR.
DEF NEW GLOBAL SHARED VAR c-num-trf AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES espec.ob-trf
&Scoped-define FIRST-EXTERNAL-TABLE espec.ob-trf


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR espec.ob-trf.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS espec.ob-trf.it-codigo espec.ob-trf.cod-refer ~
espec.ob-trf.nr-lote espec.ob-trf.corte-comerc 
&Scoped-define ENABLED-TABLES espec.ob-trf
&Scoped-define FIRST-ENABLED-TABLE espec.ob-trf
&Scoped-Define ENABLED-OBJECTS rt-key rt-mold 
&Scoped-Define DISPLAYED-FIELDS espec.ob-trf.num-trf espec.ob-trf.dt-solic ~
espec.ob-trf.it-codigo espec.ob-trf.cod-refer espec.ob-trf.nr-lote ~
espec.ob-trf.corte-comerc espec.ob-trf.dec-1 
&Scoped-define DISPLAYED-TABLES espec.ob-trf
&Scoped-define FIRST-DISPLAYED-TABLE espec.ob-trf
&Scoped-Define DISPLAYED-OBJECTS fi-cod-estabel fi-nome-estabel ~
tg-retrabalho tg-retalho fi-desc-item fi-desc-refer fi-desc-comerc 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS espec.ob-trf.dec-1 
&Scoped-define ADM-ASSIGN-FIELDS espec.ob-trf.num-trf espec.ob-trf.dt-solic ~
espec.ob-trf.dec-1 
&Scoped-define List-5 fi-cod-estabel 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
num-trf|y|y|espec.ob-trf.num-trf
it-codigo||y|espec.ob-trf.it-codigo
cod-refer||y|espec.ob-trf.cod-refer
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "num-trf",
     Keys-Supplied = "num-trf,it-codigo,cod-refer"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE fi-cod-estabel AS CHARACTER FORMAT "X(3)":U 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 4.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-comerc AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-item AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 41.43 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-refer AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 41.43 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nome-estabel AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 39 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88 BY 1.5.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88 BY 7.5.

DEFINE VARIABLE tg-retalho AS LOGICAL INITIAL no 
     LABEL "Retalho" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tg-retrabalho AS LOGICAL INITIAL no 
     LABEL "Retrabalho" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     espec.ob-trf.num-trf AT ROW 1.25 COL 18.14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .88
     espec.ob-trf.dt-solic AT ROW 1.25 COL 60 COLON-ALIGNED
          LABEL "Data Solicitaá∆o"
          VIEW-AS FILL-IN 
          SIZE 12 BY .88
     fi-cod-estabel AT ROW 2.75 COL 18 COLON-ALIGNED WIDGET-ID 10
     fi-nome-estabel AT ROW 2.75 COL 23 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     tg-retrabalho AT ROW 3.75 COL 20.14
     tg-retalho AT ROW 3.75 COL 41.14
     espec.ob-trf.it-codigo AT ROW 4.75 COL 18.14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY .88
     fi-desc-item AT ROW 4.75 COL 30.57 COLON-ALIGNED NO-LABEL
     espec.ob-trf.cod-refer AT ROW 5.75 COL 18.14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY .88
     fi-desc-refer AT ROW 5.75 COL 30.57 COLON-ALIGNED NO-LABEL
     espec.ob-trf.nr-lote AT ROW 6.75 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     espec.ob-trf.corte-comerc AT ROW 7.75 COL 18 COLON-ALIGNED
          LABEL "NOVO Corte Comercial"
          VIEW-AS FILL-IN 
          SIZE 3.14 BY .88
     fi-desc-comerc AT ROW 7.75 COL 21.57 COLON-ALIGNED NO-LABEL
     espec.ob-trf.dec-1 AT ROW 8.75 COL 18 COLON-ALIGNED
          LABEL "Quantidade"
          VIEW-AS FILL-IN 
          SIZE 14 BY .88
     rt-key AT ROW 1 COL 1
     rt-mold AT ROW 2.5 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: espec.ob-trf
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 9.25
         WIDTH              = 88.43.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{include/c-viewer.i}
{utp/ut-glob.i}
{include/i_dbtype.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME f-main:SCROLLABLE       = FALSE
       FRAME f-main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN espec.ob-trf.corte-comerc IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN espec.ob-trf.dec-1 IN FRAME f-main
   NO-ENABLE 1 2 EXP-LABEL                                              */
/* SETTINGS FOR FILL-IN espec.ob-trf.dt-solic IN FRAME f-main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN fi-cod-estabel IN FRAME f-main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN fi-desc-comerc IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-item IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-refer IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nome-estabel IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN espec.ob-trf.num-trf IN FRAME f-main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR TOGGLE-BOX tg-retalho IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-retrabalho IN FRAME f-main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-main
/* Query rebuild information for FRAME f-main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME f-main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME espec.ob-trf.cod-refer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL espec.ob-trf.cod-refer V-table-Win
ON ENTRY OF espec.ob-trf.cod-refer IN FRAME f-main /* Referància */
DO:
  
  IF SELF:SCREEN-VALUE <> "" THEN DO.
     FIND referencia WHERE
          referencia.cod-refer = INPUT FRAME {&FRAME-NAME} ob-trf.cod-refer
          NO-LOCK NO-ERROR.
     IF AVAIL referencia THEN
        ASSIGN fi-desc-refer:SCREEN-VALUE IN FRAME {&FRAME-NAME} = referencia.descricao.
  END.
  ELSE ASSIGN fi-desc-refer:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL espec.ob-trf.cod-refer V-table-Win
ON LEAVE OF espec.ob-trf.cod-refer IN FRAME f-main /* Referància */
DO:
  
  FIND referencia WHERE
       referencia.cod-refer = INPUT FRAME {&FRAME-NAME} ob-trf.cod-refer 
       NO-LOCK NO-ERROR.

  IF AVAIL referencia THEN
     ASSIGN fi-desc-refer:SCREEN-VALUE IN FRAME {&FRAME-NAME} = referencia.descricao.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL espec.ob-trf.cod-refer V-table-Win
ON MOUSE-SELECT-DBLCLICK OF espec.ob-trf.cod-refer IN FRAME f-main /* Referància */
DO:

  {include/zoomvar.i &prog-zoom=inzoom/z01in375.w
                     &campo=ob-trf.cod-refer
                     &campozoom=cod-refer
                     &parametros = "run pi-seta-inicial in wh-pesquisa(INPUT INPUT FRAME {&FRAME-NAME} ob-trf.it-codigo)."}
                     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL espec.ob-trf.cod-refer V-table-Win
ON RETURN OF espec.ob-trf.cod-refer IN FRAME f-main /* Referància */
DO:
  APPLY 'TAB' TO SELF.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME espec.ob-trf.corte-comerc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL espec.ob-trf.corte-comerc V-table-Win
ON ENTRY OF espec.ob-trf.corte-comerc IN FRAME f-main /* NOVO Corte Comercial */
DO:
  FIND corte-comerc WHERE
       corte-comerc.codigo = INPUT FRAME {&FRAME-NAME} ob-trf.corte-comerc 
       NO-LOCK NO-ERROR.
  IF AVAIL corte-comerc THEN
     ASSIGN fi-desc-comerc:SCREEN-VALUE IN FRAME {&FRAME-NAME} = corte-comerc.descricao.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL espec.ob-trf.corte-comerc V-table-Win
ON LEAVE OF espec.ob-trf.corte-comerc IN FRAME f-main /* NOVO Corte Comercial */
DO:

  FIND corte-comerc WHERE
       corte-comerc.codigo = INPUT FRAME {&FRAME-NAME} ob-trf.corte-comerc 
       NO-LOCK NO-ERROR.
  IF AVAIL corte-comerc THEN
     ASSIGN fi-desc-comerc:SCREEN-VALUE IN FRAME {&FRAME-NAME} = corte-comerc.descricao.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL espec.ob-trf.corte-comerc V-table-Win
ON MOUSE-SELECT-DBLCLICK OF espec.ob-trf.corte-comerc IN FRAME f-main /* NOVO Corte Comercial */
DO:
  {include/zoomvar.i &prog-zoom=eszoom/z01es065.w
                     &campo=ob-trf.corte-comerc
                     &campozoom=codigo}.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL espec.ob-trf.corte-comerc V-table-Win
ON RETURN OF espec.ob-trf.corte-comerc IN FRAME f-main /* NOVO Corte Comercial */
DO:
  APPLY 'TAB' TO SELF.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME espec.ob-trf.dt-solic
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL espec.ob-trf.dt-solic V-table-Win
ON RETURN OF espec.ob-trf.dt-solic IN FRAME f-main /* Data Solicitaá∆o */
DO:
  APPLY 'TAB' TO SELF.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-estabel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-estabel V-table-Win
ON ENTRY OF fi-cod-estabel IN FRAME f-main /* Estabelecimento */
DO:
  FIND estabelec WHERE
       estabelec.cod-estabel = INPUT FRAME {&FRAME-NAME} fi-cod-estabel 
       NO-LOCK NO-ERROR.
  IF AVAIL estabelec THEN 
     ASSIGN fi-nome-estabel:SCREEN-VALUE = TRIM(estabelec.cidade) + " / " + TRIM(estabelec.bairro).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-estabel V-table-Win
ON LEAVE OF fi-cod-estabel IN FRAME f-main /* Estabelecimento */
DO:
  FIND estabelec WHERE
       estabelec.cod-estabel = INPUT FRAME {&FRAME-NAME} fi-cod-estabel 
       NO-LOCK NO-ERROR.
  IF NOT AVAIL estabelec THEN DO.
     MESSAGE 'Estabelecimento n∆o Cadastrado....'
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     APPLY 'ENTRY' TO SELF.
     RETURN NO-APPLY.
  END.
  ASSIGN fi-nome-estabel:SCREEN-VALUE = TRIM(estabelec.cidade) + " / " + TRIM(estabelec.bairro).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME espec.ob-trf.it-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL espec.ob-trf.it-codigo V-table-Win
ON ENTRY OF espec.ob-trf.it-codigo IN FRAME f-main /* Item */
DO:
    IF SELF:SCREEN-VALUE <> "" THEN DO.
       FIND item WHERE
            item.it-codigo = INPUT FRAME {&FRAME-NAME} ob-trf.it-codigo
            NO-LOCK NO-ERROR.
       IF AVAIL item THEN
          ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
    END.
    ELSE ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL espec.ob-trf.it-codigo V-table-Win
ON LEAVE OF espec.ob-trf.it-codigo IN FRAME f-main /* Item */
DO:
  IF INPUT FRAME {&FRAME-NAME} ob-trf.it-codigo <> "" THEN DO.
     FIND item WHERE
          item.it-codigo = INPUT FRAME {&FRAME-NAME} ob-trf.it-codigo 
          NO-LOCK NO-ERROR.
    
     IF NOT AVAIL item THEN DO.
        MESSAGE 'Item n∆o Cadastrado...'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY 'ENTRY' TO SELF.
        RETURN NO-APPLY.
     END.
     ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
      
     /*
     IF INPUT FRAME {&FRAME-NAME} tg-retalho = NO AND
        SUBSTR(ITEM.it-codigo,6,1) = '9'  THEN DO.
        MESSAGE 'Item Ç de Retalho...'
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY 'ENTRY' TO SELF.
        RETURN NO-APPLY.
     END.

     IF INPUT FRAME {&FRAME-NAME} tg-retalho = YES AND
        SUBSTR(ITEM.it-codigo,6,1) <> '9'  THEN DO.
        MESSAGE 'Item N«O Ç de Retalho...'
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY 'ENTRY' TO SELF.
        RETURN NO-APPLY.
     END.
     */
     ASSIGN ob-trf.cod-refer:SENSITIVE = YES
            ob-trf.nr-lote:SENSITIVE = YES.
      
     IF ITEM.tipo-con-est <> 4 THEN
        ASSIGN ob-trf.cod-refer:SENSITIVE = NO.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL espec.ob-trf.it-codigo V-table-Win
ON MOUSE-SELECT-DBLCLICK OF espec.ob-trf.it-codigo IN FRAME f-main /* Item */
DO:
    {include/zoomvar.i &prog-zoom=inzoom/z01in172.w
                     &campo=ob-trf.it-codigo
                     &campozoom=it-codigo}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL espec.ob-trf.it-codigo V-table-Win
ON RETURN OF espec.ob-trf.it-codigo IN FRAME f-main /* Item */
DO:
  APPLY 'TAB' TO SELF.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME espec.ob-trf.nr-lote
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL espec.ob-trf.nr-lote V-table-Win
ON LEAVE OF espec.ob-trf.nr-lote IN FRAME f-main /* Lote */
DO:
  IF SUBSTR(ob-trf.nr-lote:SCREEN-VALUE IN FRAME {&FRAME-NAME},1,2) <> "RP" AND
     SUBSTR(ob-trf.nr-lote:SCREEN-VALUE IN FRAME {&FRAME-NAME},1,2) <> "PD" AND
     SUBSTR(ob-trf.nr-lote:SCREEN-VALUE IN FRAME {&FRAME-NAME},1,2) <> "RD" AND
     SUBSTR(ob-trf.nr-lote:SCREEN-VALUE IN FRAME {&FRAME-NAME},1,2) <> "PP" AND
     SUBSTR(ob-trf.nr-lote:SCREEN-VALUE IN FRAME {&FRAME-NAME},1,2) <> "CA" AND
     SUBSTR(ob-trf.nr-lote:SCREEN-VALUE IN FRAME {&FRAME-NAME},1,2) <> "SP" AND
     SUBSTR(ob-trf.nr-lote:SCREEN-VALUE IN FRAME {&FRAME-NAME},1,2) <> "RE" AND
     SUBSTR(ob-trf.nr-lote:SCREEN-VALUE IN FRAME {&FRAME-NAME},1,2) <> "SD" THEN DO:

      MESSAGE "Lotes Permitidos! 'RP', 'RD', 'PD', 'PP', 'CA', 'SP', 'RE', 'SD'" VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'entry' TO ob-trf.nr-lote.
      RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL espec.ob-trf.nr-lote V-table-Win
ON RETURN OF espec.ob-trf.nr-lote IN FRAME f-main /* Lote */
DO:
  APPLY 'TAB' TO SELF.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-retalho
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-retalho V-table-Win
ON VALUE-CHANGED OF tg-retalho IN FRAME f-main /* Retalho */
DO:
    ASSIGN ob-trf.it-codigo:SCREEN-VALUE    = ""
           ob-trf.cod-refer:SCREEN-VALUE    = ""
           ob-trf.nr-lote:SCREEN-VALUE      = ""
           ob-trf.corte-comerc:SCREEN-VALUE = ""
           fi-desc-comerc:SCREEN-VALUE      = ""
           ob-trf.dec-1:SCREEN-VALUE        = "0"
           ob-trf.it-codigo:LABEL           = "Item".

    ASSIGN tg-retrabalho:SENSITIVE       = YES
           ob-trf.cod-refer:SENSITIVE    = YES 
           ob-trf.nr-lote:SENSITIVE      = YES
           ob-trf.corte-comerc:SENSITIVE = YES
           ob-trf.dec-1:SENSITIVE        = NO.

    IF INPUT FRAME {&FRAME-NAME} tg-retalho THEN DO: 
       ASSIGN tg-retrabalho:SENSITIVE       = NO
              ob-trf.cod-refer:SENSITIVE    = NO
              ob-trf.nr-lote:SENSITIVE      = NO
              ob-trf.corte-comerc:SENSITIVE = NO
              ob-trf.dec-1:SENSITIVE        = NO
              ob-trf.it-codigo:LABEL        = "NOVO Item".

       APPLY 'entry' TO ob-trf.it-codigo.
       RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-retrabalho
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-retrabalho V-table-Win
ON VALUE-CHANGED OF tg-retrabalho IN FRAME f-main /* Retrabalho */
DO:
   ASSIGN ob-trf.dec-1:SENSITIVE        = NO
          ob-trf.dec-1:SCREEN-VALUE     = "0"
          ob-trf.corte-comerc:SENSITIVE = YES
          ob-trf.dt-solic:SENSITIVE     = NO
          tg-retalho:SENSITIVE          = YES.
       
   IF INPUT FRAME {&FRAME-NAME} tg-retrabalho THEN DO: 
      ASSIGN ob-trf.corte-comerc:SCREEN-VALUE = ""
             fi-desc-comerc:SCREEN-VALUE      = ""
             ob-trf.dec-1:SENSITIVE           = YES
             ob-trf.dt-solic:SENSITIVE        = NO
             ob-trf.corte-comerc:SENSITIVE    = NO
             tg-retalho:SENSITIVE             = NO.
      APPLY 'entry' TO ob-trf.it-codigo.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  ob-trf.it-codigo:LOAD-MOUSE-POINTER("image/lupa.cur"). 
  ob-trf.cod-refer:LOAD-MOUSE-POINTER("image/lupa.cur"). 
  ob-trf.corte-comerc:LOAD-MOUSE-POINTER("image/lupa.cur"). 

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-find-using-key V-table-Win  adm/support/_key-fnd.p
PROCEDURE adm-find-using-key :
/*------------------------------------------------------------------------------
  Purpose:     Finds the current record using the contents of
               the 'Key-Name' and 'Key-Value' attributes.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEF VAR key-value AS CHAR NO-UNDO.
  DEF VAR row-avail-enabled AS LOGICAL NO-UNDO.

  /* LOCK status on the find depends on FIELDS-ENABLED. */
  RUN get-attribute ('FIELDS-ENABLED':U).
  row-avail-enabled = (RETURN-VALUE eq 'yes':U).
  /* Look up the current key-value. */
  RUN get-attribute ('Key-Value':U).
  key-value = RETURN-VALUE.

  /* Find the current record using the current Key-Name. */
  RUN get-attribute ('Key-Name':U).
  CASE RETURN-VALUE:
    WHEN 'num-trf':U THEN
       {src/adm/template/find-tbl.i
           &TABLE = espec.ob-trf
           &WHERE = "WHERE espec.ob-trf.num-trf eq INTEGER(key-value)"
       }
  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win  _ADM-ROW-AVAILABLE
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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "espec.ob-trf"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "espec.ob-trf"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win  _DEFAULT-DISABLE
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
  HIDE FRAME f-main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record V-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  ASSIGN fi-cod-estabel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = c-cod-estabel
         ob-trf.num-trf:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = STRING(NEXT-VALUE(seq-trf))
         ob-trf.dt-solic:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY)
         tg-retrabalho:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'.

  FIND estabelec WHERE
       estabelec.cod-estabel = c-cod-estabel NO-LOCK NO-ERROR.
  IF AVAIL estabelec THEN
     ASSIGN fi-nome-estabel:SCREEN-VALUE = TRIM(estabelec.cidade) + " / " + TRIM(estabelec.bairro).

  APPLY 'VALUE-CHANGED' TO tg-retrabalho.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-apply-entry V-table-Win 
PROCEDURE local-apply-entry :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'apply-entry':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF adm-new-record = NO THEN DO:
     FIND referencia WHERE
          referencia.cod-refer = INPUT FRAME {&FRAME-NAME} ob-trf.cod-refer 
          NO-LOCK NO-ERROR.
     IF AVAIL referencia THEN
        ASSIGN fi-desc-refer:SCREEN-VALUE IN FRAME {&FRAME-NAME} = referencia.descricao.

     FIND corte-comerc WHERE
          corte-comerc.codigo = INPUT FRAME {&FRAME-NAME} ob-trf.corte-comerc 
          NO-LOCK NO-ERROR.
     IF AVAIL corte-comerc THEN
        ASSIGN fi-desc-comerc:SCREEN-VALUE IN FRAME {&FRAME-NAME} = corte-comerc.descricao.

     ASSIGN tg-retrabalho:SCREEN-VALUE IN FRAME {&FRAME-NAME} = IF SUBSTR(ob-trf.char-1,1,1) = 'S'
                                                                THEN 'YES' ELSE 'NO'.

     IF SUBSTR(ob-trf.char-1,2,1) = 'S' THEN DO.
        ASSIGN tg-retalho:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'YES'.

        ASSIGN tg-retrabalho:SENSITIVE       = NO
               ob-trf.cod-refer:SENSITIVE    = NO
               ob-trf.nr-lote:SENSITIVE      = NO
               ob-trf.corte-comerc:SENSITIVE = NO
               ob-trf.dec-1:SENSITIVE        = NO
               ob-trf.it-codigo:LABEL        = "NOVO Item".
     END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    {include/i-valid.i}
    
    ASSIGN INPUT FRAME {&FRAME-NAME} tg-retrabalho
           INPUT FRAME {&FRAME-NAME} tg-retalho.

    /*:T Ponha na pi-validate todas as validaá‰es */
    /*:T N∆o gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */
    RUN pi-validate.
    IF RETURN-VALUE = 'ADM-ERROR':U THEN  
       RETURN 'ADM-ERROR':U.
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.
    
    /*:T Todos os assignÔs n∆o feitos pelo assign-record devem ser feitos aqui */  
    /* Code placed here will execute AFTER standard behavior.    */

    ASSIGN c-num-trf = IF c-num-trf = ""
                       THEN STRING(ob-trf.num-trf)
                       ELSE c-num-trf + ";" + STRING(ob-trf.num-trf)
           SUBSTR(ob-trf.char-1,1,1) = IF tg-retrabalho 
                                       THEN "S" ELSE ""
           SUBSTR(ob-trf.char-1,2,1) = IF tg-retalho 
                                       THEN "S" ELSE "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-disable-fields V-table-Win 
PROCEDURE local-disable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    
    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable-fields':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */
    &if  defined(ADM-MODIFY-FIELDS) &then
    disable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.
    &endif
    
    ASSIGN tg-retrabalho:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           tg-retalho:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields V-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    
    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */
    &if  defined(ADM-MODIFY-FIELDS) &then
    if adm-new-record = yes then
        enable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.
    &endif
    ASSIGN tg-retrabalho:SENSITIVE IN FRAME {&FRAME-NAME} = YES
           tg-retalho:SENSITIVE IN FRAME {&FRAME-NAME} = YES.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-atualiza-parent V-table-Win 
PROCEDURE pi-atualiza-parent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    define input parameter v-row-parent-externo as rowid no-undo.
    
    assign v-row-parent = v-row-parent-externo.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pi-validate V-table-Win 
PROCEDURE Pi-validate :
/*:T------------------------------------------------------------------------------
  Purpose:Validar a viewer     
  Parameters:  <none>
  Notes: N∆o fazer assign aqui. Nesta procedure
  devem ser colocadas apenas validaá‰es, pois neste ponto do programa o registro 
  ainda n∆o foi criado.       
------------------------------------------------------------------------------*/
   {include/i-vldfrm.i} /*:T Validaá∆o de dicion†rio */

   FIND item WHERE 
        item.it-codigo = INPUT FRAME {&FRAME-NAME} ob-trf.it-codigo  NO-LOCK NO-ERROR.
   IF NOT AVAIL ITEM THEN DO:
      MESSAGE "ITEM n∆o cadastrado ! ! !"  VIEW-AS ALERT-BOX. 
      APPLY 'entry' TO ob-trf.it-codigo.
      RETURN 'ADM-ERROR':U.
   END.

   IF INPUT FRAME {&FRAME-NAME} ob-trf.dt-solic = ? THEN DO:
      MESSAGE "A data de solicitacao n∆o Ç v†lida." VIEW-AS ALERT-BOX. 
      APPLY 'entry' TO ob-trf.dt-solic.
      RETURN 'ADM-ERROR':U.
   END.

   IF ob-trf.cod-refer:SENSITIVE THEN DO.
      FIND referencia WHERE 
           referencia.cod-refer = INPUT FRAME {&FRAME-NAME} ob-trf.cod-refer 
           NO-LOCK NO-ERROR.
      IF NOT AVAIL referencia THEN DO:
         MESSAGE "REFERENCIA n∆o cadastrada ! ! !"  VIEW-AS ALERT-BOX. 
         APPLY 'entry' TO ob-trf.cod-refer.
         RETURN 'ADM-ERROR':U.
      END.

      FIND ref-item WHERE
           ref-item.cod-refer = INPUT FRAME {&FRAME-NAME} ob-trf.cod-refer AND
           ref-item.it-codigo = INPUT FRAME {&FRAME-NAME} ob-trf.it-codigo NO-LOCK NO-ERROR.
      IF NOT AVAIL ref-item THEN DO.
         MESSAGE "Referància n∆o Vinculada ao Item ! ! !"
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         APPLY 'entry' TO ob-trf.cod-refer.
         RETURN 'ADM-ERROR':U.
      END.
   END.

   IF ob-trf.nr-lote:SENSITIVE THEN DO.
      IF SUBSTR(ob-trf.nr-lote:SCREEN-VALUE IN FRAME {&FRAME-NAME},1,2) <> "RP" AND
         SUBSTR(ob-trf.nr-lote:SCREEN-VALUE IN FRAME {&FRAME-NAME},1,2) <> "PD" AND
         SUBSTR(ob-trf.nr-lote:SCREEN-VALUE IN FRAME {&FRAME-NAME},1,2) <> "RD" AND
         SUBSTR(ob-trf.nr-lote:SCREEN-VALUE IN FRAME {&FRAME-NAME},1,2) <> "PP" AND
         SUBSTR(ob-trf.nr-lote:SCREEN-VALUE IN FRAME {&FRAME-NAME},1,2) <> "CA" AND
         SUBSTR(ob-trf.nr-lote:SCREEN-VALUE IN FRAME {&FRAME-NAME},1,2) <> "SP" AND
         SUBSTR(ob-trf.nr-lote:SCREEN-VALUE IN FRAME {&FRAME-NAME},1,2) <> "RE" AND
         SUBSTR(ob-trf.nr-lote:SCREEN-VALUE IN FRAME {&FRAME-NAME},1,2) <> "SD" THEN DO:
    
         MESSAGE "Lotes Permitidos: 'RP','RD','PD','PP','CA','SP','RE','SD'" 
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
         APPLY 'entry' TO ob-trf.nr-lote.
         RETURN 'ADM-ERROR':U.
      END.
   END.
   /*
   IF NOT INPUT FRAME {&FRAME-NAME} tg-retalho AND
      SUBSTR(ITEM.it-codigo,6,1) = '9'  THEN DO.
      MESSAGE 'Item Ç de Retalho...'
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'entry' TO ob-trf.it-codigo.
      RETURN 'ADM-ERROR':U.
   END.

   IF INPUT FRAME {&FRAME-NAME} tg-retalho AND
      SUBSTR(ITEM.it-codigo,6,1) <> '9'  THEN DO.
      MESSAGE 'Item N«O Ç de Retalho...'
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'entry' TO ob-trf.it-codigo.
      RETURN 'ADM-ERROR':U.
   END.
   */

   IF INPUT FRAME {&FRAME-NAME} tg-retrabalho = NO AND 
      INPUT FRAME {&FRAME-NAME} tg-retalho = NO THEN DO.
      FIND corte-comerc WHERE 
           corte-comerc.codigo = INPUT FRAME {&FRAME-NAME} ob-trf.corte-comerc 
           NO-LOCK NO-ERROR.
      IF NOT AVAIL corte-comerc THEN DO:
         MESSAGE "CORTE COMERCIAL n∆o cadastrado ! ! !"  VIEW-AS ALERT-BOX. 
         APPLY 'entry' TO ob-trf.corte-comerc.
         RETURN 'ADM-ERROR':U.
      END.
   END.

   IF INPUT FRAME {&FRAME-NAME} tg-retrabalho = YES AND 
      INPUT FRAME {&FRAME-NAME} ob-trf.dec-1 = 0  THEN DO.
      MESSAGE "Para Retrabalho, Ç obrigat¢rio informar a Quantidade..."
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'entry' TO ob-trf.dec-1.
      RETURN 'ADM-ERROR':U.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key V-table-Win  adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "num-trf" "espec.ob-trf" "num-trf"}
  {src/adm/template/sndkycas.i "it-codigo" "espec.ob-trf" "it-codigo"}
  {src/adm/template/sndkycas.i "cod-refer" "espec.ob-trf" "cod-refer"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "espec.ob-trf"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/vstates.i}
  END CASE.
  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

