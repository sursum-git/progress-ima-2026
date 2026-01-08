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
{include/i-prgvrs.i V99XX999 9.99.99.999}

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
DEFINE VARIABLE var-error AS CHARACTER   NO-UNDO.

DEFINE VARIABLE var-bt-leitora AS LOGICAL INITIAL YES    NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES espec.malote
&Scoped-define FIRST-EXTERNAL-TABLE espec.malote


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR espec.malote.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS espec.malote.num-malote ~
espec.malote.descricao espec.malote.responsavel espec.malote.num-contrato ~
espec.malote.percurso espec.malote.cod-estabel-orig ~
espec.malote.cod-barras-orig espec.malote.cod-estabel-dest ~
espec.malote.cod-barras-dest espec.malote.mov-completo 
&Scoped-define ENABLED-TABLES espec.malote
&Scoped-define FIRST-ENABLED-TABLE espec.malote
&Scoped-Define ENABLED-OBJECTS rt-key rt-mold rt-mov-completo 
&Scoped-Define DISPLAYED-FIELDS espec.malote.num-malote ~
espec.malote.descricao espec.malote.responsavel espec.malote.num-contrato ~
espec.malote.percurso espec.malote.cod-estabel-orig ~
espec.malote.cod-barras-orig espec.malote.cod-estabel-dest ~
espec.malote.cod-barras-dest espec.malote.mov-completo 
&Scoped-define DISPLAYED-TABLES espec.malote
&Scoped-define FIRST-DISPLAYED-TABLE espec.malote
&Scoped-Define DISPLAYED-OBJECTS fi-nome-estab-aux-orig ~
fi-nome-estab-aux-dest 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define List-6 espec.malote.num-malote espec.malote.descricao ~
espec.malote.responsavel espec.malote.num-contrato espec.malote.percurso ~
espec.malote.cod-estabel-orig espec.malote.cod-barras-orig ~
espec.malote.cod-estabel-dest espec.malote.cod-barras-dest ~
espec.malote.mov-completo 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
num-malote|y|y|espec.malote.num-malote
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "num-malote",
     Keys-Supplied = "num-malote"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-leitora 
     IMAGE-UP FILE "image/im-f-dc.bmp":U
     LABEL "Com Leitora" 
     SIZE 6 BY 1.

DEFINE VARIABLE fi-nome-estab-aux-dest AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 34 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nome-estab-aux-orig AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 34 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 1.25.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 8.25.

DEFINE RECTANGLE rt-mov-completo
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 19 BY 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     bt-leitora AT ROW 1.08 COL 82.43 WIDGET-ID 32
     espec.malote.num-malote AT ROW 1.17 COL 19 COLON-ALIGNED WIDGET-ID 14
          VIEW-AS FILL-IN 
          SIZE 6.86 BY .88
     espec.malote.descricao AT ROW 1.17 COL 35 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 26.14 BY .88
     espec.malote.responsavel AT ROW 2.79 COL 18.86 COLON-ALIGNED WIDGET-ID 16
          VIEW-AS FILL-IN 
          SIZE 16.14 BY .88
     espec.malote.num-contrato AT ROW 3.79 COL 19 COLON-ALIGNED WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 11 BY .88 NO-TAB-STOP 
     espec.malote.percurso AT ROW 3.79 COL 38 COLON-ALIGNED WIDGET-ID 30
          VIEW-AS FILL-IN 
          SIZE 6.86 BY .88
     espec.malote.cod-estabel-orig AT ROW 5 COL 19 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 6.86 BY .88
     fi-nome-estab-aux-orig AT ROW 5 COL 26 COLON-ALIGNED NO-LABEL WIDGET-ID 18 NO-TAB-STOP 
     espec.malote.cod-barras-orig AT ROW 6 COL 19 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 41.14 BY .88
     espec.malote.cod-estabel-dest AT ROW 7.25 COL 19 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 6.86 BY .88
     fi-nome-estab-aux-dest AT ROW 7.25 COL 26 COLON-ALIGNED NO-LABEL WIDGET-ID 20 NO-TAB-STOP 
     espec.malote.cod-barras-dest AT ROW 8.25 COL 19 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 41.14 BY .88
     espec.malote.mov-completo AT ROW 9.63 COL 21.86 NO-LABEL WIDGET-ID 34
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Sim":U, yes,
"N∆o":U, no
          SIZE 18 BY .75 TOOLTIP "Registrar movimento dos dois estabel.?" NO-TAB-STOP 
     "Mov. Completo:" VIEW-AS TEXT
          SIZE 11 BY .54 AT ROW 9.71 COL 10 WIDGET-ID 38
     rt-key AT ROW 1 COL 1
     rt-mold AT ROW 2.5 COL 1
     rt-mov-completo AT ROW 9.5 COL 21.29 WIDGET-ID 26
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: espec.malote
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
         HEIGHT             = 10
         WIDTH              = 88.86.
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

/* SETTINGS FOR BUTTON bt-leitora IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN espec.malote.cod-barras-dest IN FRAME f-main
   6                                                                    */
/* SETTINGS FOR FILL-IN espec.malote.cod-barras-orig IN FRAME f-main
   6                                                                    */
/* SETTINGS FOR FILL-IN espec.malote.cod-estabel-dest IN FRAME f-main
   6                                                                    */
/* SETTINGS FOR FILL-IN espec.malote.cod-estabel-orig IN FRAME f-main
   6                                                                    */
/* SETTINGS FOR FILL-IN espec.malote.descricao IN FRAME f-main
   6                                                                    */
/* SETTINGS FOR FILL-IN fi-nome-estab-aux-dest IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nome-estab-aux-orig IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET espec.malote.mov-completo IN FRAME f-main
   6                                                                    */
/* SETTINGS FOR FILL-IN espec.malote.num-contrato IN FRAME f-main
   6                                                                    */
/* SETTINGS FOR FILL-IN espec.malote.num-malote IN FRAME f-main
   6                                                                    */
/* SETTINGS FOR FILL-IN espec.malote.percurso IN FRAME f-main
   6                                                                    */
/* SETTINGS FOR FILL-IN espec.malote.responsavel IN FRAME f-main
   6                                                                    */
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

&Scoped-define SELF-NAME bt-leitora
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-leitora V-table-Win
ON CHOOSE OF bt-leitora IN FRAME f-main /* Com Leitora */
DO:
  /*ASSIGN /*malote.num-malote:HIDDEN IN FRAME {&FRAME-NAME} = YES.*/
         malote.responsavel       :HIDDEN IN FRAME {&FRAME-NAME} = YES
         malote.percurso          :HIDDEN IN FRAME {&FRAME-NAME} = YES
         malote.num-malote        :HIDDEN IN FRAME {&FRAME-NAME} = YES
         malote.num-contrato      :HIDDEN IN FRAME {&FRAME-NAME} = YES
         malote.mov-completo      :HIDDEN IN FRAME {&FRAME-NAME} = YES
         malote.descricao         :HIDDEN IN FRAME {&FRAME-NAME} = YES
         malote.cod-estabel-orig  :HIDDEN IN FRAME {&FRAME-NAME} = YES
         malote.cod-estabel-dest  :HIDDEN IN FRAME {&FRAME-NAME} = YES
         malote.cod-barras-orig   :HIDDEN IN FRAME {&FRAME-NAME} = YES
         malote.cod-barras-dest   :HIDDEN IN FRAME {&FRAME-NAME} = YES
         rt-mov-completo          :HIDDEN IN FRAME {&FRAME-NAME} = YES
         fi-nome-estab-aux-orig   :HIDDEN IN FRAME {&FRAME-NAME} = YES 
         fi-nome-estab-aux-dest   :HIDDEN IN FRAME {&FRAME-NAME} = YES
         fi-mov-completo          :HIDDEN IN FRAME {&FRAME-NAME} = YES.*/
   
    If  var-bt-leitora = YES THEN DO:
        bt-leitora:LOAD-IMAGE("image/im-f-dc.bmp") IN FRAME {&FRAME-NAME}.     
        ENABLE {&list-6} WITH FRAME {&FRAME-NAME}.
        ASSIGN var-bt-leitora = NO.
    END.
    ELSE DO:
        bt-leitora:LOAD-IMAGE("image/tv-fillin.bmp") IN FRAME {&FRAME-NAME}.
        DISABLE {&list-6} WITH FRAME {&FRAME-NAME}.
        ASSIGN var-bt-leitora = YES.
        ENABLE malote.cod-barras-orig WITH FRAME {&FRAME-NAME}.
        APPLY "entry" TO malote.cod-barras-orig.
    END.    
                                 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME espec.malote.cod-barras-dest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL espec.malote.cod-barras-dest V-table-Win
ON LEAVE OF espec.malote.cod-barras-dest IN FRAME f-main /* Cod Barras Dest */
DO:
  IF DECIMAL(SELF:SCREEN-VALUE) <> 0 THEN DO:
     IF SUBSTRING(SELF:screen-value,31,5) = malote.num-malote:SCREEN-VALUE IN FRAME {&FRAME-NAME} AND
        SUBSTRING(SELF:screen-value,20,6) = malote.percurso:SCREEN-VALUE IN FRAME {&FRAME-NAME}   AND
        SUBSTRING(SELF:screen-value,30,1) = "1" THEN DO:

        IF malote.cod-estabel-dest:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0" THEN DO:
           FIND FIRST estabel-aux WHERE estabel-aux.cep = int(SUBSTRING(SELF:screen-value,1,8)) NO-LOCK NO-ERROR.
           IF AVAIL estabel-aux THEN DO:
              ASSIGN malote.cod-estabel-dest:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(estabel-aux.cod-estabel-aux).
              ENABLE {&list-6} WITH FRAME {&frame-name}.
           END.
           ELSE DO:
              MESSAGE "N∆o foi encontrada estabelecimento para este codigo de barras." SKIP
                      "Deseja criar outro estabelecimento?"
                  VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE msg-inclui-estab-aux AS LOGICAL.
              IF msg-inclui-estab-aux THEN
                 RUN esp/estabel-aux.w.
              APPLY "entry" TO malote.cod-barras-dest IN FRAME {&FRAME-NAME}.
              RETURN NO-APPLY.    
               /*APPLY "entry" TO malote.cod-barras-dest IN FRAME {&FRAME-NAME}.*/
            END.
        END.
        ELSE DO:
           FIND FIRST estabel-aux WHERE estabel-aux.cod-estabel-aux = INPUT FRAME {&frame-name} malote.cod-estabel-dest NO-LOCK NO-ERROR.
           IF AVAIL estabel-aux  AND estabel-aux.cep <> int(SUBSTRING(SELF:screen-value,1,8)) THEN DO:
              ASSIGN var-error = "CEP do Codigo de barras Ç diferente do CEP do destino" + CHR(10).
           END.
        END.
     END.
     ELSE DO:
        ASSIGN var-error = "Codigo de barras invalido para este malote" + CHR(10).
        IF SUBSTRING(SELF:screen-value,31,5) = malote.num-malote:SCREEN-VALUE IN FRAME {&FRAME-NAME} THEN 
           ASSIGN var-error = var-error + "- Numero do malote diferente do codigo de barras" + CHR(10).
        IF SUBSTRING(SELF:screen-value,20,6) = malote.percurso:SCREEN-VALUE IN FRAME {&FRAME-NAME} THEN
           ASSIGN var-error = var-error + "- Percurso diferente do codigo de barras" + CHR(10).
        IF SUBSTRING(SELF:screen-value,30,1) = "1" THEN 
           ASSIGN var-error = var-error + "Este codigo de barras n∆o Ç um codigo de destino" + CHR(10). 
        /*MESSAGE "Codigo de barras invalido para este malote e/ou n∆o Ç um codigo de destino"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
     END.

     IF var-error <> "" THEN DO:
        MESSAGE var-error
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        APPLY "entry" TO malote.cod-barras-dest IN FRAME {&FRAME-NAME}.
        ASSIGN var-error = "".
        RETURN NO-APPLY.
     END.
     FIND FIRST malote WHERE malote.cod-estabel-orig = INPUT FRAME {&frame-name} malote.cod-estabel-orig
                         AND malote.cod-estabel-dest = INPUT FRAME {&frame-name} malote.cod-estabel-dest NO-LOCK NO-ERROR.
     IF AVAIL malote THEN DO:
        ASSIGN malote.descricao  :SCREEN-VALUE IN FRAME {&FRAME-NAME} = malote.descricao /*+ "_" + INPUT FRAME {&frame-name} malote.num-malote*/
               malote.responsavel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = malote.responsavel.
     END.
     
     APPLY "entry" TO malote.descricao IN FRAME {&frame-name}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL espec.malote.cod-barras-dest V-table-Win
ON VALUE-CHANGED OF espec.malote.cod-barras-dest IN FRAME f-main /* Cod Barras Dest */
DO:
  IF LENGTH(SELF:SCREEN-VALUE) = 35 THEN DO:
     APPLY "leave" TO malote.cod-barras-dest IN FRAME {&FRAME-NAME}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME espec.malote.cod-barras-orig
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL espec.malote.cod-barras-orig V-table-Win
ON LEAVE OF espec.malote.cod-barras-orig IN FRAME f-main /* Cod Barras Orig */
DO:
  
  IF DECIMAL(SELF:SCREEN-VALUE) <> 0 THEN DO:
     IF SUBSTRING(SELF:screen-value,30,1) = "8" THEN DO:
          If var-bt-leitora = YES THEN DO:
          /*IF malote.num-malote:SENSITIVE IN FRAME {&FRAME-NAME} = NO THEN DO:*/
             ASSIGN malote.num-malote:SCREEN-VALUE IN FRAME {&FRAME-NAME} = SUBSTRING(SELF:screen-value,31,5)
                    malote.percurso:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = SUBSTRING(SELF:screen-value,20,6).
             FIND FIRST malote WHERE malote.num-malote = int(malote.num-malote:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
             IF NOT AVAIL malote THEN DO:
                 FIND FIRST estabel-aux WHERE estabel-aux.cep = int(SUBSTRING(SELF:screen-value,1,8)) NO-LOCK NO-ERROR.
                 IF AVAIL estabel-aux THEN DO:
                    ASSIGN malote.cod-estabel-orig:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(estabel-aux.cod-estabel-aux).
                    ENABLE malote.cod-barras-dest WITH FRAME {&frame-name}.
                    APPLY "entry" TO  malote.cod-barras-dest IN FRAME {&frame-name}.
                 END.
                 ELSE DO:
                     MESSAGE "N∆o foi encontrada estabelecimento para este codigo de barras." SKIP
                             "Deseja criar outro estabelecimento?"
                         VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE msg-inclui-estab-aux AS LOGICAL.
                     IF msg-inclui-estab-aux THEN
                        RUN esp/estabel-aux.w.
        
                     APPLY "entry" TO malote.cod-barras-orig IN FRAME {&FRAME-NAME}.
                     RETURN NO-APPLY.
                 END.
             END.
             ELSE DO:
                 MESSAGE "Malote j† cadastrado"
                     VIEW-AS ALERT-BOX INFO BUTTONS OK.
                 APPLY "entry" TO malote.cod-barras-orig IN FRAME {&FRAME-NAME}.
                     RETURN NO-APPLY.
             END.                              
          END.
          ELSE DO:
              FIND FIRST estabel-aux WHERE estabel-aux.cod-estabel-aux = INPUT FRAME {&frame-name} malote.cod-estabel-orig NO-LOCK NO-ERROR.
              IF NOT AVAIL estabel-aux OR estabel-aux.cep <> INT(SUBSTRING(SELF:screen-value,1,8)) THEN DO:
                 MESSAGE "Cep do codigo de barras diferente da origem"
                     VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                 /*APPLY "entry" TO  malote.cod-estabel-orig IN FRAME {&frame-name}.*/
                 APPLY "entry" TO malote.cod-barras-orig IN FRAME {&FRAME-NAME}.
                 RETURN NO-APPLY.
              END.
          END.
     END.
     ELSE DO:
        MESSAGE "Codigo de barras n∆o Ç de origem"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY "entry" TO malote.cod-barras-orig IN FRAME {&FRAME-NAME}.
        RETURN NO-APPLY.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL espec.malote.cod-barras-orig V-table-Win
ON VALUE-CHANGED OF espec.malote.cod-barras-orig IN FRAME f-main /* Cod Barras Orig */
DO:
  IF LENGTH(SELF:SCREEN-VALUE) = 35 THEN DO:
     APPLY "leave" TO malote.cod-barras-orig IN FRAME {&FRAME-NAME}.
     /*APPLY "entry" TO malote.cod-barras-dest IN FRAME {&FRAME-NAME}.*/
      /*SELF:MOVE-AFTER-TAB (malote.cod-barras-dest:HANDLE IN FRAME {&FRAME-NAME}).*/
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME espec.malote.cod-estabel-dest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL espec.malote.cod-estabel-dest V-table-Win
ON LEAVE OF espec.malote.cod-estabel-dest IN FRAME f-main /* Cod. Destino */
DO:
  FIND FIRST estabel-aux WHERE estabel-aux.cod-estabel-aux = SELF:INPUT-VALUE NO-LOCK NO-ERROR.
  IF AVAIL estabel-aux THEN DO:
     ASSIGN fi-nome-estab-aux-dest:SCREEN-VALUE IN FRAME {&FRAME-NAME} = estabel-aux.nome.
     IF malote.descricao:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" THEN
        ASSIGN malote.descricao:SCREEN-VALUE IN FRAME {&FRAME-NAME}         = estabel-aux.nome
               espec.malote.responsavel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = estabel-aux.contato.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL espec.malote.cod-estabel-dest V-table-Win
ON MOUSE-SELECT-DBLCLICK OF espec.malote.cod-estabel-dest IN FRAME f-main /* Cod. Destino */
DO:
 {include/zoomvar.i &prog-zoom=eszoom\z01estabel-aux.w
                     &campo=malote.cod-estabel-dest
                     &campozoom=cod-estabel-aux}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME espec.malote.cod-estabel-orig
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL espec.malote.cod-estabel-orig V-table-Win
ON LEAVE OF espec.malote.cod-estabel-orig IN FRAME f-main /* Cod. Origem */
DO:
  FIND FIRST estabel-aux WHERE estabel-aux.cod-estabel-aux = SELF:INPUT-VALUE NO-LOCK NO-ERROR.
  ASSIGN fi-nome-estab-aux-orig:SCREEN-VALUE IN FRAME {&FRAME-NAME} = IF AVAIL estabel-aux THEN estabel-aux.nome ELSE "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL espec.malote.cod-estabel-orig V-table-Win
ON MOUSE-SELECT-DBLCLICK OF espec.malote.cod-estabel-orig IN FRAME f-main /* Cod. Origem */
DO:
 {include/zoomvar.i &prog-zoom=eszoom\z01estabel-aux.w
                     &campo=malote.cod-estabel-orig
                     &campozoom=cod-estabel-aux}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME espec.malote.descricao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL espec.malote.descricao V-table-Win
ON LEAVE OF espec.malote.descricao IN FRAME f-main /* Descriá∆o */
DO:
  ASSIGN SELF:SCREEN-VALUE = UPPER(SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME espec.malote.responsavel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL espec.malote.responsavel V-table-Win
ON LEAVE OF espec.malote.responsavel IN FRAME f-main /* Responsavel */
DO:
  ASSIGN SELF:SCREEN-VALUE = UPPER(SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         



malote.cod-estabel-orig:LOAD-MOUSE-POINTER("image/lupa.cur"). 
malote.cod-estabel-dest:LOAD-MOUSE-POINTER("image/lupa.cur"). 

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
    WHEN 'num-malote':U THEN
       {src/adm/template/find-tbl.i
           &TABLE = espec.malote
           &WHERE = "WHERE espec.malote.num-malote eq INTEGER(key-value)"
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
  {src/adm/template/row-list.i "espec.malote"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "espec.malote"}

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
  
  ASSIGN malote.num-contrato:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "9912248848".

  DISABLE {&list-6} WITH FRAME {&FRAME-NAME}.
  
  ASSIGN var-bt-leitora = NO.
  APPLY "choose" TO bt-leitora IN FRAME {&FRAME-NAME}.


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
    
    /*:T Ponha na pi-validate todas as validaá‰es */
    /*:T N∆o gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */
    
    RUN pi-validate. 
    IF RETURN-VALUE = 'ADM-ERROR':U THEN 
       RETURN 'ADM-ERROR':U.
 
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    
    /*:T Todos os assignÔs n∆o feitos pelo assign-record devem ser feitos aqui */  
    /* Code placed here will execute AFTER standard behavior.    */

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
    /*DISABLE {&list-6} WITH FRAME {&FRAME-NAME}.*/
    disable bt-leitora with frame {&frame-name}.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':u ).
 ASSIGN var-bt-leitora = NO.

APPLY "leave" TO malote.cod-estabel-orig in FRAME f-main.
APPLY "leave" TO malote.cod-estabel-dest IN FRAME f-main.

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

    enable bt-leitora with frame {&frame-name}.

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
    
/*:T    Segue um exemplo de validaá∆o de programa */
/*       find tabela where tabela.campo1 = c-variavel and               */
/*                         tabela.campo2 > i-variavel no-lock no-error. */
      
      /*:T Este include deve ser colocado sempre antes do ut-msgs.p */
/*       {include/i-vldprg.i}                                             */
/*       run utp/ut-msgs.p (input "show":U, input 7, input return-value). */
/*       return 'ADM-ERROR':U.                                            */
ASSIGN var-error = "".

IF input frame {&frame-name} malote.num-malote       = 0  or
   input frame {&frame-name} malote.descricao        = "" or
   input frame {&frame-name} malote.responsavel      = "" or
   input frame {&frame-name} malote.num-contrato     = 0  or
   input frame {&frame-name} malote.cod-estabel-orig = 0  or
   input frame {&frame-name} malote.cod-estabel-dest = 0  or
   input frame {&frame-name} malote.cod-barras-orig  = ""  or
   input frame {&frame-name} malote.cod-barras-dest  = ""  THEN DO.
   /*MESSAGE "Formulario est† incompleto"
       VIEW-AS ALERT-BOX ERROR BUTTONS OK.*/
   
   ASSIGN var-error    = "- Formulario est† incompleto" + CHR(10).
END. 

IF SUBSTRING(malote.cod-barras-orig:SCREEN-VALUE IN FRAME {&frame-name},31,5) <> malote.num-malote:SCREEN-VALUE IN FRAME {&frame-name} THEN
   ASSIGN var-error    = var-error + "- Numero do malote diferente do codigo de barras origem" + CHR(10).
IF SUBSTRING(malote.cod-barras-dest:SCREEN-VALUE IN FRAME {&frame-name},31,5) <> malote.num-malote:SCREEN-VALUE IN FRAME {&frame-name} THEN
   ASSIGN var-error    = var-error + "- Numero do malote diferente do codigo de barras destino" + CHR(10).

FIND FIRST estabel-aux WHERE estabel-aux.cod-estabel-aux = INPUT FRAME {&frame-name} malote.cod-estabel-orig NO-LOCK NO-ERROR.
IF AVAIL estabel-aux THEN DO:
    IF estabel-aux.cep <> int(SUBSTRING(malote.cod-barras-orig:screen-value,1,8)) THEN DO:
       APPLY "entry" TO  malote.cod-estabel-orig IN FRAME {&frame-name}.
       ASSIGN var-error    = var-error + "- CEP do CB da ORIGEM diferente do estabel" + CHR(10).
    END.
END.
ELSE
    ASSIGN var-error    = var-error + "- Estabel ORIGEM n∆o foi encontrado" + CHR(10).

FIND FIRST estabel-aux WHERE estabel-aux.cod-estabel-aux = INPUT FRAME {&frame-name} malote.cod-estabel-dest NO-LOCK NO-ERROR.
IF AVAIL estabel-aux THEN DO:
    IF estabel-aux.cep <> int(SUBSTRING(malote.cod-barras-dest:screen-value,1,8)) THEN DO:
       APPLY "entry" TO  malote.cod-estabel-dest IN FRAME {&frame-name}.
       ASSIGN var-error    = var-error + "- CEP do CB do DESTINO diferente do estabel" + CHR(10).
    END.
END.
ELSE
    ASSIGN var-error    = var-error + "- Estabel DESTINO n∆o foi encontrado" + CHR(10).

IF malote.percurso:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> substring(malote.cod-barras-orig:SCREEN-VALUE IN FRAME {&FRAME-NAME},20,6) THEN 
   ASSIGN var-error    = var-error + "- Percurso do CB da ORIGEM diferente do informado" + CHR(10).

IF malote.percurso:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> substring(malote.cod-barras-dest:SCREEN-VALUE IN FRAME {&FRAME-NAME},20,6) THEN 
   ASSIGN var-error    = var-error + "- Percurso do CB do DESTINO diferente do informado" + CHR(10).

IF INPUT FRAME {&frame-name} malote.cod-estabel-orig = INPUT FRAME {&frame-name} malote.cod-estabel-dest THEN
   ASSIGN var-error    = var-error + "- Origem igual Destino" + CHR(10).

IF INPUT FRAME {&frame-name} malote.cod-barras-orig = INPUT FRAME {&frame-name} malote.cod-barras-dest THEN
   ASSIGN var-error    = var-error + "- CB Origem igual CB Destino" + CHR(10).

IF var-error <> "" THEN DO:
   MESSAGE var-error
       VIEW-AS ALERT-BOX ERROR BUTTONS OK.

   RETURN 'ADM-ERROR'.
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
  {src/adm/template/sndkycas.i "num-malote" "espec.malote" "num-malote"}

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
  {src/adm/template/snd-list.i "espec.malote"}

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

