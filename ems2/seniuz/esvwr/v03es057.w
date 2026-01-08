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
{include/i-prgvrs.i V03ES057 2.04.00.000}

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
DEF VAR c-container    AS CHARACTER.
DEF VAR i-tempo-ini    AS INT.
DEF VAR i-hr-saida     LIKE mp-entr-cam.hr-saida.
DEF VAR i-cont         AS INT.
DEF VAR i-aux          AS INT.
DEF VAR i-tipo-mov     AS INT.
DEF VAR de-peso-lido   AS INT.
DEF VAR de-peso-ini    AS DEC.
DEF VAR de-peso-fin    AS DEC.
DEF VAR de-peso-nf     AS DEC.
DEF VAR c-prog-balanca AS CHAR.
DEF VAR c-peso-balanca AS CHAR.
DEF VAR c-comando      AS CHAR.
DEF VAR c-peso         AS CHAR FORMAT "x(10)".
DEF VAR c-tipo         AS CHAR FORMAT "X(20)".
DEF VAR lista-aux      AS CHAR.
DEF VAR l-opc          AS LOG.

DEFINE NEW GLOBAL SHARED VAR c-cod-estabel AS CHAR.
DEF NEW GLOBAL SHARED VAR g-tipo-mov AS INT.
def var h-acomp as handle no-undo.

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
&Scoped-define EXTERNAL-TABLES espec.mp-entr-cam
&Scoped-define FIRST-EXTERNAL-TABLE espec.mp-entr-cam


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR espec.mp-entr-cam.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS espec.mp-entr-cam.nome-transp 
&Scoped-define ENABLED-TABLES espec.mp-entr-cam
&Scoped-define FIRST-ENABLED-TABLE espec.mp-entr-cam
&Scoped-Define ENABLED-OBJECTS RECT-2 rt-key 
&Scoped-Define DISPLAYED-FIELDS espec.mp-entr-cam.responsavel ~
espec.mp-entr-cam.nr-cdr espec.mp-entr-cam.nome-transp ~
espec.mp-entr-cam.dt-saida espec.mp-entr-cam.peso-bruto ~
espec.mp-entr-cam.peso-tara espec.mp-entr-cam.peso-liquido ~
espec.mp-entr-cam.peso-nf 
&Scoped-define DISPLAYED-TABLES espec.mp-entr-cam
&Scoped-define FIRST-DISPLAYED-TABLE espec.mp-entr-cam
&Scoped-Define DISPLAYED-OBJECTS sl-notas-disp sl-notas-emb fi-hr-saida ~
fi-diferenca 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS espec.mp-entr-cam.nome-transp 
&Scoped-define ADM-ASSIGN-FIELDS espec.mp-entr-cam.responsavel ~
espec.mp-entr-cam.nome-transp espec.mp-entr-cam.dt-saida ~
espec.mp-entr-cam.peso-bruto espec.mp-entr-cam.peso-tara ~
espec.mp-entr-cam.peso-liquido fi-hr-saida espec.mp-entr-cam.peso-nf 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
nr-cdr|y|y|espec.mp-entr-cam.nr-cdr
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "nr-cdr",
     Keys-Supplied = "nr-cdr"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-add 
     IMAGE-UP FILE "adeicon\next-au":U
     IMAGE-INSENSITIVE FILE "adeicon\next-ai":U
     LABEL "" 
     SIZE 7 BY 1.

DEFINE BUTTON bt-del 
     IMAGE-UP FILE "adeicon\prev-au":U
     IMAGE-INSENSITIVE FILE "adeicon\prev-ai":U
     LABEL "" 
     SIZE 7 BY 1.

DEFINE BUTTON bt-peso-bruto 
     IMAGE-UP FILE "image/ii-tick2.bmp":U
     LABEL "CapPeso" 
     SIZE 3 BY .88 TOOLTIP "Captura peso da balanáa.".

DEFINE BUTTON bt-peso-tara 
     IMAGE-UP FILE "image/ii-tick2.bmp":U
     LABEL "CapPeso" 
     SIZE 3 BY .88 TOOLTIP "Captura peso da balanáa.".

DEFINE VARIABLE fi-diferenca AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Diferenca (Balanáa/NF)" 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-hr-saida AS CHARACTER FORMAT "xx:xx":U 
     LABEL "Hora da Sa°da" 
     VIEW-AS FILL-IN 
     SIZE 5.86 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 48 BY 9.96.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 86 BY 11.

DEFINE VARIABLE sl-notas-disp AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 14 BY 8.17
     BGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE sl-notas-emb AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 14 BY 8.08
     BGCOLOR 15 FONT 6 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     espec.mp-entr-cam.responsavel AT ROW 1.58 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY .88
     espec.mp-entr-cam.nr-cdr AT ROW 2.58 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.14 BY .88
     espec.mp-entr-cam.nome-transp AT ROW 3.58 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY .88
     espec.mp-entr-cam.dt-saida AT ROW 4.58 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .88
     espec.mp-entr-cam.peso-bruto AT ROW 6.58 COL 18 COLON-ALIGNED
          LABEL "Peso Bruto do Ve°culo"
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .88
     bt-peso-bruto AT ROW 6.58 COL 32.72
     sl-notas-disp AT ROW 2.83 COL 38.43 NO-LABEL
     sl-notas-emb AT ROW 2.92 COL 65.14 NO-LABEL
     bt-add AT ROW 5.54 COL 55
     bt-del AT ROW 7.17 COL 55
     espec.mp-entr-cam.peso-tara AT ROW 7.58 COL 18 COLON-ALIGNED
          LABEL "Peso Tara do Ve°culo"
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .88
     bt-peso-tara AT ROW 7.58 COL 32.72
     espec.mp-entr-cam.peso-liquido AT ROW 8.58 COL 18 COLON-ALIGNED
          LABEL "Peso Bruto da Balanáa"
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .88
     fi-hr-saida AT ROW 5.58 COL 18 COLON-ALIGNED NO-TAB-STOP 
     espec.mp-entr-cam.peso-nf AT ROW 9.58 COL 18 COLON-ALIGNED
          LABEL "Peso das NF(s)"
          VIEW-AS FILL-IN 
          SIZE 8.29 BY .88
     fi-diferenca AT ROW 10.58 COL 18 COLON-ALIGNED
     " Notas Fiscais" VIEW-AS TEXT
          SIZE 10.29 BY .54 AT ROW 1.25 COL 38
     "Embarcadas" VIEW-AS TEXT
          SIZE 8.72 BY .54 AT ROW 2.33 COL 65.29
     "Dispon°veis" VIEW-AS TEXT
          SIZE 9 BY .54 AT ROW 2.25 COL 38.57
     RECT-2 AT ROW 1.54 COL 37
     rt-key AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: espec.mp-entr-cam
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
         HEIGHT             = 11.21
         WIDTH              = 86.29.
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
ASSIGN 
       FRAME f-main:SCROLLABLE       = FALSE
       FRAME f-main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON bt-add IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-del IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-peso-bruto IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-peso-tara IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN espec.mp-entr-cam.dt-saida IN FRAME f-main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi-diferenca IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-hr-saida IN FRAME f-main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN espec.mp-entr-cam.nome-transp IN FRAME f-main
   1 2                                                                  */
/* SETTINGS FOR FILL-IN espec.mp-entr-cam.nr-cdr IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN espec.mp-entr-cam.peso-bruto IN FRAME f-main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN espec.mp-entr-cam.peso-liquido IN FRAME f-main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN espec.mp-entr-cam.peso-nf IN FRAME f-main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN espec.mp-entr-cam.peso-tara IN FRAME f-main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN espec.mp-entr-cam.responsavel IN FRAME f-main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR SELECTION-LIST sl-notas-disp IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR SELECTION-LIST sl-notas-emb IN FRAME f-main
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

&Scoped-define SELF-NAME bt-add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-add V-table-Win
ON CHOOSE OF bt-add IN FRAME f-main
DO:
 IF sl-notas-disp:SCREEN-VALUE <> ? THEN DO:
   DO i-cont = 1 TO NUM-ENTRIES(sl-notas-disp:SCREEN-VALUE,",").
      FOR EACH ped-item-res WHERE
               ped-item-res.cod-estabel = c-cod-estabel                   AND
               ped-item-res.serie       = para-fat.serie-pad              AND
               ped-item-res.nr-nota-fis = INT(ENTRY(i-cont,sl-notas-disp:SCREEN-VALUE,",")) AND
               ped-item-res.lote BEGINS 'R' NO-LOCK. 
          IF CAN-FIND(FIRST ped-item-rom WHERE
                            ped-item-rom.nr-pedcli    = ped-item-res.nr-pedcli    AND
                            ped-item-rom.nome-abrev   = ped-item-res.nome-abrev   AND
                            ped-item-rom.nr-sequencia = ped-item-res.nr-sequencia AND
                            ped-item-rom.marca = '') THEN DO.

              MESSAGE "                 A T E N Ä « O" SKIP
                      "Existem mercadorias que n∆o foram conferidas" SKIP
                      "nesta Nota Fiscal " ENTRY(i-cont,sl-notas-disp:SCREEN-VALUE)
                      " pela EXPEDIÄ«O." SKIP
                      "Favor encaminhar o veiculo para a expediá∆o."
                  VIEW-AS ALERT-BOX ERROR BUTTONS OK.
              IF sl-notas-emb:LIST-ITEMS <> ? THEN
                ASSIGN sl-notas-disp:LIST-ITEMS IN FRAME {&FRAME-NAME} = sl-notas-disp:LIST-ITEMS +
                                                                         sl-notas-emb:LIST-ITEMS. 
                ASSIGN de-peso-nf = 0.
                ASSIGN mp-entr-cam.peso-nf:SCREEN-VALUE                = STRING(de-peso-nf)
                       sl-notas-emb:LIST-ITEMS IN FRAME {&FRAME-NAME}  = ''.
              RETURN NO-APPLY.
          END.
      END.
   END.

   ASSIGN sl-notas-emb:LIST-ITEMS = IF sl-notas-emb:LIST-ITEMS = ?
                                    THEN sl-notas-disp:SCREEN-VALUE + ','
                                    ELSE sl-notas-emb:LIST-ITEMS + sl-notas-disp:SCREEN-VALUE + ','.

   ASSIGN sl-notas-disp:LIST-ITEMS = REPLACE(sl-notas-disp:LIST-ITEMS,sl-notas-disp:SCREEN-VALUE + ',','').

   ASSIGN de-peso-nf = 0.
   DO i-cont = 1 TO NUM-ENTRIES(sl-notas-emb:LIST-ITEMS,",").
      FIND nota-fiscal WHERE
           nota-fiscal.cod-estabel = c-cod-estabel AND
           nota-fiscal.serie       = para-fat.serie-pad AND
           nota-fiscal.nr-nota-fis = ENTRY(i-cont,sl-notas-emb:LIST-ITEMS,",")
           NO-LOCK NO-ERROR.
      IF AVAIL nota-fiscal THEN
         ASSIGN de-peso-nf = de-peso-nf + nota-fiscal.peso-bru-tot.
   END.
   ASSIGN mp-entr-cam.peso-nf:SCREEN-VALUE = STRING(de-peso-nf)
          fi-diferenca:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = 
          STRING(DEC(INPUT FRAME {&FRAME-NAME} mp-entr-cam.peso-liquido) -  
                 DEC(INPUT FRAME {&FRAME-NAME} mp-entr-cam.peso-nf)).
 END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-del
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-del V-table-Win
ON CHOOSE OF bt-del IN FRAME f-main
DO:
   IF sl-notas-emb:SCREEN-VALUE <> ? THEN DO:
      ASSIGN sl-notas-disp:LIST-ITEMS = IF sl-notas-disp:LIST-ITEMS = ?
                                        THEN sl-notas-emb:SCREEN-VALUE + ','
                                        ELSE sl-notas-disp:LIST-ITEMS + sl-notas-emb:SCREEN-VALUE + ','.

      ASSIGN sl-notas-emb:LIST-ITEMS = REPLACE(sl-notas-emb:LIST-ITEMS,sl-notas-emb:SCREEN-VALUE + ",",'').

      ASSIGN de-peso-nf = 0.
      DO i-cont = 1 TO NUM-ENTRIES(sl-notas-emb:LIST-ITEMS,",").
         FIND nota-fiscal WHERE
              nota-fiscal.cod-estabel = c-cod-estabel AND
              nota-fiscal.serie       = para-fat.serie-pad  AND
              nota-fiscal.nr-nota-fis = ENTRY(i-cont,sl-notas-emb:LIST-ITEMS,",")
              NO-LOCK NO-ERROR.
         IF AVAIL nota-fiscal THEN
            ASSIGN de-peso-nf = de-peso-nf + nota-fiscal.peso-liq-tot.
      END.
      ASSIGN mp-entr-cam.peso-nf:SCREEN-VALUE = STRING(de-peso-nf)
             fi-diferenca:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = 
             STRING(DEC(INPUT FRAME {&FRAME-NAME} mp-entr-cam.peso-liquido) -  
                    DEC(INPUT FRAME {&FRAME-NAME} mp-entr-cam.peso-nf)).
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-peso-bruto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-peso-bruto V-table-Win
ON CHOOSE OF bt-peso-bruto IN FRAME f-main /* CapPeso */
DO:
  RUN pi-captura-peso.
  IF de-peso-lido > 0 THEN DO:
     ASSIGN mp-entr-cam.peso-bruto:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(de-peso-lido).
     IF DEC(INPUT FRAME {&FRAME-NAME} mp-entr-cam.peso-bruto) < 
        DEC(INPUT FRAME {&FRAME-NAME} mp-entr-cam.peso-tara) THEN DO:
        MESSAGE 'O Peso Bruto n∆o pode ser MENOR que o Peso Tara.'
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        APPLY 'entry' TO bt-peso-bruto. 
        RETURN NO-APPLY.
     END.
     ASSIGN mp-entr-cam.peso-liquido:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(DEC(INPUT FRAME {&FRAME-NAME} mp-entr-cam.peso-bruto) -
                                                                                  DEC(INPUT FRAME {&FRAME-NAME} mp-entr-cam.peso-tara)).
  END.
  ELSE
     MESSAGE "Peso n∆o foi capturado."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-peso-tara
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-peso-tara V-table-Win
ON CHOOSE OF bt-peso-tara IN FRAME f-main /* CapPeso */
DO:
  RUN pi-captura-peso.
  IF de-peso-lido > 0 THEN 
     ASSIGN mp-entr-cam.peso-tara:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(de-peso-lido).
  ELSE
     MESSAGE "Peso n∆o foi capturado."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME espec.mp-entr-cam.dt-saida
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL espec.mp-entr-cam.dt-saida V-table-Win
ON ENTRY OF espec.mp-entr-cam.dt-saida IN FRAME f-main /* Data de Sa°da */
DO:
   ASSIGN mp-entr-cam.dt-saida:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-hr-saida
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-hr-saida V-table-Win
ON ENTRY OF fi-hr-saida IN FRAME f-main /* Hora da Sa°da */
DO:
   RUN esapi/cv-hora.p (INPUT STRING(TIME,"hh:mm:ss"), OUTPUT i-hr-saida).
   ASSIGN fi-hr-saida:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(i-hr-saida,"hh:mm").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-hr-saida V-table-Win
ON LEAVE OF fi-hr-saida IN FRAME f-main /* Hora da Sa°da */
DO:
   IF INPUT FRAME {&frame-name} fi-hr-saida <> "" THEN DO.
      IF NOT (SUBSTR(INPUT FRAME {&FRAME-NAME} fi-hr-saida,1,1) >= "0" and
              SUBSTR(INPUT FRAME {&FRAME-NAME} fi-hr-saida,1,1) <= "9" AND
              SUBSTR(INPUT FRAME {&FRAME-NAME} fi-hr-saida,2,1) >= "0" AND
              SUBSTR(INPUT FRAME {&FRAME-NAME} fi-hr-saida,2,1) <= "9" AND
              SUBSTR(INPUT FRAME {&FRAME-NAME} fi-hr-saida,3,1) >= "0" and
              SUBSTR(INPUT FRAME {&FRAME-NAME} fi-hr-saida,3,1) <= "9" AND
              SUBSTR(INPUT FRAME {&FRAME-NAME} fi-hr-saida,4,1) >= "0" AND
              SUBSTR(INPUT FRAME {&FRAME-NAME} fi-hr-saida,4,1) <= "9") THEN DO:
         MESSAGE "Hora contÇm caracteres inv†lidos." VIEW-AS ALERT-BOX. 
         RETURN NO-APPLY.
      END.
      ELSE
      IF NOT (SUBSTR(INPUT FRAME {&FRAME-NAME} fi-hr-saida,1,2) >= "00" and
              SUBSTR(INPUT FRAME {&FRAME-NAME} fi-hr-saida,1,2) <= "23" AND
              SUBSTR(INPUT FRAME {&FRAME-NAME} fi-hr-saida,3,2) >= "00" AND
              SUBSTR(INPUT FRAME {&FRAME-NAME} fi-hr-saida,3,2) <= "59")  THEN DO:
         MESSAGE "Hora deve estar entre 00:00 e 23:59." VIEW-AS ALERT-BOX. 
         RETURN NO-APPLY.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME espec.mp-entr-cam.nome-transp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL espec.mp-entr-cam.nome-transp V-table-Win
ON LEAVE OF espec.mp-entr-cam.nome-transp IN FRAME f-main /* Transportador */
DO:
    FIND transporte WHERE 
         transporte.nome-abrev = INPUT FRAME {&FRAME-NAME} mp-entr-cam.nome-transp NO-LOCK NO-ERROR.
    IF NOT AVAIL transporte THEN
       FIND transporte WHERE 
            STRING(transporte.cod-transp) = INPUT FRAME {&FRAME-NAME} mp-entr-cam.nome-transp NO-LOCK NO-ERROR.

    IF AVAIL transporte THEN
        ASSIGN mp-entr-cam.nome-transp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = transporte.nome-abrev.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL espec.mp-entr-cam.nome-transp V-table-Win
ON MOUSE-SELECT-DBLCLICK OF espec.mp-entr-cam.nome-transp IN FRAME f-main /* Transportador */
DO:
    {include/zoomvar.i &prog-zoom = adzoom\z01ad268.w
                     &campo       = mp-entr-cam.nome-transp
                     &campozoom   = nome-abrev}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME espec.mp-entr-cam.peso-bruto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL espec.mp-entr-cam.peso-bruto V-table-Win
ON LEAVE OF espec.mp-entr-cam.peso-bruto IN FRAME f-main /* Peso Bruto do Ve°culo */
DO:
   IF SELF:INPUT-VALUE < INPUT FRAME {&FRAME-NAME} mp-entr-cam.peso-tara THEN DO.
      MESSAGE 'Peso Bruto n∆o pode ser Menor que a TARA...'
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY 'entry' TO SELF.
      RETURN NO-APPLY.
   END.
   ASSIGN mp-entr-cam.peso-liquido:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(INT(INPUT FRAME {&FRAME-NAME} mp-entr-cam.peso-bruto) -
                                                                                INT(INPUT FRAME {&FRAME-NAME} mp-entr-cam.peso-tara)).
   IF DEC(fi-diferenca:SCREEN-VALUE) <> 0 AND  DEC(mp-entr-cam.peso-nf:SCREEN-VALUE) <> 0 THEN
      fi-diferenca:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = 
      STRING(DEC(INPUT FRAME {&FRAME-NAME} mp-entr-cam.peso-liquido) -  
             DEC(INPUT FRAME {&FRAME-NAME} mp-entr-cam.peso-nf)).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
  mp-entr-cam.nome-transp:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.

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
    WHEN 'nr-cdr':U THEN
       {src/adm/template/find-tbl.i
           &TABLE = espec.mp-entr-cam
           &WHERE = "WHERE espec.mp-entr-cam.nr-cdr eq INTEGER(key-value)"
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
  {src/adm/template/row-list.i "espec.mp-entr-cam"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "espec.mp-entr-cam"}

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
  ASSIGN mp-entr-cam.responsavel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = c-seg-usuario
         mp-entr-cam.nome-transp:SENSITIVE IN FRAME {&FRAME-NAME}    = NO
         mp-entr-cam.peso-tara:SENSITIVE IN FRAME {&FRAME-NAME}      = NO.

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
     ASSIGN mp-entr-cam.dt-saida:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY).
     RUN esapi/cv-hora.p (INPUT STRING(TIME,"hh:mm:ss"), OUTPUT i-hr-saida).
     ASSIGN fi-hr-saida:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(i-hr-saida,"hh:mm")
            lista-aux = "".

     FOR EACH nota-fiscal WHERE
              nota-fiscal.dt-emis     >= TODAY - 50              AND  
              nota-fiscal.nome-transp  = mp-entr-cam.nome-transp AND
              (nota-fiscal.dt-embarque = TODAY OR
               nota-fiscal.dt-embarque = ?) NO-LOCK
              BREAK BY nota-fiscal.nr-nota-fis.    
             
         IF nota-fiscal.cod-estabel <> c-cod-estabel THEN NEXT.
            ASSIGN lista-aux = IF lista-aux = ''
                               THEN nota-fiscal.nr-nota-fis + ','
                               ELSE lista-aux + nota-fiscal.nr-nota-fis + ','.
     END.
     ASSIGN sl-notas-disp:LIST-ITEMS = lista-aux. 
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

 IF g-tipo-mov = 1  THEN DO:
    
    /*:T Ponha na pi-validate todas as validaá‰es */
    /*:T N∆o gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */
    
    ASSIGN INPUT FRAME {&FRAME-NAME} fi-hr-saida.

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.
    
    /*:T Todos os assignÔs n∆o feitos pelo assign-record devem ser feitos aqui */  
    /* Code placed here will execute AFTER standard behavior.    */

    IF adm-new-record = NO THEN DO:
       RUN esapi/cv-hora.p (INPUT STRING(INPUT FRAME {&FRAME-NAME} fi-hr-saida,"xx:xx:xx"), OUTPUT i-hr-saida).
       ASSIGN mp-entr-cam.hr-saida     = i-hr-saida
              mp-entr-cam.peso-liquido = mp-entr-cam.peso-bruto - mp-entr-cam.peso-tara
              mp-entr-cam.nf-embarcada = sl-notas-emb:LIST-ITEMS.
       
       /* -- GRAVA DATA SAIDA NA TABELA DE NOTAS FISCAIS --*/
       DO i-cont = 1 TO NUM-ENTRIES(sl-notas-emb:LIST-ITEMS,",").                   
          FIND nota-fiscal WHERE                                                    
               nota-fiscal.cod-estabel = c-cod-estabel AND                          
               nota-fiscal.serie = para-fat.serie-pad AND                                          
               nota-fiscal.nr-nota-fis = ENTRY(i-cont,sl-notas-emb:LIST-ITEMS,",")  
               NO-ERROR.                                                    
          IF AVAIL nota-fiscal AND nota-fiscal.dt-embarque = ? THEN                                                 
             ASSIGN nota-fiscal.dt-embarque = INPUT FRAME {&FRAME-NAME} mp-entr-cam.dt-saida.                                        
       END.                                                                         
       
       RUN pi-impr-ticket. 
       ASSIGN sl-notas-disp:LIST-ITEMS IN FRAME {&FRAME-NAME} = '' 
              sl-notas-emb:LIST-ITEMS IN FRAME {&FRAME-NAME}  = ''.
    END.
 END.
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
    ASSIGN mp-entr-cam.nome-transp:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           mp-entr-cam.peso-tara:SENSITIVE IN FRAME {&FRAME-NAME}   = NO
           mp-entr-cam.dt-saida:SENSITIVE IN FRAME {&FRAME-NAME}    = NO
           fi-hr-saida:SENSITIVE IN FRAME {&FRAME-NAME}             = NO
           mp-entr-cam.peso-bruto:SENSITIVE IN FRAME {&FRAME-NAME}  = NO.
    IF adm-new-record = NO THEN DO: 
       ASSIGN sl-notas-disp:LIST-ITEMS IN FRAME {&FRAME-NAME} = '' /* Limpando Lista de Items */
              sl-notas-emb:LIST-ITEMS IN FRAME {&FRAME-NAME}  = '' /* Limpando Lista de Items */ 
              sl-notas-disp:SENSITIVE IN FRAME {&FRAME-NAME}  = NO
              sl-notas-emb:SENSITIVE IN FRAME {&FRAME-NAME}   = NO
              bt-add:SENSITIVE IN FRAME {&FRAME-NAME}         = NO
              bt-del:SENSITIVE IN FRAME {&FRAME-NAME}         = NO.
    END.

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
    
    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ).
    
    /* Code placed here will execute AFTER standard behavior.    */
    IF g-tipo-mov = 1 THEN DO:
       FIND FIRST mp-param NO-LOCK NO-ERROR.
       IF AVAIL mp-entr-cam THEN DO.
          ASSIGN fi-hr-saida:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(mp-entr-cam.hr-saida,"HH:MM")
                 sl-notas-disp:LIST-ITEMS IN FRAME {&FRAME-NAME} = ''
                 sl-notas-emb:LIST-ITEMS IN FRAME {&FRAME-NAME} = mp-entr-cam.nf-embarcada.
          IF mp-entr-cam.dt-saida <> ?  THEN
             ASSIGN mp-entr-cam.peso-liquido:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 
                    STRING(DEC(INPUT FRAME {&FRAME-NAME} mp-entr-cam.peso-bruto) -
                           DEC(INPUT FRAME {&FRAME-NAME} mp-entr-cam.peso-tara))

                    fi-diferenca:SCREEN-VALUE IN FRAME {&FRAME-NAME}             =
                       STRING(DEC(INPUT FRAME {&FRAME-NAME} mp-entr-cam.peso-nf) -  
                             DEC(INPUT FRAME {&FRAME-NAME} mp-entr-cam.peso-liquido)).   
       END.
    END.
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
    IF adm-new-record = YES THEN
       ASSIGN sl-notas-disp:LIST-ITEMS IN FRAME {&FRAME-NAME} = ''  /* Limpando Lista de Items */
              sl-notas-emb:LIST-ITEMS IN FRAME {&FRAME-NAME}  = ''. /* Limpando Lista de Items */
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize V-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  FIND FIRST para-fat NO-LOCK NO-ERROR.

  RUN get-link-handle IN adm-broker-hdl ( INPUT THIS-PROCEDURE,
                                          INPUT "CONTAINER",
                                          OUTPUT c-container). 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-captura-peso V-table-Win 
PROCEDURE pi-captura-peso :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 
 ASSIGN c-prog-balanca = mp-param.dir-balanca + "\balanca.exe"
        c-peso-balanca = mp-param.dir-balanca + "\peso.txt"
        c-comando      = c-prog-balanca + " " + c-peso-balanca.
 IF SEARCH(c-prog-balanca) <> ? THEN DO:
    RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
    {utp/ut-liter.i Capturando_Peso *}
    RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).
    RUN pi-acompanhar IN h-acomp (INPUT "Por favor, aguarde...").
    
    OS-DELETE SILENT VALUE(c-peso-balanca).
    ASSIGN i-tempo-ini = TIME.
    DO WHILE (TIME - i-tempo-ini) < 30 AND SEARCH(c-peso-balanca) = ?:
       OS-COMMAND SILENT VALUE(c-comando).
    END.
    RUN pi-finalizar in h-acomp.
 
    IF SEARCH(c-peso-balanca) <> ? THEN DO:
       INPUT FROM value(c-peso-balanca) NO-ECHO.
       REPEAT:
          SET c-peso.
       END.
       INPUT CLOSE.
  
       IF INT(SUBSTR(c-peso,6,5)) <> 0 THEN
          ASSIGN de-peso-lido = INT(SUBSTR(c-peso,6,5)).
       ELSE
          ASSIGN de-peso-lido = 0.
    END.
    ELSE
        ASSIGN de-peso-lido = 0.
 END.
 ELSE 
    MESSAGE "Programa de captura de peso n∆o foi encontrado."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-desabilita-campos V-table-Win 
PROCEDURE pi-desabilita-campos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN mp-entr-cam.responsavel:SENSITIVE IN FRAME {&FRAME-NAME}  = NO
           mp-entr-cam.nome-transp:SENSITIVE IN FRAME {&FRAME-NAME}  = NO
           mp-entr-cam.nr-cdr:SENSITIVE IN FRAME {&FRAME-NAME}       = NO
           mp-entr-cam.dt-saida:SENSITIVE IN FRAME {&FRAME-NAME}     = NO
           fi-hr-saida:SENSITIVE IN FRAME {&FRAME-NAME}              = NO
           mp-entr-cam.peso-nf:SENSITIVE IN FRAME {&FRAME-NAME}      = NO
           mp-entr-cam.peso-liquido:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           mp-entr-cam.peso-bruto:SENSITIVE IN FRAME {&FRAME-NAME}   = NO
           mp-entr-cam.peso-tara:SENSITIVE IN FRAME {&FRAME-NAME}    = NO
           bt-peso-tara:SENSITIVE IN FRAME {&FRAME-NAME}             = NO
           bt-peso-bruto:SENSITIVE IN FRAME {&FRAME-NAME}            = NO
           sl-notas-disp:SENSITIVE IN FRAME {&FRAME-NAME}            = NO
           sl-notas-emb:SENSITIVE IN FRAME {&FRAME-NAME}             = NO
           bt-add:SENSITIVE IN FRAME {&FRAME-NAME}                   = NO
           bt-del:SENSITIVE IN FRAME {&FRAME-NAME}                   = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-habilita-campos V-table-Win 
PROCEDURE pi-habilita-campos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER p-novo-registro AS log.
  IF NOT AVAIL mp-param THEN
     FIND FIRST mp-param NO-LOCK NO-ERROR.

  IF p-novo-registro = YES THEN DO:
     ASSIGN mp-entr-cam.nome-transp:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
     IF mp-param.mod-op-balanca = YES  THEN  /* Balanáa Manual */
        ASSIGN mp-entr-cam.peso-tara:SENSITIVE IN FRAME {&FRAME-NAME} = YES
               bt-peso-tara:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
     ELSE DO:
        ASSIGN mp-entr-cam.peso-tara:SENSITIVE IN FRAME {&FRAME-NAME} = NO
               bt-peso-tara:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
        APPLY 'choose' TO bt-peso-tara.
     END.
  END.
  ELSE DO:
     ASSIGN mp-entr-cam.dt-saida:SENSITIVE IN FRAME {&FRAME-NAME}   = YES
            fi-hr-saida:SENSITIVE IN FRAME {&FRAME-NAME}            = YES
            sl-notas-disp:SENSITIVE IN FRAME {&FRAME-NAME}          = YES
            sl-notas-emb:SENSITIVE IN FRAME {&FRAME-NAME}           = YES
            bt-add:SENSITIVE IN FRAME {&FRAME-NAME}                 = YES
            bt-del:SENSITIVE IN FRAME {&FRAME-NAME}                 = YES.
     IF mp-param.mod-op-balanca = YES  THEN DO: /* Balanáa Manual */
        ASSIGN mp-entr-cam.peso-bruto:SENSITIVE IN FRAME {&FRAME-NAME}    = YES
               bt-peso-bruto:SENSITIVE IN FRAME {&FRAME-NAME}             = NO.
        APPLY 'entry' TO mp-entr-cam.peso-bruto.
     END.
     ELSE DO:
        ASSIGN mp-entr-cam.peso-bruto:SENSITIVE IN FRAME {&FRAME-NAME}    = NO
               bt-peso-bruto:SENSITIVE IN FRAME {&FRAME-NAME}             = YES.
        APPLY 'choose' TO bt-peso-bruto.
        APPLY 'entry' TO sl-notas-disp.
     END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-impr-ticket V-table-Win 
PROCEDURE pi-impr-ticket :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    IF NOT AVAIL mp-param THEN
       FIND FIRST mp-param NO-LOCK NO-ERROR.

    FIND transporte WHERE transporte.nome-abrev = mp-entr-cam.nome-transp NO-LOCK NO-ERROR.

    {esinc/i-dsrb.i mp-entr-cam.tipo-mov mp-entr-cam.tipo-mov c-tipo} 

    OUTPUT TO VALUE(mp-param.imp-balanca).
    
    PUT "TEAR TEXTIL INDUSTRIA E COMERCIO LTDA."  AT  1
        "DATA: "                                  AT 42
        STRING(TODAY,"99/99/9999") FORMAT "X(10)" AT 49
        "HORA: "                                  AT 62
        STRING(TIME,"hh:mm:ss")                   AT 69
        SKIP(1).
    
    PUT "RELATORIO DE PESAGEM DE VEICULOS" AT 25 SKIP(1).
    
    PUT "TIPO MOVIMENTACAO:"                     AT  1
         caps(c-tipo) FORMAT "x(20)"             AT 20.
    PUT "TRANSPORTADORA...:"                     AT  1
        transporte.nome                          AT 20.
    PUT "PLACA............:"                     AT  1
        mp-entr-cam.placa                        AT 20
        "DATA DA ENTRADA..:"                     AT  1
        mp-entr-cam.dt-entrada                   AT 20
        "HORA DA ENTRADA..:"                     AT  1
        STRING(mp-entr-cam.hr-entrada,"HH:MM")   AT 20 
        "DATA DA SAIDA....:"                     AT  1
        mp-entr-cam.dt-saida                     AT 20
        "HORA DA SAIDA....:"                     AT  1
        STRING(mp-entr-cam.hr-saida,"HH:MM")     AT 20
        "PESO DA BRUTO....:"                     AT  1
        mp-entr-cam.peso-bruto                   AT 20
        "PESO TARA........:"                     AT  1
        mp-entr-cam.peso-tara                    AT 20
        "PESO LIQUIDO.....:"                     AT  1
        mp-entr-cam.peso-liquido                 AT 20
        "PESO DA NFS......:"                     AT  1
        mp-entr-cam.peso-nf                      AT 20
        "DIFERENCA........:"                     AT  1 
        STRING(mp-entr-cam.peso-liquido - mp-entr-cam.peso-nf, "->>>,>>9.99") 
                                              AT 19 FORMAT "x(11)" SKIP(1).
    
    PUT "N O T A S   F I S C A I S"    AT  1
        FILL("-",80) FORMAT "x(80)"    AT  1 SKIP.
    
    IF mp-entr-cam.nf-embarcada <> ? THEN
       PUT SUBSTR(mp-entr-cam.nf-embarcada,1,80)   FORMAT "x(80)" at 1
           SUBSTR(mp-entr-cam.nf-embarcada,81,80)  FORMAT "x(80)" at 1
           SUBSTR(mp-entr-cam.nf-embarcada,161,80) FORMAT "x(80)" at 1
           SUBSTR(mp-entr-cam.nf-embarcada,241,80) FORMAT "x(80)" at 1
           SKIP(10).
    ELSE
       PUT SKIP(14).
    
    OUTPUT CLOSE.

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
    
 IF g-tipo-mov = 1 THEN DO: /* Carga */ 
    if adm-new-record = yes THEN DO: /* NOVO REGISTRO */
       FIND transporte WHERE 
            transporte.nome-abrev = INPUT FRAME {&FRAME-NAME} mp-entr-cam.nome-transp NO-LOCK NO-ERROR.
       IF NOT AVAIL transporte THEN
          FIND transporte WHERE 
               STRING(transporte.cod-transp) = INPUT FRAME {&FRAME-NAME} mp-entr-cam.nome-transp NO-LOCK NO-ERROR.
       IF NOT AVAIL transporte THEN DO:
           MESSAGE "Transportadora n∆o cadastrada." VIEW-AS ALERT-BOX.             
           APPLY 'entry' TO mp-entr-cam.nome-transp.
           RETURN 'ADM-ERROR':U.                                                    
       END.
       IF INPUT FRAME {&frame-name} mp-entr-cam.peso-tara = 0 THEN DO:
            MESSAGE "O peso TARA n∆o pode ser zero."  VIEW-AS ALERT-BOX. 
            APPLY 'entry' TO mp-entr-cam.peso-tara.
            return 'ADM-ERROR':U.
       END. 
    END.
    ELSE DO: /* ALTERAÄ«O DO REGISTRO */
       /*
       IF sl-notas-emb:LIST-ITEMS IN FRAME {&FRAME-NAME} = ?  AND 
          sl-notas-disp:LIST-ITEMS IN FRAME {&FRAME-NAME} <> ? THEN DO:
          MESSAGE "A saida deste veiculo, n∆o foi efetuada, porque" SKIP
                  "n∆o existe nota fiscal embarcada."
                   VIEW-AS ALERT-BOX ERROR BUTTONS OK.
          APPLY 'entry' TO bt-add.
          return 'ADM-ERROR':U.
       END.
       */
       IF INPUT FRAME {&frame-name} mp-entr-cam.dt-saida = ? THEN DO:
          MESSAGE "A data de saida n∆o Ç v†lida." VIEW-AS ALERT-BOX. 
          APPLY 'entry' TO mp-entr-cam.dt-saida.
          return 'ADM-ERROR':U.
       END.
       IF INPUT FRAME {&frame-name} mp-entr-cam.dt-saida > TODAY THEN DO:
          MESSAGE "A data de sa°da n∆o pode ser maior que a data de hoje." VIEW-AS ALERT-BOX. 
          APPLY 'entry' TO mp-entr-cam.dt-saida.
          return 'ADM-ERROR':U.
       END.     
       IF INPUT FRAME {&frame-name} mp-entr-cam.dt-saida < mp-entr-cam.dt-entrada THEN DO:
          MESSAGE "A data de sa°da n∆o pode ser Menor que a data de entrada: " SKIP 
                   mp-entr-cam.dt-entrada VIEW-AS ALERT-BOX. 
          APPLY 'entry' TO mp-entr-cam.dt-saida.
          return 'ADM-ERROR':U.
       END.     
       IF INPUT FRAME {&frame-name} mp-entr-cam.dt-saida = mp-entr-cam.dt-entrada THEN
          IF fi-hr-saida:SCREEN-VALUE IN FRAME {&FRAME-NAME} < STRING(mp-entr-cam.hr-entrada,"HH:MM") THEN DO:
             MESSAGE "A hora de sa°da n∆o pode ser menor que a hora de entrada." VIEW-AS ALERT-BOX. 
             APPLY 'entry' TO fi-hr-saida.
             return 'ADM-ERROR':U.
          END.     
       IF length(fi-hr-saida:SCREEN-VALUE) <> 5 THEN DO:
          MESSAGE "A hora de saida n∆o Ç valida." VIEW-AS ALERT-BOX. 
          APPLY 'entry' TO fi-hr-saida.
          return 'ADM-ERROR':U.
       END.
       IF INPUT FRAME {&frame-name} mp-entr-cam.peso-bruto <= INPUT FRAME {&frame-name} mp-entr-cam.peso-tara OR 
          INPUT FRAME {&frame-name} mp-entr-cam.peso-bruto = 0 THEN DO:
          MESSAGE "O peso bruto deve ser Maior que o peso tara e n∆o pode ser zero." VIEW-AS ALERT-BOX. 
          APPLY 'entry' TO mp-entr-cam.peso-bruto.
          return 'ADM-ERROR':U.
       END.
       IF ABS(INPUT FRAME {&frame-name} mp-entr-cam.peso-liquido -
              INPUT FRAME {&FRAME-NAME} mp-entr-cam.peso-nf) >  mp-param.peso-toler-bal THEN DO:
          MESSAGE "A Diferenáa entre o peso da(s) NF(s) e o do Veiculo," SKIP
                  "est† fora do limite Tolerado." SKIP
                   "Deseja, mesmo assim GRAVAR a sa°da do Veiculo"
                   VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO UPDATE l-opc.
          IF l-opc = NO THEN DO:
             APPLY 'entry' TO bt-del.
             return 'ADM-ERROR':U.
          END.
       END.
    END.
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
  {src/adm/template/sndkycas.i "nr-cdr" "espec.mp-entr-cam" "nr-cdr"}

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
  {src/adm/template/snd-list.i "espec.mp-entr-cam"}

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

