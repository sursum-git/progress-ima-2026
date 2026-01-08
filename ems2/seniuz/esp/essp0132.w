&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mgcad            PROGRESS
*/
&Scoped-define WINDOW-NAME w-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-digita 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESSP0132 2.04.00.000}
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: 
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---  
*/

DEFINE TEMP-TABLE tt-mp-fardo 
       FIELD nr-fardo       LIKE mp-fardo.nr-fardo
       FIELD padrao         LIKE mp-fardo.padrao
       FIELD cd-coloracao   AS CHAR FORMAT "x(2)" 
       FIELD desc-coloracao AS CHAR FORMAT "x(20)" 
       FIELD cd-tipo        AS CHAR FORMAT "x(2)"
       FIELD desc-tipo      AS CHAR FORMAT "x(15)"
       FIELD cd-compr       LIKE mp-classificacao.codigo
       FIELD letra          LIKE mp-classificacao.letra-id
       INDEX indice1 nr-fardo.

DEF VAR i-ct AS INT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Digitacao
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-fardos

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-mp-fardo

/* Definitions for BROWSE br-fardos                                     */
&Scoped-define FIELDS-IN-QUERY-br-fardos tt-mp-fardo.nr-fardo tt-mp-fardo.padrao tt-mp-fardo.cd-tipo tt-mp-fardo.desc-tipo tt-mp-fardo.cd-coloracao tt-mp-fardo.desc-coloracao tt-mp-fardo.cd-compr tt-mp-fardo.letra   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-fardos tt-mp-fardo.cd-tipo   tt-mp-fardo.cd-coloracao   tt-mp-fardo.cd-compr   
&Scoped-define ENABLED-TABLES-IN-QUERY-br-fardos tt-mp-fardo
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br-fardos tt-mp-fardo
&Scoped-define SELF-NAME br-fardos
&Scoped-define QUERY-STRING-br-fardos FOR EACH tt-mp-fardo NO-LOCK
&Scoped-define OPEN-QUERY-br-fardos OPEN QUERY {&SELF-NAME} FOR EACH tt-mp-fardo NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-fardos tt-mp-fardo
&Scoped-define FIRST-TABLE-IN-QUERY-br-fardos tt-mp-fardo


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br-fardos}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi-nota-fis fi-cod-fornec br-fardos bt-ajuda ~
bt-ok bt-cancelar RECT-1 RECT-44 RECT-7 
&Scoped-Define DISPLAYED-OBJECTS fi-nota-fis fi-cod-fornec fi-nome-emit ~
fi-dt-recebimento fi-procedencia fi-qt-fardo fi-nr-fardo fi-cd-tipo ~
fi-cd-coloracao fi-codigo 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-4 bt-exp-comprimento fi-nr-fardo fi-cd-tipo ~
fi-cd-coloracao fi-codigo bt-exp-tipo bt-exp-tonalidade 
&Scoped-define List-6 fi-nota-fis fi-cod-fornec 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-digita AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Cancelar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-exp-comprimento 
     IMAGE-UP FILE "image/im-todos.bmp":U NO-FOCUS
     LABEL "bt exp tonalidade 2" 
     SIZE 4 BY 1.21 TOOLTIP "Exporta o Comprimento da Fibra Algodao para todos Fardos.".

DEFINE BUTTON bt-exp-tipo 
     IMAGE-UP FILE "image/im-carg.bmp":U NO-FOCUS
     LABEL "Button 2" 
     SIZE 4 BY 1.21 TOOLTIP "Exporta o Tipo da Fibra para todos Fardos".

DEFINE BUTTON bt-exp-tonalidade 
     IMAGE-UP FILE "image/im-carga.bmp":U NO-FOCUS
     LABEL "Button 3" 
     SIZE 4 BY 1.21 TOOLTIP "Exporta a Tonalidade da Fibra Algodao para todos Fardos".

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "OK" 
     SIZE 10 BY 1 TOOLTIP "Grava Todas as Alteraá‰es efetuadas para a Nota".

DEFINE VARIABLE fi-cd-coloracao AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Tonalidade" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cd-tipo AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Tipo" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-fornec AS INTEGER FORMAT ">>>>>>>>9" INITIAL 0 
     LABEL "Fornecedor":R12 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE fi-codigo AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Comprimento" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 TOOLTIP "Comprimento da Fibra de Algod∆o" NO-UNDO.

DEFINE VARIABLE fi-dt-recebimento AS DATE FORMAT "99/99/9999":U 
     LABEL "Data Recebimento" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nome-emit AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 46.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nota-fis AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "Nota Fiscal" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nr-fardo AS INTEGER FORMAT "->>>>>>>>9":U INITIAL 0 
     LABEL "Nß Fardo" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE VARIABLE fi-procedencia AS CHARACTER FORMAT "X(20)":U 
     LABEL "Procedencia" 
     VIEW-AS FILL-IN 
     SIZE 21 BY .88 NO-UNDO.

DEFINE VARIABLE fi-qt-fardo AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Qtd Fardos" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 80 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-44
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 80 BY 1.75.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 80 BY 4.5.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-fardos FOR 
      tt-mp-fardo SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-fardos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-fardos w-digita _FREEFORM
  QUERY br-fardos DISPLAY
      tt-mp-fardo.nr-fardo        COLUMN-LABEL "Fardo" COLUMN-FONT 9 WIDTH 8
      tt-mp-fardo.padrao      
      tt-mp-fardo.cd-tipo         COLUMN-LABEL "Cod Tipo" 
      tt-mp-fardo.desc-tipo       COLUMN-LABEL "Tipo" WIDTH 8
      tt-mp-fardo.cd-coloracao    COLUMN-LABEL "Cod Coloracao"
      tt-mp-fardo.desc-coloracao  COLUMN-LABEL "Tonalidade"
      tt-mp-fardo.cd-compr        COLUMN-LABEL "Cod Compr"
      tt-mp-fardo.letra           COLUMN-LABEL "Letra"
ENABLE 
    tt-mp-fardo.cd-tipo
    tt-mp-fardo.cd-coloracao
    tt-mp-fardo.cd-compr
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 79 BY 10.25
         FONT 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi-nota-fis AT ROW 1.25 COL 14 COLON-ALIGNED
     bt-exp-comprimento AT ROW 6 COL 75.57
     fi-cod-fornec AT ROW 2.25 COL 14 COLON-ALIGNED
     fi-nome-emit AT ROW 2.25 COL 21.43 COLON-ALIGNED NO-LABEL
     fi-dt-recebimento AT ROW 3.25 COL 14 COLON-ALIGNED
     fi-procedencia AT ROW 4.25 COL 14 COLON-ALIGNED
     fi-qt-fardo AT ROW 4.25 COL 63 COLON-ALIGNED
     fi-nr-fardo AT ROW 6.17 COL 6.57 COLON-ALIGNED
     fi-cd-tipo AT ROW 6.17 COL 24.72 COLON-ALIGNED
     fi-cd-coloracao AT ROW 6.17 COL 45.43 COLON-ALIGNED
     fi-codigo AT ROW 6.17 COL 68.86 COLON-ALIGNED
     br-fardos AT ROW 7.75 COL 2
     bt-ajuda AT ROW 18.42 COL 70.14
     bt-ok AT ROW 18.46 COL 2.14
     bt-cancelar AT ROW 18.46 COL 13.14
     bt-exp-tipo AT ROW 6 COL 31.29
     bt-exp-tonalidade AT ROW 6 COL 51.86
     RECT-1 AT ROW 18.25 COL 1
     RECT-44 AT ROW 5.75 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 18.83
         FONT 1
         DEFAULT-BUTTON bt-ok.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Digitacao
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
   Design Page: 1
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-digita ASSIGN
         HIDDEN             = YES
         TITLE              = "Digitaá∆o da Analise Visual"
         HEIGHT             = 18.83
         WIDTH              = 80
         MAX-HEIGHT         = 22
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 22
         VIRTUAL-WIDTH      = 114.29
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-digita 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-digit.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-digita
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
                                                                        */
/* BROWSE-TAB br-fardos fi-codigo F-Main */
/* SETTINGS FOR BUTTON bt-exp-comprimento IN FRAME F-Main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR BUTTON bt-exp-tipo IN FRAME F-Main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR BUTTON bt-exp-tonalidade IN FRAME F-Main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-cd-coloracao IN FRAME F-Main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-cd-tipo IN FRAME F-Main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-cod-fornec IN FRAME F-Main
   6                                                                    */
/* SETTINGS FOR FILL-IN fi-codigo IN FRAME F-Main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-dt-recebimento IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nome-emit IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nota-fis IN FRAME F-Main
   6                                                                    */
/* SETTINGS FOR FILL-IN fi-nr-fardo IN FRAME F-Main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-procedencia IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qt-fardo IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-digita)
THEN w-digita:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-fardos
/* Query rebuild information for BROWSE br-fardos
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-mp-fardo NO-LOCK.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-fardos */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON END-ERROR OF w-digita /* Digitaá∆o da Analise Visual */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON WINDOW-CLOSE OF w-digita /* Digitaá∆o da Analise Visual */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-fardos
&Scoped-define SELF-NAME br-fardos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-fardos w-digita
ON RETURN OF br-fardos IN FRAME F-Main
ANYWHERE
DO:
    APPLY 'TAB' TO SELF.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda w-digita
ON CHOOSE OF bt-ajuda IN FRAME F-Main /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar w-digita
ON CHOOSE OF bt-cancelar IN FRAME F-Main /* Cancelar */
DO:
  apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-exp-comprimento
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-exp-comprimento w-digita
ON CHOOSE OF bt-exp-comprimento IN FRAME F-Main /* bt exp tonalidade 2 */
DO:
  FIND mp-classificacao WHERE
       mp-classificacao.codigo = INPUT FRAME {&FRAME-NAME} fi-codigo NO-LOCK NO-ERROR.

  IF AVAIL mp-classificacao THEN DO.
     FOR EACH tt-mp-fardo.
         ASSIGN tt-mp-fardo.cd-compr = INPUT FRAME {&FRAME-NAME} fi-codigo
                tt-mp-fardo.letra    = mp-classificacao.letra-id.
     END.
  END.
  {&OPEN-QUERY-br-fardos}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-exp-tipo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-exp-tipo w-digita
ON CHOOSE OF bt-exp-tipo IN FRAME F-Main /* Button 2 */
DO:
  FIND mp-tipo WHERE
       mp-tipo.codigo = INPUT FRAME {&FRAME-NAME} fi-cd-tipo NO-LOCK NO-ERROR.

  IF AVAIL mp-tipo THEN DO.
     FOR EACH tt-mp-fardo.
         ASSIGN tt-mp-fardo.cd-tipo = INPUT FRAME {&FRAME-NAME} fi-cd-tipo
                tt-mp-fardo.desc-tipo = mp-tipo.tipo.
     END.
  END.
  {&OPEN-QUERY-br-fardos}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-exp-tonalidade
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-exp-tonalidade w-digita
ON CHOOSE OF bt-exp-tonalidade IN FRAME F-Main /* Button 3 */
DO:
    FIND mp-coloracao WHERE
         mp-coloracao.codigo = INPUT FRAME {&FRAME-NAME} fi-cd-coloracao
         NO-LOCK NO-ERROR.

   IF AVAIL mp-coloracao THEN DO.
       FOR EACH tt-mp-fardo.
           ASSIGN tt-mp-fardo.cd-coloracao = INPUT FRAME {&FRAME-NAME} fi-cd-coloracao
                  tt-mp-fardo.desc-coloracao = mp-coloracao.tonalidade.
       END.
   END.
   {&OPEN-QUERY-br-fardos}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-digita
ON CHOOSE OF bt-ok IN FRAME F-Main /* OK */
DO:
   FOR EACH tt-mp-fardo.
       FIND mp-fardo OF tt-mp-fardo NO-ERROR.
       ASSIGN mp-fardo.cd-coloracao = INT(tt-mp-fardo.cd-coloracao)
              mp-fardo.cd-tipo      = INT(tt-mp-fardo.cd-tipo)
              mp-fardo.cd-compr     = tt-mp-fardo.cd-compr
              mp-fardo.letra        = tt-mp-fardo.letra
              mp-fardo.situacao     = 3.
   END.
   APPLY "close":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cd-coloracao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cd-coloracao w-digita
ON LEAVE OF fi-cd-coloracao IN FRAME F-Main /* Tonalidade */
DO:
  FIND mp-coloracao WHERE
       mp-coloracao.codigo = INPUT FRAME {&FRAME-NAME} fi-cd-coloracao
       NO-LOCK NO-ERROR.
  IF NOT AVAIL mp-coloracao THEN DO.
     MESSAGE 'Coloraá∆o n∆o Cadastrada...'
         VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     APPLY 'entry' TO fi-cd-coloracao.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cd-coloracao w-digita
ON MOUSE-SELECT-DBLCLICK OF fi-cd-coloracao IN FRAME F-Main /* Tonalidade */
DO:
  {include/zoomvar.i &prog-zoom=eszoom/z01es054.w
                     &campo=fi-cd-coloracao
                     &campozoom=codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cd-tipo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cd-tipo w-digita
ON LEAVE OF fi-cd-tipo IN FRAME F-Main /* Tipo */
DO:
  FIND mp-tipo WHERE
       mp-tipo.codigo = INPUT FRAME {&FRAME-NAME} fi-cd-tipo NO-LOCK NO-ERROR.
  IF NOT AVAIL mp-tipo THEN DO.
     MESSAGE 'Tipo n∆o Cadastrado...'
         VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     APPLY 'entry' TO SELF.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cd-tipo w-digita
ON MOUSE-SELECT-DBLCLICK OF fi-cd-tipo IN FRAME F-Main /* Tipo */
DO:
  {include/zoomvar.i &prog-zoom=eszoom/z01es055.w
                     &campo=fi-cd-tipo
                     &campozoom=codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-fornec
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-fornec w-digita
ON ENTRY OF fi-cod-fornec IN FRAME F-Main /* Fornecedor */
DO:
  FIND emitente WHERE
       emitente.cod-emite = SELF:INPUT-VALUE NO-LOCK NO-ERROR.
  IF AVAIL emitente THEN 
     ASSIGN fi-nome-emit:SCREEN-VALUE = emitente.nome-emit.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-fornec w-digita
ON LEAVE OF fi-cod-fornec IN FRAME F-Main /* Fornecedor */
DO:
   FIND emitente WHERE
        emitente.cod-emite = SELF:INPUT-VALUE NO-LOCK NO-ERROR.
   IF NOT AVAIL emitente THEN DO.
      MESSAGE "Fornecedor n∆o Cadastrado..."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'entry' TO SELF.
      RETURN NO-APPLY.
   END.
   ASSIGN fi-nome-emit:SCREEN-VALUE = emitente.nome-emit.

   FIND mp-entr-mat WHERE
        mp-entr-mat.nro-docto = fi-nota-fis:INPUT-VALUE AND
        mp-entr-mat.cod-emit  = SELF:INPUT-VALUE
        NO-LOCK NO-ERROR.

   IF NOT AVAIL mp-entr-mat THEN DO.
      MESSAGE 'Nota Fiscal n∆o Cadastrada....'
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY 'ENTRY' TO SELF.
      RETURN NO-APPLY.
   END.

   DO i-ct = 1 TO EXTENT(mp-entr-mat.qtd-fardos).
      ASSIGN fi-qt-fardo = fi-qt-fardo + mp-entr-mat.qtd-fardos[i-ct].
   END.

   ASSIGN fi-dt-recebimento:SCREEN-VALUE = STRING(mp-entr-mat.dt-recebimento)
          fi-procedencia:SCREEN-VALUE = mp-entr-mat.procedencia
          fi-qt-fardo:SCREEN-VALUE = STRING(fi-qt-fardo).

   FOR EACH mp-fardo OF mp-entr-mat NO-LOCK.

       FIND mp-coloracao  WHERE
            mp-coloracao.codigo = mp-fardo.cd-coloracao NO-LOCK NO-ERROR.

       FIND mp-tipo  WHERE
            mp-tipo.codigo = mp-fardo.cd-tipo NO-LOCK NO-ERROR.

       CREATE tt-mp-fardo.
       ASSIGN tt-mp-fardo.nr-fardo       = mp-fardo.nr-fardo       
              tt-mp-fardo.padrao         = mp-fardo.padrao         
              tt-mp-fardo.cd-coloracao   = IF mp-fardo.cd-coloracao <> ?
                                           THEN STRING(mp-fardo.cd-coloracao,">>") ELSE ''
              tt-mp-fardo.desc-coloracao = IF AVAIL mp-coloracao 
                                           THEN mp-coloracao.tonalidade ELSE ''
              tt-mp-fardo.cd-tipo        =  IF mp-fardo.cd-tipo <> ?
                                           THEN STRING(mp-fardo.cd-tipo,">>") ELSE ''
              tt-mp-fardo.desc-tipo      = IF AVAIL mp-tipo
                                           THEN mp-tipo.tipo ELSE ''
              tt-mp-fardo.cd-compr       = mp-fardo.cd-compr
              tt-mp-fardo.letra          = mp-fardo.letra.
   END.
   {&OPEN-QUERY-br-fardos}

   ENABLE {&list-4} WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-codigo w-digita
ON LEAVE OF fi-codigo IN FRAME F-Main /* Comprimento */
DO:
  FIND mp-coloracao WHERE
       mp-coloracao.codigo = INPUT FRAME {&FRAME-NAME} fi-cd-coloracao
       NO-LOCK NO-ERROR.
  IF NOT AVAIL mp-coloracao THEN DO.
     MESSAGE 'Coloraá∆o n∆o Cadastrada...'
         VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     APPLY 'entry' TO fi-cd-coloracao.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-codigo w-digita
ON MOUSE-SELECT-DBLCLICK OF fi-codigo IN FRAME F-Main /* Comprimento */
DO:
  {include/zoomvar.i &prog-zoom=eszoom/z01es056.w
                     &campo=fi-codigo
                     &campozoom=codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nota-fis
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nota-fis w-digita
ON LEAVE OF fi-nota-fis IN FRAME F-Main /* Nota Fiscal */
DO:
   FIND LAST mp-entr-mat WHERE
             mp-entr-mat.nro-docto = SELF:INPUT-VALUE NO-LOCK NO-ERROR.
   IF AVAIL mp-entr-mat THEN
      ASSIGN fi-cod-fornec:SCREEN-VALUE = STRING(mp-entr-mat.cod-emit).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nr-fardo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nr-fardo w-digita
ON RETURN OF fi-nr-fardo IN FRAME F-Main /* Nß Fardo */
DO:
  APPLY 'entry' TO tt-mp-fardo.cd-coloracao IN BROWSE br-fardos.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nr-fardo w-digita
ON VALUE-CHANGED OF fi-nr-fardo IN FRAME F-Main /* Nß Fardo */
DO:
  FIND LAST tt-mp-fardo WHERE
            tt-mp-fardo.nr-fardo <= SELF:INPUT-VALUE NO-LOCK NO-ERROR.
  IF AVAIL tt-mp-fardo THEN
     br-fardos:QUERY:REPOSITION-TO-ROWID(ROWID(tt-mp-fardo)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-digita 


/* ***************************  Main Block  *************************** */
ON 'leave':U OF tt-mp-fardo.cd-coloracao IN BROWSE {&browse-name} DO:
    IF INPUT BROWSE {&browse-name} tt-mp-fardo.cd-coloracao <> '' THEN DO.
       FIND mp-coloracao WHERE
            mp-coloracao.codigo = INTEGER(INPUT BROWSE {&browse-name} tt-mp-fardo.cd-coloracao)
            NO-LOCK NO-ERROR.
       IF NOT AVAIL mp-coloracao THEN DO.
          MESSAGE 'Coloraá∆o n∆o Cadastrada...'
              VIEW-AS ALERT-BOX ERROR BUTTONS OK.
          APPLY 'entry' TO SELF.
          RETURN NO-APPLY.
       END.
       ASSIGN tt-mp-fardo.desc-coloracao = mp-coloracao.tonalidade.
       DISP tt-mp-fardo.desc-coloracao WITH BROWSE {&browse-name}. 
    END.
END.
ON 'F5':U, 'mouse-select-dblclick':U OF tt-mp-fardo.cd-coloracao IN BROWSE {&browse-name} DO:
    {include/zoomvar.i &prog-zoom=eszoom/z01es054.w
                       &campo=tt-mp-fardo.cd-coloracao
                       &campozoom=codigo
                       &BROWSE=br-fardos}
END.

ON 'leave':U OF tt-mp-fardo.cd-tipo IN BROWSE {&browse-name} DO:
    IF INPUT BROWSE {&browse-name} tt-mp-fardo.cd-tipo <> '' THEN DO.
       FIND mp-tipo WHERE
            mp-tipo.codigo = INT(INPUT BROWSE {&browse-name} tt-mp-fardo.cd-tipo) NO-LOCK NO-ERROR.
       IF NOT AVAIL mp-tipo THEN DO.
          MESSAGE 'Tipo n∆o Cadastrado...'
              VIEW-AS ALERT-BOX ERROR BUTTONS OK.
          APPLY 'entry' TO SELF.
          RETURN NO-APPLY.
       END.
       ASSIGN tt-mp-fardo.desc-tipo = mp-tipo.tipo.
       DISP tt-mp-fardo.desc-tipo WITH BROWSE {&browse-name}. 
    END.
END.
ON 'F5':U, 'mouse-select-dblclick':U OF tt-mp-fardo.cd-tipo IN BROWSE {&browse-name} DO:
    {include/zoomvar.i &prog-zoom=eszoom/z01es055.w
                       &campo=tt-mp-fardo.cd-tipo
                       &campozoom=codigo
                       &BROWSE=br-fardos}
END.

ON 'leave':U OF tt-mp-fardo.cd-compr IN BROWSE {&browse-name} DO:
    IF INPUT BROWSE {&browse-name} tt-mp-fardo.cd-compr <> '' THEN DO.
       FIND mp-classificacao WHERE
            mp-classificacao.codigo = INT(INPUT BROWSE {&browse-name} tt-mp-fardo.cd-compr) NO-LOCK NO-ERROR.
       IF NOT AVAIL mp-classificacao THEN DO.
          MESSAGE 'Comprimento de Fibra n∆o Cadastrado...'
             VIEW-AS ALERT-BOX ERROR BUTTONS OK.
          APPLY 'entry' TO SELF.
          RETURN NO-APPLY.
       END.
       ASSIGN tt-mp-fardo.letra = mp-classificacao.letra-id.
       DISP tt-mp-fardo.letra WITH BROWSE {&browse-name}. 
    END.
END.

ON 'F5':U, 'mouse-select-dblclick':U OF tt-mp-fardo.cd-compr IN BROWSE {&browse-name} DO:
    {include/zoomvar.i &prog-zoom=eszoom/z01es056.w
                       &campo=tt-mp-fardo.cd-compr
                       &campozoom=codigo
                       &BROWSE=br-fardos}
END.

fi-cd-coloracao:LOAD-MOUSE-POINTER("image/lupa.cur").
fi-cd-tipo:LOAD-MOUSE-POINTER("image/lupa.cur").
fi-codigo:LOAD-MOUSE-POINTER("image/lupa.cur").

tt-mp-fardo.cd-coloracao:LOAD-MOUSE-POINTER("image/lupa.cur") IN BROWSE {&browse-name}.
tt-mp-fardo.cd-tipo:LOAD-MOUSE-POINTER("image/lupa.cur") IN BROWSE {&browse-name}.
tt-mp-fardo.cd-compr:LOAD-MOUSE-POINTER("image/lupa.cur") IN BROWSE {&browse-name}.

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-digita  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  /* Select a Startup page. */
  IF adm-current-page eq 0 
  THEN RUN select-page IN THIS-PROCEDURE ( 1 ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-digita  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-digita  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-digita)
  THEN DELETE WIDGET w-digita.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-digita  _DEFAULT-ENABLE
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
  DISPLAY fi-nota-fis fi-cod-fornec fi-nome-emit fi-dt-recebimento 
          fi-procedencia fi-qt-fardo fi-nr-fardo fi-cd-tipo fi-cd-coloracao 
          fi-codigo 
      WITH FRAME F-Main IN WINDOW w-digita.
  ENABLE fi-nota-fis fi-cod-fornec br-fardos bt-ajuda bt-ok bt-cancelar RECT-1 
         RECT-44 RECT-7 
      WITH FRAME F-Main IN WINDOW w-digita.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW w-digita.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-digita 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-digita 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-digita 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  {utp/ut9000.i "ESSP0132" "2.04.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  {include/i-inifld.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-digita  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-mp-fardo"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-digita 
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

