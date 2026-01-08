&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
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
{include/i-prgvrs.i ACT-DOCAS 2.04.00.000}
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
DEF TEMP-TABLE tt-ob-etiqueta LIKE ob-etiqueta
    FIELD marca AS CHAR.

DEF BUFFER b-tt-ob-etiqueta FOR tt-ob-etiqueta.
/* Local Variable Definitions ---                                       */
DEF VAR c-desc-item    AS CHAR FORMAT "x(36)".
DEF VAR c-desc-refer   AS CHAR.
DEF VAR c-un           AS CHAR.
DEF VAR c-comando      AS CHAR.
DEF VAR c-retalho      AS CHAR INITIAL "RETALHO".
DEF VAR c-corte        AS CHAR INITIAL "SC".
DEF VAR c-form-epl     AS CHAR FORMAT "x(30)".
DEF VAR c-prog-epl     AS CHAR FORMAT "x(50)".
DEF VAR c-composicao   LIKE composi.descricao EXTENT 3.
DEF VAR v-defeito      AS CHAR EXTENT 3.
DEF VAR i-ct           AS INT.
DEF VAR i-lote         AS INT.
DEF VAR i-num-bar      AS INT.
DEF VAR i-nr-sequencia AS INT.

{esinc/sz-pcl.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Digitacao
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-ob-etiqueta

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-ob-etiqueta

/* Definitions for BROWSE br-ob-etiqueta                                */
&Scoped-define FIELDS-IN-QUERY-br-ob-etiqueta tt-ob-etiqueta.num-etiqueta tt-ob-etiqueta.it-codigo fn-item() @ c-desc-item fn-un() @ c-un tt-ob-etiqueta.cod-refer tt-ob-etiqueta.localizacao tt-ob-etiqueta.quantidade tt-ob-etiqueta.marca   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-ob-etiqueta   
&Scoped-define SELF-NAME br-ob-etiqueta
&Scoped-define OPEN-QUERY-br-ob-etiqueta RUN pi-total. OPEN QUERY {&SELF-NAME} FOR EACH tt-ob-etiqueta NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-ob-etiqueta tt-ob-etiqueta
&Scoped-define FIRST-TABLE-IN-QUERY-br-ob-etiqueta tt-ob-etiqueta


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br-ob-etiqueta}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-ob-etiqueta bt-cancelar fi-cod-estabel ~
fi-serie fi-cod-emit fi-nr-nota-fis fi-local bt-processa RECT-10 RECT-45 
&Scoped-Define DISPLAYED-OBJECTS fi-tot-nota fi-cod-estabel fi-nome-estabel ~
fi-serie fi-cod-emit fi-nome-emit fi-nr-nota-fis fi-local fi-tot-etiquetas 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-item w-digita 
FUNCTION fn-item RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-un w-digita 
FUNCTION fn-un RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-digita AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     IMAGE-UP FILE "image/im-exi.bmp":U
     LABEL "Cancelar" 
     SIZE 6 BY 1.5 TOOLTIP "Cancela Operaá∆o".

DEFINE BUTTON bt-desmarca AUTO-GO 
     IMAGE-UP FILE "image/im-cllps.bmp":U
     LABEL "" 
     SIZE 7 BY 1.5 TOOLTIP "Desmarca Etiqueta"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-marca AUTO-GO 
     IMAGE-UP FILE "image/im-expan.bmp":U
     LABEL "" 
     SIZE 7 BY 1.5 TOOLTIP "Marca Etiquetas"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-nenhum AUTO-GO 
     IMAGE-UP FILE "image/im-nenhum.bmp":U
     LABEL "" 
     SIZE 7 BY 1.5 TOOLTIP "Desmarca TODAS as Etiquetas"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-ok AUTO-GO 
     IMAGE-UP FILE "image/im-cq.bmp":U
     LABEL "OK" 
     SIZE 7 BY 1.5 TOOLTIP "Grava a Nova Localizaá∆o".

DEFINE BUTTON bt-processa 
     IMAGE-UP FILE "image/im-vapra.bmp":U
     LABEL "Button 1" 
     SIZE 7 BY 1.13 TOOLTIP "Processa Nota Fiscal".

DEFINE BUTTON bt-todos AUTO-GO 
     IMAGE-UP FILE "N:\image\im-ran_a.bmp":U
     LABEL "" 
     SIZE 7 BY 1.5 TOOLTIP "Marca TODAS as Etiquetas"
     BGCOLOR 8 FONT 10.

DEFINE VARIABLE fi-cod-emit AS INTEGER FORMAT ">>>,>>9" INITIAL 0 
     LABEL "Cliente" 
     VIEW-AS FILL-IN 
     SIZE 6.14 BY .88
     BGCOLOR 15 FONT 1.

DEFINE VARIABLE fi-cod-estabel AS CHARACTER FORMAT "X(3)":U 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE fi-local AS CHARACTER FORMAT "999/999":U INITIAL "0" 
     LABEL "Nova Localizaá∆o" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi-nome-emit AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 36.29 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi-nome-estabel AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 43.72 BY .88
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE fi-nr-nota-fis AS CHARACTER FORMAT "x(16)" 
     LABEL "Nr Nota Fiscal":R17 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE fi-serie AS CHARACTER FORMAT "x(5)" INITIAL "1" 
     LABEL "SÇrie":R7 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE fi-tot-etiquetas AS INTEGER FORMAT "->>>,>>>,>>9":R29 INITIAL 0 
     LABEL "Total de Etiquetas" 
     VIEW-AS FILL-IN 
     SIZE 13.57 BY 1
     FGCOLOR 12 FONT 6 NO-UNDO.

DEFINE VARIABLE fi-tot-nota AS DECIMAL FORMAT "->>>,>>>,>>9.99":R23 INITIAL 0 
     LABEL "Total da Nota Fiscal" 
     VIEW-AS FILL-IN 
     SIZE 13.57 BY 1
     FGCOLOR 12 FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 7.86 BY 12.25
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-45
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 87 BY 4.38.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-ob-etiqueta FOR 
      tt-ob-etiqueta SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-ob-etiqueta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-ob-etiqueta w-digita _FREEFORM
  QUERY br-ob-etiqueta NO-LOCK DISPLAY
      tt-ob-etiqueta.num-etiqueta                 COLUMN-LABEL "Etiqueta"
      tt-ob-etiqueta.it-codigo  FORMAT "x(6)":U   COLUMN-LABEL "Item"           WIDTH 7
      fn-item() @ c-desc-item                     COLUMN-LABEL "Descriá∆o Item" WIDTH 22
      fn-un() @ c-un                              COLUMN-LABEL "Und"            WIDTH 4 
      tt-ob-etiqueta.cod-refer  FORMAT "x(8)":U   COLUMN-LABEL "Referància"     WIDTH 9
      tt-ob-etiqueta.localizacao FORMAT "999/999"  COLUMN-LABEL "Local Atual"   WIDTH 10  COLUMN-FGCOLOR 12
      tt-ob-etiqueta.quantidade FORMAT ">>>,>>9.99":U COLUMN-LABEL "Quantidade"
      tt-ob-etiqueta.marca      FORMAT "x(1)" COLUMN-LABEL "M"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 79 BY 10.83
         FONT 6
         TITLE "Etiquetas da Nota Fiscal" ROW-HEIGHT-CHARS .58.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi-tot-nota AT ROW 16.75 COL 61.29 COLON-ALIGNED
     br-ob-etiqueta AT ROW 5.67 COL 2
     bt-ok AT ROW 14 COL 81.72
     bt-cancelar AT ROW 16.42 COL 82
     fi-cod-estabel AT ROW 1.25 COL 16.43 COLON-ALIGNED
     fi-nome-estabel AT ROW 1.25 COL 21.43 COLON-ALIGNED NO-LABEL
     fi-serie AT ROW 2.25 COL 16.43 COLON-ALIGNED HELP
          "SÇrie da nota fiscal"
     fi-cod-emit AT ROW 3.25 COL 16.43 COLON-ALIGNED
     fi-nome-emit AT ROW 3.25 COL 23.14 COLON-ALIGNED NO-LABEL
     fi-nr-nota-fis AT ROW 4.25 COL 16 COLON-ALIGNED HELP
          "N£mero da nota fiscal"
     fi-local AT ROW 4.25 COL 49.72 COLON-ALIGNED
     bt-processa AT ROW 4.04 COL 80.72
     fi-tot-etiquetas AT ROW 16.75 COL 14 COLON-ALIGNED
     bt-desmarca AT ROW 5.92 COL 81.72
     bt-marca AT ROW 7.42 COL 81.72
     bt-todos AT ROW 9.21 COL 81.72
     bt-nenhum AT ROW 10.67 COL 81.57
     RECT-10 AT ROW 5.75 COL 81.14
     RECT-45 AT ROW 1.13 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 88.43 BY 17.25
         FONT 1.


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
         TITLE              = "Nova Localizaá∆o de Etiquetas Faturadas"
         HEIGHT             = 17.25
         WIDTH              = 90.29
         MAX-HEIGHT         = 28.63
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 28.63
         VIRTUAL-WIDTH      = 146.29
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
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB br-ob-etiqueta fi-tot-nota F-Main */
/* SETTINGS FOR BUTTON bt-desmarca IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-marca IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-nenhum IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-ok IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-todos IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nome-emit IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nome-estabel IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-etiquetas IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-nota IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-digita)
THEN w-digita:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-ob-etiqueta
/* Query rebuild information for BROWSE br-ob-etiqueta
     _START_FREEFORM
RUN pi-total.
OPEN QUERY {&SELF-NAME} FOR EACH tt-ob-etiqueta NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-ob-etiqueta */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON END-ERROR OF w-digita /* Nova Localizaá∆o de Etiquetas Faturadas */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON WINDOW-CLOSE OF w-digita /* Nova Localizaá∆o de Etiquetas Faturadas */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
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


&Scoped-define SELF-NAME bt-desmarca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-desmarca w-digita
ON CHOOSE OF bt-desmarca IN FRAME F-Main
DO:
  IF AVAIL tt-ob-etiqueta AND tt-ob-etiqueta.marca = '*' THEN DO:
     ASSIGN tt-ob-etiqueta.marca = "".
     ASSIGN fi-tot-nota      = fi-tot-nota - tt-ob-etiqueta.quantidade
            fi-tot-etiquetas = fi-tot-etiquetas - 1.
      br-ob-etiqueta:REFRESH().
      APPLY 'value-changed' TO br-ob-etiqueta IN FRAME {&FRAME-NAME}.
      DISP fi-tot-nota
           fi-tot-etiquetas
           WITH FRAME {&FRAME-NAME}.
      FIND FIRST b-tt-ob-etiqueta WHERE
                 b-tt-ob-etiqueta.marca = '*' NO-LOCK NO-ERROR.
      IF NOT AVAIL b-tt-ob-etiqueta THEN
         ASSIGN bt-ok:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-marca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-marca w-digita
ON CHOOSE OF bt-marca IN FRAME F-Main
DO:
  IF AVAIL tt-ob-etiqueta AND tt-ob-etiqueta.marca = '' THEN DO:
     ASSIGN tt-ob-etiqueta.marca    = "*".
     ASSIGN fi-tot-nota      = fi-tot-nota + tt-ob-etiqueta.quantidade
            fi-tot-etiquetas = fi-tot-etiquetas + 1.
     ASSIGN bt-ok:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
     APPLY 'value-changed' TO br-ob-etiqueta.
     br-ob-etiqueta:REFRESH().
     DISP fi-tot-nota
          fi-tot-etiquetas
          WITH FRAME {&FRAME-NAME}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-nenhum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-nenhum w-digita
ON CHOOSE OF bt-nenhum IN FRAME F-Main
DO:
    ASSIGN fi-tot-nota      = 0
           fi-tot-etiquetas = 0.
    FOR EACH tt-ob-etiqueta NO-LOCK.                                            
        ASSIGN tt-ob-etiqueta.marca = "".
    END.
    br-ob-etiqueta:REFRESH().
    APPLY 'value-changed' TO br-ob-etiqueta.
    ASSIGN bt-ok:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
    DISP fi-tot-nota
         fi-tot-etiquetas
         WITH FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-digita
ON CHOOSE OF bt-ok IN FRAME F-Main /* OK */
DO:
  FOR EACH tt-ob-etiqueta WHERE 
           tt-ob-etiqueta.marca = '*' NO-LOCK.
      FIND ob-etiqueta WHERE
           ob-etiqueta.cod-estabel  = tt-ob-etiqueta.cod-estabel AND
           ob-etiqueta.num-etiqueta = tt-ob-etiqueta.num-etiqueta NO-ERROR.
      IF AVAIL ob-etiqueta THEN
         ASSIGN ob-etiqueta.localizacao = INPUT FRAME {&FRAME-NAME} fi-local.
  END.
  EMPTY TEMP-TABLE tt-ob-etiqueta.
  ASSIGN fi-cod-emit:SCREEN-VALUE    = ''
         fi-nome-emit:SCREEN-VALUE   = ''
         fi-nr-nota-fis:SCREEN-VALUE = ''
         fi-local:SCREEN-VALUE       = ''.
  ASSIGN bt-desmarca:SENSITIVE IN FRAME {&FRAME-NAME} = NO 
         bt-marca:SENSITIVE IN FRAME {&FRAME-NAME}    = NO 
         bt-todos:SENSITIVE IN FRAME {&FRAME-NAME}    = NO 
         bt-nenhum:SENSITIVE IN FRAME {&FRAME-NAME}   = NO 
         bt-ok:SENSITIVE IN FRAME {&FRAME-NAME}       = NO
         fi-cod-estabel:SENSITIVE IN FRAME {&FRAME-NAME} = YES 
         fi-serie:SENSITIVE IN FRAME {&FRAME-NAME}       = YES  
         fi-cod-emit:SENSITIVE IN FRAME {&FRAME-NAME}    = YES 
         fi-nr-nota-fis:SENSITIVE IN FRAME {&FRAME-NAME} = YES
         fi-local:SENSITIVE IN FRAME {&FRAME-NAME}       = YES.

  ASSIGN fi-tot-nota      = 0 
         fi-tot-etiquetas = 0.
  DISP fi-tot-nota
       fi-tot-etiquetas
       WITH FRAME {&FRAME-NAME}.


  {&OPEN-QUERY-br-ob-etiqueta}
  APPLY 'entry' TO fi-cod-emit.
  RETURN NO-APPLY.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-processa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-processa w-digita
ON CHOOSE OF bt-processa IN FRAME F-Main /* Button 1 */
DO:
    FIND estabelec WHERE
         estabelec.cod-estabel = INPUT FRAME {&FRAME-NAME} fi-cod-estabel NO-LOCK NO-ERROR.
    IF NOT AVAIL estabelec THEN DO.
       MESSAGE "Estabelecimento n∆o Cadastrado..." VIEW-AS ALERT-BOX.
       APPLY 'entry' TO fi-cod-estabel.
       RETURN NO-APPLY.
    END.

    FIND emitente WHERE
         emitente.cod-emit = INPUT FRAME {&FRAME-NAME} fi-cod-emit NO-LOCK NO-ERROR.
    IF NOT AVAIL emitente THEN DO.
       MESSAGE "Emitente n∆o Cadastrado..." VIEW-AS ALERT-BOX.
       APPLY 'entry' TO fi-cod-emit.
       RETURN NO-APPLY.
    END.

    FIND nota-fiscal WHERE
         nota-fiscal.cod-estabel  = INPUT FRAME {&FRAME-NAME} fi-cod-estabel AND
         nota-fiscal.serie        = INPUT FRAME {&FRAME-NAME} fi-serie       AND 
         nota-fiscal.cod-emitente = INPUT FRAME {&FRAME-NAME} fi-cod-emit    AND 
         nota-fiscal.nr-nota-fis  = INPUT FRAME {&FRAME-NAME} fi-nr-nota-fis NO-LOCK NO-ERROR.
    IF NOT AVAIL nota-fiscal THEN DO:
       MESSAGE "Nota Fiscal n∆o Cadastrada..." VIEW-AS ALERT-BOX.
       APPLY 'entry' TO fi-nr-nota-fis.
       RETURN NO-APPLY.
    END.

    IF LENGTH(INPUT FRAME {&FRAME-NAME} fi-local) <> 6 THEN DO:
       MESSAGE "Favor informar a nova localizacao..." VIEW-AS ALERT-BOX.
       APPLY 'entry' TO fi-local.
       RETURN NO-APPLY.
    END.

    ASSIGN br-ob-etiqueta:TITLE IN FRAME {&FRAME-NAME}  = "ETIQUETAS DA NOTA FISCAL " + TRIM(fi-nr-nota-fis:SCREEN-VALUE).
  /*  ASSIGN br-ob-etiqueta:TITLE-FGCOLOR IN FRAME {&FRAME-NAME} =  1
           br-ob-etiqueta:TITLE-BGCOLOR IN FRAME {&FRAME-NAME} = 12.  */

    FOR EACH dev-item-rom WHERE
             dev-item-rom.nome-abrev = nota-fiscal.nome-ab-cli AND
             dev-item-rom.nr-pedcli  = nota-fiscal.nr-pedcli NO-LOCK.
        FIND ob-etiqueta WHERE
             ob-etiqueta.cod-estabel  = nota-fiscal.cod-estabel AND
             ob-etiqueta.num-etiqueta = dev-item-rom.num-etiqueta NO-ERROR.
        IF AVAIL ob-etiqueta THEN DO:
           CREATE tt-ob-etiqueta.
           BUFFER-COPY ob-etiqueta TO tt-ob-etiqueta.
           ASSIGN tt-ob-etiqueta.localizacao = INPUT FRAME {&FRAME-NAME} fi-local.
        END.
    END.
    ASSIGN bt-desmarca:SENSITIVE IN FRAME {&FRAME-NAME} = YES
           bt-marca:SENSITIVE IN FRAME {&FRAME-NAME}    = YES
           bt-todos:SENSITIVE IN FRAME {&FRAME-NAME}    = YES
           bt-nenhum:SENSITIVE IN FRAME {&FRAME-NAME}   = YES
           fi-cod-estabel:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           fi-serie:SENSITIVE IN FRAME {&FRAME-NAME}       = NO 
           fi-cod-emit:SENSITIVE IN FRAME {&FRAME-NAME}    = NO
           fi-nr-nota-fis:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           fi-local:SENSITIVE IN FRAME {&FRAME-NAME}       = NO.


    {&OPEN-QUERY-br-ob-etiqueta}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-todos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-todos w-digita
ON CHOOSE OF bt-todos IN FRAME F-Main
DO:
   ASSIGN fi-tot-nota      = 0
          fi-tot-etiquetas = 0.

   FOR EACH tt-ob-etiqueta NO-LOCK.                                            
       ASSIGN tt-ob-etiqueta.marca = "*".
       ASSIGN fi-tot-nota      = fi-tot-nota + tt-ob-etiqueta.quantidade
              fi-tot-etiquetas = fi-tot-etiquetas + 1.
   END.
   br-ob-etiqueta:REFRESH().
   APPLY 'value-changed' TO br-ob-etiqueta.
   ASSIGN bt-ok:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

   DISP fi-tot-nota
        fi-tot-etiquetas
        WITH FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-emit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-emit w-digita
ON ENTRY OF fi-cod-emit IN FRAME F-Main /* Cliente */
DO:
  FIND emitente WHERE
       emitente.cod-emit = SELF:INPUT-VALUE NO-LOCK NO-ERROR.
  IF AVAIL emitente THEN
     ASSIGN fi-nome-emit:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.nome-emit.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-emit w-digita
ON LEAVE OF fi-cod-emit IN FRAME F-Main /* Cliente */
DO:
  FIND emitente WHERE
       emitente.cod-emit = SELF:INPUT-VALUE NO-LOCK NO-ERROR.
  IF AVAIL emitente THEN
     ASSIGN fi-nome-emit:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.nome-emit.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-emit w-digita
ON MOUSE-SELECT-DBLCLICK OF fi-cod-emit IN FRAME F-Main /* Cliente */
DO:
  {include/zoomvar.i &prog-zoom=adzoom/z01ad098.w
                     &campo=fi-cod-emit
                     &campozoom=cod-emitente}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-estabel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-estabel w-digita
ON ENTRY OF fi-cod-estabel IN FRAME F-Main /* Estabelecimento */
DO:
  IF SELF:SCREEN-VALUE <> "" THEN DO.
     FIND estabelec WHERE
          estabelec.cod-estabel = INPUT FRAME {&FRAME-NAME} fi-cod-estabel
          NO-LOCK NO-ERROR.
     IF AVAIL estabelec THEN
        ASSIGN fi-nome-estabel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = estabelec.nome.
  END.
  ELSE ASSIGN fi-nome-estabel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-estabel w-digita
ON LEAVE OF fi-cod-estabel IN FRAME F-Main /* Estabelecimento */
DO:
  FIND estabelec WHERE
       estabelec.cod-estabel = INPUT FRAME {&FRAME-NAME} fi-cod-estabel NO-LOCK NO-ERROR.

  IF NOT AVAIL estabelec THEN DO.
     MESSAGE "Estabelecimento n∆o Cadastrado..." VIEW-AS ALERT-BOX.
     RETURN NO-APPLY.
  END.
  ASSIGN fi-nome-estabel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = estabelec.nome.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-estabel w-digita
ON MOUSE-SELECT-DBLCLICK OF fi-cod-estabel IN FRAME F-Main /* Estabelecimento */
DO:
  {include/zoomvar.i &prog-zoom = adzoom/z01ad107.w
                     &campo     = fi-cod-estabel
                     &campozoom = cod-estabel}
                  /*   &FRAME     = f-pg-sel} */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-ob-etiqueta
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-digita 


/* ***************************  Main Block  *************************** */

 fi-cod-estabel:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.
 fi-cod-emit:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.

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
  DISPLAY fi-tot-nota fi-cod-estabel fi-nome-estabel fi-serie fi-cod-emit 
          fi-nome-emit fi-nr-nota-fis fi-local fi-tot-etiquetas 
      WITH FRAME F-Main IN WINDOW w-digita.
  ENABLE br-ob-etiqueta bt-cancelar fi-cod-estabel fi-serie fi-cod-emit 
         fi-nr-nota-fis fi-local bt-processa RECT-10 RECT-45 
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

  {utp/ut9000.i "ACT-DOCAS" "2.04.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  {include/i-inifld.i}

  ASSIGN fi-cod-estabel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '2'
         fi-serie:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '1'.
  FIND estabelec WHERE
       estabelec.cod-estabel = INPUT FRAME {&FRAME-NAME} fi-cod-estabel NO-LOCK NO-ERROR.
  ASSIGN fi-nome-estabel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = estabelec.nome.
  APPLY 'entry' TO fi-cod-emit.
  RETURN 'ADM-ERROR':U.                                                    



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-total w-digita 
PROCEDURE pi-total :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 ASSIGN fi-tot-nota      = 0
        fi-tot-etiquetas = 0.
 FOR EACH tt-ob-etiqueta WHERE 
          tt-ob-etiqueta.marca = "*" NO-LOCK.
     ASSIGN fi-tot-nota      = fi-tot-nota + tt-ob-etiqueta.quantidade
            fi-tot-etiquetas = fi-tot-etiquetas + 1.
 END.
 DISP fi-tot-nota
      fi-tot-etiquetas
      WITH FRAME {&FRAME-NAME}.

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
  {src/adm/template/snd-list.i "tt-ob-etiqueta"}

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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-item w-digita 
FUNCTION fn-item RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

   FIND item WHERE
       item.it-codigo = tt-ob-etiqueta.it-codigo  NO-LOCK NO-ERROR.

   IF AVAIL item THEN 
      RETURN item.desc-item.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-un w-digita 
FUNCTION fn-un RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

   FIND item WHERE
       item.it-codigo = tt-ob-etiqueta.it-codigo NO-LOCK NO-ERROR.

   IF AVAIL item THEN 
      RETURN item.un.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

