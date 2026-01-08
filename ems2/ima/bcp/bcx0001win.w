&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME w-cadpaifilho-filho


/* Temp-Table and Buffer definitions                                    */
DEFINE BUFFER bf-it-doc-fisico FOR it-doc-fisico.
DEFINE NEW GLOBAL SHARED TEMP-TABLE tt-etiqueta LIKE bc-etiqueta
       FIELD cod-emitente LIKE emitente.cod-emitente
       FIELD desc-item    LIKE item.desc-item
       field log-registro as logical
       field r-rowid      as rowid
       field qt-etiqueta  as integer format ">>>9" label "Qt Etiq"
       index codigo is primary nr-etiq.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-cadpaifilho-filho 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i BCX0001WIN 1.00.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

def var p-table       AS rowid                  NO-UNDO.
DEF VAR r-rowid       AS ROWID                  NO-UNDO.
DEF VAR i-total-itens LIKE bc-etiqueta.qt-item  NO-UNDO.
DEF VAR d-qt-etiq-ant LIKE bc-etiqueta.qt-item  NO-UNDO.

DEF NEW GLOBAL SHARED VAR gr-it-doc-fisico AS ROWID     NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-container      AS HANDLE    NO-UNDO.

def temp-table tt-erro no-undo
    field i-sequen as int             
    field cd-erro  as int
    field mensagem as char format "x(255)".

DEF VAR l-hab-todos     AS LOGICAL NO-UNDO.
DEF VAR i-qt-etiq       AS INTEGER NO-UNDO.
DEF VAR i-cont          AS INTEGER NO-UNDO.
DEF VAR i-tp-embal      AS INTEGER.

DEF VAR i-br            AS INTEGER FORMAT "999" NO-UNDO.

DEF VAR i-seq           AS INTEGER NO-UNDO.

DEF VAR l-diminui       AS LOGICAL NO-UNDO.
DEF VAR i-nr-etiq-ant   AS INTEGER NO-UNDO.

DEF VAR l-hab-efet      AS LOGICAL INITIAL NO NO-UNDO.

DEF BUFFER bf-tt-etiq FOR tt-etiqueta.

DEF VAR c-cod-chave-param-ext LIKE bc-param-ext.cod-chave-param-ext.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-paifil
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad
&Scoped-define BROWSE-NAME br-table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-etiqueta

/* Definitions for BROWSE br-table                                      */
&Scoped-define FIELDS-IN-QUERY-br-table tt-etiqueta.it-codigo ~
tt-etiqueta.progressivo tt-etiqueta.inteiro-livre-1 tt-etiqueta.qt-item ~
tt-etiqueta.num-pedido tt-etiqueta.un tt-etiqueta.dt-criacao tt-etiqueta.hr-criacao ~
tt-etiqueta.usuar-criacao 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-table tt-etiqueta.inteiro-livre-1 ~
tt-etiqueta.qt-item tt-etiqueta.num-pedido
&Scoped-define ENABLED-TABLES-IN-QUERY-br-table tt-etiqueta
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br-table tt-etiqueta
&Scoped-define QUERY-STRING-br-table FOR EACH tt-etiqueta ~
      WHERE  tt-etiqueta.nr-nota-fis   = bf-it-doc-fisico.nro-docto ~
      AND tt-etiqueta.serie         = bf-it-doc-fisico.serie-docto ~
      AND tt-etiqueta.it-codigo     = bf-it-doc-fisico.it-codigo ~
      AND tt-etiqueta.nr-romaneio   = bf-it-doc-fisico.sequencia NO-LOCK ~
    BY tt-etiqueta.inteiro-livre-2 ~
       BY tt-etiqueta.progressivo INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-table OPEN QUERY br-table FOR EACH tt-etiqueta ~
      WHERE  tt-etiqueta.nr-nota-fis   = bf-it-doc-fisico.nro-docto ~
      AND tt-etiqueta.serie         = bf-it-doc-fisico.serie-docto ~
      AND tt-etiqueta.it-codigo     = bf-it-doc-fisico.it-codigo ~
      AND tt-etiqueta.nr-romaneio   = bf-it-doc-fisico.sequencia NO-LOCK ~
    BY tt-etiqueta.inteiro-livre-2 ~
       BY tt-etiqueta.progressivo INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-table tt-etiqueta
&Scoped-define FIRST-TABLE-IN-QUERY-br-table tt-etiqueta


/* Definitions for FRAME f-cad                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-cad ~
    ~{&OPEN-QUERY-br-table}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-button br-table bt-incluir bt-modificar ~
bt-eliminar bt-imprime-todos bt-imprime d-qt-recebido d-qt-lido 
&Scoped-Define DISPLAYED-OBJECTS d-qt-recebido d-qt-lido 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-cadpaifilho-filho AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU mi-arquivo 
       MENU-ITEM mi-primeiro    LABEL "&Primeiro"      ACCELERATOR "CTRL-HOME"
       MENU-ITEM mi-anterior    LABEL "An&terior"      ACCELERATOR "CTRL-CURSOR-LEFT"
       MENU-ITEM mi-proximo     LABEL "Pr&¢ximo"       ACCELERATOR "CTRL-CURSOR-RIGHT"
       MENU-ITEM mi-ultimo      LABEL "&Èltimo"        ACCELERATOR "CTRL-END"
       MENU-ITEM mi-va-para     LABEL "&V† para"       ACCELERATOR "CTRL-T"
       MENU-ITEM mi-pesquisa    LABEL "Pes&quisa"      ACCELERATOR "CTRL-F5"
       RULE
       MENU-ITEM mi-consultas   LABEL "Co&nsultas"     ACCELERATOR "CTRL-L"
       MENU-ITEM mi-imprimir    LABEL "&Relat¢rios"    ACCELERATOR "CTRL-P"
       RULE
       MENU-ITEM mi-sair        LABEL "&Sair"          ACCELERATOR "CTRL-X".

DEFINE SUB-MENU mi-ajuda 
       MENU-ITEM mi-conteudo    LABEL "&Conteudo"     
       MENU-ITEM mi-sobre       LABEL "&Sobre..."     .

DEFINE MENU m-cadastro MENUBAR
       SUB-MENU  mi-arquivo     LABEL "&Arquivo"      
       SUB-MENU  mi-ajuda       LABEL "&Ajuda"        .


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_p-exihel AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-navega AS HANDLE NO-UNDO.
DEFINE VARIABLE h_q01im001 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v01im163 AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-confirma 
     IMAGE-UP FILE "image/im-chck1.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-check.bmp":U
     LABEL "" 
     SIZE 4 BY 1.25.

DEFINE BUTTON bt-eliminar 
     LABEL "&Eliminar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-imprime 
     LABEL "&Imprimir" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-imprime-todos 
     LABEL "Imprimir &Todos" 
     SIZE 15 BY 1.

DEFINE BUTTON bt-incluir 
     LABEL "&Incluir" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-modificar 
     LABEL "&Modificar" 
     SIZE 10 BY 1.

DEFINE VARIABLE d-qt-lido AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "Qt Peáas Lidas" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE d-qt-recebido AS DECIMAL FORMAT ">,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Qt Recebido" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 90 BY 1.46
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-table FOR 
      tt-etiqueta SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-table w-cadpaifilho-filho _STRUCTURED
  QUERY br-table NO-LOCK DISPLAY
      tt-etiqueta.it-codigo WIDTH 6
      tt-etiqueta.progressivo COLUMN-LABEL "Etiqueta" WIDTH 15
      tt-etiqueta.inteiro-livre-1 COLUMN-LABEL "Qt C¢pias" WIDTH 5
      tt-etiqueta.qt-item FORMAT ">,>>>,>>>,>>9.9999":U
      tt-etiqueta.num-pedido COLUMN-LABEL "Container" 
      tt-etiqueta.un WIDTH 4
      tt-etiqueta.dt-criacao
      tt-etiqueta.hr-criacao
      tt-etiqueta.usuar-criacao
  ENABLE
      tt-etiqueta.inteiro-livre-1 HELP "Qtde de etiquetas a serem geradas"
      tt-etiqueta.qt-item
      tt-etiqueta.num-pedido
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 87 BY 7
         FONT 1
         TITLE "Etiquetas" ROW-HEIGHT-CHARS .54.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     bt-confirma AT ROW 1.17 COL 62
     br-table AT ROW 8.25 COL 2
     bt-incluir AT ROW 15.42 COL 1.72
     bt-modificar AT ROW 15.42 COL 12
     bt-eliminar AT ROW 15.42 COL 22.43
     bt-imprime-todos AT ROW 15.42 COL 33
     bt-imprime AT ROW 15.42 COL 48.43
     d-qt-recebido AT ROW 15.42 COL 73 COLON-ALIGNED
     d-qt-lido AT ROW 16.38 COL 73 COLON-ALIGNED
     rt-button AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 17
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-paifil
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: bf-it-doc-fisico B "?" ? movind it-doc-fisico
      TABLE: tt-etiqueta T "NEW GLOBAL SHARED" ? mgcld bc-etiqueta
      ADDITIONAL-FIELDS:
          FIELD cod-emitente LIKE emitente.cod-emitente
          FIELD desc-item    LIKE item.desc-item
          field log-registro as logical
          field r-rowid      as rowid
          field qt-etiqueta  as integer format ">>>9" label "Qt Etiq"
          index codigo is primary nr-etiq
      END-FIELDS.
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-cadpaifilho-filho ASSIGN
         HIDDEN             = YES
         TITLE              = "Geracao de Etiquetas no Recebimento"
         HEIGHT             = 16.5
         WIDTH              = 90
         MAX-HEIGHT         = 27.17
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 27.17
         VIRTUAL-WIDTH      = 146.29
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU m-cadastro:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-cadpaifilho-filho 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-paifil.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-cadpaifilho-filho
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-cad
   FRAME-NAME L-To-R                                                    */
/* BROWSE-TAB br-table bt-confirma f-cad */
/* SETTINGS FOR BUTTON bt-confirma IN FRAME f-cad
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-cadpaifilho-filho)
THEN w-cadpaifilho-filho:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-table
/* Query rebuild information for BROWSE br-table
     _TblList          = "Temp-Tables.tt-etiqueta"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "Temp-Tables.tt-etiqueta.inteiro-livre-2|yes,Temp-Tables.tt-etiqueta.progressivo|yes"
     _Where[1]         = " tt-etiqueta.nr-nota-fis   = bf-it-doc-fisico.nro-docto
      AND tt-etiqueta.serie         = bf-it-doc-fisico.serie-docto
      AND tt-etiqueta.it-codigo     = bf-it-doc-fisico.it-codigo
      AND tt-etiqueta.nr-romaneio   = bf-it-doc-fisico.sequencia"
     _FldNameList[1]   > Temp-Tables.tt-etiqueta.it-codigo
"tt-etiqueta.it-codigo" ? ? "" ? ? ? ? ? ? no ? no no "6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.tt-etiqueta.progressivo
"tt-etiqueta.progressivo" "Etiqueta" ? "character" ? ? ? ? ? ? no "N£mero da Etiqueta" no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.tt-etiqueta.inteiro-livre-1
"tt-etiqueta.inteiro-livre-1" "Qt C¢pias" ? "integer" ? ? ? ? ? ? yes "Qtde de etiquetas a serem geradas" no no "5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.tt-etiqueta.qt-item
"tt-etiqueta.qt-item" ? ">,>>>,>>>,>>9.9999" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.tt-etiqueta.un
"tt-etiqueta.un" ? ? "" ? ? ? ? ? ? no ? no no "4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   = Temp-Tables.tt-etiqueta.dt-criacao
     _FldNameList[7]   = Temp-Tables.tt-etiqueta.hr-criacao
     _FldNameList[8]   = Temp-Tables.tt-etiqueta.usuar-criacao
     _Query            is OPENED
*/  /* BROWSE br-table */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-cadpaifilho-filho
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-cadpaifilho-filho w-cadpaifilho-filho
ON END-ERROR OF w-cadpaifilho-filho /* Geracao de Etiquetas no Recebimento */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-cadpaifilho-filho w-cadpaifilho-filho
ON WINDOW-CLOSE OF w-cadpaifilho-filho /* Geracao de Etiquetas no Recebimento */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-table
&Scoped-define SELF-NAME br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table w-cadpaifilho-filho
ON RETURN OF br-table IN FRAME f-cad /* Etiquetas */
ANYWHERE
DO:
    
    IF  tt-etiqueta.log-registro = NO and
        d-qt-etiq-ant <> INPUT BROWSE {&browse-name} tt-etiqueta.qt-item THEN DO:
        APPLY 'leave' TO tt-etiqueta.qt-item IN BROWSE {&browse-name}.
        RETURN NO-APPLY.        
    END.

    ASSIGN INPUT BROWSE {&browse-name} tt-etiqueta.qt-item 
           INPUT BROWSE {&browse-name} tt-etiqueta.num-pedido
           INPUT BROWSE {&browse-name} tt-etiqueta.inteiro-livre-1.

    IF  AVAIL tt-etiqueta and
        tt-etiqueta.log-registro and
        INPUT BROWSE br-table tt-etiqueta.qt-item <> d-qt-etiq-ant THEN DO:
        ASSIGN tt-etiqueta.logico-livre-5 = NO
               l-diminui                  = YES.
    END.

    
    IF  AVAIL tt-etiqueta AND
        NOT tt-etiqueta.logico-livre-5 and
        DEC(tt-etiqueta.qt-item:SCREEN-VALUE IN BROWSE br-table) > 0 THEN DO:
        ASSIGN d-qt-recebido = d-qt-recebido + (dec(tt-etiqueta.qt-item:SCREEN-VALUE IN BROWSE br-table) * 
                                                INTEGER(tt-etiqueta.inteiro-livre-1:SCREEN-VALUE IN BROWSE br-table))
               tt-etiqueta.logico-livre-5 = YES.
        IF  l-diminui THEN
            ASSIGN d-qt-recebido = d-qt-recebido - (d-qt-etiq-ant * i-nr-etiq-ant).
        ASSIGN d-qt-recebido:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(d-qt-recebido).
    END.

    DISPLAY d-qt-recebido WITH FRAME {&FRAME-NAME}.
    APPLY "choose" TO bt-incluir.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table w-cadpaifilho-filho
ON ROW-DISPLAY OF br-table IN FRAME f-cad /* Etiquetas */
DO:
    
    IF LASTKEY = 13 THEN DO: /* enter */
        IF  AVAIL tt-etiqueta THEN DO:
            ASSIGN i-total-itens = i-total-itens + tt-etiqueta.qt-item.
            IF  NOT l-hab-todos AND
                tt-etiqueta.log-registro = YES THEN
                ASSIGN l-hab-efet = YES.
            IF  tt-etiqueta.log-registro = YES THEN
                ASSIGN l-hab-todos   = YES.
            ASSIGN d-qt-lido:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(num-results("br-table")).
        END.
    END.
    RUN pi-calcula-recebido.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table w-cadpaifilho-filho
ON ROW-ENTRY OF br-table IN FRAME f-cad /* Etiquetas */
DO:
    IF AVAIL tt-etiqueta AND tt-etiqueta.log-registro = NO THEN DO:
       APPLY "value-changed" TO br-table.
       RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table w-cadpaifilho-filho
ON ROW-LEAVE OF br-table IN FRAME f-cad /* Etiquetas */
DO:
  if br-table:NEW-ROW in frame {&frame-name} then 
        do transaction on error undo, return no-apply:
            create tt-etiqueta.
            ASSIGN tt-etiqueta.it-codigo        = bf-it-doc-fisico.it-codigo
                   tt-etiqueta.un               = bf-it-doc-fisico.un
                   tt-etiqueta.dt-criacao       = TODAY
                   tt-etiqueta.hr-criacao       = STRING(TIME,"HH:MM:SS")
                   tt-etiqueta.usuar-criacao    = c-seg-usuario
                   tt-etiqueta.log-registro     = YES
                   tt-etiqueta.nr-nota-fis      = bf-it-doc-fisico.nro-docto
                   tt-etiqueta.serie            = bf-it-doc-fisico.serie-docto.
        
            br-table:CREATE-RESULT-LIST-ENTRY() in frame {&frame-name}.
          
        end.
        else do transaction on error undo, return no-apply:
               IF  AVAIL tt-etiqueta then
                assign input browse br-table tt-etiqueta.qt-item tt-etiqueta.num-pedido.
           
        end.
  RUN pi-calcula-recebido.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table w-cadpaifilho-filho
ON VALUE-CHANGED OF br-table IN FRAME f-cad /* Etiquetas */
DO:
 
  IF  AVAIL tt-etiqueta THEN DO:  
     
     
      IF  tt-etiqueta.log-registro = NO THEN
          ASSIGN bt-imprime:SENSITIVE   IN FRAME {&FRAME-NAME} = YES
                 bt-imprime-todos:SENSITIVE IN FRAME {&FRAME-NAME} = YES
                 bt-modificar:SENSITIVE IN FRAME {&FRAME-NAME} = NO
                 bt-eliminar:SENSITIVE  IN FRAME {&FRAME-NAME} = NO.
      ELSE
          ASSIGN bt-imprime:SENSITIVE   IN FRAME {&FRAME-NAME} = NO
                 bt-imprime-todos:SENSITIVE IN FRAME {&FRAME-NAME} = NO
                 bt-modificar:SENSITIVE IN FRAME {&FRAME-NAME} = YES
                 bt-eliminar:SENSITIVE  IN FRAME {&FRAME-NAME} = YES.


      IF  tt-etiqueta.cod-estado = 2 THEN
          ASSIGN bt-imprime:SENSITIVE IN FRAME {&FRAME-NAME} = NO
                 bt-imprime-todos:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
      ELSE
          ASSIGN bt-imprime:SENSITIVE IN FRAME {&FRAME-NAME} = YES
                 bt-imprime-todos:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

       
  END.
  ELSE DO:
    ASSIGN bt-imprime:SENSITIVE IN FRAME {&FRAME-NAME} = NO
             bt-imprime-todos:SENSITIVE IN FRAME {&FRAME-NAME} = NO
             bt-modificar:SENSITIVE IN FRAME {&FRAME-NAME} = NO
             bt-eliminar:SENSITIVE  IN FRAME {&FRAME-NAME} = NO.
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-confirma
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-confirma w-cadpaifilho-filho
ON CHOOSE OF bt-confirma IN FRAME f-cad
DO:
  IF  SESSION:SET-WAIT-STATE("general") THEN.
  ASSIGN i-total-itens = 0.
  FOR EACH tt-etiqueta NO-LOCK
      WHERE tt-etiqueta.nr-nota-fis   = bf-it-doc-fisico.nro-docto
        AND tt-etiqueta.serie         = bf-it-doc-fisico.serie-docto
        AND tt-etiqueta.it-codigo     = bf-it-doc-fisico.it-codigo
        AND tt-etiqueta.referencia    = bf-it-doc-fisico.cod-refer
        AND tt-etiqueta.nr-romaneio   = bf-it-doc-fisico.sequencia:
  
     IF tt-etiqueta.num-pedido = 0 THEN DO.
        MESSAGE "Container n∆o foi Informado.... " SKIP
                "Confirma etiqueta sem Container ?"
            VIEW-AS ALERT-BOX QUESTION TITLE "Container n∆o informado" UPDATE l-confirma AS LOGICAL.
        IF NOT l-confirma THEN 
           RETURN NO-APPLY.
    END.
    ELSE DO.
        FIND pp-container WHERE
             pp-container.nr-container = tt-etiqueta.num-pedido NO-LOCK NO-ERROR.
        IF NOT AVAIL pp-container THEN DO.
            MESSAGE "Container n∆o Cadastrado !!!" 
                VIEW-AS ALERT-BOX ERROR TITLE "Erro".
            RETURN NO-APPLY.
        END.
    END.

      IF  tt-etiqueta.inteiro-livre-1 = 0 THEN
          ASSIGN i-qt-etiq = 1.
      ELSE
          ASSIGN i-qt-etiq = tt-etiqueta.inteiro-livre-1.

      IF  tt-etiqueta.qt-item = 0 AND
          tt-etiqueta.log-registro = YES THEN DO:
      
          MESSAGE "N∆o Ç permitido criar etiquetas com quantidades zeradas!" VIEW-AS ALERT-BOX ERROR TITLE "Erro".
          RETURN NO-APPLY.
      END.
      ASSIGN i-total-itens = i-total-itens + (tt-etiqueta.qt-item * i-qt-etiq).
  END.    
  
  RUN pi-efetiva-registro.    

  IF  SESSION:SET-WAIT-STATE("") THEN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-eliminar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-eliminar w-cadpaifilho-filho
ON CHOOSE OF bt-eliminar IN FRAME f-cad /* Eliminar */
DO:

    IF  AVAIL tt-etiqueta THEN
        ASSIGN d-qt-recebido = d-qt-recebido - tt-etiqueta.qt-item
               d-qt-recebido:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(d-qt-recebido).
       
    if  br-table:num-selected-rows > 0 then do on error undo, return no-apply:
        get current br-table.
        IF  tt-etiqueta.log-registro THEN
            delete tt-etiqueta.
        ELSE DO:
            MESSAGE "Etiqueta j† foi efetivada. N∆o Ç poss°vel elimin†-la!" VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
        END.

      

        if  br-table:delete-current-row() in frame {&FRAME-NAME} then.
        APPLY "value-changed" TO br-table IN FRAME {&FRAME-NAME}.

    end.
    
    FIND FIRST tt-etiqueta NO-LOCK
        WHERE tt-etiqueta.nr-nota-fis   = bf-it-doc-fisico.nro-docto
          AND tt-etiqueta.serie         = bf-it-doc-fisico.serie-docto
          AND tt-etiqueta.it-codigo     = bf-it-doc-fisico.it-codigo
          AND tt-etiqueta.nr-romaneio   = bf-it-doc-fisico.sequencia
          AND tt-etiqueta.log-registro  = YES
        NO-ERROR.
    IF  NOT AVAIL tt-etiqueta THEN
        ASSIGN bt-modificar:SENSITIVE IN FRAME {&FRAME-NAME} = NO
               bt-eliminar:SENSITIVE  IN FRAME {&FRAME-NAME} = NO
               bt-confirma:SENSITIVE  IN FRAME {&FRAME-NAME} = NO.    
    IF d-qt-lido > 0  THEN DO:
       ASSIGN d-qt-lido = d-qt-lido - 1
              d-qt-lido:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(d-qt-lido) .          
    END.

    if  num-results("br-table") = 0 THEN
        assign bt-modificar:SENSITIVE IN FRAME {&FRAME-NAME}        = NO
               bt-eliminar:SENSITIVE  IN FRAME {&FRAME-NAME}        = NO
               bt-confirma:SENSITIVE  IN FRAME {&FRAME-NAME}        = NO
               d-qt-lido                                            = 0
               d-qt-recebido                                        = 0
               d-qt-lido:SCREEN-VALUE IN FRAME {&FRAME-NAME}        = STRING(d-qt-lido)
               d-qt-recebido:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = string(d-qt-recebido).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-imprime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-imprime w-cadpaifilho-filho
ON CHOOSE OF bt-imprime IN FRAME f-cad /* Imprimir */
DO:

    FOR EACH tt-erro:
        DELETE tt-erro.
    END.

  
    RUN bcp/bcx2000.p (INPUT tt-etiqueta.r-rowid,
                        INPUT "IMA0001Q",
                        INPUT-OUTPUT TABLE tt-erro).
   
    FIND FIRST tt-erro
        NO-LOCK NO-ERROR.
    IF  AVAIL tt-erro THEN DO:
        MESSAGE 'Ocorreram erros na impress∆o da etiqueta.' SKIP
                'Verifique os erros antes de imprimir.' SKIP
                tt-erro.cd-erro ' - ' tt-erro.mensagem '.' VIEW-AS ALERT-BOX ERROR TITLE "Erro".
        RETURN NO-APPLY.
    END.
    ELSE
        ASSIGN tt-etiqueta.cod-estado = 2.
    APPLY "value-changed" TO br-table IN FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-imprime-todos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-imprime-todos w-cadpaifilho-filho
ON CHOOSE OF bt-imprime-todos IN FRAME f-cad /* Imprimir Todos */
DO:
    FOR EACH bf-tt-etiq NO-LOCK
      WHERE bf-tt-etiq.nr-nota-fis   = bf-it-doc-fisico.nro-docto
        AND bf-tt-etiq.serie         = bf-it-doc-fisico.serie-docto
        AND bf-tt-etiq.it-codigo     = bf-it-doc-fisico.it-codigo
        AND bf-tt-etiq.nr-romaneio   = bf-it-doc-fisico.sequencia    
        AND bf-tt-etiq.cod-estado    <> 2
        AND bf-tt-etiq.cod-estado    <> 4
        BY  bf-tt-etiq.inteiro-livre-2
        BY  bf-tt-etiq.progressivo:
        
        FOR EACH tt-erro:
            DELETE tt-erro.
        END.
    
    
        RUN bcp/bcx2000.p (INPUT bf-tt-etiq.r-rowid,
                            INPUT "IMA0001Q",
                            INPUT-OUTPUT TABLE tt-erro).
    
        FIND FIRST tt-erro
            NO-LOCK NO-ERROR.
        IF  AVAIL tt-erro THEN DO:
            MESSAGE 'Ocorreram erros na impress∆o da etiqueta.' SKIP
                    'Verifique os erros antes de imprimir.' SKIP
                    tt-erro.cd-erro ' - ' tt-erro.mensagem '.' VIEW-AS ALERT-BOX ERROR TITLE "Erro".
            RETURN NO-APPLY.
        END.
        ELSE
            ASSIGN bf-tt-etiq.cod-estado = 2.
        APPLY "value-changed" TO br-table IN FRAME {&FRAME-NAME}.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-incluir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-incluir w-cadpaifilho-filho
ON CHOOSE OF bt-incluir IN FRAME f-cad /* Incluir */
DO:

   
    assign bt-modificar:SENSITIVE in frame f-cad    = yes
           bt-eliminar:SENSITIVE  in frame f-cad    = yes.

/*     if  num-results("br-table") > 0 THEN DO:                   */
/*         IF  br-table:SELECT-ROW(NUM-RESULTS("br-table")) THEN. */
/*         br-table:INSERT-ROW("after") in frame f-cad.           */
/*     END.                                                       */
    
   ASSIGN i-seq = num-results("br-table").  
    

    do transaction:
        FIND bf-it-doc-fisico
            WHERE rowid(bf-it-doc-fisico) = gr-it-doc-fisico
            NO-LOCK NO-ERROR.

        create tt-etiqueta.
        ASSIGN tt-etiqueta.it-codigo        = bf-it-doc-fisico.it-codigo
               tt-etiqueta.un               = bf-it-doc-fisico.un
               tt-etiqueta.dt-criacao       = TODAY
               tt-etiqueta.hr-criacao       = STRING(TIME,"HH:MM:SS")
               tt-etiqueta.usuar-criacao    = c-seg-usuario
               tt-etiqueta.log-registro     = YES
               tt-etiqueta.nr-nota-fis      = bf-it-doc-fisico.nro-docto
               tt-etiqueta.serie            = bf-it-doc-fisico.serie-docto
               tt-etiqueta.nr-romaneio      = bf-it-doc-fisico.sequencia
               tt-etiqueta.referencia       = bf-it-doc-fisico.cod-refer
               r-rowid                      = ROWID(tt-etiqueta)
               tt-etiqueta.inteiro-livre-1  = 1
               tt-etiqueta.inteiro-livre-2  = i-seq + 1
               tt-etiqueta.logico-livre-5   = NO.

        {&OPEN-QUERY-{&BROWSE-NAME}}

        REPOSITION {&BROWSE-NAME} TO ROWID r-rowid.

        apply "entry" to tt-etiqueta.qt-item in browse br-table.
        ASSIGN bt-confirma:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
    end.
    
/*     IF  it-doc-fisico.quantidade <= i-total-itens THEN DO:                                                                                           */
/*         MESSAGE "N∆o poder† ser criada uma nova etiqueta, pois a quantidade total deste item j† foi atendida!" VIEW-AS ALERT-BOX ERROR TITLE "Erro". */
/*         RETURN NO-APPLY.                                                                                                                             */
/*     END.                                                                                                                                             */
/*     ASSIGN h-programa = THIS-PROCEDURE.                                                                                                              */
/*     RUN pi-Incmod ('incluir').                                                                                                                       */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-modificar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-modificar w-cadpaifilho-filho
ON CHOOSE OF bt-modificar IN FRAME f-cad /* Modificar */
DO:
    APPLY "entry" TO tt-etiqueta.qt-item IN BROWSE {&browse-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-anterior
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-anterior w-cadpaifilho-filho
ON CHOOSE OF MENU-ITEM mi-anterior /* Anterior */
DO:
  RUN pi-anterior IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-arquivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-arquivo w-cadpaifilho-filho
ON MENU-DROP OF MENU mi-arquivo /* Arquivo */
DO:
  run pi-disable-menu.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-consultas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-consultas w-cadpaifilho-filho
ON CHOOSE OF MENU-ITEM mi-consultas /* Consultas */
DO:
  RUN pi-consulta IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-conteudo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-conteudo w-cadpaifilho-filho
ON CHOOSE OF MENU-ITEM mi-conteudo /* Conteudo */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  RUN pi-ajuda IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-imprimir w-cadpaifilho-filho
ON CHOOSE OF MENU-ITEM mi-imprimir /* Relat¢rios */
DO:
  RUN pi-imprimir IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-pesquisa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-pesquisa w-cadpaifilho-filho
ON CHOOSE OF MENU-ITEM mi-pesquisa /* Pesquisa */
DO:
  RUN pi-pesquisa IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-primeiro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-primeiro w-cadpaifilho-filho
ON CHOOSE OF MENU-ITEM mi-primeiro /* Primeiro */
DO:
  RUN pi-primeiro IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-proximo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-proximo w-cadpaifilho-filho
ON CHOOSE OF MENU-ITEM mi-proximo /* Pr¢ximo */
DO:
  RUN pi-proximo IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sair w-cadpaifilho-filho
ON CHOOSE OF MENU-ITEM mi-sair /* Sair */
DO:
  RUN pi-sair IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sobre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sobre w-cadpaifilho-filho
ON CHOOSE OF MENU-ITEM mi-sobre /* Sobre... */
DO:
  {include/sobre.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-ultimo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-ultimo w-cadpaifilho-filho
ON CHOOSE OF MENU-ITEM mi-ultimo /* Èltimo */
DO:
  RUN pi-ultimo IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-va-para
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-va-para w-cadpaifilho-filho
ON CHOOSE OF MENU-ITEM mi-va-para /* V† para */
DO:
  RUN pi-vapara IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-cadpaifilho-filho 


/* ***************************  Main Block  *************************** */

ON 'entry':U OF tt-etiqueta.qt-item IN BROWSE {&browse-name}
DO:
    IF  AVAIL tt-etiqueta THEN
        ASSIGN d-qt-etiq-ant = dec(tt-etiqueta.qt-item:SCREEN-VALUE IN BROWSE {&browse-name})
               i-nr-etiq-ant = int(tt-etiqueta.inteiro-livre-1:SCREEN-VALUE IN BROWSE {&browse-name}).
END.

/*
ON 'leave':U OF tt-etiqueta.qt-item IN BROWSE {&browse-name}
DO:
    IF  AVAIL tt-etiqueta and
        NOT tt-etiqueta.log-registro and
        INPUT BROWSE br-table tt-etiqueta.qt-item <> d-qt-etiq-ant THEN DO:
        MESSAGE "Etiqueta j† impressa. N∆o Ç permitida alteraá∆o." VIEW-AS ALERT-BOX ERROR TITLE "Erro".
        ASSIGN tt-etiqueta.qt-item:SCREEN-VALUE IN BROWSE br-table = string(d-qt-etiq-ant).
    END.
    
    IF  AVAIL tt-etiqueta and
        tt-etiqueta.log-registro and
        INPUT BROWSE br-table tt-etiqueta.qt-item <> d-qt-etiq-ant THEN DO:
        ASSIGN tt-etiqueta.logico-livre-5 = NO
               l-diminui                  = YES.
    END.


    IF  AVAIL tt-etiqueta AND
        NOT tt-etiqueta.logico-livre-5 and
        DEC(tt-etiqueta.qt-item:SCREEN-VALUE IN BROWSE br-table) > 0 THEN DO:
        
        ASSIGN d-qt-recebido = d-qt-recebido + (INTEGER(tt-etiqueta.qt-item:SCREEN-VALUE IN BROWSE br-table) * 
                                                INTEGER(tt-etiqueta.inteiro-livre-1:SCREEN-VALUE IN BROWSE br-table))
               tt-etiqueta.logico-livre-5 = YES.
        IF  l-diminui THEN 
            ASSIGN d-qt-recebido = d-qt-recebido - (d-qt-etiq-ant * i-nr-etiq-ant).
        ASSIGN d-qt-recebido:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(d-qt-recebido).

    END.
    
    
END.
*/

ON 'leave':U OF tt-etiqueta.inteiro-livre-1
DO:
    IF  AVAIL tt-etiqueta THEN
        ASSIGN INPUT BROWSE {&browse-name} tt-etiqueta.inteiro-livre-1.
END.

/* Include custom  Main Block code for SmartWindows. */

ASSIGN h-container = THIS-PROCEDURE
       i-seq       = 0.
/*
FOR EACH bc-etiqueta NO-LOCK:
    CREATE tt-etiqueta.
    BUFFER-COPY bc-etiqueta TO tt-etiqueta.
    ASSIGN tt-etiqueta.log-registro     = NO
           tt-etiqueta.r-rowid          = ROWID(bc-etiqueta)
           tt-etiqueta.logico-livre-5   = NO
           tt-etiqueta.inteiro-livre-1  = 1.
END.
*/


{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-cadpaifilho-filho  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panel/p-exihel.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Edge-Pixels = 0,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-exihel ).
       RUN set-position IN h_p-exihel ( 1.13 , 74.14 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panel/p-navega.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Edge-Pixels = 0,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-navega ).
       RUN set-position IN h_p-navega ( 1.17 , 1.57 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 24.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'invwr/v01im163.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v01im163 ).
       RUN set-position IN h_v01im163 ( 2.75 , 1.00 ) NO-ERROR.
       /* Size in UIB:  ( 5.25 , 90.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'inqry/q01in163.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'ProgPesquisa = inzoom/z01in163.w,
                     ProgVaPara = clgo/g01cl005.w,
                     ProgIncMod = ,
                     Implantar = no':U ,
             OUTPUT h_q01im001 ).
       RUN set-position IN h_q01im001 ( 1.00 , 50.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.63 , 7.72 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Links to SmartViewer h_v01im163. */
       RUN add-link IN adm-broker-hdl ( h_q01im001 , 'Record':U , h_v01im163 ).

       /* Links to SmartQuery h_q01im001. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , h_q01im001 ).
       RUN add-link IN adm-broker-hdl ( h_p-navega , 'Navigation':U , h_q01im001 ).
       RUN add-link IN adm-broker-hdl ( h_p-navega , 'State':U , h_q01im001 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             bt-confirma:HANDLE IN FRAME f-cad , 'BEFORE':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-navega ,
             h_p-exihel , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v01im163 ,
             bt-confirma:HANDLE IN FRAME f-cad , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.
  /* Select a Startup page. */
  IF adm-current-page eq 0 
  THEN RUN select-page IN THIS-PROCEDURE ( 1 ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-cadpaifilho-filho  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-cadpaifilho-filho  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-cadpaifilho-filho)
  THEN DELETE WIDGET w-cadpaifilho-filho.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-cadpaifilho-filho  _DEFAULT-ENABLE
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
  DISPLAY d-qt-recebido d-qt-lido 
      WITH FRAME f-cad IN WINDOW w-cadpaifilho-filho.
  ENABLE rt-button br-table bt-incluir bt-modificar bt-eliminar 
         bt-imprime-todos bt-imprime d-qt-recebido d-qt-lido 
      WITH FRAME f-cad IN WINDOW w-cadpaifilho-filho.
  {&OPEN-BROWSERS-IN-QUERY-f-cad}
  VIEW w-cadpaifilho-filho.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-cadpaifilho-filho 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable w-cadpaifilho-filho 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  ASSIGN br-table:SENSITIVE IN FRAME f-cad = YES
         d-qt-recebido:SENSITIVE IN FRAME f-cad = NO
         d-qt-lido:SENSITIVE IN FRAME f-cad = NO.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-cadpaifilho-filho 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-cadpaifilho-filho 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  {include/win-size.i}

  run pi-before-initialize.

  {utp/ut9000.i "BCX0001WIN" "1.00.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .


  /* Code placed here will execute AFTER standard behavior.    */
  run pi-after-initialize.

  APPLY "value-changed" TO br-table IN FRAME {&FRAME-NAME}.



  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-calcula-recebido w-cadpaifilho-filho 
PROCEDURE pi-calcula-recebido :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER bb-it-doc-fisico FOR it-doc-fisico.
DEF BUFFER bb-tt-etiqueta   FOR tt-etiqueta.

    ASSIGN d-qt-recebido = 0
           d-qt-lido     = 0.    

        FOR EACH bb-tt-etiqueta NO-LOCK:

            ASSIGN d-qt-recebido = d-qt-recebido + (bb-tt-etiqueta.inteiro-livre-1 * bb-tt-etiqueta.qt-item)
                   d-qt-lido     = d-qt-lido     + bb-tt-etiqueta.inteiro-livre-1
                   bb-tt-etiqueta.logico-livre-5 = YES.
        END.
       
     
    ASSIGN d-qt-recebido:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(d-qt-recebido)
           d-qt-lido:SCREEN-VALUE IN FRAME {&FRAME-NAME}     = STRING(d-qt-lido).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-efetiva-registro w-cadpaifilho-filho 
PROCEDURE pi-efetiva-registro :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 DEF BUFFER bf-etiq         FOR bc-etiqueta.
 DEF BUFFER bf-emit         FOR emitente.

 DEF BUFFER bf-tt-etiq      FOR tt-etiqueta.
 DEF BUFFER bf-tt-etiq2     FOR tt-etiqueta.
 DEF BUFFER b-tt-etiq-aux   FOR tt-etiqueta.

 DEF VAR i-cont  AS INTEGER                      NO-UNDO.
 DEF VAR i-seq   AS INTEGER                      NO-UNDO.

 FIND bf-it-doc-fisico
    WHERE rowid(bf-it-doc-fisico) = gr-it-doc-fisico
    NO-LOCK NO-ERROR.

IF  i-total-itens > bf-it-doc-fisico.quantidade THEN DO:
     MESSAGE "Est† sendo gerado etiquetas com quantidade superior ao dispon°vel no item!" skip
             "Confirma efetivaá∆o?" UPDATE l-resp AS LOGICAL VIEW-AS ALERT-BOX ERROR BUTTONS YES-NO TITLE "Erro".
     IF  NOT l-resp THEN 
         RETURN "NOK":U.    

END.

 

 FOR EACH b-tt-etiq-aux
     WHERE b-tt-etiq-aux.log-registro  = YES
       AND b-tt-etiq-aux.nr-nota-fis   = bf-it-doc-fisico.nro-docto
       AND b-tt-etiq-aux.serie         = bf-it-doc-fisico.serie-docto
       AND b-tt-etiq-aux.it-codigo     = bf-it-doc-fisico.it-codigo
       AND b-tt-etiq-aux.referencia    = bf-it-doc-fisico.cod-refer
       AND b-tt-etiq-aux.nr-romaneio   = bf-it-doc-fisico.sequencia
        BY b-tt-etiq-aux.inteiro-livre-2
        BY b-tt-etiq-aux.progressivo:

    

     FIND FIRST bf-emit
         WHERE bf-emit.cod-emitente = bf-it-doc-fisico.cod-emitente
         NO-LOCK NO-ERROR.

     FIND FIRST doc-fisico
         WHERE doc-fisico.serie-docto = bf-it-doc-fisico.serie-docto
           AND doc-fisico.nro-docto   = bf-it-doc-fisico.nro-docto
           AND doc-fisico.cod-emitente = bf-it-doc-fisico.cod-emitente
         NO-LOCK NO-ERROR.
     IF  NOT AVAIL doc-fisico THEN DO:
         MESSAGE "Existem problemas de integridade dos dados. Tabela Documento F°sico n∆o encontrada!" VIEW-AS ALERT-BOX ERROR TITLE "Erro".
         RETURN "ADM-ERROR":U.
     END.
     IF  b-tt-etiq-aux.inteiro-livre-1 = 0 THEN
         ASSIGN i-qt-etiq = 1.
     ELSE
         ASSIGN i-qt-etiq = b-tt-etiq-aux.inteiro-livre-1.
     

     IF i-ep-codigo-usuario = '5' THEN
        ASSIGN c-cod-chave-param-ext = "MED0001Q".
     ELSE
        ASSIGN c-cod-chave-param-ext = "IMA0001Q".

     DO i-cont = 1 TO i-qt-etiq:
        FIND bc-param-ext WHERE
             bc-param-ext.cod-chave-param-ext    = c-cod-chave-param-ext AND
             bc-param-ext.cod-entidade-param-ext = 'bc-tipo-trans' AND   
             bc-param-ext.cod-param-ext          = ("NR-SEQ-ETIQ" + "-" + TRIM(doc-fisico.cod-estabel))
             SHARE-LOCK NO-ERROR.
        IF  NOT AVAIL bc-param-ext THEN DO:
            MESSAGE "Os parÉmetros da transaá∆o IMA0001Q n∆o foram encontrados para este estabelecimento!" SKIP
                     "Verifique este problema com o administrador do m¢dulo Coleta de Dados!" VIEW-AS ALERT-BOX ERROR TITLE "Erro".
            RETURN "ADM-ERROR":U.
        END.
        ELSE DO: 
             ASSIGN i-seq                      = bc-param-ext.param-inteiro + 1
                    bc-param-ext.param-inteiro = i-seq.
             FIND CURRENT bc-param-ext NO-LOCK NO-ERROR.
        END.
    
         CREATE bc-etiqueta.
         ASSIGN bc-etiqueta.serie            = b-tt-etiq-aux.serie
                bc-etiqueta.cod-estabel      = doc-fisico.cod-estabel
                bc-etiqueta.nr-nota-fis      = b-tt-etiq-aux.nr-nota-fis
                bc-etiqueta.nome-abrev       = IF AVAIL bf-emit THEN bf-emit.nome-abrev ELSE ""
                bc-etiqueta.cod-estado       = 1
                bc-etiqueta.it-codigo        = b-tt-etiq-aux.it-codigo
                bc-etiqueta.qt-item          = b-tt-etiq-aux.qt-item
                bc-etiqueta.num-pedido       = b-tt-etiq-aux.num-pedido
                bc-etiqueta.qt-un-1          = b-tt-etiq-aux.qt-item
                bc-etiqueta.un               = b-tt-etiq-aux.un
                bc-etiqueta.dt-criacao       = b-tt-etiq-aux.dt-criacao
                bc-etiqueta.hr-criacao       = b-tt-etiq-aux.hr-criacao
                bc-etiqueta.usuar-criacao    = b-tt-etiq-aux.usuar-criacao
                bc-etiqueta.nr-seq-fat       = b-tt-etiq-aux.nr-seq-fat
                bc-etiqueta.cd-trans         = "IMA0001Q"
                bc-etiqueta.progressivo      = STRING(i-ep-codigo-usuario,"9") + trim(string(doc-fisico.cod-estabel,"x(03)")) + STRING(i-seq,"999999999")           
                bc-etiqueta.referencia       = bf-it-doc-fisico.cod-refer
                bc-etiqueta.lote             = bf-it-doc-fisico.cod-refer
                bc-etiqueta.nr-romaneio      = bf-it-doc-fisico.sequencia
                bc-etiqueta.cod-layout       = 1
                bc-etiqueta.num-versao       = 1
                bc-etiqueta.inteiro-livre-2  = b-tt-etiq-aux.inteiro-livre-2
                bc-etiqueta.log-datasul      = NO.

         /* Cria Etiqueta Nova Logistica */
         FIND ITEM WHERE
              ITEM.it-codigo = bc-etiqueta.it-codigo NO-LOCK NO-ERROR.

         ASSIGN i-tp-embal = 1.
         IF ITEM.un = 'kg' THEN 
            ASSIGN i-tp-embal = 5.

         FIND corte-comerc WHERE
              corte-comerc.compr-min <= bc-etiqueta.qt-item AND
              corte-comerc.compr-max >= bc-etiqueta.qt-item AND
              corte-comerc.tp-embalag = i-tp-embal NO-LOCK NO-ERROR.

         CREATE ob-etiqueta.
         ASSIGN ob-etiqueta.cod-estabel     = bc-etiqueta.cod-estabel
                ob-etiqueta.dt-emissao      = TODAY
                ob-etiqueta.hr-emissao      = STRING(TIME,"HH:MM")
                ob-etiqueta.acondic         = ""
                ob-etiqueta.it-codigo       = bc-etiqueta.it-codigo
                ob-etiqueta.cod-refer       = bc-etiqueta.referencia
                ob-etiqueta.nr-lote         = IF bc-etiqueta.lote = '888'
                                              THEN 'RD' ELSE 'RP'
                ob-etiqueta.cod-qualid      = IF bc-etiqueta.lote = '888'
                                              THEN 'D' ELSE 'B'
                ob-etiqueta.corte-comerc    = IF AVAIL corte-comerc
                                              THEN corte-comerc.codigo
                                              ELSE ''
                ob-etiqueta.quantidade      = bc-etiqueta.qt-item
                ob-etiqueta.ob-origem       = STRING(bc-etiqueta.num-pedido)
                ob-etiqueta.localizacao     = ''
                ob-etiqueta.situacao        = 3
                ob-etiqueta.num-etiqueta    = IF bc-etiqueta.cod-estabel = '1' 
                                              THEN NEXT-VALUE(seq-etq-estoq-ima)
                                              ELSE NEXT-VALUE(seq-etq-estoq-med)
                ob-etiqueta.progressivo     = bc-etiqueta.progressivo.

         IF  i-cont = 1 then
             ASSIGN b-tt-etiq-aux.progressivo      = STRING(i-ep-codigo-usuario,"9") + trim(string(doc-fisico.cod-estabel,"x(03)")) + STRING(i-seq,"999999999")
                    b-tt-etiq-aux.log-registro     = NO
                    b-tt-etiq-aux.nr-etiq          = bc-etiqueta.nr-etiq
                    b-tt-etiq-aux.r-rowid          = ROWID(bc-etiqueta)
                    b-tt-etiq-aux.inteiro-livre-1  = 1.

         
         IF  i-cont > 1 THEN DO:
             CREATE bf-tt-etiq.
             ASSIGN bf-tt-etiq.serie            = bc-etiqueta.serie
/*                     bf-tt-etiq.nr-etiq          = bc-etiqueta.nr-etiq  */
                    bf-tt-etiq.cod-estabel      = bc-etiqueta.cod-estabel
                    bf-tt-etiq.nr-nota-fis      = bc-etiqueta.nr-nota-fis
                    bf-tt-etiq.nome-abrev       = bc-etiqueta.nome-abrev
                    bf-tt-etiq.cod-estado       = 1
                    bf-tt-etiq.it-codigo        = bc-etiqueta.it-codigo
                    bf-tt-etiq.qt-item          = bc-etiqueta.qt-item
                    bf-tt-etiq.num-pedido       = bc-etiqueta.num-pedido
                    bf-tt-etiq.qt-un-1          = bc-etiqueta.qt-item
                    bf-tt-etiq.un               = bc-etiqueta.un
                    bf-tt-etiq.dt-criacao       = bc-etiqueta.dt-criacao
                    bf-tt-etiq.hr-criacao       = bc-etiqueta.hr-criacao
                    bf-tt-etiq.usuar-criacao    = bc-etiqueta.usuar-criacao
                    bf-tt-etiq.nr-seq-fat       = bc-etiqueta.nr-seq-fat
                    bf-tt-etiq.cd-trans         = "IMA0001Q"
                    bf-tt-etiq.progressivo      = bc-etiqueta.progressivo
                    bf-tt-etiq.referencia       = bc-etiqueta.referencia
                    bf-tt-etiq.lote             = bc-etiqueta.lote
                    bf-tt-etiq.num-pedido       = bc-etiqueta.num-pedido
                    bf-tt-etiq.nr-romaneio      = bc-etiqueta.nr-romaneio
                    bf-tt-etiq.cod-layout       = 1
                    bf-tt-etiq.num-versao       = 1
                    bf-tt-etiq.log-datasul      = NO
                    bf-tt-etiq.log-registro     = NO
                    bf-tt-etiq.r-rowid          = ROWID(bc-etiqueta)
                    bf-tt-etiq.inteiro-livre-1  = 1
                    bf-tt-etiq.inteiro-livre-2  = bc-etiqueta.inteiro-livre-2
                    /*bf-tt-etiq.inteiro-livre-2  = (b-tt-etiq-aux.inteiro-livre-2 - 1) + i-cont*/.
         END.
     END.
 END.
 {&OPEN-QUERY-{&BROWSE-NAME}}
 APPLY "row-entry" TO br-table IN FRAME f-cad.

FOR EACH  b-tt-etiq-aux
     WHERE  b-tt-etiq-aux.nr-nota-fis   = bf-it-doc-fisico.nro-docto
       AND b-tt-etiq-aux.serie         = bf-it-doc-fisico.serie-docto:
     IF  b-tt-etiq-aux.cod-estado    = 2 THEN 
         br-table:SELECT-next-row() NO-ERROR.
     ELSE 
         LEAVE.
END.


  APPLY "value-changed" TO br-table IN FRAME {&FRAME-NAME}.
 /*ASSIGN bt-imprime:SENSITIVE       IN FRAME {&FRAME-NAME} = YES
        bt-imprime-todos:SENSITIVE IN FRAME {&FRAME-NAME} = YES
        bt-confirma:SENSITIVE      IN FRAME {&FRAME-NAME} = NO. */

  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-habilitar-botao w-cadpaifilho-filho 
PROCEDURE pi-habilitar-botao :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM p-log-habilita AS LOGICAL.

ASSIGN bt-confirma:SENSITIVE IN FRAME {&FRAME-NAME} = p-log-habilita.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-mostra-filhos w-cadpaifilho-filho 
PROCEDURE pi-mostra-filhos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 ASSIGN l-hab-todos   = NO
        d-qt-recebido = 0
        d-qt-lido     = 0.

 IF  gr-it-doc-fisico <> ? THEN DO:

    ASSIGN bt-incluir:SENSITIVE       IN FRAME {&FRAME-NAME} = YES
           bt-modificar:SENSITIVE     IN FRAME {&FRAME-NAME} = YES
           bt-eliminar:SENSITIVE      IN FRAME {&FRAME-NAME} = YES.
     FIND bf-it-doc-fisico
         WHERE ROWID(bf-it-doc-fisico) = gr-it-doc-fisico
         NO-LOCK NO-ERROR.
     IF  AVAIL bf-it-doc-fisico THEN
         {&OPEN-QUERY-{&BROWSE-NAME}}
     ELSE
         MESSAGE "Item do Recebimento F°sico n∆o dispon°vel" VIEW-AS ALERT-BOX.
    APPLY 'value-changed' TO br-table IN FRAME {&FRAME-NAME}.
    RUN pi-calcula-recebido .    
 END.

 /*DISPLAY d-qt-recebido d-qt-lido WITH FRAME {&FRAME-NAME}.*/

 ASSIGN bt-imprime-todos:SENSITIVE IN FRAME {&FRAME-NAME} = l-hab-todos
        bt-confirma:SENSITIVE IN FRAME {&FRAME-NAME}      = l-hab-efet.
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-cadpaifilho-filho  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-etiqueta"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-cadpaifilho-filho 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     Manuseia trocas de estado dos SmartObjects
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.

  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

