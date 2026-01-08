&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME w-cadsim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-cadsim 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESSP0129 2.04.00.000}


/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF TEMP-TABLE tt-requisicao
    FIELD padrao        LIKE mp-fardo.padrao
    FIELD codificacao   AS CHAR FORMAT "x(3)" 
    FIELD qtd-estoq     AS INT FORMAT ">>>9" 
    FIELD perc-estoq    AS DEC FORMAT ">>9.99" 
    FIELD fator         AS DEC FORMAT ">9" 
    FIELD qtd-fd-req    AS INT FORMAT ">>>9" 
    FIELD marca         AS CHAR FORMAT "x"
    INDEX indice1 codificacao ASC padrao ASC.

DEF TEMP-TABLE tt-distribuicao
    FIELD nr-seq        AS   INT
    FIELD padrao        LIKE mp-fardo.padrao
    FIELD codificacao   AS CHAR FORMAT "x(3)"
    INDEX indice1 IS PRIMARY UNIQUE nr-seq.

DEF BUFFER b-distribuicao FOR tt-distribuicao.

DEF VAR c-classif      AS CHAR.
DEF VAR i-ct           AS INT.
DEF VAR i-nr-seq       AS INT.
DEF VAR v-row-table    AS ROWID.
DEF VAR c-marca        AS CHAR INIT "*".
DEF VAR i-qtd-fardos   AS INT.
DEF VAR i-qtd-bancadas AS INT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-cadsim
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad
&Scoped-define BROWSE-NAME br-distribuicao

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-distribuicao tt-requisicao

/* Definitions for BROWSE br-distribuicao                               */
&Scoped-define FIELDS-IN-QUERY-br-distribuicao tt-distribuicao.codificacao tt-distribuicao.padrao   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-distribuicao   
&Scoped-define SELF-NAME br-distribuicao
&Scoped-define QUERY-STRING-br-distribuicao FOR EACH tt-distribuicao
&Scoped-define OPEN-QUERY-br-distribuicao OPEN QUERY {&SELF-NAME} FOR EACH tt-distribuicao.
&Scoped-define TABLES-IN-QUERY-br-distribuicao tt-distribuicao
&Scoped-define FIRST-TABLE-IN-QUERY-br-distribuicao tt-distribuicao


/* Definitions for BROWSE br-requisicao                                 */
&Scoped-define FIELDS-IN-QUERY-br-requisicao tt-requisicao.codificacao tt-requisicao.padrao tt-requisicao.qtd-estoq tt-requisicao.perc-estoq tt-requisicao.fator tt-requisicao.qtd-fd-req tt-requisicao.marca   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-requisicao tt-requisicao.fator   
&Scoped-define ENABLED-TABLES-IN-QUERY-br-requisicao tt-requisicao
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br-requisicao tt-requisicao
&Scoped-define SELF-NAME br-requisicao
&Scoped-define QUERY-STRING-br-requisicao FOR EACH tt-requisicao NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-requisicao OPEN QUERY {&SELF-NAME} FOR EACH tt-requisicao NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-requisicao tt-requisicao
&Scoped-define FIRST-TABLE-IN-QUERY-br-requisicao tt-requisicao


/* Definitions for FRAME f-cad                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-cad ~
    ~{&OPEN-QUERY-br-distribuicao}~
    ~{&OPEN-QUERY-br-requisicao}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-button bt-imprime br-requisicao ~
br-distribuicao 
&Scoped-Define DISPLAYED-OBJECTS fi-tot-estoq fi-tot-perc fi-tot-fator ~
fi-tot-req fi-fd-bancada 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-6 bt-marca bt-desmarca bt-up bt-todos bt-down bt-nenhum 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-cadsim AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU mi-arquivo 
       MENU-ITEM mi-primeiro    LABEL "&Primeiro"      ACCELERATOR "CTRL-HOME"
       MENU-ITEM mi-anterior    LABEL "An&terior"      ACCELERATOR "CTRL-CURSOR-LEFT"
       MENU-ITEM mi-proximo     LABEL "Pr&¢ximo"       ACCELERATOR "CTRL-CURSOR-RIGHT"
       MENU-ITEM mi-ultimo      LABEL "&éltimo"        ACCELERATOR "CTRL-END"
       MENU-ITEM mi-va-para     LABEL "&V  para"       ACCELERATOR "CTRL-T"
       MENU-ITEM mi-pesquisa    LABEL "Pes&quisa"      ACCELERATOR "CTRL-F5"
       RULE
       MENU-ITEM mi-incluir     LABEL "&Incluir"       ACCELERATOR "CTRL-INS"
       MENU-ITEM mi-copiar      LABEL "C&opiar"        ACCELERATOR "CTRL-C"
       MENU-ITEM mi-alterar     LABEL "A&lterar"       ACCELERATOR "CTRL-A"
       MENU-ITEM mi-eliminar    LABEL "&Eliminar"      ACCELERATOR "CTRL-DEL"
       RULE
       MENU-ITEM mi-desfazer    LABEL "&Desfazer"      ACCELERATOR "CTRL-U"
       MENU-ITEM mi-cancelar    LABEL "&Cancelar"      ACCELERATOR "CTRL-F4"
       RULE
       MENU-ITEM mi-salvar      LABEL "Sal&var"        ACCELERATOR "CTRL-S"
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
       SUB-MENU  mi-ajuda       LABEL "A&juda"        .


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_p-cadsim AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-exihel AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-navega AS HANDLE NO-UNDO.
DEFINE VARIABLE h_q01es066 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v01es066 AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-desmarca 
     IMAGE-UP FILE "image/im-rml.bmp":U
     LABEL "Desm" 
     SIZE 6 BY 1.13 TOOLTIP "Desmarca PadrÆo".

DEFINE BUTTON bt-down 
     IMAGE-UP FILE "image/im-down.bmp":U
     LABEL "Button 5" 
     SIZE 5.57 BY 1.75 TOOLTIP "Desce PadrÆo / Codifica‡Æo".

DEFINE BUTTON bt-imprime 
     IMAGE-UP FILE "image/im-pri.bmp":U
     LABEL "Button 1" 
     SIZE 4 BY 1.25.

DEFINE BUTTON bt-marca 
     IMAGE-UP FILE "image/im-inl.bmp":U
     LABEL "Marca" 
     SIZE 6 BY 1.13 TOOLTIP "Marca PadrÆo".

DEFINE BUTTON bt-nenhum 
     IMAGE-UP FILE "image/im-nenhum.bmp":U
     LABEL "Nenhum" 
     SIZE 6 BY 1.13 TOOLTIP "Desmarca Todos".

DEFINE BUTTON bt-todos 
     IMAGE-UP FILE "image/im-todos.bmp":U
     LABEL "Todos" 
     SIZE 6 BY 1.13 TOOLTIP "Marca Todos".

DEFINE BUTTON bt-up 
     IMAGE-UP FILE "image/im-uptear.bmp":U
     LABEL "Button 6" 
     SIZE 5.57 BY 1.63 TOOLTIP "Sobe PadrÆo / Codifica‡Æo".

DEFINE VARIABLE fi-fd-bancada AS INTEGER FORMAT "ZZ":U INITIAL 0 
     LABEL "Fardos por Bancada" 
     VIEW-AS FILL-IN 
     SIZE 3.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-tot-estoq AS INTEGER FORMAT "ZZZZZZZZZZ":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-tot-fator AS INTEGER FORMAT "ZZZ":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 3.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-tot-perc AS DECIMAL FORMAT ">>9.9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-tot-req AS INTEGER FORMAT "ZZZZZZZZZZZZ":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 7.57 BY .88
     FONT 1 NO-UNDO.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 89.43 BY 1.46
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-distribuicao FOR 
      tt-distribuicao SCROLLING.

DEFINE QUERY br-requisicao FOR 
      tt-requisicao SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-distribuicao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-distribuicao w-cadsim _FREEFORM
  QUERY br-distribuicao DISPLAY
      tt-distribuicao.codificacao  COLUMN-LABEL "Cod"       WIDTH 5
      tt-distribuicao.padrao       COLUMN-LABEL "PadrÆo"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 23.29 BY 12
         FONT 1
         TITLE "Distribui‡Æo".

DEFINE BROWSE br-requisicao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-requisicao w-cadsim _FREEFORM
  QUERY br-requisicao NO-LOCK DISPLAY
      tt-requisicao.codificacao  COLUMN-LABEL "Cod"       WIDTH 5
      tt-requisicao.padrao       COLUMN-LABEL "PadrÆo" 
      tt-requisicao.qtd-estoq    COLUMN-LABEL "Qtd Estoq" 
      tt-requisicao.perc-estoq   COLUMN-LABEL "%" 
      tt-requisicao.fator        COLUMN-LABEL "Fator" 
      tt-requisicao.qtd-fd-req   COLUMN-LABEL "Qtd Requis"
      tt-requisicao.marca        COLUMN-LABEL "M" COLUMN-FGCOLOR 12 WIDTH 2 COLUMN-FONT 0
ENABLE 
      tt-requisicao.fator
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 51 BY 12
         FONT 1
         TITLE "Fardos Dispon¡veis".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     bt-imprime AT ROW 1.33 COL 62.72
     br-requisicao AT ROW 5.75 COL 2
     br-distribuicao AT ROW 5.75 COL 61
     bt-marca AT ROW 12.92 COL 53.29
     bt-desmarca AT ROW 14.17 COL 53.29
     bt-up AT ROW 14.29 COL 84.72
     bt-todos AT ROW 15.42 COL 53.29
     bt-down AT ROW 16.04 COL 84.72
     bt-nenhum AT ROW 16.67 COL 53.29
     fi-tot-estoq AT ROW 17.92 COL 20.72 COLON-ALIGNED NO-LABEL
     fi-tot-perc AT ROW 17.92 COL 28 COLON-ALIGNED NO-LABEL
     fi-tot-fator AT ROW 17.92 COL 33.43 COLON-ALIGNED NO-LABEL
     fi-tot-req AT ROW 17.92 COL 37.57 COLON-ALIGNED NO-LABEL
     fi-fd-bancada AT ROW 17.92 COL 75.29 COLON-ALIGNED
     "Totais:" VIEW-AS TEXT
          SIZE 6 BY .75 AT ROW 18 COL 15.86
          FONT 6
     rt-button AT ROW 1.21 COL 1.57
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90.57 BY 18.04
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-cadsim
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-cadsim ASSIGN
         HIDDEN             = YES
         TITLE              = "Manuten‡Æo Programa‡Æo de Mistura"
         HEIGHT             = 18.04
         WIDTH              = 90.57
         MAX-HEIGHT         = 18.13
         MAX-WIDTH          = 94.57
         VIRTUAL-HEIGHT     = 18.13
         VIRTUAL-WIDTH      = 94.57
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-cadsim 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-cadsim.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-cadsim
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-cad
   FRAME-NAME L-To-R                                                    */
/* BROWSE-TAB br-requisicao bt-imprime f-cad */
/* BROWSE-TAB br-distribuicao br-requisicao f-cad */
/* SETTINGS FOR BUTTON bt-desmarca IN FRAME f-cad
   NO-ENABLE 6                                                          */
/* SETTINGS FOR BUTTON bt-down IN FRAME f-cad
   NO-ENABLE 6                                                          */
/* SETTINGS FOR BUTTON bt-marca IN FRAME f-cad
   NO-ENABLE 6                                                          */
/* SETTINGS FOR BUTTON bt-nenhum IN FRAME f-cad
   NO-ENABLE 6                                                          */
/* SETTINGS FOR BUTTON bt-todos IN FRAME f-cad
   NO-ENABLE 6                                                          */
/* SETTINGS FOR BUTTON bt-up IN FRAME f-cad
   NO-ENABLE 6                                                          */
/* SETTINGS FOR FILL-IN fi-fd-bancada IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-estoq IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-fator IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-perc IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-req IN FRAME f-cad
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-cadsim)
THEN w-cadsim:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-distribuicao
/* Query rebuild information for BROWSE br-distribuicao
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-distribuicao.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-distribuicao */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-requisicao
/* Query rebuild information for BROWSE br-requisicao
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-requisicao NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-requisicao */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-cadsim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-cadsim w-cadsim
ON END-ERROR OF w-cadsim /* Manuten‡Æo Programa‡Æo de Mistura */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-cadsim w-cadsim
ON WINDOW-CLOSE OF w-cadsim /* Manuten‡Æo Programa‡Æo de Mistura */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-desmarca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-desmarca w-cadsim
ON CHOOSE OF bt-desmarca IN FRAME f-cad /* Desm */
DO:
    ASSIGN tt-requisicao.marca = ''. 
    FOR EACH tt-distribuicao WHERE
             tt-distribuicao.padrao = tt-requisicao.padrao AND
             tt-distribuicao.codificacao = tt-requisicao.codificacao EXCLUSIVE-LOCK.
        DELETE tt-distribuicao.
        ASSIGN fi-fd-bancada = fi-fd-bancada - 1.
    END.
    {&OPEN-QUERY-br-requisicao}
    {&OPEN-QUERY-br-distribuicao}

    DISP fi-fd-bancada
         WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-down
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-down w-cadsim
ON CHOOSE OF bt-down IN FRAME f-cad /* Button 5 */
DO:
    FIND b-distribuicao WHERE
         b-distribuicao.nr-seq = tt-distribuicao.nr-seq NO-ERROR.
    FIND NEXT b-distribuicao NO-ERROR.
    IF AVAIL b-distribuicao THEN DO.
       ASSIGN i-nr-seq = tt-distribuicao.nr-seq.
       ASSIGN tt-distribuicao.nr-seq = b-distribuicao.nr-seq
              b-distribuicao.nr-seq  = i-nr-seq.

       ASSIGN bt-up:SENSITIVE   = YES
              bt-down:SENSITIVE = YES.
    END.
    ELSE 
       ASSIGN bt-up:SENSITIVE   = YES
              bt-down:SENSITIVE = NO.

    ASSIGN v-row-table = ROWID(tt-distribuicao).
    {&OPEN-QUERY-br-distribuicao}
    br-distribuicao:QUERY:REPOSITION-TO-ROWID(v-row-table) NO-ERROR.   
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-imprime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-imprime w-cadsim
ON CHOOSE OF bt-imprime IN FRAME f-cad /* Button 1 */
DO:
   RUN pi-imprime IN h_v01es066.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-marca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-marca w-cadsim
ON CHOOSE OF bt-marca IN FRAME f-cad /* Marca */
DO:
  FIND FIRST tt-distribuicao WHERE
             tt-distribuicao.padrao      = tt-requisicao.padrao AND
             tt-distribuicao.codificacao = tt-requisicao.codificacao NO-LOCK NO-ERROR.
  IF AVAIL tt-distribuicao THEN
     RETURN NO-APPLY.

  IF tt-requisicao.fator + fi-fd-bancada > i-qtd-fardos THEN DO.
     MESSAGE "Quantidade Requisitada maior que permitido por Bancada..."
         VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     RETURN NO-APPLY.
  END.

  ASSIGN tt-requisicao.marca = c-marca.

  FIND LAST tt-distribuicao NO-ERROR.
  ASSIGN i-nr-seq = IF AVAIL tt-distribuicao THEN
                    tt-distribuicao.nr-seq ELSE 1.

  DO i-ct = 1 TO tt-requisicao.fator.
     ASSIGN i-nr-seq = i-nr-seq + 1.
     CREATE tt-distribuicao.
     ASSIGN tt-distribuicao.nr-seq      = i-nr-seq
            tt-distribuicao.padrao      = tt-requisicao.padrao 
            tt-distribuicao.codificacao = tt-requisicao.codificacao.

     ASSIGN fi-fd-bancada = fi-fd-bancada + 1.
  END.

  ASSIGN v-row-table = ROWID(tt-requisicao).

  {&OPEN-QUERY-br-requisicao}
  {&OPEN-QUERY-br-distribuicao}

  br-requisicao:QUERY:REPOSITION-TO-ROWID(v-row-table) NO-ERROR.   

  DISP fi-fd-bancada
       WITH FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-nenhum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-nenhum w-cadsim
ON CHOOSE OF bt-nenhum IN FRAME f-cad /* Nenhum */
DO:
   FOR EACH tt-requisicao EXCLUSIVE-LOCK.
       ASSIGN tt-requisicao.marca = ''. 

       FOR EACH tt-distribuicao WHERE
                tt-distribuicao.padrao = tt-requisicao.padrao AND
                tt-distribuicao.codificacao = tt-requisicao.codificacao EXCLUSIVE-LOCK.
           DELETE tt-distribuicao.
       END.
       ASSIGN fi-fd-bancada = 0.
   END.

   {&OPEN-QUERY-br-requisicao}
   {&OPEN-QUERY-br-distribuicao}
    DISP fi-fd-bancada
         WITH FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-todos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-todos w-cadsim
ON CHOOSE OF bt-todos IN FRAME f-cad /* Todos */
DO:
  FOR EACH tt-distribuicao WHERE
           tt-distribuicao.padrao = tt-requisicao.padrao AND
           tt-distribuicao.codificacao = tt-requisicao.codificacao EXCLUSIVE-LOCK.
      DELETE tt-distribuicao.
  END.

  ASSIGN i-nr-seq = 0.
  FOR EACH tt-requisicao EXCLUSIVE-LOCK.
      ASSIGN tt-requisicao.marca = c-marca. 

      DO i-ct = 1 TO tt-requisicao.fator.
         ASSIGN i-nr-seq = i-nr-seq + 1.
         CREATE tt-distribuicao.
         ASSIGN tt-distribuicao.nr-seq      = i-nr-seq
                tt-distribuicao.padrao      = tt-requisicao.padrao 
                tt-distribuicao.codificacao = tt-requisicao.codificacao.
         ASSIGN fi-fd-bancada = fi-fd-bancada + 1.
      END.
  END.
  {&OPEN-QUERY-br-requisicao}
  {&OPEN-QUERY-br-distribuicao}
   DISP fi-fd-bancada
        WITH FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-up
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-up w-cadsim
ON CHOOSE OF bt-up IN FRAME f-cad /* Button 6 */
DO:
   FIND b-distribuicao WHERE
        b-distribuicao.nr-seq = tt-distribuicao.nr-seq NO-ERROR.
   FIND PREV b-distribuicao NO-ERROR.
   IF AVAIL b-distribuicao THEN DO.
      ASSIGN i-nr-seq = tt-distribuicao.nr-seq.
      ASSIGN tt-distribuicao.nr-seq = b-distribuicao.nr-seq
             b-distribuicao.nr-seq = i-nr-seq.
      ASSIGN bt-up:SENSITIVE   = YES
             bt-down:SENSITIVE = YES.
   END.
   ELSE 
      ASSIGN bt-up:SENSITIVE = NO
             bt-down:SENSITIVE = YES.

   ASSIGN v-row-table = ROWID(tt-distribuicao).
   {&OPEN-QUERY-br-distribuicao}
   br-distribuicao:QUERY:REPOSITION-TO-ROWID(v-row-table) NO-ERROR.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-alterar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-alterar w-cadsim
ON CHOOSE OF MENU-ITEM mi-alterar /* Alterar */
DO:
  RUN pi-alterar IN h_p-cadsim.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-anterior
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-anterior w-cadsim
ON CHOOSE OF MENU-ITEM mi-anterior /* Anterior */
DO:
  RUN pi-anterior IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-arquivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-arquivo w-cadsim
ON MENU-DROP OF MENU mi-arquivo /* Arquivo */
DO:
  run pi-disable-menu.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-cancelar w-cadsim
ON CHOOSE OF MENU-ITEM mi-cancelar /* Cancelar */
DO:
  RUN pi-cancelar IN h_p-cadsim.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-consultas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-consultas w-cadsim
ON CHOOSE OF MENU-ITEM mi-consultas /* Consultas */
DO:
  RUN pi-consulta IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-conteudo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-conteudo w-cadsim
ON CHOOSE OF MENU-ITEM mi-conteudo /* Conteudo */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  RUN pi-ajuda IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-copiar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-copiar w-cadsim
ON CHOOSE OF MENU-ITEM mi-copiar /* Copiar */
DO:
  RUN pi-copiar IN h_p-cadsim.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-desfazer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-desfazer w-cadsim
ON CHOOSE OF MENU-ITEM mi-desfazer /* Desfazer */
DO:
  RUN pi-desfazer IN h_p-cadsim.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-eliminar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-eliminar w-cadsim
ON CHOOSE OF MENU-ITEM mi-eliminar /* Eliminar */
DO:
  RUN pi-eliminar IN h_p-cadsim.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-imprimir w-cadsim
ON CHOOSE OF MENU-ITEM mi-imprimir /* Relat¢rios */
DO:
  RUN pi-imprimir IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-incluir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-incluir w-cadsim
ON CHOOSE OF MENU-ITEM mi-incluir /* Incluir */
DO:
  RUN pi-incluir IN h_p-cadsim.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-pesquisa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-pesquisa w-cadsim
ON CHOOSE OF MENU-ITEM mi-pesquisa /* Pesquisa */
DO:
  RUN pi-pesquisa IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-primeiro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-primeiro w-cadsim
ON CHOOSE OF MENU-ITEM mi-primeiro /* Primeiro */
DO:
  RUN pi-primeiro IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-proximo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-proximo w-cadsim
ON CHOOSE OF MENU-ITEM mi-proximo /* Pr¢ximo */
DO:
  RUN pi-proximo IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sair w-cadsim
ON CHOOSE OF MENU-ITEM mi-sair /* Sair */
DO:
  RUN pi-sair IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-salvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-salvar w-cadsim
ON CHOOSE OF MENU-ITEM mi-salvar /* Salvar */
DO:
  RUN pi-salvar IN h_p-cadsim.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sobre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sobre w-cadsim
ON CHOOSE OF MENU-ITEM mi-sobre /* Sobre... */
DO:
  {include/sobre.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-ultimo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-ultimo w-cadsim
ON CHOOSE OF MENU-ITEM mi-ultimo /* éltimo */
DO:
  RUN pi-ultimo IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-va-para
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-va-para w-cadsim
ON CHOOSE OF MENU-ITEM mi-va-para /* V  para */
DO:
  RUN pi-vapara IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-distribuicao
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-cadsim 


/* ***************************  Main Block  *************************** */
ON 'leave':U OF  tt-requisicao.fator IN BROWSE br-requisicao DO:
    ASSIGN tt-requisicao.qtd-fd-req:SCREEN-VALUE IN BROWSE br-requisicao = SELF:INPUT-VALUE * i-qtd-bancadas.
    ASSIGN INPUT BROWSE br-requisicao tt-requisicao.qtd-fd-req.
END.


/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-cadsim  _ADM-CREATE-OBJECTS
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
             INPUT  'panel/p-navega.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Edge-Pixels = 0,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-navega ).
       RUN set-position IN h_p-navega ( 1.33 , 2.14 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 24.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panel/p-cadsim.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Edge-Pixels = 0,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-cadsim ).
       RUN set-position IN h_p-cadsim ( 1.33 , 27.29 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 28.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panel/p-exihel.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Edge-Pixels = 0,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-exihel ).
       RUN set-position IN h_p-exihel ( 1.33 , 74.29 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'esvwr/v01es066.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v01es066 ).
       RUN set-position IN h_v01es066 ( 2.75 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.50 , 88.57 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'esqry/q01es066.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'ProgPesquisa = eszoom\z01es066.w,
                     ProgVaPara = esgo\g01es066.w,
                     ProgIncMod = ,
                     Implantar = no':U ,
             OUTPUT h_q01es066 ).
       RUN set-position IN h_q01es066 ( 1.25 , 67.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 7.72 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-cadsim , 'State':U , h_p-exihel ).

       /* Links to SmartViewer h_v01es066. */
       RUN add-link IN adm-broker-hdl ( h_p-cadsim , 'TableIO':U , h_v01es066 ).
       RUN add-link IN adm-broker-hdl ( h_q01es066 , 'Record':U , h_v01es066 ).

       /* Links to SmartQuery h_q01es066. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , h_q01es066 ).
       RUN add-link IN adm-broker-hdl ( h_p-navega , 'Navigation':U , h_q01es066 ).
       RUN add-link IN adm-broker-hdl ( h_p-navega , 'State':U , h_q01es066 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-navega ,
             bt-imprime:HANDLE IN FRAME f-cad , 'BEFORE':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-cadsim ,
             h_p-navega , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             bt-imprime:HANDLE IN FRAME f-cad , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v01es066 ,
             h_p-exihel , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-cadsim  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-cadsim  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-cadsim)
  THEN DELETE WIDGET w-cadsim.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-cadsim  _DEFAULT-ENABLE
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
  DISPLAY fi-tot-estoq fi-tot-perc fi-tot-fator fi-tot-req fi-fd-bancada 
      WITH FRAME f-cad IN WINDOW w-cadsim.
  ENABLE rt-button bt-imprime br-requisicao br-distribuicao 
      WITH FRAME f-cad IN WINDOW w-cadsim.
  {&OPEN-BROWSERS-IN-QUERY-f-cad}
  VIEW w-cadsim.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-cadsim 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-cadsim 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-cadsim 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  {include/win-size.i}

  run pi-before-initialize.

  {utp/ut9000.i "ESSP0129" "2.04.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
 
  RUN enable-modifica IN h_p-cadsim (INPUT NO).

  RUN enable-copia IN h_p-cadsim (INPUT NO).

  RUN pi-ultimo IN h_p-navega.

  run pi-after-initialize.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-busca w-cadsim 
PROCEDURE pi-busca :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF OUTPUT PARAMETER p-fd-bancada AS INT.
    ASSIGN p-fd-bancada = fi-fd-bancada:INPUT-VALUE IN FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-grava w-cadsim 
PROCEDURE pi-grava :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-nr-mistura LIKE mp-mistura.nr-mistura.

    FOR EACH tt-requisicao WHERE
             tt-requisicao.marca = '*' NO-LOCK.
        CREATE mp-comp-mistura.
        ASSIGN mp-comp-mistura.nr-mistura  = p-nr-mistura
               mp-comp-mistura.padrao      = tt-requisicao.padrao
               mp-comp-mistura.codificacao = tt-requisicao.cod
               mp-comp-mistura.qtd-estoq   = tt-requisicao.qtd-estoq
               mp-comp-mistura.perc-estoq  = tt-requisicao.perc-estoq
               mp-comp-mistura.fator       = tt-requisicao.fator
               mp-comp-mistura.qtd-fd-req  = tt-requisicao.qtd-fd-req.
    END.

    FOR EACH tt-distribuicao NO-LOCK.
        CREATE mp-distribuicao.
        ASSIGN mp-distribuicao.nr-mistura  = p-nr-mistura
               mp-distribuicao.nr-seq      = tt-distribuicao.nr-seq
               mp-distribuicao.padrao      = tt-distribuicao.padrao
               mp-distribuicao.codificacao = tt-distribuicao.codificacao.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-habilita w-cadsim 
PROCEDURE pi-habilita :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-acao AS LOG.
    RUN enable-elimina IN h_p-cadsim (INPUT p-acao).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-mostra w-cadsim 
PROCEDURE pi-mostra :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-nr-mistura LIKE mp-mistura.nr-mistura.

    FOR EACH tt-requisicao.
        DELETE tt-requisicao.
    END.

    FOR EACH tt-distribuicao.
        DELETE tt-distribuicao.
    END.
    
    ASSIGN fi-tot-estoq  = 0
           fi-tot-req    = 0
           fi-tot-fator  = 0
           fi-tot-perc   = 0
           fi-fd-bancada = 0.

    FOR EACH mp-comp-mistura WHERE
             mp-comp-mistura.nr-mistura = p-nr-mistura NO-LOCK.
        CREATE tt-requisicao.
        ASSIGN tt-requisicao.padrao      = mp-comp-mistura.padrao 
               tt-requisicao.codificacao = mp-comp-mistura.codificacao
               tt-requisicao.qtd-estoq   = mp-comp-mistura.qtd-estoq   
               tt-requisicao.perc-estoq  = mp-comp-mistura.perc-estoq  
               tt-requisicao.fator       = mp-comp-mistura.fator       
               tt-requisicao.qtd-fd-req  = mp-comp-mistura.qtd-fd-req
               tt-requisicao.marca       = '*'.  

        ASSIGN fi-tot-estoq = fi-tot-estoq + mp-comp-mistura.qtd-estoq
               fi-tot-req   = fi-tot-req   + mp-comp-mistura.qtd-fd-req
               fi-tot-fator = fi-tot-fator + mp-comp-mistura.fator
               fi-tot-perc  = fi-tot-perc  + mp-comp-mistura.perc-estoq.
    END.

    FOR EACH mp-distribuicao WHERE
             mp-distribuicao.nr-mistura = p-nr-mistura NO-LOCK.
        CREATE tt-distribuicao.
        ASSIGN tt-distribuicao.nr-seq      = mp-distribuicao.nr-seq
               tt-distribuicao.padrao      = mp-distribuicao.padrao 
               tt-distribuicao.codificacao = mp-distribuicao.codificacao.

        ASSIGN fi-fd-bancada = fi-fd-bancada + 1.
    END.

    {&OPEN-QUERY-br-requisicao}
    {&OPEN-QUERY-br-distribuicao}

    DISABLE {&list-6} WITH FRAME {&FRAME-NAME}.

    DISP fi-tot-estoq
         fi-tot-perc
         fi-tot-fator
         fi-tot-req
         fi-fd-bancada
         WITH FRAME {&FRAME-NAME}.

    ASSIGN tt-requisicao.fator:READ-ONLY IN BROWSE br-requisicao = YES.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-processa w-cadsim 
PROCEDURE pi-processa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-qtd-bancadas AS INT.
    DEF INPUT PARAMETER p-qtd-fardos AS INT.

    ASSIGN i-qtd-fardos   = p-qtd-fardos
           i-qtd-bancadas = p-qtd-bancadas
             fi-tot-estoq = 0
             fi-tot-req   = 0
             fi-tot-fator = 0
             fi-tot-perc  = 0.

    FOR EACH mp-fardo WHERE
             mp-fardo.situacao = 3 NO-LOCK.

        ASSIGN c-classif = mp-fardo.letra + STRING(mp-fardo.cd-coloracao) + 
                                            STRING(mp-fardo.cd-tipo).

        FIND tt-requisicao WHERE
             tt-requisicao.padrao      = mp-fardo.padrao AND
             tt-requisicao.codificacao = c-classif NO-LOCK NO-ERROR.

        IF NOT AVAIL tt-requisicao THEN DO.
           CREATE tt-requisicao.
           ASSIGN tt-requisicao.padrao      = mp-fardo.padrao 
                  tt-requisicao.codificacao = c-classif.
        END.
        ASSIGN tt-requisicao.qtd-estoq = tt-requisicao.qtd-estoq + 1
               fi-tot-estoq            = fi-tot-estoq + 1.
    END.

    FOR EACH tt-requisicao EXCLUSIVE-LOCK.
        ASSIGN tt-requisicao.perc-estoq = (tt-requisicao.qtd-estoq / fi-tot-estoq) * 100
               tt-requisicao.fator      = ROUND(i-qtd-fardos * tt-requisicao.perc-estoq / 100,0).

        ASSIGN tt-requisicao.fator      = IF tt-requisicao.fator < 1 THEN 1 ELSE tt-requisicao.fator
               tt-requisicao.qtd-fd-req = tt-requisicao.fator *  i-qtd-bancadas.

        ASSIGN fi-tot-req   = fi-tot-req   + tt-requisicao.qtd-fd-req
               fi-tot-perc  = fi-tot-perc  + tt-requisicao.perc-estoq
               fi-tot-fator = fi-tot-fator + tt-requisicao.fator.
    END.

    {&OPEN-QUERY-br-requisicao}
    {&OPEN-QUERY-br-distribuicao}

    ENABLE {&list-6} WITH FRAME {&FRAME-NAME}.

    DISP fi-tot-estoq
         fi-tot-perc
         fi-tot-fator
         fi-tot-req
         WITH FRAME {&FRAME-NAME}.

    ASSIGN tt-requisicao.fator:READ-ONLY IN BROWSE br-requisicao = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-cadsim  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-requisicao"}
  {src/adm/template/snd-list.i "tt-distribuicao"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-cadsim 
PROCEDURE state-changed :
/*:T -----------------------------------------------------------
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

