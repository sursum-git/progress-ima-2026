&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-livre 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i XX9999 9.99.99.999}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
/* Temp Table Definitions ---                                           */
DEF TEMP-TABLE tt-itens 
    FIELD it-codigo     LIKE item.it-codigo
    FIELD desc-item     LIKE item.desc-item
    FIELD qtd-prod      AS DEC 
    FIELD qtd-etq       AS INT
    FIELD ob-origem     LIKE ob-etiqueta.ob-origem
    FIELD tipo          AS INT
    INDEX indice1 IS PRIMARY it-codigo.

DEF TEMP-TABLE tt-ob-etiqueta LIKE ob-etiqueta.

DEF BUFFER b-ob-etiqueta FOR ob-etiqueta.
DEF BUFFER b-tt-itens FOR tt-itens.

/* --- Local Variable Definitions --- */
DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.
DEF VAR h-acomp           AS HANDLE NO-UNDO.
DEF VAR h-query           AS HANDLE.

DEF VAR c-dia           AS CHAR.
DEF VAR c-it-codigo     AS CHAR.
DEF VAR i-nr-sequencia  AS INT.
DEF VAR i-ct            AS INT.
DEF VAR i-ct-lin        AS INT.
DEF VAR i-ct-col        AS INT.
DEF VAR i-num-bar       AS INT.

DEF VAR c-cod-estabel   AS CHAR.
DEF VAR da-data-ini     AS DATE.     
DEF VAR da-data-fin     AS DATE.
DEF VAR c-it-codigo-ini AS CHAR. 
DEF VAR c-it-codigo-fin AS CHAR.
DEF VAR l-ok            AS LOG.

DEF VAR c-prod AS CHAR.

{esinc/sz-pcl.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad
&Scoped-define BROWSE-NAME br-itens

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-itens

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br-itens                                      */
&Scoped-define FIELDS-IN-QUERY-br-itens tt-itens.it-codigo tt-itens.desc-item tt-itens.qtd-prod tt-itens.qtd-etq   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-itens tt-itens.qtd-etq   
&Scoped-define ENABLED-TABLES-IN-QUERY-br-itens tt-itens
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br-itens tt-itens
&Scoped-define SELF-NAME br-itens
&Scoped-define QUERY-STRING-br-itens FOR EACH tt-itens WHERE                                  (tt-itens.tipo = rs-tipo OR rs-tipo = 3) AND                                  tt-itens.qtd-prod > 0 NO-LOCK
&Scoped-define OPEN-QUERY-br-itens OPEN QUERY {&SELF-NAME} FOR EACH tt-itens WHERE                                  (tt-itens.tipo = rs-tipo OR rs-tipo = 3) AND                                  tt-itens.qtd-prod > 0 NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-itens tt-itens
&Scoped-define FIRST-TABLE-IN-QUERY-br-itens tt-itens


/* Definitions for FRAME f-cad                                          */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-button rt-buttom rs-tipo br-itens ~
bt-param bt-vapara 
&Scoped-Define DISPLAYED-OBJECTS rs-tipo 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-livre AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU mi-programa 
       MENU-ITEM mi-consultas   LABEL "Co&nsultas"     ACCELERATOR "CTRL-L"
       MENU-ITEM mi-imprimir    LABEL "&Relat¢rios"    ACCELERATOR "CTRL-P"
       RULE
       MENU-ITEM mi-sair        LABEL "&Sair"          ACCELERATOR "CTRL-X".

DEFINE SUB-MENU m_Ajuda 
       MENU-ITEM mi-conteudo    LABEL "&Conteudo"     
       MENU-ITEM mi-sobre       LABEL "&Sobre..."     .

DEFINE MENU m-livre MENUBAR
       SUB-MENU  mi-programa    LABEL "&Nome-do-Programa"
       SUB-MENU  m_Ajuda        LABEL "&Ajuda"        .


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_p-exihel AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-cancela AUTO-GO 
     IMAGE-UP FILE "image/im-cance.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Cancelar Pedido"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-modifica AUTO-END-KEY 
     IMAGE-UP FILE "image/im-mod.bmp":U
     LABEL "&Sair" 
     SIZE 5 BY 1.25 TOOLTIP "Modifca Quantidade de Etiquetas"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-ok AUTO-GO 
     IMAGE-UP FILE "image/im-sav.bmp":U
     LABEL "OK" 
     SIZE 5 BY 1.25 TOOLTIP "Grava / Imprime Etiquetas de Retalho".

DEFINE BUTTON bt-param AUTO-GO 
     IMAGE-UP FILE "image/im-fil.bmp":U
     LABEL "&Sair" 
     SIZE 5 BY 1.25 TOOLTIP "ParÉmetros"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-vapara AUTO-GO 
     IMAGE-UP FILE "image/im-vapra.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Posicionar no Item"
     BGCOLOR 8 FONT 10.

DEFINE VARIABLE rs-tipo AS INTEGER INITIAL 3 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Somente ÷ndigos", 1,
"N∆o ÷ndigos", 2,
"Ambos", 3
     SIZE 71 BY 1
     FGCOLOR 9 FONT 6 NO-UNDO.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 7 BY 12.17
     BGCOLOR 8 FGCOLOR 0 .

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 89.72 BY 1.46
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-itens FOR 
      tt-itens SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-itens
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-itens w-livre _FREEFORM
  QUERY br-itens NO-LOCK DISPLAY
      tt-itens.it-codigo FORMAT "x(6)":U     COLUMN-LABEL "Item"           WIDTH  6
      tt-itens.desc-item FORMAT "x(35)":U    COLUMN-LABEL "Descriá∆o"      WIDTH 35
      tt-itens.qtd-prod  FORMAT ">>>,>>9.99" COLUMN-LABEL "Qtd. Produzida" WIDTH 10
      tt-itens.qtd-etq   FORMAT ">>9"        COLUMN-LABEL "Qtd. Etiquetas" WIDTH 10  COLUMN-FONT 6
ENABLE
      tt-itens.qtd-etq
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 81.57 BY 12.17
         FONT 1
         TITLE "Itens Produzidos" ROW-HEIGHT-CHARS .54.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     rs-tipo AT ROW 1.25 COL 2.14 NO-LABEL WIDGET-ID 2
     br-itens AT ROW 2.58 COL 1.43 WIDGET-ID 200
     bt-param AT ROW 2.88 COL 84.57 WIDGET-ID 30
     bt-vapara AT ROW 4.21 COL 84.57 WIDGET-ID 36
     bt-modifica AT ROW 10.17 COL 84.72 WIDGET-ID 28
     bt-cancela AT ROW 11.46 COL 84.72 WIDGET-ID 26
     bt-ok AT ROW 13.21 COL 84.72 WIDGET-ID 34
     rt-button AT ROW 1 COL 1
     rt-buttom AT ROW 2.58 COL 83.57 WIDGET-ID 32
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 14.04
         FONT 1 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-livre
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-livre ASSIGN
         HIDDEN             = YES
         TITLE              = "Gerenciamento da Produá∆o de Retalhos - ESSP0163"
         HEIGHT             = 13.88
         WIDTH              = 90.43
         MAX-HEIGHT         = 17
         MAX-WIDTH          = 91.72
         VIRTUAL-HEIGHT     = 17
         VIRTUAL-WIDTH      = 91.72
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU m-livre:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-livre 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-livre.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-livre
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-cad
   FRAME-NAME L-To-R                                                    */
/* BROWSE-TAB br-itens rs-tipo f-cad */
/* SETTINGS FOR BUTTON bt-cancela IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-modifica IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-ok IN FRAME f-cad
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-itens
/* Query rebuild information for BROWSE br-itens
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-itens WHERE
                                 (tt-itens.tipo = rs-tipo OR rs-tipo = 3) AND
                                 tt-itens.qtd-prod > 0 NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Query            is NOT OPENED
*/  /* BROWSE br-itens */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON END-ERROR OF w-livre /* Gerenciamento da Produá∆o de Retalhos - ESSP0163 */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-CLOSE OF w-livre /* Gerenciamento da Produá∆o de Retalhos - ESSP0163 */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-itens
&Scoped-define SELF-NAME br-itens
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-itens w-livre
ON ROW-DISPLAY OF br-itens IN FRAME f-cad /* Itens Produzidos */
DO:
  IF tt-itens.qtd-etq > 0 THEN
     ASSIGN  tt-itens.qtd-etq:FGCOLOR IN BROWSE {&browse-name} = 12.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-itens w-livre
ON ROW-LEAVE OF br-itens IN FRAME f-cad /* Itens Produzidos */
DO:
  ASSIGN INPUT BROWSE br-itens tt-itens.qtd-etq.

  RELEASE tt-itens.

  APPLY "ENTRY" TO bt-modifica.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-itens w-livre
ON VALUE-CHANGED OF br-itens IN FRAME f-cad /* Itens Produzidos */
DO:
   ASSIGN bt-modifica:SENSITIVE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancela
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancela w-livre
ON CHOOSE OF bt-cancela IN FRAME f-cad
DO:
   ASSIGN tt-itens.qtd-etq = 0.
   br-itens:REFRESH().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-modifica
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-modifica w-livre
ON CHOOSE OF bt-modifica IN FRAME f-cad /* Sair */
DO:
    ASSIGN tt-itens.qtd-etq:READ-ONLY IN BROWSE br-itens = NO.
    APPLY 'entry' TO br-itens.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-modifica w-livre
ON ENTRY OF bt-modifica IN FRAME f-cad /* Sair */
DO:
  ASSIGN tt-itens.qtd-etq:READ-ONLY IN BROWSE br-itens = YES.
  ASSIGN bt-ok:SENSITIVE = CAN-FIND(FIRST b-tt-itens WHERE
                                          b-tt-itens.qtd-etq > 0).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-livre
ON CHOOSE OF bt-ok IN FRAME f-cad /* OK */
DO:
  EMPTY TEMP-TABLE tt-ob-etiqueta.
  FOR EACH tt-itens  WHERE 
           tt-itens.qtd-etq > 0 NO-LOCK.

      FIND LAST ordem-benefic WHERE
                ordem-benefic.nr-ob = INT(ENTRY(NUM-ENTRIES(tt-itens.ob-origem,";"),tt-itens.ob-origem,";"))
                NO-LOCK NO-ERROR.

      ASSIGN i-nr-sequencia = 1.
      FIND LAST b-ob-etiqueta WHERE
                b-ob-etiqueta.nr-ob         = ordem-benefic.nr-ob    AND
                b-ob-etiqueta.dt-ob         = ordem-benefic.dt-ob    AND
                b-ob-etiqueta.nr-carro      = ordem-benefic.nr-carro NO-LOCK NO-ERROR.

      ASSIGN i-nr-sequencia = IF AVAIL b-ob-etiqueta 
                              THEN b-ob-etiqueta.nr-sequencia + 1
                              ELSE 1.
 
      DO i-ct = 1 TO tt-itens.qtd-etq.
         CREATE ob-etiqueta.
         ASSIGN ob-etiqueta.nr-ob           = ordem-benefic.nr-ob
                ob-etiqueta.dt-ob           = ordem-benefic.dt-ob
                ob-etiqueta.nr-carro        = ordem-benefic.nr-carro
                ob-etiqueta.acondic         = "Saco"
                ob-etiqueta.nr-sequencia    = i-nr-sequencia
                ob-etiqueta.resp-revisao    = c-seg-usuario
                ob-etiqueta.dt-emissao      = TODAY 
                ob-etiqueta.hr-emissao      = STRING(TIME, "HH:MM")
                ob-etiqueta.tipo-ordem      = 1  /* 1=Produá∆o  2=Retrabalho   3=Conserto */
                ob-etiqueta.nr-lote         = "SC" 
                ob-etiqueta.un              = "Kg". 

         IF LOOKUP(tt-itens.it-codigo,c-prod,";") > 0 THEN
            ASSIGN ob-etiqueta.it-codigo       = tt-itens.it-codigo.
         ELSE 
            ASSIGN ob-etiqueta.it-codigo       = SUBSTR(tt-itens.it-codigo,1,5) + '9'.

         ASSIGN ob-etiqueta.situacao        = 2 /* 1=Impressa 2=Produá∆o 3=Estoque 4=Reservada 5=Faturada */
                ob-etiqueta.num-etiqueta    = IF ordem-benefic.cod-estabel = '1'
                                              THEN NEXT-VALUE(seq-etq-estab1)
                                              ELSE NEXT-VALUE(seq-etq-estoq)
                ob-etiqueta.cod-estabel     = c-cod-estabel
                ob-etiqueta.corte-comerc    = "J"
                ob-etiqueta.cod-qualid      = "R"
                ob-etiqueta.ob-origem       = tt-itens.ob-origem.

         ASSIGN i-nr-sequencia = i-nr-sequencia + 1.

         CREATE tt-ob-etiqueta.
         BUFFER-COPY ob-etiqueta TO tt-ob-etiqueta.
      END.
  END.
  
  IF c-cod-estabel <> '1' THEN
     RUN pi-etiqueta. /* Imprime as Etiqueta */  
  ELSE
     RUN pi-etiq-par.
  
  FOR EACH tt-itens.
      ASSIGN tt-itens.qtd-etq = 0.
  END.
  br-itens:REFRESH().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-param
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-param w-livre
ON CHOOSE OF bt-param IN FRAME f-cad /* Sair */
DO:
   ASSIGN w-livre:SENSITIVE = NO.
   RUN esp/essp0163a.w (INPUT-OUTPUT c-cod-estabel,
                        INPUT-OUTPUT da-data-ini,   
                        INPUT-OUTPUT da-data-fin,   
                        INPUT-OUTPUT c-it-codigo-ini,   
                        INPUT-OUTPUT c-it-codigo-fin,   
                        INPUT-OUTPUT l-ok). 
   IF l-ok THEN DO.
      RUN pi-popula-browse.
      APPLY 'value-changed' TO br-itens.
   END.

    ASSIGN w-livre:SENSITIVE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-vapara
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-vapara w-livre
ON CHOOSE OF bt-vapara IN FRAME f-cad
DO:
   ASSIGN c-it-codigo = "".
   RUN esdlg/d01essp0163.w (OUTPUT c-it-codigo).

   IF c-it-codigo <> "" THEN DO:
      FIND tt-itens WHERE
           tt-itens.it-codigo = c-it-codigo NO-LOCK NO-ERROR. 

      IF NOT AVAIL tt-itens THEN DO.
         MESSAGE "Item n∆o est† contido na seleá∆o!"
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         RETURN NO-APPLY.
      END.

      h-query:REPOSITION-TO-ROWID(ROWID(tt-itens)) NO-ERROR.
      APPLY 'VALUE-CHANGED' TO br-itens.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-consultas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-consultas w-livre
ON CHOOSE OF MENU-ITEM mi-consultas /* Consultas */
DO:
  RUN pi-consulta IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-conteudo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-conteudo w-livre
ON CHOOSE OF MENU-ITEM mi-conteudo /* Conteudo */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  RUN pi-ajuda IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-imprimir w-livre
ON CHOOSE OF MENU-ITEM mi-imprimir /* Relat¢rios */
DO:
  RUN pi-imprimir IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-programa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-programa w-livre
ON MENU-DROP OF MENU mi-programa /* Nome-do-Programa */
DO:
  run pi-disable-menu.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sair w-livre
ON CHOOSE OF MENU-ITEM mi-sair /* Sair */
DO:
  RUN pi-sair IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sobre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sobre w-livre
ON CHOOSE OF MENU-ITEM mi-sobre /* Sobre... */
DO:
  {include/sobre.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-tipo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-tipo w-livre
ON VALUE-CHANGED OF rs-tipo IN FRAME f-cad
DO:
   ASSIGN INPUT FRAME {&FRAME-NAME} rs-tipo.
  {&OPEN-QUERY-br-itens}

  APPLY 'value-changed' TO br-itens IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-livre 


/* ***************************  Main Block  *************************** */

ASSIGN h-query = br-itens:QUERY.

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-livre  _ADM-CREATE-OBJECTS
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
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-exihel ).
       RUN set-position IN h_p-exihel ( 1.17 , 74.14 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             rs-tipo:HANDLE IN FRAME f-cad , 'BEFORE':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-livre  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-livre  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
  THEN DELETE WIDGET w-livre.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-livre  _DEFAULT-ENABLE
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
  DISPLAY rs-tipo 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE rt-button rt-buttom rs-tipo br-itens bt-param bt-vapara 
      WITH FRAME f-cad IN WINDOW w-livre.
  {&OPEN-BROWSERS-IN-QUERY-f-cad}
  VIEW w-livre.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-livre 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-livre 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-livre 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  run pi-before-initialize.

  {include/win-size.i}

  {utp/ut9000.i "ESSP0163" "2.04.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  run pi-after-initialize.

  ASSIGN tt-itens.qtd-etq:READ-ONLY IN BROWSE br-itens = YES.

  RUN esapi/busca-estab-usuar.p (INPUT c-seg-usuario,
                                 OUTPUT c-cod-estabel).
  IF c-cod-estabel = '' OR c-cod-estabel = '0' THEN 
     ASSIGN c-cod-estabel = '1'.

  ASSIGN da-data-ini = TODAY - DAY(TODAY) + 1
         da-data-fin = TODAY    
         c-it-codigo-ini = ""
         c-it-codigo-fin = "ZZZZZZZZZZZZZZZZ".

  APPLY 'choose' TO bt-param.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-etiq-par w-livre 
PROCEDURE pi-etiq-par :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEF VAR i-lin AS INT EXTENT 20 INIT [265,415,565,717,865,1010,1155,1307,1460,1602,1752,1904,2052,2197,2342,2496,2637,2787,2937,3085].
    DEF VAR i-col AS INT EXTENT 4 INIT [205,775,1335,1895].

    OUTPUT TO PRINTER PAGED.
    PUT UNFORMATTED 
        "~033&l26A"
        "~033&l1E".
    
    ASSIGN i-ct-lin = 1
           i-ct-col = 0.
    FOR EACH tt-ob-etiqueta NO-LOCK
        BREAK BY tt-ob-etiqueta.it-codigo
              BY tt-ob-etiqueta.num-etiqueta.

        ASSIGN i-ct-col = i-ct-col + 1.
        IF i-ct-col > 4 THEN DO.
           ASSIGN i-ct-col = 1
                  i-ct-lin = i-ct-lin + 1.

           IF i-ct-lin > 20 THEN DO.
              ASSIGN i-ct-lin = 1.
              PAGE.
           END.
        END.

        RUN pi-imp-etiqueta (INPUT i-lin[i-ct-lin], INPUT i-col[i-ct-col]). 

        FIND ob-etiqueta WHERE
             ob-etiqueta.cod-estabel  = tt-ob-etiqueta.cod-estabel AND
             ob-etiqueta.num-etiqueta = tt-ob-etiqueta.num-etiqueta NO-ERROR.
        IF AVAIL ob-etiqueta THEN
           ASSIGN ob-etiqueta.situacao = 2.  /* 1=Impressa  2=Produá∆o 3=Estoque
                                                4=Reservada 5=Faturada */

        IF LAST-OF(tt-ob-etiqueta.it-codigo) THEN /* Salta Linha e Branco */
           ASSIGN i-ct-col = 0
                  i-ct-lin = i-ct-lin + 2.
    END.
    OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-etiqueta w-livre 
PROCEDURE pi-etiqueta :
/*--------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/                    
    DEF VAR i-lin AS INT EXTENT 20 INIT [235,385,535,687,835,985,1135,1287,1440,1582,1732,1884,2032,2177,2322,2476,2617,2767,2917,3065].
    DEF VAR i-col AS INT EXTENT 4 INIT [155,725,1285,1845].

    OUTPUT TO PRINTER PAGED.
    PUT UNFORMATTED 
        "~033&l26A"
        "~033&l1E".
    
    ASSIGN i-ct-lin = 1
           i-ct-col = 0.
    FOR EACH tt-ob-etiqueta NO-LOCK
        BREAK BY tt-ob-etiqueta.it-codigo
              BY tt-ob-etiqueta.num-etiqueta.

        ASSIGN i-ct-col = i-ct-col + 1.
        IF i-ct-col > 4 THEN DO.
           ASSIGN i-ct-col = 1
                  i-ct-lin = i-ct-lin + 1.

           IF i-ct-lin > 20 THEN DO.
              ASSIGN i-ct-lin = 1.
              PAGE.
           END.
        END.

        RUN pi-imp-etiqueta (INPUT i-lin[i-ct-lin], INPUT i-col[i-ct-col]). 

        FIND ob-etiqueta WHERE
             ob-etiqueta.cod-estabel  = tt-ob-etiqueta.cod-estabel AND
             ob-etiqueta.num-etiqueta = tt-ob-etiqueta.num-etiqueta NO-ERROR.
        IF AVAIL ob-etiqueta THEN
           ASSIGN ob-etiqueta.situacao = 2.  /* 1=Impressa  2=Produá∆o 3=Estoque
                                                4=Reservada 5=Faturada */

        IF LAST-OF(tt-ob-etiqueta.it-codigo) THEN /* Salta Linha e Branco */
           ASSIGN i-ct-col = 0
                  i-ct-lin = i-ct-lin + 2.
    END.
    OUTPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imp-etiqueta w-livre 
PROCEDURE pi-imp-etiqueta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER i-lin AS INT.
  DEF INPUT PARAMETER i-col AS INT.

  ASSIGN i-num-bar = INT(STRING(tt-ob-etiqueta.num-etiqueta) + fn-calc-digito(INPUT STRING(tt-ob-etiqueta.num-etiqueta,"999999999"))). 

  FIND ITEM WHERE
       ITEM.it-codigo = tt-ob-etiqueta.it-codigo NO-LOCK NO-ERROR.

  PUT UNFORMATTED  /* Dados da Etiqueta */
      fn-texto(INPUT i-col      , INPUT i-lin      , INPUT tt-ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
      fn-texto(INPUT i-col +  10, INPUT i-lin - 100, INPUT SUBSTR(ITEM.desc-item,1,20),                     INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
      fn-texto(INPUT i-col + 320, INPUT i-lin - 100, INPUT STRING(tt-ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").

  PUT UNFORMATTED  /* Codigo de Barra */
      fn-code25 (INPUT i-col +  10, input i-lin - 90,
                 INPUT STRING(i-num-bar,"9999999999"),
                 INPUT "H",              
                 INPUT 1.8,
                 INPUT 4.4).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-popula-browse w-livre 
PROCEDURE pi-popula-browse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
   {utp/ut-liter.i Selecionando_Retalhos_Produzidos *}
   RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

   ASSIGN c-prod = "599901;599902;599903;599904;599905;599907;599908;599909".

   EMPTY TEMP-TABLE tt-itens.

   FOR EACH ob-etiqueta USE-INDEX indice8 WHERE
            ob-etiqueta.cod-estabel = c-cod-estabel AND
            ob-etiqueta.dt-emissao >= da-data-ini AND
            ob-etiqueta.dt-emissao <= da-data-fin AND
            ob-etiqueta.it-codigo  >= c-it-codigo-ini AND
            ob-etiqueta.it-codigo  <= c-it-codigo-fin NO-LOCK.

       RUN pi-acompanhar in h-acomp (INPUT "Data: " +  STRING(ob-etiqueta.dt-emissao) + "  Item: " + ob-etiqueta.it-codigo).

       FIND ordem-benefic OF ob-etiqueta NO-LOCK NO-ERROR.

       IF NOT AVAIL ordem-benefic THEN NEXT.
       IF ordem-benefic.cor-etiqueta = 100 THEN NEXT.

       IF ob-etiqueta.nr-lote = 'sc' THEN NEXT.

       FIND item WHERE
            item.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.

       IF NOT AVAIL ITEM THEN NEXT.

       FIND item-ext WHERE
            item-ext.it-codigo = item.it-codigo NO-LOCK NO-ERROR.

       FIND tt-itens WHERE
            tt-itens.it-codigo = item.it-codigo NO-LOCK NO-ERROR.

       IF NOT AVAIL tt-itens THEN DO.
          CREATE tt-itens.
          ASSIGN tt-itens.it-codigo = ob-etiqueta.it-codigo
                 tt-itens.tipo = IF item-ext.indigo THEN 1 ELSE 2
                 tt-itens.desc-item = item.desc-item.
       END.
       ASSIGN tt-itens.qtd-prod = tt-itens.qtd-prod + ob-etiqueta.quantidade.

       IF LOOKUP(STRING(ob-etiqueta.nr-ob),tt-itens.ob-origem,";") = 0 THEN
          ASSIGN tt-itens.ob-origem = IF tt-itens.ob-origem = ""
                                      THEN STRING(ob-etiqueta.nr-ob) 
                                      ELSE tt-itens.ob-origem + ";" + STRING(ob-etiqueta.nr-ob).
   END.

   FOR EACH ITEM WHERE
            LOOKUP(ITEM.it-codigo,c-prod,";") > 0 
            NO-LOCK.
    
        FIND item-ext WHERE
             item-ext.it-codigo = ITEM.it-codigo NO-LOCK NO-ERROR.

        FIND LAST ob-etiqueta WHERE
                  ob-etiqueta.dt-emissao >= da-data-ini AND
                  ob-etiqueta.dt-emissao <= da-data-fin NO-LOCK NO-ERROR.

        FIND tt-itens WHERE
             tt-itens.it-codigo = ITEM.it-codigo NO-LOCK NO-ERROR.
        IF NOT AVAIL tt-itens THEN DO.
            CREATE tt-itens.
            ASSIGN tt-itens.it-codigo = ITEM.it-codigo
                   tt-itens.tipo = IF item-ext.indigo THEN 1 ELSE 2
                   tt-itens.desc-item = item.desc-item
                   tt-itens.qtd-prod = 1.

            IF LOOKUP(STRING(ob-etiqueta.nr-ob),tt-itens.ob-origem,";") = 0 THEN
               ASSIGN tt-itens.ob-origem = IF tt-itens.ob-origem = ""
                                           THEN STRING(ob-etiqueta.nr-ob) 
                                           ELSE tt-itens.ob-origem + ";" + STRING(ob-etiqueta.nr-ob).
        END.
   END.
   

   RUN pi-finalizar in h-acomp.

   {&OPEN-QUERY-br-itens}

   APPLY 'value-changed' TO br-itens IN FRAME {&FRAME-NAME}.
   APPLY 'entry' TO br-itens IN FRAME {&FRAME-NAME}.
   RETURN NO-APPLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-livre  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-itens"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-livre 
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

