&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          ems5             PROGRESS
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

DEFINE VARIABLE i_num_id_movto_tit_ap AS INTEGER     NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES movto_tit_ap aprop_ctbl_ap

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 movto_tit_ap.cod_usuario ~
movto_tit_ap.dat_transacao movto_tit_ap.ind_trans_ap_abrev ~
movto_tit_ap.val_movto_ap movto_tit_ap.num_id_movto_tit_ap ~
movto_tit_ap.log_movto_estordo 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2 
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH movto_tit_ap ~
      WHERE movto_tit_ap.num_id_tit_ap = tit_ap.num_id_tit_ap NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY BROWSE-2 FOR EACH movto_tit_ap ~
      WHERE movto_tit_ap.num_id_tit_ap = tit_ap.num_id_tit_ap NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 movto_tit_ap
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 movto_tit_ap


/* Definitions for BROWSE BROWSE-3                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-3 aprop_ctbl_ap.dat_transacao ~
aprop_ctbl_ap.ind_natur_lancto_ctbl aprop_ctbl_ap.val_aprop_ctbl ~
aprop_ctbl_ap.cod_cta_ctbl 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-3 
&Scoped-define QUERY-STRING-BROWSE-3 FOR EACH aprop_ctbl_ap ~
      WHERE aprop_ctbl_ap.num_id_movto_tit_ap = i_num_id_movto_tit_ap NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-3 OPEN QUERY BROWSE-3 FOR EACH aprop_ctbl_ap ~
      WHERE aprop_ctbl_ap.num_id_movto_tit_ap = i_num_id_movto_tit_ap NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-3 aprop_ctbl_ap
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-3 aprop_ctbl_ap


/* Definitions for FRAME f-cad                                          */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 rt-button RECT-2 RECT-3 fi_cod_estab ~
fi_id_tit_ap fi_cod_tit_ap fi_parcela fi_cod_espec_docto fi_nome_abrev ~
fi_cdn_fornec fi_serie btPesquisar BROWSE-2 BROWSE-3 fi_conta fi_desc_conta ~
btAtualizar 
&Scoped-Define DISPLAYED-OBJECTS fi_cod_estab fi_id_tit_ap fi_titulo ~
fi_cod_tit_ap fi_parcela fi_cod_espec_docto fi_nome_abrev fi_cdn_fornec ~
fi_serie fi_conta fi_desc_conta 

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
DEFINE BUTTON btAtualizar 
     LABEL "Atualizar" 
     SIZE 15 BY 1.13.

DEFINE BUTTON btPesquisar 
     LABEL "Pesquisar" 
     SIZE 15 BY 1.13.

DEFINE VARIABLE fi_cdn_fornec AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "C¢digo" 
     VIEW-AS FILL-IN 
     SIZE 10.29 BY 1 NO-UNDO.

DEFINE VARIABLE fi_cod_espec_docto AS CHARACTER FORMAT "X(3)":U 
     LABEL "Especie" 
     VIEW-AS FILL-IN 
     SIZE 5.72 BY 1 NO-UNDO.

DEFINE VARIABLE fi_cod_estab AS CHARACTER FORMAT "X(3)":U 
     LABEL "Estab." 
     VIEW-AS FILL-IN 
     SIZE 5.72 BY 1 NO-UNDO.

DEFINE VARIABLE fi_cod_tit_ap AS CHARACTER FORMAT "X(12)":U 
     LABEL "Documento" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fi_conta AS CHARACTER FORMAT "X(20)":U 
     LABEL "Conta" 
     VIEW-AS FILL-IN 
     SIZE 14.29 BY .79 NO-UNDO.

DEFINE VARIABLE fi_desc_conta AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 49.43 BY .79 NO-UNDO.

DEFINE VARIABLE fi_id_tit_ap AS INTEGER FORMAT ">>>>>>>>>>>":U INITIAL 0 
     LABEL "ID Titulo a Pagar" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fi_nome_abrev AS CHARACTER FORMAT "X(12)":U 
     LABEL "Fornecedor" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fi_parcela AS CHARACTER FORMAT "X(2)":U 
     LABEL "Parcela" 
     VIEW-AS FILL-IN 
     SIZE 7.57 BY 1 NO-UNDO.

DEFINE VARIABLE fi_serie AS CHARACTER FORMAT "X(3)":U 
     LABEL "Serie" 
     VIEW-AS FILL-IN 
     SIZE 7.57 BY 1 NO-UNDO.

DEFINE VARIABLE fi_titulo AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 55.29 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 83 BY 1.75.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 82.43 BY 3.17.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 83 BY 1.75.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 89.72 BY 1.46
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      movto_tit_ap SCROLLING.

DEFINE QUERY BROWSE-3 FOR 
      aprop_ctbl_ap SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 w-livre _STRUCTURED
  QUERY BROWSE-2 NO-LOCK DISPLAY
      movto_tit_ap.cod_usuario FORMAT "x(12)":U
      movto_tit_ap.dat_transacao FORMAT "99/99/9999":U
      movto_tit_ap.ind_trans_ap_abrev FORMAT "X(04)":U
      movto_tit_ap.val_movto_ap FORMAT "->>>,>>>,>>9.99":U
      movto_tit_ap.num_id_movto_tit_ap FORMAT "9999999999":U
      movto_tit_ap.log_movto_estordo FORMAT "Sim/N∆o":U WIDTH 15.29
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 82 BY 4.5
         FONT 1
         TITLE "Movimento Titulo".

DEFINE BROWSE BROWSE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-3 w-livre _STRUCTURED
  QUERY BROWSE-3 NO-LOCK DISPLAY
      aprop_ctbl_ap.dat_transacao FORMAT "99/99/9999":U
      aprop_ctbl_ap.ind_natur_lancto_ctbl FORMAT "X(02)":U
      aprop_ctbl_ap.val_aprop_ctbl FORMAT "->>>,>>>,>>9.99":U
      aprop_ctbl_ap.cod_cta_ctbl FORMAT "x(20)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 82 BY 4.5
         FONT 1
         TITLE "Apropriaá∆o Movimento".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     fi_cod_estab AT ROW 3 COL 15.14 COLON-ALIGNED WIDGET-ID 30
     fi_id_tit_ap AT ROW 5.04 COL 15.57 COLON-ALIGNED WIDGET-ID 2
     fi_titulo AT ROW 5.04 COL 29.72 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     fi_cod_tit_ap AT ROW 6.79 COL 16 COLON-ALIGNED WIDGET-ID 14
     fi_parcela AT ROW 6.79 COL 39.43 COLON-ALIGNED WIDGET-ID 16
     fi_cod_espec_docto AT ROW 6.79 COL 69.14 COLON-ALIGNED WIDGET-ID 22
     fi_nome_abrev AT ROW 8 COL 16 COLON-ALIGNED WIDGET-ID 34
     fi_cdn_fornec AT ROW 8 COL 39.72 COLON-ALIGNED WIDGET-ID 18
     fi_serie AT ROW 8 COL 69.14 COLON-ALIGNED WIDGET-ID 20
     btPesquisar AT ROW 9.63 COL 5.29 WIDGET-ID 6
     BROWSE-2 AT ROW 10.83 COL 5 WIDGET-ID 200
     BROWSE-3 AT ROW 15.79 COL 5 WIDGET-ID 300
     fi_conta AT ROW 20.58 COL 8.72 COLON-ALIGNED WIDGET-ID 8
     fi_desc_conta AT ROW 20.58 COL 23.57 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     btAtualizar AT ROW 21.54 COL 10.29 WIDGET-ID 12
     RECT-1 AT ROW 4.46 COL 4.57 WIDGET-ID 24
     rt-button AT ROW 1 COL 1
     RECT-2 AT ROW 6.29 COL 4.86 WIDGET-ID 26
     RECT-3 AT ROW 2.63 COL 4.57 WIDGET-ID 28
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.72 BY 24.25
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
         TITLE              = "Template Livre <Insira complemento>"
         HEIGHT             = 24.25
         WIDTH              = 89.72
         MAX-HEIGHT         = 24.29
         MAX-WIDTH          = 99.86
         VIRTUAL-HEIGHT     = 24.29
         VIRTUAL-WIDTH      = 99.86
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
/* BROWSE-TAB BROWSE-2 btPesquisar f-cad */
/* BROWSE-TAB BROWSE-3 BROWSE-2 f-cad */
/* SETTINGS FOR FILL-IN fi_titulo IN FRAME f-cad
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _TblList          = "ems5.movto_tit_ap"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "ems5.movto_tit_ap.num_id_tit_ap = tit_ap.num_id_tit_ap"
     _FldNameList[1]   = ems5.movto_tit_ap.cod_usuario
     _FldNameList[2]   = ems5.movto_tit_ap.dat_transacao
     _FldNameList[3]   = ems5.movto_tit_ap.ind_trans_ap_abrev
     _FldNameList[4]   = ems5.movto_tit_ap.val_movto_ap
     _FldNameList[5]   = ems5.movto_tit_ap.num_id_movto_tit_ap
     _FldNameList[6]   > ems5.movto_tit_ap.log_movto_estordo
"movto_tit_ap.log_movto_estordo" ? ? "logical" ? ? ? ? ? ? no ? no no "15.29" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-3
/* Query rebuild information for BROWSE BROWSE-3
     _TblList          = "ems5.aprop_ctbl_ap"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "ems5.aprop_ctbl_ap.num_id_movto_tit_ap = i_num_id_movto_tit_ap"
     _FldNameList[1]   = ems5.aprop_ctbl_ap.dat_transacao
     _FldNameList[2]   = ems5.aprop_ctbl_ap.ind_natur_lancto_ctbl
     _FldNameList[3]   = ems5.aprop_ctbl_ap.val_aprop_ctbl
     _FldNameList[4]   = ems5.aprop_ctbl_ap.cod_cta_ctbl
     _Query            is NOT OPENED
*/  /* BROWSE BROWSE-3 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON END-ERROR OF w-livre /* Template Livre <Insira complemento> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-CLOSE OF w-livre /* Template Livre <Insira complemento> */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&Scoped-define SELF-NAME BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 w-livre
ON VALUE-CHANGED OF BROWSE-2 IN FRAME f-cad /* Movimento Titulo */
DO:
  IF AVAIL movto_tit_ap THEN DO:
     ASSIGN  i_num_id_movto_tit_ap = movto_tit_ap.num_id_movto_tit_ap.
  END.
  ELSE
     ASSIGN i_num_id_movto_tit_ap = 0.

  {&OPEN-QUERY-BROWSE-3}
  APPLY 'value-changed' TO BROWSE browse-3. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-3
&Scoped-define SELF-NAME BROWSE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-3 w-livre
ON VALUE-CHANGED OF BROWSE-3 IN FRAME f-cad /* Apropriaá∆o Movimento */
DO:
  IF AVAIL aprop_ctbl_ap THEN DO:
     ASSIGN fi_conta:SCREEN-VALUE  = aprop_ctbl_ap.cod_cta_ctbl.
     FIND FIRST cta_ctbl OF aprop_ctbl_ap NO-LOCK NO-ERROR.
     IF AVAIL cta_ctbl THEN
        ASSIGN fi_desc_conta:SCREEN-VALUE  = cta_ctbl.des_tit_ctbl.

         

  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btAtualizar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btAtualizar w-livre
ON CHOOSE OF btAtualizar IN FRAME f-cad /* Atualizar */
DO:
  DISABLE TRIGGERS FOR LOAD OF aprop_ctbl_ap.
  FIND CURRENT aprop_ctbl_ap EXCLUSIVE-LOCK.
  IF AVAIL aprop_ctbl_ap THEN DO:
     ASSIGN aprop_ctbl_ap.cod_cta_ctbl = fi_conta:SCREEN-VALUE .
     RELEASE aprop_ctbl_ap.
  END.
  APPLY 'choose' TO btPesquisar.
  
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btPesquisar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btPesquisar w-livre
ON CHOOSE OF btPesquisar IN FRAME f-cad /* Pesquisar */
DO:

  IF INPUT FRAME {&frame-name} fi_id_tit_ap <> 0 THEN DO:
     FIND FIRST tit_ap
      WHERE tit_ap.cod_estab = INPUT FRAME {&FRAME-NAME} fi_cod_estab
      AND  tit_ap.num_id_tit_ap = INPUT FRAME {&frame-name} fi_id_tit_ap
       NO-LOCK NO-ERROR.
      IF  AVAIL tit_ap THEN DO:
         ASSIGN fi_titulo:SCREEN-VALUE = "Fornecedor:" + string(tit_ap.cdn_fornec) + "/ Documento:" + tit_ap.cod_tit_ap + "/ Serie:" +
                                      tit_ap.cod_ser_docto + "/ Especie:" + tit_ap.cod_espec_docto + "/ Parcela:" +
                                      tit_ap.cod_parcela.
         {&OPEN-QUERY-BROWSE-2}
         APPLY 'value-changed'  TO BROWSE browse-2.
      END.
      ELSE DO:
          MESSAGE "titulo n∆o encontrado"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
          APPLY 'entry' TO fi_id_tit_ap.
          RETURN NO-APPLY.
      END.   
  END.
  ELSE DO:
     FIND FIRST tit_ap 
      WHERE tit_ap.cod_estab        = INPUT FRAME {&FRAME-NAME} fi_cod_estab
      AND   tit_ap.cod_tit_ap       = INPUT FRAME {&frame-name} fi_cod_tit_ap
      AND   tit_ap.cod_parcela      = INPUT FRAME {&FRAME-NAME} fi_parcela
      AND   tit_ap.cod_espec_docto  = INPUT FRAME {&FRAME-NAME} fi_cod_espec_docto
      AND   tit_ap.cdn_fornec       = INPUT FRAME {&FRAME-NAME} fi_cdn_fornec
      AND   tit_ap.cod_ser_docto    = INPUT FRAME {&FRAME-NAME} fi_serie
      NO-LOCK NO-ERROR.
      IF  AVAIL tit_ap THEN DO:
         ASSIGN fi_titulo:SCREEN-VALUE = "Fornecedor:" + string(tit_ap.cdn_fornec) + "/ Documento:" + tit_ap.cod_tit_ap + "/ Serie:" +
                                      tit_ap.cod_ser_docto + "/ Especie:" + tit_ap.cod_espec_docto + "/ Parcela:" +
                                      tit_ap.cod_parcela.
         {&OPEN-QUERY-BROWSE-2}
         APPLY 'value-changed'  TO BROWSE browse-2.
      END.
      ELSE DO:
          MESSAGE "titulo n∆o encontrado"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
          APPLY 'entry' TO fi_id_tit_ap.
          RETURN NO-APPLY.
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_conta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_conta w-livre
ON LEAVE OF fi_conta IN FRAME f-cad /* Conta */
DO:
  FIND FIRST cta_ctbl
      WHERE cta_ctbl.cod_cta_ctbl = fi_conta:SCREEN-VALUE NO-LOCK NO-ERROR.
     IF AVAIL cta_ctbl THEN
        ASSIGN fi_desc_conta:SCREEN-VALUE  = cta_ctbl.des_tit_ctbl.
     ELSE DO:
         MESSAGE 'CONTA N«O EXISTE'
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         APPLY 'ENTRY' TO fi_conta IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
     END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_nome_abrev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_nome_abrev w-livre
ON LEAVE OF fi_nome_abrev IN FRAME f-cad /* Fornecedor */
DO:
  FIND FIRST ems5.fornecedor
      WHERE fornecedor.nom_abrev = fi_nome_abrev:SCREEN-VALUE IN FRAME {&FRAME-NAME}
      NO-LOCK NO-ERROR.
  IF AVAIL fornecedor THEN
     ASSIGN fi_cdn_fornec:SCREEN-VALUE = IF AVAIL fornecedor THEN STRING(fornecedor.cdn_fornecedor)
                                         ELSE '0'.
  ELSE DO:
      IF  fi_nome_abrev:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> '' THEN DO:
          MESSAGE 'Fornecedor n∆o encontrado'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
          APPLY 'entry' TO fi_nome_abrev.
          RETURN NO-APPLY.
      END.                
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


&Scoped-define BROWSE-NAME BROWSE-2
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-livre 


/* ***************************  Main Block  *************************** */

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
             fi_cod_estab:HANDLE IN FRAME f-cad , 'BEFORE':U ).
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
  DISPLAY fi_cod_estab fi_id_tit_ap fi_titulo fi_cod_tit_ap fi_parcela 
          fi_cod_espec_docto fi_nome_abrev fi_cdn_fornec fi_serie fi_conta 
          fi_desc_conta 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE RECT-1 rt-button RECT-2 RECT-3 fi_cod_estab fi_id_tit_ap fi_cod_tit_ap 
         fi_parcela fi_cod_espec_docto fi_nome_abrev fi_cdn_fornec fi_serie 
         btPesquisar BROWSE-2 BROWSE-3 fi_conta fi_desc_conta btAtualizar 
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

  {utp/ut9000.i "XX9999" "9.99.99.999"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  run pi-after-initialize.
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
  {src/adm/template/snd-list.i "aprop_ctbl_ap"}
  {src/adm/template/snd-list.i "movto_tit_ap"}

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

