&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation ("PSC"),       *
* 14 Oak Park, Bedford, MA 01730, and other contributors as listed   *
* below.  All Rights Reserved.                                       *
*                                                                    *
* The Initial Developer of the Original Code is PSC.  The Original   *
* Code is Progress IDE code released to open source December 1, 2000.*
*                                                                    *
* The contents of this file are subject to the Possenet Public       *
* License Version 1.0 (the "License"); you may not use this file     *
* except in compliance with the License.  A copy of the License is   *
* available as of the date of this notice at                         *
* http://www.possenet.org/license.html                               *
*                                                                    *
* Software distributed under the License is distributed on an "AS IS"*
* basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. You*
* should refer to the License for the specific language governing    *
* rights and limitations under the License.                          *
*                                                                    *
* Contributors:                                                      *
*                                                                    *
*********************************************************************/
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

DEFINE STREAM str-rp.

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE excelappl AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE i-inx AS INT INITIAL 0 NO-UNDO.
DEFINE VARIABLE i-idi AS INT INITIAL 0 NO-UNDO.

DEFINE VARIABLE c-cfop AS CHARACTER NO-UNDO. 
DEFINE VARIABLE c-desc AS CHARACTER NO-UNDO.
DEFINE VARIABLE d-pis AS DEC NO-UNDO.
DEFINE VARIABLE c-trib-pis AS CHARACTER NO-UNDO.
DEFINE VARIABLE d-cofins AS DEC NO-UNDO.
DEFINE VARIABLE c-trib-cofins AS CHARACTER NO-UNDO.
DEFINE VARIABLE i-cst AS INT NO-UNDO.
DEFINE VARIABLE c-es AS CHARACTER NO-UNDO.

DEFINE VARIABLE c-pis AS CHARACTER FORMAT "X(10)" NO-UNDO.
DEFINE VARIABLE c-fin AS CHARACTER FORMAT "X(10)" NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RADIO-SET-1 i-cod-tribut d-dt-ini c-arquivo ~
Btn_OK BtnDone 
&Scoped-Define DISPLAYED-OBJECTS RADIO-SET-1 i-cod-tribut d-dt-ini ~
c-arquivo c-executar 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Sair" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO DEFAULT 
     LABEL "Executar" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE c-arquivo AS CHARACTER FORMAT "X(100)":U INITIAL "c:~\temp~\cst.xls" 
     LABEL "Arquivo" 
     VIEW-AS FILL-IN 
     SIZE 25 BY .88 NO-UNDO.

DEFINE VARIABLE c-executar AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 14 BY .67 NO-UNDO.

DEFINE VARIABLE d-dt-ini AS DATE FORMAT "99/99/9999":U 
     LABEL "Data Validade Inicial" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE i-cod-tribut AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "C¢d. Tribut ( 2 ou 3 )" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE RADIO-SET-1 AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Gerar planilha CFOP", 1,
"Importar planilha CD0303", 2
     SIZE 32 BY 2 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RADIO-SET-1 AT ROW 1.5 COL 5 NO-LABEL WIDGET-ID 2
     i-cod-tribut AT ROW 3.5 COL 22 COLON-ALIGNED WIDGET-ID 10
     d-dt-ini AT ROW 4.5 COL 22 COLON-ALIGNED WIDGET-ID 12
     c-arquivo AT ROW 5.5 COL 22 COLON-ALIGNED WIDGET-ID 14
     Btn_OK AT ROW 7.83 COL 5 WIDGET-ID 6
     BtnDone AT ROW 7.83 COL 15.57 WIDGET-ID 8
     c-executar AT ROW 6.5 COL 22 COLON-ALIGNED NO-LABEL WIDGET-ID 16
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 53.14 BY 8
         DEFAULT-BUTTON BtnDone WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "CST"
         HEIGHT             = 8
         WIDTH              = 53.14
         MAX-HEIGHT         = 17
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 17
         VIRTUAL-WIDTH      = 80
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN c-executar IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* CST */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* CST */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone W-Win
ON CHOOSE OF BtnDone IN FRAME F-Main /* Sair */
DO:
  &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN
    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
    &ELSE
      RUN exitObject.
    &ENDIF
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK W-Win
ON CHOOSE OF Btn_OK IN FRAME F-Main /* Executar */
DO:
  
  ASSIGN radio-set-1 i-cod-tribut d-dt-ini c-arquivo.
  
  IF radio-set-1 = 1 THEN
    RUN pi-gerar-planilha.

  IF radio-set-1 = 2 AND d-dt-ini <> ? AND ( i-cod-tribut = 2 OR i-cod-tribut = 3 ) THEN
    RUN pi-importar-planilha.
  
  APPLY 'CHOOSE' TO BtnDone.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RADIO-SET-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RADIO-SET-1 W-Win
ON ENTRY OF RADIO-SET-1 IN FRAME F-Main
DO:
  APPLY 'VALUE-CHANGED' TO radio-set-1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RADIO-SET-1 W-Win
ON VALUE-CHANGED OF RADIO-SET-1 IN FRAME F-Main
DO:
  IF radio-set-1:SCREEN-VALUE = "1" THEN
    DISABLE i-cod-tribut d-dt-ini c-arquivo WITH FRAME f-Main.
  ELSE 
    ENABLE i-cod-tribut d-dt-ini c-arquivo WITH FRAME f-Main.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
  THEN DELETE WIDGET W-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win  _DEFAULT-ENABLE
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
  DISPLAY RADIO-SET-1 i-cod-tribut d-dt-ini c-arquivo c-executar 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE RADIO-SET-1 i-cod-tribut d-dt-ini c-arquivo Btn_OK BtnDone 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-gerar-planilha W-Win 
PROCEDURE pi-gerar-planilha :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/**/
CREATE "Excel.Application" excelAppl.
excelAppl:Workbooks:Add.
excelappl:worksheets:ITEM(1):SELECT.

ASSIGN i-inx = 1.
excelappl:range("A" + string(i-inx, "99999")):value = "CFOP".
excelappl:range("B" + string(i-inx, "99999")):value = "Denominacao".
excelappl:range("C" + string(i-inx, "99999")):value = "% Pis".
excelappl:range("D" + string(i-inx, "99999")):value = "Trib.".
excelappl:range("E" + string(i-inx, "99999")):value = "% Cofins".
excelappl:range("F" + string(i-inx, "99999")):value = "Trib.".
excelappl:range("G" + string(i-inx, "99999")):value = "CST".
excelappl:range("H" + string(i-inx, "99999")):value = "E/S".

FOR EACH natur-oper NO-LOCK:
    ASSIGN c-pis = IF SUBSTRING(natur-oper.char-1,86,1) = "1" THEN "Tributado"
              ELSE IF SUBSTRING(natur-oper.char-1,86,1) = "2" THEN "Isento"
              ELSE IF SUBSTRING(natur-oper.char-1,86,1) = "3" THEN "Outros"
              ELSE IF SUBSTRING(natur-oper.char-1,86,1) = "4" THEN "Reduzido"
              ELSE "".
    ASSIGN c-fin = IF SUBSTRING(natur-oper.char-1,87,1) = "1" THEN "Tributado"
              ELSE IF SUBSTRING(natur-oper.char-1,87,1) = "2" THEN "Isento"
              ELSE IF SUBSTRING(natur-oper.char-1,87,1) = "3" THEN "Outros"
              ELSE IF SUBSTRING(natur-oper.char-1,87,1) = "4" THEN "Reduzido"
              ELSE "".

    ASSIGN i-inx = i-inx + 1.
    excelappl:range("A" + string(i-inx, "99999")):value = natur-oper.nat-operacao.
    excelappl:range("B" + string(i-inx, "99999")):value = natur-oper.denominacao.
    excelappl:range("C" + string(i-inx, "99999")):value = natur-oper.perc-pis[1].
    excelappl:range("D" + string(i-inx, "99999")):value = c-pis.
    excelappl:range("E" + string(i-inx, "99999")):value = natur-oper.per-fin-soc[1].
    excelappl:range("F" + string(i-inx, "99999")):value = c-fin.
    
    ASSIGN c-executar:SCREEN-VALUE IN FRAME f-Main = STRING(i-inx).

END.

ASSIGN i-inx = i-inx + 1.
excelappl:range("A" + string(i-inx, "99999")):value = "Fim".

excelAppl:VISIBLE=TRUE.

RELEASE OBJECT excelAppl.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-importar-planilha W-Win 
PROCEDURE pi-importar-planilha :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

CREATE "excel.application" excelappl.
excelappl:VISIBLE = NO. 
excelappl:workbooks:ADD(c-arquivo). 
excelappl:worksheets:ITEM(1):SELECT.
/**/
ASSIGN i-inx = 1.

REPEAT:

  ASSIGN i-inx = i-inx + 1.

  IF excelappl:range("A" + STRING(i-inx, "999999")):VALUE = "Fim" THEN
    LEAVE.

  
  ASSIGN c-cfop = excelappl:range("A" + STRING(i-inx, "999999")):VALUE.
  ASSIGN c-desc = excelappl:range("B" + STRING(i-inx, "999999")):VALUE.
  ASSIGN d-pis = DEC(excelappl:range("C" + STRING(i-inx, "999999")):VALUE).
  ASSIGN c-trib-pis = excelappl:range("D" + STRING(i-inx, "999999")):VALUE.
  ASSIGN d-cofins = DEC(excelappl:range("E" + STRING(i-inx, "999999")):VALUE).
  ASSIGN c-trib-cofins = excelappl:range("F" + STRING(i-inx, "999999")):VALUE.
  ASSIGN i-cst = INT(excelappl:range("G" + STRING(i-inx, "999999")):VALUE).
  ASSIGN c-es = excelappl:range("H" + STRING(i-inx, "999999")):VALUE.


  IF i-cst <= 0 THEN
    NEXT.
  IF i-cst <= ? THEN
    NEXT.

  FIND sit-tribut WHERE sit-tribut.cdn-tribut = i-cod-tribut
                    AND sit-tribut.cdn-sit-tribut = i-cst
                    AND sit-tribut.dat-valid-inic = d-dt-ini
                NO-LOCK NO-ERROR.
  IF NOT AVAILABLE sit-tribut THEN
  DO:
    CREATE sit-tribut.
    ASSIGN sit-tribut.cdn-tribut = i-cod-tribut
           sit-tribut.cdn-sit-tribut = i-cst   
           sit-tribut.dat-valid-inic = d-dt-ini.
  END.
  
  ASSIGN i-idi = IF TRIM(c-es) = "E" THEN 1 ELSE 2.




  FIND sit-tribut-relacto WHERE sit-tribut-relacto.cdn-tribut = i-cod-tribut
                            AND sit-tribut-relacto.idi-tip-docto = i-idi
                            AND sit-tribut-relacto.cod-estab = "*"
                            AND sit-tribut-relacto.cod-natur-operac = c-cfop
                            AND sit-tribut-relacto.cod-ncm = "*"
                            AND sit-tribut-relacto.cod-item = "*"
                            AND sit-tribut-relacto.cdn-grp-emit = 0
                            AND sit-tribut-relacto.cdn-emitente = 0
                            AND sit-tribut-relacto.dat-valid-inic = d-dt-ini
                            AND sit-tribut-relacto.cdn-sit-tribut = i-cst
                       NO-ERROR.
  IF NOT AVAILABLE sit-tribut-relacto THEN
    CREATE sit-tribut-relacto.
  ASSIGN sit-tribut-relacto.cdn-tribut = i-cod-tribut
         sit-tribut-relacto.idi-tip-docto = i-idi    
         sit-tribut-relacto.cod-estab = "*"          
         sit-tribut-relacto.cod-natur-operac = c-cfop
         sit-tribut-relacto.cod-ncm = "*"            
         sit-tribut-relacto.cod-item = "*"           
         sit-tribut-relacto.cdn-grp-emit = 0         
         sit-tribut-relacto.cdn-emitente = 0         
         sit-tribut-relacto.dat-valid-inic = d-dt-ini
         sit-tribut-relacto.cdn-sit-tribut = i-cst. 

  ASSIGN c-executar:SCREEN-VALUE IN FRAME f-Main = STRING(i-inx).

END.  

RELEASE OBJECT excelAppl.

MESSAGE "Importa‡Æo finalizada!"
    VIEW-AS ALERT-BOX.

END PROCEDURE.

/*
Table: sit-tribut-relacto

Flags Index Name                    St Area Cnt Field Name
----- ----------------------------- ------- --- ------------------------------
      sttrbtrl-cst                  6         3 + cdn-tribut
                                                + cdn-sit-tribut
                                                + dat-valid-inic

      sttrbtrl-emit                 6         2 + cdn-emitente
                                                + dat-valid-inic

      sttrbtrl-estab                6         2 + cod-estab
                                                + dat-valid-inic

pu    sttrbtrl-id                   6        10 + cdn-tribut
                                                + idi-tip-docto
                                                + cod-estab
                                                + cod-natur-operac
                                                + cod-ncm
                                                + cod-item
                                                + cdn-grp-emit
                                                + cdn-emitente
                                                + dat-valid-inic
                                                + cdn-sit-tribut

      sttrbtrl-item                 6         2 + cod-item
                                                + dat-valid-inic

      sttrbtrl-oper                 6         2 + cod-natur-operac
                                                + dat-valid-inic


*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartWindow, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Win 
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

