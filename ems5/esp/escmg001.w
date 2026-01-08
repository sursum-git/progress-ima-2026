&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
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

DEFINE TEMP-TABLE ttConta
    FIELD conta AS CHAR FORMAT 'x(12)'
    FIELD valor AS DECIMAL FORMAT '->>>,>>>,>>>.99'.

DEFINE TEMP-TABLE ttEstab
    FIELD codEstab AS CHAR LABEL "Estab."
    FIELD valor AS DECIMAL FORMAT '->>>,>>>,>>>.99' LABEL "Valor"
    FIELD tipo  AS CHAR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-button btexecutar fiData 
&Scoped-Define DISPLAYED-OBJECTS fiData 

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
DEFINE BUTTON btexecutar 
     LABEL "Gerar Lanáamentos Fluxo" 
     SIZE 20.14 BY 1.13.

DEFINE VARIABLE fiData AS DATE FORMAT "99/99/9999":U 
     LABEL "Data Corte" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 89.72 BY 1.46
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     btexecutar AT ROW 4.17 COL 50.57 WIDGET-ID 2
     fiData AT ROW 4.25 COL 33 COLON-ALIGNED WIDGET-ID 4
     rt-button AT ROW 1 COL 1
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
         TITLE              = "Template Livre <Insira complemento>"
         HEIGHT             = 8
         WIDTH              = 88.29
         MAX-HEIGHT         = 17
         MAX-WIDTH          = 90.43
         VIRTUAL-HEIGHT     = 17
         VIRTUAL-WIDTH      = 90.43
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
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
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


&Scoped-define SELF-NAME btexecutar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btexecutar w-livre
ON CHOOSE OF btexecutar IN FRAME f-cad /* Gerar Lanáamentos Fluxo */
DO:
  ASSIGN btExecutar:SENSITIVE = NO
         btExecutar:LABEL = "Processando...".
  RUN piDelMovFluxo.
  RUN piBuscarSaldo.
  RUN piBuscarTitVencido.
  RUN piGerarFluxo.
  RUN piGerarLog.
  ASSIGN btExecutar:SENSITIVE = YES
         btExecutar:LABEL = "Gerar Lanáamentos Fluxo".
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
       RUN set-position IN h_p-exihel ( 1.17 , 73.29 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             btexecutar:HANDLE IN FRAME f-cad , 'BEFORE':U ).
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
  DISPLAY fiData 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE rt-button btexecutar fiData 
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
  ASSIGN fidata:SCREEN-VALUE = STRING(TODAY).
  /* Code placed here will execute AFTER standard behavior.    */

  run pi-after-initialize.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piBuscarSaldo w-livre 
PROCEDURE piBuscarSaldo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
------------------------------------------------------------------------------*/
DEFINE VARIABLE dTotal   AS DECIMAL     NO-UNDO FORMAT '->>>,>>>,>>>,>>>.99'.
DEFINE VARIABLE iSinal   AS INTEGER     NO-UNDO.

/*OUTPUT TO C:\TEMP\PERFORMANCE.TXT.*/
FOR EACH cta_corren  NO-LOCK
    WHERE log_atualiz_fluxo_cx = YES,
    EACH movto_cta_corren OF cta_corren NO-LOCK
    WHERE movto_cta_corren.ind_tip_movto_cta_corren = 're'
    AND  movto_cta_corren.dat_movto_cta_corren < INPUT FRAME {&FRAME-NAME} fiData:
    /*DISP cta_corren.COD_CTA_CORREN movto_cta_corren.val_movto_cta_corren.*/
    
    FIND FIRST ttEstab
        WHERE ttEstab.codEstab =  cta_corren.cod_estab 
        AND   ttEstab.tipo = 'saldo' NO-LOCK NO-ERROR.

    IF NOT AVAIL ttEstab THEN DO:
      CREATE ttEstab.
      ASSIGN ttEstab.codEstab =  cta_corren.cod_estab
             ttEstab.tipo     = 'saldo' .
    END.
    FIND FIRST ttConta
        WHERE ttConta.conta = cta_corren.cod_cta_corren NO-LOCK NO-ERROR.
    IF NOT AVAIL ttConta THEN DO:
       CREATE ttConta.
       ASSIGN ttConta.conta = cta_corren.cod_cta_corren.
    END.  

    IF movto_cta_corren.ind_fluxo_movto_cta_corren = 'ent' THEN
       ASSIGN iSinal = 1.
    ELSE
      ASSIGN iSinal = -1.

    ASSIGN ttEstab.valor = ttEstab.valor + movto_cta_corren.val_movto_cta_corren  * iSinal
           ttConta.valor = ttConta.valor + movto_cta_corren.val_movto_cta_corren  * iSinal.
END.
/*OUTPUT CLOSE.*/



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piBuscarTitVencido w-livre 
PROCEDURE piBuscarTitVencido :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE iSinal AS INTEGER     NO-UNDO.
/*DEFINE VARIABLE dSaldo AS DECIMAL     NO-UNDO FORMAT '->>>,>>>,>>>,>>>.>>'.*/
OUTPUT TO c:\temp\logTitVenc.txt.
FOR EACH tit_acr NO-LOCK
    WHERE tit_acr.dat_vencto_tit_acr < TODAY
    AND   tit_acr.log_sdo_tit_acr = YES,
    EACH espec_docto_financ OF tit_acr NO-LOCK
    WHERE espec_docto_financ.LOG_atualiz_fluxo_cx = YES.
    FIND FIRST ttEstab
        WHERE ttEstab.codEstab = tit_acr.cod_estab
        AND   ttEstab.tipo     = 'vencido' NO-LOCK NO-ERROR.
    IF NOT AVAIL ttEstab THEN DO:
        CREATE ttEstab.
        ASSIGN ttEstab.codEstab = tit_acr.cod_estab  
               ttEstab.tipo     = 'vencido'.
    END.
    FIND FIRST ems5.espec_docto OF espec_docto_financ NO-LOCK NO-ERROR.
    IF AVAIL espec_docto  THEN DO:
       IF espec_docto.ind_tip_espec_docto = 'Antecipaá∆o' THEN DO:
           ASSIGN iSinal = -1.
       END.
       ELSE DO: 
           /*IF  tit_acr.cod_espec_docto = 'an' THEN
               MESSAGE espec_docto.ind_tip_espec_docto
                   VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
         ASSIGN iSinal = 1.
       END.
         
    END.
    ELSE DO:
        ASSIGN iSinal = 1.
    END.
      
    ASSIGN ttEstab.valor = ttEstab.valor + tit_acr.val_sdo_tit_acr * iSinal.
    DISP  tit_acr.dat_vencto_tit_acr tit_acr.cod_estab tit_acr.cod_espec_docto tit_acr.cod_tit_acr tit_acr.cod_parcela tit_acr.val_sdo_tit_acr * iSinal COLUMN-LABEL "Vl.Original"
        val_origin_tit_acr * iSinal COLUMN-LABEL "Vl.Saldo"
        WITH WIDTH 550.

END.

FOR EACH ttEstab
    WHERE tipo = "vencido":
    DISP ttestab.
END.
OUTPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piDelMovFluxo w-livre 
PROCEDURE piDelMovFluxo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH movto_fluxo_cx EXCLUSIVE-LOCK 
    WHERE movto_fluxo_cx.dat_movto_fluxo_cx  = INPUT FRAME {&FRAME-NAME} fidata
    AND   movto_fluxo_cx.num_livre_2 = 111:
    DELETE movto_fluxo_cx.

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piGerarFluxo w-livre 
PROCEDURE piGerarFluxo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cHistorico AS CHARACTER   NO-UNDO FORMAT 'x(60)'.
DEFINE VARIABLE cTipo      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iSeq       AS INTEGER     NO-UNDO.
FOR EACH ttEstab 
    WHERE ttEstab.valor <> 0 :
    IF ttEstab.tipo = 'saldo' THEN DO:
       ASSIGN cHistorico = 'Saldo Anterior das contas banc†rias'
              cTipo      ='0S'.
              
    END.
    ELSE DO:
       ASSIGN cHistorico = 'Valores de Titulos a Receber Vencidos'
              cTipo      = '0V'.
              
    END.
    RUN piGerarSeq(0,INPUT FRAME {&frame-name} fiData, OUTPUT iSeq).
    /*MESSAGE 'criei registro'
        VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
    CREATE movto_fluxo_cx.
    ASSIGN movto_fluxo_cx.ind_tip_movto_fluxo_cx                = 'PR'
           movto_fluxo_cx.num_fluxo_cx                          =  0
           movto_fluxo_cx.num_seq_movto_fluxo_cx                = iSeq
           movto_fluxo_cx.dat_movto_fluxo_cx                    = INPUT FRAME {&frame-name} fiData
           movto_fluxo_cx.cod_estab                             = ttEstab.codEstab
           movto_fluxo_cx.cod_unid_negoc                        = '001'
           movto_fluxo_cx.cod_tip_fluxo_financ                  = cTipo
           movto_fluxo_cx.ind_fluxo_movto_cx                    = IF ttEstab.valor > 0 THEN 'ent' ELSE 'sai'
           movto_fluxo_cx.cod_modul_dtsul                       = 'cfl'
           movto_fluxo_cx.des_histor_movto_fluxo_cx             = cHistorico
           movto_fluxo_cx.num_id_movto_fluxo_cx                 = next-value(seq_movto_fluxo_cx)
           movto_fluxo_cx.ind_tip_secao_fluxo_cx                = 'Anal°tica'
           movto_fluxo_cx.num_livre_2                           = 111
           movto_fluxo_cx.val_movto_fluxo_cx                    = IF ttEstab.valor < 0 THEN ttEstab.valor * -1 ELSE ttEstab.valor.

END.

END PROCEDURE.

/*

=========================================================================
============================= Table: movto_fluxo_cx =====================

              Table Flags: "f" = frozen, "s" = a SQL table


Table                            Table Field Index Table
Name                             Flags Count Count Label
-------------------------------- ----- ----- ----- ------------------------
movto_fluxo_cx                            28     8 Movimento Fluxo Caixa

    Dump Name: fin601
  Description: Movimento Fluxo Caixa
 Storage Area: dados

       Trigger Event Trigger Procedure    Overridable? Check CRC?
       ------------- -------------------- ------------ ----------
       delete        database/tgfin/tdp/t yes          no
       write         database/tgfin/twp/t yes          no


============================= FIELD SUMMARY =============================
============================= Table: movto_fluxo_cx =====================

Flags: <c>ase sensitive, <i>ndex component, <m>andatory, <v>iew component

Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 num_fluxo_cx                     inte        im
   20 dat_movto_fluxo_cx               date        im
   30 num_seq_movto_fluxo_cx           inte        im
   40 cod_estab                        char        im
   50 cod_unid_negoc                   char        im
   60 cod_tip_fluxo_financ             char        im
   70 ind_fluxo_movto_cx               char        m
   80 ind_tip_movto_fluxo_cx           char        im
   90 cod_modul_dtsul                  char        im
  100 val_movto_fluxo_cx               deci-2      m
  110 val_perc_cop_fluxo_cx            deci-2      m
  120 cod_histor_padr                  char        im
  130 des_histor_movto_fluxo_cx        char        im
  140 cod_empresa                      char        m
  150 ind_tip_secao_fluxo_cx           char        m
  160 num_id_movto_fluxo_cx            inte        im
  170 cod_livre_1                      char
  180 dat_prev_orig                    date
  190 cod_estab_orig                   char        m
  200 cod_livre_2                      char
  210 dat_livre_1                      date
  220 dat_livre_2                      date
  230 log_livre_1                      logi
  240 log_livre_2                      logi
  250 num_livre_1                      inte
  260 num_livre_2                      inte
  270 val_livre_1                      deci-4
  280 val_livre_2                      deci-4

Field Name                       Format
-------------------------------- -----------------------------
num_fluxo_cx                     >>>>>,>>9
dat_movto_fluxo_cx               99/99/9999
num_seq_movto_fluxo_cx           >>>>9
cod_estab                        x(5)
cod_unid_negoc                   x(3)
cod_tip_fluxo_financ             x(12)
ind_fluxo_movto_cx               X(3)
ind_tip_movto_fluxo_cx           X(2)
cod_modul_dtsul                  x(3)
val_movto_fluxo_cx               >>,>>>,>>>,>>9.99
val_perc_cop_fluxo_cx            ->>,>>>,>>>,>>9.99
cod_histor_padr                  x(8)
des_histor_movto_fluxo_cx        x(2000)
cod_empresa                      x(3)
ind_tip_secao_fluxo_cx           X(11)
num_id_movto_fluxo_cx            9999999999
cod_livre_1                      x(100)
dat_prev_orig                    99/99/9999
cod_estab_orig                   x(5)
cod_livre_2                      x(100)
dat_livre_1                      99/99/9999
dat_livre_2                      99/99/9999
log_livre_1                      Sim/N∆o
log_livre_2                      Sim/N∆o
num_livre_1                      >>>>>9
num_livre_2                      >>>>>9
val_livre_1                      >>>,>>>,>>9.9999
val_livre_2                      >>>,>>>,>>9.9999

Field Name                       Initial
-------------------------------- -----------------------------
num_fluxo_cx                     0
dat_movto_fluxo_cx               today
num_seq_movto_fluxo_cx           0
cod_estab
cod_unid_negoc
cod_tip_fluxo_financ
ind_fluxo_movto_cx               ENT
ind_tip_movto_fluxo_cx           PR
cod_modul_dtsul
val_movto_fluxo_cx               0
val_perc_cop_fluxo_cx            0
cod_histor_padr
des_histor_movto_fluxo_cx
cod_empresa
ind_tip_secao_fluxo_cx           Anal°tica
num_id_movto_fluxo_cx            0
cod_livre_1
dat_prev_orig                    ?
cod_estab_orig
cod_livre_2
dat_livre_1                      ?
dat_livre_2                      ?
log_livre_1                      no
log_livre_2                      no
num_livre_1                      0
num_livre_2                      0
val_livre_1                      0
val_livre_2                      0

Field Name                     Label                  Column Label
------------------------------ ---------------------- ----------------------
num_fluxo_cx                   Fluxo Caixa            Fluxo Caixa
dat_movto_fluxo_cx             Data Movimento         Data Movimento
num_seq_movto_fluxo_cx         Sequància              Sequància
cod_estab                      Estabelecimento        Estab
cod_unid_negoc                 Unid Neg¢cio           Un Neg
cod_tip_fluxo_financ           Tipo Fluxo Financ      Tipo Fluxo Financ
ind_fluxo_movto_cx             Fluxo Movimento        Fluxo Movimento
ind_tip_movto_fluxo_cx         Tipo Movimento         Tipo Movimento
cod_modul_dtsul                M¢dulo                 M¢dulo
val_movto_fluxo_cx             Valor Movto            Valor Movto
val_perc_cop_fluxo_cx          Perc Copia Fluxo       Perc Copia Fluxo
cod_histor_padr                Hist¢rico Padr∆o       Hist¢rico Padr∆o
des_histor_movto_fluxo_cx      Hist¢rico Movimento    Hist¢rico Movimento
cod_empresa                    Empresa                Empresa
ind_tip_secao_fluxo_cx         Tipo Seá∆o Fluxo       Seá∆o Fluxo
num_id_movto_fluxo_cx          Id Movto Fluxo Cx      Id Movto Fluxo Cx
cod_livre_1                    Livre 1                Livre 1
dat_prev_orig                  Previsao Origem        Previsao Origem
cod_estab_orig                 Estab Origem           Estab Origem
cod_livre_2                    Livre 2                Livre 2
dat_livre_1                    Livre 1                Livre 1
dat_livre_2                    Livre 2                Livre 2
log_livre_1                    Livre 1                Livre 1
log_livre_2                    Livre 2                Livre 2
num_livre_1                    Livre 1                Livre 1
num_livre_2                    Livre 2                Livre 2
val_livre_1                    Livre 1                Livre 1
val_livre_2                    Livre 2                Livre 2


============================= INDEX SUMMARY =============================
============================= Table: movto_fluxo_cx =====================

Flags: <p>rimary, <u>nique, <w>ord, <a>bbreviated, <i>nactive, + asc, - desc

Flags Index Name                       Cnt Field Name
----- -------------------------------- --- ---------------------------------
      mvtflxcx_estab_unid_negoc          3 + num_fluxo_cx
                                           + cod_estab
                                           + cod_unid_negoc

w     mvtflxcx_historico                 1 + des_histor_movto_fluxo_cx

      mvtflxcx_hstrpdr                   1 + cod_histor_padr

pu    mvtflxcx_id                        3 + num_fluxo_cx
                                           + dat_movto_fluxo_cx
                                           + num_seq_movto_fluxo_cx

      mvtflxcx_modul                     4 + num_fluxo_cx
                                           + cod_modul_dtsul
                                           + dat_movto_fluxo_cx
                                           + num_seq_movto_fluxo_cx

      mvtflxcx_tip_fluxo                 5 + cod_tip_fluxo_financ
                                           + dat_movto_fluxo_cx
                                           + cod_unid_negoc
                                           + cod_estab
                                           + num_fluxo_cx

      mvtflxcx_tip_movto                 3 + num_fluxo_cx
                                           + ind_tip_movto_fluxo_cx
                                           + dat_movto_fluxo_cx

u     mvtflxcx_token                     1 + num_id_movto_fluxo_cx

** Index Name: mvtflxcx_estab_unid_negoc
 Storage Area: indices
** Index Name: mvtflxcx_historico
 Storage Area: indices
** Index Name: mvtflxcx_hstrpdr
 Storage Area: indices
** Index Name: mvtflxcx_id
 Storage Area: indices
** Index Name: mvtflxcx_modul
 Storage Area: indices
** Index Name: mvtflxcx_tip_fluxo
 Storage Area: indices
** Index Name: mvtflxcx_tip_movto
 Storage Area: indices
** Index Name: mvtflxcx_token
 Storage Area: indices


============================= FIELD DETAILS =============================
============================= Table: movto_fluxo_cx =====================

** Field Name: num_fluxo_cx
  Description: N£mero Fluxo Caixa
         Help: N£mero Fluxo Caixa

** Field Name: dat_movto_fluxo_cx
  Description: Data do Movimento de Fluxo de Caixa
         Help: Data do Movimento de Fluxo de Caixa

** Field Name: num_seq_movto_fluxo_cx
  Description: N£mero de Sequància do Movimento de Fluxo Caixa
         Help: N£mero de Sequància do Movimento de Fluxo Caixa

** Field Name: cod_estab
  Description: C¢digo Estabelecimento
         Help: C¢digo Estabelecimento

** Field Name: cod_unid_negoc
  Description: C¢digo Unidade Neg¢cio
         Help: C¢digo Unidade Neg¢cio

** Field Name: cod_tip_fluxo_financ
  Description: C¢digo Tipo Fluxo Financeiro
         Help: C¢digo Tipo Fluxo Financeiro

** Field Name: ind_fluxo_movto_cx
  Description: Fluxo do Movimento de Caixa
         Help: Fluxo do Movimento de Caixa
      Val-Msg: FV: Valor n∆o contido na lista de valores poss°veis !
      Val-Exp: can-do("ENT,SAI", movto_fluxo_cx.ind_fluxo_movto_cx)

** Field Name: ind_tip_movto_fluxo_cx
  Description: Tipo de Movimento de Fluxo Caixa
         Help: Tipo de Movimento de Fluxo Caixa
      Val-Msg: FV: Valor n∆o contido na lista de valores poss°veis !
      Val-Exp: can-do("PR,RE,OR", movto_fluxo_cx.ind_tip_movto_fluxo_cx)

** Field Name: cod_modul_dtsul
  Description: C¢digo M¢dulo
         Help: C¢digo M¢dulo

** Field Name: val_movto_fluxo_cx
  Description: Valor do Movimento de Fluxo Caixa
         Help: Valor do Movimento de Fluxo Caixa

** Field Name: val_perc_cop_fluxo_cx
  Description: Valor Percentual Copia Fluxo Caixa
         Help: Valor Percentual Copia Fluxo Caixa

** Field Name: cod_histor_padr
  Description: C¢digo Hist¢rico Padr∆o
         Help: C¢digo Hist¢rico Padr∆o

** Field Name: des_histor_movto_fluxo_cx
  Description: Hist¢rico Movimento Fluxo Caixa
         Help: Hist¢rico Movimento Fluxo Caixa

** Field Name: cod_empresa
  Description: C¢digo Empresa
         Help: C¢digo Empresa

** Field Name: ind_tip_secao_fluxo_cx
  Description: Indicador do Tipo de Seá∆o do Fluxo de Caixa
         Help: Indicador do Tipo de Seá∆o do Fluxo de Caixa
      Val-Msg: FV: Valor n∆o contido na lista de valores poss°veis !
      Val-Exp: can-do("Anal°tica,SintÇtica,Totalizaá∆o,Sdo Inicial,Sdo
               Final", movto_fluxo_cx.ind_tip_secao_fluxo_cx)

** Field Name: num_id_movto_fluxo_cx
  Description: N£mero de Identificaá∆o do Movimento de Fluxo Caixa
         Help: N£mero de Identificaá∆o do Movimento de Fluxo Caixa

** Field Name: cod_livre_1
  Description: Atributo livre para uso interno
         Help: Atributo livre para uso interno

** Field Name: dat_prev_orig
  Description: Data Previsao Origem
         Help: Data Previsao Origem

** Field Name: cod_estab_orig
  Description: C¢digo Estabelecimento Origem
         Help: C¢digo Estabelecimento Origem

** Field Name: cod_livre_2
  Description: Atributo livre para uso interno
         Help: Atributo livre para uso interno

** Field Name: dat_livre_1
  Description: Atributo livre para uso interno
         Help: Atributo livre para uso interno

** Field Name: dat_livre_2
  Description: Atributo livre para uso interno
         Help: Atributo livre para uso interno

** Field Name: log_livre_1
  Description: Atributo livre para uso interno
         Help: Atributo livre para uso interno

** Field Name: log_livre_2
  Description: Atributo livre para uso interno
         Help: Atributo livre para uso interno

** Field Name: num_livre_1
  Description: Atributo livre para uso interno
         Help: Atributo livre para uso interno

** Field Name: num_livre_2
  Description: Atributo livre para uso interno
         Help: Atributo livre para uso interno

** Field Name: val_livre_1
  Description: Atributo livre para uso interno
         Help: Atributo livre para uso interno

** Field Name: val_livre_2
  Description: Atributo livre para uso interno
         Help: Atributo livre para uso interno


*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piGerarLog w-livre 
PROCEDURE piGerarLog :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cArquivo AS CHARACTER   NO-UNDO FORMAT 'x(50)'.
ASSIGN cArquivo = "saldo.txt".
OUTPUT TO value("c:\temp\" + cArquivo).
FOR EACH ttConta:
    DISP ttConta.
    /*ASSIGN dTotal = dTotal + ttConta.valor.*/
END.
PUT SKIP "Registros Gerados" SKIP FILL("=", 80) SKIP.
PUT "estab." AT 1
    "valor"  AT 10 SKIP. 
FOR EACH ttEstab:
   PUT  ttEstab.codEstab AT 1
        ttEstab.valor    AT 10 
        ttEstab.tipo     AT 30 SKIP.
END.
OUTPUT CLOSE.

OS-COMMAND SILENT START notepad c:\temp\logTitVenc.txt.
OS-COMMAND SILENT VALUE('start notepad c:\temp\' + cArquivo).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piGerarSeq w-livre 
PROCEDURE piGerarSeq :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER  p_num_fluxo AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER  p_data      AS DATE        NO-UNDO.
DEFINE OUTPUT PARAMETER v_num_seq_movto_fluxo_cx  AS INTEGER     NO-UNDO.
find last movto_fluxo_cx
     where movto_fluxo_cx.num_fluxo_cx       = p_num_fluxo
     and   movto_fluxo_cx.dat_movto_fluxo_cx = p_data
     no-lock no-error.
if  avail movto_fluxo_cx then
    assign v_num_seq_movto_fluxo_cx = movto_fluxo_cx.num_seq_movto_fluxo_cx + 10.
else
    assign v_num_seq_movto_fluxo_cx = 10.

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

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this w-livre, and there are no
     tables specified in any contained Browse, Query, or Frame. */

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

