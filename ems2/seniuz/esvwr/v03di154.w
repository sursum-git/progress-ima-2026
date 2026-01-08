&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          ems206           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i V03DI154 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
&Scop adm-attribute-dlg support/viewerd.w

/* buffer definitions */

/* global variable definitions */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
def var v-row-parent as rowid no-undo.

DEF VAR i-dv-dig-calc    LIKE ref-item-ext.dv.
DEF VAR l-dv-ok          AS LOG.
DEF VAR c-lote           AS CHAR.
DEF VAR c-situacao       AS CHAR.
DEF VAR de-acm-res       AS DEC.
DEF VAR l-achou          AS LOG.
DEF VAR i-volume-ini-aux AS INT.
DEF VAR i-volume-fim-aux AS INT.
DEF VAR l-conf-exc       AS LOG.
DEF VAR l-completo       AS LOG.
DEF VAR l-seq-vol-ok     AS LOG.

DEF VAR i-sit-aval LIKE ped-venda.cod-sit-aval.
DEF VAR i-cod-mess LIKE ped-venda.cod-message-alert.      
DEF VAR da-dt-mess LIKE ped-venda.dt-mensagem.
DEF VAR c-desc-for LIKE ped-venda.desc-forc-cr.           
DEF VAR l-dsp-fat  LIKE ped-venda.dsp-pre-fat.

DEF NEW GLOBAL SHARED VAR h-p-cadsi2      AS HANDLE.
DEF NEW GLOBAL SHARED VAR h-w-cadsi2      AS HANDLE.
DEF NEW GLOBAL SHARED VAR fi-dias-res     AS INT.
DEF NEW GLOBAL SHARED VAR fi-serie-entr   LIKE movto-estoq.serie.
DEF NEW GLOBAL SHARED VAR fi-perc-var-res AS INT.
DEF NEW GLOBAL SHARED VAR fi-ajuste-res   AS INT.

DEF NEW GLOBAL SHARED VAR v-nome-abrev   LIKE ped-item.nome-abrev.
DEF NEW GLOBAL SHARED VAR v-nr-pedcli    LIKE ped-item.nr-pedcli.
DEF NEW GLOBAL SHARED VAR v-nr-sequencia LIKE ped-item.nr-sequencia.

DEF BUFFER b-ped-item-res FOR ped-item-res.
DEF BUFFER b-ped-item-ext FOR ped-item-ext.

DEF TEMP-TABLE tt-ped-item LIKE ped-item 
    FIELD r-rowid AS ROWID.

DEFINE TEMP-TABLE RowErrors NO-UNDO
       FIELD ErrorSequence    AS INTEGER
       FIELD ErrorNumber      AS INTEGER
       FIELD ErrorDescription AS CHARACTER
       FIELD ErrorParameters  AS CHARACTER
       FIELD ErrorType        AS CHARACTER
       FIELD ErrorHelp        AS CHARACTER
       FIELD ErrorSubType     AS CHARACTER.

DEF VAR h-bodi154 AS HANDLE.
DEF VAR h-bodi154com AS HANDLE.

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
&Scoped-define EXTERNAL-TABLES ped-item ped-venda
&Scoped-define FIRST-EXTERNAL-TABLE ped-item


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR ped-item, ped-venda.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS ped-item.nr-sequencia 
&Scoped-define ENABLED-TABLES ped-item
&Scoped-define FIRST-ENABLED-TABLE ped-item
&Scoped-Define ENABLED-OBJECTS RECT-26 RECT-27 rt-key rt-mold 
&Scoped-Define DISPLAYED-FIELDS ped-item.nome-abrev ped-item.nr-pedcli ~
ped-venda.dt-implant ped-item.nr-sequencia ped-item.dt-entrega ~
ped-item.it-codigo ped-item.cod-refer ped-item.qt-pedida 
&Scoped-define DISPLAYED-TABLES ped-item ped-venda
&Scoped-define FIRST-DISPLAYED-TABLE ped-item
&Scoped-define SECOND-DISPLAYED-TABLE ped-venda
&Scoped-Define DISPLAYED-OBJECTS fi-nome-emit fi-desc-situacao fi-desc-item ~
fi-dv to-faturado fi-cod-lote fi-cod-refer fi-qtd-ultent fi-quantidade ~
fi-dat-ultent fi-qtd-volum fi-volume-ini fi-qtd-dispon fi-volume-fin ~
fi-sld-dispon fi-sigla-emb fi-desc-dentro 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define List-4 fi-dv fi-quantidade fi-qtd-volum fi-volume-ini ~
fi-volume-fin fi-sigla-emb fi-desc-dentro 
&Scoped-define List-5 ped-item.nr-sequencia 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
/**************************
</EXECUTING-CODE> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-exclui-res 
     LABEL "Excluir Reserva" 
     SIZE 16 BY 1.13 TOOLTIP "Exclui a reserva.".

DEFINE VARIABLE fi-cod-lote AS CHARACTER FORMAT "X(256)":U 
     LABEL "Lote" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-refer AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dat-ultent AS DATE FORMAT "99/99/9999":U 
     LABEL "Data" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-dentro AS CHARACTER FORMAT "X(10)":U 
     LABEL "Desc Dentro Emb" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-item AS CHARACTER FORMAT "x(60)" 
     VIEW-AS FILL-IN 
     SIZE 52.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-situacao AS CHARACTER FORMAT "X(20)":U 
     LABEL "Situaá∆o" 
     VIEW-AS FILL-IN 
     SIZE 13.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dv AS CHARACTER FORMAT "9":U INITIAL "0" 
     LABEL "DV" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 TOOLTIP "D°gito verificador do ÷tem" NO-UNDO.

DEFINE VARIABLE fi-nome-emit AS CHARACTER FORMAT "X(40)" 
     VIEW-AS FILL-IN 
     SIZE 53.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-qtd-dispon AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Dispon°vel" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 TOOLTIP "Quantidade dispon°vel antes dessa reserva." NO-UNDO.

DEFINE VARIABLE fi-qtd-ultent AS DECIMAL FORMAT ">>,>>9.99":U INITIAL 0 
     LABEL "Quantidade" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE fi-qtd-volum AS CHARACTER FORMAT "X(256)":U 
     LABEL "Quant de Volumes" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-quantidade AS DECIMAL FORMAT ">>,>>9.99":U INITIAL 0 
     LABEL "Quantidade" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 TOOLTIP "Quantidade a ser reservada." NO-UNDO.

DEFINE VARIABLE fi-sigla-emb AS CHARACTER FORMAT "!xx":U 
     LABEL "Sigla Embalagem" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-sld-dispon AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Saldo" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 TOOLTIP "Saldo dispon°vel ap¢s essa reserva." NO-UNDO.

DEFINE VARIABLE fi-volume-fin AS CHARACTER FORMAT "X(256)":U 
     LABEL "Volume Final" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-volume-ini AS CHARACTER FORMAT "X(256)":U 
     LABEL "Volume Inicial" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-26
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 22 BY 3.25.

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 22 BY 2.96.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88 BY 5.25.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88 BY 7.29.

DEFINE VARIABLE to-faturado AS LOGICAL INITIAL no 
     LABEL "Faturado" 
     VIEW-AS TOGGLE-BOX
     SIZE 10.14 BY .88 TOOLTIP "Indica se a Reserva j† foi faturada." NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     ped-item.nome-abrev AT ROW 1.17 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16.14 BY .88
     fi-nome-emit AT ROW 1.17 COL 32.29 COLON-ALIGNED HELP
          "Nome Completo do Emitente" NO-LABEL
     ped-item.nr-pedcli AT ROW 2.17 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16.14 BY .88
     ped-venda.dt-implant AT ROW 2.17 COL 75.86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     ped-item.nr-sequencia AT ROW 3.17 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6 BY .88
     fi-desc-situacao AT ROW 3.17 COL 52 COLON-ALIGNED
     ped-item.dt-entrega AT ROW 3.17 COL 75.86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     ped-item.it-codigo AT ROW 4.17 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.14 BY .88
     fi-desc-item AT ROW 4.17 COL 33.29 COLON-ALIGNED NO-LABEL
     ped-item.cod-refer AT ROW 5.17 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY .88
     ped-item.qt-pedida AT ROW 5.25 COL 52 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.72 BY .88
     fi-dv AT ROW 6.67 COL 16 COLON-ALIGNED
     to-faturado AT ROW 6.67 COL 28.86
     bt-exclui-res AT ROW 6.71 COL 41.43
     fi-cod-lote AT ROW 7.67 COL 16 COLON-ALIGNED
     fi-cod-refer AT ROW 7.67 COL 20.57 COLON-ALIGNED NO-LABEL
     fi-qtd-ultent AT ROW 7.88 COL 72.72 COLON-ALIGNED
     fi-quantidade AT ROW 8.67 COL 16 COLON-ALIGNED
     fi-dat-ultent AT ROW 8.88 COL 72.72 COLON-ALIGNED
     fi-qtd-volum AT ROW 9.67 COL 16 COLON-ALIGNED
     fi-volume-ini AT ROW 10.67 COL 16 COLON-ALIGNED
     fi-qtd-dispon AT ROW 11.13 COL 72.72 COLON-ALIGNED
     fi-volume-fin AT ROW 11.67 COL 16 COLON-ALIGNED
     fi-sld-dispon AT ROW 12.13 COL 72.72 COLON-ALIGNED
     fi-sigla-emb AT ROW 12.67 COL 16 COLON-ALIGNED
     fi-desc-dentro AT ROW 12.67 COL 39 COLON-ALIGNED
     "Èltima Entrada" VIEW-AS TEXT
          SIZE 10 BY .54 AT ROW 6.83 COL 71
     RECT-26 AT ROW 7.13 COL 65
     RECT-27 AT ROW 10.54 COL 65
     rt-key AT ROW 1.04 COL 1
     rt-mold AT ROW 6.46 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: movdis.ped-item,movdis.ped-venda
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
         HEIGHT             = 12.75
         WIDTH              = 88.
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

/* SETTINGS FOR BUTTON bt-exclui-res IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ped-item.cod-refer IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ped-item.dt-entrega IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ped-venda.dt-implant IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-cod-lote IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-cod-refer IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-dat-ultent IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-dentro IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-desc-item IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-situacao IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-dv IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-nome-emit IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-dispon IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-ultent IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-volum IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-quantidade IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-sigla-emb IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-sld-dispon IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-volume-fin IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-volume-ini IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN ped-item.it-codigo IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ped-item.nome-abrev IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ped-item.nr-pedcli IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ped-item.nr-sequencia IN FRAME f-main
   5                                                                    */
/* SETTINGS FOR FILL-IN ped-item.qt-pedida IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX to-faturado IN FRAME f-main
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

&Scoped-define SELF-NAME bt-exclui-res
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-exclui-res V-table-Win
ON CHOOSE OF bt-exclui-res IN FRAME f-main /* Excluir Reserva */
DO:
  RUN pi-exclui-res.
  RUN local-display-fields.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-lote
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-lote V-table-Win
ON LEAVE OF fi-cod-lote IN FRAME f-main /* Lote */
DO:
   IF LOOKUP(INPUT FRAME {&frame-name} fi-cod-lote,"PP,PD,RP,RD") = 0 THEN DO.
      MESSAGE "Lote deve ser PP,PD,RP,RD..." VIEW-AS ALERT-BOX.
      APPLY 'entry' TO SELF.
      RETURN NO-APPLY.
   END.
   ASSIGN fi-cod-refer:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ped-item.cod-refer.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-dv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-dv V-table-Win
ON LEAVE OF fi-dv IN FRAME f-main /* DV */
DO:
  FIND ref-item-ext WHERE
       ref-item-ext.it-codigo = ped-item.it-codigo AND
       ref-item-ext.cod-refer = ped-item.cod-refer
       NO-LOCK NO-ERROR.

  ASSIGN l-dv-ok = YES.
  IF AVAIL ref-item-ext THEN DO:
     ASSIGN i-dv-dig-calc = ref-item-ext.dv.

     IF i-dv-dig-calc <> INPUT FRAME {&FRAME-NAME} fi-dv THEN DO:
        MESSAGE "D°gito Verificador n∆o confere." VIEW-AS ALERT-BOX.
        ASSIGN l-dv-ok = NO.
        APPLY "entry" TO SELF.
        RETURN NO-APPLY.
     END.
  END.

  /* --- Busca ult.reserva e calcula saldo ---*/             
  /*
  find ped-item-ext where
       ped-item-ext.nome-abrev   = ped-item.nome-abrev and
       ped-item-ext.nr-pedcli    = ped-item.nr-pedcli and
       ped-item-ext.nr-sequencia = ped-item.nr-sequencia and
       ped-item-ext.it-codigo    = ped-item.it-codigo and
       ped-item-ext.cod-refer    = ped-item.cod-refer no-error.

  if avail ped-item-ext then
     assign c-lote = substr(ped-item-ext.acondicionamento,1,1).
  else
     assign c-lote = "".
  */
    
  find last movto-estoq where
            movto-estoq.esp-docto  = 1 /*aca*/ and
            movto-estoq.serie      = fi-serie-entr and
            movto-estoq.lote       begins c-lote and
            movto-estoq.tipo-trans = 1   and
            movto-estoq.it-codigo  = ped-item.it-codigo and
            movto-estoq.cod-refer  = ped-item.cod-refer
            no-lock no-error.

   if avail movto-estoq then                                 
      assign fi-qtd-ultent:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(movto-estoq.quantidade)         
             fi-dat-ultent:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(movto-estoq.dt-trans).          
   else                                                      
      assign fi-qtd-ultent:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""                              
             fi-dat-ultent:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

   assign de-acm-res = 0.
   FOR each b-ped-item-res WHERE b-ped-item-res.it-codigo =  ped-item.it-codigo
                             and b-ped-item-res.cod-refer =  ped-item.cod-refer
                             and b-ped-item-res.dt-trans  >= TODAY - int(fi-dias-res)
                             and b-ped-item-res.dt-trans  <= TODAY
                           no-lock:
       assign de-acm-res = de-acm-res + b-ped-item-res.qt-pedida.
   END.
   ASSIGN de-acm-res = de-acm-res - dec(fi-quantidade:SCREEN-VALUE IN FRAME {&FRAME-NAME}).

   ASSIGN fi-qtd-dispon:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(DEC(fi-qtd-ultent:SCREEN-VALUE IN FRAME {&FRAME-NAME}) - de-acm-res)
          fi-sld-dispon:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(DEC(fi-qtd-ultent:SCREEN-VALUE IN FRAME {&FRAME-NAME}) - de-acm-res).
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-qtd-volum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-qtd-volum V-table-Win
ON LEAVE OF fi-qtd-volum IN FRAME f-main /* Quant de Volumes */
DO:
  IF INT(INPUT FRAME {&frame-name} fi-qtd-volum) = 0 THEN DO:
     MESSAGE "Quantidade de volumes deve ser maior que zero." VIEW-AS ALERT-BOX.
     APPLY 'entry' TO fi-qtd-volum IN FRAME {&FRAME-NAME}.
     return NO-APPLY.                                                                    
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-quantidade
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-quantidade V-table-Win
ON LEAVE OF fi-quantidade IN FRAME f-main /* Quantidade */
DO:
  IF dec(INPUT FRAME {&frame-name} fi-quantidade) = 0 THEN DO:
     MESSAGE "Quantidade deve ser maior que zero." VIEW-AS ALERT-BOX.
     APPLY 'entry' TO fi-quantidade IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END.
  ASSIGN fi-sld-dispon:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(DEC(fi-qtd-ultent:SCREEN-VALUE IN FRAME {&FRAME-NAME}) - de-acm-res -
                                 INPUT FRAME {&FRAME-NAME} fi-quantidade).

  IF ((DEC(INPUT FRAME {&FRAME-NAME} fi-quantidade) / ped-item.qt-pedida) * 100) - 100 > fi-perc-var-res THEN DO.
     MESSAGE "A variaá∆o entre Quantidade Pedida e Reservada Ç superior a 10%."
              VIEW-AS ALERT-BOX.

     MESSAGE "Tem Certeza que deseja Reservar ?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
             UPDATE l-reserva AS LOG.

     IF NOT l-reserva THEN DO.
        APPLY 'entry' TO fi-quantidade IN FRAME {&FRAME-NAME}.
        RETURN NO-APPLY.
     END.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-sigla-emb
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-sigla-emb V-table-Win
ON LEAVE OF fi-sigla-emb IN FRAME f-main /* Sigla Embalagem */
DO:
  FIND FIRST embalag WHERE 
             embalag.sigla-emb = INPUT FRAME {&FRAME-NAME} fi-sigla-emb NO-LOCK NO-ERROR.
  IF NOT AVAIL embalag THEN DO:
     MESSAGE "Embalagem inv†lida." VIEW-AS ALERT-BOX.
     APPLY 'entry' TO fi-sigla-emb IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END.
  ELSE DO.
     IF INPUT FRAME {&FRAME-NAME} fi-sigla-emb = "cx" THEN
        ASSIGN fi-desc-dentro:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Pecas".
     ELSE
        ASSIGN fi-desc-dentro:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Rolos".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-sigla-emb V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-sigla-emb IN FRAME f-main /* Sigla Embalagem */
DO:
  {include/zoomvar.i &prog-zoom = dizoom/z01di040.w
                     &campo     = fi-sigla-emb
                     &campozoom = sigla-emb}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-volume-fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-volume-fin V-table-Win
ON LEAVE OF fi-volume-fin IN FRAME f-main /* Volume Final */
DO:
  IF INT(INPUT FRAME {&frame-name} fi-volume-fin) = 0 THEN DO:
     MESSAGE "Volume final deve ser maior que zero." VIEW-AS ALERT-BOX.
     APPLY 'entry' TO fi-volume-fin IN FRAME {&FRAME-NAME}.
     return NO-APPLY.                                                                    
  END.
  IF int(INPUT FRAME {&FRAME-NAME} fi-volume-fin) - int(INPUT FRAME {&FRAME-NAME} fi-volume-ini) + 1 <>
     int(INPUT FRAME {&FRAME-NAME} fi-qtd-volum) THEN DO:
     MESSAGE "Volume Inicial e/ou Final incompat°vel com Quantidade de Volumes." VIEW-AS ALERT-BOX.
     ASSIGN fi-volume-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
            fi-volume-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
     APPLY 'entry' TO fi-qtd-volum.
     RETURN NO-APPLY.
  END.

  /*
  assign l-achou = no.
  for each b-ped-item-res where b-ped-item-res.nr-pedcli    =  ped-item.nr-pedcli
                            and b-ped-item-res.nome-abrev   =  ped-item.nome-abrev
                            AND b-ped-item-res.nr-sequencia <> ped-item.nr-sequencia
                          no-lock.
     if b-ped-item-res.volume-ini = int(INPUT FRAME {&frame-name} fi-volume-ini)
     or b-ped-item-res.volume-fim = int(INPUT FRAME {&frame-name} fi-volume-fin) then
        assign l-achou = yes
               i-volume-ini-aux = b-ped-item-res.volume-ini
               i-volume-fim-aux = b-ped-item-res.volume-fim.
  end.
  if l-achou = yes then do:
     message "J† existe o Volume: " i-volume-ini-aux " a " i-volume-fim-aux VIEW-AS ALERT-BOX.
     ASSIGN fi-volume-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
            fi-volume-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
     APPLY 'entry' TO fi-qtd-volum.
     RETURN NO-APPLY.
  end.
  */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-volume-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-volume-ini V-table-Win
ON LEAVE OF fi-volume-ini IN FRAME f-main /* Volume Inicial */
DO:
  IF INT(INPUT FRAME {&frame-name} fi-volume-ini) = 0 THEN DO:
     MESSAGE "Volume inicial deve ser maior que zero." VIEW-AS ALERT-BOX.
     APPLY 'entry' TO fi-volume-ini IN FRAME {&FRAME-NAME}.
     return NO-APPLY.                                                                    
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  

  /************************ INTERNAL PROCEDURES ********************/

ASSIGN fi-dias-res     = 5
       fi-serie-entr   = "leo"
       fi-perc-var-res = 10.

fi-sigla-emb:LOAD-MOUSE-POINTER("image\lupa.cur") IN FRAME {&FRAME-NAME}.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  {src/adm/template/row-list.i "ped-item"}
  {src/adm/template/row-list.i "ped-venda"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "ped-item"}
  {src/adm/template/row-find.i "ped-venda"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    {include/i-valid.i}
    
    /* Ponha na pi-validate todas as validaá‰es */
    /* N∆o gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */
    
  /* Dispatch standard ADM method.                             */
  /* --- Comentado porque viewer s¢ tem campos fill-in habilitados ---
  * RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
  * if RETURN-VALUE = 'ADM-ERROR':U then 
  *    return 'ADM-ERROR':U. 
  -----------------------------------Gilvando Nov/2003---------------*/
  RUN pi-validate.

  IF RETURN-VALUE = 'ADM-ERROR':U THEN 
     RETURN 'ADM-ERROR':U.

  IF NOT AVAIL ped-item-res THEN DO:
     CREATE ped-item-res.
     ASSIGN ped-item-res.nome-abrev   = ped-item.nome-abrev
            ped-item-res.nr-pedcli    = ped-item.nr-pedcli
            ped-item-res.nr-sequencia = ped-item.nr-sequencia
            ped-item-res.it-codigo    = ped-item.it-codigo
            ped-item-res.cod-refer    = ped-item.cod-refer.
  END.
  ASSIGN ped-item-res.lote            = INPUT FRAME {&FRAME-NAME} fi-cod-lote +
                                        INPUT FRAME {&FRAME-NAME} fi-cod-refer
         ped-item-res.qt-pedida       = dec(INPUT FRAME {&FRAME-NAME} fi-quantidade)     
         ped-item-res.dt-trans        = today                                            
         ped-item-res.hr-trans        = string(time,"hh:mm:ss")                          
         ped-item-res.sigla-emb       = INPUT FRAME {&frame-name} fi-sigla-emb           
         ped-item-res.desc-dentro-emb = INPUT FRAME {&frame-name} fi-desc-dentro         
         ped-item-res.volume-ini      = int(INPUT FRAME {&frame-name} fi-volume-ini)     
         ped-item-res.volume-fim      = int(INPUT FRAME {&frame-name} fi-volume-fin).    

  CASE fi-ajuste-res.
      WHEN 1 THEN RUN pi-manut-peditem (INPUT "A"). /* Ajusta Item Original */
      WHEN 2 THEN DO.
           RUN pi-manut-peditem (INPUT "C"). /* Cria novo Item */
           RUN pi-manut-peditem (INPUT "A"). /* Ajusta Item Original */
      END.
  END CASE.

  RUN pi-habilita IN h-w-cadsi2.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-cancel-record V-table-Win 
PROCEDURE local-cancel-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN pi-habilita IN h-w-cadsi2. 

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

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
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable-fields':U ).
    
    /* Code placed here will execute AFTER standard behavior.    */
    &if defined(ADM-MODIFY-FIELDS) &then
        disable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.
    &endif
    
    &if defined(list-4) &then
        DISABLE {&list-4} WITH FRAME {&FRAME-NAME}.
    &endif
    
    ASSIGN bt-exclui-res:SENSITIVE IN FRAME {&FRAME-NAME}= NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    
    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ).
    
    /* Code placed here will execute AFTER standard behavior.    */
    IF AVAIL ped-item THEN DO:
       {esinc/i-dsrb.i ped-item.cod-sit-item ped-item.cod-sit-item c-situacao}
       ASSIGN fi-desc-situacao:SCREEN-VALUE IN FRAME {&FRAME-NAME} = c-situacao.
    END.

    IF AVAIL ped-item THEN
       ASSIGN v-nome-abrev = ped-item.nome-abrev
              v-nr-pedcli  = ped-item.nr-pedcli
              v-nr-sequencia = ped-item.nr-sequencia.

    FIND emitente WHERE emitente.nome-abrev = ped-item.nome-abrev NO-LOCK NO-ERROR.
    IF AVAIL emitente THEN
       ASSIGN fi-nome-emit:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.nome-emit.
    
    FIND ITEM WHERE ITEM.it-codigo = ped-item.it-codigo NO-LOCK NO-ERROR.
    IF AVAIL ITEM THEN
       ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ITEM.desc-item.
    
    FIND ped-item-ext WHERE
         ped-item-ext.nome-abrev   = ped-item.nome-abrev   AND
         ped-item-ext.nr-pedcli    = ped-item.nr-pedcli    AND
         ped-item-ext.nr-sequencia = ped-item.nr-sequencia AND
         ped-item-ext.it-codigo    = ped-item.it-codigo    AND
         ped-item-ext.cod-refer    = ped-item.cod-refer    NO-ERROR.

    IF AVAIL ped-item-ext THEN
       ASSIGN fi-cod-lote:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = SUBSTR(ped-item-ext.lote,1,2)
              fi-cod-refer:SCREEN-VALUE IN FRAME {&FRAME-NAME} = SUBSTR(ped-item-ext.lote,3,7).

    FIND ped-item-res WHERE
         ped-item-res.nome-abrev   = ped-item.nome-abrev   AND
         ped-item-res.nr-pedcli    = ped-item.nr-pedcli    AND
         ped-item-res.nr-sequencia = ped-item.nr-sequencia AND
         ped-item-res.it-codigo    = ped-item.it-codigo    AND
         ped-item-res.cod-refer    = ped-item.cod-refer    NO-ERROR.

    IF AVAIL ped-item-res THEN DO:
       ASSIGN to-faturado:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = STRING(ped-item-res.faturado)
              fi-quantidade:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = STRING(ped-item-res.qt-pedida)
              fi-qtd-volum:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = STRING(ped-item-res.volume-fim -
                                                                          ped-item-res.volume-ini + 1)
              fi-volume-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = STRING(ped-item-res.volume-ini)
              fi-volume-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = STRING(ped-item-res.volume-fim)
              fi-sigla-emb:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = ped-item-res.sigla-emb
              fi-desc-dentro:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ped-item-res.desc-dentro.

       IF ped-item-res.faturado = NO THEN
          ASSIGN bt-exclui-res:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
    END.
    ELSE
       ASSIGN fi-quantidade:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
              fi-qtd-volum:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
              fi-volume-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""                              
              fi-volume-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
              fi-sigla-emb:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
              fi-desc-dentro:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
              bt-exclui-res:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

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
    
    /*
    IF NOT AVAIL ped-item-res THEN DO.
       FIND LAST b-ped-item-res WHERE
                 b-ped-item-res.nome-abrev = ped-item.nome-abrev AND
                 b-ped-item-res.nr-pedcli  = ped-item.nr-pedcli 
                 USE-INDEX indice4 NO-LOCK NO-ERROR.

       IF AVAIL b-ped-item-res THEN
          ASSIGN fi-quantidade:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = STRING(b-ped-item-res.qt-pedida)      
                 fi-qtd-volum:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = STRING(b-ped-item-res.volume-fim - b-ped-item-res.volume-ini + 1)
                 fi-sigla-emb:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = b-ped-item-res.sigla-emb          
                 fi-desc-dentro:SCREEN-VALUE IN FRAME {&FRAME-NAME} = b-ped-item-res.desc-dentro-emb    
                 fi-volume-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = STRING(b-ped-item-res.volume-ini)     
                 fi-volume-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = STRING(b-ped-item-res.volume-fim).
    END.
    */
    &if defined(list-5) &then
        DISABLE {&list-5} WITH FRAME {&FRAME-NAME}.
    &endif
    
    &if defined(list-4) &then
       ENABLE {&list-4} WITH FRAME {&FRAME-NAME}.
    &endif
     
    FIND ref-item-ext WHERE
         ref-item-ext.it-codigo = ped-item.it-codigo AND
         ref-item-ext.cod-refer = ped-item.cod-refer
         NO-LOCK NO-ERROR.

    IF NOT AVAIL ref-item-ext THEN DO.
       ASSIGN fi-dv:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
       APPLY 'entry' TO fi-quantidade IN FRAME {&FRAME-NAME}.
       ASSIGN l-dv-ok = YES.
    END.
    ELSE
       APPLY "entry" TO fi-dv IN FRAME {&FRAME-NAME}.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-exclui-res V-table-Win 
PROCEDURE pi-exclui-res :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    MESSAGE "Vocà quer realmente excluir a Reserva ?" 
            VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO UPDATE l-conf-exc.

    IF l-conf-exc = YES THEN DO:
       IF AVAIL ped-item-res THEN
          DELETE ped-item-res.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-manut-peditem V-table-Win 
PROCEDURE pi-manut-peditem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-tipo AS CHAR.

    FOR EACH tt-ped-item.
        DELETE tt-ped-item.
    END.

    FIND ped-venda OF ped-item USE-INDEX ch-pedido NO-LOCK NO-ERROR.

    ASSIGN l-completo = ped-venda.completo
           i-sit-aval = ped-venda.cod-sit-aval
           i-cod-mess = ped-venda.cod-message-alert
           da-dt-mess =  ped-venda.dt-mensagem
           c-desc-for = ped-venda.desc-forc-cr
            l-dsp-fat =  ped-venda.dsp-pre-fat. 

    IF NOT VALID-HANDLE(h-bodi154) or
       h-bodi154:TYPE      <> "PROCEDURE":U OR
       h-bodi154:FILE-NAME <> "dibo/bodi154.p":U THEN
       RUN dibo/bodi154.p PERSISTENT SET h-bodi154.

    IF NOT VALID-HANDLE(h-bodi154com) OR
       h-bodi154com:TYPE      <> "PROCEDURE":U OR
       h-bodi154com:FILE-NAME <> "dibo/bodi154com.p":U THEN
       RUN dibo/bodi159com.p PERSISTENT SET h-bodi154com.

    IF p-tipo = "C" THEN DO.
       BUFFER-COPY ped-item TO tt-ped-item 
              ASSIGN tt-ped-item.nr-sequencia = ped-item.nr-sequencia + 1
                     tt-ped-item.qt-pedida = ped-item.qt-pedida - ped-item-res.qt-pedida
                     tt-ped-item.vl-liq-abe = tt-ped-item.qt-pedida * tt-ped-item.vl-preori.
        
       RUN openQueryStatic IN h-bodi154(INPUT "Main":U).
    END.
    ELSE DO.
       BUFFER-COPY ped-item TO tt-ped-item 
              ASSIGN tt-ped-item.qt-pedida = ped-item-res.qt-pedida
                     tt-ped-item.vl-liq-abe = tt-ped-item.qt-pedida * tt-ped-item.vl-preori. 

       RUN setConstraintKey IN h-bodi154 (INPUT tt-ped-item.nome-abrev,
                                          INPUT tt-ped-item.nr-pedcli,
                                          INPUT tt-ped-item.nr-sequencia,
                                          INPUT tt-ped-item.it-codigo,
                                          INPUT tt-ped-item.cod-refer).

       RUN openQueryStatic in h-bodi154 (input "Key":U).        
    END.
    
    RUN emptyRowErrors IN h-bodi154.
    RUN setRecord IN h-bodi154(INPUT TABLE tt-ped-item).

    IF p-tipo = "C" THEN 
       RUN createRecord IN h-bodi154.
    ELSE
       RUN updateRecord IN h-bodi154.
    
    RUN getRowErrors IN h-bodi154(OUTPUT TABLE RowErrors).
    
    IF CAN-FIND(FIRST RowErrors 
                WHERE RowErrors.ErrorSubType = "ERROR":U  ) THEN DO:
       FOR EACH rowerrors WHERE RowErrors.ErrorSubType = "ERROR":U:
           MESSAGE rowerrors.errornumber
                   rowerrors.errordescription VIEW-AS ALERT-BOX.
       END.
    END.
    ELSE DO.
        IF p-tipo = "C" THEN DO.
           FIND b-ped-item-ext OF ped-item.
           CREATE ped-item-ext.
           BUFFER-COPY b-ped-item-ext TO ped-item-ext
                       ASSIGN ped-item-ext.nr-sequencia = b-ped-item-ext.nr-sequencia + 1.
        END.

        /* Completa o Pedido */
        IF l-completo THEN DO.
           RUN emptyRowErrors IN h-bodi154.
           RUN completeOrder IN h-bodi154com (INPUT ROWID(ped-venda),
                                              OUTPUT TABLE Rowerrors).
           FOR EACH Rowerrors:
               MESSAGE rowerrors.errornumber
                       rowerrors.errordescription VIEW-AS ALERT-BOX.
           END.
        END.
    END.
    
    FIND ped-venda OF ped-item USE-INDEX ch-pedido NO-ERROR.
    ASSIGN ped-venda.cod-sit-aval = i-sit-aval
           ped-venda.cod-message-alert = i-cod-mess 
           ped-venda.dt-mensagem = da-dt-mess 
           ped-venda.desc-forc-cr = c-desc-for 
           ped-venda.dsp-pre-fat = l-dsp-fat.

    IF VALID-HANDLE(h-bodi154) THEN
       DELETE PROCEDURE h-bodi154.

    IF VALID-HANDLE(h-bodi154com) THEN
       DELETE PROCEDURE h-bodi154com.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pi-validate V-table-Win 
PROCEDURE Pi-validate :
/*------------------------------------------------------------------------------
  Purpose:Validar a viewer     
  Parameters:  <none>
  Notes: N∆o fazer assign aqui. Nesta procedure
  devem ser colocadas apenas validaá‰es, pois neste ponto do programa o registro 
  ainda n∆o foi criado.       
------------------------------------------------------------------------------*/
    {include/i-vldfrm.i} /* Validaá∆o de dicion†rio */

    /*--- Validaá∆o de DV do ÷tem ---*/
    IF NOT l-dv-ok THEN DO:
        MESSAGE "D°gito Verificador n∆o confere." VIEW-AS ALERT-BOX.
        APPLY 'entry' TO fi-dv IN FRAME {&FRAME-NAME}.                                                          
        return 'ADM-ERROR':U.                                                                    
    END.

    /*--- Validaá∆o de Quantidade da Reserva ---*/
    IF dec(INPUT FRAME {&frame-name} fi-quantidade) = 0 THEN DO:
       MESSAGE "Quantidade deve ser maior que zero." VIEW-AS ALERT-BOX.
       APPLY 'entry' TO fi-quantidade IN FRAME {&FRAME-NAME}.
       return 'ADM-ERROR':U.                                                                    
    END.
    
    IF dec(INPUT FRAME {&frame-name} fi-quantidade) <> ped-item.qt-pedida THEN DO:
       ASSIGN fi-ajuste-res = 3. 
       RUN esdlg/d01es040.w.
       IF fi-ajuste-res > 2 THEN DO:
          APPLY 'entry' TO fi-quantidade IN FRAME {&FRAME-NAME}.
          return 'ADM-ERROR':U.
       END.
    END.

    /*--- Validaá∆o de Volumes ---*/
    IF INT(INPUT FRAME {&frame-name} fi-qtd-volum) = 0 THEN DO:
       MESSAGE "Quantidade de volumes deve ser maior que zero." VIEW-AS ALERT-BOX.
       APPLY 'entry' TO fi-qtd-volum IN FRAME {&FRAME-NAME}.
       return 'ADM-ERROR':U.                                                                    
    END.
    IF INT(INPUT FRAME {&frame-name} fi-volume-ini) = 0 THEN DO:
       MESSAGE "Volume inicial deve ser maior que zero." VIEW-AS ALERT-BOX.
       APPLY 'entry' TO fi-volume-ini IN FRAME {&FRAME-NAME}.
       return 'ADM-ERROR':U.                                                                    
    END.
    IF INT(INPUT FRAME {&frame-name} fi-volume-fin) = 0 THEN DO:
       MESSAGE "Volume final deve ser maior que zero." VIEW-AS ALERT-BOX.
       APPLY 'entry' TO fi-volume-fin IN FRAME {&FRAME-NAME}.
       return 'ADM-ERROR':U.                                                                    
    END.
    IF int(INPUT FRAME {&FRAME-NAME} fi-volume-fin) - int(INPUT FRAME {&FRAME-NAME} fi-volume-ini) + 1 <>
       int(INPUT FRAME {&FRAME-NAME} fi-qtd-volum) THEN DO:
       MESSAGE "Volume Inicial e/ou Final incompat°vel com Quantidade de Volumes." VIEW-AS ALERT-BOX.
       ASSIGN fi-volume-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
              fi-volume-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
       APPLY 'entry' TO fi-qtd-volum IN FRAME {&FRAME-NAME}.
       return 'ADM-ERROR':U.                                                                    
    END.

    ASSIGN l-seq-vol-ok = YES.
    FOR EACH b-ped-item-res WHERE b-ped-item-res.nome-abrev = ped-item.nome-abrev
                              AND b-ped-item-res.nr-pedcli  = ped-item.nr-pedcli
                              AND b-ped-item-res.it-codigo + SUBSTR(b-ped-item-res.cod-refer,1,6) <>
                                  ped-item.it-codigo + SUBSTR(ped-item.cod-refer,1,6)
                            NO-LOCK:
        IF (INT(INPUT FRAME {&FRAME-NAME} fi-volume-ini) < b-ped-item-res.volume-ini OR
            INT(INPUT FRAME {&FRAME-NAME} fi-volume-ini) > b-ped-item-res.volume-fim) AND
           (INT(INPUT FRAME {&FRAME-NAME} fi-volume-fin) < b-ped-item-res.volume-ini OR  
            INT(INPUT FRAME {&FRAME-NAME} fi-volume-fin) > b-ped-item-res.volume-fim) THEN
           ASSIGN l-seq-vol-ok = YES.
        ELSE DO:
           ASSIGN l-seq-vol-ok = NO.
           LEAVE.
        END.
    END.
    IF l-seq-vol-ok = NO THEN DO:
       MESSAGE "Volume j† foi utilizado em outra seqÅància." skip
               "Ped: "   b-ped-item-res.nr-pedcli
               " Seq: "  b-ped-item-res.nr-sequencia
               " Item: " b-ped-item-res.it-codigo
               " Ref: "  b-ped-item-res.cod-refer SKIP
               "Vol.ini: "  b-ped-item-res.volume-ini
               " Vol.fin: " b-ped-item-res.volume-fim
               VIEW-AS ALERT-BOX.
       ASSIGN fi-volume-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
              fi-volume-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
       APPLY 'entry' TO fi-qtd-volum IN FRAME {&FRAME-NAME}.
       return 'ADM-ERROR':U.                                                                    
    END.

    /*--- Validaá∆o de Embalagens ---*/
    find first embalag where embalag.sigla-emb = INPUT FRAME {&FRAME-NAME} fi-sigla-emb
                       no-lock no-error.
    if not avail embalag then do:
       message "Embalagem inv†lida." VIEW-AS ALERT-BOX.
       APPLY 'entry' TO fi-sigla-emb IN FRAME {&FRAME-NAME}.
       return 'ADM-ERROR':U.                                                                    
    END.

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
  {src/adm/template/snd-list.i "ped-item"}
  {src/adm/template/snd-list.i "ped-venda"}

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

  IF p-state = "update-begin" THEN DO:
     IF ped-item.cod-sit-item = 3  OR
        ped-item.cod-sit-item >= 6 THEN DO:
        MESSAGE "÷tem de Pedido n∆o pode ser reservado, porque n∆o est† aberto." VIEW-AS ALERT-BOX.
        RUN pi-cancelar IN h-p-cadsi2.
        RETURN NO-APPLY.
     END.
     IF AVAIL ped-item-res THEN DO:
        MESSAGE "Reserva n∆o pode ser alterada..." VIEW-AS ALERT-BOX.
        RUN pi-cancelar IN h-p-cadsi2.
        RETURN NO-APPLY.
     END.
     RUN pi-desabilita IN h-w-cadsi2.
  END.

  CASE p-state:
      {src/adm/template/vstates.i} 
  END CASE.

  run pi-trata-state (p-issuer-hdl, p-state).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

