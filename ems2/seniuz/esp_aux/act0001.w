&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          espec            PROGRESS
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
{include/i-prgvrs.i ACT001 2.04.00.000}
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

/* Local Variable Definitions ---                                       */
DEF VAR i-tp-embal AS INT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Digitacao
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS ob-etiqueta.num-etiqueta ~
ob-etiqueta.quantidade ob-etiqueta.dt-emissao 
&Scoped-define ENABLED-TABLES ob-etiqueta
&Scoped-define FIRST-ENABLED-TABLE ob-etiqueta
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 fi-cod-estabel bt-etiqueta ~
bt-cancelar bt-ajuda 
&Scoped-Define DISPLAYED-FIELDS ob-etiqueta.num-etiqueta ~
ob-etiqueta.nr-sequencia ob-etiqueta.nr-ob ob-etiqueta.localizacao ~
ob-etiqueta.situacao ob-etiqueta.quantidade ob-etiqueta.dt-emissao ~
ob-etiqueta.it-codigo ob-etiqueta.nr-lote ob-etiqueta.cod-refer 
&Scoped-define DISPLAYED-TABLES ob-etiqueta
&Scoped-define FIRST-DISPLAYED-TABLE ob-etiqueta
&Scoped-Define DISPLAYED-OBJECTS fi-cod-estabel fi-localizacao ed-motivo ~
rs-situacao fi-romaneio fi-corte-comerc fi-corte-sugerido fi-hora ~
fi-reserva fi-quantidade fi-peso-bruto fi-data-invent 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-digita AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-ant 
     IMAGE-UP FILE "image/im-ante.bmp":U
     LABEL "Button 2" 
     SIZE 4 BY 1 TOOLTIP "Consulta Etiqueta Anterior".

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Sair" 
     SIZE 10 BY 1 TOOLTIP "Sair do Programa.".

DEFINE BUTTON bt-desfaz AUTO-END-KEY 
     LABEL "Desfazer" 
     SIZE 10 BY 1 TOOLTIP "Desfaz a alteraá∆o.".

DEFINE BUTTON bt-etiqueta 
     IMAGE-UP FILE "image/im-vapra.bmp":U
     LABEL "Button 1" 
     SIZE 4 BY .88.

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "OK" 
     SIZE 10 BY 1 TOOLTIP "Confirma a alteraá∆o.".

DEFINE BUTTON bt-prox 
     IMAGE-UP FILE "image/im-nex.bmp":U
     LABEL "Button 3" 
     SIZE 4 BY 1 TOOLTIP "Consulta Proxima Etiqueta".

DEFINE VARIABLE ed-motivo AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 200 SCROLLBAR-VERTICAL
     SIZE 28 BY 5.25 NO-UNDO.

DEFINE VARIABLE fi-cod-estabel AS CHARACTER FORMAT "X(3)":U INITIAL "1" 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 NO-UNDO.

DEFINE VARIABLE fi-corte-comerc AS CHARACTER FORMAT "!" 
     LABEL "Corte-Comercial" 
     VIEW-AS FILL-IN 
     SIZE 2.57 BY .83.

DEFINE VARIABLE fi-corte-sugerido AS CHARACTER FORMAT "!":U 
     VIEW-AS FILL-IN 
     SIZE 2.57 BY .83 NO-UNDO.

DEFINE VARIABLE fi-data-invent AS DATE FORMAT "99/99/9999" 
     LABEL "Data Invent." 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-hora AS CHARACTER FORMAT "99:99":U 
     LABEL "Hora" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-localizacao AS CHARACTER FORMAT "X(6)":U 
     VIEW-AS FILL-IN 
     SIZE 6.57 BY .79 TOOLTIP "Nova localizaá∆o." NO-UNDO.

DEFINE VARIABLE fi-peso-bruto AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "Peso Bruto (KG)" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-quantidade AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "Quantidade (M)" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-reserva AS LOGICAL FORMAT "Sim/N∆o":U INITIAL NO 
     LABEL "Reserva" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .79 NO-UNDO.

DEFINE VARIABLE fi-romaneio AS LOGICAL FORMAT "Sim/N∆o":U INITIAL NO 
     LABEL "Romaneio" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .79 NO-UNDO.

DEFINE VARIABLE rs-situacao AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Impressa", 1,
"Em Produá∆o", 2,
"Em Estoque", 3,
"Reservada", 4,
"Faturada", 5,
"Em Reprocesso", 6
     SIZE 14 BY 4.25 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 80 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 20 BY 5.25.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi-cod-estabel AT ROW 1.29 COL 12 COLON-ALIGNED WIDGET-ID 6
     bt-ant AT ROW 2.13 COL 30
     bt-prox AT ROW 2.13 COL 34
     ob-etiqueta.num-etiqueta AT ROW 2.29 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY .79
     bt-etiqueta AT ROW 2.29 COL 23.43
     ob-etiqueta.nr-sequencia AT ROW 3.29 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5 BY .79
     ob-etiqueta.nr-ob AT ROW 3.29 COL 30 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7 BY .79
     ob-etiqueta.localizacao AT ROW 4.29 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.57 BY .79
     fi-localizacao AT ROW 4.29 COL 19.57 COLON-ALIGNED NO-LABEL
     ed-motivo AT ROW 4.75 COL 53 NO-LABEL WIDGET-ID 10
     ob-etiqueta.situacao AT ROW 5.29 COL 14 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Impressa", 1,
"Em Produá∆o", 2,
"Em Estoque", 3,
"Reservada", 4,
"Faturada", 5,
"Em Reprocesso", 6
          SIZE 14.29 BY 4.25
     rs-situacao AT ROW 5.29 COL 35 NO-LABEL
     ob-etiqueta.quantidade AT ROW 9.71 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8.29 BY .79
     ob-etiqueta.dt-emissao AT ROW 10.71 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY .79
     fi-romaneio AT ROW 10.71 COL 48.29 COLON-ALIGNED
     fi-corte-comerc AT ROW 10.71 COL 67.57 COLON-ALIGNED
     fi-corte-sugerido AT ROW 10.71 COL 71 COLON-ALIGNED NO-LABEL
     fi-hora AT ROW 10.75 COL 30.86 COLON-ALIGNED
     ob-etiqueta.it-codigo AT ROW 11.71 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .79
     fi-reserva AT ROW 11.71 COL 48.14 COLON-ALIGNED
     fi-quantidade AT ROW 11.71 COL 67.72 COLON-ALIGNED
     ob-etiqueta.nr-lote AT ROW 11.75 COL 31 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 6.86 BY .79
     ob-etiqueta.cod-refer AT ROW 12.71 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.86 BY .79
     fi-peso-bruto AT ROW 12.71 COL 67.72 COLON-ALIGNED
     fi-data-invent AT ROW 12.75 COL 31 COLON-ALIGNED
     bt-ok AT ROW 13.96 COL 1.72
     bt-desfaz AT ROW 13.96 COL 12
     bt-cancelar AT ROW 13.96 COL 25.14
     bt-ajuda AT ROW 13.96 COL 70.57
     "Situaá∆o:" VIEW-AS TEXT
          SIZE 6.29 BY .54 AT ROW 5.25 COL 7.14
     " Motivo Alteraá∆o" VIEW-AS TEXT
          SIZE 28 BY .79 AT ROW 3.88 COL 53 WIDGET-ID 14
          BGCOLOR 1 FGCOLOR 15 FONT 6
     "Nova Situaá∆o" VIEW-AS TEXT
          SIZE 10.14 BY .54 AT ROW 4.42 COL 37
     RECT-1 AT ROW 13.75 COL 1
     RECT-2 AT ROW 4.75 COL 32
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 14.29
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
         TITLE              = "Acerta Situaá∆o de Etiquetas - act0001"
         HEIGHT             = 14.33
         WIDTH              = 81
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
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON bt-ant IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-desfaz IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-ok IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-prox IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ob-etiqueta.cod-refer IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR ed-motivo IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-corte-comerc IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-corte-sugerido IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-data-invent IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-hora IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-localizacao IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-peso-bruto IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-quantidade IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-reserva IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-romaneio IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ob-etiqueta.it-codigo IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ob-etiqueta.localizacao IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ob-etiqueta.nr-lote IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ob-etiqueta.nr-ob IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ob-etiqueta.nr-sequencia IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET rs-situacao IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET ob-etiqueta.situacao IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-digita)
THEN w-digita:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON END-ERROR OF w-digita /* Acerta Situaá∆o de Etiquetas - act0001 */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON WINDOW-CLOSE OF w-digita /* Acerta Situaá∆o de Etiquetas - act0001 */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
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


&Scoped-define SELF-NAME bt-ant
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ant w-digita
ON CHOOSE OF bt-ant IN FRAME F-Main /* Button 2 */
DO:
  FIND PREV ob-etiqueta WHERE 
            ob-etiqueta.cod-estabel  = INPUT FRAME {&FRAME-NAME} fi-cod-estabel 
             NO-LOCK NO-ERROR.
  IF AVAIL ob-etiqueta THEN DO:
     ob-etiqueta.num-etiqueta:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ob-etiqueta.num-etiqueta).
     apply "choose":U to bt-etiqueta.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar w-digita
ON CHOOSE OF bt-cancelar IN FRAME F-Main /* Sair */
DO:
  apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-desfaz
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-desfaz w-digita
ON CHOOSE OF bt-desfaz IN FRAME F-Main /* Desfazer */
DO:
  apply "choose":U to bt-etiqueta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-etiqueta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-etiqueta w-digita
ON CHOOSE OF bt-etiqueta IN FRAME F-Main /* Button 1 */
DO:
  FIND ob-etiqueta WHERE 
       ob-etiqueta.cod-estabel  = INPUT FRAME {&FRAME-NAME} fi-cod-estabel AND
       ob-etiqueta.num-etiqueta = INPUT FRAME {&FRAME-NAME} ob-etiqueta.num-etiqueta 
       NO-LOCK NO-ERROR.

  IF NOT AVAIL ob-etiqueta THEN DO:
     MESSAGE "Etiqueta n∆o encontrada."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     APPLY 'entry' TO ob-etiqueta.num-etiqueta IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END. 
  ELSE DO:
     ASSIGN rs-situacao:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            fi-corte-comerc:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            fi-localizacao:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            fi-peso-bruto:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            ed-motivo:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

     ASSIGN ob-etiqueta.nr-ob:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ob-etiqueta.nr-ob)
            ob-etiqueta.nr-sequencia:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ob-etiqueta.nr-sequencia)
            ob-etiqueta.localizacao:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ob-etiqueta.localizacao
            ob-etiqueta.situacao:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ob-etiqueta.situacao)
            ob-etiqueta.quantidade:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ob-etiqueta.quantidade)
            ob-etiqueta.dt-emissao:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ob-etiqueta.dt-emissao)
            ob-etiqueta.it-codigo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ob-etiqueta.it-codigo
            fi-peso-bruto:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ob-etiqueta.peso-bruto)
            fi-quantidade:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ob-etiqueta.quantidade)
            fi-hora:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ob-etiqueta.hr-emissao
            ob-etiqueta.cod-refer:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ob-etiqueta.cod-refer
            ob-etiqueta.nr-lote:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ob-etiqueta.nr-lote.

     ASSIGN rs-situacao:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ob-etiqueta.situacao).


     ASSIGN fi-romaneio:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "N∆o"
            fi-reserva:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "N∆o"
            fi-corte-comerc:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ob-etiqueta.corte-comerc
            fi-localizacao:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ob-etiqueta.localizacao.

     bt-ant:SENSITIVE IN FRAME {&FRAME-NAME}  = YES.
     bt-prox:SENSITIVE IN FRAME {&FRAME-NAME} = YES.


     /*-- Acha o corte comercial compativel com o tamanho do rolo --*/
     IF ob-etiqueta.acondic BEGINS "R" THEN
        ASSIGN i-tp-embal = 1.
     ELSE
     IF ob-etiqueta.acondic BEGINS "P" THEN
        ASSIGN i-tp-embal = 2.
     ELSE
     IF ob-etiqueta.acondic BEGINS "C" THEN
        ASSIGN i-tp-embal = 4.
     ELSE
        ASSIGN i-tp-embal = 3.

     FIND corte-comerc WHERE
          corte-comerc.compr-min <= ob-etiqueta.quantidade AND
          corte-comerc.compr-max >= ob-etiqueta.quantidade AND
          corte-comerc.tp-embalag = i-tp-embal NO-LOCK NO-ERROR.

     IF AVAIL corte-comerc THEN
        ASSIGN fi-corte-sugerido:SCREEN-VALUE IN FRAME {&FRAME-NAME} = corte-comerc.codigo.
     ELSE
        ASSIGN fi-corte-sugerido:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "?".
     /*------------------------------------------------------------------------*/
         
     FIND ped-item-rom WHERE ped-item-rom.num-etiqueta = ob-etiqueta.num-etiqueta NO-LOCK NO-ERROR.
     ASSIGN fi-romaneio:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(AVAIL ped-item-rom).
     IF AVAIL ped-item-rom THEN DO:
        FIND ped-item-res WHERE ped-item-res.nome-abrev   = ped-item-rom.nome-abrev
                            AND ped-item-res.nr-pedcli    = ped-item-rom.nr-pedcli
                            AND ped-item-res.nr-sequencia = ped-item-rom.nr-sequencia
                          NO-LOCK NO-ERROR.
        ASSIGN fi-reserva:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(AVAIL ped-item-res).
     END.
/*
     FIND LAST inv-acab WHERE inv-acab.num-etiqueta = ob-etiqueta.num-etiqueta NO-LOCK NO-ERROR.
     IF AVAIL inv-acab THEN
        ASSIGN fi-data-invent:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(inv-acab.data-invent).
     ELSE
        ASSIGN fi-data-invent:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
*/
     ASSIGN bt-ok:SENSITIVE IN FRAME {&FRAME-NAME} = NO
            bt-desfaz:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-digita
ON CHOOSE OF bt-ok IN FRAME F-Main /* OK */
DO:
  IF INPUT FRAME {&FRAME-NAME} rs-situacao <> ob-etiqueta.situacao:SCREEN-VALUE IN FRAME {&FRAME-NAME} OR 
     INPUT FRAME {&FRAME-NAME} fi-corte-comerc <> ob-etiqueta.corte-comerc OR 
     INPUT FRAME {&FRAME-NAME} ob-etiqueta.dt-emissao <> ob-etiqueta.dt-emissao OR 
     INPUT FRAME {&FRAME-NAME} fi-localizacao <> ob-etiqueta.localizacao THEN DO:
     IF INPUT FRAME {&FRAME-NAME} rs-situacao <> "5" THEN DO: /* Elmina romaneio */
        FIND ped-item-rom WHERE ped-item-rom.num-etiqueta = ob-etiqueta.num-etiqueta NO-ERROR.
        IF AVAIL ped-item-rom THEN
           DELETE ped-item-rom.
     END.
     
     FIND CURRENT ob-etiqueta EXCLUSIVE-LOCK NO-ERROR.
     /*
     FIND corte-comerc WHERE corte-comerc.codigo = fi-corte-comerc:SCREEN-VALUE IN FRAME {&FRAME-NAME}
                       NO-LOCK. */
     ASSIGN ob-etiqueta.situacao = INT(INPUT FRAME {&FRAME-NAME} rs-situacao)
            ob-etiqueta.situacao:SCREEN-VALUE IN FRAME {&FRAME-NAME} = INPUT FRAME {&FRAME-NAME} rs-situacao
            ob-etiqueta.dt-emissao = INPUT FRAME {&FRAME-NAME} ob-etiqueta.dt-emissao
            ob-etiqueta.localizacao = fi-localizacao:SCREEN-VALUE IN FRAME {&FRAME-NAME}
            ob-etiqueta.quantidade = INPUT FRAME {&FRAME-NAME} ob-etiqueta.quantidade
            ob-etiqueta.peso-bruto = DEC(INPUT FRAME {&FRAME-NAME} fi-peso-bruto)
            SUBSTR(ob-etiqueta.char-1,50,200) = SUBSTR(ob-etiqueta.char-1,50,200) + ed-motivo:SCREEN-VALUE.
  END.
  ELSE
     MESSAGE "Nenhuma alteraá∆o foi feita."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
  
  ASSIGN rs-situacao:SENSITIVE IN FRAME {&FRAME-NAME} = NO
         fi-corte-comerc:SENSITIVE IN FRAME {&FRAME-NAME} = NO
         fi-localizacao:SENSITIVE IN FRAME {&FRAME-NAME} = NO
         fi-peso-bruto:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

  APPLY 'entry' TO ob-etiqueta.num-etiqueta.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-prox
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-prox w-digita
ON CHOOSE OF bt-prox IN FRAME F-Main /* Button 3 */
DO:
   FIND NEXT ob-etiqueta WHERE
             ob-etiqueta.cod-estabel  = INPUT FRAME {&FRAME-NAME} fi-cod-estabel
             NO-LOCK NO-ERROR.

   IF AVAIL ob-etiqueta THEN DO:
      ob-etiqueta.num-etiqueta:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ob-etiqueta.num-etiqueta).
      apply "choose":U to bt-etiqueta.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ob-etiqueta.dt-emissao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ob-etiqueta.dt-emissao w-digita
ON LEAVE OF ob-etiqueta.dt-emissao IN FRAME F-Main /* Dt Emiss∆o Etq */
DO:
  IF INPUT FRAME {&FRAME-NAME} ob-etiqueta.dt-emissao <> ob-etiqueta.dt-emissao THEN DO:
     ASSIGN bt-ok:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            bt-desfaz:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-corte-comerc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-corte-comerc w-digita
ON VALUE-CHANGED OF fi-corte-comerc IN FRAME F-Main /* Corte-Comercial */
DO:
   IF INPUT FRAME {&FRAME-NAME} fi-corte-comerc <> ob-etiqueta.corte-comerc THEN DO:
      FIND corte-comerc WHERE corte-comerc.codigo = INPUT FRAME {&FRAME-NAME} fi-corte-comerc
                        NO-ERROR.
      IF NOT AVAIL corte-comerc THEN
         MESSAGE "Corte comercial n∆o encontrado."
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
      ELSE
         ASSIGN bt-ok:SENSITIVE IN FRAME {&FRAME-NAME} = YES
                bt-desfaz:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
   END.
   ELSE
      ASSIGN bt-ok:SENSITIVE IN FRAME {&FRAME-NAME} = NO
             bt-desfaz:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-localizacao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-localizacao w-digita
ON VALUE-CHANGED OF fi-localizacao IN FRAME F-Main
DO:
   IF INPUT FRAME {&FRAME-NAME} fi-localizacao <> ob-etiqueta.localizacao THEN
      ASSIGN bt-ok:SENSITIVE IN FRAME {&FRAME-NAME} = YES
             bt-desfaz:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
   ELSE
      ASSIGN bt-ok:SENSITIVE IN FRAME {&FRAME-NAME} = NO
             bt-desfaz:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ob-etiqueta.num-etiqueta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ob-etiqueta.num-etiqueta w-digita
ON LEAVE OF ob-etiqueta.num-etiqueta IN FRAME F-Main /* Num Etiqueta */
DO:
  APPLY 'choose' TO bt-etiqueta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ob-etiqueta.num-etiqueta w-digita
ON RETURN OF ob-etiqueta.num-etiqueta IN FRAME F-Main /* Num Etiqueta */
DO:
   APPLY 'choose' TO bt-etiqueta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-situacao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-situacao w-digita
ON VALUE-CHANGED OF rs-situacao IN FRAME F-Main
DO:
   IF INPUT FRAME {&FRAME-NAME} rs-situacao <> 
      ob-etiqueta.situacao:SCREEN-VALUE IN FRAME {&FRAME-NAME} THEN
      ASSIGN bt-ok:SENSITIVE IN FRAME {&FRAME-NAME} = YES
             bt-desfaz:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
   ELSE
      ASSIGN bt-ok:SENSITIVE IN FRAME {&FRAME-NAME} = NO
             bt-desfaz:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-digita 


/* ***************************  Main Block  *************************** */

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
  DISPLAY fi-cod-estabel fi-localizacao ed-motivo rs-situacao fi-romaneio 
          fi-corte-comerc fi-corte-sugerido fi-hora fi-reserva fi-quantidade 
          fi-peso-bruto fi-data-invent 
      WITH FRAME F-Main IN WINDOW w-digita.
  IF AVAILABLE ob-etiqueta THEN 
    DISPLAY ob-etiqueta.num-etiqueta ob-etiqueta.nr-sequencia ob-etiqueta.nr-ob 
          ob-etiqueta.localizacao ob-etiqueta.situacao ob-etiqueta.quantidade 
          ob-etiqueta.dt-emissao ob-etiqueta.it-codigo ob-etiqueta.nr-lote 
          ob-etiqueta.cod-refer 
      WITH FRAME F-Main IN WINDOW w-digita.
  ENABLE RECT-1 RECT-2 fi-cod-estabel ob-etiqueta.num-etiqueta bt-etiqueta 
         ob-etiqueta.quantidade ob-etiqueta.dt-emissao bt-cancelar bt-ajuda 
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

  /* {utp/ut9000.i "ACT0001" "2.04.00.000"} */

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

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this Digitacao, and there are no
     tables specified in any contained Browse, Query, or Frame. */

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

