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
{include/i-prgvrs.i ESSP0165 2.04.00.000}
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
       FIELD nr-fardo LIKE mp-fardo.nr-fardo
       FIELD padrao   LIKE mp-fardo.padrao
       FIELD letra          AS CHAR FORMAT "x(3)"
       FIELD desc-coloracao AS CHAR FORMAT "x(16)" 
       FIELD desc-tipo      AS CHAR FORMAT "x(15)"
       FIELD sl2            AS CHAR FORMAT "x(9)" 
       FIELD peso           LIKE mp-fardo.peso
       FIELD peso-calc      LIKE mp-fardo.peso
       INDEX indice1 nr-fardo.

DEF VAR i-ct      AS INT.
DEF VAR l-opc     AS LOG.
DEF VAR de-valor  AS DEC.
DEF VAR de-acerto AS DEC.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Digitacao
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-fardos

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-mp-fardo

/* Definitions for BROWSE br-fardos                                     */
&Scoped-define FIELDS-IN-QUERY-br-fardos tt-mp-fardo.nr-fardo tt-mp-fardo.padrao tt-mp-fardo.letra tt-mp-fardo.desc-coloracao tt-mp-fardo.desc-tipo tt-mp-fardo.sl2 tt-mp-fardo.peso tt-mp-fardo.peso-calc   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-fardos   
&Scoped-define SELF-NAME br-fardos
&Scoped-define OPEN-QUERY-br-fardos RUN pi-soma. OPEN QUERY {&SELF-NAME} FOR EACH tt-mp-fardo NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-fardos tt-mp-fardo
&Scoped-define FIRST-TABLE-IN-QUERY-br-fardos tt-mp-fardo


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br-fardos}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi-nro-docto fi-peso rs-tipo-oper ~
bt-processa bt-ok br-fardos bt-ajuda bt-cancelar RECT-1 RECT-44 RECT-54 ~
RECT-55 
&Scoped-Define DISPLAYED-OBJECTS fi-nro-docto fi-peso rs-tipo-oper ~
fi-cod-emit fi-dt-emissao-nf fi-dt-recebimento fi-nome-emit fi-nro-contrato ~
fi-padrao fi-peso-nf fi-qtd-fardos fi-qt-fardo fi-peso-fardo fi-peso-calc 

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

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Cancelar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "OK" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-processa 
     IMAGE-UP FILE "image/im-vapra.bmp":U
     LABEL "Button 1" 
     SIZE 4 BY 1.

DEFINE VARIABLE fi-cod-emit AS INTEGER FORMAT "->,>>>,>>9" INITIAL 0 
     LABEL "Fornecedor" 
     VIEW-AS FILL-IN 
     SIZE 9.43 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-emissao-nf AS DATE FORMAT "99/99/9999" 
     LABEL "Data Emiss∆o" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-recebimento AS DATE FORMAT "99/99/9999" 
     LABEL "Data Recebimento" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nome-emit AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 35 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nro-contrato AS CHARACTER FORMAT "X(10)" 
     LABEL "Contrato" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nro-docto AS INTEGER FORMAT "->,>>>,>>9" INITIAL 0 
     LABEL "Nota Fiscal" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 TOOLTIP "Informe o n£mero da nota fiscal" NO-UNDO.

DEFINE VARIABLE fi-padrao AS CHARACTER FORMAT "X(20)":U 
     LABEL "Padr∆o" 
     VIEW-AS FILL-IN 
     SIZE 21 BY .88 NO-UNDO.

DEFINE VARIABLE fi-peso AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Peso Acertar" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-peso-calc AS DECIMAL FORMAT ">>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88
     FGCOLOR 12  NO-UNDO.

DEFINE VARIABLE fi-peso-fardo AS DECIMAL FORMAT ">>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Pesos" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-peso-nf AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "Peso NF" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-qt-fardo AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Qtd Fardos" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-qtd-fardos AS INTEGER FORMAT "->,>>>,>>9" INITIAL 0 
     LABEL "Qtd Fardos" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE rs-tipo-oper AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Saida Estoque", 1,
"Entrada Estoque", 2
     SIZE 28 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 80 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-44
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 80 BY 6.75.

DEFINE RECTANGLE RECT-54
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 78 BY 1.5.

DEFINE RECTANGLE RECT-55
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 78 BY 4.5.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-fardos FOR 
      tt-mp-fardo SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-fardos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-fardos w-digita _FREEFORM
  QUERY br-fardos DISPLAY
      tt-mp-fardo.nr-fardo  COLUMN-LABEL "Fardo"
tt-mp-fardo.padrao    
tt-mp-fardo.letra           COLUMN-LABEL "letra"
tt-mp-fardo.desc-coloracao  COLUMN-LABEL "Tonalidade"
tt-mp-fardo.desc-tipo       COLUMN-LABEL "Tipo"
tt-mp-fardo.sl2             COLUMN-LABEL "Comprimento"
tt-mp-fardo.peso            COLUMN-LABEL "Peso Fardo"
tt-mp-fardo.peso-calc       COLUMN-LABEL "Novo Peso"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 79 BY 10.75
         FONT 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi-nro-docto AT ROW 1.5 COL 10 COLON-ALIGNED
     fi-peso AT ROW 1.5 COL 30.43 COLON-ALIGNED
     rs-tipo-oper AT ROW 1.5 COL 44.29 NO-LABEL
     bt-processa AT ROW 1.42 COL 75
     bt-ok AT ROW 20 COL 2.14
     br-fardos AT ROW 8 COL 2
     fi-cod-emit AT ROW 3.75 COL 11 COLON-ALIGNED
     fi-dt-emissao-nf AT ROW 4.75 COL 11 COLON-ALIGNED
     fi-dt-recebimento AT ROW 4.75 COL 40.43 COLON-ALIGNED
     fi-nome-emit AT ROW 3.75 COL 20.57 COLON-ALIGNED NO-LABEL
     fi-nro-contrato AT ROW 3.75 COL 66 COLON-ALIGNED
     fi-padrao AT ROW 5.75 COL 29.57 COLON-ALIGNED
     fi-peso-nf AT ROW 4.75 COL 66 COLON-ALIGNED
     fi-qtd-fardos AT ROW 5.75 COL 11 COLON-ALIGNED
     fi-qt-fardo AT ROW 18.83 COL 11 COLON-ALIGNED
     fi-peso-fardo AT ROW 18.83 COL 59.14 COLON-ALIGNED
     bt-ajuda AT ROW 19.96 COL 70.14
     bt-cancelar AT ROW 20 COL 13.14
     fi-peso-calc AT ROW 18.83 COL 68.29 COLON-ALIGNED NO-LABEL
     RECT-1 AT ROW 19.79 COL 1
     RECT-44 AT ROW 1 COL 1
     RECT-54 AT ROW 1.25 COL 2
     RECT-55 AT ROW 3 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 20.33
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
         TITLE              = "Acerto Estoque de Algod∆o"
         HEIGHT             = 20.33
         WIDTH              = 80
         MAX-HEIGHT         = 29
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 29
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
/* BROWSE-TAB br-fardos bt-ok F-Main */
/* SETTINGS FOR FILL-IN fi-cod-emit IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-dt-emissao-nf IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-dt-recebimento IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nome-emit IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nro-contrato IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-padrao IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-peso-calc IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-peso-fardo IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-peso-nf IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qt-fardo IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-fardos IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-digita)
THEN w-digita:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-fardos
/* Query rebuild information for BROWSE br-fardos
     _START_FREEFORM
RUN pi-soma.
OPEN QUERY {&SELF-NAME} FOR EACH tt-mp-fardo NO-LOCK.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-fardos */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON END-ERROR OF w-digita /* Acerto Estoque de Algod∆o */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON WINDOW-CLOSE OF w-digita /* Acerto Estoque de Algod∆o */
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


&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar w-digita
ON CHOOSE OF bt-cancelar IN FRAME F-Main /* Cancelar */
DO:
  apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-digita
ON CHOOSE OF bt-ok IN FRAME F-Main /* OK */
DO:
   IF SEARCH("n:\interfac\mail\blat.exe") = ? THEN DO:
      MESSAGE "N∆o consigo conectar com o programa de transmiss∆o de email ! MOVIMENTAÄ«O ESTOQUE N«O FOI EFETUADA"
              VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY 'entry' TO bt-ok IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.

   FOR EACH tt-mp-fardo.
       FIND mp-fardo OF tt-mp-fardo NO-ERROR.
       ASSIGN mp-fardo.peso = tt-mp-fardo.peso-calc.
   END.
   RUN envia-email.

   ASSIGN bt-ok:SENSITIVE IN FRAME {&FRAME-NAME}        = NO 
          fi-nro-docto:SENSITIVE IN FRAME {&FRAME-NAME} = YES 
          fi-peso:SENSITIVE IN FRAME {&FRAME-NAME}      = YES 
          rs-tipo-oper:SENSITIVE IN FRAME {&FRAME-NAME} = YES 
          bt-processa:SENSITIVE IN FRAME {&FRAME-NAME}  = YES.

   ASSIGN fi-nro-docto:SCREEN-VALUE IN FRAME {&FRAME-NAME}      = ''
          fi-peso:SCREEN-VALUE IN FRAME {&FRAME-NAME}           = ''
          fi-nome-emit:SCREEN-VALUE IN FRAME {&FRAME-NAME}      = ''
          fi-cod-emit:SCREEN-VALUE IN FRAME {&FRAME-NAME}       = ''
          fi-nro-contrato:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = ''
          fi-dt-emissao-nf:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = ''
          fi-dt-recebimento:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ''
          fi-peso-nf:SCREEN-VALUE IN FRAME {&FRAME-NAME}        = ''
          fi-qtd-fardos:SCREEN-VALUE IN FRAME {&FRAME-NAME}     = ''
          fi-padrao:SCREEN-VALUE IN FRAME {&FRAME-NAME}         = ''.

   EMPTY TEMP-TABLE tt-mp-fardo.
   {&OPEN-QUERY-br-fardos}

   APPLY 'entry' TO fi-nro-docto.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-processa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-processa w-digita
ON CHOOSE OF bt-processa IN FRAME F-Main /* Button 1 */
DO:
  FIND LAST mp-entr-mat WHERE
            mp-entr-mat.nro-docto = fi-nro-docto:INPUT-VALUE NO-LOCK NO-ERROR.
  IF NOT AVAIL mp-entr-mat THEN DO:
     MESSAGE "Nota fiscal n∆o foi encontrada ! ! !"
         VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     APPLY 'entry' to fi-nro-docto.
     RETURN NO-APPLY.
  END.

  IF fi-peso:INPUT-VALUE = 0 THEN DO:
     MESSAGE "Favor Informar o PESO a ACERTAR ! ! !"
         VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     APPLY 'entry' to fi-peso.
     RETURN NO-APPLY.
  END.
  
  FIND FIRST tt-mp-fardo NO-LOCK NO-ERROR.
  IF NOT AVAIL tt-mp-fardo THEN DO:
     MESSAGE "Todos os fardos desta nota, ja foram  BAIXADOS." SKIP 
             "Deseja Fazer o acerto de peso pelos fardos mais antigos." SKIP(1)
             "Confirma Acerto Estoque dos Fardos ?"
             VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE l-conf AS LOG.

     IF NOT l-conf THEN DO.
        ASSIGN fi-nro-docto:SCREEN-VALUE IN FRAME {&FRAME-NAME}      = ''
               fi-peso:SCREEN-VALUE IN FRAME {&FRAME-NAME}           = ''
               fi-nome-emit:SCREEN-VALUE IN FRAME {&FRAME-NAME}      = ''
               fi-cod-emit:SCREEN-VALUE IN FRAME {&FRAME-NAME}       = ''
               fi-nro-contrato:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = ''
               fi-dt-emissao-nf:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = ''
               fi-dt-recebimento:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ''
               fi-peso-nf:SCREEN-VALUE IN FRAME {&FRAME-NAME}        = ''
               fi-qtd-fardos:SCREEN-VALUE IN FRAME {&FRAME-NAME}     = ''
               fi-padrao:SCREEN-VALUE IN FRAME {&FRAME-NAME}         = ''.

        APPLY 'entry' TO fi-nro-docto.
        RETURN NO-APPLY.
     END.
     RUN pi-fardos-ant.
  END.
  
  ASSIGN INPUT FRAME {&FRAME-NAME} fi-peso.

  ASSIGN de-valor  = TRUNCATE(fi-peso / fi-qt-fardo, 2)
         de-acerto = fi-peso - (de-valor * fi-qt-fardo).

  FOR EACH tt-mp-fardo.
      IF rs-tipo-oper:INPUT-VALUE = 1 THEN
         ASSIGN tt-mp-fardo.peso-calc = tt-mp-fardo.peso - de-valor.
      ELSE
         ASSIGN tt-mp-fardo.peso-calc = tt-mp-fardo.peso + de-valor.
  END.
  FIND LAST tt-mp-fardo NO-ERROR.
  IF AVAIL tt-mp-fardo THEN DO:
     IF rs-tipo-oper:INPUT-VALUE = 1 THEN
        ASSIGN tt-mp-fardo.peso-calc = tt-mp-fardo.peso-calc - de-acerto.
     ELSE
        ASSIGN tt-mp-fardo.peso-calc = tt-mp-fardo.peso-calc + de-acerto.

  END.
  {&OPEN-QUERY-br-fardos}
  ASSIGN bt-ok:SENSITIVE IN FRAME {&FRAME-NAME}        = YES
         fi-nro-docto:SENSITIVE IN FRAME {&FRAME-NAME} = NO
         fi-peso:SENSITIVE IN FRAME {&FRAME-NAME}      = NO
         rs-tipo-oper:SENSITIVE IN FRAME {&FRAME-NAME} = NO
         bt-processa:SENSITIVE IN FRAME {&FRAME-NAME}  = NO.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nro-docto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nro-docto w-digita
ON LEAVE OF fi-nro-docto IN FRAME F-Main /* Nota Fiscal */
DO:
  EMPTY TEMP-TABLE tt-mp-fardo.
  ASSIGN fi-qt-fardo          = 0
         fi-peso:SCREEN-VALUE = ''
         fi-peso-fardo        = 0.

  FIND LAST mp-entr-mat WHERE
            mp-entr-mat.nro-docto = SELF:INPUT-VALUE NO-LOCK NO-ERROR.
  IF AVAIL mp-entr-mat THEN DO:
     FIND emitente WHERE
          emitente.cod-emit = mp-entr-mat.cod-emit NO-LOCK NO-ERROR.
     IF AVAIL emitente THEN
        ASSIGN fi-cod-emit:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(mp-entr-mat.cod-emit).

     ASSIGN fi-nome-emit:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.nome-emit
            fi-nro-contrato:SCREEN-VALUE IN FRAME {&FRAME-NAME} = mp-entr-mat.nro-contrato
            fi-dt-emissao-nf:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(mp-entr-mat.dt-emissao-nf)
            fi-dt-recebimento:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(mp-entr-mat.dt-recebimento)
            fi-peso-nf:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(mp-entr-mat.peso-nf)
            fi-qtd-fardos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING((mp-entr-mat.qtd-fardos[1] +
                                                                        mp-entr-mat.qtd-fardos[2] +
                                                                        mp-entr-mat.qtd-fardos[3] +
                                                                        mp-entr-mat.qtd-fardos[4] +
                                                                        mp-entr-mat.qtd-fardos[5]))
            fi-padrao:SCREEN-VALUE IN FRAME {&FRAME-NAME} = mp-entr-mat.padrao[1].
        
      FOR EACH mp-fardo OF mp-entr-mat WHERE
               mp-fardo.situacao > 1 AND
               mp-fardo.situacao < 4 NO-LOCK.
          FIND tt-mp-fardo WHERE
               tt-mp-fardo.nr-fardo = mp-fardo.nr-fardo 
               NO-LOCK NO-ERROR.     
          IF NOT AVAIL tt-mp-fardo THEN DO:
             CREATE tt-mp-fardo.
             ASSIGN tt-mp-fardo.nr-fardo       = mp-fardo.nr-fardo       
                    tt-mp-fardo.padrao         = mp-fardo.padrao         
                    tt-mp-fardo.letra          = mp-fardo.letra + STRING(mp-fardo.cd-coloracao) + 
                                                                  STRING(mp-fardo.cd-tipo)
                    tt-mp-fardo.desc-coloracao = IF AVAIL mp-coloracao 
                                                 THEN mp-coloracao.tonalidade ELSE ''
                    tt-mp-fardo.desc-tipo      = IF AVAIL mp-tipo
                                                 THEN mp-tipo.tipo ELSE ''
                    tt-mp-fardo.sl2            = STRING(mp-fardo.sl2, ">>9.99")
                    tt-mp-fardo.peso           = mp-fardo.peso. 

              ASSIGN fi-qt-fardo = fi-qt-fardo + 1
                     fi-peso-fardo = fi-peso-fardo + mp-fardo.peso
                     fi-qt-fardo:SCREEN-VALUE   = STRING(fi-qt-fardo)
                     fi-peso-fardo:SCREEN-VALUE = STRING(fi-peso-fardo).
          END.
      END.
      {&OPEN-QUERY-br-fardos}
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-fardos
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
  DISPLAY fi-nro-docto fi-peso rs-tipo-oper fi-cod-emit fi-dt-emissao-nf 
          fi-dt-recebimento fi-nome-emit fi-nro-contrato fi-padrao fi-peso-nf 
          fi-qtd-fardos fi-qt-fardo fi-peso-fardo fi-peso-calc 
      WITH FRAME F-Main IN WINDOW w-digita.
  ENABLE fi-nro-docto fi-peso rs-tipo-oper bt-processa bt-ok br-fardos bt-ajuda 
         bt-cancelar RECT-1 RECT-44 RECT-54 RECT-55 
      WITH FRAME F-Main IN WINDOW w-digita.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW w-digita.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE envia-email w-digita 
PROCEDURE envia-email :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 DEF VAR c-mensagem AS CHAR.                     
 DEF VAR c-movto AS CHAR FORMAT "x(15)" EXTENT 2.
 DEF VAR i-opc   AS INT.
 ASSIGN c-movto[1] = "Saida Estoque"
        c-movto[2] = "Entrada Estoque"
        i-opc = rs-tipo-oper:INPUT-VALUE IN FRAME {&FRAME-NAME}.

 ASSIGN c-mensagem = "                 MOVIMENTAÄ«O DO ESTOQUE DE ALGOD«O" + CHR(13) + CHR(13) +
                     "O Estoque de fardos de algod∆o foi movimentado no dia: " + STRING(TODAY) + ", devido" +
                     chr(13) +  "a quebra de algod∆o." + CHR(13) + 
                     "Nota Fiscal: " +  fi-nro-docto:SCREEN-VALUE IN FRAME {&FRAME-NAME} +
                     "   Peso Acertar: " +  fi-peso:SCREEN-VALUE IN FRAME {&FRAME-NAME} +
                     "   Tipo de Movimentaá∆o: " + c-movto[i-opc] + CHR(13) + CHR(13) +
                     "                    DADOS DA NOTA FISCAL " + CHR(13) + 
                     "Fornecedor: " + fi-cod-emit:SCREEN-VALUE IN FRAME {&FRAME-NAME} + " " +
                     fi-nome-emit:SCREEN-VALUE IN FRAME {&FRAME-NAME} +  "    Contrato: " +
                     fi-nro-contrato:SCREEN-VALUE IN FRAME {&FRAME-NAME} +  CHR(13) +
                     "Data Emiss∆o: " + STRING(fi-dt-emissao-nf:SCREEN-VALUE IN FRAME {&FRAME-NAME}) + 
                     "   Data Emiss∆o: " + STRING(fi-dt-recebimento:SCREEN-VALUE IN FRAME {&FRAME-NAME}) + 
                     "   Peso NF: " +  fi-peso-nf:SCREEN-VALUE IN FRAME {&FRAME-NAME} +  CHR(13) +
                     "Qtd Fardos: " +  fi-qtd-fardos:SCREEN-VALUE IN FRAME {&FRAME-NAME} + 
                     "   Padrao do Algodao: " +  fi-padrao:SCREEN-VALUE IN FRAME {&FRAME-NAME} + CHR(13) + CHR(13) +
                     "             Almoxarifado" + CHR(13) +
                     "TEAR TEXTIL INDUSTRIA E COMERCIO LTDA".
                     
RUN esapi/esapi002.p (INPUT "teartextil@teartextil.com.br", /* e-mail remetente*/
                      INPUT "mirian.campos@teartextil.com.br", /* e-mail destino */
                      INPUT "Acerto de Estoque de Algod∆o", /* Assunto */
                      INPUT c-mensagem, /* Mensagem */
                      INPUT "",  /*arquivo anexo*/
                      INPUT YES). /* Mostra Erros */

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

  {utp/ut9000.i "ESSP0165" "2.04.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  ASSIGN bt-ok:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-fardos-ant w-digita 
PROCEDURE pi-fardos-ant :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 FOR EACH mp-fardo WHERE
          mp-fardo.situacao > 1 AND 
          mp-fardo.situacao < 4 NO-LOCK.
     FIND tt-mp-fardo WHERE
          tt-mp-fardo.nr-fardo = mp-fardo.nr-fardo 
          NO-LOCK NO-ERROR.     
     IF NOT AVAIL tt-mp-fardo THEN DO:
        CREATE tt-mp-fardo.
        ASSIGN tt-mp-fardo.nr-fardo       = mp-fardo.nr-fardo       
               tt-mp-fardo.padrao         = mp-fardo.padrao         
               tt-mp-fardo.letra          = mp-fardo.letra + STRING(mp-fardo.cd-coloracao) + 
                                                             STRING(mp-fardo.cd-tipo)
               tt-mp-fardo.desc-coloracao = IF AVAIL mp-coloracao 
                                            THEN mp-coloracao.tonalidade ELSE ''
               tt-mp-fardo.desc-tipo      = IF AVAIL mp-tipo
                                            THEN mp-tipo.tipo ELSE ''
               tt-mp-fardo.sl2            = STRING(mp-fardo.sl2, ">>9.99")
               tt-mp-fardo.peso           = mp-fardo.peso. 

         ASSIGN fi-qt-fardo = fi-qt-fardo + 1
                fi-peso-fardo = fi-peso-fardo + mp-fardo.peso
                fi-qt-fardo:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = STRING(fi-qt-fardo)
                fi-peso-fardo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(fi-peso-fardo).
         IF fi-qt-fardo = INT(fi-qtd-fardos:SCREEN-VALUE IN FRAME {&FRAME-NAME}) THEN LEAVE.
     END.
 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-soma w-digita 
PROCEDURE pi-soma :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 ASSIGN fi-qt-fardo   = 0
        fi-peso-fardo = 0
        fi-peso-calc  = 0.

 FOR EACH tt-mp-fardo NO-LOCK.
     ASSIGN fi-qt-fardo   = fi-qt-fardo + 1
            fi-peso-fardo = fi-peso-fardo + tt-mp-fardo.peso
            fi-peso-calc  = fi-peso-calc  + tt-mp-fardo.peso-calc.
 END.
 DISP fi-qt-fardo
      fi-peso-fardo
      fi-peso-calc 
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

