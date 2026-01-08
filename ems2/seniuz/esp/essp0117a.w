&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
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
{include/i-prgvrs.i ESSP0117A 2.04.00.000}
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
DEF INPUT PARAMETER p-row-table AS ROWID NO-UNDO.

/* Local Variable Definitions ---                                       */
DEF VAR i-ct AS INT.
DEF VAR i-ct-lin AS INT.
DEF VAR i-ct-col AS INT.
DEF VAR i-qt-fardos LIKE mp-entr-mat.qtd-fardos.
DEF VAR de-peso-liquido-cam LIKE mp-entr-cam.peso-liquido.
DEF VAR l-opc AS LOG INITIAL NO.

/* Include com as Funcá‰es em PCL e Codigo de Barras */
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

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-13 RECT-14 fi-cod-emit ~
fi-nro-docto fi-nro-contrato fi-dt-emissao fi-dt-recebimento fi-procedencia ~
fi-peso-nf fi-deposito fi-padrao1 fi-qtd-fardos1 fi-padrao2 fi-qtd-fardos2 ~
fi-padrao3 fi-qtd-fardos3 fi-padrao4 fi-qtd-fardos4 fi-padrao5 ~
fi-qtd-fardos5 bt-ok bt-cancelar bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS fi-placa fi-dt-entrada fi-hora ~
fi-peso-bruto fi-cod-emit fi-nome-emit fi-nro-docto fi-nro-contrato ~
fi-dt-emissao fi-dt-recebimento fi-procedencia fi-peso-nf fi-deposito ~
fi-padrao1 fi-qtd-fardos1 fi-padrao2 fi-qtd-fardos2 fi-padrao3 ~
fi-qtd-fardos3 fi-padrao4 fi-qtd-fardos4 fi-padrao5 fi-qtd-fardos5 

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

DEFINE VARIABLE fi-cod-emit AS INTEGER FORMAT ">>>,>>9" INITIAL 0 
     LABEL "Fornecedor" 
     VIEW-AS FILL-IN 
     SIZE 6.14 BY .88.

DEFINE VARIABLE fi-deposito AS CHARACTER FORMAT "X(8)":U 
     LABEL "Deposito" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-emissao AS DATE FORMAT "99/99/9999" 
     LABEL "Data Emiss∆o" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88.

DEFINE VARIABLE fi-dt-entrada AS DATE FORMAT "99/99/9999" 
     LABEL "Data de Entrada" 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-recebimento AS DATE FORMAT "99/99/9999" 
     LABEL "Data Recebimento" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88.

DEFINE VARIABLE fi-hora AS CHARACTER FORMAT "99:99":U 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nome-emit AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 36.29 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nro-contrato AS CHARACTER FORMAT "X(10)" 
     LABEL "Contrato" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88.

DEFINE VARIABLE fi-nro-docto AS INTEGER FORMAT "->>>>>>9" INITIAL 0 
     LABEL "Nota Fiscal" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88.

DEFINE VARIABLE fi-padrao1 AS CHARACTER FORMAT "X(20)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .88 NO-UNDO.

DEFINE VARIABLE fi-padrao2 AS CHARACTER FORMAT "X(20)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .88 NO-UNDO.

DEFINE VARIABLE fi-padrao3 AS CHARACTER FORMAT "X(20)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .88 NO-UNDO.

DEFINE VARIABLE fi-padrao4 AS CHARACTER FORMAT "X(20)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .88 NO-UNDO.

DEFINE VARIABLE fi-padrao5 AS CHARACTER FORMAT "X(20)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .88 NO-UNDO.

DEFINE VARIABLE fi-peso-bruto AS DECIMAL FORMAT ">>>,>>9.99" INITIAL 0 
     LABEL "Peso Bruto" 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-peso-nf AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     LABEL "Peso Liquido NF" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88.

DEFINE VARIABLE fi-placa AS CHARACTER FORMAT "XXX-9999" 
     LABEL "Placa" 
     VIEW-AS FILL-IN 
     SIZE 10.29 BY .88 NO-UNDO.

DEFINE VARIABLE fi-procedencia AS CHARACTER FORMAT "X(20)" 
     LABEL "Procedància" 
     VIEW-AS FILL-IN 
     SIZE 20 BY .88.

DEFINE VARIABLE fi-qtd-fardos1 AS INTEGER FORMAT ">,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4.72 BY .88.

DEFINE VARIABLE fi-qtd-fardos2 AS INTEGER FORMAT ">,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4.72 BY .88.

DEFINE VARIABLE fi-qtd-fardos3 AS INTEGER FORMAT ">,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4.72 BY .88.

DEFINE VARIABLE fi-qtd-fardos4 AS INTEGER FORMAT ">,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4.72 BY .88.

DEFINE VARIABLE fi-qtd-fardos5 AS INTEGER FORMAT ">,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4.72 BY .88.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 69 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 69 BY 3.33.

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 69 BY 11.21.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi-placa AT ROW 1.5 COL 17 COLON-ALIGNED
     fi-dt-entrada AT ROW 2.5 COL 17 COLON-ALIGNED
     fi-hora AT ROW 2.5 COL 30 COLON-ALIGNED NO-LABEL
     fi-peso-bruto AT ROW 3.5 COL 17 COLON-ALIGNED
     fi-cod-emit AT ROW 5 COL 17 COLON-ALIGNED
     fi-nome-emit AT ROW 5 COL 23.72 COLON-ALIGNED NO-LABEL
     fi-nro-docto AT ROW 6 COL 17 COLON-ALIGNED
     fi-nro-contrato AT ROW 6 COL 50 COLON-ALIGNED
     fi-dt-emissao AT ROW 7 COL 17 COLON-ALIGNED
     fi-dt-recebimento AT ROW 7 COL 50 COLON-ALIGNED
     fi-procedencia AT ROW 8 COL 17 COLON-ALIGNED
     fi-peso-nf AT ROW 8 COL 50 COLON-ALIGNED
     fi-deposito AT ROW 9 COL 17 COLON-ALIGNED
     fi-padrao1 AT ROW 10.75 COL 17 COLON-ALIGNED NO-LABEL
     fi-qtd-fardos1 AT ROW 10.75 COL 50 COLON-ALIGNED NO-LABEL
     fi-padrao2 AT ROW 11.75 COL 17 COLON-ALIGNED NO-LABEL
     fi-qtd-fardos2 AT ROW 11.75 COL 50 COLON-ALIGNED NO-LABEL
     fi-padrao3 AT ROW 12.75 COL 17 COLON-ALIGNED NO-LABEL
     fi-qtd-fardos3 AT ROW 12.75 COL 50 COLON-ALIGNED NO-LABEL
     fi-padrao4 AT ROW 13.75 COL 17 COLON-ALIGNED NO-LABEL
     fi-qtd-fardos4 AT ROW 13.75 COL 50 COLON-ALIGNED NO-LABEL
     fi-padrao5 AT ROW 14.75 COL 17 COLON-ALIGNED NO-LABEL
     fi-qtd-fardos5 AT ROW 14.75 COL 50 COLON-ALIGNED NO-LABEL
     bt-ok AT ROW 16.25 COL 3
     bt-cancelar AT ROW 16.25 COL 14
     bt-ajuda AT ROW 16.29 COL 59.86
     "Qt Fardos" VIEW-AS TEXT
          SIZE 11 BY .75 AT ROW 10 COL 52
          FGCOLOR 9 FONT 8
     "Padr∆o" VIEW-AS TEXT
          SIZE 8 BY .75 AT ROW 10 COL 19
          FGCOLOR 9 FONT 8
     RECT-1 AT ROW 16.04 COL 1.72
     RECT-13 AT ROW 1.25 COL 2
     RECT-14 AT ROW 4.75 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 16.58
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
         TITLE              = "Descarga de Caminh∆o"
         HEIGHT             = 17.54
         WIDTH              = 70.86
         MAX-HEIGHT         = 28.54
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 28.54
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
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN fi-dt-entrada IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-hora IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nome-emit IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-peso-bruto IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-placa IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-digita)
THEN w-digita:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON END-ERROR OF w-digita /* Descarga de Caminh∆o */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON WINDOW-CLOSE OF w-digita /* Descarga de Caminh∆o */
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
    DO TRANSACTION:
        IF INPUT FRAME {&FRAME-NAME} fi-peso-nf <= 0 THEN DO.
            MESSAGE "O Peso Liquido da Nota Fiscal deve ser Informado . . ."
                    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            APPLY 'entry' TO fi-peso-nf.
            RETURN NO-APPLY.
        END.

        IF fi-padrao1:SCREEN-VALUE = "" AND 
           fi-qtd-fardos1:INPUT-VALUE IN FRAME {&FRAME-NAME} > 0 THEN DO.
           MESSAGE "O padr∆o deve ser Informado . . ."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
           APPLY 'entry' TO fi-padrao1.
           RETURN NO-APPLY.
        END.
        IF fi-padrao2:SCREEN-VALUE = "" AND 
           fi-qtd-fardos2:INPUT-VALUE IN FRAME {&FRAME-NAME} > 0 THEN DO.
           MESSAGE "O padr∆o deve ser Informado . . ."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
           APPLY 'entry' TO fi-padrao2.
           RETURN NO-APPLY.
        END.
        IF fi-padrao3:SCREEN-VALUE = "" AND 
           fi-qtd-fardos3:INPUT-VALUE IN FRAME {&FRAME-NAME} > 0 THEN DO.
           MESSAGE "O padr∆o deve ser Informado . . ."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
           APPLY 'entry' TO fi-padrao3.
           RETURN NO-APPLY.
        END.
        IF fi-padrao4:SCREEN-VALUE = "" AND 
           fi-qtd-fardos4:INPUT-VALUE IN FRAME {&FRAME-NAME} > 0 THEN DO.
           MESSAGE "O padr∆o deve ser Informado . . ."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
           APPLY 'entry' TO fi-padrao4.
           RETURN NO-APPLY.
        END.
        IF fi-padrao5:SCREEN-VALUE = "" AND 
           fi-qtd-fardos5:INPUT-VALUE IN FRAME {&FRAME-NAME} > 0 THEN DO.
           MESSAGE "O padr∆o deve ser Informado . . ."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
           APPLY 'entry' TO fi-padrao5.
           RETURN NO-APPLY.
        END.
        
        IF fi-qtd-fardos1:INPUT-VALUE IN FRAME {&FRAME-NAME} + 
           fi-qtd-fardos2:INPUT-VALUE IN FRAME {&FRAME-NAME} + 
           fi-qtd-fardos3:INPUT-VALUE IN FRAME {&FRAME-NAME} + 
           fi-qtd-fardos4:INPUT-VALUE IN FRAME {&FRAME-NAME} + 
           fi-qtd-fardos5:INPUT-VALUE IN FRAME {&FRAME-NAME} > 160 THEN DO:
           MESSAGE "A quantidade de fardos informado para este veiculo," SKIP 
                   "Ç muito superior a sua capacidade de transporte." SKIP(1)
                   "Confirma Inclus∆o dos Fardos ?"
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE l-conf AS LOG.

           IF NOT l-conf THEN DO.
              APPLY 'entry' TO fi-qtd-fardos1.
              RETURN NO-APPLY.
           END.
        END.
        
       FIND emitente WHERE
            emitente.cod-emit = INPUT FRAME {&FRAME-NAME} fi-cod-emit
            NO-LOCK NO-ERROR.
       IF NOT AVAIL emitente THEN DO.
          MESSAGE "Fornecedor n∆o Cadastrado..."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          APPLY 'entry' TO fi-cod-emit.
          RETURN NO-APPLY.
       END.
    
       IF fi-procedencia:SCREEN-VALUE = '' THEN DO.
          MESSAGE "Procedància deve ser Informada..."
               VIEW-AS ALERT-BOX ERROR BUTTONS OK.
          APPLY 'entry' TO fi-procedencia.
          RETURN NO-APPLY.
       END.

       IF fi-dt-recebimento:SCREEN-VALUE = '' THEN DO.
          MESSAGE "Data de Recebimento deve ser Informada..."
               VIEW-AS ALERT-BOX ERROR BUTTONS OK.
          APPLY 'entry' TO fi-dt-recebimento.
          RETURN NO-APPLY.
       END.

       IF INPUT FRAME {&FRAME-NAME} fi-peso-nf <> de-peso-liquido-cam THEN DO.
           MESSAGE "O Peso Liqu°do Informado est† incompat°vel com o Peso Liqu°do Informado na Balanáa." SKIP
                   "Favor verificar o Peso Liqu°do Informado e o Peso Liqu°do da NF de Algod∆o."  SKIP
                   "Deseja, mesmo assim Considerar O Peso Liqu°do Informado para Rateio nos FARDOS ?"
                    VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO UPDATE l-opc.
           IF l-opc = NO THEN DO:
              APPLY 'entry' TO fi-peso-nf.
              return 'ADM-ERROR':U.
           END.
       END.

       FIND mp-entr-mat WHERE
            mp-entr-mat.nr-cdr = mp-entr-cam.nr-cdr NO-ERROR.
       IF NOT AVAIL mp-entr-mat THEN DO.
          CREATE mp-entr-mat.
          ASSIGN mp-entr-mat.nr-cdr = mp-entr-cam.nr-cdr.
       END.

       ASSIGN mp-entr-mat.cod-emit = INPUT FRAME {&FRAME-NAME} fi-cod-emit
              mp-entr-mat.nro-docto = INPUT FRAME {&FRAME-NAME} fi-nro-docto
              mp-entr-mat.nro-contrato = INPUT FRAME {&FRAME-NAME} fi-nro-contrato
              mp-entr-mat.dt-emissao = INPUT FRAME {&FRAME-NAME} fi-dt-emissao
              mp-entr-mat.dt-recebimento = INPUT FRAME {&FRAME-NAME} fi-dt-recebimento
              mp-entr-mat.peso-nf = INPUT FRAME {&FRAME-NAME} fi-peso-nf
              mp-entr-mat.procedencia = INPUT FRAME {&FRAME-NAME} fi-procedencia
              mp-entr-mat.padrao[1] = INPUT FRAME {&FRAME-NAME} fi-padrao1
              mp-entr-mat.qtd-fardos[1] = INPUT FRAME {&FRAME-NAME} fi-qtd-fardos1
              mp-entr-mat.padrao[2] = INPUT FRAME {&FRAME-NAME} fi-padrao2
              mp-entr-mat.qtd-fardos[2] = INPUT FRAME {&FRAME-NAME} fi-qtd-fardos2
              mp-entr-mat.padrao[3] = INPUT FRAME {&FRAME-NAME} fi-padrao3
              mp-entr-mat.qtd-fardos[3] = INPUT FRAME {&FRAME-NAME} fi-qtd-fardos3
              mp-entr-mat.padrao[4] = INPUT FRAME {&FRAME-NAME} fi-padrao4
              mp-entr-mat.qtd-fardos[4] = INPUT FRAME {&FRAME-NAME} fi-qtd-fardos4
              mp-entr-mat.padrao[5] = INPUT FRAME {&FRAME-NAME} fi-padrao5
              mp-entr-mat.qtd-fardos[5] = INPUT FRAME {&FRAME-NAME} fi-qtd-fardos5.
    
       ASSIGN i-qt-fardos = 0.
       FOR EACH mp-fardo OF mp-entr-mat NO-LOCK.
           DO i-ct = 1 TO EXTENT(mp-entr-mat.padrao).
              IF mp-fardo.padrao = mp-entr-mat.padrao[i-ct] THEN LEAVE.
           END.
           ASSIGN i-qt-fardos[i-ct] = i-qt-fardos[i-ct] + 1.
       END.
      
       DO i-ct = 1 TO EXTENT(mp-entr-mat.padrao).
          IF mp-entr-mat.qtd-fardos[i-ct] > i-qt-fardos[i-ct] THEN
             RUN pi-cria-fardos (INPUT mp-entr-mat.qtd-fardos[i-ct] - i-qt-fardos[i-ct],
                                 INPUT mp-entr-mat.padrao[i-ct]).
       END.
    END.

    RUN pi-etiqueta.
    
    APPLY "close":U TO THIS-PROCEDURE.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-emit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-emit w-digita
ON ENTRY OF fi-cod-emit IN FRAME F-Main /* Fornecedor */
DO:
  FIND emitente WHERE
       emitente.cod-emit = SELF:INPUT-VALUE NO-LOCK NO-ERROR.
  IF AVAIL emitente THEN
     ASSIGN fi-nome-emit:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.nome-emit.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-emit w-digita
ON LEAVE OF fi-cod-emit IN FRAME F-Main /* Fornecedor */
DO:
  FIND emitente WHERE
       emitente.cod-emit = SELF:INPUT-VALUE NO-LOCK NO-ERROR.
  IF AVAIL emitente THEN
     ASSIGN fi-nome-emit:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.nome-emit.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-emit w-digita
ON MOUSE-SELECT-DBLCLICK OF fi-cod-emit IN FRAME F-Main /* Fornecedor */
DO:
  {include/zoomvar.i &prog-zoom=adzoom/z01ad098.w
                     &campo=fi-cod-emit
                     &campozoom=cod-emitente}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-qtd-fardos1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-qtd-fardos1 w-digita
ON LEAVE OF fi-qtd-fardos1 IN FRAME F-Main
DO:
  IF fi-padrao1:SCREEN-VALUE = "" AND 
     fi-qtd-fardos1:INPUT-VALUE IN FRAME {&FRAME-NAME} > 0 THEN DO.
     MESSAGE "O padr∆o deve ser Informado . . ."
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     APPLY 'entry' TO fi-padrao1.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-qtd-fardos2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-qtd-fardos2 w-digita
ON LEAVE OF fi-qtd-fardos2 IN FRAME F-Main
DO:
  IF fi-padrao2:SCREEN-VALUE = "" AND 
     fi-qtd-fardos2:INPUT-VALUE IN FRAME {&FRAME-NAME} > 0 THEN DO.
     MESSAGE "O padr∆o deve ser Informado . . ."
              VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     APPLY 'entry' TO fi-padrao2.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-qtd-fardos3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-qtd-fardos3 w-digita
ON LEAVE OF fi-qtd-fardos3 IN FRAME F-Main
DO:
  IF fi-padrao3:SCREEN-VALUE = "" AND 
     fi-qtd-fardos3:INPUT-VALUE IN FRAME {&FRAME-NAME} > 0 THEN DO.
     MESSAGE "O padr∆o deve ser Informado . . ."
             VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     APPLY 'entry' TO fi-padrao3.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-qtd-fardos4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-qtd-fardos4 w-digita
ON LEAVE OF fi-qtd-fardos4 IN FRAME F-Main
DO:
   IF fi-padrao4:SCREEN-VALUE = "" AND 
      fi-qtd-fardos4:INPUT-VALUE IN FRAME {&FRAME-NAME} > 0 THEN DO.
      MESSAGE "O padr∆o deve ser Informado . . ."
              VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY 'entry' TO fi-padrao4.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-qtd-fardos5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-qtd-fardos5 w-digita
ON LEAVE OF fi-qtd-fardos5 IN FRAME F-Main
DO:
   IF fi-padrao5:SCREEN-VALUE = "" AND 
      fi-qtd-fardos5:INPUT-VALUE IN FRAME {&FRAME-NAME} > 0 THEN DO.
      MESSAGE "O padr∆o deve ser Informado . . ."
              VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY 'entry' TO fi-padrao5.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-digita 


/* ***************************  Main Block  *************************** */

fi-cod-emit:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY fi-placa fi-dt-entrada fi-hora fi-peso-bruto fi-cod-emit fi-nome-emit 
          fi-nro-docto fi-nro-contrato fi-dt-emissao fi-dt-recebimento 
          fi-procedencia fi-peso-nf fi-deposito fi-padrao1 fi-qtd-fardos1 
          fi-padrao2 fi-qtd-fardos2 fi-padrao3 fi-qtd-fardos3 fi-padrao4 
          fi-qtd-fardos4 fi-padrao5 fi-qtd-fardos5 
      WITH FRAME F-Main IN WINDOW w-digita.
  ENABLE RECT-1 RECT-13 RECT-14 fi-cod-emit fi-nro-docto fi-nro-contrato 
         fi-dt-emissao fi-dt-recebimento fi-procedencia fi-peso-nf fi-deposito 
         fi-padrao1 fi-qtd-fardos1 fi-padrao2 fi-qtd-fardos2 fi-padrao3 
         fi-qtd-fardos3 fi-padrao4 fi-qtd-fardos4 fi-padrao5 fi-qtd-fardos5 
         bt-ok bt-cancelar bt-ajuda 
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

  {utp/ut9000.i "ESSP0117A" "2.04.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  FIND mp-entr-cam WHERE
       ROWID(mp-entr-cam) = p-row-table NO-LOCK NO-ERROR.

  ASSIGN fi-placa:SCREEN-VALUE IN FRAME {&FRAME-NAME} = mp-entr-cam.placa
         fi-dt-entrada:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(mp-entr-cam.dt-entrada,"99/99/9999")
         fi-peso-bruto:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(mp-entr-cam.peso-bruto)
         de-peso-liquido-cam = mp-entr-cam.peso-liquido
         fi-hora:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(mp-entr-cam.hr-entrada,"HH:MM").

  ASSIGN fi-dt-recebimento:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY,"99/99/9999").

  FIND mp-entr-mat WHERE
       mp-entr-mat.nr-cdr = mp-entr-cam.nr-cdr NO-LOCK NO-ERROR.
  IF AVAIL mp-entr-mat THEN DO.
     FIND emitente WHERE
          emitente.cod-emit = mp-entr-mat.cod-emit NO-LOCK NO-ERROR.
     IF AVAIL emitente THEN
        ASSIGN fi-nome-emit:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.nome-emit.

     ASSIGN fi-cod-emit:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(mp-entr-mat.cod-emit)
            fi-nro-docto:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(mp-entr-mat.nro-docto)
            fi-nro-contrato:SCREEN-VALUE IN FRAME {&FRAME-NAME} = mp-entr-mat.nro-contrato
            fi-dt-emissao:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(mp-entr-mat.dt-emissao,"99/99/9999")
            fi-dt-recebimento:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(mp-entr-mat.dt-recebimento,"99/99/9999")
            fi-peso-nf:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(mp-entr-mat.peso-nf)
            fi-procedencia:SCREEN-VALUE IN FRAME {&FRAME-NAME} = mp-entr-mat.procedencia
            fi-padrao1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = mp-entr-mat.padrao[1]
            fi-qtd-fardos1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(mp-entr-mat.qtd-fardos[1])
            fi-padrao2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = mp-entr-mat.padrao[2]
            fi-qtd-fardos2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(mp-entr-mat.qtd-fardos[2])
            fi-padrao3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = mp-entr-mat.padrao[3]
            fi-qtd-fardos3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(mp-entr-mat.qtd-fardos[3])
            fi-padrao4:SCREEN-VALUE IN FRAME {&FRAME-NAME} = mp-entr-mat.padrao[4]
            fi-qtd-fardos4:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(mp-entr-mat.qtd-fardos[4])
            fi-padrao5:SCREEN-VALUE IN FRAME {&FRAME-NAME} = mp-entr-mat.padrao[5]
            fi-qtd-fardos5:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(mp-entr-mat.qtd-fardos[5]).

     IF mp-entr-mat.qtd-fardos[1] > 0 THEN
        ASSIGN fi-padrao1:SENSITIVE = NO
               fi-qtd-fardos1:SENSITIVE = NO.
     IF mp-entr-mat.qtd-fardos[2] > 0 THEN 
        ASSIGN fi-padrao2:SENSITIVE = NO
               fi-qtd-fardos2:SENSITIVE = NO.
     IF mp-entr-mat.qtd-fardos[3] > 0 THEN
        ASSIGN fi-padrao3:SENSITIVE = NO
               fi-qtd-fardos3:SENSITIVE = NO.
     IF mp-entr-mat.qtd-fardos[4] > 0 THEN
        ASSIGN fi-padrao4:SENSITIVE = NO
               fi-qtd-fardos4:SENSITIVE = NO.
     IF mp-entr-mat.qtd-fardos[5] > 0 THEN
        ASSIGN fi-padrao5:SENSITIVE = NO
               fi-qtd-fardos5:SENSITIVE = NO.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-fardos w-digita 
PROCEDURE pi-cria-fardos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-qtd-fardos AS INT.
    DEF INPUT PARAMETER p-padrao AS CHAR.
    DEF VAR i-fd AS INT.
    DEF VAR i-ind AS INT.

    ASSIGN i-fd = INT(SUBSTR(STRING(mp-entr-mat.dt-recebimento,"999999"),3,4) + '0001').
    FIND FIRST mp-fardo WHERE
               mp-fardo.nr-fardo = i-fd NO-LOCK NO-ERROR.

    IF NOT AVAIL mp-fardo THEN
       CURRENT-VALUE(seq-fardo) = i-fd - 1.  /* No primeiro fardo do Mes/Ano, zera sequencial */
                                             /* para o Next-Value n∆o pular o 0001 */
    DO i-ind = 1 TO p-qtd-fardos.
       CREATE mp-fardo.
       ASSIGN mp-fardo.nr-cdr       = mp-entr-mat.nr-cdr
              mp-fardo.nr-fardo     = NEXT-VALUE(seq-fardo)  /* Soma 1 no n£mero do fardo */
              mp-fardo.padrao       = p-padrao
              mp-fardo.cod-depos    = INPUT FRAME {&FRAME-NAME} fi-deposito
              mp-fardo.cd-coloracao = ?
              mp-fardo.cd-tipo      = ?
              mp-fardo.situacao     = 1.   /* Em Descarga */
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-etiqueta w-digita 
PROCEDURE pi-etiqueta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR i-lin AS INT EXTENT 6 INIT [0,570,1170,1770,2370,2930].
    DEF VAR i-col AS INT EXTENT 3 INIT [0,820,1640].

    OUTPUT TO PRINTER CONVERT TARGET "ibm850" PAGED.

    PUT UNFORMATTED 
        "~033&l26A"
        "~033&l1E".

    ASSIGN i-ct-lin = 1
           i-ct-col = 0.
    FOR EACH mp-fardo OF mp-entr-mat WHERE
             mp-fardo.situacao = 1 EXCLUSIVE-LOCK.

        ASSIGN i-ct-col = i-ct-col + 1.
        IF i-ct-col > 3 THEN DO.
           ASSIGN i-ct-col = 1
                  i-ct-lin = i-ct-lin + 1.

           IF i-ct-lin > 3 THEN DO.
              ASSIGN i-ct-lin = 1.
              PAGE.
           END.
        END.

        RUN pi-imp-etiqueta (INPUT i-lin[i-ct-lin * 2 - 1], INPUT i-col[i-ct-col]). 
        RUN pi-imp-etiqueta (INPUT i-lin[i-ct-lin * 2], INPUT i-col[i-ct-col]).

        ASSIGN mp-fardo.situacao = 2.
    END.
    OUTPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imp-etiqueta w-digita 
PROCEDURE pi-imp-etiqueta :
DEF INPUT PARAMETER i-lin AS INT.
    DEF INPUT PARAMETER i-col AS INT.

    PUT UNFORMATTED 
        fn-retangulo(INPUT i-col, INPUT i-lin, INPUT i-col + 690, INPUT i-lin + 450, INPUT 4)
        fn-linha(INPUT i-col + 340, INPUT i-lin, INPUT 135, INPUT 4, INPUT "V")
        fn-linha(INPUT i-col + 510, INPUT i-lin, INPUT 135, INPUT 4, INPUT "V")
        fn-linha(INPUT i-col, INPUT i-lin + 135, INPUT 690, INPUT 4, INPUT "H").

    PUT UNFORMATTED
        fn-code25 (INPUT i-col + 50, INPUT i-lin + 70,
                   INPUT STRING(mp-fardo.nr-fardo,"99999999"),
                   INPUT "H",
                   INPUT 1.0,
                   INPUT 3.0).

    PUT UNFORMATTED 
        fn-texto(INPUT i-col + 30, INPUT i-lin + 60, INPUT STRING(mp-fardo.nr-fardo,"99999999"),INPUT 16602, INPUT 15, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 400, INPUT i-lin + 30, INPUT "Tipo",INPUT 16602, INPUT 5, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 545, INPUT i-lin + 30, INPUT "Coloraá∆o",INPUT 16602, INPUT 5, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 10, INPUT i-lin + 160, INPUT "PADR«O",INPUT 16602, INPUT 5, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 30, INPUT i-lin + 200, INPUT mp-fardo.padrao,INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 10, INPUT i-lin + 240, INPUT "PROCED“NCIA",INPUT 16602, INPUT 5, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 30, INPUT i-lin + 280, INPUT mp-entr-mat.procedencia,INPUT 16602, INPUT 10, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 10, INPUT i-lin + 320, INPUT "FORNECEDOR",INPUT 16602, INPUT 5, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 30, INPUT i-lin + 360, INPUT STRING(mp-entr-mat.cod-emit) + " - " + emitente.nome-abrev,INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")      
        fn-texto(INPUT i-col + 10, INPUT i-lin + 400, INPUT "NOTA FISCAL",INPUT 16602, INPUT 5, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 30, INPUT i-lin + 440, INPUT STRING(mp-entr-mat.nro-docto),INPUT 16602, INPUT 10, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 410, INPUT i-lin + 400, INPUT "DATA RECEBIMENTO",INPUT 16602, INPUT 5, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 430, INPUT i-lin + 440, INPUT STRING(mp-entr-mat.dt-recebimento,"99/99/9999"),INPUT 16602, INPUT 10, INPUT 3, INPUT 0, INPUT 1, INPUT "H").
  
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

