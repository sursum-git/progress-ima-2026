/*******************************************************************
** Programa: epc-cd0704.p                                         **
** Objetivo: Epc do programa de cadastro de emitente(cd0704)      **
** Observ..:                                                      **  
*******************************************************************/
DEF INPUT PARAM p-ind-event   AS CHAR          NO-UNDO.
DEF INPUT PARAM p-ind-object  AS CHAR          NO-UNDO.
DEF INPUT PARAM p-wgh-object  AS HANDLE        NO-UNDO.
DEF INPUT PARAM p-wgh-frame   AS WIDGET-HANDLE NO-UNDO.
DEF INPUT PARAM p-cod-table   AS CHAR          NO-UNDO.
DEF INPUT PARAM p-row-table   AS ROWID         NO-UNDO.

/* Global Variable Definitions **********************************************/
DEFINE NEW GLOBAL SHARED VAR adm-broker-hdl  AS HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR h-container     AS HANDLE. 
DEFINE NEW GLOBAL SHARED VAR h-folder        AS HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR h_b01ad098      AS HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR h_b01es075      AS HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR h-bt-historico  AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-bt-documentos AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR h-data-implant  AS HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR c-seg-usuario   AS CHAR NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR gr-emitente     AS ROWID NO-UNDO.

/* TON 29/01/2016
DEF NEW GLOBAL SHARED VAR h-fi-ramo      AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-fi-desc-ramo AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-tx-ramo      AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR vg-cod-ramo-ativ AS INT NO-UNDO.
FIM TON 29/01/2016 */

DEF NEW GLOBAL SHARED VAR tx-coligada AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-coligada AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR c-coligada  AS CHAR.

DEF NEW GLOBAL SHARED VAR tx-classif1 AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-classif1 AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR c-classif1  AS CHAR.
DEFINE VARIABLE cListaClassif1        AS CHARACTER   NO-UNDO.

DEF NEW GLOBAL SHARED VAR tx-classif2 AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-classif2 AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR c-classif2  AS CHAR.
DEFINE VARIABLE cListaClassif2        AS CHARACTER   NO-UNDO.

DEF NEW GLOBAL SHARED VAR tx-classif3  AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-classif3  AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR c-classif13  AS CHAR.
DEFINE VARIABLE cListaClassif3         AS CHARACTER   NO-UNDO.

DEFINE VARIABLE tamanho AS INTEGER     NO-UNDO.
/* Variable Definitions *****************************************************/
DEFINE VAR h-objeto       AS WIDGET-HANDLE NO-UNDO.
DEFINE VAR wh-obj         AS WIDGET-HANDLE EXTENT 20 NO-UNDO.
DEFINE VAR i-level        AS INTEGER INITIAL 1.
DEFINE VAR c-objeto       AS CHAR NO-UNDO.
DEFINE VAR i-ct           AS INT.
DEFINE VAR c-folder       AS CHARACTER NO-UNDO.
DEFINE VAR c-objects      AS CHARACTER NO-UNDO.
DEFINE VAR h-object       AS HANDLE    NO-UNDO.
DEFINE VAR i-objects      AS INTEGER   NO-UNDO.
DEFINE VAR l-record       AS LOGICAL   NO-UNDO INITIAL NO.
DEFINE VAR l-group-assign AS LOGICAL   NO-UNDO INITIAL NO.

DEF VAR i-pag AS INTEGER.
DEF VAR c-list AS CHAR.

/* Main Block ***************************************************************/
ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:private-data, "~/").

/*
RUN CepOnline/upc/upc-cd0704.p (INPUT p-ind-event,
                                INPUT p-ind-object,
                                INPUT p-wgh-object,
                                INPUT p-wgh-frame,
                                INPUT p-cod-table,
                                INPUT p-row-table).
*/


IF p-ind-event = "CHANGE-PAGE" THEN DO:
   IF VALID-HANDLE(h-folder) THEN DO.
      RUN get-attribute-list IN h-folder ( OUTPUT c-list /* CHARACTER */).

      IF ENTRY(6,c-list,"|") = "" THEN DO.
         RUN create-folder-page IN h-folder (INPUT 6, INPUT "Exportaá∆o":U).
         RUN create-folder-label IN h-folder (INPUT 6, INPUT "Exportaá∆o":U).

         RUN disable-folder-page IN h-folder (INPUT 6).
      END.
   END.
END.

IF p-ind-event = "INITIALIZE" AND
   p-ind-object = "CONTAINER" THEN DO: 

   ASSIGN h-container = p-wgh-object
          c-coligada = ''
          c-classif1 = ''.

   /* Cria o Folder SΩcio */
   RUN get-link-handle IN adm-broker-hdl (INPUT p-wgh-object,
                                          INPUT "PAGE-SOURCE":U,
                                          OUTPUT c-folder).

   ASSIGN h-folder = WIDGET-HANDLE(c-folder) NO-ERROR.

   IF VALID-HANDLE(h-folder) THEN DO.
      RUN create-folder-page IN h-folder (INPUT 7, INPUT "Atividade":U).
      RUN create-folder-page IN h-folder (INPUT 8, INPUT "S¢cios":U).
       
      RUN create-folder-label IN h-folder (INPUT 7, INPUT "Atividade":U).
      RUN create-folder-label IN h-folder (INPUT 8, INPUT "S¢cios":U).
      
    
      RUN select-page IN p-wgh-object (INPUT 7).
      RUN init-object IN p-wgh-object (INPUT "esbrw/b01ad098.w":U, /* Nome do Objeto Viewer */
                                       INPUT p-wgh-frame,
                                       INPUT "Layout = ":U,
                                       OUTPUT h_b01ad098).

      RUN select-page IN p-wgh-object (INPUT 8).
      RUN init-object IN p-wgh-object (INPUT "esbrw/b01es075.w":U, /* Nome do Objeto Viewer */
                                       INPUT p-wgh-frame,
                                       INPUT "Layout = ":U,
                                       OUTPUT h_b01es075).


      RUN set-position IN h_b01ad098 ( 6.40, 3.00).
      RUN set-position IN h_b01es075 ( 6.40, 3.00).

      RUN get-link-handle IN adm-broker-hdl (INPUT p-wgh-object,
                                             INPUT "CONTAINER-TARGET":U,
                                             OUTPUT c-objects).
      DO i-objects = 1 TO NUM-ENTRIES(c-objects):
          ASSIGN h-object = WIDGET-HANDLE(ENTRY(i-objects, c-objects)).

          IF INDEX(h-object:PRIVATE-DATA, "qry") <> 0 AND  /* Vocº deve verificar se e a query principal */
             NOT l-record THEN DO:
             ASSIGN l-record = YES.
             RUN add-link IN adm-broker-hdl (INPUT h-object,
                                             INPUT "Record":U,
                                             INPUT h_b01ad098).

             RUN add-link IN adm-broker-hdl (INPUT h-object,
                                             INPUT "Record":U,
                                             INPUT h_b01es075).
          END.


          IF INDEX(h-object:PRIVATE-DATA, "v01") <> 0 AND /* Voce deve verificar se e a viewer principal */
             NOT l-group-assign THEN DO:
             ASSIGN l-group-assign = YES.
             RUN add-link IN adm-broker-hdl (INPUT h-object, 
                                             INPUT "Group-Assign":U,
                                             INPUT h_b01ad098).

             RUN add-link IN adm-broker-hdl (INPUT h-object, 
                                             INPUT "Group-Assign":U,
                                             INPUT h_b01es075).
          END.
      END.

      RUN dispatch IN h_b01es075 ("initialize":U).
      RUN select-page IN p-wgh-object (INPUT 8).

      RUN dispatch IN h_b01ad098 ("initialize":U).
      RUN select-page IN p-wgh-object (INPUT 7).

      RUN select-page IN p-wgh-object (INPUT 1).
   END.

   /* Encontra Botío Historico */
   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto):
      IF h-objeto:TYPE <> "field-group" THEN DO:
         IF h-objeto:NAME = "bt-historico" THEN
            ASSIGN h-bt-historico = h-objeto.
         ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
      END.
      ELSE 
         ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   END.
   
END.

IF p-ind-event  = "BEFORE-INITIALIZE"    AND 
   p-ind-object = "VIEWER"               AND 
   c-objeto     = "'advwr\v24ad098.w'"  THEN DO:
   
   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto):
      IF h-objeto:NAME = 'atividade' THEN
         h-objeto:VISIBLE = NO.

      IF h-objeto:NAME = 'data-implant' THEN 
         ASSIGN h-data-implant = h-objeto.

      ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
   END.
   
   /*ASSIGN tamanho = p-wgh-frame:HEIGHT .
   MESSAGE  tamanho
       VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
   ASSIGN   p-wgh-frame:HEIGHT  = p-wgh-frame:HEIGHT + 10.

   CREATE TEXT tx-coligada
        ASSIGN FRAME         = p-wgh-frame
               FORMAT        = "x(16)"
               WIDTH         = 15.5
               SCREEN-VALUE  = "Coligadas"
               ROW           = 2.1
               COL           = 58.8
               VISIBLE       = YES.

   RUN esapi/getOpcoesListaCB.p(16,OUTPUT cListaClassif1).
   
   CREATE TEXT tx-classif1
        ASSIGN FRAME         = p-wgh-frame
               FORMAT        = "x(16)"
               WIDTH         = 15.5
               SCREEN-VALUE  = "Classif.1"
               ROW           = 10.3
               COL           = 25
               VISIBLE       = YES.

   RUN esapi/getOpcoesListaCB.p(4,OUTPUT cListaClassif2).
   
   CREATE TEXT tx-classif2
        ASSIGN FRAME         = p-wgh-frame
               FORMAT        = "x(16)"
               WIDTH         = 15.5
               SCREEN-VALUE  = "Classif.2"
               ROW           = 10.3
               COL           = 43
               VISIBLE       = YES.

   
   RUN esapi/getOpcoesListaCB.p(5,OUTPUT cListaClassif3).

   CREATE TEXT tx-classif3
        ASSIGN FRAME         = p-wgh-frame
               FORMAT        = "x(16)"
               WIDTH         = 15.5
               SCREEN-VALUE  = "Classif.3"
               ROW           = 10.3
               COL           = 60
               VISIBLE       = YES.

   
   CREATE COMBO-BOX  wh-classif1
        ASSIGN FRAME              = p-wgh-frame
               ROW                = 11
               col                = 25
               WIDTH              = 15
               INNER-LINES        = 10
               VISIBLE            = YES
               SENSITIVE          = NO
               LIST-ITEM-PAIRS    = cListaClassif1 
               HELP               = "Informe a Classificaá∆o 1".

   CREATE COMBO-BOX  wh-classif2
        ASSIGN FRAME              = p-wgh-frame
               ROW                = 11
               col                = 43
               WIDTH              = 15
               INNER-LINES        = 10 
               VISIBLE            = YES
               SENSITIVE          = NO
               LIST-ITEM-PAIRS    = cListaClassif2
               HELP               = "Informe a Classificaá∆o 2".

   CREATE COMBO-BOX  wh-classif3
        ASSIGN FRAME              = p-wgh-frame
               ROW                = 11
               col                = 60
               WIDTH              = 15
               INNER-LINES        = 10
               VISIBLE            = YES
               SENSITIVE          = NO
               LIST-ITEM-PAIRS    = cListaClassif3
               HELP               = "Informe a Classificaá∆o 3".


   CREATE EDITOR wh-coligada
        ASSIGN FRAME              = p-wgh-frame
               WIDTH              = 25
               HEIGHT             = 3.2
               ROW                = 2.7
               COL                = 58.8
               SCROLLBAR-VERTICAL = YES
               VISIBLE            = YES
               SENSITIVE          = NO
               HELP               = "Informe as Coligadas Separadas por V°rgula (,)"
               TRIGGERS:
                   ON "MOUSE-SELECT-DBLCLICK":U PERSISTENT RUN esepc/epc-cd0704z.p.
                   ON "LEAVE":U PERSISTENT RUN esepc/epc-cd0704l1.p.
                   ON "ENTRY":U PERSISTENT RUN esepc/epc-cd0704e1.p.
               END TRIGGERS.

    wh-coligada:LOAD-MOUSE-POINTER("image/lupa.cur").
END.


IF p-ind-event = "ADD" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "'advwr\v24ad098.w'" THEN DO: 
   CREATE TEXT tx-coligada
          ASSIGN FRAME         = p-wgh-frame
                 FORMAT        = "x(16)"
                 WIDTH         = 15.5
                 SCREEN-VALUE  = "Coligadas"
                 ROW           = 2.1
                 COL           = 58.8
                 VISIBLE       = YES.
END.

IF p-ind-event = "ENABLE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "'advwr\v24ad098.w'" THEN DO: 
   ASSIGN wh-coligada:SENSITIVE = YES
          wh-classif1:SENSITIVE = YES
          wh-classif2:SENSITIVE = YES
          wh-classif3:SENSITIVE = YES.
END.

IF p-ind-event = "DISABLE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "'advwr\v24ad098.w'" THEN DO: 
   ASSIGN wh-coligada:SENSITIVE = NO
          wh-classif1:SENSITIVE = NO
          wh-classif2:SENSITIVE = NO
          wh-classif3:SENSITIVE = NO.
END.

IF p-ind-event  = 'AFTER-ENABLE'     AND
   c-objeto     = "'advwr\v24ad098.w'"  THEN DO:
   ASSIGN h-data-implant:SENSITIVE = NO.
END.

IF p-ind-event = "DISPLAY" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "'advwr\v24ad098.w'" THEN DO:
   FIND emitente WHERE
        ROWID(emitente) = p-row-table NO-LOCK NO-ERROR.

   IF VALID-HANDLE(h-bt-historico) THEN
      ASSIGN h-bt-historico:SENSITIVE = emitente.ind-cre-cli <> 4.

   ASSIGN gr-emitente = ROWID(emitente).

   FIND ext-emitente WHERE 
        ext-emitente.cod-emitente = emitente.cod-emitente SHARE-LOCK NO-ERROR.

   ASSIGN wh-coligada:SCREEN-VALUE = ''.
   IF AVAIL ext-emitente THEN
      ASSIGN wh-coligada:SCREEN-VALUE = ext-emitente.coligada
             wh-classif1:SCREEN-VALUE = string(ext-emitente.classif_01)
             wh-classif2:SCREEN-VALUE = string(ext-emitente.classif_02)
             wh-classif3:SCREEN-VALUE = string(ext-emitente.classif_03).
END.

IF p-ind-event = "VALIDATE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "'advwr\v24ad098.w'" THEN DO: 

   IF wh-coligada:SCREEN-VALUE <> '' THEN DO.
      DO i-ct = 1 TO NUM-ENTRIES(wh-coligada:SCREEN-VALUE).
         FIND emitente WHERE
              emitente.nome-abrev = ENTRY(i-ct,wh-coligada:SCREEN-VALUE)
              NO-LOCK NO-ERROR.
         IF NOT AVAIL emitente THEN DO.
            FIND emitente WHERE
                 emitente.cod-emitente = INTEGER(ENTRY(i-ct,wh-coligada:SCREEN-VALUE))
                 NO-LOCK NO-ERROR.
            IF NOT AVAIL emitente THEN DO.
               MESSAGE 'Empresa Coligada ' ENTRY(i-ct,wh-coligada:SCREEN-VALUE) ' n∆o Cadastrada...'
                     VIEW-AS ALERT-BOX ERROR BUTTONS OK.
               APPLY 'ENTRY' TO wh-coligada.
               RETURN 'NOK'.
            END.
         END.
      END.
   END.
   
   IF wh-classif1:SCREEN-VALUE = ? OR wh-classif1:SCREEN-VALUE = '0' THEN DO:
      MESSAGE 'ê necess†rio informar uma classificaá∆o 1 para o cliente'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'entry' TO wh-classif1.
      RETURN 'nok'.
   END.
   IF wh-classif2:SCREEN-VALUE = ? OR wh-classif2:SCREEN-VALUE = '0' THEN DO:
      MESSAGE 'ê necess†rio informar uma classificaá∆o 2 para o cliente'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'entry' TO wh-classif2.
      RETURN 'nok'.
   END.
   IF wh-classif3:SCREEN-VALUE = ? OR wh-classif3:SCREEN-VALUE = '0' THEN DO:
      MESSAGE 'ê necess†rio informar uma classificaá∆o 3 para o cliente'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'entry' TO wh-classif3.
      RETURN 'nok'.
   END.
END.

IF p-ind-event  = 'ASSIGN'     AND
   c-objeto     = "'advwr\v24ad098.w'"  THEN DO:
   
   FIND emitente WHERE 
        ROWID(emitente) = p-row-table SHARE-LOCK NO-ERROR.
   
   FIND ext-emitente WHERE 
        emitente.cod-emitente = ext-emitente.cod-emitente SHARE-LOCK NO-ERROR.
   
   IF NOT AVAIL ext-emitente THEN DO:
      CREATE ext-emitente.
      ASSIGN ext-emitente.cod-emitente = emitente.cod-emitente.
   END.
   ASSIGN ext-emitente.coligada = wh-coligada:SCREEN-VALUE
          ext-emitente.classif_01 = int(wh-classif1:SCREEN-VALUE)
          ext-emitente.classif_02 = int(wh-classif2:SCREEN-VALUE)
          ext-emitente.classif_03 = int(wh-classif3:SCREEN-VALUE).

   ASSIGN gr-emitente = ROWID(emitente).
END.

