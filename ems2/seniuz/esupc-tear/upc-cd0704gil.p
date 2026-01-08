/****************************************************************************
** Programa: upc-cd0704.p 
** Objetivo: Criar o folder Socios, que  gravar† na  tabela SOCIO-EMIT, os
**           s¢cios desta empresa.Esta Rotina ir† procurar nesta tabela se
**           um s¢cio ja Ç socio de outra empresa.
**
**           Quando cadastrar ENDEREÄO COBRANÄA ou altera-lo, uma rotina ir†
**           verificar se existe um fornecedor para este ENDEREÄO.
**
**           Quando cadastrar/alterar a UPC ira solicitar que informado seja
**           informado o TIPO DO CLIENTE: (Normal/Especial)
**
** Autor   : FµBIO COELHO LANZA - ABRIL/2007 
*****************************************************************************/
/* Parameter Definitions ****************************************************/
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER.
DEFINE INPUT PARAMETER p-row-table  AS ROWID.

/* Global Variable Definitions **********************************************/
DEFINE NEW GLOBAL SHARED VAR adm-broker-hdl AS HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR h-container    AS HANDLE. 
DEFINE NEW GLOBAL SHARED VAR h-folder       AS HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR h_b01es075     AS HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR h-bt-historico AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-bt-documentos  AS WIDGET-HANDLE NO-UNDO.

/* Variaveis para o CEPONLINE */
DEFINE NEW GLOBAL SHARED VAR h-cep          AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-endereco     AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-bairro       AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-cidade       AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-estado       AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-cep-cob      AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-endereco-cob AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-bairro-cob   AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-cidade-cob   AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-estado-cob   AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-e-mail       AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-natureza     AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-atividade    AS HANDLE.

/* Variable Definitions *****************************************************/
DEFINE VAR i-ct           AS INT.
DEFINE VAR c-folder       AS CHARACTER NO-UNDO.
DEFINE VAR c-objects      AS CHARACTER NO-UNDO.
DEFINE VAR h-object       AS HANDLE    NO-UNDO.
DEFINE VAR i-objects      AS INTEGER   NO-UNDO.
DEFINE VAR l-record       AS LOGICAL   NO-UNDO INITIAL NO.
DEFINE VAR l-group-assign AS LOGICAL   NO-UNDO INITIAL NO.
DEFINE VAR h-objeto       AS WIDGET-HANDLE NO-UNDO.
DEFINE VAR c-objeto       AS CHAR NO-UNDO.

/* Variavies criadas na viewer dinamicamente*********************************/
ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:PRIVATE-DATA, "~/").
DEF NEW GLOBAL SHARED VAR tx-tp-cliente AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-tp-cliente AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR tx-cuit AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-cuit AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR c-tipos AS CHAR NO-UNDO.
DEF NEW GLOBAL SHARED VAR gr-emitente AS ROWID   NO-UNDO.

/* Main Block ***************************************************************/
/*
MESSAGE "p-wgh-frame  " p-wgh-frame         SKIP
        "p-ind-event  " p-ind-event         SKIP
        "p-ind-object " p-ind-object        SKIP
        "p-cod-table  " STRING(p-cod-table) SKIP 
        "p-row-table  " STRING(P-row-table)    SKIP
        "c-objeto     " c-objeto               SKIP
        "p-wgh-object " p-wgh-object:FILE-NAME SKIP
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
*/

IF p-ind-event = "BEFORE-INITIALIZE" AND
   c-objeto = "'advwr\v27ad098.w'" THEN DO: 
   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto):
      IF h-objeto:TYPE <> "field-group" THEN DO:
         IF h-objeto:NAME = "cb-natureza" THEN
            ASSIGN h-natureza = h-objeto.  
         
         ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
      END.
      ELSE 
         ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   END.
END.

IF p-ind-event = "BEFORE-INITIALIZE" AND
   c-objeto = "'advwr\v29ad098.w'" THEN DO: 
   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto):
      IF h-objeto:TYPE <> "field-group" THEN DO:
         IF h-objeto:NAME = "e-mail" THEN DO:
            ASSIGN h-objeto:FORMAT = "x(200)".  
            ASSIGN h-e-mail = h-objeto.  
            ON 'leave':U OF h-objeto PERSISTENT RUN esupc/upc-cd0704g.p.
         END.
         ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
      END.
      ELSE 
         ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   END.
END.

IF p-ind-event = "BEFORE-INITIALIZE" AND
   c-objeto = "'advwr\v24ad098.w'" THEN DO: 
   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto):
      IF h-objeto:TYPE <> "field-group" THEN DO:
         IF h-objeto:NAME = "atividade" THEN
            ASSIGN h-atividade = h-objeto.  

         ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
      END.
      ELSE 
         ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   END.
END.

IF p-ind-event = "BEFORE-INITIALIZE" AND
   c-objeto = "'advwr\v28ad098.w'" THEN DO: /* Localiza endereáo-cob e chama procedimento */

   ON 'entry':U OF p-wgh-frame PERSISTENT RUN esupc/upc-cd0704e1.p.

   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto):
      IF h-objeto:TYPE <> "field-group" THEN DO:
         CASE h-objeto:NAME.
             WHEN "endereco" THEN ASSIGN h-endereco = h-objeto.
             WHEN "bairro"   THEN ASSIGN h-bairro = h-objeto.
             WHEN "cidade"   THEN ASSIGN h-cidade = h-objeto.
             WHEN "estado"   THEN ASSIGN h-estado = h-objeto.
             WHEN "cep" THEN DO.
                ASSIGN h-cep = h-objeto.
                ON 'leave':U OF h-objeto PERSISTENT RUN esupc/upc-cd0704a.p (INPUT h-cep,
                                                                             INPUT h-endereco,
                                                                             INPUT h-bairro,
                                                                             INPUT h-cidade,
                                                                             INPUT h-estado).  
             END.
             WHEN "endereco-cob" THEN DO:
                ASSIGN h-endereco-cob = h-objeto.
                ON 'leave':U OF h-objeto PERSISTENT RUN esupc/upc-cd0704l1.p.
             END.
             WHEN "bairro-cob"   THEN ASSIGN h-bairro-cob = h-objeto.
             WHEN "cidade-cob"   THEN ASSIGN h-cidade-cob = h-objeto.
             WHEN "estado-cob"   THEN ASSIGN h-estado-cob = h-objeto.
             WHEN "cep-cob" THEN DO.
                ASSIGN h-cep-cob = h-objeto.
                ON 'leave':U OF h-objeto PERSISTENT RUN esupc/upc-cd0704a.p (INPUT h-cep-cob,
                                                                             INPUT h-endereco-cob,
                                                                             INPUT h-bairro-cob,
                                                                             INPUT h-cidade-cob,
                                                                             INPUT h-estado-cob).  
             END.
         END CASE.

         ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
      END.
      ELSE 
         ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   END.
END.

IF p-ind-event = "INITIALIZE" AND
   p-ind-object = "CONTAINER" THEN DO: 

   CREATE BUTTON h-bt-documentos
          ASSIGN FRAME = p-wgh-frame
                 WIDTH        = 5
                 HEIGHT       = 1.22
                 ROW          = 1.31
                 COL          = 65.3
                 VISIBLE      = YES
                 SENSITIVE    = YES
                 TOOLTIP      = "Documentos Digitalizados do Cliente"
                 TRIGGERS:
                     ON "CHOOSE":U PERSISTENT RUN esapi/doctos-digitalizados.p.
                END TRIGGERS.

   h-bt-documentos:LOAD-IMAGE("image/im-docs.bmp").

   /* Encontra Bot∆o Historico */
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

   /* Cria o Folder S¢cio */
   ASSIGN h-container = p-wgh-object.
   RUN get-link-handle IN adm-broker-hdl (INPUT p-wgh-object,
                                          INPUT "PAGE-SOURCE":U,
                                          OUTPUT c-folder).
   ASSIGN h-folder = WIDGET-HANDLE(c-folder) NO-ERROR.
   IF VALID-HANDLE(h-folder) THEN DO:                     
      RUN create-folder-page IN h-folder (INPUT 7, INPUT "S¢cios":U).
      RUN create-folder-label IN h-folder (INPUT 7, INPUT "S¢cios":U).

      RUN select-page IN p-wgh-object (INPUT 7).
      RUN init-object IN p-wgh-object (INPUT "esbrw/b01es075.w":U, /* Nome do Objeto Viewer */
                                       INPUT p-wgh-frame,
                                       INPUT "Layout = ":U,
                                       OUTPUT h_b01es075).

      RUN set-position IN h_b01es075 ( 6.40, 3.00).
      RUN get-link-handle IN adm-broker-hdl (INPUT p-wgh-object,
                                             INPUT "CONTAINER-TARGET":U,
                                             OUTPUT c-objects).
      DO i-objects = 1 TO NUM-ENTRIES(c-objects):
         ASSIGN h-object = WIDGET-HANDLE(entry(i-objects, c-objects)).
         IF INDEX(h-object:PRIVATE-DATA, "qry") <> 0 AND  /* Vocà deve verificar se e a query principal */
            NOT l-record THEN DO:
            ASSIGN l-record = YES.
            RUN add-link IN adm-broker-hdl (INPUT h-object,
                                            INPUT "Record":U,
                                            INPUT h_b01es075).
         END.
         IF INDEX(h-object:PRIVATE-DATA, "v01") <> 0 AND /* Voce deve verificar se e a viewer principal */
            NOT l-group-assign THEN DO:
            ASSIGN l-group-assign = YES.
            RUN add-link IN adm-broker-hdl (INPUT h-object, 
                                            INPUT "Group-Assign":U,
                                            INPUT h_b01es075).
         END.
      END.
      RUN dispatch IN h_b01es075 ("initialize":U).
      RUN select-page IN p-wgh-object (INPUT 1).
   END. 
   
END.

/****************************************************************************
** Rotina de Gravaá∆o/Alteraá∆o do TIPO DO CLIENTE 
*****************************************************************************/

IF p-ind-event = "BEFORE-INITIALIZE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "'advwr\v24ad098.w'" THEN DO:

   CREATE TEXT tx-tp-cliente
        ASSIGN FRAME         = p-wgh-frame
               FORMAT        = "x(16)"
               WIDTH         = 15.5
               SCREEN-VALUE  = "Tipo Cliente:"
               ROW           = 3.1
               COL           = 59.8
               VISIBLE       = YES.

   ASSIGN c-tipos = "".
   {esinc/i-dsallrb.i emitente-ext.tipo-cliente c-tipos}

   CREATE COMBO-BOX wh-tp-cliente
        ASSIGN FRAME             = p-wgh-frame
               SIDE-LABEL-HANDLE = tx-tp-cliente:HANDLE /* Inserir Label na Inclus∆o */
               LABEL             = "Tipo Cliente:"      /* Inserir Label na Inclus∆o */
               ROW        = 3
               COL        = 68.8
               LIST-ITEMS = c-tipos 
               VISIBLE    = YES
               SENSITIVE  = NO.
END.

IF p-ind-event = "BEFORE-INITIALIZE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "'advwr\v27ad098.w'" THEN DO:

   CREATE TEXT tx-cuit
          ASSIGN frame        = p-wgh-frame
                 format       = "x(5)"
                 width        = 14
                 screen-value = "Cuit:"
                 ROW          = 2.40
                 COL          = 55.8
                 VISIBLE      = YES.
   CREATE FILL-IN wh-cuit
          ASSIGN frame              = p-wgh-frame
                 side-label-handle  = tx-cuit:HANDLE 
                 DATA-TYPE          = "CHARACTER" 
                 width              = 16
                 height             = 0.88
                 row                = 2.30
                 col                = 58.8  
                 label              = "Cuit:" 
                 visible            = YES
                 sensitive          = no.

END.

IF p-ind-event = "ENABLE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "'advwr\v24ad098.w'" THEN DO: 
   ASSIGN wh-tp-cliente:SENSITIVE = YES.
END.

IF p-ind-event = "ENABLE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "'advwr\v27ad098.w'" THEN DO: 
   ASSIGN wh-cuit:SENSITIVE = YES.
END.

IF p-ind-event = "DISABLE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "'advwr\v24ad098.w'" THEN DO: 
   ASSIGN wh-tp-cliente:SENSITIVE = NO .
END.

IF p-ind-event = "DISABLE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "'advwr\v27ad098.w'" THEN DO: 
   ASSIGN wh-cuit:SENSITIVE = NO .
END.


IF p-ind-event = "ADD" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "'advwr\v24ad098.w'" THEN DO: 
   ASSIGN wh-tp-cliente:SCREEN-VALUE = "Normal"
          wh-tp-cliente:LABEL = "Tipo Cliente:".
END.

IF p-ind-event = "ADD" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "'advwr\v27ad098.w'" THEN DO: 
   ASSIGN wh-cuit:SCREEN-VALUE = "".
END.


IF p-ind-event = "DISPLAY" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "'advwr\v24ad098.w'" THEN DO:
   FIND emitente WHERE
        ROWID(emitente) = p-row-table NO-LOCK NO-ERROR.
   FIND emitente-ext WHERE
        emitente-ext.cod-emitente = emitente.cod-emitente NO-LOCK NO-ERROR.
   IF AVAIL emitente-ext THEN
      ASSIGN wh-tp-cliente:SCREEN-VALUE = ENTRY(emitente-ext.tipo-cliente,c-tipos).

   IF VALID-HANDLE(h-bt-historico) THEN
      ASSIGN h-bt-historico:SENSITIVE = emitente.ind-cre-cli <> 4.

   ASSIGN gr-emitente = ROWID(emitente).
END.

IF p-ind-event = "DISPLAY" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "'advwr\v27ad098.w'" THEN DO:
   FIND emitente WHERE
        ROWID(emitente) = p-row-table NO-LOCK NO-ERROR.
   FIND emitente-ext WHERE
        emitente-ext.cod-emitente = emitente.cod-emitente NO-LOCK NO-ERROR.
   ASSIGN wh-cuit:SCREEN-VALUE = "".
   IF AVAIL emitente-ext THEN
      ASSIGN wh-cuit:SCREEN-VALUE = emitente-ext.cuit.
END.

IF p-ind-event = "VALIDATE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "'advwr\v24ad098.w'" THEN DO:
   IF LOOKUP(wh-tp-cliente:SCREEN-VALUE,c-tipos)= 0 THEN DO. 
      MESSAGE "O Tipo do Cliente deve ser 'Normal' ou 'Especial'" VIEW-AS ALERT-BOX.
      RUN select-page IN h-container (INPUT 1). 
      APPLY 'entry' TO wh-tp-cliente.
      RETURN 'NOK'.
   END.
END.

IF p-ind-event = "ASSIGN" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "'advwr\v24ad098.w'" THEN DO:
   FIND emitente WHERE
        ROWID(emitente) = p-row-table NO-LOCK NO-ERROR.
   FIND emitente-ext WHERE
        emitente-ext.cod-emitente = emitente.cod-emitente NO-ERROR.
   IF NOT AVAIL emitente-ext THEN DO:
      CREATE emitente-ext.
      ASSIGN emitente-ext.cod-emitente = emitente.cod-emitente.
   END.
   ASSIGN emitente-ext.tipo-cliente = LOOKUP(wh-tp-cliente:SCREEN-VALUE,c-tipos).
END.

IF p-ind-event = "ASSIGN" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "'advwr\v27ad098.w'" THEN DO:

   FIND emitente WHERE
        ROWID(emitente) = p-row-table NO-LOCK NO-ERROR.
   FIND emitente-ext WHERE
        emitente-ext.cod-emitente = emitente.cod-emitente NO-ERROR.
   IF NOT AVAIL emitente-ext THEN DO:
      CREATE emitente-ext.
      ASSIGN emitente-ext.cod-emitente = emitente.cod-emitente.
   END.
   ASSIGN emitente-ext.cuit = wh-cuit:SCREEN-VALUE.
   
END.
