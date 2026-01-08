/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
/*{include/i-prgvrs.i UT-GENXML 2.00.00.000}  /*** 010000 ***/*/
/*******************************************************************************
** Programa: 
** Objetivo: Fazer o tratamento basico e simplificado de XML, para leitura ou
**           geracao.
*******************************************************************************/


/******************************************************************************
 Lista de Procedure
 ==============================================================================
 
 Publicos
 --------

    reset               => Limpa o conteudo de todas as temp-tables internas.
    setEncoding         => Define o padrao de codificacao de caracteres.
    setAttribute        => "Seta" o valor de um atributo de um determinado no
                           (nodeId).
    addNode	            => Adiciona um novo no a arvore (como nome e valor).
    deleteNode          => Remove um no da arvore.
    loadXMLFromFile     => Le o XML de um arquivo e faz a extracao das infor-
                           macoes.
    loadXMLFromString   => Le o XML de uma string e faz a extracao das infor-
                           macoes.
    loadXMLFromMemptr   => Le o XML de um MEMPTR e faz a extracao das infor-
                           macoes.
    loadXML             => Le o conteudo de um documento XML (hDoc) e o armazena
                           no formato interno, padrao temp-table.
    getValue            => Retorna o valor de um no, especificado por iId.
    setValue            => Altera o valor de um no, especificado por iId.
    getTagName          => Dado a identificacao de um no (iId), retorna seu Tag.
    getChildrenList     => Retorna uma lista de Id's separados por virgula que
                           correspondem aos nos-filhos do no especificado em iId.
    searchTag           => Procura pelo primeiro no cujo tag seja igual a
                           cTagName, dentre os nos-filhos do no identificado por 
                           iId.
    loadValue           => Procura pelo primeiro no cujo tag seja igual a 
                           cTagName, dentre os nos-filhos do no identificado por
                           iId e retorna o seu valor.
    getAttrList         => Retorna a lista de atributos (nomes) de um determinado
                           no, sepa- rados por virgula.
    getAttrValue        => Retorna o valor do atributo de um no.
    generateXMLToFile   => Gera o XML em um arquivo.
    generateXMLToString => Gera o XML em uma string.
    generateXMLToMemptr => Gera o XML em um MEMPTR.
    generateXML         => Gera o documento XML com base nas informacoes exis-
                           tentes nas temp-tables internas.

    InsertXML           => Insere um XML como filho de um no.
    extractXML          => Extrai um novo XML a partir de um tag.

  Uso Interno
  -----------    

    loadNode            => Le o conteudo de um no e armazena suas informacoes nas
                           temp-tables internas. 
    generateNode        => Gera um no do documento XML com base nas informacoes 
                           das temp-tables internas.
    emptyHandle         => Elimina os objetos alocados para o parser XML sem 
                           perder a estrutura e os dados previamente inseridos.
    doCopy              => Copia o No, seus atributos e todos os filhos para a 
                           tabela de copia.
    getData             => Retorna as temp-tables internas de controle.
    putData             => Seta as temp-tables internas de controle.

  Testes
  ------    

    listTags  => Exibe toda a estrutura cadastrada nas temp-tables internas. Deve
                 ser utilizada apenas para fins de testes.
    listAttr  => Exibe todos os atributos de nos cadastrados nas temp-tables in-
                 ternas. Deve ser utilizada apenas para fins de testes.

********************************************************************************/



/*------------------------------------------------------------------------------
   Temp-table: ttStruct
   Armazena todos os nos da arvore que compoem o XML com seus respectivos nos
   e valores.
   
   <PrimeiroNo>valor do no</PrimeiroNo>
         ^          ^
       Nome       valor
------------------------------------------------------------------------------*/
DEFINE TEMP-TABLE ttStruct NO-UNDO
    FIELD id            AS INTEGER
    FIELD ParentNode    AS INTEGER
    FIELD tagName       AS CHARACTER
    FIELD nodeValue     AS CHARACTER
    FIELD x-hNode       AS HANDLE
    FIELD x-hValue      AS HANDLE
    INDEX principal IS PRIMARY UNIQUE
        id
    INDEX ParentNode
        ParentNode
    INDEX searchIndex
        parentNode
        tagName.
DEFINE TEMP-TABLE ttCopyStr NO-UNDO LIKE ttStruct.

/*-----------------------------------------------------------------------------
  Temp-table: ttAttrubute
  Armazena os atributos de um determinado no, bem como seus valores.
  
  <primeiroNo chave="0x022abd03" metodo="Insert">
                 ^      ^
             atributo  valor
-----------------------------------------------------------------------------*/
DEFINE TEMP-TABLE ttAttribute NO-UNDO
    FIELD nodeId        AS INTEGER
    FIELD AttrName      AS CHARACTER
    FIELD attrValue     AS CHARACTER
    INDEX principal IS PRIMARY
        nodeId
        attrName.
DEFINE TEMP-TABLE ttCopyAttr NO-UNDO LIKE ttAttribute.


DEFINE VARIABLE encoding    AS CHARACTER    INITIAL ?   NO-UNDO.



/*=============================================================================
  Procedure: setEncoding
   Objetivo: Define o padrao de codificacao de caracteres.
=============================================================================*/   
PROCEDURE setEncoding:
    DEFINE INPUT PARAMETER c-enc AS CHARACTER   NO-UNDO.
    encoding = c-enc.
END PROCEDURE.

/*=============================================================================
  Procedure: reset
   Objetivo: Limpa o conteudo de todas as temp-tables internas.
=============================================================================*/
PROCEDURE reset:
    /* Elimina os registros de estritura */
    FOR EACH ttStruct:
        DELETE OBJECT ttStruct.x-hnode  NO-ERROR.
        DELETE OBJECT ttstruct.x-hvalue NO-ERROR.
        DELETE ttStruct.
    END.

    /* Elimina os registros de atributos */
    FOR EACH ttAttribute:
        DELETE ttAttribute.
    END.
END.

/*=============================================================================
  Procedure: emptyHandle
   Objetivo: Elimina os objetos alocados para o parser XML sem perder a 
             estrutura e os dados previamente inseridos.
=============================================================================*/
PROCEDURE emptyhandle:
    FOR EACH ttStruct:
        DELETE OBJECT ttStruct.x-hnode NO-ERROR.
        DELETE OBJECT ttstruct.x-hvalue NO-ERROR.
    END.
END.

/*=============================================================================
  Procedure: setAttribute
   Objetivo: "Seta" o valor de um atributo de um determinado no (nodeId).
=============================================================================*/
PROCEDURE setAttribute:
    DEFINE INPUT PARAMETER nodeId       AS INTEGER      NO-UNDO.
    DEFINE INPUT PARAMETER cAttribute   AS CHARACTER    NO-UNDO.
    DEFINE INPUT PARAMETER cValue       AS CHARACTER    NO-UNDO.

    CREATE ttAttribute.
    ASSIGN
        ttAttribute.nodeId    = nodeId
        ttAttribute.AttrName  = cAttribute
        ttAttribute.attrValue = cValue.
END.

/*==============================================================================
  Procedure: addNode
   Objetivo: Adiciona um novo no a arvore (como nome e valor).
==============================================================================*/
PROCEDURE addNode:
    DEFINE  INPUT PARAMETER iParent AS INTEGER      NO-UNDO.
    DEFINE  INPUT PARAMETER cTag    AS CHARACTER    NO-UNDO.
    DEFINE  INPUT PARAMETER cValue  AS CHARACTER    NO-UNDO.
    DEFINE OUTPUT PARAMETER iId     AS INTEGER      NO-UNDO.

    FIND LAST ttStruct
        NO-LOCK NO-ERROR.
    IF NOT AVAIL ttStruct THEN
        iId = 1.
    ELSE
        iId = ttStruct.id + 1.

    CREATE ttStruct.
    ASSIGN
        ttStruct.id         = iId
        ttStruct.parentNode = iParent
        ttStruct.tagName    = cTag.

    IF cValue <> ? THEN
        ttStruct.nodeValue  = cValue.
END PROCEDURE.

/*==============================================================================
  Procedure: deleteNode
   Objetivo: Remove um no da arvore.
==============================================================================*/
PROCEDURE deleteNode:
    DEFINE INPUT PARAMETER iId     AS INTEGER      NO-UNDO.

    DEFINE BUFFER bStruct FOR ttStruct.

    /* Elimina todos os nos-filhos */
    FOR EACH bStruct
        WHERE bStruct.parentNode = iId
        NO-LOCK:
        RUN removeNode IN THIS-PROCEDURE (bStruct.id).
    END.

    FIND ttStruct
        WHERE ttStruct.id = iId
        EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL ttStruct THEN DO:
        IF VALID-HANDLE(ttStruct.x-hNode) THEN
            DELETE OBJECT ttStruct.x-hNode.
        IF VALID-HANDLE(ttStruct.x-hValue) THEN
            DELETE OBJECT ttStruct.x-hValue.
        DELETE ttStruct.
    END.

END PROCEDURE.

/*=============================================================================
  Procedure: loadXMLFromFile
   Objetivo: Le o XML de um arquivo e faz a extracao das informacoes.
=============================================================================*/   
PROCEDURE loadXMLFromFile:
    DEFINE INPUT PARAMETER cFileName        AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE hDoc                    AS HANDLE       NO-UNDO.

    cFileName = SEARCH(cFileName).

    IF cFileName = ? THEN
        RETURN.

    CREATE X-DOCUMENT hDoc.
    hDoc:LOAD("file":U, cFileName, FALSE) NO-ERROR.

    RUN loadXML IN THIS-PROCEDURE (INPUT hDoc).
END.

/*=============================================================================
  Procedure: loadXMLFromString
   Objetivo: Le o XML de uma string e faz a extracao das informacoes.
=============================================================================*/   
PROCEDURE loadXMLFromString:
    DEFINE INPUT PARAMETER cString          AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE hDoc                    AS HANDLE       NO-UNDO.
    DEFINE VARIABLE inPtr                   AS MEMPTR       NO-UNDO.

    /* pega ponteiro da string */
    ASSIGN
        SET-SIZE(inPtr)      = LENGTH(cString) + 1.
        PUT-STRING(inPtr, 1) = cString
        NO-ERROR.

    CREATE X-DOCUMENT hDoc.
    hDoc:LOAD("memptr":U,inPtr, FALSE) NO-ERROR.

    RUN loadXML IN THIS-PROCEDURE (INPUT hDoc).
END.


/*=============================================================================
  Procedure: loadXMLFromMemptr
   Objetivo: Le o XML de um memptr e faz a extracao das informacoes.
=============================================================================*/   
PROCEDURE loadXMLFromMemptr:
    DEFINE INPUT PARAMETER inPtr            AS MEMPTR       NO-UNDO.
    DEFINE VARIABLE hDoc                    AS HANDLE       NO-UNDO.

    CREATE X-DOCUMENT hDoc.
    hDoc:LOAD("memptr":U,inPtr, FALSE) NO-ERROR.

    RUN loadXML IN THIS-PROCEDURE (INPUT hDoc).
END.

/*=============================================================================
  Procedure: loadXML
   Objetivo: Le o conteudo de um documento XML (hDoc) e o armazena no formato
             interno, padrao temp-table.
==============================================================================*/
PROCEDURE loadXML:
    DEFINE INPUT PARAMETER hDoc     AS HANDLE   NO-UNDO.
    DEFINE VARIABLE hRoot       AS HANDLE       NO-UNDO.

    /* Limpa conteudo da tabela de estrutura */
    FOR EACH ttStruct
        EXCLUSIVE-LOCK:
        DELETE ttStruct.
    END.

    /* Limpa conteudo da tabela de atributos */
    FOR EACH ttAttribute
        EXCLUSIVE-LOCK:
        DELETE ttAttribute.
    END.

    /* Inicia a leitura dos nos */
    CREATE X-NODEREF hRoot.
    hDoc:GET-DOCUMENT-ELEMENT(hRoot).

    RUN loadNode(hRoot, 0).
END PROCEDURE.

/*=============================================================================
  Procedure: getValue
   Objetivo: Retorna o valor de um no, especificado por iId.
=============================================================================*/
PROCEDURE getValue:
    DEFINE  INPUT PARAMETER iId     AS INTEGER      NO-UNDO.
    DEFINE OUTPUT PARAMETER cValue  AS CHARACTER    NO-UNDO.

    FIND ttStruct
        WHERE ttStruct.id = iId
        NO-LOCK NO-ERROR.

    IF AVAIL ttStruct THEN
        cValue = ttStruct.nodeValue.
    ELSE
        cValue = ?.
END PROCEDURE.

/*=============================================================================
  Procedure: setValue
   Objetivo: Altera o valor de um no, especificado por iId.
=============================================================================*/
PROCEDURE setValue:
    DEFINE INPUT PARAMETER iId     AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER cValue  AS CHARACTER NO-UNDO.

    FIND ttStruct
        WHERE ttStruct.id = iId
        NO-LOCK NO-ERROR.

    IF AVAILABLE ttStruct AND cValue <> ? THEN
        ttStruct.nodeValue = cValue.
END PROCEDURE.

/*=============================================================================
  Procedure: getTagName
   Objetivo: Dado a identificacao de um no (iId), retorna seu Tag.
=============================================================================*/
PROCEDURE getTagName:
    DEFINE  INPUT PARAMETER iId     AS INTEGER      NO-UNDO.
    DEFINE OUTPUT PARAMETER cValue  AS CHARACTER    NO-UNDO.

    FIND ttStruct
        WHERE ttStruct.id = iId
        NO-LOCK NO-ERROR.

    IF AVAIL ttStruct THEN
        cValue = ttStruct.TagName.
    ELSE
        cValue = ?.
END PROCEDURE.

/*=============================================================================
  Procedure: getChildrenList
   Objetivo: Retorna uma lista de Id's separados por virgula que correspondem
             aos nos-filhos do no especificado em iId.
=============================================================================*/
PROCEDURE getChildrenList:
    DEFINE  INPUT PARAMETER iId     AS INTEGER      NO-UNDO.
    DEFINE OUTPUT PARAMETER cValue  AS CHARACTER    NO-UNDO.

    FOR EACH ttStruct
        WHERE ttStruct.parentNode = iId
        NO-LOCK:
        cValue = cValue + ",":U + STRING(ttStruct.id).
    END.

    IF LENGTH(cValue) > 0 THEN
        cValue = SUBSTRING(cValue, 2).
END PROCEDURE.


/*=============================================================================
  Procedure: searchTag
   Objetivo: Procura pelo primeiro no cujo tag seja igual a cTagName, dentre os
             nos-filhos do no identificado por iId.
=============================================================================*/
PROCEDURE searchTag:
    DEFINE  INPUT PARAMETER cTagName        AS CHARACTER    NO-UNDO.
    DEFINE  INPUT PARAMETER iParent         AS INTEGER      NO-UNDO.
    DEFINE OUTPUT PARAMETER iId             AS INTEGER      NO-UNDO.

    FIND FIRST ttStruct
        WHERE /*ttStruct.parentNode = iParent
          AND*/ ttStruct.tagName    = cTagName
        NO-LOCK NO-ERROR.

    IF AVAILABLE ttStruct THEN
        iId = ttStruct.id.
    ELSE
        iId = ?.
END.


/*=============================================================================
  Procedure: loadValue
   Objetivo: Procura pelo primeiro no cujo tag seja igual a cTagName, dentre os
             nos-filhos do no identificado por iId e retorna o seu valor.
=============================================================================*/
PROCEDURE loadValue:
    DEFINE INPUT  PARAMETER cTagName    AS CHARACTER    NO-UNDO.
    DEFINE INPUT  PARAMETER iParentNode AS CHARACTER    NO-UNDO.
    DEFINE OUTPUT PARAMETER cValue      AS CHARACTER    NO-UNDO.

    DEFINE VARIABLE iId                 AS INTEGER      NO-UNDO.

    RUN searchTag IN THIS-PROCEDURE (cTagName, iParentNode, OUTPUT iId).
    RUN getValue IN THIS-PROCEDURE (iId, OUTPUT cValue).
END.


/*=============================================================================
  Procedure: getAttrList
   Objetivo: Retorna a lista de atributos (nomes) de um determinado no, sepa-
             rados por virgula.
=============================================================================*/
PROCEDURE getAttrList:
    DEFINE  INPUT PARAMETER iId     AS INTEGER      NO-UNDO.
    DEFINE OUTPUT PARAMETER cValue  AS CHARACTER    NO-UNDO.

    FOR EACH ttAttribute
        WHERE ttAttribute.nodeId = iId
        NO-LOCK:
        cValue = cValue + ",":U + ttAttribute.attrName.
    END.

    IF LENGTH(cValue) > 0 THEN
        cValue = SUBSTRING(cValue, 2).
END PROCEDURE.

/*=============================================================================
  Procedure: getAttrValue
   Objetivo: Retorna o valor do atributo de um no.
=============================================================================*/
PROCEDURE getAttrValue:
    DEFINE  INPUT PARAMETER iId     AS INTEGER      NO-UNDO.
    DEFINE  INPUT PARAMETER cAttr   AS CHARACTER    NO-UNDO.
    DEFINE OUTPUT PARAMETER cValue  AS CHARACTER    NO-UNDO.

    FIND ttAttribute
        WHERE ttAttribute.nodeId    = iId
          AND ttAttribute.attrName  = cAttr
        NO-LOCK NO-ERROR.

    IF AVAIL ttAttribute THEN
        cValue = ttAttribute.attrValue.
    ELSE
        cValue = ?.
END PROCEDURE.

/*=============================================================================
  Procedure: InsertXML
   Objetivo: Insere um XML como filho de um no.
=============================================================================*/
PROCEDURE InsertXML:
    DEFINE  INPUT PARAMETER iParent     AS INTEGER      NO-UNDO.
    DEFINE  INPUT PARAMETER hObject     AS HANDLE       NO-UNDO.
    DEFINE OUTPUT PARAMETER iFirstId    AS INTEGER      NO-UNDO.

    DEFINE VARIABLE hAux    AS HANDLE   NO-UNDO.

    IF NOT VALID-HANDLE(hObject) THEN
        RETURN.

    FIND LAST ttStruct
        NO-LOCK NO-ERROR.
    IF AVAILABLE ttStruct THEN
        iFirstId = ttStruct.Id + 1.

    CASE hObject:TYPE:
        WHEN "X-DOCUMENT":U THEN DO:
            RUN utp/ut-genXML.p PERSISTENT SET hAux.
            RUN loadXML   IN hAux (INPUT hObject).
            RUN InsertXML IN THIS-PROCEDURE (iParent, hAux, OUTPUT iFirstId).
            DELETE OBJECT hAux.
        END.
        WHEN "PROCEDURE":U THEN DO:
            /* Pega os dados internos */
            RUN getData IN hObject (OUTPUT TABLE ttCopyStr,
                                    OUTPUT TABLE ttCopyAttr).

            /* Soma (iFirstId - 1) para todos os ID's */
            FOR EACH ttCopyStr
                BY id DESC:
                ASSIGN 
                    ttCopyStr.Id         = ttCopyStr.Id + iFirstId - 1.
                IF ttCopyStr.ParentNode = 0 THEN  /* Se o pai original for 0 */
                    ttCopyStr.ParentNode = iParent. /* Assume o novo pai */
                ELSE   /* Senao aplica calculo do novo Id */
                    ttCopyStr.ParentNode = ttCopyStr.ParentNode + iFirstId - 1.
            END.
            FOR EACH ttCopyAttr
                BY nodeId DESC:
                ASSIGN
                    ttCopyAttr.NodeId = ttCopyAttr.NodeId + iFirstId - 1.
            END.

            /* Transfere todos os registros das temp-tables copiadas para as
               Temp-tables internas */
            FOR EACH ttCopyStr:
                CREATE ttStruct.
                BUFFER-COPY ttCopyStr TO ttStruct.
            END.
            FOR EACH ttCopyAttr:
                CREATE ttAttribute.
                BUFFER-COPY ttCopyAttr TO ttAttribute.
            END.
        END.
    END CASE.
END.

/*=============================================================================
  Procedure: extractXML
   Objetivo: Extrai um novo XML a partir de um tag.
=============================================================================*/
PROCEDURE extractXML:
    DEFINE  INPUT PARAMETER iNode       AS INTEGER  NO-UNDO.
    DEFINE OUTPUT PARAMETER hNew        AS HANDLE   NO-UNDO.

    /* Elimina conteudo das tabelas auxiliares */
    FOR EACH ttCopyStr:  DELETE ttCopyStr.  END.
    FOR EACH ttCopyAttr: DELETE ttCopyAttr. END.

    /* Copia a hierarquia do no para area de trabalho */
    RUN doCopy IN THIS-PROCEDURE (iNode).

    /* Converte os Ids, diminuindo (iNode + 1) */
    FOR EACH ttCopyStr:
        ASSIGN
            ttCopyStr.Id = ttCopyStr.Id - iNode + 1.
        IF ttCopyStr.Id = 1 THEN
            ttCopyStr.ParentNode = 0.
        ELSE
            ttCopyStr.ParentNode = ttCopyStr.ParentNode - iNode + 1.
    END.
    FOR EACH ttCopyAttr:
        ASSIGN
            ttCopyAttr.NodeId = ttCopyAttr.NodeId - iNode + 1.
    END.

    /* Carrega a nova instancia da API */
    RUN utp/ut-genXML.p PERSISTENT SET hNew.
    RUN putData IN hNew (INPUT TABLE ttCopyStr,
                         INPUT TABLE ttCopyAttr).
END.

/*=============================================================================
  Procedure: doCopy
   Objetivo: Copia o No, seus atributos e todos os filhos para a tabela de copia.
=============================================================================*/
PROCEDURE doCopy PRIVATE:
    DEFINE INPUT PARAMETER iNode    AS INTEGER  NO-UNDO.
    FIND ttStruct
        WHERE ttStruct.Id = iNode
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE ttStruct THEN
        RETURN.

    /* Copia registro */
    CREATE ttCopyStr.
    BUFFER-COPY ttStruct TO ttCopyStr.

    /* Copia todos os atributos */
    FOR EACH ttAttribute
        WHERE ttAttribute.nodeId = iNode
        NO-LOCK:
        CREATE ttCopyAttr.
        BUFFER-COPY ttAttribute TO ttCopyAttr.
    END.

    /* Faz a copia para todos os filhos */
    FOR EACH ttStruct
        WHERE ttStruct.parentNode = iNode
        NO-LOCK:
        RUN doCopy (ttStruct.Id).
    END.
END.

/*=============================================================================
  Procedure: getData
   Objetivo: Retorna as temp-tables internas de controle.
=============================================================================*/
PROCEDURE getData:
    DEFINE OUTPUT PARAMETER TABLE FOR ttStruct.
    DEFINE OUTPUT PARAMETER TABLE FOR ttAttribute.
END.

/*=============================================================================
  Procedure: putData
   Objetivo: Seta as temp-tables internas de controle.
=============================================================================*/
PROCEDURE putData:
    DEFINE INPUT PARAMETER TABLE FOR ttStruct.
    DEFINE INPUT PARAMETER TABLE FOR ttAttribute.
END.

/*=============================================================================
  Procedure: loadNode
   Objetivo: Le o conteudo de um no e armazena suas informacoes nas temp-tables
             internas. 
=============================================================================*/
PROCEDURE loadNode PRIVATE:
    DEFINE INPUT PARAMETER hNode        AS HANDLE       NO-UNDO.
    DEFINE INPUT PARAMETER iIdParent    AS INTEGER      NO-UNDO.

    DEFINE VARIABLE iId                 AS INTEGER      NO-UNDO.
    DEFINE VARIABLE cValue              AS CHARACTER    NO-UNDO.

    DEFINE VARIABLE cAttr               AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE cAttrValue          AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE i-cont              AS INTEGER      NO-UNDO.
    DEFINE VARIABLE hChild              AS HANDLE       NO-UNDO.
    CREATE X-NODEREF hChild.

    /* Verifica o valor do elemento */
    IF hNode:NUM-CHILDREN > 0 THEN 
        DO i-cont = 1 TO hNode:NUM-CHILDREN:
            IF hNode:GET-CHILD(hChild, i-cont) AND hChild:SUBTYPE = "Text":U THEN
                cValue = hChild:NODE-VALUE.
        END.

    RUN addNode(iIdParent, 
                hNode:NAME,
                cValue,
                OUTPUT iId).

    /* Processa Atributos */
    DO i-cont = 1 TO NUM-ENTRIES(hNode:ATTRIBUTE-NAMES):
        ASSIGN
            cAttr       = ENTRY(i-cont, hNode:ATTRIBUTE-NAMES)
            cAttrValue  = hNode:GET-ATTRIBUTE(cAttr).

        RUN setAttribute (iId, cAttr, cAttrValue).
    END.

    /* Processa Filhos */
    IF hNode:NUM-CHILDREN > 0 THEN 
        DO i-cont = 1 TO hNode:NUM-CHILDREN:
            IF hNode:GET-CHILD(hChild, i-cont) AND 
               hChild:SUBTYPE = "Element":U  THEN
                RUN loadNode( hChild, iId).
        END.

END PROCEDURE.


/*=============================================================================
  Procedure: generateXMLToFile
   Objetivo: Gera o XML em um arquivo.
=============================================================================*/   
PROCEDURE generateXMLToFile:
    DEFINE INPUT PARAMETER cFile    AS CHARACTER        NO-UNDO.

    DEFINE VARIABLE hDoc            AS HANDLE           NO-UNDO.
    
    /* Libera memoria dos objetos previamente gerados */
    RUN emptyHandle.

    /* Cria novo documento XML */
    RUN generateXML (OUTPUT hDoc).
    hDoc:SAVE("file":U, cFile).

    /* Elimina objetos da memoria */
    DELETE OBJECT hDoc.
    RUN emptyHandle.
    
END.

/*=============================================================================
  Procedure: generateXMLToString
   Objetivo: Gera o XML em uma string.
=============================================================================*/   
PROCEDURE generateXMLToString:
    DEFINE OUTPUT PARAMETER cString  AS CHARACTER        NO-UNDO.

    DEFINE VARIABLE hDoc            AS HANDLE           NO-UNDO.
    DEFINE VARIABLE inPtr           AS MEMPTR           NO-UNDO.

    /* Libera memoria dos objetos previamente gerados */
    RUN emptyHandle.

    /* Cria novo documento XML */
    RUN generateXML (OUTPUT hDoc).
    hDoc:SAVE("memptr":U, inPtr).
    cString = GET-STRING(inPtr, 1).

    /* Retira o lixo que fica no final */
    cString = SUBSTRING(cString, 1, R-INDEX(cString, ">":U)).

    /* Elimina objetos da memoria */
    DELETE OBJECT hDoc.
    RUN emptyHandle.

    /* Elimina string da memoria */
    SET-SIZE(inPtr) = 0.
END.


/*=============================================================================
  Procedure: generateXMLToMemptr
   Objetivo: Gera o XML em um memptr.
=============================================================================*/   
PROCEDURE generateXMLToMemptr:
    DEFINE OUTPUT PARAMETER inPtr AS MEMPTR NO-UNDO.

    DEFINE VARIABLE hDoc          AS HANDLE NO-UNDO.

    RUN generateXML (OUTPUT hDoc).
    hDoc:SAVE("memptr":U, inPtr).
END.

/*=============================================================================
  Procedure: generateXML
   Objetivo: Gera o documento XML com base nas informacoes existentes nas temp-
             tables internas.
=============================================================================*/
PROCEDURE generateXML:
    DEFINE OUTPUT PARAMETER hDoc    AS HANDLE   NO-UNDO.

    DEFINE VARIABLE hNode   AS HANDLE   NO-UNDO.

    /* Cria documento XML */
    CREATE X-DOCUMENT hDoc.
    IF encoding <> ? THEN DO: 
        ASSIGN hDoc:Encoding = encoding.
    END.
    RUN generateNode (INPUT hDoc, INPUT 1, INPUT hDoc, OUTPUT hNode).
END PROCEDURE.


 /*=============================================================================
  Procedure: generateNode
   Objetivo: Gera um no do documento XML com base nas informacoes das temp-
             tables internas.
=============================================================================*/
PROCEDURE generateNode PRIVATE:
    DEFINE  INPUT PARAMETER hDoc         AS HANDLE   NO-UNDO.
    DEFINE  INPUT PARAMETER nodeId       AS INTEGER  NO-UNDO.
    DEFINE  INPUT PARAMETER parentNode   AS HANDLE   NO-UNDO.
    DEFINE OUTPUT PARAMETER hNode   AS HANDLE   NO-UNDO.

    DEFINE BUFFER b-ttStruct    FOR ttStruct.
    DEFINE BUFFER b-ttStructSon FOR ttStruct.

    /* DEFINE VARIABLE hNode       AS HANDLE   NO-UNDO. */
    DEFINE VARIABLE hValue      AS HANDLE   NO-UNDO.

    FIND b-ttStruct
        WHERE b-ttStruct.id = nodeId
        NO-LOCK NO-ERROR.

    IF NOT AVAIL b-ttStruct THEN
        RETURN.

    /* Cria o No com Tag*/
    CREATE X-NODEREF hNode.
    hDoc:CREATE-NODE(hNode, b-ttStruct.tagName, "Element":U).
    parentNode:APPEND-CHILD(hNode).
    ASSIGN b-ttStruct.x-hNode = hNode.

    /* Atributos */
    FOR EACH ttAttribute
        WHERE ttAttribute.nodeId = nodeId
        NO-LOCK:
        hNode:SET-ATTRIBUTE(ttAttribute.attrName, ttAttribute.attrValue).
    END.

    /* Verifica se existem nos-filhos */
    FIND FIRST b-ttStructSon
        WHERE b-ttStructSon.PARENT = nodeId
        NO-LOCK NO-ERROR.

    IF AVAILABLE b-ttStructSon THEN
        FOR EACH b-ttStructSon
            WHERE b-ttStructSon.ParentNode = nodeId
            NO-LOCK:

            RUN generateNode( hDoc, b-ttStructSon.id, hNode, OUTPUT hValue).
            hNode:APPEND-CHILD(hValue).
        END.
    ELSE IF b-ttStruct.nodeValue <> "" THEN DO:
            CREATE X-NODEREF hValue.
            hDoc:CREATE-NODE(hValue, ?, "Text":U).
            hValue:NODE-VALUE = b-ttStruct.nodeValue.
            hNode:APPEND-CHILD(hValue).
            ASSIGN b-ttStruct.x-hValue = hValue.
        END.

END PROCEDURE.


/*************/

/*=============================================================================
  Procedure: listTags
   Objetivo: Exibe toda a estrutura cadastrada nas temp-tables internas. Deve
             ser utilizada apenas para fins de testes.
=============================================================================*/
PROCEDURE listTags:
    FOR EACH ttStruct:
        DISPLAY ttstruct EXCEPT x-hNode x-hValue.
    END.
END.

/*=============================================================================
  Procedure: listAttr
   Objetivo: Exibe todos os atributos de nos cadastrados nas temp-tables in-
             ternas. Deve ser utilizada apenas para fins de testes.
=============================================================================*/
PROCEDURE listAttr:
    FOR EACH ttAttribute:
        DISPLAY ttAttribute.
    END.
END.




