DEFINE INPUT  PARAMETER p-servico   AS INTEGER.
DEFINE INPUT  PARAMETER p-codigo    AS CHARACTER.
DEFINE INPUT  PARAMETER p-recursivo AS LOGICAL.
DEFINE OUTPUT PARAMETER p-retorno   AS LOGICAL.

DEFINE VARIABLE hWebService             AS HANDLE               NO-UNDO.
DEFINE VARIABLE hConsultaValorPortType  AS HANDLE               NO-UNDO.
DEFINE VARIABLE ConsultaValorResponse   AS LONGCHAR             NO-UNDO.

DEFINE VARIABLE c-webservice-nf         AS CHARACTER INITIAL "" NO-UNDO.
DEFINE VARIABLE c-webservice-req        AS CHARACTER INITIAL "" NO-UNDO.
DEFINE VARIABLE c-webservice-not        AS CHARACTER INITIAL "" NO-UNDO.

DEFINE VARIABLE c-ambiente              AS CHARACTER            NO-UNDO.

DEFINE VARIABLE i-quant                 AS INTEGER              NO-UNDO.

DEFINE VARIABLE lg-ret                  AS LOGICAL              NO-UNDO.

DEFINE VARIABLE c-hora                  AS CHARACTER            NO-UNDO.

IF c-ambiente = "Teste" THEN
    ASSIGN c-webservice-nf  = "-WSDL 'http://172.19.203.112:57772/csp/logistico/lhp.logistico.bs.RecIdNotaFiscalEMS.cls?WSDL=1'"
           c-webservice-req = "-WSDL 'http://172.19.203.112:57772/csp/logistico/lhp.logistico.bs.RecRequisicaoProdEMS.cls?WSDL=1'"
           c-webservice-not = "-WSDL 'http://172.19.203.112:57772/csp/logistico/lhp.logistico.bs.RecNotificacaoEMS.cls?WSDL=1'".

IF c-ambiente = "Producao" THEN
    ASSIGN c-webservice-nf  = "-WSDL 'http://172.19.203.110:57772/csp/logistico/lhp.logistico.bs.RecIdNotaFiscalEMS.cls?WSDL=1'"
           c-webservice-req = "-WSDL 'http://172.19.203.110:57772/csp/logistico/lhp.logistico.bs.RecRequisicaoProdEMS.cls?WSDL=1'"
           c-webservice-not = "-WSDL 'http://172.19.203.110:57772/csp/logistico/lhp.logistico.bs.RecNotificacaoEMS.cls?WSDL=1'".

IF c-ambiente = "Homologacao" THEN
    ASSIGN c-webservice-nf  = "-WSDL 'http://172.19.203.112:57772/csp/logistico_producao/lhp.logistico.bs.RecIdNotaFiscalEMS.cls?WSDL=1'"
           c-webservice-req = "-WSDL 'http://172.19.203.112:57772/csp/logistico_producao/lhp.logistico.bs.RecRequisicaoProdEMS.cls?WSDL=1'"
           c-webservice-not = "-WSDL 'http://172.19.203.112:57772/csp/logistico_producao/lhp.logistico.bs.RecNotificacaoEMS.cls?WSDL=1'".

IF c-webservice-nf = "" THEN DO:
    ASSIGN p-retorno = NO.
    RETURN.
END.


/*
 *  http://172.19.203.112:57772/csp/logistico_producao/lhp.logistico.bs.RecIdNotaFiscalEMS.cls?WSDL=1
 *  http://172.19.203.112:57772/csp/logistico_producao/lhp.logistico.bs.RecRequisicaoProdEMS.cls?WSDL=1
 *  http://172.19.203.112:57772/csp/logistico_producao/lhp.logistico.bs.RecNotificacaoEMS.cls?WSDL=1
 *
*/

ASSIGN p-retorno = NO.

/*
 *  p-servico:
 *
 *  1: Envio de notas de compra
 *  2: Envio de requisi»„es
 *  3; Envio de notifica»„es
 *
 */


/*  Pausa necessÿria para que os dados sejam efetivamente gravados na tabela  */
/* PAUSE 5 NO-MESSAGE.  */

CASE p-servico:
    WHEN 1 THEN DO:
        FIND FIRST il-compra
            WHERE il-compra.chave-nota = p-codigo
            SHARE-LOCK NO-ERROR.

        IF NOT AVAIL il-compra THEN DO:
            ASSIGN c-hora = STRING(TIME + 10, "HH:MM:SS").

            DO WHILE NOT AVAIL il-compra AND STRING(TIME, "HH:MM:SS") <= c-hora:
                FIND FIRST il-compra
                    WHERE il-compra.chave-nota = p-codigo
                    SHARE-LOCK NO-ERROR.
            END.
        END.

        IF NOT AVAIL il-compra THEN DO:
            MESSAGE "Registro de compra n’o estÿ dispon­vel para enviar ao Ensemble"
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            RETURN.
        END.

        CREATE SERVER hWebService.
        hWebService:CONNECT(c-webservice-nf) NO-ERROR.

        IF VALID-HANDLE(hWebService) THEN DO:
            RUN RecIdNotaFiscalEMSSoap SET hConsultaValorPortType ON hWebService NO-ERROR.

            IF VALID-HANDLE(hConsultaValorPortType) THEN DO:
                RUN RecIdNotaFiscalEMS IN hConsultaValorPortType(INPUT p-codigo, OUTPUT p-retorno) NO-ERROR.
                DELETE procedure hConsultaValorPortType NO-ERROR.
            END.

            hWebService:DISCONNECT() NO-ERROR.
            DELETE OBJECT hWebService NO-ERROR.
        END.

        FIND FIRST il-compra
            WHERE il-compra.chave-nota = p-codigo
            SHARE-LOCK NO-ERROR.

        IF AVAIL il-compra THEN DO:
            ASSIGN il-compra.envio = IF p-retorno THEN 0 ELSE il-compra.envio + 1.
            
            ASSIGN i-quant = 0.
            FOR EACH il-compra-item
                WHERE il-compra-item.chave-nota = p-codigo
                NO-LOCK:
                
                ASSIGN i-quant = i-quant + 1.
            END.
            
            ASSIGN il-compra.qt-item = i-quant.
        END.

        IF p-recursivo = NO THEN DO:
            FOR EACH il-compra
                WHERE il-compra.envio > 0
                NO-LOCK:

                RUN ilapi\avisa-ensemble.p (INPUT  p-servico,
                                            INPUT  il-compra.chave-nota,
                                            INPUT  YES,
                                            OUTPUT lg-ret).
            END.
        END.
    END.

    WHEN 2 THEN DO:
        FIND FIRST il-req
            WHERE il-req.codigo = p-codigo
            SHARE-LOCK NO-ERROR.

        IF NOT AVAIL il-req THEN DO:
            ASSIGN c-hora = STRING(TIME + 10, "HH:MM:SS").

            DO WHILE NOT AVAIL il-req AND STRING(TIME, "HH:MM:SS") <= c-hora:
                FIND FIRST il-req
                    WHERE il-req.codigo = p-codigo
                    SHARE-LOCK NO-ERROR.
            END.
        END.

        IF NOT AVAIL il-req THEN DO:
            IF SUBSTRING(il-req.codigo, 1, 1) = "P" THEN
                MESSAGE "Registro do Pedido de Venda n’o estÿ dispon­vel para enviar ao Ensemble"
                    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            ELSE
                MESSAGE "Registro da Reqisi»’o n’o estÿ dispon­vel para enviar ao Ensemble"
                    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            RETURN.
        END.
        
        ASSIGN i-quant = 0.
        FOR EACH il-req-item
            WHERE il-req-item.codigo = p-codigo
            NO-LOCK:
            
            ASSIGN i-quant = i-quant + 1.
        END.
        
        FIND FIRST il-req
            WHERE il-req.codigo = p-codigo
            SHARE-LOCK NO-ERROR.
        
        ASSIGN il-req.qt-item = i-quant.
        

        CREATE SERVER hWebService.
        hWebService:CONNECT(c-webservice-req) NO-ERROR.

        IF VALID-HANDLE(hWebService) THEN DO:
            RUN RecRequisicaoProdEMSSoap SET hConsultaValorPortType ON hWebService NO-ERROR.

            IF VALID-HANDLE(hConsultaValorPortType) THEN DO:
                RUN RecRequisicaoProdEMS IN hConsultaValorPortType(INPUT p-codigo, OUTPUT p-retorno) NO-ERROR.
                DELETE procedure hConsultaValorPortType NO-ERROR.
            END.

            hWebService:DISCONNECT() NO-ERROR.
            DELETE OBJECT hWebService NO-ERROR.
        END.

        IF AVAIL il-req THEN
            ASSIGN il-req.envio = IF p-retorno THEN 0 ELSE il-req.envio + 1.

        IF p-recursivo = NO THEN DO:
            FOR EACH il-req
                WHERE il-req.envio > 0
                NO-LOCK:

                RUN ilapi\avisa-ensemble.p (INPUT  p-servico,
                                            INPUT  il-req.codigo,
                                            INPUT  YES,
                                            OUTPUT lg-ret).
            END.
        END.
    END.

    WHEN 3 THEN DO:
        FIND FIRST il-erro-receb
            WHERE il-erro-receb.id-trans = INTEGER(p-codigo)
            SHARE-LOCK NO-ERROR.

        IF NOT AVAIL il-erro-receb THEN DO:
            ASSIGN c-hora = STRING(TIME + 10, "HH:MM:SS").

            DO WHILE NOT AVAIL il-erro-receb AND STRING(TIME, "HH:MM:SS") <= c-hora:
                FIND FIRST il-erro-receb
                    WHERE il-erro-receb.id-trans = INTEGER(p-codigo)
                    SHARE-LOCK NO-ERROR.
            END.
        END.

        IF NOT AVAIL il-erro-receb THEN DO:
            MESSAGE "Registro de Notifica»’o n’o estÿ dispon­vel para enviar ao Ensemble"
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.

            RETURN.
        END.

        CREATE SERVER hWebService.
        hWebService:CONNECT(c-webservice-not) NO-ERROR.

        IF VALID-HANDLE(hWebService) THEN DO:
            RUN RecNotificacaoEMSSoap SET hConsultaValorPortType ON hWebService NO-ERROR.

            IF VALID-HANDLE(hConsultaValorPortType) THEN DO:
                RUN RecNotificacaoEMS IN hConsultaValorPortType(INPUT p-codigo, OUTPUT p-retorno) NO-ERROR.
                DELETE procedure hConsultaValorPortType NO-ERROR.
            END.

            hWebService:DISCONNECT() NO-ERROR.
            DELETE OBJECT hWebService NO-ERROR.
        END.
    END.
END CASE.
