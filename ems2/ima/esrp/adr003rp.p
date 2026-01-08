
{utp\ut-glob.i}

DEFINE INPUT  PARAMETER pData AS DATE        NO-UNDO.
DEFINE INPUT  PARAMETER pInativo AS LOGICAL     NO-UNDO.
DEFINE INPUT  PARAMETER pAtivo AS LOGICAL     NO-UNDO.

MESSAGE pAtivo SKIP pInativo
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
DEFINE TEMP-TABLE ttCliente
    FIELD cod-cliente  LIKE emitente.cod-emitente
    FIELD nome-abrev   LIKE emitente.nome-abrev
    FIELD email        LIKE emitente.e-mail
    FIELD dt-ult-ima   AS DATE
    FIELD dt-ult-med   AS DATE
    FIELD dt-refer     AS DATE
    FIELD dt-ult-venda AS DATE
    FIELD qt-dias      AS INT
    FIELD inativo      AS LOG LABEL "Sim/NÆo"
    FIELD desc-ramo-ativ   AS CHAR FORMAT "x(100)".

DEF VAR h-acomp        AS HANDLE  NO-UNDO.
RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
RUN pi-inicializar IN h-acomp (INPUT "Processando...").

FOR EACH emitente WHERE
         emitente.identific <> 2.
    RUN pi-acompanhar IN h-acomp (INPUT string(emitente.cod-emitente) + "-" + emitente.nome-abrev).
    CREATE ttCliente.
    ASSIGN ttCliente.cod-cliente = emitente.cod-emitente
           ttCliente.nome-abrev = emitente.nome-abrev
           ttCliente.email = emitente.e-mail.
           ttCliente.dt-refer = pData.

    FIND LAST ems2ima.nota-fiscal WHERE
              ems2ima.nota-fiscal.dt-cancela = ? AND
              ems2ima.nota-fiscal.cod-emitente = emitente.cod-emitente 
              USE-INDEX ch-emi-nota NO-LOCK NO-ERROR.

    IF AVAIL ems2ima.nota-fiscal THEN DO:

       IF i-ep-codigo-usuario = "1" THEN 
           ASSIGN ttCliente.dt-ult-ima = ems2ima.nota-fiscal.dt-emis-nota.
       ELSE 
           ASSIGN ttCliente.dt-ult-med = ems2ima.nota-fiscal.dt-emis-nota.
    END.

    FIND LAST dbaux.nota-fiscal WHERE
              dbaux.nota-fiscal.dt-cancela = ? AND
              dbaux.nota-fiscal.cod-emitente = emitente.cod-emitente 
              USE-INDEX ch-emi-nota NO-LOCK NO-ERROR.

    IF AVAIL dbaux.nota-fiscal THEN DO:

       IF i-ep-codigo-usuario = "1" THEN 
           ASSIGN ttCliente.dt-ult-med = dbaux.nota-fiscal.dt-emis-nota.
       ELSE 
           ASSIGN ttCliente.dt-ult-ima = dbaux.nota-fiscal.dt-emis-nota.
    END.

    IF ttCliente.dt-ult-ima = ? THEN
       ASSIGN ttCliente.dt-ult-ima = 01.01.2001.
    IF ttCliente.dt-ult-med = ? THEN
       ASSIGN ttCliente.dt-ult-med = 01.01.2001.

    
    ASSIGN ttCliente.dt-ult-venda = IF ttCliente.dt-ult-ima > ttCliente.dt-ult-med THEN 
                                ttCliente.dt-ult-ima
                               ELSE ttCliente.dt-ult-med.
           ttCliente.qt-dias = TODAY - ttCliente.dt-ult-venda.
   ASSIGN ttCliente.inativo =  TODAY - ttCliente.qt-dias < pData.
END.
RUN pi-acompanhar IN h-acomp (INPUT 'Exportando...').
OUTPUT TO c:\temp\clientes.txt.

PUT "Cod. Cliente | Nome | Email | Dt Ultima compra IMA | Dt Ultima compra MED | Data Referˆncia | éltima Venda | Dias inativo | Inativo | Ramo de Atividade" SKIP.

FOR EACH ttCliente.
    IF ttCliente.inativo = YES AND pInativo = NO  THEN NEXT.
    IF ttCliente.inativo = NO  AND pAtivo   = NO   THEN NEXT.

    FIND cont-emit WHERE
         cont-emit.cod-emitente = ttCliente.cod-cliente AND
         cont-emit.area = "comercial" NO-LOCK NO-ERROR.

    IF NOT AVAIL cont-emit THEN NEXT.

    ASSIGN ttCliente.email = cont-emit.e-mail.

    FIND FIRST ext-emitente WHERE
               ext-emitente.cod-emitente  = ttCliente.cod-cliente NO-LOCK NO-ERROR.

    IF AVAIL ext-emitente THEN DO:
        FIND FIRST ramo-ativ WHERE
                   ramo-ativ.cod-ramo-ativ = ext-emitente.cod-ramo-ativ NO-LOCK NO-ERROR.
    END.
    ASSIGN ttCliente.desc-ramo-ativ = IF AVAIL ramo-ativ THEN ramo-ativ.descricao ELSE "".
    EXPORT DELIMITER "|" ttCliente.                                                       
END.

OUTPUT CLOSE.

OS-COMMAND SILENT VALUE ("START excel /t T:\especificos\excel\clientes.xlsx").

RUN pi-finalizar IN h-acomp.
                                    
