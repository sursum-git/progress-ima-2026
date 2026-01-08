DEFINE VARIABLE hBo AS HANDLE      NO-UNDO.
DEFINE VARIABLE iTipoAtividade AS INTEGER     NO-UNDO.
DEFINE VARIABLE cDescTipoAtiv  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE codCnae        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lCorrige       AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cCorrigido     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cAval          AS CHARACTER   NO-UNDO.
RUN esbo/bonat001.p PERSISTENT SET  hBo.
UPDATE lCorrige.
OUTPUT TO value('c:\temp\cliente_class_' + string(TIME) + '.csv').
PUT "Avaliaá∆o;corrigido?;codigo;nome;nome abrev;tipo Ativ;desc.tipo ativ.; CNAE;dt.ult.fat.med" SKIP.

FOR EACH emitente 
    WHERE emitente.estado = 'es' NO-LOCK.
    FIND FIRST nota-fiscal OF emitente 
          WHERE nota-fiscal.esp-docto = 22 NO-LOCK NO-ERROR.
    IF NOT AVAIL  nota-fiscal AND emitente.identific <> 1  THEN NEXT.
    FIND ext-emitente OF emitente NO-LOCK NO-ERROR. 
    RUN retornarTipoAtividadeCliente IN hBo(emitente.cod-emitente, OUTPUT iTipoAtividade, OUTPUT codCnae).
    RUN buscarDescTipoAtividade IN hBo(INPUT iTipoAtividade ,OUTPUT cDescTipoAtiv ).
    IF (iTipoAtividade = 1 OR iTipoAtividade = 3) AND emitente.contrib-icms = NO THEN
        ASSIGN cAval = "MUDAR".
    ELSE
       ASSIGN cAVal = "OK".
    IF AVAIL ext-emitente AND (iTipoAtividade = 1 OR iTipoAtividade = 3) AND lCorrige  AND emitente.contrib-icms = NO   THEN DO:
       FIND CURRENT emitente EXCLUSIVE-LOCK NO-ERROR. 
       ASSIGN emitente.contrib-icms = YES. 
       FIND CURRENT emitente NO-LOCK NO-ERROR. 
       ASSIGN cCorrigido = 'sim'.
    END.
    ELSE DO:
       ASSIGN cCorrigido = 'n∆o'.
    END.

    
    EXPORT DELIMITER ";" cAval cCorrigido emitente.cod-emitente emitente.nome-emit emitente.nome-abrev iTipoAtividade cDescTipoAtiv codCNAE IF AVAIL ext-emitente THEN ext-emitente.dt-ult-fat-med ELSE ? .


END.
OUTPUT CLOSE.
