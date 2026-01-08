DEFINE INPUT  PARAMETER pRowid AS ROWID       NO-UNDO.
DEFINE OUTPUT PARAMETER cErro  AS CHARACTER   NO-UNDO.


DEFINE BUFFER b-lisa-integra FOR lisa-integra.
DEFINE VARIABLE rowidNF     AS ROWID       NO-UNDO.
DEFINE VARIABLE cChaveNF    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lEntrouAcao AS LOGICAL     NO-UNDO.

FIND lisa-integra NO-LOCK
    WHERE ROWID(lisa-integra) = pRowid
    NO-ERROR.
IF NOT  AVAIL lisa-integra THEN DO:
   ASSIGN cErro = "Registro Lisa Integra N∆o encontrado".
END.
/*
MESSAGE "transacao:" lisa-integra.cod-trans SKIP
        "situacao:" lisa-integra.ind-situacao SKIP
        "acao:" lisa-integra.acao
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/

IF  lisa-integra.cod-trans = 'NotaRemessa' AND lisa-integra.ind-situacao = 1 THEN DO:
    RUN lisa/integraNotaRemessa.p(ROWID(lisa-integra),OUTPUT cErro).
    ASSIGN lEntrouAcao = YES.
END.
    


IF lisa-integra.cod-trans = 'NotaAvulsa' AND lisa-integra.ind-situacao = 1 THEN DO:
   RUN lisa/integraNotaAvulsa.p(ROWID(lisa-integra),OUTPUT cErro).
   ASSIGN lEntrouAcao = YES.
END.

IF  lisa-integra.cod-trans      = 'PackingList' AND lisa-integra.acao  = 'ENVIAR'   AND lisa-integra.ind-situacao   = 1   THEN DO:
    ASSIGN lEntrouAcao = YES.
    RUN lisa/integraPackList.p(ROWID(lisa-integra), OUTPUT cErro).
END.
    


IF lisa-integra.cod-trans = 'PackingAvulso' AND lisa-integra.acao = 'ENVIAR' AND lisa-integra.ind-situacao = 1 THEN DO:
   RUN lisa/integraPacklistAvulso(ROWID(lisa-integra), OUTPUT cErro).
   ASSIGN lEntrouAcao = YES.
END.
   


IF  lisa-integra.cod-trans = 'ISF' AND  lisa-integra.ind-situacao = 1 AND lisa-integra.acao = 'ENVIAR' THEN DO:
    RUN lisa/integraIsfEnvio.p(ROWID(lisa-integra), OUTPUT cErro).
    ASSIGN lEntrouAcao = YES.
END.
    


IF  lisa-integra.cod-trans = 'ISF' AND  lisa-integra.ind-situacao = 1 AND lisa-integra.acao = 'APROVAR' THEN DO:
    FIND ped-venda no-lock
        WHERE  ped-venda.nr-pedcli  = entry(1,lisa-integra.chave,"|")
        AND    ped-venda.nome-abrev = entry(2,lisa-integra.chave,"|") 
        NO-ERROR.
    IF NOT AVAIL ped-venda THEN DO:
       ASSIGN cErro = "Pedido de venda n∆o encontrado:" + ped-venda.nome-abrev  + "-" + ped-venda.nr-pedcli.
       RETURN 'adm-error'.
    END.
    RUN esapi/getNFVendaPedido.p(ROWID(ped-venda), OUTPUT rowidNF, OUTPUT cErro ) .

    FIND nota-fiscal NO-LOCK
        WHERE rowid(nota-fiscal) = rowidNF NO-ERROR.
    IF AVAIL nota-fiscal THEN DO:
       FIND LAST b-lisa-integra NO-LOCK
           WHERE b-lisa-integra.cod-trans       = "REMESSANOTAVENDA"
           AND   b-lisa-integra.ind-situacao    = 1
           AND   b-lisa-integra.acao            = "ENVIAR"
           AND   b-lisa-integra.chave           = nota-fiscal.cod-estabel + "|" + nota-fiscal.serie + "|" + nota-fiscal.nr-nota-fis NO-ERROR.
       IF NOT AVAIL b-lisa-integra THEN DO:
          ASSIGN cErro = "N∆o encontrado registro transaá∆o:REMESSANOTAVENDA  - Situaá∆o: Pendente - Aá∆o: Enviar - Chave:" +
                nota-fiscal.cod-estabel + "|" + nota-fiscal.serie + "|" + nota-fiscal.nr-nota-fis .
          RETURN 'adm-error'.
       END.
       RUN lisa/integraNFVenda.p(ROWID(b-lisa-integra),OUTPUT cErro).
       ASSIGN lEntrouAcao = YES.
    END.
END.

IF lisa-integra.cod-trans      = 'ISF' AND  lisa-integra.ind-situacao   <= 3 AND lisa-integra.acao   = 'RESERVAR' THEN DO:
   RUN lisa/integraIsfRetorno.p(ROWID(lisa-integra), OUTPUT cErro).
   ASSIGN lEntrouAcao = YES.
END.
   
// Aguardando Integraá∆o 
IF  lisa-integra.cod-trans = "RemessaNotaVenda" AND lisa-integra.ind-situacao = 1  THEN DO:
    RUN lisa/integraNotaVenda.p(ROWID(lisa-integra),OUTPUT cErro).
    ASSIGN lEntrouAcao = YES.
END.
    

// Aguardando Integraá∆o 
IF  lisa-integra.cod-trans = "ConfEtiquetas" AND lisa-integra.ind-situacao = 1 THEN DO: 
    RUN lisa/integraConfEtiqueta.p(ROWID(lisa-integra), OUTPUT cErro).
    ASSIGN lEntrouAcao = YES.

END.


IF NOT lEntrouAcao THEN DO:
   MESSAGE "O c¢digo de transaá∆o:" +  lisa-integra.cod-trans + " - Aá∆o:" +  lisa-integra.acao + " - Situaá∆o:" + STRING(lisa-integra.ind-situacao)
       + " n∆o tem aá∆o associada"
       VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
END.

/*IF lisa-integra.cod-trans = "NotaRetorno" AND lisa-integra.ind-situacao = 1 // Aguardando Integraá∆o 
    THEN
    //VERIFICAR
END.*/


