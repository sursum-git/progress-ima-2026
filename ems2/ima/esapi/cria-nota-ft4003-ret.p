DEF TEMP-TABLE tt-it-terc-nf NO-UNDO 
    FIELD rw-saldo-terc     AS ROWID
    FIELD sequencia         LIKE saldo-terc.sequencia
    FIELD it-codigo         LIKE saldo-terc.it-codigo
    FIELD cod-refer         LIKE saldo-terc.cod-refer
    FIELD desc-nar          LIKE item.desc-item
    FIELD quantidade        LIKE saldo-terc.quantidade
    FIELD qt-alocada        LIKE saldo-terc.quantidade
    FIELD qt-disponivel     LIKE saldo-terc.quantidade
    FIELD qt-disponivel-inf LIKE saldo-terc.quantidade
    FIELD preco-total       LIKE componente.preco-total[1]
    FIELD preco-total-inf   LIKE componente.preco-total[1]
    FIELD selecionado       AS LOG
    FIELD qt-faturada       AS DECIMAL 
    FIELD un-faturada       AS CHAR
    FIELD un-estoque        AS CHAR
    INDEX codigo 
          sequencia
    INDEX selecionado
          selecionado.

DEF TEMP-TABLE tt-notas-geradas NO-UNDO
    FIELD rw-nota-fiscal AS   ROWID
    FIELD nr-nota        LIKE nota-fiscal.nr-nota-fis
    FIELD seq-wt-docto   LIKE wt-docto.seq-wt-docto.

DEF VAR de-acum AS DEC.
DEF VAR de-qt-retorno AS DECIMAL.

DEF VAR i-seq-wt-docto AS INT.
DEF VAR l-altera-quantidade AS LOG.
DEF VAR l-altera-preco AS LOG.

/* Temp-Table tt-erro Definitions ---*/
{method/dbotterr.i}

/* Definiá∆o da vari†veis GLOBAIS */
DEF NEW GLOBAL SHARED VAR c-seg-usuario        AS CHAR   NO-UNDO.

/* Definiá∆o da vari†veis */
DEF VAR h-bodi317pr          AS HANDLE NO-UNDO.
DEF VAR h-bodi317sd          AS HANDLE NO-UNDO.
DEF VAR h-bodi317im1bra      AS HANDLE NO-UNDO.
DEF VAR h-bodi317va          AS HANDLE NO-UNDO.
DEF VAR h-bodi317in          AS HANDLE NO-UNDO.
DEF VAR h-bodi317ef          AS HANDLE NO-UNDO.
def var h-boin404te as handle no-undo.
DEF VAR c-ultimo-metodo-exec AS CHAR.

DEF VAR l-proc-ok-aux AS LOGICAL.

DEF INPUT PARAMETER p-nr-pedcli LIKE ped-venda.nr-pedcli.
DEF OUTPUT PARAMETER TABLE FOR tt-notas-geradas.

FIND ped-venda WHERE
     ped-venda.nr-pedcli = p-nr-pedcli NO-LOCK NO-ERROR.

DEF TEMP-TABLE tt-saldo-terc LIKE saldo-terc
    FIELD qt-pedida LIKE ped-item.qt-pedida.

/* Inicializaá∆o das BOS para C†lculo */
RUN dibo/bodi317in.p PERSISTENT SET h-bodi317in.
RUN inicializaBOS IN h-bodi317in (OUTPUT h-bodi317pr,
                                  OUTPUT h-bodi317sd,     
                                  OUTPUT h-bodi317im1bra,
                                  OUTPUT h-bodi317va).


/* Cria o registro WT-DOCTO para o pedido */
RUN criaWtDocto IN h-bodi317sd (INPUT  c-seg-usuario,
                                INPUT  '504',
                                INPUT  '3',
                                INPUT  "1", 
                                INPUT  'MEDTEXTIL',
                                INPUT  "",
                                INPUT  4,    
                                INPUT  4003, 
                                INPUT  TODAY,
                                INPUT  0,  
                                INPUT  '51209M',
                                INPUT  "",
                                OUTPUT i-seq-wt-docto,
                                OUTPUT l-proc-ok-aux).

// Descobre quais documentos ser∆o baixados
FOR EACH ped-item OF ped-venda WHERE 
         ped-item.cod-sit-item = 1 NO-LOCK.

    ASSIGN de-qt-retorno = 0.
    FOR EACH ped-item-rom WHERE
             ped-item-rom.nome-abrev = ped-item.nome-abrev AND
             ped-item-rom.nr-pedcli = ped-item.nr-pedcli AND
             ped-item-rom.nr-sequencia = ped-item.nr-sequencia 
             NO-LOCK.

        FIND ob-etiqueta WHERE
             ob-etiqueta.cod-estab = '504' AND
             ob-etiqueta.num-etiqueta = ped-item-rom.num-etiqueta 
             NO-LOCK NO-ERROR.

        IF AVAIL ob-etiqueta THEN
           ASSIGN de-qt-retorno = de-qt-retorno + ob-etiqueta.quantidade.
    END.

    IF de-qt-retorno = 0 THEN NEXT.

    FOR EACH saldo-terc WHERE
             saldo-terc.cod-estab = '504' AND
             saldo-terc.it-codigo = ped-item.it-codigo AND
             saldo-terc.cod-refer = ped-item.cod-refer AND
             saldo-terc.quantidade > 0 NO-LOCK
             BY saldo-terc.dt-retorno.
    
        FIND tt-saldo-terc WHERE
             tt-saldo-terc.cod-emitente = saldo-terc.cod-emitente AND
             tt-saldo-terc.nro-docto = saldo-terc.nro-docto AND
             tt-saldo-terc.serie-docto = saldo-terc.serie-docto NO-ERROR.
        IF NOT AVAIL tt-saldo-terc THEN DO.
           CREATE tt-saldo-terc.
           ASSIGN tt-saldo-terc.cod-emitente = saldo-terc.cod-emitente
                  tt-saldo-terc.nro-docto = saldo-terc.nro-docto 
                  tt-saldo-terc.serie-docto = saldo-terc.serie-docto
                  tt-saldo-terc.nat-operacao = saldo-terc.nat-operacao
                  tt-saldo-terc.dt-retorno = saldo-terc.dt-retorno.
        END.
    END.
END.

// busca os Itens do Documento de Remessa
FOR EACH tt-saldo-terc BREAK BY tt-saldo-terc.dt-retorno.
    RUN geraItensTerceirosTtItTercNf IN h-bodi317sd (INPUT i-seq-wt-docto, 
                                                     INPUT tt-saldo-terc.serie-docto,
                                                     INPUT tt-saldo-terc.nro-docto,
                                                     INPUT tt-saldo-terc.nat-operacao,
                                                     INPUT 0,
                                                     INPUT 0,
                                                     OUTPUT TABLE tt-it-terc-nf,
                                                     OUTPUT l-proc-ok-aux).
END.     



// Marca os Itens e a quantidade que ser∆o faturados 

FOR EACH ped-item OF ped-venda WHERE 
         ped-item.cod-sit-item = 1 NO-LOCK.

    ASSIGN de-qt-retorno = 0.
    FOR EACH ped-item-rom WHERE
             ped-item-rom.nome-abrev = ped-item.nome-abrev AND
             ped-item-rom.nr-pedcli = ped-item.nr-pedcli AND
             ped-item-rom.nr-sequencia = ped-item.nr-sequencia 
             NO-LOCK.

        FIND ob-etiqueta WHERE
             ob-etiqueta.cod-estab = '504' AND
             ob-etiqueta.num-etiqueta = ped-item-rom.num-etiqueta 
             NO-LOCK NO-ERROR.
        IF AVAIL ob-etiqueta THEN
           ASSIGN de-qt-retorno = de-qt-retorno + ob-etiqueta.quantidade.
    END.

    ASSIGN de-acum = 0.
    FOR EACH tt-it-terc-nf WHERE
             tt-it-terc-nf.it-codigo = ped-item.it-codigo AND
             tt-it-terc-nf.cod-refer = ped-item.cod-refer AND 
             tt-it-terc-nf.quantidade > 0 NO-LOCK.

        IF tt-it-terc-nf.quantidade >= (de-qt-retorno - de-acum) THEN DO.
           ASSIGN tt-it-terc-nf.qt-disponivel-inf = (de-qt-retorno - de-acum)
                  tt-it-terc-nf.selecionado = YES.
           LEAVE.
        END.
        ELSE
           ASSIGN tt-it-terc-nf.selecionado = YES.

        ASSIGN de-acum = de-acum + tt-it-terc-nf.quantidade.
    END.
END.

FIND FIRST wt-docto WHERE
           wt-docto.seq-wt-docto = i-seq-wt-docto NO-ERROR.

ASSIGN wt-docto.nr-tabpre = ''.

RUN emptyRowErrors IN h-bodi317in.
RUN inbo/boin404te.p PERSISTENT SET h-boin404te.
RUN setaHandleBoin404te IN h-bodi317sd (INPUT h-boin404te).

FOR EACH tt-it-terc-nf WHERE
         tt-it-terc-nf.selecionado NO-LOCK.

    RUN geraWtItDoctoPartindoDoTtItTercNf in h-bodi317sd(INPUT  wt-docto.seq-wt-docto,
                                                         INPUT  10,
                                                         INPUT  10,
                                                         INPUT  TABLE tt-it-terc-nf,
                                                         OUTPUT l-proc-ok-aux).

    RUN devolveErrosbodi317sd             IN h-bodi317sd (OUTPUT c-ultimo-metodo-exec,
                                                          OUTPUT TABLE RowErrors).

    FIND FIRST RowErrors NO-LOCK NO-ERROR.

    /* Caso tenha achado algum erro ou advertància, mostra em tela */
    IF AVAIL RowErrors THEN DO.
       FOR EACH RowErrors:
           /*MESSAGE rowerrors.errordescription
                   VIEW-AS ALERT-BOX ERROR BUTTONS OK
                   TITLE "Erro - NF". */
       END.
    END.

    IF VALID-HANDLE(h-boin404te) THEN
       DELETE PROCEDURE h-boin404te.

    ASSIGN h-boin404te = ?.

END.
 


FOR EACH wt-it-docto WHERE
         wt-it-docto.seq-wt-docto = i-seq-wt-docto SHARE-LOCK.

    ASSIGN wt-it-docto.nr-tabpre = ''
           wt-it-docto.un[2] = wt-it-docto.un[1]
           wt-it-docto.quantidade[2] = wt-it-docto.quantidade[1].

    /* Limpar a tabela de erros em todas as BOS */
    RUN emptyRowErrors IN h-bodi317in.


    /* Atende todos os itens do pedido, com tela de acompanhamento */
    RUN inicializaAcompanhamento IN h-bodi317pr.
    RUN atendeTotalSeq IN h-bodi317pr (INPUT  i-seq-wt-docto,
                                       INPUT  wt-it-docto.seq-wt-it-docto,
                                       OUTPUT l-proc-ok-aux).
    RUN finalizaAcompanhamento IN h-bodi317pr.
     
    /* Busca poss°veis erros que ocorreram nas validaá‰es */
    RUN devolveErrosbodi317pr IN h-bodi317pr (OUTPUT c-ultimo-metodo-exec,
                                              OUTPUT TABLE RowErrors).

    /* Pesquisa algum erro ou advertància que tenha ocorrido */
    FIND FIRST RowErrors NO-LOCK NO-ERROR.

    /* Caso tenha achado algum erro ou advertància, mostra em tela */
    IF AVAIL RowErrors THEN DO.
       FOR EACH RowErrors:
            MESSAGE rowerrors.errordescription 
                VIEW-AS ALERT-BOX ERROR BUTTONS OK
                TITLE "ERRO - Atendendo Reservas".
       END.
    END.

    /* Caso ocorreu problema nas validaá‰es, n∆o continua o processo */
    IF NOT l-proc-ok-aux THEN
       UNDO, LEAVE.
END.



/* Limpar a tabela de erros em todas as BOS */
RUN emptyRowErrors           IN h-bodi317in.

/* Calcula o pedido, com acompanhamento */
RUN inicializaAcompanhamento IN h-bodi317pr.
RUN confirmaCalculo          IN h-bodi317pr (INPUT  i-seq-wt-docto,
                                             OUTPUT l-proc-ok-aux).
RUN finalizaAcompanhamento   IN h-bodi317pr.

/* Busca poss°veis erros que ocorreram nas validaá‰es */
RUN devolveErrosbodi317pr    IN h-bodi317pr (OUTPUT c-ultimo-metodo-exec,
                                             OUTPUT TABLE RowErrors).

/* Pesquisa algum erro ou advertància que tenha ocorrido */
FIND FIRST RowErrors NO-LOCK NO-ERROR.

/* Caso tenha achado algum erro ou advertància, mostra em tela */
IF AVAIL RowErrors THEN
   FOR EACH RowErrors:
       MESSAGE rowerrors.errordescription
                VIEW-AS ALERT-BOX ERROR BUTTONS OK
                TITLE "Erro - Calculo".
   END.

/* Caso ocorreu problema nas validaá‰es, n∆o continua o processo */
IF NOT l-proc-ok-aux THEN
   UNDO, LEAVE.

FIND wt-docto WHERE
     wt-docto.seq-wt-docto = i-seq-wt-docto.

/* Efetiva os pedidos e cria a nota */
RUN dibo/bodi317ef.p PERSISTENT SET h-bodi317ef.
RUN emptyRowErrors           IN h-bodi317in.
RUN inicializaAcompanhamento IN h-bodi317ef.
RUN setaHandlesBOS           IN h-bodi317ef(h-bodi317pr,     
                                            h-bodi317sd, 
                                            h-bodi317im1bra, 
                                            h-bodi317va).
RUN efetivaNota              IN h-bodi317ef (INPUT i-seq-wt-docto,
                                             INPUT YES,
                                             OUTPUT l-proc-ok-aux).
RUN finalizaAcompanhamento   IN h-bodi317ef.


/* Busca poss°veis erros que ocorreram nas validaá‰es */
RUN devolveErrosbodi317ef    IN h-bodi317ef (OUTPUT c-ultimo-metodo-exec,
                                             OUTPUT TABLE RowErrors).

/* Pesquisa algum erro ou advertància que tenha ocorrido */
FIND FIRST RowErrors WHERE
           RowErrors.ErrorSubType = "ERROR":U NO-ERROR.

/* Caso tenha achado algum erro ou advertància, mostra em tela */
IF AVAIL RowErrors THEN
   FOR EACH RowErrors:
       MESSAGE rowerrors.errordescription
               VIEW-AS ALERT-BOX ERROR BUTTONS OK
               TITLE "Erro - Efetivaá∆o".
   END.

/* Caso ocorreu problema nas validaá‰es, n∆o continua o processo */
IF NOT l-proc-ok-aux THEN DO:
   DELETE PROCEDURE h-bodi317ef.
   UNDO, LEAVE.
END.

/* Busca as notas fiscais geradas */
RUN buscaTTNotasGeradas IN h-bodi317ef (OUTPUT l-proc-ok-aux,
                                        OUTPUT TABLE tt-notas-geradas).

/* Elimina o handle do programa bodi317ef */
DELETE PROCEDURE h-bodi317ef.

/* Finalizaá∆o das BOS utilizada no c†lculo */
RUN finalizaBOS in h-bodi317in.



