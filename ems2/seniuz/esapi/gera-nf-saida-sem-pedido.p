DEF TEMP-TABLE tt-wt-docto LIKE wt-docto.
DEF TEMP-TABLE tt-it-nota-fisc LIKE it-nota-fisc
    FIELD nr-lote AS CHAR.

DEFINE TEMP-TABLE wt-notas-geradas
       FIELD rw-nota-fiscal AS ROWID.

/* Definicao da tabela temporaria tt-notas-geradas, include {dibo/bodi317ef.i1} */
DEF TEMP-TABLE tt-notas-geradas NO-UNDO
    FIELD rw-nota-fiscal AS   ROWID
    FIELD nr-nota        LIKE nota-fiscal.nr-nota-fis
    FIELD seq-wt-docto   LIKE wt-docto.seq-wt-docto.

/* Temp-Table tt-erro Definitions ---*/
{method/dbotterr.i}
/* {dibo/bodi317.i1} */

DEF TEMP-TABLE tt-itens 
    FIELD nr-sequencia AS INT
    FIELD data         AS DATE FORMAT "99/99/9999" 
    FIELD item-ori     LIKE ITEM.it-codigo
    FIELD nr-lote      AS CHAR FORMAT "x(2)"
    FIELD un-ori       LIKE ITEM.un
    FIELD qtde-ori     AS DEC
    FIELD it-codigo    LIKE ITEM.it-codigo
    FIELD un           LIKE ITEM.un
    FIELD quantidade   AS DEC
    FIELD valor        AS DEC
    FIELD vlr-tot-item AS DEC
    INDEX indice1 IS PRIMARY nr-sequencia.


DEFINE TEMP-TABLE tt-erro-nf NO-UNDO
    FIELD mensagem AS CHARACTER FORMAT "x(1000)".


/* Definiá∆o da vari†veis */
DEF VAR h-bodi317pr          AS HANDLE NO-UNDO.
DEF VAR h-bodi317sd          AS HANDLE NO-UNDO.
DEF VAR h-bodi317im1bra      AS HANDLE NO-UNDO.
DEF VAR h-bodi317va          AS HANDLE NO-UNDO.
DEF VAR h-bodi317in          AS HANDLE NO-UNDO.
DEF VAR h-bodi317ef          AS HANDLE NO-UNDO.
DEF VAR l-proc-ok-aux        AS LOG    NO-UNDO.
DEF VAR c-ultimo-metodo-exec AS CHAR   NO-UNDO.
DEF VAR da-dt-emis-nota      AS DATE   NO-UNDO.
DEF VAR da-dt-base-dup       AS DATE   NO-UNDO.
DEF VAR da-dt-prvenc         AS DATE   NO-UNDO.
DEF VAR c-nome-abrev         AS CHAR   NO-UNDO.   
DEF VAR c-nat-operacao       AS CHAR   NO-UNDO.
DEF VAR i-seq-wt-docto       AS INT    NO-UNDO.
DEF VAR i-seq-wt-it-docto    AS INT    NO-UNDO.

DEF VAR i-nr-seq             AS INT    NO-UNDO.

DEF VAR lg-ret               AS LOG    NO-UNDO.

DEF VAR c-serie              AS CHAR   NO-UNDO.
DEF VAR c-cod-depos          AS CHAR   NO-UNDO.
DEF VAR c-natur-oper         LIKE nota-fiscal.nat-oper.
DEF VAR c-cod-estabel     LIKE estabelec.cod-estabel.
DEF VAR c-cliente         LIKE emitente.nome-abrev.

DEFINE VARIABLE data-validade AS DATE  INIT 12.31.9999.

DEFINE VARIABLE i-nr-requisicao AS INTEGER     NO-UNDO.

DEFINE VARIABLE c-erro-aux    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-it-codigo   AS CHARACTER   NO-UNDO.

DEFINE VARIABLE de-saldo-disp AS DECIMAL     NO-UNDO.

DEFINE VARIABLE lg-debug AS LOGICAL NO-UNDO.

DEFINE SHARED VARIABLE c-seg-usuario AS CHARACTER NO-UNDO.

DEFINE VARIABLE i-qtd-ja-atend AS INTEGER NO-UNDO.

DEFINE VARIABLE c-msg-erro AS CHARACTER   NO-UNDO.
DEFINE VARIABLE i-ind      AS INTEGER     NO-UNDO.


DEFINE INPUT  PARAMETER TABLE FOR tt-wt-docto.
DEFINE INPUT  PARAMETER TABLE FOR tt-it-nota-fisc.


ASSIGN l-proc-ok-aux = YES.
ASSIGN c-msg-erro    = "".

FIND FIRST tt-wt-docto NO-LOCK NO-ERROR.

PROCESSO_NFS:
DO TRANSACTION:

    /* Inicializaá∆o das BOS para C†lculo */
    RUN dibo/bodi317in.p PERSISTENT SET h-bodi317in.
    RUN inicializaBOS IN h-bodi317in (OUTPUT h-bodi317pr,
                                      OUTPUT h-bodi317sd,     
                                      OUTPUT h-bodi317im1bra,
                                      OUTPUT h-bodi317va).

    FIND FIRST tt-it-nota-fisc NO-LOCK.

    /* Limpar a tabela de erros em todas as BOS */
    RUN emptyRowErrors        IN h-bodi317in.
    
    
    /* Cria o registro WT-DOCTO para o pedido */
    RUN criaWtDocto IN h-bodi317sd (INPUT  tt-wt-docto.usuario,
                                    INPUT  tt-wt-docto.cod-estabel,
                                    INPUT  tt-wt-docto.serie,
                                    INPUT  "1", 
                                    INPUT  tt-wt-docto.nome-abrev,
                                    INPUT  ?,
                                    INPUT  4,
                                    INPUT  9999, 
                                    INPUT  tt-wt-docto.dt-emis,
                                    INPUT  0,  
                                    INPUT  tt-wt-docto.nat-operacao,
                                    INPUT  "",
                                    OUTPUT i-seq-wt-docto,
                                    OUTPUT l-proc-ok-aux).


    /* Busca poss°veis erros que ocorreram nas validaá‰es */
    RUN devolveErrosbodi317sd IN h-bodi317sd (OUTPUT c-ultimo-metodo-exec,
                                              OUTPUT TABLE RowErrors).

    /* Pesquisa algum erro ou advertància que tenha ocorrido */
    FIND FIRST RowErrors
        WHERE RowErrors.ErrorSubType = "ERROR":U
        NO-LOCK NO-ERROR.

    /* Caso tenha achado algum erro ou advertància, mostra em tela */
    IF AVAIL RowErrors THEN DO.
        FOR EACH RowErrors
            WHERE RowErrors.ErrorSubType = "ERROR":U:

            ASSIGN c-msg-erro = c-msg-erro + CHR(27) + "(NFS-1) " + RowErrors.ErrorDescription.

            IF INDEX(RowErrors.ErrorSubType, "rie/estabelecimento") > 0 THEN DO:
                ASSIGN c-msg-erro = c-msg-erro + " (Estab: '" + tt-wt-docto.cod-estabel + "' SÇrie: '" + tt-wt-docto.serie + "')".
            END.
        END.
        MESSAGE c-msg-erro
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.

    /* Caso ocorreu problema nas validaá‰es, n∆o continua o processo */
    IF NOT l-proc-ok-aux THEN DO:
        UNDO PROCESSO_NFS, LEAVE PROCESSO_NFS.
    END.


    FIND FIRST wt-docto
        WHERE wt-docto.seq-wt-docto = i-seq-wt-docto
        NO-ERROR.

    FIND mensagem WHERE
         mensagem.cod-men = 9 NO-LOCK NO-ERROR.
    
    ASSIGN wt-docto.observ-nota = mensagem.texto-mens.

    FOR EACH tt-it-nota-fisc NO-LOCK BY tt-it-nota-fisc.it-codigo.

        /* Limpar a tabela de erros em todas as BOS */
        RUN emptyRowErrors        IN h-bodi317in.

        RUN inicializaAcompanhamento      IN h-bodi317sd.
        RUN criaWtItDocto IN h-bodi317sd (INPUT  "",
                                          INPUT  "",
                                          INPUT  tt-it-nota-fisc.nr-seq-fat,
                                          INPUT  tt-it-nota-fisc.it-codigo, 
                                          INPUT  tt-it-nota-fisc.cod-refer,
                                          INPUT  tt-it-nota-fisc.nat-oper,    
                                          OUTPUT i-seq-wt-it-docto,
                                          OUTPUT l-proc-ok-aux).
        RUN finalizaAcompanhamento        IN h-bodi317sd.

        /* Busca poss°veis erros que ocorreram nas validaá‰es */
        RUN devolveErrosbodi317sd         IN h-bodi317sd (OUTPUT c-ultimo-metodo-exec,
                                                          OUTPUT TABLE RowErrors).

        /* Pesquisa algum erro ou advertància que tenha ocorrido */
        FIND FIRST RowErrors
            WHERE RowErrors.ErrorSubType = "ERROR":U
            NO-LOCK NO-ERROR.

        /* Caso tenha achado algum erro ou advertància, mostra em tela */
        IF AVAIL RowErrors THEN DO.
            FOR EACH RowErrors
                WHERE RowErrors.ErrorSubType = "ERROR":U:

                ASSIGN c-msg-erro = c-msg-erro + CHR(27) + "(NFS-2) " + RowErrors.ErrorDescription.

                IF INDEX(RowErrors.ErrorSubType, "rie/estabelecimento") > 0 THEN DO:
                    ASSIGN c-msg-erro = c-msg-erro + " (Estab: '" + tt-wt-docto.cod-estabel + "' SÇrie: '" + tt-wt-docto.serie + "')".
                END.
            END.
            MESSAGE c-msg-erro
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.

        /* Caso ocorreu problema nas validaá‰es, n∆o continua o processo */
        IF NOT l-proc-ok-aux THEN
            UNDO PROCESSO_NFS, LEAVE PROCESSO_NFS.


        RUN GravaInfGeraisWtItDocto IN h-bodi317sd (INPUT  i-seq-wt-docto,
                                                    INPUT  i-seq-wt-it-docto,
                                                    INPUT  tt-it-nota-fisc.qt-faturada[1],
                                                    INPUT  tt-it-nota-fisc.vl-preuni, 
                                                    INPUT  0,
                                                    INPUT  0).

        /* Busca poss°veis erros que ocorreram nas validaá‰es */
        RUN devolveErrosbodi317sd         IN h-bodi317sd (OUTPUT c-ultimo-metodo-exec,
                                                          OUTPUT TABLE RowErrors).

        /* Pesquisa algum erro ou advertància que tenha ocorrido */
        FIND FIRST RowErrors
            WHERE RowErrors.ErrorSubType = "ERROR":U
            NO-LOCK NO-ERROR.

        /* Caso tenha achado algum erro ou advertància, mostra em tela */
        IF AVAIL RowErrors THEN DO.
            FOR EACH RowErrors
                WHERE RowErrors.ErrorSubType = "ERROR":U:

                ASSIGN c-msg-erro = c-msg-erro + CHR(27) + "(NFS-3) " + RowErrors.ErrorDescription.
            END.
            MESSAGE c-msg-erro
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.

        /* Caso ocorreu problema nas validaá‰es, n∆o continua o processo */
        IF NOT l-proc-ok-aux THEN
            UNDO PROCESSO_NFS, LEAVE PROCESSO_NFS.

        
        FIND FIRST ITEM
            WHERE ITEM.it-codigo = tt-it-nota-fisc.it-codigo
            NO-LOCK NO-ERROR.

        
        /*  Seta o lote do °tem  */
        RUN criaAlteraWtFatSerLote IN h-bodi317sd (INPUT  YES,
                                                   INPUT  i-seq-wt-docto,
                                                   INPUT  i-seq-wt-it-docto,
                                                   INPUT  tt-it-nota-fisc.it-codigo,
                                                   INPUT  tt-it-nota-fisc.cod-depos,
                                                   INPUT  "",
                                                   INPUT  tt-it-nota-fisc.nr-lote,
                                                   INPUT  tt-it-nota-fisc.qt-faturada[1],
                                                   INPUT  0,
                                                   INPUT  data-validade,
                                                   OUTPUT l-proc-ok-aux).

        /* Pesquisa algum erro ou advertància que tenha ocorrido */
        FIND FIRST RowErrors
            WHERE RowErrors.ErrorSubType = "ERROR":U
            NO-LOCK NO-ERROR.

        /* Caso tenha achado algum erro ou advertància, mostra em tela */
        IF AVAIL RowErrors THEN DO.
            FOR EACH RowErrors
                WHERE RowErrors.ErrorSubType = "ERROR":U:

                ASSIGN c-msg-erro = c-msg-erro + CHR(27) + "(NFS-4) " + RowErrors.ErrorDescription.
            END.
        END.
        
        /* Caso ocorreu problema nas validaá‰es, n∆o continua o processo */
        IF NOT l-proc-ok-aux THEN
            UNDO PROCESSO_NFS, LEAVE PROCESSO_NFS.
    END.

    FOR EACH wt-it-docto
        WHERE wt-it-docto.seq-wt-docto = i-seq-wt-docto
        SHARE-LOCK.
        
        IF wt-it-docto.vl-pretab = ? THEN
            ASSIGN wt-it-docto.vl-pretab = 0.
        
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
        FIND FIRST RowErrors
            WHERE RowErrors.ErrorSubType = "ERROR":U
            NO-LOCK NO-ERROR.

        ASSIGN l-proc-ok-aux = YES.

        /* Caso tenha achado algum erro ou advertància, mostra em tela */
        IF AVAIL RowErrors THEN DO:
            FOR EACH RowErrors
                WHERE RowErrors.ErrorSubType = "ERROR":U:

                IF RowErrors.ErrorDescription MATCHES("*LocalizaCidadeZF*") THEN NEXT.

                ASSIGN c-erro-aux    = ""
                       l-proc-ok-aux = NO.

                IF RowErrors.ErrorDescription = "Lote deve ser diferente de branco" THEN
                    ASSIGN c-msg-erro = c-msg-erro + CHR(27) + "(NFS-5) Item: " + wt-it-docto.it-codigo + " - " + RowErrors.ErrorDescription.
                ELSE
                IF RowErrors.ErrorDescription MATCHES("*LocalizaCidadeZF*") THEN DO:
                    ASSIGN c-msg-erro = c-msg-erro + CHR(27) + "(NFS-5) Cidade: " + wt-docto.cidade + " Estado: " + wt-docto.estado + " - " + RowErrors.ErrorDescription.
                END.
                ELSE
                    ASSIGN c-msg-erro = c-msg-erro + CHR(27) + "(NFS-5) " + RowErrors.ErrorDescription.
            END.

            MESSAGE c-msg-erro
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

        END.

        /* Caso ocorreu problema nas validaá‰es, n∆o continua o processo */
        IF NOT l-proc-ok-aux THEN
            UNDO PROCESSO_NFS, LEAVE PROCESSO_NFS.
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
    FIND FIRST RowErrors
        WHERE RowErrors.ErrorSubType = "ERROR":U
        NO-LOCK NO-ERROR.

    /* Caso tenha achado algum erro ou advertància, mostra em tela */
    IF AVAIL RowErrors THEN DO:
        FOR EACH RowErrors
            WHERE RowErrors.ErrorSubType = "ERROR":U:

            ASSIGN c-msg-erro = c-msg-erro + CHR(27) + "(NFS-6) " + RowErrors.ErrorDescription.
        END.
        MESSAGE c-msg-erro
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

    END.

    /* Caso ocorreu problema nas validaá‰es, n∆o continua o processo */
    IF NOT l-proc-ok-aux THEN
        UNDO PROCESSO_NFS, LEAVE PROCESSO_NFS.


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
    FIND FIRST RowErrors
        WHERE RowErrors.ErrorSubType = "ERROR":U
        NO-ERROR.

    /*
    IF c-seg-usuario = "totvs" THEN
        MESSAGE "ERRO:" AVAIL RowErrors SKIP
                "l-proc-ok-aux:" l-proc-ok-aux
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    */

    /* Caso tenha achado algum erro ou advertància, mostra em tela */
    IF AVAIL RowErrors THEN DO:
        FOR EACH RowErrors
            WHERE RowErrors.ErrorSubType = "ERROR":U:

            /*
            IF c-seg-usuario = "totvs" THEN
                MESSAGE "ERRO:" SKIP
                        RowErrors.ErrorDescription
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
            */

            ASSIGN c-msg-erro = c-msg-erro + CHR(27) + "(NFS-7) " + RowErrors.ErrorDescription.
        END.
        MESSAGE c-msg-erro
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

    END.

    /* Caso ocorreu problema nas validaá‰es, n∆o continua o processo */
    IF NOT l-proc-ok-aux THEN DO:
       DELETE PROCEDURE h-bodi317ef.
       UNDO PROCESSO_NFS, LEAVE PROCESSO_NFS.
    END.
    


    /* Busca as notas fiscais geradas */
    RUN buscaTTNotasGeradas IN h-bodi317ef (OUTPUT l-proc-ok-aux,
                                            OUTPUT TABLE tt-notas-geradas).


    /*
    IF c-seg-usuario = "totvs" THEN
        MESSAGE "gera-nf-saida-sem-pedido" SKIP
                "Passou 2"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    */

    /* Elimina o handle do programa bodi317ef */
    DELETE PROCEDURE h-bodi317ef.

    /* Finalizaá∆o das BOS utilizada no c†lculo */
    RUN finalizaBOS in h-bodi317in.

    FIND FIRST tt-notas-geradas NO-LOCK NO-ERROR.

    IF AVAIL tt-notas-geradas THEN DO.
        FIND FIRST nota-fiscal
            WHERE ROWID(nota-fiscal) = tt-notas-geradas.rw-nota-fiscal
            SHARE-LOCK NO-ERROR.
    
        MESSAGE "Nota:" IF AVAIL nota-fiscal THEN nota-fiscal.nr-nota-fis ELSE "***"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
    
        IF NOT l-proc-ok-aux THEN
            UNDO PROCESSO_NFS, LEAVE PROCESSO_NFS.
    END.

    LEAVE.
END.

