DEF TEMP-TABLE tt-nota-fisc LIKE nota-fiscal.
DEF TEMP-TABLE tt-it-nota-fisc LIKE it-nota-fisc.
 
DEF TEMP-TABLE tt-notas-geradas NO-UNDO
    FIELD rw-nota-fiscal AS   ROWID
    FIELD nr-nota        LIKE nota-fiscal.nr-nota-fis
    FIELD seq-wt-docto   LIKE wt-docto.seq-wt-docto.

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
DEF VAR l-proc-ok-aux        AS LOG    NO-UNDO.
DEF VAR c-ultimo-metodo-exec AS CHAR   NO-UNDO.
DEF VAR da-dt-emis-nota      AS DATE   NO-UNDO.
DEF VAR da-dt-base-dup       AS DATE   NO-UNDO.
DEF VAR da-dt-prvenc         AS DATE   NO-UNDO.
DEF VAR c-nome-abrev         AS CHAR   NO-UNDO.   
DEF VAR c-nat-operacao       AS CHAR   NO-UNDO.
DEF VAR c-cod-canal-venda    AS CHAR   NO-UNDO.
DEF VAR i-seq-wt-docto       AS INT    NO-UNDO.
DEF VAR i-seq-wt-it-docto    AS INT    NO-UNDO.

DEF VAR i-nr-seq             AS INT    NO-UNDO.

DEF VAR c-serie              AS CHAR   NO-UNDO.
DEF VAR c-natur-oper         LIKE nota-fiscal.nat-oper.

DEF INPUT PARAMETER TABLE FOR tt-nota-fisc.
DEF INPUT PARAMETER TABLE FOR tt-it-nota-fisc.
DEF OUTPUT PARAMETER TABLE FOR tt-notas-geradas.

/* Inicializaá∆o das BOS para C†lculo */
RUN dibo/bodi317in.p PERSISTENT SET h-bodi317in.
RUN inicializaBOS IN h-bodi317in (OUTPUT h-bodi317pr,
                                  OUTPUT h-bodi317sd,     
                                  OUTPUT h-bodi317im1bra,
                                  OUTPUT h-bodi317va).

FIND FIRST tt-nota-fisc NO-LOCK NO-ERROR.

/* Limpar a tabela de erros em todas as BOS */
RUN emptyRowErrors        IN h-bodi317in.


/* Cria o registro WT-DOCTO para o pedido */
RUN criaWtDocto IN h-bodi317sd (INPUT  c-seg-usuario,
                                INPUT  tt-nota-fisc.cod-estabel,
                                INPUT  tt-nota-fisc.serie,
                                INPUT  "1", 
                                INPUT  tt-nota-fisc.nome-ab-cli,
                                INPUT  "",
                                INPUT  1,   // forma de emiss∆o 1 verificar
                                INPUT  4003, 
                                INPUT  tt-nota-fisc.dt-emis-nota,
                                INPUT  0,  
                                INPUT  tt-nota-fisc.nat-operacao,
                                INPUT  "",
                                OUTPUT i-seq-wt-docto,
                                OUTPUT l-proc-ok-aux).

/* Busca poss°veis erros que ocorreram nas validaá‰es */
RUN devolveErrosbodi317sd IN h-bodi317sd (OUTPUT c-ultimo-metodo-exec,
                                          OUTPUT TABLE RowErrors).

/* Pesquisa algum erro ou advertància que tenha ocorrido */
FIND FIRST RowErrors NO-LOCK NO-ERROR.

/* Caso tenha achado algum erro ou advertància, mostra em tela */
IF AVAIL RowErrors THEN DO.
   FOR EACH RowErrors:                  
       MESSAGE rowerrors.errordescription
               VIEW-AS ALERT-BOX ERROR BUTTONS OK
               TITLE "Erro - NF".
   END.
END.

/* Caso ocorreu problema nas validaá‰es, n∆o continua o processo */
IF NOT l-proc-ok-aux THEN
   UNDO, LEAVE.

FIND FIRST wt-docto WHERE
           wt-docto.seq-wt-docto = i-seq-wt-docto NO-ERROR.

ASSIGN wt-docto.nr-tabpre = ''.
    

FOR EACH tt-it-nota-fisc WHERE
         tt-it-nota-fisc.cod-estabel = tt-nota-fisc.cod-estabel AND
         tt-it-nota-fisc.serie = tt-nota-fisc.serie AND
         tt-it-nota-fisc.nat-oper = tt-nota-fisc.nat-oper AND
         tt-it-nota-fisc.dt-emis-nota = tt-nota-fisc.dt-emis-nota NO-LOCK.

    /* Limpar a tabela de erros em todas as BOS */
    RUN emptyRowErrors IN h-bodi317in.

    RUN inicializaAcompanhamento      IN h-bodi317sd.
    RUN criaWtItDocto IN h-bodi317sd (INPUT  "",
                                      INPUT  "",
                                      INPUT  tt-it-nota-fisc.nr-seq-fat,
                                      INPUT  tt-it-nota-fisc.it-codigo, 
                                      INPUT  tt-it-nota-fisc.cod-refer,
                                      INPUT  tt-it-nota-fisc.nat-oper,    
                                      OUTPUT i-seq-wt-it-docto,
                                      OUTPUT l-proc-ok-aux).

    RUN finalizaAcompanhamento IN h-bodi317sd.

    /* Busca poss°veis erros que ocorreram nas validaá‰es */
    RUN devolveErrosbodi317sd IN h-bodi317sd (OUTPUT c-ultimo-metodo-exec,
                                              OUTPUT TABLE RowErrors).

    /* Pesquisa algum erro ou advertància que tenha ocorrido */
    FIND FIRST RowErrors NO-LOCK NO-ERROR.

    /* Caso tenha achado algum erro ou advertància, mostra em tela */
    IF AVAIL RowErrors THEN DO.
       FOR EACH RowErrors:
           MESSAGE rowerrors.errordescription 
               VIEW-AS ALERT-BOX ERROR BUTTONS OK
               TITLE 'Erro - Itens NF'.
       END.
    END.

    /* Caso ocorreu problema nas validaá‰es, n∆o continua o processo */
    IF NOT l-proc-ok-aux THEN
       UNDO, LEAVE.

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
    FIND FIRST RowErrors NO-LOCK NO-ERROR.

    /* Caso tenha achado algum erro ou advertància, mostra em tela */
    IF AVAIL RowErrors THEN DO.
       FOR EACH RowErrors:
           MESSAGE rowerrors.errordescription 
               VIEW-AS ALERT-BOX ERROR BUTTONS OK
               TITLE 'Erro - Itens NF'.
       END.
    END.

    /* Caso ocorreu problema nas validaá‰es, n∆o continua o processo */
    IF NOT l-proc-ok-aux THEN
       UNDO, LEAVE.
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

