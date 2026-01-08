/* Definiá∆o da vari†veis */
DEF VAR h-bodi317pr          AS HANDLE NO-UNDO.
DEF VAR h-bodi317sd          AS HANDLE NO-UNDO.
DEF VAR h-bodi317im1bra      AS HANDLE NO-UNDO.
DEF VAR h-bodi317va          AS HANDLE NO-UNDO.
DEF VAR h-bodi317in          AS HANDLE NO-UNDO.
DEF VAR h-bodi317ef          AS HANDLE NO-UNDO.
DEF VAR l-proc-ok-aux         AS LOG    NO-UNDO.
DEF VAR c-ultimo-metodo-exec AS CHAR   NO-UNDO.
DEF VAR c-cod-estabel        AS CHAR   NO-UNDO.
DEF VAR c-serie              AS CHAR   NO-UNDO.
DEF VAR da-dt-emis-nota      AS DATE   NO-UNDO.
DEF VAR da-dt-base-dup       AS DATE   NO-UNDO.
DEF VAR da-dt-prvenc         AS DATE   NO-UNDO.
DEF VAR c-seg-usuario        AS CHAR   NO-UNDO.
DEF VAR c-nome-abrev         AS CHAR   NO-UNDO.   
DEF VAR c-nr-pedcli          AS CHAR   NO-UNDO.
DEF VAR c-nat-operacao       AS CHAR   NO-UNDO.
DEF VAR c-cod-canal-venda    AS CHAR   NO-UNDO.
DEF VAR i-seq-wt-docto       AS INT    NO-UNDO.

DEF VAR c-clibnf      AS   CHAR FORMAT "x(200)".
DEF VAR c-cliadq      AS   CHAR FORMAT "x(200)".

/* Def temp-table de erros. Ela tbÇm est† definida na include dbotterr.i */
DEF TEMP-TABLE rowerrors NO-UNDO
    FIELD errorsequence    AS INT
    FIELD errornumber      as INT
    FIELD errordescription AS CHAR
    FIELD errorparameters  AS CHAR
    FIELD errortype        AS CHAR
    FIELD errorhelp        AS CHAR
    FIELD errorsubtype     AS CHAR.

/* Definicao da tabela temporaria tt-notas-geradas, include {dibo/bodi317ef.i1} */
DEF TEMP-TABLE tt-notas-geradas NO-UNDO
    FIELD rw-nota-fiscal AS   ROWID
    FIELD nr-nota        LIKE nota-fiscal.nr-nota-fis
    FIELD seq-wt-docto   LIKE wt-docto.seq-wt-docto.

/* Definiá∆o de um buffer para tt-notas-geradas */
DEF BUFFER b-tt-notas-geradas FOR tt-notas-geradas.

DEFINE TEMP-TABLE wt-notas-geradas
       FIELD rw-nota-fiscal AS ROWID.

DEF BUFFER b-nota-fiscal FOR nota-fiscal.

DEF TEMP-TABLE tt-ped-venda LIKE ped-venda.

FIND ped-venda WHERE
     ped-venda.nr-pedcli = '10850' NO-LOCK NO-ERROR.

CREATE tt-ped-venda.
BUFFER-COPY ped-venda TO tt-ped-venda.


FIND FIRST tt-ped-venda NO-LOCK NO-ERROR.
FIND FIRST para-fat NO-LOCK NO-ERROR.

/* Informaá‰es do embarque para c†lculo */
ASSIGN c-seg-usuario     = c-seg-usuario             /* Usu†rio                    */
       c-cod-estabel     = tt-ped-venda.cod-estabel  /* Estabelecimento do pedido  */
       c-serie           = para-fat.serie-pad        /* SÇrie das notas            */
       c-nome-abrev      = tt-ped-venda.nome-abrev   /* Nome abreviado do cliente  */
       c-nr-pedcli       = tt-ped-venda.nr-pedcli    /* Nr pedido do cliente       */
       da-dt-emis-nota   = TODAY                     /* Data de emiss∆o da nota    */
       c-nat-operacao    = ?                         /* Quando Ç ? busca do pedido */
       c-cod-canal-venda = ?.                        /* Quando Ç ? busca do pedido */

FIND ped-venda WHERE
     ped-venda.nome-abrev = tt-ped-venda.nome-abrev AND 
     ped-venda.nr-pedcli = tt-ped-venda.nr-pedcli
     NO-ERROR.

/* Inicializaá∆o das BOS para C†lculo */
RUN dibo/bodi317in.p PERSISTENT SET h-bodi317in.
RUN inicializaBOS IN h-bodi317in (OUTPUT h-bodi317pr,
                                  OUTPUT h-bodi317sd,     
                                  OUTPUT h-bodi317im1bra,
                                  OUTPUT h-bodi317va).

/* In°cio da transaá∆o */
REPEAT TRANS:
    /* Limpar a tabela de erros em todas as BOS */
    RUN emptyRowErrors        IN h-bodi317in.

    /* Cria o registro WT-DOCTO para o pedido */
    RUN criaWtDocto IN h-bodi317sd (INPUT  c-seg-usuario,
                                    INPUT  c-cod-estabel,
                                    INPUT  c-serie,
                                    INPUT  "1", 
                                    INPUT  c-nome-abrev,
                                    INPUT  c-nr-pedcli,
                                    INPUT  1,    
                                    INPUT  9999, 
                                    INPUT  da-dt-emis-nota,
                                    INPUT  0,  
                                    INPUT  c-nat-operacao,
                                    INPUT  c-cod-canal-venda,
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

    IF tt-ped-venda.dt-base-ft <> ? THEN 
       IF AVAIL wt-docto THEN
          ASSIGN wt-docto.dt-base-dup = tt-ped-venda.dt-base-ft.

    /* Limpar a tabela de erros em todas as BOS */
    RUN emptyRowErrors        IN h-bodi317in.

    /* Gera os itens para o pedido, com tela de acompanhamento */
    RUN inicializaAcompanhamento      IN h-bodi317sd.
    RUN geraWtItDoctoComItensDoPedido IN h-bodi317sd (OUTPUT l-proc-ok-aux).
    RUN finalizaAcompanhamento        IN h-bodi317sd.

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

    FOR EACH wt-it-docto WHERE
             wt-it-docto.seq-wt-docto = i-seq-wt-docto NO-LOCK.

        FIND ped-item-res WHERE
             ped-item-res.nr-pedcli  = c-nr-pedcli  AND
             ped-item-res.nome-abrev = c-nome-abrev AND 
             ped-item-res.nr-sequencia = wt-it-docto.nr-sequencia AND 
             ped-item-res.faturado = NO 
             NO-LOCK NO-ERROR.

        IF NOT AVAIL ped-item-res THEN NEXT.

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

        /* Calcula peso Bruto da NF */ 
        FIND FIRST wt-docto WHERE
                   wt-docto.seq-wt-docto = i-seq-wt-docto NO-ERROR.
        FOR EACH ped-item-rom WHERE
                 ped-item-rom.nome-abrev = ped-item-res.nome-abrev AND
                 ped-item-rom.nr-pedcli = ped-item-res.nr-pedcli AND
                 ped-item-rom.nr-sequencia = ped-item-res.nr-sequencia
                 BREAK BY ped-item-rom.nr-volume.
            FIND ob-etiqueta WHERE
                 ob-etiqueta.num-etiqueta = ped-item-rom.num-etiqueta
                 NO-LOCK NO-ERROR.
            ASSIGN wt-docto.peso-bru-tot-inf = wt-docto.peso-bru-tot-inf + ob-etiqueta.peso-bruto.
        END.
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

    LEAVE.
END.
        
/* Finalizaá∆o das BOS utilizada no c†lculo */
RUN finalizaBOS in h-bodi317in.

FOR EACH tt-notas-geradas NO-LOCK:
    CREATE wt-notas-geradas.
    ASSIGN wt-notas-geradas.rw-nota-fiscal = tt-notas-geradas.rw-nota-fiscal.

    FIND FIRST nota-fiscal WHERE
         ROWID(nota-fiscal) = tt-notas-geradas.rw-nota-fiscal
         NO-ERROR.

    FIND natur-oper WHERE
         natur-oper.nat-oper = nota-fiscal.nat-oper NO-LOCK NO-ERROR.

    FIND ped-venda WHERE
         ped-venda.nome-abrev = nota-fiscal.nome-ab-cli AND 
         ped-venda.nr-pedcli = nota-fiscal.nr-pedcli
         NO-LOCK NO-ERROR.

    FIND ped-venda-ext WHERE
         ped-venda-ext.nr-pedido = ped-venda.nr-pedido NO-LOCK NO-ERROR.

    IF AVAIL ped-venda-ext AND
       ped-venda-ext.tp-frete <> 'Cif Destaque NF' THEN
       ASSIGN nota-fiscal.vl-frete = 0.

    IF natur-oper.log-oper-triang THEN DO.
       FIND emitente WHERE
            emitente.nome-abrev = nota-fiscal.nome-ab-cli NO-LOCK NO-ERROR.
       ASSIGN c-cliadq = emitente.nome-emit + " - " +
                         emitente.endereco + " - " +
                         emitente.bairro + " - CEP: " +
                         emitente.cep + " - " +
                         emitente.cidade + " - " +
                         emitente.estado + " - CNPJ: " +
                         emitente.cgc + " - I.E.: " +
                         emitente.ins-estadual.

       /* Busca a Nota Beneficiada */
       FIND FIRST b-nota-fiscal WHERE
                  b-nota-fiscal.nr-nota-fis >= nota-fiscal.nr-nota-fis AND
                  b-nota-fiscal.cod-estabel = nota-fiscal.cod-estabel AND
                  b-nota-fiscal.serie = nota-fiscal.serie AND
                  b-nota-fiscal.nome-ab-cli = ped-venda.nome-abrev-tri AND
                  b-nota-fiscal.vl-tot-nota = nota-fiscal.vl-tot-nota
                  NO-ERROR. 

       /* Modifica Observaá∆o da Nota que Adquiriu */
       ASSIGN nota-fiscal.observ-nota = "NF. REF. ∑ NF. " + b-nota-fiscal.nr-nota-fis + " DE " +
                                        STRING(b-nota-fiscal.dt-emis,"99/99/9999") +  ' " ' +
                                        nota-fiscal.observ-nota.

       /* Modifica Observaá∆o da Nota Beneficada */
       ASSIGN b-nota-fiscal.observ-nota = "NF. REF. ∑ NF. " + nota-fiscal.nr-nota-fis + " DE " +
                                           STRING(nota-fiscal.dt-emis,"99/99/9999") + ' " ' +
                                           b-nota-fiscal.observ-nota.

       FIND emitente WHERE
            emitente.nome-abrev = b-nota-fiscal.nome-ab-cli NO-LOCK NO-ERROR.
       ASSIGN c-clibnf = emitente.nome-emit + " - " +
                         emitente.endereco + " - " +
                         emitente.bairro + " - CEP: " +
                         emitente.cep + " - " +
                         emitente.cidade + " - " +
                         emitente.estado + " - CNPJ: " +
                         emitente.cgc + " - I.E.: " +
                         emitente.ins-estadual.

       IF INDEX(nota-fiscal.observ-nota,"#CLIBNF") > 0 THEN
          ASSIGN nota-fiscal.observ-nota = REPLACE(nota-fiscal.observ-nota,"#CLIBNF",TRIM(c-clibnf)).

       IF INDEX(b-nota-fiscal.observ-nota,"#CLIADQ") > 0 THEN
          ASSIGN b-nota-fiscal.observ-nota = REPLACE(b-nota-fiscal.observ-nota,"#CLIADQ",TRIM(c-cliadq)).
    END.

    IF AVAIL ped-venda-ext AND ped-venda-ext.compl-observ <> '' THEN 
       ASSIGN nota-fiscal.observ-nota = ped-venda-ext.compl-observ + nota-fiscal.observ-nota.

    ASSIGN nota-fiscal.observ-nota = nota-fiscal.observ-nota + 
                                     " Pedido Interno: "  + STRING(ped-venda.nr-pedido,">>>>>>9") +
                                     " Pedido Cliente: " + ped-venda.nr-pedcli +
                                     " Pedido Repres: "  + ped-venda.nr-pedrep.

    IF nota-fiscal.nome-tr-red <> "" THEN DO:
       FIND transporte WHERE
            transporte.nome-abrev = nota-fiscal.nome-tr-red NO-LOCK NO-ERROR.
    
       IF AVAIL transporte THEN DO:
          ASSIGN nota-fiscal.observ-nota = nota-fiscal.observ-nota + 
                 "REDESPACHO por conta do " + "EMITENTE" +
                 " Nome/Raz∆o Social: " + transporte.nome +
                 " Endereáo: "          + transporte.endereco +
                 " Munic°pio: "         + transporte.cidade +
                 " UF: "                + transporte.estado.
       END.
    END.
END.



