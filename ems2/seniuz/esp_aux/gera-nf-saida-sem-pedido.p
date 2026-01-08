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
    DEF VAR c-seg-usuario        AS CHAR   NO-UNDO.
    DEF VAR c-nome-abrev         AS CHAR   NO-UNDO.   
    DEF VAR c-nat-operacao       AS CHAR   NO-UNDO.
    DEF VAR c-cod-canal-venda    AS CHAR   NO-UNDO.
    DEF VAR i-seq-wt-docto       AS INT    NO-UNDO.
    DEF VAR i-seq-wt-it-docto    AS INT    NO-UNDO.

    DEF VAR i-nr-seq             AS INT    NO-UNDO.

    DEF VAR c-serie              AS CHAR   NO-UNDO.
    DEF VAR c-natur-oper         LIKE nota-fiscal.nat-oper.

    DEF VAR c-cod-estabel     LIKE estabelec.cod-estabel.
    DEF VAR c-cliente         LIKE emitente.nome-abrev.

    /* Temp-Table tt-erro Definitions ---*/
    {method/dbotterr.i}

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

    DEF TEMP-TABLE tt-nota-fisc LIKE nota-fiscal.
    DEF TEMP-TABLE tt-it-nota-fisc LIKE it-nota-fisc.

    DEFINE TEMP-TABLE wt-notas-geradas
           FIELD rw-nota-fiscal AS ROWID.

    /* Definicao da tabela temporaria tt-notas-geradas, include {dibo/bodi317ef.i1} */
    DEF TEMP-TABLE tt-notas-geradas NO-UNDO
        FIELD rw-nota-fiscal AS   ROWID
        FIELD nr-nota        LIKE nota-fiscal.nr-nota-fis
        FIELD seq-wt-docto   LIKE wt-docto.seq-wt-docto.

    /* Definiá∆o de um buffer para tt-notas-geradas */
    DEF BUFFER b-tt-notas-geradas for tt-notas-geradas.

    INPUT FROM c:\temp\itens.txt.
    REPEAT.
        CREATE tt-itens.
        IMPORT tt-itens.
    END.
    INPUT CLOSE.

    FOR EACH tt-itens NO-LOCK.
        FIND ITEM WHERE
             ITEM.it-codigo = tt-itens.it-codigo NO-LOCK NO-ERROR.

        FIND tt-nota-fisc WHERE
             tt-nota-fisc.cod-estabel = c-cod-estabel AND 
             tt-nota-fisc.serie = c-serie AND
             tt-nota-fisc.nat-oper = c-natur-oper AND
             tt-nota-fisc.dt-emis-nota = tt-itens.data NO-LOCK NO-ERROR.

        IF NOT AVAIL tt-nota-fisc THEN DO.
           CREATE tt-nota-fisc.
           ASSIGN tt-nota-fisc.cod-estabel = c-cod-estabel  
                  tt-nota-fisc.serie = c-serie
                  tt-nota-fisc.nat-oper = c-natur-oper
                  tt-nota-fisc.dt-emis-nota = tt-itens.data. 

           ASSIGN i-nr-seq = 0.
        END.

        ASSIGN i-nr-seq = i-nr-seq + 10.
        
        CREATE tt-it-nota-fisc.
        ASSIGN tt-it-nota-fisc.nr-seq-fat = i-nr-seq
               tt-it-nota-fisc.cod-estabel = tt-nota-fisc.cod-estabel  
               tt-it-nota-fisc.serie = tt-nota-fisc.serie
               tt-it-nota-fisc.nat-oper = tt-nota-fisc.nat-oper
               tt-it-nota-fisc.dt-emis-nota = tt-nota-fisc.dt-emis-nota
               tt-it-nota-fisc.it-codigo = tt-itens.it-codigo
               tt-it-nota-fisc.qt-faturada[1] = tt-itens.quantidade
               tt-it-nota-fisc.vl-preuni = tt-itens.valor / tt-itens.quantidade.
    END.
    
    FOR EACH tt-nota-fisc NO-LOCK. 
        FIND FIRST nota-fiscal WHERE
                   nota-fiscal.cod-estabel = tt-nota-fisc.cod-estabel  AND
                   nota-fiscal.dt-emis-nota = tt-nota-fisc.dt-emis-nota
                   NO-LOCK NO-ERROR.
        IF AVAIL nota-fiscal THEN DO:
           MESSAGE "J† Existe Nota Fiscal para a Data " tt-nota-fisc.dt-emis-nota SKIP
                   "Deseja Continuar ? " 
                   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                   UPDATE l-continua AS LOGICAL.

           IF NOT l-continua THEN
              RETURN "ADM-ERROR".
        END.
    END.


    /* Inicializaá∆o das BOS para C†lculo */
    RUN dibo/bodi317in.p PERSISTENT SET h-bodi317in.
    RUN inicializaBOS IN h-bodi317in (OUTPUT h-bodi317pr,
                                      OUTPUT h-bodi317sd,     
                                      OUTPUT h-bodi317im1bra,
                                      OUTPUT h-bodi317va).

    FOR EACH tt-nota-fisc NO-LOCK.
        /* Limpar a tabela de erros em todas as BOS */
        RUN emptyRowErrors        IN h-bodi317in.
    
        /* Cria o registro WT-DOCTO para o pedido */
        RUN criaWtDocto IN h-bodi317sd (INPUT  c-seg-usuario,
                                        INPUT  tt-nota-fisc.cod-estabel,
                                        INPUT  tt-nota-fisc.serie,
                                        INPUT  "1", 
                                        INPUT  c-cliente,
                                        INPUT  "",
                                        INPUT  2,    
                                        INPUT  9999, 
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
    
        FOR EACH tt-it-nota-fisc WHERE
                 tt-it-nota-fisc.cod-estabel = tt-nota-fisc.cod-estabel AND
                 tt-it-nota-fisc.serie = tt-nota-fisc.serie AND
                 tt-it-nota-fisc.nat-oper = tt-nota-fisc.nat-oper AND
                 tt-it-nota-fisc.dt-emis-nota = tt-nota-fisc.dt-emis-nota NO-LOCK.

            /* Limpar a tabela de erros em todas as BOS */
            RUN emptyRowErrors        IN h-bodi317in.

            RUN inicializaAcompanhamento      IN h-bodi317sd.
            RUN criaWtItDocto IN h-bodi317sd (INPUT  "",
                                              INPUT  "",
                                              INPUT  tt-it-nota-fisc.nr-seq-fat,
                                              INPUT  tt-it-nota-fisc.it-codigo, 
                                              INPUT  "",
                                              INPUT  tt-it-nota-fisc.nat-oper,    
                                              OUTPUT i-seq-wt-it-docto,
                                              OUTPUT l-proc-ok-aux).

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
                 wt-it-docto.seq-wt-docto = i-seq-wt-docto NO-LOCK.
    
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
    
        FOR EACH tt-notas-geradas.
            FIND FIRST nota-fiscal WHERE
                 ROWID(nota-fiscal) = tt-notas-geradas.rw-nota-fiscal
                 NO-ERROR.

            MESSAGE nota-fiscal.nr-nota-fis
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.


        /* Elimina o handle do programa bodi317ef */
        DELETE PROCEDURE h-bodi317ef.
    
        LEAVE.
    END.
            
    /* Finalizaá∆o das BOS utilizada no c†lculo */
    RUN finalizaBOS in h-bodi317in.
