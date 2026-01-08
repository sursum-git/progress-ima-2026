DEF TEMP-TABLE tt-aux 
    FIELD c-linha AS CHAR.

// Defini‡äes da tabela tt-mensagem.
{utp/utapi019.i}

DEFINE INPUT  PARAMETER p-nr-pedido LIKE ped-venda.nr-pedido.
DEFINE OUTPUT PARAMETER TABLE FOR tt-mensagem.

DEF VAR c-loc-entrega    AS CHAR.
DEF VAR c-cifra          AS CHAR.
DEF VAR da-dt-chegada    AS DATE.

FIND ped-venda WHERE
     ped-venda.nr-pedido = p-nr-pedido NO-LOCK NO-ERROR.

RUN pi-imprime-pedido.


PROCEDURE pi-imprime-pedido.
    DEF VAR i-nr-seq         AS INTEGER.
    DEF VAR c-desc-condpag   AS CHAR FORMAT "x(50)".
    DEF VAR de-tot-ped       AS DEC FORMAT ">>>,>>>,>>9.9999". 
    DEF VAR c-tp-pedido      AS CHAR.
    DEF VAR c-entrega        AS CHAR.
    DEF VAR c-meses          AS CHAR INIT "JAN,FEV,MAR,ABR,MAI,JUN,JUL,AGO,SET,OUT,NOV,DEZ".

    FIND ped-venda-ext WHERE
         ped-venda-ext.cod-estabel = ped-venda.cod-estabel AND
         ped-venda-ext.nr-pedido = ped-venda.nr-pedido NO-LOCK NO-ERROR.

    FIND cond-pagto WHERE
         cond-pagto.cod-cond-pag = ped-venda.cod-cond-pag NO-LOCK NO-ERROR.

    IF AVAIL cond-pagto THEN
       ASSIGN c-desc-condpag = cond-pagto.descricao.
    ELSE DO.
       FOR EACH cond-ped OF ped-venda NO-LOCK.
           IF cond-ped.nr-dias <> 0 THEN
              ASSIGN c-desc-condpag = IF c-desc-condpag = ''
                                      THEN STRING(cond-ped.nr-dias)
                                      ELSE c-desc-condpag + "," + STRING(cond-ped.nr-dias). 
           ELSE
              ASSIGN c-desc-condpag = IF c-desc-condpag = ''
                                      THEN STRING(cond-ped.data-pagto)
                                      ELSE c-desc-condpag + "," + STRING(cond-ped.data-pagto). 
       END.
       ASSIGN c-desc-condpag = c-desc-condpag + " DD".
    END.

    IF ped-venda.mo-codigo = 0 THEN
       ASSIGN c-cifra = 'R$: '.
    ELSE
       ASSIGN c-cifra = 'US$: '.

    IF ped-venda.tp-pedido = 'PE' THEN
       ASSIGN c-entrega = 'IMEDIATO'.
    ELSE DO.
        FIND pp-container WHERE
             pp-container.nr-container = ped-venda-ext.nr-container NO-LOCK NO-ERROR.
        ASSIGN da-dt-chegada = pp-container.dt-prev-chegada + 7.

       ASSIGN c-entrega = '1a Quinzena / ' + ENTRY(MONTH(da-dt-chegada),c-meses).
       IF DAY(da-dt-chegada) > 15 THEN
          ASSIGN c-entrega = REPLACE(c-entrega,"1a","2a").
    END.

    ASSIGN c-tp-pedido = IF ped-venda.tp-pedido = 'PI' 
                         THEN "Pedido Importa‡Æo"
                         ELSE "Pronta Entrega".

    FIND emitente WHERE
         emitente.nome-abrev = ped-venda.nome-abrev NO-LOCK NO-ERROR.

    FIND cont-emit OF emitente WHERE
         cont-emit.area = 'COMERCIAL' NO-LOCK NO-ERROR.
    IF NOT AVAIL cont-emit THEN RETURN 'ADM-ERROR'.

    FIND repres WHERE
         repres.nome-abrev = ped-venda.no-ab-reppri NO-LOCK.


    FIND FIRST loc-entr WHERE
               loc-entr.nome-abrev = emitente.nome-abrev NO-LOCK NO-ERROR.
    IF AVAIL loc-entr THEN
       ASSIGN c-loc-entrega = loc-entr.endereco + ", " + 
                              loc-entr.bairro + ", " + 
                              loc-entr.cidade + " - " + 
                              loc-entr.estado.

    IF SEARCH("layout\ped_web_1.txt") <> ? THEN DO.
       INPUT FROM VALUE(SEARCH("layout\ped_web_1.txt")) NO-ECHO CONVERT TARGET "ibm850".
       REPEAT.
           CREATE tt-aux.
           IMPORT UNFORMATTED c-linha.
    
           ASSIGN tt-aux.c-linha = REPLACE(tt-aux.c-linha,"#nr_pedido",ped-venda.nr-pedcli).
           ASSIGN tt-aux.c-linha = REPLACE(tt-aux.c-linha,"#nome_cliente",ped-venda.nome-abrev).
           ASSIGN tt-aux.c-linha = REPLACE(tt-aux.c-linha,"#nome_ab_rep",ped-venda.no-ab-reppri).
           ASSIGN tt-aux.c-linha = REPLACE(tt-aux.c-linha,"#transportadora",ped-venda.nome-transp).
           ASSIGN tt-aux.c-linha = REPLACE(tt-aux.c-linha,"#tp_ped",c-tp-pedido).
           ASSIGN tt-aux.c-linha = REPLACE(tt-aux.c-linha,"#dt_implant",STRING(ped-venda.dt-implant,"99/99/9999")).
           ASSIGN tt-aux.c-linha = REPLACE(tt-aux.c-linha,"#cidade",ped-venda.cidade).
           ASSIGN tt-aux.c-linha = REPLACE(tt-aux.c-linha,"#cond_pagto",c-desc-condpag).
           ASSIGN tt-aux.c-linha = REPLACE(tt-aux.c-linha,"#tp_frete",ped-venda-ext.tp-frete).
           ASSIGN tt-aux.c-linha = REPLACE(tt-aux.c-linha,"#entrega",c-entrega).
           ASSIGN tt-aux.c-linha = REPLACE(tt-aux.c-linha,"#local_entrega",c-loc-entrega).
       END.
       INPUT CLOSE.
    END.

    FOR EACH ped-item OF ped-venda NO-LOCK.

        FIND item WHERE
             item.it-codigo = ped-item.it-codigo NO-LOCK NO-ERROR.

        IF SEARCH("layout\ped_web_2.txt") <> ? THEN DO.
           INPUT FROM VALUE(SEARCH("layout\ped_web_2.txt")) NO-ECHO CONVERT TARGET "ibm850".
           REPEAT.
               CREATE tt-aux.
               IMPORT UNFORMATTED c-linha.
    
               ASSIGN tt-aux.c-linha = REPLACE(tt-aux.c-linha,"#cod_item",ped-item.it-codigo).
               ASSIGN tt-aux.c-linha = REPLACE(tt-aux.c-linha,"#desc_item",item.desc-item).
               ASSIGN tt-aux.c-linha = REPLACE(tt-aux.c-linha,"#cod_refer",ped-item.cod-refer).
               ASSIGN tt-aux.c-linha = REPLACE(tt-aux.c-linha,"#qt_pedida",STRING(ped-item.qt-pedida,">>,>>9.99")).
               ASSIGN tt-aux.c-linha = REPLACE(tt-aux.c-linha,"#un",item.un).
               ASSIGN tt-aux.c-linha = REPLACE(tt-aux.c-linha,"#vl_un",c-cifra + STRING(ped-item.vl-preori,">>,>>9.99")).
               ASSIGN tt-aux.c-linha = REPLACE(tt-aux.c-linha,"#vl_tot_item",c-cifra + STRING(ped-item.qt-pedida * ped-item.vl-preori,">>,>>9.99")).
           END.
           INPUT CLOSE.
        END.
        ASSIGN de-tot-ped = de-tot-ped + (ped-item.qt-pedida * ped-item.vl-preori).
    END.

    IF SEARCH("layout\ped_web_3.txt") <> ? THEN DO.
       INPUT FROM VALUE(SEARCH("layout\ped_web_3.txt")) NO-ECHO CONVERT TARGET "ibm850".
       REPEAT.
           CREATE tt-aux.
           IMPORT UNFORMATTED c-linha.
    
           ASSIGN tt-aux.c-linha = REPLACE(tt-aux.c-linha,"#vl-tot-ped",c-cifra + STRING(de-tot-ped,">>>,>>9.99")).
       END.
       INPUT CLOSE.
    END.

    ASSIGN i-nr-seq = 0.
    FOR EACH tt-aux.
        ASSIGN i-nr-seq = i-nr-seq + 1.
        CREATE tt-mensagem.
        ASSIGN tt-mensagem.seq-mensagem = i-nr-seq
               tt-mensagem.mensagem     = tt-aux.c-linha.
    END.
    INPUT CLOSE.
END.

