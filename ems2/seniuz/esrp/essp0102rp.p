/* Programa de controle de versío e seguranªa do Datasul EMS */
{include/i-prgvrs.i ESSP0102RP 2.04.00.001}

/* Definiá∆o das Includes para o Reporte */
{cdp/cdcfgman.i}
{cdp/cd0666.i}
{cpp/cpapi001.i}
{cpp/cpapi018.i}
{cpp/cpapi301.i}
{cep/ceapi001.i}

DEFINE TEMP-TABLE tt-param NO-UNDO
    FIELD destino          AS INTEGER
    FIELD arquivo          AS CHAR FORMAT "x(35)"
    FIELD usuario          AS CHAR FORMAT "x(12)"
    FIELD data-exec        AS DATE
    FIELD hora-exec        AS INTEGER
    FIELD cod-estab-ini    LIKE ob-etiqueta.cod-estabel
    FIELD cod-estab-fim    LIKE ob-etiqueta.cod-estabel
    FIELD nr-ob-ini        LIKE ob-etiqueta.nr-ob
    FIELD nr-ob-fim        LIKE ob-etiqueta.nr-ob
    FIELD emissao-ini      LIKE ob-etiqueta.dt-emissao
    FIELD emissao-fim      LIKE ob-etiqueta.dt-emissao
    FIELD carro-ini        LIKE ob-etiqueta.nr-carro
    FIELD carro-fim        LIKE ob-etiqueta.nr-carro
    FIELD acondic-ini      LIKE ob-etiqueta.acondic
    FIELD acondic-fim      LIKE ob-etiqueta.acondic
    FIELD sequencia-ini    LIKE ob-etiqueta.nr-sequencia
    FIELD sequencia-fim    LIKE ob-etiqueta.nr-sequencia
    FIELD e-mail           AS   CHAR.

DEFINE TEMP-TABLE tt-digita NO-UNDO
    FIELD ordem             AS INTEGER   FORMAT ">>>>9"
    FIELD exemplo           AS CHARACTER FORMAT "x(30)"
    INDEX id ordem.

/* Parametros de entrada logica obrigatoria */
DEF TEMP-TABLE tt-raw-digita
    FIELD raw-digita       AS RAW.

DEF INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEF INPUT PARAMETER TABLE FOR tt-raw-digita.

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

FOR EACH tt-raw-digita.
    CREATE tt-digita.
    RAW-TRANSFER tt-raw-digita.raw-digita TO tt-digita.
END.

DEF TEMP-TABLE tt-reporte
    FIELD cod-estabel   LIKE ord-prod.cod-estabel
    FIELD it-codigo     LIKE ord-prod.it-codigo
    FIELD cod-refer     LIKE ord-prod.cod-refer
    FIELD nr-lote       LIKE ord-prod.lote
    FIELD dt-emissao    LIKE ord-prod.dt-emissao
    FIELD qtd-acabado   LIKE ord-prod.qt-produzida
    FIELD qtd-retalho-m LIKE ord-prod.qt-produzida      /* Retalho em Metros */
    FIELD rw-etiquetas  AS CHAR
    INDEX ch-item IS PRIMARY cod-estabel it-codigo cod-refer nr-lote dt-emissao.

DEF TEMP-TABLE tt-pcp-reporte
    FIELD it-codigo    LIKE ord-prod.it-codigo
    FIELD cod-refer    LIKE ord-prod.cod-refer
    FIELD nr-lote      LIKE ord-prod.lote
    FIELD dt-emissao   LIKE ord-prod.dt-emissao
    FIELD num-progr    LIKE ob-pcp.num-progr
    FIELD quantidade   LIKE ordem-benefic.quantidade
    INDEX ch-item IS PRIMARY  it-codigo cod-refer nr-lote dt-emissao num-progr.

DEF TEMP-TABLE tt-trf
    FIELD cod-estabel  LIKE ord-prod.cod-estabel 
    FIELD it-codigo    LIKE ord-prod.it-codigo
    FIELD cod-refer    LIKE ord-prod.cod-refer
    FIELD nr-lote      LIKE ord-prod.lote
    FIELD quantidade   LIKE ordem-benefic.quantidade
    FIELD tipo-trans   LIKE movto-estoq.tipo-trans
    FIELD rw-etiquetas AS   CHAR
    INDEX ch-item IS PRIMARY it-codigo cod-refer nr-lote.

DEF TEMP-TABLE tt-retalho
    FIELD cod-estabel  LIKE ob-etiqueta.cod-estabel
    FIELD num-etiqueta LIKE ob-etiqueta.num-etiqueta
    FIELD it-codigo    LIKE ob-etiqueta.it-codigo
    FIELD nr-ord-prod  LIKE ord-prod.nr-ord-prod
    FIELD qtd-ordem    LIKE ord-prod.qt-ordem
    FIELD qtd-retalho  LIKE ord-prod.qt-produzida
    FIELD reportado    AS LOG INIT NO
    INDEX ch-ordem IS PRIMARY nr-ord-prod.

DEF BUFFER b-etiqueta  FOR ob-etiqueta.
DEF BUFFER b-ordem-benefic FOR ordem-benefic.
DEF BUFFER b-tt-retalho FOR tt-retalho.    

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

DEF VAR h-acomp AS HANDLE NO-UNDO.

DEF VAR i-ndx          AS   INT.
DEF VAR i-ct           AS   INT.
DEF VAR i-cont         AS   INT.
DEF VAR c-mensagem     AS   CHAR.
DEF VAR c-item-cru     LIKE estrutura.it-codigo.
DEF VAR c-item-msg     LIKE ord-prod.it-codigo.
DEF VAR de-sld-estoque LIKE saldo-estoq.qtidade-atu.
DEF VAR de-qtd-requis  LIKE movto-estoq.quantidade.
DEF VAR de-qtd-item    LIKE estrutura.qtd-item.
DEF VAR de-qtd-usada   LIKE estrutura.quant-usada.
DEF VAR de-perc-pron   AS   DEC.
DEF VAR de-tot-aca     AS   DEC.
DEF VAR c-erro         AS   CHAR FORMAT "x(100)".
DEF VAR c-justif       AS   CHAR.

ASSIGN c-justif = 'Movto Transformaá∆o do Item'.

FIND FIRST param-cp NO-LOCK NO-ERROR.

FIND FIRST ob-param NO-LOCK NO-ERROR.
IF AVAIL ob-param THEN
   ASSIGN tt-param.e-mail = ob-param.e-mails-erro-rep.

FIND ocorrenc WHERE
     ocorrenc.descricao = 'ESSP0102' NO-LOCK NO-ERROR.
IF AVAIL ocorrenc THEN NEXT.

CREATE ocorrenc.
ASSIGN ocorrenc.oc-codigo = 0102
       ocorrenc.descricao = "ESSP0102".

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
{utp/ut-liter.i Imprimindo *}
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

/* Incia processo de reporte */
FOR EACH ob-etiqueta WHERE
         ob-etiqueta.cod-estab    >= tt-param.cod-estab-ini AND
         ob-etiqueta.cod-estab    <= tt-param.cod-estab-fim AND
         ob-etiqueta.nr-ob        >= tt-param.nr-ob-ini     AND
         ob-etiqueta.nr-ob        <= tt-param.nr-ob-fim     AND
         ob-etiqueta.dt-emissao   >= tt-param.emissao-ini   AND
         ob-etiqueta.dt-emissao   <= tt-param.emissao-fim   AND
         ob-etiqueta.nr-carro     >= tt-param.carro-ini     AND
         ob-etiqueta.nr-carro     <= tt-param.carro-fim     AND
         ob-etiqueta.acondic      >= tt-param.acondic-ini   AND
         ob-etiqueta.acondic      <= tt-param.acondic-fim   AND
         ob-etiqueta.situacao     >= 3 AND
         ob-etiqueta.nr-reporte    = 0 AND
         ob-etiqueta.quantidade    > 0 SHARE-LOCK,
    FIRST ordem-benefic OF ob-etiqueta NO-LOCK.
    
    RUN pi-acompanhar IN h-acomp (INPUT ob-etiqueta.num-etiqueta). 

    IF ob-etiqueta.tipo-ordem = 4 THEN DO.
       ASSIGN ob-etiqueta.nr-reporte = 1.
       NEXT.
    END.

    IF ob-etiqueta.tipo-ordem = 1 THEN DO.
       IF ob-etiqueta.nr-lote <> 'SC' THEN DO. /* N∆o Ç Retalho */
          FIND FIRST tt-reporte WHERE
                     tt-reporte.cod-estabel = param-cp.cod-estabel AND
                     tt-reporte.it-codigo = ordem-benefic.it-codigo AND
                     tt-reporte.cod-refer = ordem-benefic.cod-refer AND
                     tt-reporte.nr-lote = ob-etiqueta.nr-lote + ordem-benefic.cod-refer AND
                     tt-reporte.dt-emissao = ob-etiqueta.dt-emissao 
                     NO-LOCK NO-ERROR.
        
          IF NOT AVAIL tt-reporte THEN DO.
             CREATE tt-reporte.
             ASSIGN tt-reporte.cod-estabel = param-cp.cod-estabel
                    tt-reporte.it-codigo = ordem-benefic.it-codigo 
                    tt-reporte.cod-refer = ordem-benefic.cod-refer
                    tt-reporte.nr-lote   = ob-etiqueta.nr-lote + ordem-benefic.cod-refer
                    tt-reporte.dt-emissao = ob-etiqueta.dt-emissao.
          END.
          ASSIGN tt-reporte.rw-etiquetas = IF tt-reporte.rw-etiquetas = ""
                                           THEN STRING(ROWID(ob-etiqueta))  
                                           ELSE tt-reporte.rw-etiquetas + ";" + STRING(ROWID(ob-etiqueta)).
          
          ASSIGN tt-reporte.qtd-acabado = tt-reporte.qtd-acabado + ob-etiqueta.quantidade.

          IF ob-etiqueta.nr-ord-prod = 0 THEN DO.
             FOR EACH mov-est-acbd WHERE
                      mov-est-acbd.cod-estab = ob-etiqueta.cod-estab AND
                      mov-est-acbd.data-mov = ob-etiqueta.dt-emissao AND
                      mov-est-acbd.num-lote = ob-etiqueta.nr-ob AND
                      mov-est-acbd.nr-carro = ob-etiqueta.nr-carro AND
                      mov-est-acbd.acondic = ob-etiqueta.acondic AND
                      mov-est-acbd.nr-sequencia = ob-etiqueta.nr-sequencia AND
                      mov-est-acbd.classif = "RT" NO-LOCK.
                 ASSIGN tt-reporte.qtd-retalho-m = tt-reporte.qtd-retalho-m + mov-est-acbd.qtd-defeit.
             END.
          END.

          IF ordem-benefic.num-progr <> 0 THEN DO.
             FIND FIRST tt-pcp-reporte WHERE
                        tt-pcp-reporte.it-codigo  = tt-reporte.it-codigo AND
                        tt-pcp-reporte.cod-refer  = tt-reporte.cod-refer AND
                        tt-pcp-reporte.nr-lote    = tt-reporte.nr-lote AND
                        tt-pcp-reporte.dt-emissao = tt-reporte.dt-emissao AND
                        tt-pcp-reporte.num-progr  = ordem-benefic.num-progr
                        NO-LOCK NO-ERROR.

             IF NOT AVAIL tt-pcp-reporte THEN DO.
                CREATE tt-pcp-reporte.
                ASSIGN tt-pcp-reporte.it-codigo = tt-reporte.it-codigo 
                       tt-pcp-reporte.cod-refer = tt-reporte.cod-refer
                       tt-pcp-reporte.nr-lote = tt-reporte.nr-lote 
                       tt-pcp-reporte.dt-emissao = tt-reporte.dt-emissao
                       tt-pcp-reporte.num-progr = ordem-benefic.num-progr.
             END.
             ASSIGN tt-pcp-reporte.quantidade = tt-pcp-reporte.quantidade + ob-etiqueta.quantidade.
          END.
       END.
       ELSE DO.  /* Ç etiqueta de retalho */
          ASSIGN de-tot-aca = 0
                     i-cont = 0.

          DO i-ct = NUM-ENTRIES(ob-etiqueta.ob-origem,";") TO 1 BY -1.

             IF i-cont > 5 THEN NEXT.  /* reporta as ultimas 5 ob's */

             FIND LAST b-ordem-benefic WHERE
                       b-ordem-benefic.cod-estab = ob-etiqueta.cod-estab AND
                       b-ordem-benefic.nr-ob = INTEGER(ENTRY(i-ct,ob-etiqueta.ob-origem,";"))
                       NO-LOCK NO-ERROR.

             IF NOT AVAIL b-ordem-benefic THEN NEXT.

             FOR EACH b-etiqueta WHERE
                      b-etiqueta.cod-estab = b-ordem-benefic.cod-estab AND
                      b-etiqueta.nr-ob = b-ordem-benefic.nr-ob AND
                      b-etiqueta.nr-reporte > 0 NO-LOCK.

                 IF MONTH(b-etiqueta.dt-emis) <> MONTH(ob-etiqueta.dt-emis) THEN NEXT.

                 FIND LAST movto-estoq WHERE 
                           movto-estoq.nr-reporte = b-etiqueta.nr-reporte AND
                           movto-estoq.esp-docto = 1  /* aca */
                           USE-INDEX nr-reporte NO-LOCK NO-ERROR.
    
                 IF NOT AVAIL movto-estoq THEN NEXT.

                 FIND ord-prod WHERE
                      ord-prod.nr-ord-prod = movto-estoq.nr-ord-prod
                      SHARE-LOCK NO-ERROR.

                 IF ord-prod.valorizada THEN NEXT.

                 IF ord-prod.estado >= 7 THEN DO.
                    IF MONTH(ord-prod.dt-emis) <> MONTH(b-etiqueta.dt-emissao) THEN
                       NEXT.

                    ASSIGN ord-prod.estado = 6.
                 END.

                 FIND tt-retalho WHERE
                      tt-retalho.num-etiqueta = ob-etiqueta.num-etiqueta AND
                      tt-retalho.nr-ord-prod = movto-estoq.nr-ord-prod NO-LOCK NO-ERROR.
                 IF NOT AVAIL tt-retalho THEN DO.
                    CREATE tt-retalho.
                    ASSIGN tt-retalho.cod-estabel = ob-etiqueta.cod-estabel
                           tt-retalho.num-etiqueta = ob-etiqueta.num-etiqueta
                           tt-retalho.nr-ord-prod = movto-estoq.nr-ord-prod
                           tt-retalho.it-codigo = ob-etiqueta.it-codigo.
                 END.

                 ASSIGN tt-retalho.qtd-ordem = tt-retalho.qtd-ordem + movto-estoq.quantidade
                        de-tot-aca = de-tot-aca + movto-estoq.quantidade.

                 ASSIGN i-cont = i-cont + 1.
             END.
          END.

          FOR EACH tt-retalho WHERE
                   tt-retalho.num-etiqueta = ob-etiqueta.num-etiqueta NO-LOCK.

              ASSIGN tt-retalho.qtd-retalho = tt-retalho.qtd-retalho +
                                             ROUND((tt-retalho.qtd-ordem / de-tot-aca) * ob-etiqueta.quantidade,2).
          END.
       END.
    END.
    ELSE DO.
        FIND ob-trf WHERE
             ob-trf.num-trf = ordem-benefic.num-trf AND
             ob-trf.situacao = 1 SHARE-LOCK NO-ERROR.

        /* verifica saida */ 
        IF AVAIL ob-trf THEN DO.
           ASSIGN ob-trf.situacao = 2.

           FIND FIRST tt-trf WHERE
                      tt-trf.cod-estabel = ordem-benefic.cod-estabel AND
                      tt-trf.it-codigo  = ob-trf.it-codigo AND
                      tt-trf.cod-refer  = ob-trf.cod-refer AND
                      tt-trf.nr-lote    = ob-trf.nr-lote + ob-trf.cod-refer AND
                      tt-trf.tipo-trans = 2   /* saida */
                      NO-LOCK NO-ERROR.

           IF NOT AVAIL tt-trf THEN DO.
              CREATE tt-trf.
              ASSIGN tt-trf.cod-estabel = ordem-benefic.cod-estabel
                     tt-trf.it-codigo   = ob-trf.it-codigo 
                     tt-trf.cod-refer   = ob-trf.cod-refer
                     
                     tt-trf.nr-lote     = ob-trf.nr-lote + ob-trf.cod-refer
                     tt-trf.tipo-trans  = 2.  /* saida */
           END.

           IF SUBSTR(ob-trf.char-1,1,1) = "S" THEN DO. /* Ç retrabalho */
              ASSIGN tt-trf.quantidade = ob-trf.dec-1.

              IF SUBSTR(tt-trf.it-codigo,6,1) = '0' THEN  /* cru */
                 ASSIGN tt-trf.nr-lote = "".
           END.
           ELSE DO.
              FOR EACH ob-etq-trf WHERE 
                       ob-etq-trf.num-trf = ob-trf.num-trf NO-LOCK.
                  FIND b-etiqueta WHERE 
                       b-etiqueta.cod-estabel  = ob-etiqueta.cod-estabel AND
                       b-etiqueta.num-etiqueta = ob-etq-trf.num-etiqueta NO-LOCK NO-ERROR.
                  IF AVAIL ob-etiqueta THEN
                     ASSIGN tt-trf.quantidade = tt-trf.quantidade + b-etiqueta.quantidade.
              END.
           END.
        END.


        /* verifica entrada */ 
        FIND FIRST tt-trf WHERE
                   tt-trf.cod-estabel = ob-etiqueta.cod-estabel AND
                   tt-trf.it-codigo   = ob-etiqueta.it-codigo AND
                   tt-trf.cod-refer   = ob-etiqueta.cod-refer AND
                   tt-trf.nr-lote     = ob-etiqueta.nr-lote + ob-etiqueta.cod-refer AND
                   tt-trf.tipo-trans  = 1 /* entrada */
                   NO-LOCK NO-ERROR.

        IF NOT AVAIL tt-trf THEN DO.
           CREATE tt-trf.
           ASSIGN tt-trf.cod-estabel = ob-etiqueta.cod-estabel
                  tt-trf.it-codigo   = ob-etiqueta.it-codigo 
                  tt-trf.cod-refer   = ob-etiqueta.cod-refer
                  tt-trf.nr-lote     = ob-etiqueta.nr-lote + ob-etiqueta.cod-refer
                  tt-trf.tipo-trans  = 1. /* entrada */
        END.
        ASSIGN tt-trf.quantidade = tt-trf.quantidade + ob-etiqueta.quantidade.

        ASSIGN tt-trf.rw-etiquetas = IF tt-trf.rw-etiquetas = ""
                                     THEN STRING(ROWID(ob-etiqueta))  
                                     ELSE tt-trf.rw-etiquetas + ";" + STRING(ROWID(ob-etiqueta)).
    END.
END.

/* Efetua Transaá‰es de Transformaá∆o */
FOR EACH tt-trf NO-LOCK 
         BY tt-trf.tipo-trans.

    RUN esapi/cria-movto-estoq.p (INPUT tt-trf.cod-estabel,
                                  INPUT tt-trf.it-codigo,
                                  INPUT tt-trf.cod-refer,
                                  INPUT tt-trf.nr-lote, 
                                  INPUT tt-trf.quantidade,
                                  INPUT 6,
                                  INPUT tt-trf.tipo-trans,
                                  INPUT c-justif,
                                  OUTPUT c-erro).

    IF RETURN-VALUE = "OK" THEN DO.
       DO i-ndx = 1 TO NUM-ENTRIES(tt-trf.rw-etiquetas,";").
          FIND ob-etiqueta WHERE
               ROWID(ob-etiqueta) = TO-ROWID(ENTRY(i-ndx,tt-trf.rw-etiquetas,";"))
               SHARE-LOCK NO-ERROR.
          IF AVAIL ob-etiqueta THEN DO.
             ASSIGN ob-etiqueta.nr-reporte = 1
                    ob-etiqueta.nr-ord-prod = 1.
    
             FIND ordem-benefic OF ob-etiqueta SHARE-LOCK NO-ERROR.
             IF AVAIL ordem-benefic AND ordem-benefic.situacao = 4 THEN
                ASSIGN ordem-benefic.situacao = 5.
          END.
       END.
    END.
    ELSE DO.
       ASSIGN c-mensagem = "N∆o foi poss°vel criar as TD's para a Transformaá∆o do Item " 
                           + tt-trf.it-codigo + " Referencia " + tt-trf.cod-refer + CHR(13) + 
                           c-erro.

       RUN esapi/esapi002.p (INPUT "controle.acabado@teartextil.com.br", /* e-mail remetente */
                             INPUT "antonio.souza@teartextil.com.br," + tt-param.e-mail, /* e-mail destinat†rio */
                             INPUT "Erro no Reporte Autom†tico" , /* Assunto */
                             INPUT c-mensagem, /* Mensagem */
                             INPUT "", /*arquivo anexo*/
                             INPUT NO). /* Mostra Erros */
    END.
END.


/* Efetua Transaá‰es de Reporte */
FOR EACH tt-reporte WHERE
         tt-reporte.qtd-acabado > 0 NO-LOCK.

    FIND item WHERE
         item.it-codigo = tt-reporte.it-codigo NO-LOCK NO-ERROR.
    
    FIND FIRST item-uni-estab WHERE
               item-uni-estab.cod-estabel = tt-reporte.cod-estabel AND
               item-uni-estab.it-codigo = item.it-codigo NO-LOCK NO-ERROR.
    
    FIND lin-prod WHERE
         lin-prod.cod-estabel = item-uni-estab.cod-estabel AND
         lin-prod.nr-linha = item-uni-estab.nr-linha NO-LOCK NO-ERROR.
    IF NOT AVAIL lin-prod THEN NEXT.

    FIND LAST ord-prod WHERE 
              ord-prod.it-codigo = tt-reporte.it-codigo AND 
              ord-prod.cod-refer = tt-reporte.cod-refer AND
              STRING(YEAR(ord-prod.dt-emissao)) + 
              STRING(MONTH(ord-prod.dt-emissao),"99") = STRING(YEAR(tt-reporte.dt-emissao)) + 
                                                        STRING(MONTH(tt-reporte.dt-emissao),"99") AND 
              ord-prod.nr-linha = lin-prod.nr-linha AND 
              ord-prod.cd-planejado = "AUTOMATICO" AND
              ord-prod.estado <= 6
              USE-INDEX item-emiss NO-LOCK NO-ERROR.

    IF NOT AVAIL ord-prod THEN DO.
       RUN pi-abre-op.

       IF RETURN-VALUE = "ADM-ERROR":U THEN NEXT.

       FIND FIRST tt-ord-prod NO-ERROR.
       FIND ord-prod WHERE 
            ROWID(ord-prod) = tt-ord-prod.rw-ord-prod NO-LOCK NO-ERROR.
        
       IF NOT AVAIL ord-prod THEN NEXT.
        
       CREATE tt-dados.
       ASSIGN tt-dados.requis-por-ordem = YES
              tt-dados.estado = 1
              tt-dados.cod-versao-integracao = 001.
        
       CREATE tt-ord-prod-2.
       ASSIGN tt-ord-prod-2.nr-ord-produ = ord-prod.nr-ord-prod
              tt-ord-prod-2.it-codigo   = ord-prod.it-codigo
              tt-ord-prod-2.cod-estabel = ord-prod.cod-estabel
              tt-ord-prod-2.nr-linha    = ord-prod.nr-linha
              tt-ord-prod-2.qt-ordem    = ord-prod.qt-ordem.
        
       FOR EACH tt-erro.
           DELETE tt-erro.
       END.

       RUN cpp/cpapi018.p (INPUT TABLE tt-dados,
                           INPUT TABLE tt-ord-prod-2,
                           INPUT-OUTPUT TABLE tt-req-sum,
                           INPUT-OUTPUT TABLE tt-erro,
                           INPUT YES).
       FIND FIRST tt-erro NO-LOCK NO-ERROR.
       IF AVAIL tt-erro THEN DO.
          FOR EACH tt-erro.
              ASSIGN c-mensagem = "Erro ao Sumarizar a OP " + STRING(ord-prod.nr-ord-prod) + CHR(10) +
                                  tt-erro.mensagem.

              RUN esapi/esapi002.p (INPUT "controle.acabado@teartextil.com.br", /* e-mail remetente */
                                    INPUT tt-param.e-mail, /* e-mail destinat†rio */
                                    INPUT "Erro ao Sumarizar OP Autom†tica" , /* Assunto */
                                    INPUT c-mensagem, /* Mensagem */
                                    INPUT "", /*arquivo anexo*/
                                    INPUT YES). /* Mostra Erros */
          END.
          NEXT.
       END.
    END.

    RUN pi-reporta-mov.
    IF RETURN-VALUE = "ADM-ERROR":U THEN NEXT.
    
    FIND FIRST tt-rep-prod NO-ERROR.

    /*-- Verifica parÉmetro para Zerar a quantidade Pronto --*/
    FIND ob-param NO-LOCK NO-ERROR.

    FOR EACH tt-pcp-reporte WHERE
             tt-pcp-reporte.it-codigo  = tt-reporte.it-codigo AND
             tt-pcp-reporte.cod-refer  = tt-reporte.cod-refer AND
             tt-pcp-reporte.nr-lote    = tt-reporte.nr-lote AND
             tt-pcp-reporte.dt-emissao = tt-reporte.dt-emissao NO-LOCK.

        FIND ob-pcp WHERE
             ob-pcp.num-progr = tt-pcp-reporte.num-progr SHARE-LOCK NO-ERROR.
    
        IF AVAIL ob-pcp THEN DO.
           FIND ob-pcp-ref OF ob-pcp WHERE
                ob-pcp-ref.cod-refer = ord-prod.cod-refer
                SHARE-LOCK NO-ERROR.
    
           IF AVAIL ob-pcp-ref THEN DO.
              ASSIGN de-perc-pron = 0.
              IF ob-pcp-ref.qtd-pron > 0 THEN DO.
                 ASSIGN de-perc-pron = tt-pcp-reporte.quantidade / ob-pcp-ref.qtd-pron * 100.
    
                 ASSIGN ob-pcp-ref.char-1 = STRING(ob-pcp-ref.qtd-pron,"->>>,>>9.99") + "   " +
                                            STRING(tt-pcp-reporte.quantidade,"->>>,>>9.99")
                        ob-pcp-ref.qtd-pron = ob-pcp-ref.qtd-pron - tt-pcp-reporte.quantidade.
    
                 IF de-perc-pron >= ob-param.perc-bx-pronto OR 
                    ob-pcp-ref.qtd-pron <= 0 THEN DO.
                    ASSIGN ob-pcp-ref.char-1 = ob-pcp-ref.char-1 + "   Zerou   " + STRING(de-perc-pron,">>9.99") + "  " +
                                               STRING(ob-pcp-ref.qtd-pron,"->>>,>>9.99") 
                           ob-pcp-ref.qtd-pron = 0.

                    IF ob-pcp-ref.qtd-pron = 0 AND
                       ob-pcp-ref.qtd-proc = 0 AND
                       ob-pcp-ref.qtd-sld-prog = 0 THEN
                       ASSIGN ob-pcp-ref.situacao = 2.
    
                    IF NOT CAN-FIND(FIRST ob-pcp-ref OF ob-pcp WHERE
                                          ob-pcp-ref.situacao = 1) THEN
                       ASSIGN ob-pcp.situacao = 2.
                 END.
                 ASSIGN ob-pcp-ref.usr-ult-pron = c-seg-usuario
                        ob-pcp-ref.dt-ult-pron  = TODAY.
              END.
           END.
        END.
    END.

    DO i-ndx = 1 TO NUM-ENTRIES(tt-reporte.rw-etiquetas,";").
       FIND ob-etiqueta WHERE
            ROWID(ob-etiqueta) = TO-ROWID(ENTRY(i-ndx,tt-reporte.rw-etiquetas,";"))
            SHARE-LOCK NO-ERROR.
       IF AVAIL ob-etiqueta THEN DO.
          ASSIGN ob-etiqueta.nr-reporte = tt-rep-prod.nr-reporte
                 ob-etiqueta.nr-ord-prod = tt-rep-prod.nr-ord-prod.

          FIND ordem-benefic OF ob-etiqueta SHARE-LOCK NO-ERROR.
          IF AVAIL ordem-benefic AND ordem-benefic.situacao = 4 THEN
             ASSIGN ordem-benefic.situacao = 5.
       END.
    END.
END.


/* Reporta Retalhos */
{utp/ut-liter.i Reportando_Retalhos *}
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

RUN pi-reporta-retalho.

FIND CURRENT ocorrenc NO-ERROR.
DELETE ocorrenc.

RUN pi-finalizar IN h-acomp.

RETURN "OK".


/*-------------------- Procedures -------------------- */

PROCEDURE pi-abre-op:
    FOR EACH tt-ord-prod.
        DELETE tt-ord-prod.
    END.

    FOR EACH tt-erro.
        DELETE tt-erro.
    END.

    CREATE tt-ord-prod.
    ASSIGN tt-ord-prod.nr-ord-prod = 0
           tt-ord-prod.cod-estabel = tt-reporte.cod-estabel
           tt-ord-prod.it-codigo = tt-reporte.it-codigo
           tt-ord-prod.cod-refer = tt-reporte.cod-refer
           tt-ord-prod.lote = tt-reporte.nr-lote
           tt-ord-prod.qt-ordem = 1500000
           tt-ord-prod.un = item.un
           tt-ord-prod.dt-inicio = tt-reporte.dt-emissao
           tt-ord-prod.dt-termino = DATE(MONTH(tt-reporte.dt-emissao),1,YEAR(tt-reporte.dt-emissao)) + 31
           tt-ord-prod.dt-termino = DATE(MONTH(tt-ord-prod.dt-termino),1,YEAR(tt-ord-prod.dt-termino)) - 1
           tt-ord-prod.cd-planejado = lin-prod.cd-planejado
           tt-ord-prod.estado = 2
           tt-ord-prod.cod-depos = item.deposito-pad
           tt-ord-prod.dt-emissao = tt-reporte.dt-emissao
           tt-ord-prod.conta-ordem = lin-prod.conta-ordem
           tt-ord-prod.ct-codigo = lin-prod.ct-ordem 
           tt-ord-prod.sc-codigo = lin-prod.sc-ordem
           tt-ord-prod.nr-linha = lin-prod.nr-linha 
           tt-ord-prod.reporte-ggf = 2
           tt-ord-prod.reporte-mob = 2
           tt-ord-prod.tipo = 1
           tt-ord-prod.rep-prod = 1
           tt-ord-prod.ind-tipo-movto = 1
           tt-ord-prod.cd-planejado = "AUTOMATICO"
           tt-ord-prod.faixa-numeracao = 1
           tt-ord-prod.cod-versao-integracao = 003.
    
    RUN cpp/cpapi301.p (INPUT-OUTPUT TABLE tt-ord-prod,
                        INPUT-OUTPUT TABLE tt-reapro,
                        INPUT-OUTPUT TABLE tt-erro, 
                        INPUT YES).
    
    FIND FIRST tt-erro NO-LOCK NO-ERROR.
    IF AVAIL tt-erro THEN DO.
       FOR EACH tt-erro.
           RUN esapi/esapi002.p (INPUT "controle.acabado@teartextil.com.br", /* e-mail remetente */
                                 INPUT tt-param.e-mail, /* e-mail destinat†rio */
                                 INPUT "Erro na Abertura Autom†tica de OP" + " - Item " + tt-reporte.it-codigo , /* Assunto */
                                 INPUT tt-erro.mensagem, /* Mensagem */
                                 INPUT "", /*arquivo anexo*/
                                 INPUT YES). /* Mostra Erros */
       END.
       RETURN "ADM-ERROR".
    END.
END PROCEDURE.

PROCEDURE pi-reporta-mov.
    FOR EACH tt-rep-prod.
        DELETE tt-rep-prod.
    END.
    FOR EACH tt-res-neg.
        DELETE tt-res-neg.
    END.
    FOR EACH tt-erro.
        DELETE tt-erro.
    END.

    FIND item WHERE 
         item.it-codigo = ord-prod.it-codigo NO-LOCK NO-ERROR.

    CREATE tt-rep-prod.
    ASSIGN tt-rep-prod.tipo         = 1
           tt-rep-prod.nr-ord-produ = ord-prod.nr-ord-prod
           tt-rep-prod.it-codigo    = ord-prod.it-codigo
           tt-rep-prod.cod-refer    = ord-prod.cod-refer.

    ASSIGN tt-rep-prod.qt-reporte     = tt-reporte.qtd-acabado
           tt-rep-prod.procura-saldos = NO
           tt-rep-prod.cod-depos-sai  = param-cp.dep-fabrica
           tt-rep-prod.data           = tt-reporte.dt-emissao
           tt-rep-prod.un             = ord-prod.un
           tt-rep-prod.conta-contabil = param-cp.ct-ordem + param-cp.sc-ordem
           tt-rep-prod.conta-refugo   = param-cp.ct-refugo + param-cp.sc-refugo
           tt-rep-prod.nro-docto      = STRING(ord-prod.nr-ord-produ)
           tt-rep-prod.cod-depos      = item.deposito-pad
           tt-rep-prod.cod-localiz    = item.cod-localiz
           tt-rep-prod.lote           = tt-reporte.nr-lote 
           tt-rep-prod.dt-vali-lote   = 12.31.9999
           tt-rep-prod.reserva        = NO
           tt-rep-prod.finaliza-ordem = NO
           tt-rep-prod.cod-versao-integracao = 001.


    /* Popula tabela de itens a serem requisitados */
    FOR EACH reservas WHERE
             reservas.nr-ord-produ = ord-prod.nr-ord-prod NO-LOCK. 

        FIND FIRST tt-res-neg WHERE
                   tt-res-neg.it-codigo = reservas.it-codigo NO-ERROR.

        IF NOT AVAIL tt-res-neg THEN DO.           
           CREATE tt-res-neg.
           ASSIGN tt-res-neg.nr-ord-produ = ord-prod.nr-ord-prod
                  tt-res-neg.it-codigo    = reservas.it-codigo
                  tt-res-neg.cod-depos    = param-cp.dep-fabrica
                  tt-res-neg.positivo = IF reservas.quant-orig >= 0
                                        THEN YES ELSE NO.
        END.          
        ASSIGN tt-res-neg.quantidade = tt-res-neg.quantidade + 
                                       ((reservas.quant-orig / ord-prod.qt-ordem) *
                                        (tt-reporte.qtd-acabado + tt-reporte.qtd-retalho-m)).
    END. 

    RUN cpp/cpapi001.p (INPUT-OUTPUT TABLE tt-rep-prod,
                        INPUT        TABLE tt-refugo,
                        INPUT        TABLE tt-res-neg,
                        INPUT        TABLE tt-apont-mob,
                        INPUT-OUTPUT TABLE tt-erro,
                        INPUT        YES).

    FIND FIRST tt-erro NO-LOCK NO-ERROR.
    IF AVAIL tt-erro THEN DO.
       FOR EACH tt-erro.
           ASSIGN c-item-cru = ""
                  c-item-msg = "".
           RUN pi-acha-item-cru. 

           IF ENTRY(1,tt-erro.mensagem,"") <> c-item-cru THEN DO.
              ASSIGN c-item-cru = ENTRY(1,tt-erro.mensagem,"").
              FIND estrutura WHERE
                   estrutura.it-codigo = ord-prod.it-codigo AND
                   estrutura.es-codigo = c-item-cru NO-LOCK NO-ERROR.
              IF AVAIL estrutura THEN DO.
                 FIND item WHERE
                      item.it-codigo = estrutura.es-codigo NO-LOCK NO-ERROR.
                 IF item.ge-codigo = 50 THEN
                    ASSIGN de-qtd-item = estrutura.qtd-item  
                           de-qtd-usada = estrutura.quant-usada.
              END.
              ELSE
                 ASSIGN de-qtd-item = 1
                        de-qtd-usada = 1.
           END.

           ASSIGN de-sld-estoque = 0
                  de-qtd-requis = 0.
           IF c-item-cru <> "" THEN DO.
              /* Verifica saldo em estoque */
              FOR EACH saldo-estoq WHERE
                       saldo-estoq.cod-estabel = tt-reporte.cod-estabel AND
                       saldo-estoq.it-codigo = c-item-cru AND 
                       saldo-estoq.cod-depos = 'BEN' NO-LOCK.
                  ASSIGN de-sld-estoque = de-sld-estoque + saldo-estoq.qtidade-atu.
              END.    
    
              FIND tt-res-neg WHERE
                   tt-res-neg.it-codigo = c-item-cru NO-LOCK NO-ERROR.

              IF AVAIL tt-res-neg THEN
                 ASSIGN de-qtd-requis = tt-res-neg.quantidade.
           END.

           ASSIGN c-mensagem = "Erro no Reporte da OP " + STRING(ord-prod.nr-ord-prod) + CHR(10) + CHR(10) +
                               "Estabelecimento: " + tt-reporte.cod-estabel + CHR(10) + 
                               " Qtd Ö Produzir: " + STRING(tt-reporte.qtd-acabado,">>>,>>>,>>9.9999") + CHR(10) +
                               "    Qtd Retalho: " + STRING(tt-reporte.qtd-retalho-m,">>>,>>>,>>9.9999") + CHR(10) +
                               "          Total: " + STRING(tt-reporte.qtd-acabado + tt-reporte.qtd-retalho-m,">>>,>>>,>>9.9999") + CHR(10) + CHR(10) +
                               " Qtd Requisitar (CRU): " + STRING(de-qtd-requis) + CHR(10) + 
                               "  Saldo Estoque (CRU): " + STRING(de-sld-estoque) + CHR(10) +
                               "            Diferenáa: " + STRING(de-qtd-requis - de-sld-estoque) + CHR(10) + CHR(10) +
                               tt-erro.mensagem.

           RUN esapi/esapi002.p (INPUT "controle.acabado@teartextil.com.br", /* e-mail remetente */
                                 INPUT tt-param.e-mail, /* e-mail destinat†rio */
                                 INPUT "Erro no Reporte Autom†tico" , /* Assunto */
                                 INPUT c-mensagem, /* Mensagem */
                                 INPUT "", /*arquivo anexo*/
                                 INPUT YES). /* Mostra Erros */
       END.
       RETURN "ADM-ERROR".
    END.
END PROCEDURE.

PROCEDURE pi-reporta-retalho.

    FOR EACH tt-retalho BREAK BY tt-retalho.nr-ord-prod.
        RUN pi-acompanhar IN h-acomp (INPUT "Ordem: " + STRING(tt-retalho.nr-ord-prod) + " Etiqueta: " + STRING(tt-retalho.num-etiqueta)). 

        ACCUMULATE tt-retalho.qtd-retalho (TOTAL BY tt-retalho.nr-ord-prod).

        IF LAST-OF(tt-retalho.nr-ord-prod) THEN DO.
           FIND ord-prod WHERE
                ord-prod.nr-ord-prod = tt-retalho.nr-ord-prod NO-LOCK NO-ERROR.

           FIND item WHERE 
                item.it-codigo = tt-retalho.it-codigo NO-LOCK NO-ERROR.

           IF NOT AVAIL item THEN DO.
              ASSIGN c-mensagem = "Item de Retalho " + tt-retalho.it-codigo + 
                                  " para a Ordem " + STRING(ord-prod.nr-ord-prod) +
                                  " n∆o Existe...".
    
              RUN esapi/esapi002.p (INPUT "controle.acabado@teartextil.com.br", /* e-mail remetente */
                                    INPUT tt-param.e-mail, /* e-mail destinat†rio */
                                    INPUT "Erro no Reporte Autom†tico" , /* Assunto */
                                    INPUT c-mensagem, /* Mensagem */
                                    INPUT "", /*arquivo anexo*/
                                    INPUT YES). /* Mostra Erros */
              NEXT.
           END.

           FIND LAST movto-estoq WHERE
                     movto-estoq.nr-ord-produ = ord-prod.nr-ord-prod AND
                     movto-estoq.esp-docto = 1
                     USE-INDEX operacao NO-LOCK NO-ERROR.

           FOR EACH tt-movto.
               DELETE tt-movto.
           END.
           FOR EACH tt-erro.
               DELETE tt-erro.
           END.

           CREATE tt-movto.
           ASSIGN tt-movto.cod-versao-integracao   = 1                          /* acrescentado*/
                  tt-movto.cod-prog-orig           = "ESSP0102"                 /* acrescentado*/
                  tt-movto.usuario                 = tt-param.usuario           /* acrescentado*/
                  tt-movto.cod-estabel             = movto-estoq.cod-estabel
                  tt-movto.ct-codigo               = movto-estoq.ct-codigo
                  tt-movto.sc-codigo               = movto-estoq.sc-codigo
                  tt-movto.esp-docto               = 35
                  tt-movto.tipo-trans              = 1
                  tt-movto.cod-depos               = ord-prod.cod-depos
                  tt-movto.serie                   = movto-estoq.serie
                  tt-movto.dt-trans                = movto-estoq.dt-trans
                  tt-movto.it-codigo               = item.it-codigo       
                  tt-movto.nro-docto               = STRING(ord-prod.nr-ord-prod)
                  tt-movto.nr-ord-produ            = ord-prod.nr-ord-prod
                  tt-movto.quantidade              = (ACCUM TOTAL BY tt-retalho.nr-ord-prod tt-retalho.qtd-retalho)
                  tt-movto.un                      = item.un.
        
           RUN cep/ceapi001.p (INPUT-OUTPUT TABLE tt-movto,
                               INPUT-OUTPUT TABLE tt-erro,
                               INPUT YES). /* Deleta erros? */

           FIND FIRST tt-erro NO-LOCK NO-ERROR.
           IF NOT AVAIL tt-erro THEN DO.
              ASSIGN tt-retalho.reportado = YES.
           END.
           ELSE DO.
              FOR EACH tt-erro.
                  RUN esapi/esapi002.p (INPUT "controle.acabado@teartextil.com.br", /* e-mail remetente */
                                        INPUT tt-param.e-mail, /* e-mail destinat†rio */
                                        INPUT "Erro no Reporte Autom†tico do Retalho" , /* Assunto */
                                        INPUT tt-erro.mensagem + CHR(13) +
                                              'Item ' + movto-estoq.it-codigo + 
                                              ' Referància ' + movto-estoq.cod-refer, /* Mensagem */
                                        INPUT "", /*arquivo anexo*/
                                        INPUT YES). /* Mostra Erros */
              END.
           END.
        END.
    END.

    FOR EACH b-tt-retalho WHERE
             b-tt-retalho.reportado NO-LOCK,
        EACH tt-retalho WHERE
             tt-retalho.nr-ord-prod = b-tt-retalho.nr-ord-prod NO-LOCK.

        RUN pi-acompanhar IN h-acomp (INPUT "Alterando Etiqueta: " + STRING(tt-retalho.nr-ord-prod) + " Etiqueta: " + STRING(tt-retalho.num-etiqueta)). 

        FIND ob-etiqueta WHERE
             ob-etiqueta.cod-estabel = tt-retalho.cod-estabel AND
             ob-etiqueta.num-etiqueta = tt-retalho.num-etiqueta
             SHARE-LOCK NO-ERROR.

        IF AVAIL ob-etiqueta THEN
           ASSIGN ob-etiqueta.nr-reporte = 1
                  ob-etiqueta.nr-ord-prod = tt-retalho.nr-ord-prod.
    END.
END PROCEDURE.

PROCEDURE pi-acha-item-cru.
    ASSIGN c-item-cru = "".
    FOR EACH ref-estrut WHERE
             ref-estrut.it-codigo = ord-prod.it-codigo AND
             ref-estrut.cod-ref-it = ord-prod.cod-refer
             NO-LOCK.

        FIND LAST estrutura WHERE
                  estrutura.it-codigo = ref-estrut.it-codigo AND
                  estrutura.es-codigo = ref-estrut.es-codigo 
                  NO-LOCK NO-ERROR.

        IF estrutura.fantasma THEN 
           RUN pi-ver-fantasma (estrutura.es-codigo). 
        ELSE DO.
            FIND item WHERE
                 item.it-codigo = estrutura.es-codigo NO-LOCK NO-ERROR.
            IF item.ge-codigo = 50 THEN
               ASSIGN c-item-cru = estrutura.es-codigo
                      de-qtd-item = estrutura.qtd-item  
                      de-qtd-usada = estrutura.quant-usada.
        END.
        IF c-item-cru <> "" THEN LEAVE.
    END.
END PROCEDURE.

PROCEDURE pi-ver-fantasma.
    DEF INPUT PARAMETER p-it-codigo AS CHAR.
    FOR EACH estrutura WHERE
             estrutura.it-codigo = p-it-codigo NO-LOCK.
    
        IF estrutura.fantasma THEN
           RUN pi-ver-fantasma (estrutura.es-codigo).
        ELSE DO.
            FIND item WHERE
                 item.it-codigo = estrutura.es-codigo NO-LOCK NO-ERROR.
            IF item.ge-codigo = 50 THEN
               ASSIGN c-item-cru = estrutura.es-codigo
                      de-qtd-item = estrutura.qtd-item  
                      de-qtd-usada = estrutura.quant-usada.
        END.
    END.
END PROCEDURE.

