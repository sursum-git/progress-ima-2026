/* Programa: ESSP0187.W
** Modulo..: Controle de Expedi‡Æo
** Objetivo: Sumarizar etiquetas e mostrar resumo de itens e referencias
** Autor...: Toninho
** Obs.....: Especifico da TEAR TEXTIL IND.COM.LTDA.
**
*/
/* include de controle de versÆo */
{include/i-prgvrs.i ESSP0187RP 2.04.00.000}
DEFINE BUFFER empresa FOR mgadm.empresa.
DEFINE TEMP-TABLE tt-param NO-UNDO
    FIELD destino          AS INTEGER
    FIELD arquivo          AS CHAR FORMAT "x(35)"
    FIELD usuario          AS CHAR FORMAT "x(12)"
    FIELD data-exec        AS DATE
    FIELD hora-exec        AS INTEGER
    FIELD classifica       AS INTEGER
    FIELD desc-classifica  AS CHAR FORMAT "x(40)"
    FIELD modelo-rtf       AS CHAR FORMAT "x(35)"
    FIELD cod-estabel      AS CHAR
    FIELD l-habilitaRtf    AS LOG
    FIELD l-retorna-sem    AS LOG
    FIELD l-espelho        AS LOG
    FIELD nome-abrev       LIKE emitente.nome-abrev
    FIELD l-etiquetas      AS LOG
    FIELD it-codigo        AS CHAR
    FIELD cod-refer        AS CHAR
    FIELD lote             AS CHAR
    FIELD localiz          AS CHAR
    FIELD l-retorna-com    AS LOG
    FIELD l-altera-qtd     AS LOG
    FIELD l-consumo        AS LOG
    FIELD i-acerto         AS INT
    FIELD justificativa    AS CHAR FORMAT "x(100)".

define temp-table tt-digita no-undo
    FIELD c-localiz        AS CHAR
    FIELD num-etiqueta     LIKE ob-etiqueta.num-etiqueta
    FIELD quantidade       LIKE ob-etiqueta.quantidade.

DEFINE TEMP-TABLE tt-raw-digita
       FIELD raw-digita AS RAW.

DEF INPUT parameter raw-param as raw no-undo.
DEF INPUT parameter table for tt-raw-digita.

CREATE tt-param.
RAW-TRANSFER raw-param to tt-param.

FOR EACH tt-raw-digita.
    CREATE tt-digita.
    raw-transfer tt-raw-digita.raw-digita to tt-digita.
END.

DEF TEMP-TABLE tt-itens
    FIELD it-codigo    LIKE ob-etiqueta.it-codigo
    FIELD cod-refer    LIKE ob-etiqueta.cod-refer
    FIELD lote         LIKE ob-etiqueta.nr-lote
    FIELD quantidade   LIKE ob-etiqueta.quantidade
    FIELD nova-qtd     LIKE ob-etiqueta.quantidade
    FIELD qt-etq       AS   INT
    FIELD notas-ori    AS   CHAR FORMAT "x(45)"
    FIELD num-trf      LIKE ob-trf.num-trf
    INDEX indice1 IS PRIMARY it-codigo cod-refer.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* include padrÆo para impressÆo de campos editores em relat¢rio  */
{include/tt-edit.i}

/* defini‡Æo de vari veis  */
DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

DEF VAR h-acomp        AS HANDLE NO-UNDO.
DEF VAR c-erro         AS CHAR FORMAT "x(100)".
DEF VAR de-qtd         LIKE movto-estoq.quantidade.

FORM
    tt-itens.it-codigo                            LABEL "Item"
    ITEM.desc-item        FORMAT  "x(35)"         LABEL "Descri‡Æo" 
    tt-itens.cod-refer    FORMAT "99-9999-9"      LABEL "Referˆncia"
    tt-itens.lote         FORMAT "x(5)"           LABEL "Lote" 
    tt-itens.quantidade   FORMAT ">,>>>,>>9.99"   LABEL "Qtd Faturada"
    tt-itens.qt-etq       FORMAT ">>9"            LABEL "Tot Etq" 
    tt-itens.notas-ori    FORMAT "x(35)"          LABEL "Notas Origem" 
    WITH NO-BOX NO-LABEL 55 DOWN WIDTH 132 STREAM-IO FRAME f-detalhe.

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i}

/* bloco principal do programa */
FIND FIRST param-global NO-LOCK NO-ERROR.
FIND FIRST empresa
     WHERE empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR. 

ASSIGN c-empresa = (IF AVAIL empresa THEN empresa.razao-social ELSE "").

{utp/ut-liter.i ESPECÖFICOS r}
ASSIGN c-sistema = TRIM(RETURN-VALUE).
{utp/ut-liter.i Resumo_de_Etiquetas r}
ASSIGN c-titulo-relat = TRIM(RETURN-VALUE).

VIEW FRAME f-cabec.
VIEW FRAME f-rodape.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
{utp/ut-liter.i Imprimindo *}
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).
 
IF tt-param.l-espelho THEN RUN pi-espelho.
IF tt-param.l-etiquetas OR tt-param.l-retorna-sem OR tt-param.l-altera-qtd THEN
   RUN pi-altera-etq.

CASE tt-param.i-acerto.
    WHEN 1 THEN RUN pi-altera-sit (INPUT 3).
    WHEN 2 THEN RUN pi-altera-sit (INPUT 9).
END.

/* echamento do output do relat¢rio  */
{include/i-rpclo.i}
RUN pi-finalizar IN h-acomp.
RETURN "OK":U.

/*----------------------- PROCEDURES -----------------------*/
PROCEDURE pi-espelho.
   FOR EACH tt-digita.
       FIND ob-etiqueta WHERE
            ob-etiqueta.cod-estabel  = tt-param.cod-estabel AND
            ob-etiqueta.num-etiqueta = tt-digita.num-etiqueta NO-LOCK.
   
       FIND FIRST tt-itens WHERE
                  tt-itens.it-codigo = ob-etiqueta.it-codigo AND 
                  tt-itens.cod-refer = ob-etiqueta.cod-refer AND
                  tt-itens.lote      = ob-etiqueta.cod-refer
                  NO-ERROR.      
       IF NOT AVAIL tt-itens THEN DO.
          CREATE tt-itens.
          ASSIGN tt-itens.it-codigo    = ob-etiqueta.it-codigo  
                 tt-itens.cod-refer    = ob-etiqueta.cod-refer
                 tt-itens.lote         = ob-etiqueta.cod-refer.
       END.
       ASSIGN tt-itens.quantidade = tt-itens.quantidade + ob-etiqueta.quantidade
              tt-itens.nova-qtd = tt-itens.nova-qtd + tt-digita.quantidade
              tt-itens.qt-etq = tt-itens.qt-etq + 1.

       FIND ped-item-rom WHERE
            ped-item-rom.cod-estabel  = ob-etiqueta.cod-estabel AND
            ped-item-rom.num-etiqueta = ob-etiqueta.num-etiqueta 
            NO-LOCK NO-ERROR.

       FIND ped-item-res WHERE
            ped-item-res.cod-estabel = ped-item-rom.cod-estabel AND
            ped-item-res.nome-abrev = ped-item-rom.nome-abrev  AND
            ped-item-res.nr-pedcli = ped-item-rom.nr-pedcli   AND
            ped-item-res.nr-sequencia = ped-item-rom.nr-sequencia
            NO-LOCK NO-ERROR.
       IF AVAIL ped-item-res AND 
          LOOKUP(STRING(ped-item-res.nr-nota-fis),tt-itens.notas-ori) = 0 THEN
          ASSIGN tt-itens.notas-ori = IF tt-itens.notas-ori = ""
                                      THEN STRING(ped-item-res.nr-nota-fis)
                                      ELSE tt-itens.notas-ori + "," + STRING(ped-item-res.nr-nota-fis).
   END.

   FOR EACH tt-itens BREAK BY tt-itens.it-codigo
                           BY tt-itens.cod-refer.
       FIND ITEM WHERE
            ITEM.it-codigo = tt-itens.it-codigo NO-LOCK NO-ERROR.
   
       ACCUMULATE tt-itens.quantidade (TOTAL BY tt-itens.it-codigo).
       ACCUMULATE tt-itens.quantidade (TOTAL BY tt-itens.cod-refer).
       ACCUMULATE tt-itens.quantidade (TOTAL).
       ACCUMULATE tt-itens.qt-etq (TOTAL).
   
       DISP tt-itens.it-codigo   
            ITEM.desc-item
            tt-itens.cod-refer   
            tt-itens.lote
            tt-itens.quantidade
            tt-itens.qt-etq
            tt-itens.notas-ori   
            WITH FRAME f-detalhe.
       DOWN WITH FRAME f-detalhe.
   
       IF LAST-OF(tt-itens.cod-refer) THEN DO.
           DISP "---------------" @ tt-itens.quantidade
                WITH FRAME f-detalhe.
           DOWN WITH FRAME f-detalhe.
           DISP (ACCUM TOTAL BY tt-itens.cod-refer tt-itens.quantidade) @ tt-itens.quantidade
                WITH FRAME f-detalhe.
           DOWN 2 WITH FRAME f-detalhe.
       END.
   
       IF LAST-OF(tt-itens.it-codig) THEN DO.
          DISP "---------------" @ tt-itens.quantidade
               WITH FRAME f-detalhe.
          DOWN WITH FRAME f-detalhe.
          DISP (ACCUM TOTAL BY tt-itens.it-codigo tt-itens.quantidade) @ tt-itens.quantidade
               WITH FRAME f-detalhe.
          DOWN 2 WITH FRAME f-detalhe.
       END.
   END. 
   
   DOWN 1 WITH FRAME f-detalhe.
   DISP "---------------" @ tt-itens.quantidade
        WITH FRAME f-detalhe.
   DOWN WITH FRAME f-detalhe.
   DISP (ACCUM TOTAL tt-itens.quantidade) @ tt-itens.quantidade
        (ACCUM TOTAL tt-itens.qt-etq) @ tt-itens.qt-etq
       WITH FRAME f-detalhe.
   DOWN 1 WITH FRAME f-detalhe.

END PROCEDURE.

PROCEDURE pi-altera-etq.
    FOR EACH tt-digita NO-LOCK.
        FIND ob-etiqueta WHERE
             ob-etiqueta.cod-estabel  = tt-param.cod-estabel AND
             ob-etiqueta.num-etiqueta = tt-digita.num-etiqueta NO-ERROR.

        FIND FIRST tt-itens WHERE
                   tt-itens.it-codigo = ob-etiqueta.it-codigo AND 
                   tt-itens.cod-refer = ob-etiqueta.cod-refer AND
                   tt-itens.lote      = ob-etiqueta.cod-refer
                   NO-ERROR.      
        IF NOT AVAIL tt-itens THEN DO.
           CREATE tt-itens.
           ASSIGN tt-itens.it-codigo = ob-etiqueta.it-codigo  
                  tt-itens.cod-refer = ob-etiqueta.cod-refer
                  tt-itens.lote = ob-etiqueta.cod-refer.
        END.
        ASSIGN tt-itens.quantidade = tt-itens.quantidade + ob-etiqueta.quantidade
               tt-itens.nova-qtd = tt-itens.nova-qtd + tt-digita.quantidade.
    END.

    /* Acerta Estoque */ 
    ESTOQUE:
    FOR EACH tt-itens.
        FIND ITEM WHERE
             ITEM.it-codigo = tt-itens.it-codigo NO-LOCK NO-ERROR.

        IF tt-param.cod-refer <> "" THEN DO.
           RUN esapi/cria-movto-estoq.p (INPUT tt-param.cod-estabel,
                                         INPUT tt-itens.it-codigo,
                                         INPUT tt-itens.cod-refer,
                                         INPUT tt-itens.cod-refer, 
                                         INPUT tt-itens.quantidade,
                                         INPUT 33,   /* TRA */
                                         INPUT 2,    /* Sa¡da */
                                         INPUT tt-param.justificativa,
                                         OUTPUT c-erro). 

           IF RETURN-VALUE = 'ADM-ERROR' THEN DO.
              PUT SKIP(1)
                  "ERRO ao Efetuar a Sa¡da da Quantidade do Estoque:" 
                  c-erro 
                  SKIP.
              LEAVE ESTOQUE.
           END.

           RUN esapi/cria-movto-estoq.p (INPUT tt-param.cod-estabel,
                                         INPUT tt-itens.it-codigo,
                                         INPUT tt-param.cod-refer, 
                                         INPUT tt-param.cod-refer,
                                         INPUT tt-itens.quantidade,
                                         INPUT 33,  /* TRA */ 
                                         INPUT 1,   /* Entrada */ 
                                         INPUT tt-param.justificativa,
                                         OUTPUT c-erro).
        END.

        IF tt-param.l-consumo = YES THEN DO.
           RUN esapi/cria-movto-estoq.p (INPUT tt-param.cod-estabel,
                                         INPUT tt-itens.it-codigo,
                                         INPUT tt-itens.cod-refer,
                                         INPUT tt-itens.cod-refer, 
                                         INPUT tt-itens.quantidade,
                                         INPUT 6,   /* DIV */
                                         INPUT 2,   /* Sa¡da */
                                         INPUT tt-param.justificativa,
                                         OUTPUT c-erro). 

           IF RETURN-VALUE = 'ADM-ERROR' THEN DO.
              PUT SKIP(1)
                  "ERRO ao Efetuar a Sa¡da da Quantidade do Estoque:" 
                  c-erro 
                  SKIP.
              LEAVE ESTOQUE.
           END.
        END.


        IF tt-param.it-codigo <> "" THEN DO.
           RUN esapi/cria-movto-estoq.p (INPUT tt-param.cod-estabel,
                                         INPUT tt-itens.it-codigo,
                                         INPUT tt-itens.cod-refer,
                                         INPUT tt-itens.cod-refer, 
                                         INPUT tt-itens.quantidade,
                                         INPUT 6,   /* DIV */
                                         INPUT 2,   /* Sa¡da */
                                         INPUT tt-param.justificativa,
                                         OUTPUT c-erro). 

           IF RETURN-VALUE = 'ADM-ERROR' THEN DO.
              PUT SKIP(1)
                  "ERRO ao Efetuar a Sa¡da da Quantidade do Estoque:" 
                  c-erro 
                  SKIP.
              LEAVE ESTOQUE.
           END.

           IF tt-param.it-codigo <> "" THEN 
              RUN esapi/cria-movto-estoq.p (INPUT tt-param.cod-estabel,
                                            INPUT tt-param.it-codigo,
                                            INPUT tt-param.cod-refer,
                                            INPUT tt-param.cod-refer,
                                            INPUT tt-itens.quantidade,
                                            INPUT 6,  /* DIV */ 
                                            INPUT 1,  /* Entrada */ 
                                            INPUT tt-param.justificativa,
                                            OUTPUT c-erro).
        END. 

        IF tt-param.l-altera-qtd AND 
           tt-itens.quantidade <> tt-itens.nova-qtd THEN DO.
           ASSIGN de-qtd = tt-itens.quantidade - tt-itens.nova-qtd.

           IF de-qtd < 0 THEN DO. /* Acrescentar no Estoque */
              RUN esapi/cria-movto-estoq.p (INPUT tt-param.cod-estabel,
                                            INPUT tt-itens.it-codigo,
                                            INPUT tt-itens.cod-refer,
                                            INPUT tt-itens.cod-refer, 
                                            INPUT ABS(de-qtd),
                                            INPUT 6,   /* DIV */
                                            INPUT 1,   /* Entrada */
                                            INPUT tt-param.justificativa,
                                            OUTPUT c-erro). 

              IF RETURN-VALUE = 'ADM-ERROR' THEN DO.
                 MESSAGE "ERRO ao Efetuar Entrada da Quantidade no Estoque:" 
                         c-erro 
                     VIEW-AS ALERT-BOX INFO BUTTONS OK.
                 LEAVE ESTOQUE.
              END.
           END.
           ELSE DO. /* Retirar do Estoque */
               RUN esapi/cria-movto-estoq.p (INPUT tt-param.cod-estabel,
                                             INPUT tt-itens.it-codigo,
                                             INPUT tt-itens.cod-refer,
                                             INPUT tt-itens.cod-refer, 
                                             INPUT ABS(de-qtd),
                                             INPUT 6,   /* DIV */
                                             INPUT 2,   /* Sa¡da */
                                             INPUT tt-param.justificativa,
                                             OUTPUT c-erro). 

               IF RETURN-VALUE = 'ADM-ERROR' THEN DO.
                  MESSAGE "ERRO ao Efetuar a Sa¡da da Quantidade do Estoque:" 
                          c-erro 
                      VIEW-AS ALERT-BOX INFO BUTTONS OK.
                  LEAVE ESTOQUE.
               END.
           END.
        END.

        IF l-retorna-com THEN DO.
           RUN esapi/cria-movto-estoq.p (INPUT tt-param.cod-estabel,
                                         INPUT tt-itens.it-codigo,
                                         INPUT tt-itens.cod-refer,
                                         INPUT tt-itens.cod-refer, 
                                         INPUT tt-itens.quantidade,
                                         INPUT 6,  /* DIV */ 
                                         INPUT 1,  /* Entrada */ 
                                         INPUT tt-param.justificativa,
                                         OUTPUT c-erro).

           IF RETURN-VALUE = 'ADM-ERROR' THEN DO.
              PUT SKIP(1)
                  "ERRO ao Efetuar a Entrada da Quantidade no Estoque:" 
                  c-erro 
                  SKIP.
              LEAVE ESTOQUE.
           END.
        END.
    END.
    IF c-erro <> "" THEN NEXT.

    /* Acerta Etiquetas */ 
    FOR EACH tt-digita NO-LOCK.
        FIND ob-etiqueta WHERE
             ob-etiqueta.cod-estabel  = tt-param.cod-estabel AND
             ob-etiqueta.num-etiqueta = tt-digita.num-etiqueta NO-ERROR.

        IF tt-param.it-codigo <> '' THEN
           ASSIGN ob-etiqueta.it-codigo = tt-param.it-codigo.

        IF tt-param.cod-refer <> "" THEN 
           ASSIGN ob-etiqueta.cod-refer = tt-param.cod-refer.

        IF tt-param.lote <> "" THEN
           ASSIGN ob-etiqueta.nr-lote = tt-param.lote.

        IF tt-param.l-consumo THEN 
           ASSIGN ob-etiqueta.situacao = 9.

        IF tt-param.l-altera-qtd THEN 
           ASSIGN ob-etiqueta.quantidade = tt-digita.quantidade.

        IF tt-param.l-retorna-com OR 
           tt-param.l-retorna-sem THEN DO.
           ASSIGN ob-etiqueta.situacao = 3
                  ob-etiqueta.dt-fatur = ?.
           
           FIND ped-item-rom WHERE
                ped-item-rom.cod-estabel = ob-etiqueta.cod-estabel AND
                ped-item-rom.num-etiqueta = ob-etiqueta.num-etiqueta NO-ERROR.
           IF AVAIL ped-item-rom THEN
              DELETE ped-item-rom.
        END.
    END.

    PUT SKIP(1)
        "ETIQUETAS ALTERADAS...." 
        SKIP.
END.


PROCEDURE pi-altera-sit.
    DEF INPUT PARAMETER p-situacao AS INT.
    FOR EACH tt-digita NO-LOCK.
        FIND ob-etiqueta WHERE
             ob-etiqueta.cod-estabel  = tt-param.cod-estabel AND
             ob-etiqueta.num-etiqueta = tt-digita.num-etiqueta NO-ERROR.

        ASSIGN ob-etiqueta.situacao = p-situacao.
    END.

    PUT SKIP(1)
        "SITUCAO DAS ETIQUETAS ALTERADAS..." 
        SKIP.

END PROCEDURE.
