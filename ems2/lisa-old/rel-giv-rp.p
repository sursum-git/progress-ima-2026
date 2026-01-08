/* include de controle de vers’o */
{include/i-prgvrs.i rel-giv-rp 1.00.00.001}

/*
1.00.00.001 - Tadeu -tsp01- 04/2024 - substitui‡Æo da api envia-nfs-venda-lisa para a a envia-nfs-venda-lisa-2
que possibilita tanto enviar a nota de venda ou a nota triangular na aprova‡Æo do pedido de venda
a partir do parametro passado para a api.
*/

/* defini»’o das temp-tables para recebimento de parƒmetros */
DEFINE TEMP-TABLE tt-param NO-UNDO
    FIELD destino          AS INTEGER
    FIELD arquivo          AS CHAR FORMAT "x(35)"
    FIELD usuario          AS CHAR FORMAT "x(12)"
    FIELD data-exec        AS DATE
    FIELD hora-exec        AS INTEGER
    FIELD classifica       AS INTEGER
    FIELD desc-classifica  AS CHAR format "x(40)"
    FIELD modelo-rtf       AS CHAR format "x(35)"
    FIELD l-habilitaRtf    AS LOG
    FIELD l-nota-remessa   AS LOG
    FIELD l-pck-list       AS LOG
    FIELD l-env-isf        AS LOG
    FIELD l-ret-isf        AS LOG
    FIELD l-env-nfs-venda  AS LOG
    FIELD l-nota-retorno   AS LOG
    FIELD l-conf-etiquetas AS LOG.

DEF TEMP-TABLE tt-ped-venda 
    FIELD row-ped-venda AS ROWID.

DEF TEMP-TABLE tt-ped-item NO-UNDO LIKE ped-item
     FIELD r-rowid AS ROWID.

DEF TEMP-TABLE tt-nota-fisc LIKE nota-fiscal.
DEF TEMP-TABLE tt-it-nota-fisc LIKE it-nota-fisc.

DEF TEMP-TABLE tt-notas-geradas NO-UNDO
    FIELD rw-nota-fiscal AS   ROWID
    FIELD nr-nota        LIKE nota-fiscal.nr-nota-fis
    FIELD seq-wt-docto   LIKE wt-docto.seq-wt-docto.

DEF BUFFER b-lisa-integra FOR lisa-integra.

/* Parametros de entrada logica obrigatoria */
DEF TEMP-TABLE tt-raw-digita
   	FIELD raw-digita    AS RAW.

/* recebimento de parƒmetros */
def input parameter raw-param as raw no-undo.
def input parameter TABLE for tt-raw-digita.

{lisa/extrairInfCp5ConfEtq.i}

create tt-param.
raw-transfer raw-param to tt-param.

/* include padrÆo para variÿveis de relat¢rio  */
{include/i-rpvar.i}

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

DEF VAR h-acomp     AS HANDLE NO-UNDO.
DEF VAR c-status    AS CHAR.
DEF VAR c-chave     AS CHAR.
DEF VAR c-arq-xml   AS CHAR.
DEF VAR c-pedido    AS CHAR.
DEF VAR l-erro-res  AS LOG.
DEF VAR c-it-codigo AS CHAR.
DEF VAR cErro       AS CHAR.
DEFINE VARIABLE nrContainer AS INTEGER     NO-UNDO.
DEFINE VARIABLE idEtqLisa   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE localiz     AS CHARACTER   NO-UNDO.

DEFINE VARIABLE hBoParam AS HANDLE      NO-UNDO.
DEFINE VARIABLE tpNfAprovPedLisa AS CHARACTER   NO-UNDO.
RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
{utp/ut-liter.i Sincronizando_Dados *}
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

/* bloco principal do programa */
IF tt-param.l-nota-remessa THEN DO.
   FOR EACH lisa-integra WHERE
            lisa-integra.cod-trans = 'NotaRemessa' AND
            lisa-integra.ind-situacao = 1 SHARE-LOCK.
       
       RUN pi-acompanhar IN h-acomp (INPUT "Integrando Nota do Container: " + lisa-integra.chave ).

       FIND pp-container WHERE
            pp-container.nr-container = INTEGER(lisa-integra.chave)
            NO-LOCK NO-ERROR.

       FIND processo-imp WHERE
            processo-imp.nr-proc-imp = STRING(pp-container.nr-container)
            NO-LOCK NO-ERROR.

       FIND FIRST item-doc-est WHERE
                  item-doc-est.num-pedido = processo-imp.num-pedido NO-LOCK NO-ERROR.

       FIND nota-fiscal WHERE
            nota-fiscal.cod-estabel = pp-container.cod-estabel AND
            nota-fiscal.serie = item-doc-est.serie-docto AND
            nota-fiscal.nr-nota-fis = item-doc-est.nro-docto NO-LOCK NO-ERROR.

       FIND estabelec OF nota-fiscal NO-LOCK NO-ERROR.
       FIND param-nf-estab OF estabelec NO-LOCK NO-ERROR.

       FIND emitente WHERE
            emitente.cod-emit = nota-fiscal.cod-emit NO-LOCK NO-ERROR.

       IF lisa-integra.acao = 'GERAR' THEN DO.
          RUN pi-gerar-nota-envio.
          IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

          ASSIGN lisa-integra.acao = 'ENVIAR'.
       END.
       
       // Localizar a Nota Gerada
       FIND nota-fiscal WHERE
            nota-fiscal.cod-estabel = pp-container.cod-estabel AND
            nota-fiscal.nro-proc-entrada = pp-container.nr-container AND
            nota-fiscal.cod-protoc <> '' AND
            nota-fiscal.dt-cancela = ? 
            NO-LOCK NO-ERROR.
       IF AVAIL nota-fiscal THEN DO.
          IF nota-fiscal.cod-protoc = '' THEN DO.
             CREATE lisa-log-integr.
             ASSIGN lisa-log-integr.cod-trans = lisa-integra.cod-trans   
                    lisa-log-integr.data = TODAY
                    lisa-log-integr.hora = TIME
                    lisa-log-integr.usuario = c-seg-usuario
                    lisa-log-integr.acao = 'ENVIAR' 
                    lisa-log-integr.log-erro = YES   
                    lisa-log-integr.narrativa = 'Nota ' + nota-fiscal.nr-nota-fis + ' sem Protoclo de Autoriza‡ao'.
             NEXT.
          END.

          ASSIGN c-arq-xml = param-nf-estab.cod-caminho-xml + "\" + nota-fiscal.cod-estabel + FILL("0", (3 - LENGTH(nota-fiscal.serie))) + nota-fiscal.serie + nota-fiscal.nr-nota-fis + ".xml".
          FILE-INFO:FILE-NAME = c-arq-xml.
          IF FILE-INFO:FILE-NAME = ? THEN NEXT.

          RUN pi-acompanhar IN h-acomp (INPUT "Enviando Nota " + nota-fiscal.nr-nota-fis +  " do Container: " + lisa-integra.chave ).

          // se chegar aqui ‚ porque achou o xml
          RUN esapi/envia-nfs-remessa-lisa.p (INPUT ROWID(nota-fiscal)).
          IF RETURN-VALUE <> 'ADM-OK' THEN NEXT.
    
          ASSIGN lisa-integra.acao = ''
                 lisa-integra.ind-situacao = 2.

          // Marca para Enviar o Packing LIST
          FIND b-lisa-integra WHERE
               b-lisa-integra.cod-trans = 'PackingList' AND
               b-lisa-integra.chave = STRING(pp-container.nr-container)
               SHARE-LOCK NO-ERROR.
          IF NOT AVAIL lisa-integra THEN DO.
             CREATE b-lisa-integra.
             ASSIGN b-lisa-integra.cod-trans = 'PackingList'
                    b-lisa-integra.chave = STRING(pp-container.nr-container)
                    b-lisa-integra.acao = 'ENVIAR'
                    b-lisa-integra.ind-situacao = 1.
          END.
       END.
   END.

   // Nota Avulsa
   FOR EACH lisa-integra WHERE
            lisa-integra.cod-trans = 'NotaAvulsa' AND
            lisa-integra.ind-situacao = 1 SHARE-LOCK.
       
       RUN pi-acompanhar IN h-acomp (INPUT "Integrando Nota Avulsa: " + lisa-integra.chave).

       FIND nota-fiscal WHERE
            nota-fiscal.cod-estabel = ENTRY(1,lisa-integra.chave,"|") AND
            nota-fiscal.serie = ENTRY(2,lisa-integra.chave,"|") AND
            nota-fiscal.nr-nota-fis = ENTRY(3,lisa-integra.chave,"|") 
            NO-LOCK NO-ERROR.

       FIND estabelec OF nota-fiscal NO-LOCK NO-ERROR.
       FIND param-nf-estab OF estabelec NO-LOCK NO-ERROR.

       FIND emitente WHERE
            emitente.cod-emit = nota-fiscal.cod-emit NO-LOCK NO-ERROR.

       IF AVAIL nota-fiscal THEN DO.
          ASSIGN c-arq-xml = param-nf-estab.cod-caminho-xml + "\" + nota-fiscal.cod-estabel + FILL("0", (3 - LENGTH(nota-fiscal.serie))) + nota-fiscal.serie + nota-fiscal.nr-nota-fis + ".xml".
          FILE-INFO:FILE-NAME = c-arq-xml.
          IF FILE-INFO:FILE-NAME = ? THEN NEXT.

          RUN pi-acompanhar IN h-acomp (INPUT "Enviando Nota Avulsa " + nota-fiscal.nr-nota-fis +  " do Container: " + lisa-integra.chave ).

          // se chegar aqui ‚ porque achou o xml
          RUN esapi/envia-nfs-remessa-lisa.p (INPUT ROWID(nota-fiscal)).
          IF RETURN-VALUE <> 'ADM-OK' THEN NEXT.
    
          ASSIGN lisa-integra.acao = ''
                 lisa-integra.ind-situacao = 2.
       END.
   END.

END.

IF tt-param.l-pck-list THEN DO.
   FOR EACH lisa-integra WHERE
            lisa-integra.cod-trans = 'PackingList' AND
            lisa-integra.acao = 'ENVIAR' AND
            lisa-integra.ind-situacao = 1 SHARE-LOCK.


       RUN pi-acompanhar IN h-acomp (INPUT "Acertando Etiquetas: " + lisa-integra.chave ).
       

       RUN pi-acompanhar IN h-acomp (INPUT "Enviando Packing-List: " + lisa-integra.chave ).

       FIND pp-container WHERE
            pp-container.nr-container = INTEGER(lisa-integra.chave)
            NO-LOCK NO-ERROR.

       FIND nota-fiscal WHERE
            nota-fiscal.cod-estabel = pp-container.cod-estabel AND
            nota-fiscal.nro-proc-entrada = pp-container.nr-container AND
            nota-fiscal.dt-cancela = ? NO-LOCK NO-ERROR.
       IF NOT AVAIL nota-fiscal THEN NEXT.

       RUN esapi/envia-pckl-lisa.p (INPUT ROWID(nota-fiscal)).
       IF RETURN-VALUE <> 'ADM-OK' THEN NEXT.

       RUN mudarEtqParaEstoque(INTEGER(lisa-integra.chave)).

       ASSIGN lisa-integra.acao = ''
              lisa-integra.ind-situacao = 2.
   END.

   FOR EACH lisa-integra WHERE
            lisa-integra.cod-trans = 'PackingAvulso' AND
            lisa-integra.acao = 'ENVIAR' AND
            lisa-integra.ind-situacao = 1 SHARE-LOCK.

       RUN pi-acompanhar IN h-acomp (INPUT "Enviando Packing Avulso " + lisa-integra.chave ).

       FIND nota-fiscal WHERE
            nota-fiscal.cod-estabel = ENTRY(1,lisa-integra.chave,"|") AND
            nota-fiscal.serie = ENTRY(2,lisa-integra.chave,"|") AND
            nota-fiscal.nr-nota-fis = ENTRY(3,lisa-integra.chave,"|") 
            NO-LOCK NO-ERROR.
       IF NOT AVAIL nota-fiscal THEN NEXT.

       RUN esapi/envia-pckl-avulso.p (INPUT ROWID(nota-fiscal)).
       IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

       ASSIGN lisa-integra.acao = ''
              lisa-integra.ind-situacao = 2.
   END.

END.

IF tt-param.l-env-isf THEN DO.
   FOR EACH lisa-integra WHERE
            lisa-integra.cod-trans = 'ISF' AND
            lisa-integra.ind-situacao = 1 SHARE-LOCK.

        RUN pi-acompanhar IN h-acomp (INPUT "Enviando ISF " + lisa-integra.chave ).

        FIND ped-venda WHERE
             ped-venda.nr-pedcli = ENTRY(1,lisa-integra.chave,"|") AND
             ped-venda.nome-abrev = ENTRY(2,lisa-integra.chave,"|")
             NO-LOCK NO-ERROR.

        IF lisa-integra.acao = 'ENVIAR' THEN DO.
           IF lisa-integra.val-livre-1 = '' THEN
              RUN esapi/envia-isf-lisa.p (INPUT ROWID(ped-venda)).
           ELSE DO:
              RUN esapi/envia-alteracao-isf-lisa.p (INPUT ROWID(ped-venda), INPUT lisa-integra.val-livre-1).
           END.                                            
           IF RETURN-VALUE <> 'ADM-OK' THEN NEXT.              

           ASSIGN lisa-integra.acao = 'SEPARAR'.

        END.
        /*IF lisa-integra.acao = 'ALTERAR' THEN DO.
           RUN esapi/envia-alteracao-isf-lisa.p (INPUT ROWID(ped-venda)).
           IF RETURN-VALUE <> 'ADM-OK' THEN NEXT.

           ASSIGN lisa-integra.acao = 'SEPARAR'.
        END.*/
   END.
END.

IF tt-param.l-ret-isf THEN DO.
   FIND FIRST para-ped NO-LOCK NO-ERROR.

   // Alterou a ISF para Reservar na api de Retorno chamada pela Lisa 
   // esapi/retorno-isf-lisa.p
   // tambem criou a Transa‡ao 'RetornoISF'
   /*FIND lisa-integra WHERE
          lisa-integra.cod-trans = 'ISF' AND    
          lisa-integra.chave begins ENTRY(1,lisa-integra.chave,"|") SHARE-LOCK.
   ASSIGN lisa-integra.acao = 'Reservar'. 
   */


   // Cria Reserva das Pe‡as
   FOR EACH lisa-integra WHERE
            lisa-integra.cod-trans = 'ISF' AND    // Instru‡ao de Separa‡ao
            lisa-integra.ind-situacao <= 3 AND 
            lisa-integra.acao = 'RESERVAR' SHARE-LOCK.

       ASSIGN c-pedido = ENTRY(1,lisa-integra.chave,"|").

       FIND ped-venda WHERE
            ped-venda.nr-pedcli = ENTRY(1,lisa-integra.chave,"|") AND
            ped-venda.nome-abrev = ENTRY(2,lisa-integra.chave,"|")
            NO-LOCK NO-ERROR.
       IF NOT AVAIL ped-venda THEN NEXT.

       FIND ped-venda-ext WHERE
            ped-venda-ext.cod-estabel = ped-venda.cod-estabel AND
            ped-venda-ext.nr-pedido = ped-venda.nr-pedido
            SHARE-LOCK NO-ERROR.
       IF NOT AVAIL ped-venda-ext THEN DO.
          MESSAGE ped-venda.nr-pedcli SKIP '  1-pedido com erro'
              VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
          NEXT.
       END.
       ASSIGN ped-venda-ext.qt-fardos = 0.

       EMPTY TEMP-TABLE tt-ped-venda.

       FOR EACH b-lisa-integra WHERE
                b-lisa-integra.cod-trans = "RetornoISF" AND
                b-lisa-integra.chave BEGINS c-pedido
                SHARE-LOCK.
    
           RUN pi-acompanhar IN h-acomp (INPUT "Processando Retorno ISF " + b-lisa-integra.chave ).
    
           FIND ped-item OF ped-venda WHERE
                ped-item.cod-sit-item = 1 AND
                ped-item.it-codigo = ENTRY(2,b-lisa-integra.chave,"|") AND
                ped-item.cod-refer = ENTRY(3,b-lisa-integra.chave,"|") 
                NO-LOCK NO-ERROR.
    
           FIND ped-venda-ext WHERE
                ped-venda-ext.cod-estabel = ped-venda.cod-estabel AND
                ped-venda-ext.nr-pedido = ped-venda.nr-pedido
                SHARE-LOCK NO-ERROR.

           IF NOT AVAIL ped-venda-ext THEN DO.
              MESSAGE ped-venda.nr-pedcli SKIP ' 2-pedido com erro'
                  VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
              NEXT.
           END.

           IF ped-venda-ext.nr-pedext = '' THEN
              ASSIGN ped-venda-ext.nr-pedext = b-lisa-integra.val-livre-1.
    
           FIND ob-etiqueta WHERE
                ob-etiqueta.cod-estabel = ped-venda.cod-estabel AND
                ob-etiqueta.num-etiqueta = INTEGER(b-lisa-integra.conteudo)
                NO-LOCK NO-ERROR.
    
           IF AVAIL ob-etiqueta AND ob-etiqueta.situacao = 3 THEN DO.
              RUN esapi/cria-reserva.p (INPUT ROWID(ped-item),
                                        INPUT ROWID(ob-etiqueta)).
    
              IF RETURN-VALUE = 'ADM-OK' THEN DO.
                 CREATE tt-ped-venda.
                 ASSIGN tt-ped-venda.row-ped-venda = ROWID(ped-venda).
              END.
           END.
       END.
    
       // Ajusta o Pedido
       ASSIGN l-erro-res = NO.
       FOR EACH tt-ped-venda NO-LOCK.
           ASSIGN c-chave = ped-venda.nr-pedcli + "|" + 
                            ped-venda.nome-abrev.

           RUN pi-acompanhar IN h-acomp (INPUT "Ajustando Pedido de Venda " + c-chave ).

           RUN pi-ajusta-pedido (INPUT tt-ped-venda.row-ped-venda).
           IF RETURN-VALUE = 'ADM-ERROR' THEN DO.
              ASSIGN l-erro-res = YES.
              NEXT.
           END.
       END.

       IF l-erro-res = NO THEN DO.
          ASSIGN lisa-integra.acao = 'FATURAR'.

          FOR EACH b-lisa-integra WHERE
                   b-lisa-integra.cod-trans = "RetornoISF" AND
                   b-lisa-integra.chave BEGINS c-pedido SHARE-LOCK.
              ASSIGN b-lisa-integra.acao = ''
                     b-lisa-integra.ind-situacao = 2.  // Efetuado
          END.
       END.
   END.
END.

IF tt-param.l-env-nfs-venda THEN DO.
   FOR EACH lisa-integra WHERE
            lisa-integra.cod-trans = "RemessaNotaVenda" AND
            lisa-integra.ind-situacao = 1 // Aguardando Integra‡ao
            SHARE-LOCK.

       RUN pi-acompanhar IN h-acomp (INPUT "Enviando Nota de Venda " + lisa-integra.chave ).

       FIND nota-fiscal WHERE
            nota-fiscal.cod-estabel = ENTRY(1,lisa-integra.chave,"|") AND
            nota-fiscal.serie = ENTRY(2,lisa-integra.chave,"|") AND
            nota-fiscal.nr-nota-fis = ENTRY(3,lisa-integra.chave,"|") 
            NO-LOCK NO-ERROR.
       IF nota-fiscal.dt-cancela <> ? THEN DO.
          DELETE lisa-integra.
          NEXT.
       END.

       // Verifica se tem o PrePedido Gravado
       FIND ped-venda WHERE
            ped-venda.nr-pedcli = nota-fiscal.nr-pedcli AND
            ped-venda.nome-abrev = nota-fiscal.nome-ab-cli
            NO-LOCK NO-ERROR.
       
       FIND ped-venda-ext WHERE
            ped-venda-ext.cod-estabel = ped-venda.cod-estabel AND 
            ped-venda-ext.nr-pedido = ped-venda.nr-pedido
            SHARE-LOCK NO-ERROR.
        
       IF ped-venda-ext.nr-pedext = '' THEN DO.
          ASSIGN c-chave = ped-venda.nr-pedcli + "|" + 
                           ped-venda.nome-abrev.

          // Instru‡ao de Separa‡ao
          FIND b-lisa-integra WHERE
               b-lisa-integra.cod-trans = 'ISF' AND  
               b-lisa-integra.chave = c-chave NO-LOCK.

          IF ped-venda-ext.nr-pedext = '' THEN
             ASSIGN ped-venda-ext.nr-pedext = lisa-integra.val-livre-1.
       END.

       //tsp01 - inicio
       RUN esbo/boconsParam.p PERSIST SET hBoParam.
       RUN getTipoNFAprovPedLisa IN hBoParam(OUTPUT tpNfAprovPedLisa).
       IF VALID-HANDLE(hBoParam) THEN
          DELETE PROCEDURE hBoParam.

       IF lisa-integra.acao = 'ENVIAR' THEN
          /*RUN esapi/envia-nfs-venda-lisa.p (INPUT ROWID(nota-fiscal),
                                              INPUT NO
                                            ).   // NÆo ‚ Troca*/


          RUN esapi/envia-nfs-venda-lisa-2.p (INPUT ROWID(nota-fiscal),
                                              INPUT NO,
                                              INPUT tpNfAProvPedLisa).   // NÆo ‚ Troca
       //tsp01- fim

       ELSE IF lisa-integra.acao = 'EXCLUIR' THEN DO.
           // Exclui a Nota na Lisa
           RUN esapi/envia-canc-pedido-lisa.p (INPUT ROWID(ped-venda), 
                                               OUTPUT cErro).
       END.

       IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

       ASSIGN lisa-integra.ind-situacao = 2
              lisa-integra.acao = ''.

       FIND ped-venda WHERE
            ped-venda.nr-pedcli = nota-fiscal.nr-pedcli AND
            ped-venda.nome-abrev = nota-fiscal.nome-ab-cli
            NO-LOCK NO-ERROR.

       ASSIGN c-chave = ped-venda.nr-pedcli + "|" + 
                        ped-venda.nome-abrev.

       // Instru‡ao de Separa‡ao
       FIND b-lisa-integra WHERE
            b-lisa-integra.cod-trans = 'ISF' AND  
            b-lisa-integra.chave = c-chave SHARE-LOCK.
       ASSIGN b-lisa-integra.acao = ''
              b-lisa-integra.ind-situacao = 2.  // Finalizado
   END.
END.

IF tt-param.l-nota-retorno THEN DO.
    FOR EACH lisa-integra WHERE
             lisa-integra.cod-trans = "NotaRetorno" AND
             lisa-integra.ind-situacao = 1 // Aguardando Integra‡ao
             SHARE-LOCK.
    END.
END.

IF tt-param.l-conf-etiquetas THEN DO.
   // ajusta as Etiquetas
   FOR EACH lisa-integra WHERE
            lisa-integra.cod-trans = "ConfEtiquetas" AND
            lisa-integra.ind-situacao = 1 // Aguardando Integra‡ao
            SHARE-LOCK.


       RUN extrairInfCp5ConfEtq(lisa-integra.val-livre-5, OUTPUT nrContainer,OUTPUT idEtqLisa,OUTPUT localiz).

       IF lisa-integra.dt-trans = ? THEN
          ASSIGN lisa-integra.dt-trans = TODAY.

       ASSIGN lisa-integra.acao = TRIM(lisa-integra.acao).

       IF lisa-integra.chave <> '0' THEN DO.
          FIND ob-etiqueta WHERE
               ob-etiqueta.cod-estabel = '505' AND
               ob-etiqueta.num-etiqueta = INTEGER(lisa-integra.chave)
               SHARE-LOCK NO-ERROR.

          IF lisa-integra.acao = 'ALTERAR' THEN DO.
             IF ob-etiqueta.situacao = 3  THEN //somente pode alterar se estiver em estoque.
                ASSIGN ob-etiqueta.quantidade = DECIMAL(lisa-integra.conteudo).
             ELSE DO:
                 MESSAGE "A Etiqueta(J  Faturada):" + STRING(ob-etiqueta.num-etiqueta) + " tem a quantidade de:" STRING(ob-etiqueta.quantidade) + 
                   " sendo que no arquivo da LISA a quantidade informada ‚ :" + lisa-integra.conteudo
                  VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
             END.
          END.

          IF ob-etiqueta.quantidade = 0 OR 
             lisa-integra.acao = 'BAIXAR' THEN 
             ASSIGN ob-etiqueta.situacao = 9.  // Consumida
       END.
       ELSE DO.
           ASSIGN c-it-codigo = IF lisa-integra.val-livre-2 <> ''
                                THEN lisa-integra.val-livre-2 
                                ELSE SUBSTR(lisa-integra.val-livre-4,4).

           FIND FIRST ob-etiqueta WHERE
                      ob-etiqueta.cod-estab    = '505' AND 
                      ob-etiqueta.nr-container = nrContainer                   AND
                      ob-etiqueta.it-codigo    = c-it-codigo                   AND
                      ob-etiqueta.cod-refer    = lisa-integra.val-livre-3      AND
                      ob-etiqueta.num-rolo-imp  = INTEGER(lisa-integra.val-livre-1)
                      NO-LOCK NO-ERROR.
           IF NOT AVAIL ob-etiqueta THEN DO.
              RUN pi-cria-etiqueta.
              IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.
           END.
       END.

       ASSIGN lisa-integra.ind-situacao = 2.
   END.
END.


RUN pi-finalizar IN h-acomp.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
RETURN "OK":U.     

//----------- PROCEDURES ---------------------

PROCEDURE pi-ajusta-pedido.
   DEF INPUT PARAMETER p-row-ped-venda AS ROWID.
   DEF VAR c-desc-reserva AS CHAR.

   FIND ped-venda WHERE
        ROWID(ped-venda) = p-row-ped-venda NO-LOCK NO-ERROR.

   ASSIGN l-erro-res = NO.
   FOR EACH ped-item-res WHERE
            ped-item-res.cod-estabel = ped-venda.cod-estabel AND
            ped-item-res.nome-abrev = ped-venda.nome-abrev AND
            ped-item-res.nr-pedcli = ped-venda.nr-pedcli AND
            ped-item-res.qt-pedida > 0 NO-LOCK.    

       FIND ped-item OF ped-venda WHERE
            ped-item.nr-sequencia = ped-item-res.nr-sequencia NO-LOCK NO-ERROR.

       ASSIGN c-desc-reserva = "Seq. " + TRIM(STRING(ped-item.nr-sequencia,">>>9")) + " RESERVADA" .

       // Qtde Reservada est  diferente do Item do Pedido
       IF ped-item.qt-pedida <> ped-item-res.qt-pedida THEN DO.
          EMPTY TEMP-TABLE tt-ped-item.
          CREATE tt-ped-item.
          BUFFER-COPY ped-item TO tt-ped-item
                      ASSIGN tt-ped-item.qt-pedida = ped-item-res.qt-pedida.
           
          RUN esapi/altera-peditem-v2.p (INPUT TABLE tt-ped-item).
          IF RETURN-VALUE = 'NOK' THEN DO.
             ASSIGN l-erro-res = YES.
             UNDO, NEXT. 
          END.

          ASSIGN c-desc-reserva = c-desc-reserva + " e Ajustada a Quantidade" + 
                          "   De: " + TRIM(STRING(ped-item.qt-pedida,">>>,>>9.99")) +                   
                          " Para: " + TRIM(STRING(ped-item-res.qt-pedida,">>>,>>9.99")).

          RUN esapi/cria-log-pedvenda.p (INPUT ped-venda.nr-pedcli,
                                         INPUT ped-venda.nome-abrev,
                                         INPUT c-desc-reserva,
                                         INPUT YES).
       END.
    END.
    IF l-erro-res THEN RETURN 'ADM-ERROR'.

    RUN esapi/completa-pedvenda.p (INPUT ped-venda.nr-pedcli).
    RUN esapi/cria-log-pedvenda.p (INPUT ped-venda.nr-pedcli,
                                   INPUT ped-venda.nome-abrev,
                                   INPUT "SEPARA€ÇO Completa, disponivel para Faturamento", 
                                   INPUT YES).
END PROCEDURE.


PROCEDURE pi-gerar-nota-envio.
    DEF VAR c-natur-oper AS CHAR.
    DEF VAR i-nr-seq AS INT.
    
    ASSIGN c-natur-oper = '59207i'.

    FIND usuar-depos WHERE
         usuar-depos.cod-estab = estabelec.cod-estabel AND 
         usuar-depos.cod-usuario = c-seg-usuario NO-LOCK NO-ERROR.
    IF NOT AVAIL usuar-depos THEN DO.
        MESSAGE 'Usu rio ' + c-seg-usuario + ' nÆo est  relacionado ao Dep¢sito do Container' SKIP
                'Ou existe em mais de um deposito no mesmo Estabelecimento' 
                'Utilize cd1760' 
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        RETURN 'ADM-ERROR'.
    END.

    FIND deposito WHERE
         deposito.cod-depos = usuar-depos.cod-depos NO-LOCK NO-ERROR.

    FIND emitente WHERE
         emitente.nome-abrev = deposito.nome-abrev NO-LOCK NO-ERROR.

    CREATE tt-nota-fisc.
    ASSIGN tt-nota-fisc.cod-estabel = estabelec.cod-estabel  
           tt-nota-fisc.serie = estabelec.serie
           tt-nota-fisc.nome-ab-cli = emitente.nome-abrev
           tt-nota-fisc.nat-oper = c-natur-oper 
           tt-nota-fisc.dt-emis-nota = TODAY
           tt-nota-fisc.nro-proc-entrada = pp-container.nr-container. 

    ASSIGN i-nr-seq = 0.

    FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK.
        FIND ITEM WHERE
             ITEM.it-codigo = it-nota-fisc.it-codigo NO-LOCK NO-ERROR.
    
        ASSIGN i-nr-seq = i-nr-seq + 10.
        
        CREATE tt-it-nota-fisc.
        ASSIGN tt-it-nota-fisc.nr-seq-fat = i-nr-seq
               tt-it-nota-fisc.cod-estabel = tt-nota-fisc.cod-estabel  
               tt-it-nota-fisc.serie = tt-nota-fisc.serie
               tt-it-nota-fisc.nat-oper = tt-nota-fisc.nat-oper
               tt-it-nota-fisc.dt-emis-nota = tt-nota-fisc.dt-emis-nota
               tt-it-nota-fisc.it-codigo = it-nota-fisc.it-codigo
               tt-it-nota-fisc.cod-refer = it-nota-fisc.cod-refer
               tt-it-nota-fisc.cod-depos = 'ARM'
               tt-it-nota-fisc.un-fatur[1] = item.un
               tt-it-nota-fisc.un-fatur[2] = item.un
               tt-it-nota-fisc.qt-faturada[1] = it-nota-fisc.qt-faturada[1]
               tt-it-nota-fisc.qt-faturada[2] = it-nota-fisc.qt-faturada[1]
               tt-it-nota-fisc.vl-preuni = it-nota-fisc.vl-preuni.
    END.
    
    RUN esapi/cria-nota-ft4003.p (INPUT TABLE tt-nota-fisc,
                                  INPUT TABLE tt-it-nota-fisc,
                                  OUTPUT TABLE tt-notas-geradas).

    FIND FIRST tt-notas-geradas NO-LOCK NO-ERROR.
    IF NOT AVAIL tt-notas-geradas THEN 
       RETURN 'ADM-ERROR'.

    FIND nota-fiscal WHERE
         ROWID(nota-fiscal) = tt-notas-geradas.rw-nota-fiscal SHARE-LOCK NO-ERROR.
    IF nota-fiscal.nro-proc-entrada = 0 THEN 
       ASSIGN nota-fiscal.nro-proc-entrada = pp-container.nr-container.

    RETURN 'ADM-OK'.
END PROCEDURE.

PROCEDURE pi-cria-etiqueta.
    DEFINE VARIABLE idEtqLisa AS CHARACTER   NO-UNDO.
    FIND ITEM WHERE
         ITEM.it-codigo = c-it-codigo NO-LOCK NO-ERROR.

   
    CREATE ob-etiqueta.
    ASSIGN ob-etiqueta.cod-estabel     = '505'
           ob-etiqueta.dt-emissao      = TODAY
           ob-etiqueta.hr-emissao      = STRING(TIME,"HH:MM")
           ob-etiqueta.acondic         = ""
           ob-etiqueta.it-codigo       = c-it-codigo
           ob-etiqueta.cod-refer       = lisa-integra.val-livre-3
           ob-etiqueta.nr-container    = INT(entry(1,lisa-integra.val-livre-5,"|"))
           ob-etiqueta.nr-lote         = 'CA'
           ob-etiqueta.cod-qualid      = 'D' 
           ob-etiqueta.corte-comerc    = ''
           ob-etiqueta.quantidade      = DECIMAL(lisa-integra.conteudo)
           ob-etiqueta.localizacao     = localiz
           ob-etiqueta.situacao        = 3
           ob-etiqueta.cod-depos       = 'ITA'.

     ASSIGN ob-etiqueta.num-etiqueta = NEXT-VALUE(seq-etq-estoq-itj).

     ASSIGN ob-etiqueta.num-rolo-imp    = INTEGER(lisa-integra.val-livre-1)
            ob-etiqueta.ob-origem       = ''.

     IF NUM-ENTRIES(lisa-integra.val-livre-5,"|") > 1 THEN DO:
        ASSIGN idEtqLisa = ENTRY(2,lisa-integra.val-livre-5,"|").
        RUN esapi/gravarEtqLisa.p(ob-etiqueta.cod-estabel,
                                 ob-etiqueta.num-etiqueta,
                                 ob-etiqueta.it-Codigo,
                                 ob-etiqueta.cod-estabel,
                                 ob-etiqueta.nr-container,
                                 0,
                                 0,
                                 idEtqLisa,
                                 2).
     END.

END PROCEDURE.




PROCEDURE mudarEtqParaEstoque:

    DEFINE INPUT  PARAMETER pNrContainer AS INTEGER     NO-UNDO.

    FOR EACH ob-etiqueta EXCLUSIVE-LOCK
        WHERE ob-etiqueta.situacao  = 2
        AND   ob-etiqueta.nr-container = pNrContainer.
        ASSIGN ob-etiqueta.situacao = 3 .
    END.


END PROCEDURE.




