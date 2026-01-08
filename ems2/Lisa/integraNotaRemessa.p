DEFINE INPUT  PARAMETER pRowid AS ROWID       NO-UNDO.
DEFINE OUTPUT PARAMETER cErro  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-arq-xml AS CHARACTER   NO-UNDO.
DEFINE VARIABLE h-acomp   AS HANDLE      NO-UNDO.
{utp/ut-glob.i}
DEFINE TEMP-TABLE tt-nota-fisc      LIKE nota-fiscal.
DEFINE TEMP-TABLE tt-it-nota-fisc   LIKE it-nota-fisc.
DEFINE BUFFER b-lisa-integra FOR lisa-integra.


DEF TEMP-TABLE tt-notas-geradas NO-UNDO
    FIELD rw-nota-fiscal AS   ROWID
    FIELD nr-nota        LIKE nota-fiscal.nr-nota-fis
    FIELD seq-wt-docto   LIKE wt-docto.seq-wt-docto.

RUN utp/ut-acomp PERSIST SET h-acomp.
{utp/ut-liter.i Integra‡Æo Nota de Remessa *}
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

FIND lisa-integra NO-LOCK
    WHERE ROWID(lisa-integra) = pRowid NO-ERROR.



RUN pi-acompanhar IN h-acomp (INPUT "Integrando Nota do Container: " + lisa-integra.chave ).

FIND pp-container WHERE
            pp-container.nr-container = INTEGER(lisa-integra.chave) NO-LOCK NO-ERROR.

FIND processo-imp WHERE
    processo-imp.nr-proc-imp = STRING(pp-container.nr-container)  NO-LOCK NO-ERROR.

FIND FIRST item-doc-est WHERE
          item-doc-est.num-pedido = processo-imp.num-pedido NO-LOCK NO-ERROR.

FIND nota-fiscal WHERE
    nota-fiscal.cod-estabel = pp-container.cod-estabel AND
    nota-fiscal.serie       = item-doc-est.serie-docto AND
    nota-fiscal.nr-nota-fis = item-doc-est.nro-docto NO-LOCK NO-ERROR.

FIND estabelec OF nota-fiscal NO-LOCK NO-ERROR.
FIND param-nf-estab OF estabelec NO-LOCK NO-ERROR.

FIND emitente WHERE
    emitente.cod-emit = nota-fiscal.cod-emit NO-LOCK NO-ERROR.

IF lisa-integra.acao = 'GERAR' THEN DO.
  RUN pi-gerar-nota-envio.
  IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
     RUN pi-finalizar IN h-acomp.
     RETURN RETURN-VALUE.
  END.
     

  ASSIGN lisa-integra.acao = 'ENVIAR'.
END.

// Localizar a Nota Gerada
FIND nota-fiscal WHERE
    nota-fiscal.cod-estabel         = pp-container.cod-estabel AND
    nota-fiscal.nro-proc-entrada    = pp-container.nr-container AND
    nota-fiscal.cod-protoc          <> '' AND
    nota-fiscal.dt-cancela          = ? 
    NO-LOCK NO-ERROR.
IF AVAIL nota-fiscal THEN DO.
   IF nota-fiscal.cod-protoc = '' THEN DO.
      CREATE lisa-log-integr.
      ASSIGN lisa-log-integr.cod-trans   = lisa-integra.cod-trans   
             lisa-log-integr.data        = TODAY
             lisa-log-integr.hora        = TIME
             lisa-log-integr.usuario     = c-seg-usuario
             lisa-log-integr.acao        = 'ENVIAR' 
             lisa-log-integr.log-erro    = YES   
             lisa-log-integr.narrativa   = 'Nota ' + nota-fiscal.nr-nota-fis + ' sem Protoclo de Autorizaçao'.
      ASSIGN cErro = lisa-log-integr.narrativa.
      RETURN 'adm-error':u.
   END.
 
   ASSIGN c-arq-xml = param-nf-estab.cod-caminho-xml + "\" + nota-fiscal.cod-estabel + FILL("0", (3 - LENGTH(nota-fiscal.serie))) + nota-fiscal.serie + nota-fiscal.nr-nota-fis + ".xml".
   FILE-INFO:FILE-NAME = c-arq-xml.
   IF FILE-INFO:FILE-NAME = ? THEN  DO:
      ASSIGN cErro = "Arquivo:" + c-arq-xml + " não encontrado".
      RUN pi-finalizar IN h-acomp.
      RETURN 'ADM-ERROR'.
   END.
 
   RUN pi-acompanhar IN h-acomp (INPUT "Enviando Nota " + nota-fiscal.nr-nota-fis +  " do Container: " + lisa-integra.chave ).
 
   // se chegar aqui é porque achou o xml
   RUN esapi/envia-nfs-remessa-lisa.p (INPUT ROWID(nota-fiscal)).
   IF RETURN-VALUE <> 'ADM-OK' THEN DO:
      ASSIGN cErro = "Erro no envio da NFS de Remessa".
      RUN pi-finalizar IN h-acomp.
      RETURN RETURN-VALUE.
 
   END.
 
   FIND CURRENT lisa-integra EXCLUSIVE-LOCK NO-ERROR.
                                            
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
   RELEASE lisa-integra.

   

END.
RUN pi-finalizar IN h-acomp.


PROCEDURE pi-gerar-nota-envio.
    DEF VAR c-natur-oper AS CHAR.
    DEF VAR i-nr-seq AS INT.
    
    ASSIGN c-natur-oper = '59207i'.

    FIND usuar-depos WHERE
         usuar-depos.cod-estab      = estabelec.cod-estabel AND 
         usuar-depos.cod-usuario    = c-seg-usuario NO-LOCK NO-ERROR.
    IF NOT AVAIL usuar-depos THEN DO.
        ASSIGN cErro =  'Usu rio ' + c-seg-usuario + ' nÆo est  relacionado ao Dep¢sito do Container ou existe em mais de um deposito no mesmo Estabelecimento - Utilize cd1760' .
        RETURN 'ADM-ERROR'.
    END.

    FIND deposito WHERE
         deposito.cod-depos = usuar-depos.cod-depos NO-LOCK NO-ERROR.

    FIND emitente WHERE
         emitente.nome-abrev = deposito.nome-abrev NO-LOCK NO-ERROR.

    CREATE tt-nota-fisc.
    ASSIGN tt-nota-fisc.cod-estabel         = estabelec.cod-estabel  
           tt-nota-fisc.serie               = estabelec.serie
           tt-nota-fisc.nome-ab-cli         = emitente.nome-abrev
           tt-nota-fisc.nat-oper            = c-natur-oper 
           tt-nota-fisc.dt-emis-nota        = TODAY
           tt-nota-fisc.nro-proc-entrada    = pp-container.nr-container. 

    ASSIGN i-nr-seq = 0.

    FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK.

        FIND ITEM WHERE
             ITEM.it-codigo = it-nota-fisc.it-codigo NO-LOCK NO-ERROR.
    
        ASSIGN i-nr-seq = i-nr-seq + 10.
        
        CREATE tt-it-nota-fisc.
        ASSIGN tt-it-nota-fisc.nr-seq-fat       = i-nr-seq
               tt-it-nota-fisc.cod-estabel      = tt-nota-fisc.cod-estabel  
               tt-it-nota-fisc.serie            = tt-nota-fisc.serie
               tt-it-nota-fisc.nat-oper         = tt-nota-fisc.nat-oper
               tt-it-nota-fisc.dt-emis-nota     = tt-nota-fisc.dt-emis-nota
               tt-it-nota-fisc.it-codigo        = it-nota-fisc.it-codigo
               tt-it-nota-fisc.cod-refer        = it-nota-fisc.cod-refer
               tt-it-nota-fisc.cod-depos        = 'ARM'
               tt-it-nota-fisc.un-fatur[1]      = item.un
               tt-it-nota-fisc.un-fatur[2]      = item.un
               tt-it-nota-fisc.qt-faturada[1]   = it-nota-fisc.qt-faturada[1]
               tt-it-nota-fisc.qt-faturada[2]   = it-nota-fisc.qt-faturada[1]
               tt-it-nota-fisc.vl-preuni        = it-nota-fisc.vl-preuni .

    END.
    
    RUN esapi/cria-nota-ft4003.p (INPUT TABLE tt-nota-fisc,
                                  INPUT TABLE tt-it-nota-fisc,
                                  OUTPUT TABLE tt-notas-geradas).

    FIND FIRST tt-notas-geradas NO-LOCK NO-ERROR.
    IF NOT AVAIL tt-notas-geradas THEN DO:
       RUN pi-finalizar IN h-acomp.
       ASSIGN cErro = "Nenhuma nota fiscal foi gerada".
       RETURN 'ADM-ERROR'.
    END.
       

    FIND nota-fiscal WHERE
         ROWID(nota-fiscal) = tt-notas-geradas.rw-nota-fiscal SHARE-LOCK NO-ERROR.
    IF nota-fiscal.nro-proc-entrada = 0 THEN 
       ASSIGN nota-fiscal.nro-proc-entrada = pp-container.nr-container.

    RETURN 'ADM-OK'.
END PROCEDURE.


