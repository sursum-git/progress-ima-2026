DEF VAR c-arq-saida AS CHAR.
DEF VAR c-var-saida AS CHAR.
DEF VAR c-cnpj-tra  LIKE transporte.cgc.
DEF VAR c-nome-tra  LIKE transporte.nome.
DEF VAR c-end-tra   LIKE transporte.endereco.
DEF VAR c-bai-tra   LIKE transporte.bairro.
DEF VAR c-cid-tra   LIKE transporte.cidade.
DEF VAR c-est-tra   LIKE transporte.estado.
DEF VAR c-cep-tra   LIKE transporte.cep.
DEF VAR c-pai-tra   LIKE transporte.pais.
DEF VAR c-tel-tra   LIKE transporte.telefone.
DEF VAR l-tem-residuo AS LOG.
DEF VAR c-mascara AS CHAR.

def var h-acomp as handle no-undo.

DEF BUFFER b-it-nota-fisc FOR it-nota-fisc.
DEF BUFFER b-nota-fiscal FOR nota-fiscal.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Pesquisando_Notas_Emitidas *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

FOR EACH b-nota-fiscal WHERE b-nota-fiscal.cod-estabel  =  "2"
                         AND b-nota-fiscal.serie        =  "1"
                         AND b-nota-fiscal.dt-emis-nota >= 01/01/2007
                         AND b-nota-fiscal.dt-cancela   =  ?
                        NO-LOCK:

    run pi-acompanhar in h-acomp (input "NF: " + b-nota-fiscal.nr-nota-fis +
                                        " - " + STRING(b-nota-fiscal.dt-emis-nota,"99/99/9999")). 

    ASSIGN l-tem-residuo = NO.
    FOR EACH b-it-nota-fisc OF b-nota-fiscal NO-LOCK.
        FIND ITEM WHERE ITEM.it-codigo = b-it-nota-fisc.it-codigo
                  NO-LOCK.
        IF AVAIL ITEM AND ITEM.fm-codigo = "62" THEN DO:
           ASSIGN l-tem-residuo = YES.
           LEAVE.
        END.
    END.
    IF l-tem-residuo THEN
       RUN pi-gera-isb.
END.

PROCEDURE pi-gera-isb:
FIND nota-fiscal WHERE nota-fiscal.cod-estabel = b-it-nota-fisc.cod-estabel
                   AND nota-fiscal.serie       = b-it-nota-fisc.serie
                   AND nota-fiscal.nr-nota-fis = b-it-nota-fisc.nr-nota-fis
                 NO-LOCK.
FIND transporte WHERE transporte.nome-abrev = nota-fiscal.nome-transp NO-LOCK NO-ERROR.
FIND emitente WHERE emitente.cod-emitente = nota-fiscal.cod-emitente NO-LOCK NO-ERROR.
IF NOT AVAIL transporte OR nota-fiscal.nome-transp = "" OR nota-fiscal.nome-transp = "proprio" THEN
   ASSIGN c-cnpj-tra = nota-fiscal.cgc
          c-nome-tra = emitente.nome-emit
          c-end-tra  = emitente.endereco
          c-bai-tra  = emitente.bairro
          c-cid-tra  = emitente.cidade
          c-est-tra  = emitente.estado
          c-cep-tra  = emitente.cep
          c-pai-tra  = emitente.pais
          c-tel-tra  = emitente.telefone[1].
ELSE
   ASSIGN c-cnpj-tra = transporte.cgc
          c-nome-tra = transporte.nome
          c-end-tra  = transporte.endereco 
          c-bai-tra  = transporte.bairro   
          c-cid-tra  = transporte.cidade   
          c-est-tra  = transporte.estado   
          c-cep-tra  = transporte.cep      
          c-pai-tra  = transporte.pais     
          c-tel-tra  = transporte.telefone.

FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK.
    FIND ITEM WHERE ITEM.it-codigo = it-nota-fisc.it-codigo
              NO-LOCK NO-ERROR.
    IF AVAIL ITEM AND ITEM.fm-codigo = "62" THEN DO: /* Res¡duos */
       ASSIGN c-arq-saida = "spool/NOTAS_E.TXT".
       OUTPUT TO VALUE(c-arq-saida) APPEND CONVERT SOURCE "ibm850".
       /* NUM_NF | VALOR | COD_PRODUTO | UNIDADE | QUANTIDADE | CNPJ_TRANSPORTADOR | NOME_TRANSPORTADOR | 
          ENDERECO_TRANSPORTADOR | BAIRRO_TRANSPORTADOR | CIDADE_TRANSPORTADOR | UF_TRANSPORTADOR | 
          PAIS_TRANSPORTADOR | CEP_TRANSPORTADOR | TELEFONE_TRANSPORTADOR | CPNJ_EMPRESA_DESTINO | 
          NOME_EMPRESA_DESTINO | ENDERECO_EMPRESA_DESTINO | BAIRRO_EMPRESA_DESTINO | CIDADE_EMPRESA_DESTINO | 
          UF_EMPRESA_DESTINO | PAIS_EMPRESA_DESTINO | CEP_EMPRESA_DESTINO | TELEFONE_EMPRESA_DESTINO | 
          DATA_SAIDA | HORA_SAIDA */

       ASSIGN c-var-saida = 
           TRIM(it-nota-fisc.nr-nota-fis) + "|" +
           TRIM(STRING(it-nota-fisc.qt-faturada[1] * it-nota-fisc.vl-preuni * 100,"999999999999")) + "|" +
           TRIM(it-nota-fisc.it-codigo) + "|" +
           TRIM(ITEM.un) + "|" +
           TRIM(STRING(it-nota-fisc.qt-faturada[1] * 100,"999999999999")) + "|" +
           TRIM(c-cnpj-tra) + "|" +
           TRIM(c-nome-tra) + "|" +
           TRIM(c-end-tra) + "|" +
           TRIM(c-bai-tra) + "|" +
           TRIM(c-cid-tra) + "|" +
           TRIM(c-est-tra) + "|" +
           TRIM(c-cep-tra) + "|" +
           TRIM(c-pai-tra) + "|" +
           TRIM(c-tel-tra) + "|" +
           TRIM(nota-fiscal.cgc) + "|" +
           TRIM(emitente.nome-emit) + "|" +
           TRIM(emitente.endereco) + "|" +
           TRIM(emitente.bairro) + "|" +
           TRIM(emitente.cidade) + "|" +
           TRIM(emitente.estado) + "|" +
           TRIM(emitente.pais) + "|" +
           TRIM(emitente.cep) + "|" +
           TRIM(emitente.telefone[1]) + "|" +
           STRING(DAY(nota-fiscal.dt-emis-nota),"99") + STRING(MONTH(nota-fiscal.dt-emis-nota),"99") +
                                               STRING(YEAR(nota-fiscal.dt-emis-nota),"9999") + "|" +
           STRING(TIME,"HH:MM").
       ASSIGN c-mascara = "x(" + STRING(LENGTH(c-var-saida),"999") + ")".
       PUT c-var-saida FORMAT c-mascara
           SKIP.
       OUTPUT CLOSE.
    END.
END.
END PROCEDURE.
