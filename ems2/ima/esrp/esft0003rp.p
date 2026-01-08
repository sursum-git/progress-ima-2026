/******************************************************************************
** PROGRAMA: ESFT0003RP.P                                                    **
** DATA    : DEZEMBRO                                                        **
** AUTOR   : Marcelo Torres                                                  **
** OBJETIVO: Exporta‡Æo para o Serasa                                        **
******************************************************************************/
DEF BUFFER empresa FOR mgcad.empresa.
/* Programa de controle de versao e seguran»a do Datasul EMS */

{include/i-prgvrs.i ESFT0003 2.04.00.001}

/* Definicao de variaveis globais comuns a todos os programas do EMS */
{utp/ut-glob.i}

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)"
    field modelo-rtf       as char format "x(35)"
    field l-habilitaRtf    as LOG
    FIELD dt-datini        AS DATE FORMAT "99/99/9999"
    FIELD dt-datfim        AS DATE FORMAT "99/99/9999"
    FIELD c-periodo        AS CHARACTER FORMAT "x"
    FIELD i-tipo-rel       AS INTEGER
    FIELD dt-datexp        AS DATE FORMAT "99/99/9999"
    FIELD c-nomarq         AS CHARACTER FORMAT "x(80)".

/* Parametros de entrada logica obrigatoria */
DEF TEMP-TABLE tt-raw-digita
    FIELD raw-digita                   AS RAW.

DEF INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEF INPUT PARAMETER TABLE FOR tt-raw-digita.       

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

FIND FIRST tt-param.

DEFINE TEMP-TABLE tt-remhdr
    FIELD ident   AS CHARACTER FORMAT "x(02)" INITIAL "00"
    FIELD descr   AS CHARACTER FORMAT "x(20)" INITIAL "RELATO COMP NEGOCIOS"
    FIELD cnpj    AS CHARACTER FORMAT "x(14)"
    FIELD inicio  AS CHARACTER FORMAT "x(08)" INITIAL "CORRECAO"
    FIELD fim     AS CHARACTER FORMAT "x(08)"
    FIELD period  AS CHARACTER FORMAT "x"  /* D=Diario M=Mensal S=Semanal Q=Quinzenal */
    FIELD reserv  AS CHARACTER FORMAT "x(15)"
    FIELD filler1 AS CHARACTER FORMAT "x(03)"
    FIELD filler2 AS CHARACTER FORMAT "x(29)"
    FIELD id-lay1 AS CHARACTER FORMAT "x(02)" INITIAL "V."
    FIELD id-lay2 AS CHARACTER FORMAT "x(02)" INITIAL "01"
    FIELD filler3 AS CHARACTER FORMAT "x(26)".

DEFINE TEMP-TABLE tt-remdet-tempo
    FIELD ident   AS CHARACTER FORMAT "x(02)" INITIAL "01"
    FIELD cnpj    AS CHARACTER FORMAT "x(14)"
    FIELD tipo    AS CHARACTER FORMAT "x(02)" INITIAL "01"
    FIELD desde   AS CHARACTER FORMAT "x(08)"
    FIELD tipcli  AS CHARACTER FORMAT "x(01)"
    FIELD filler1 AS CHARACTER FORMAT "x(38)"
    FIELD filler2 AS CHARACTER FORMAT "x(34)"
    FIELD filler3 AS CHARACTER FORMAT "x(01)"
    FIELD filler4 AS CHARACTER FORMAT "x(30)".

DEFINE TEMP-TABLE tt-titcon
    FIELD numtit     AS CHARACTER FORMAT "x(07)"
    FIELD parcela    AS CHARACTER FORMAT "x(02)"
    FIELD cod-emit   AS INT.

DEFINE TEMP-TABLE tt-remdet
    FIELD ident   AS CHARACTER FORMAT "x(02)" INITIAL "01"
    FIELD cnpj    AS CHARACTER FORMAT "x(14)"
    FIELD tipo    AS CHARACTER FORMAT "x(02)" INITIAL "05"
    FIELD numtit  AS CHARACTER FORMAT "x(10)"
    FIELD emissao AS CHARACTER FORMAT "x(08)"
    FIELD valor   AS CHARACTER FORMAT "x(13)"
    FIELD vencto  AS CHARACTER FORMAT "x(08)"
    FIELD pagto   AS CHARACTER FORMAT "x(08)"
    FIELD reserva AS CHARACTER FORMAT "x(34)"
    FIELD filler1 AS CHARACTER FORMAT "x(01)"
    FIELD filler2 AS CHARACTER FORMAT "x(24)"
    FIELD filler3 AS CHARACTER FORMAT "x(02)"
    FIELD filler4 AS CHARACTER FORMAT "x(01)"
    FIELD filler5 AS CHARACTER FORMAT "x(03)"
    FIELD parcela AS CHARACTER FORMAT "x(02)"
    FIELD tit_acr AS CHARACTER FORMAT "x(12)"
    INDEX idx-remdet cnpj tit_acr.

DEFINE TEMP-TABLE tt-remtrl
    FIELD ident   AS CHARACTER FORMAT "x(02)" INITIAL "99"
    FIELD qtdtmp  AS CHARACTER FORMAT "x(11)"
    FIELD zeros1  AS CHARACTER FORMAT "x(44)" INITIAL "00000000000000000000000000000000000000000000"
    FIELD qtdreg  AS CHARACTER FORMAT "x(11)"
    FIELD zeros2  AS CHARACTER FORMAT "x(11)" INITIAL "00000000000"
    FIELD zeros3  AS CHARACTER FORMAT "x(11)" INITIAL "00000000000"
    FIELD filler1 AS CHARACTER FORMAT "x(09)"
    FIELD filler2 AS CHARACTER FORMAT "x(30)".

DEFINE TEMP-TABLE tt-rethdr
    FIELD numero  AS CHARACTER FORMAT "x(02)"
    FIELD texto   AS CHARACTER FORMAT "x(186)".

DEFINE TEMP-TABLE tt-reterr
    FIELD numero  AS CHARACTER FORMAT "x(03)"
    FIELD descri  AS CHARACTER FORMAT "x(70)".

DEFINE TEMP-TABLE tt-retdet
    FIELD cnpj    AS CHARACTER FORMAT "x(14)"
    FIELD numtit  AS CHARACTER FORMAT "x(10)"
    FIELD parcela AS CHARACTER FORMAT "x(02)"
    FIELD emissao AS CHARACTER FORMAT "x(08)"
    FIELD valor   AS CHARACTER FORMAT "x(13)"
    FIELD vencto  AS CHARACTER FORMAT "x(08)"
    FIELD pagto   AS CHARACTER FORMAT "x(08)"
    FIELD reserva AS CHARACTER FORMAT "x(34)"
    FIELD erros   AS CHARACTER FORMAT "x(90)".

DEFINE TEMP-TABLE tt-retimp
    FIELD numtit  AS CHARACTER FORMAT "x(10)"
    FIELD parcela AS CHARACTER FORMAT "x(02)"
    FIELD cnpj    AS CHARACTER FORMAT "x(18)"
    FIELD emissao AS DATE      FORMAT "99/99/9999"
    FIELD valor   AS DECIMAL   FORMAT ">,>>>,>>>,>>9.99"
    FIELD vencto  AS DATE      FORMAT "99/99/9999"
    FIELD pagto   AS DATE      FORMAT "99/99/9999"
    FIELD coderr  AS CHARACTER FORMAT "x(03)"
    FIELD deserr  AS CHARACTER FORMAT "x(50)"
    INDEX idx-retimp numtit coderr.

DEFINE TEMP-TABLE tt-rettrl1
    FIELD qtdcli  AS CHARACTER FORMAT "x(06)".

DEFINE TEMP-TABLE tt-rettrl2
    FIELD qtdpag  AS CHARACTER FORMAT "x(08)"
    FIELD soma    AS CHARACTER FORMAT "x(18)".

DEFINE TEMP-TABLE tt-cliente
    FIELD cod-emitente LIKE emitente.cod-emitente.

DEFINE VARIABLE h-acomp     AS HANDLE     NO-UNDO.

DEFINE VARIABLE c-cnpj      AS CHARACTER FORMAT "x(14)"      NO-UNDO.
DEFINE VARIABLE c-tipmov    AS CHARACTER FORMAT "x(01)"      NO-UNDO.
DEFINE VARIABLE c-num       AS CHARACTER FORMAT "x(20)"      NO-UNDO.
DEFINE VARIABLE c-arquivo   AS CHARACTER                     NO-UNDO.
DEFINE VARIABLE c-linha     AS CHARACTER FORMAT "x(190)"     NO-UNDO.
DEFINE VARIABLE c-mens      AS CHARACTER                     NO-UNDO.

DEFINE VARIABLE i-ind       AS INTEGER                       NO-UNDO.
DEFINE VARIABLE i-qtdrem    AS INTEGER                       NO-UNDO.
DEFINE VARIABLE i-qtdtmp    AS INTEGER                       NO-UNDO.
DEFINE VARIABLE i-ordem     AS INTEGER                       NO-UNDO.
DEFINE VARIABLE i-quant     AS INTEGER                       NO-UNDO.

DEFINE VARIABLE lg-concilia AS LOGICAL                       NO-UNDO.  /*  YES=Arquivo de concilia‡Æo  */
DEFINE VARIABLE var-dt-arq  AS DATE      FORMAT "99/99/9999" NO-UNDO.

DEFINE VARIABLE dt-datini   AS DATE      FORMAT "99/99/9999" NO-UNDO.
DEFINE VARIABLE dt-datfim   AS DATE      FORMAT "99/99/9999" NO-UNDO.

DEFINE VARIABLE i-tipo      AS INTEGER                       NO-UNDO.  /*  1-Exporta‡Æo  2-Re-exporta‡Æo  */
DEFINE VARIABLE dt-datexp   AS DATE      FORMAT "99/99/9999" NO-UNDO.  /*  Data de exporta‡Æo (para o caso de re-exporta‡Æo  */

DEFINE VARIABLE i-servico   AS INTEGER                       NO-UNDO.  /*  1-Exporta‡Æo  2-Retorno  */

DEFINE STREAM s-arq.

DEFINE SHARED VARIABLE lg-houve-erros AS LOGICAL             NO-UNDO.

DEFINE BUFFER b-movto_tit_acr FOR movto_tit_acr.

FORM
   tt-retimp.numtit   COLUMN-LABEL "Titulo"
   tt-retimp.parcela  COLUMN-LABEL "Parcela"
   tt-retimp.cnpj     COLUMN-LABEL "CNPJ"
   tt-retimp.emissao  COLUMN-LABEL "EmissÆo"
   tt-retimp.valor    COLUMN-LABEL "Valor"
   tt-retimp.vencto   COLUMN-LABEL "Vencto"
   tt-retimp.pagto    COLUMN-LABEL "Pagto"
   tt-retimp.deserr   COLUMN-LABEL "Erro"
   WITH WIDTH 255 55 DOWN STREAM-IO FRAME f-detalhe.

{include/i-rpvar.i}   

ASSIGN lg-concilia = NO.

IF tt-param.i-tipo-rel = 1 THEN   /* Exporta‡Æo */
    ASSIGN i-tipo    = 1
           i-servico = 1.
ELSE
    IF tt-param.i-tipo-rel = 2 THEN  /* Re-Exporta‡Æo */
        ASSIGN i-tipo    = 2
               i-servico = 1.
    ELSE
        ASSIGN i-tipo    = 3         /* Importa‡Æo */
               i-servico = 2.

FIND empresa NO-LOCK WHERE
     empresa.ep-codigo = "1" NO-ERROR.
IF NOT AVAIL empresa THEN
   RETURN "ADM-ERROR":U.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.

IF i-servico = 1 THEN DO:
    RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).
    RUN pi-exporta (INPUT NO).
END.
ELSE DO:
    ASSIGN c-programa     = "ESFT0003RP":U
           c-versao       = "2.04":U
           c-revisao      = ".00.001":U
           c-empresa      = empresa.razao-social
           c-sistema      = "EMS":U.

    ASSIGN c-titulo-relat = "RELATORIO DE ERROS NO ARQUIVO DE RETORNO":U.

    {include/i-rpout.i}

    {include/i-rpcab.i}
    VIEW FRAME f-cabec.
    VIEW FRAME f-rodape.

    {utp/ut-liter.i Imprimindo *}
    RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

    RUN pi-retorno.
END.

RUN pi-finalizar in h-acomp.


/*************************** PROCEDURES *****************************/
PROCEDURE pi-exporta.
    DEFINE INPUT PARAMETER p-concilia AS LOGICAL.

    RUN pi-acompanhar IN  h-acomp (INPUT "Buscando Titulo").

    ASSIGN c-cnpj    = TRIM(empresa.cgc)
           dt-datini = tt-param.dt-datini
           dt-datfim = tt-param.dt-datfim
           dt-datexp = tt-param.dt-datexp
           c-tipmov  = IF p-concilia THEN "M" ELSE tt-param.c-periodo
           c-arquivo = tt-param.c-nomarq.
    
    IF p-concilia THEN DO:
        IF c-arquivo MATCHES "*.txt*" THEN
            ASSIGN c-arquivo = REPLACE(c-arquivo, ".txt", "-rem.txt").
        ELSE
            ASSIGN c-arquivo = c-arquivo + "-rem.txt".
    END.
    ELSE DO:
        IF i-tipo <> 1 THEN DO:
            FIND FIRST ima-titulo-ext
                WHERE ima-titulo-ext.dt-exporta   = dt-datexp AND
                      ima-titulo-ext.sit-retorno >= 2
                NO-ERROR.
    
            IF AVAIL ima-titulo-ext THEN
                ASSIGN c-tipmov = ima-titulo-ext.period.
            ELSE
                ASSIGN c-tipmov = "D".
        END.
    END.


    CREATE tt-remhdr.
    ASSIGN tt-remhdr.cnpj = c-cnpj.

    IF i-tipo = 1 THEN
        ASSIGN tt-remhdr.inicio = SUBSTRING(STRING(dt-datini, "99/99/9999"), 7, 4) +
                                  SUBSTRING(STRING(dt-datini, "99/99/9999"), 4, 2) +
                                  SUBSTRING(STRING(dt-datini, "99/99/9999"), 1, 2).
    ELSE
        IF p-concilia THEN
            ASSIGN tt-remhdr.inicio = "CONCILIA".
        ELSE
            ASSIGN tt-remhdr.inicio = "CORRECAO".
        
    ASSIGN tt-remhdr.fim    = SUBSTRING(STRING(dt-datfim, "99/99/9999"), 7, 4) +
                              SUBSTRING(STRING(dt-datfim, "99/99/9999"), 4, 2) +
                              SUBSTRING(STRING(dt-datfim, "99/99/9999"), 1, 2)
           tt-remhdr.period = c-tipmov.
    
    ASSIGN i-qtdrem = 0
           i-qtdtmp = 0
           i-ordem  = 0.
    
    IF p-concilia = NO THEN DO:
        IF i-tipo = 1 THEN DO:
           FOR EACH movto_tit_acr WHERE 
                    movto_tit_acr.dat_transacao   >= dt-datini AND
                    movto_tit_acr.dat_transacao   <= dt-datfim AND
                    movto_tit_acr.cod_espec_docto  = "DP"      AND
                    (movto_tit_acr.ind_trans_acr   = 'implantacao' OR 
                     movto_tit_acr.ind_trans_acr   = 'liquidacao') NO-LOCK
                    BY movto_tit_acr.dat_transacao:
    
                FIND tit_acr OF movto_tit_acr NO-LOCK NO-ERROR.

                RUN pi-acompanhar IN  h-acomp (INPUT "Buscando tit_acrs - " + STRING(movto_tit_acr.dat_transacao, "99/99/9999")).

                RUN pi-exporta-tit_acr (INPUT movto_tit_acr.cod_empresa,
                                        INPUT movto_tit_acr.cod_estab,
                                        INPUT movto_tit_acr.cod_espec_docto,
                                        INPUT tit_acr.cod_ser_docto,
                                        INPUT tit_acr.cod_ser_docto,
                                        INPUT tit_acr.cod_parcela,
                                        INPUT i-tipo).
           END.
        END.
        ELSE DO:
            FOR EACH ima-titulo-ext WHERE 
                     ima-titulo-ext.dt-exporta = dt-datexp NO-LOCK
                     BY ima-titulo-ext.nr-docto:
    
                RUN pi-acompanhar IN  h-acomp (INPUT "Buscando tit_acrs - " + TRIM(ima-titulo-ext.nr-docto)).
    
                RUN pi-exporta-tit_acr (INPUT ima-titulo-ext.ep-codigo,
                                        INPUT ima-titulo-ext.cod-estabel,
                                        INPUT ima-titulo-ext.cod-esp,
                                        INPUT ima-titulo-ext.serie,
                                        INPUT ima-titulo-ext.nr-docto,
                                        INPUT ima-titulo-ext.parcela,
                                        INPUT i-tipo).
            END.
        END.
        
        FOR EACH tt-cliente NO-LOCK:
        
            ASSIGN i-qtdtmp = i-qtdtmp + 1.
        
            RUN pi-acompanhar IN  h-acomp (INPUT "Buscando Clientes - " + STRING(emitente.cod-emitente)).
        
            FIND FIRST emitente
                WHERE emitente.cod-emitente = tt-cliente.cod-emitente
                NO-LOCK NO-ERROR.
        
            CREATE tt-remdet-tempo.
            ASSIGN tt-remdet-tempo.cnpj   = emitente.cgc
                   tt-remdet-tempo.desde  = SUBSTRING(STRING(emitente.data-implant, "99/99/9999"), 7, 4) +
                                            SUBSTRING(STRING(emitente.data-implant, "99/99/9999"), 4, 2) +
                                            SUBSTRING(STRING(emitente.data-implant, "99/99/9999"), 1 ,2).
        
            IF emitente.ind-sit-emitente = 1 THEN
                IF (TODAY - emitente.data-implant) > 365 THEN
                    ASSIGN tt-remdet-tempo.tipcli = "1".  /* Cliente antigo */
                ELSE
                    ASSIGN tt-remdet-tempo.tipcli = "2".  /* Cliente novo */
            ELSE
                ASSIGN tt-remdet-tempo.tipcli = "3".  /* Inativo */
        END.
    END.
    
    ASSIGN i-quant = 0.
    FOR EACH tt-remdet NO-LOCK:
        ASSIGN i-quant = i-quant + 1.
    END.

    IF i-quant = 0 THEN DO:
       MESSAGE "NÆo foram encontrados registros para serem enviados ao Serasa"
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       RETURN.
    END.

    OUTPUT STREAM s-arq TO VALUE(c-arquivo).
        /* GRAVA HEADER */
        FOR EACH tt-remhdr:
            PUT STREAM s-arq tt-remhdr.ident.
            PUT STREAM s-arq tt-remhdr.descr.
            PUT STREAM s-arq tt-remhdr.cnpj.
            PUT STREAM s-arq tt-remhdr.inicio.
            PUT STREAM s-arq tt-remhdr.fim.
            PUT STREAM s-arq tt-remhdr.period.
            PUT STREAM s-arq tt-remhdr.reserv.
            PUT STREAM s-arq tt-remhdr.filler1.
            PUT STREAM s-arq tt-remhdr.filler2.
            PUT STREAM s-arq tt-remhdr.id-lay1.
            PUT STREAM s-arq tt-remhdr.id-lay2.
            PUT STREAM s-arq tt-remhdr.filler3.
            PUT STREAM s-arq SKIP.
        END.
        
        /* GRAVA DETALHE - TEMPO DE RELACIONAMENTO */
        RUN pi-acompanhar IN  h-acomp (INPUT "Gravando clientes").
    
        ASSIGN i-qtdtmp = 0.
    
        FOR EACH tt-remdet-tempo:
            PUT STREAM s-arq tt-remdet-tempo.ident.
            PUT STREAM s-arq tt-remdet-tempo.cnpj.
            PUT STREAM s-arq tt-remdet-tempo.tipo.
            PUT STREAM s-arq tt-remdet-tempo.desde.
            PUT STREAM s-arq tt-remdet-tempo.tipcli.
            PUT STREAM s-arq tt-remdet-tempo.filler1.
            PUT STREAM s-arq tt-remdet-tempo.filler2.
            PUT STREAM s-arq tt-remdet-tempo.filler3.
            PUT STREAM s-arq tt-remdet-tempo.filler4.
            PUT STREAM s-arq SKIP.
    
            ASSIGN i-qtdtmp = i-qtdtmp + 1.
        END.
        
        /* GRAVA DETALHE - tit_acrS */
        RUN pi-acompanhar IN  h-acomp (INPUT "Gravando t¡tulos").
    
        ASSIGN i-qtdrem = 0.
    
        FOR EACH tt-remdet:
            PUT STREAM s-arq tt-remdet.ident.
            PUT STREAM s-arq tt-remdet.cnpj.
            PUT STREAM s-arq tt-remdet.tipo.
            PUT STREAM s-arq tt-remdet.numtit.
            PUT STREAM s-arq tt-remdet.emissao.
            PUT STREAM s-arq tt-remdet.valor.
            PUT STREAM s-arq tt-remdet.vencto.
            PUT STREAM s-arq tt-remdet.pagto.
            PUT STREAM s-arq tt-remdet.reserva.
            PUT STREAM s-arq tt-remdet.filler1.
            PUT STREAM s-arq tt-remdet.filler2.
            PUT STREAM s-arq tt-remdet.filler3.
            PUT STREAM s-arq tt-remdet.filler4.
            PUT STREAM s-arq tt-remdet.filler5.
            PUT STREAM s-arq SKIP.
    
            ASSIGN i-qtdrem = i-qtdrem + 1.
        END.
        
        CREATE tt-remtrl.
        ASSIGN tt-remtrl.qtdtmp = STRING(i-qtdtmp, "99999999999")
               tt-remtrl.qtdreg = STRING(i-qtdrem, "99999999999").
        
        /* GRAVA TRAILLER */
        FOR EACH tt-remtrl:
            PUT STREAM s-arq tt-remtrl.ident.
            PUT STREAM s-arq tt-remtrl.qtdtmp.
            PUT STREAM s-arq tt-remtrl.zeros1.
            PUT STREAM s-arq tt-remtrl.qtdreg.
            PUT STREAM s-arq tt-remtrl.zeros2.
            PUT STREAM s-arq tt-remtrl.zeros3.
            PUT STREAM s-arq tt-remtrl.filler1.
            PUT STREAM s-arq tt-remtrl.filler2.
            PUT STREAM s-arq SKIP.
        END.

    OUTPUT STREAM s-arq CLOSE.
END PROCEDURE.



PROCEDURE pi-retorno.
    DEFINE VARIABLE i-cont AS INTEGER     NO-UNDO.
    
    ASSIGN c-arquivo = tt-param.c-nomarq.
                                                       
    RUN pi-acompanhar IN  h-acomp (INPUT "Tratando arquivo de retorno").

    INPUT STREAM s-arq FROM VALUE(c-arquivo) NO-ECHO.
    REPEAT:
        IMPORT STREAM s-arq DELIMITER "ù" c-linha.
        
        RUN pi-acompanhar IN  h-acomp (INPUT "Importando " + SUBSTRING(c-linha,03,14) + " / " + SUBSTRING(c-linha,68,10)).

        IF SUBSTRING(c-linha, 37, 08) = "CONCILIA" THEN
            ASSIGN lg-concilia = YES
                   var-dt-arq = DATE(INT(SUBSTRING(c-linha, 62, 02)),INT(SUBSTRING(c-linha, 59, 02)),INT(SUBSTRING(c-linha, 65, 04))).

        IF lg-concilia THEN DO:
            
            IF SUBSTRING(c-linha, 1, 2) = "01" THEN DO:
                ASSIGN i-cont = i-cont + 1.
                FIND FIRST emitente WHERE 
                           emitente.cgc = SUBSTRING(c-linha, 03, 14) 
                           USE-INDEX cgc NO-LOCK NO-ERROR.
                CREATE tt-titcon.
                ASSIGN tt-titcon.numtit     = STRING(INTEGER(SUBSTRING(c-linha, 68, 10)), "9999999")
                       tt-titcon.parcela    = SUBSTRING(c-linha, 79, 2)
                       tt-titcon.cod-emit   = emitente.cod-emitente WHEN AVAIL emitente.
            END.
        END.
        ELSE DO:
            IF SUBSTRING(c-linha, 1, 2) = "77" THEN DO:
               CREATE tt-rethdr.
               ASSIGN tt-rethdr.numero = SUBSTRING(c-linha, 3, 2)
                      tt-rethdr.texto  = SUBSTRING(c-linha, 5, 186).
            END.
    
            IF SUBSTRING(c-linha, 1, 2) = "88" THEN DO:
               CREATE tt-reterr.
               ASSIGN tt-reterr.numero = SUBSTRING(c-linha, 3, 3)
                      tt-reterr.descri = SUBSTRING(c-linha, 8, 70).
            END.
    
            IF SUBSTRING(c-linha, 1, 2) = "01" AND SUBSTRING(c-linha, 17, 2) = "05" THEN DO:
               CREATE tt-retdet.
               ASSIGN tt-retdet.cnpj    = SUBSTRING(c-linha, 3, 14)
                      tt-retdet.numtit  = SUBSTRING(c-linha, 68, 10)
                      tt-retdet.parcela = SUBSTRING(c-linha, 79, 2)
                      tt-retdet.emissao = SUBSTRING(c-linha, 35, 2) + "/" + SUBSTRING(c-linha, 33, 2) + "/" + SUBSTRING(c-linha, 29, 4)
                      tt-retdet.valor   = SUBSTRING(c-linha, 37, 13)
                      tt-retdet.vencto  = SUBSTRING(c-linha, 56, 2) + "/" + SUBSTRING(c-linha, 54, 2) + "/" + SUBSTRING(c-linha, 50, 4)
                      tt-retdet.pagto   = SUBSTRING(c-linha, 64, 2) + "/" + SUBSTRING(c-linha, 62, 2) + "/" + SUBSTRING(c-linha, 58, 4)
                      tt-retdet.erros   = SUBSTRING(c-linha, 101, 90).
    
            END.
            ELSE DO:
                CREATE tt-rettrl1.
                ASSIGN tt-rettrl1.qtdcli = SUBSTRING(c-linha, 3, 6).
            END.
    
            IF SUBSTRING(c-linha, 1, 2) = "05" THEN DO:
                CREATE tt-rettrl2.
                ASSIGN tt-rettrl2.qtdpag = SUBSTRING(c-linha, 3, 8)
                       tt-rettrl2.soma   = SUBSTRING(c-linha, 11, 18).
            END.
        END.
    END.

    INPUT STREAM s-arq CLOSE.
    ASSIGN i-cont = 0.

    IF lg-concilia THEN DO:
       RUN pi-acompanhar IN  h-acomp (INPUT "Conciliando...").

       FOR EACH tt-titcon NO-LOCK:
           RUN pi-acompanhar IN  h-acomp (INPUT "Exportando t¡tulo " + tt-titcon.numtit + "/" + tt-titcon.parcela).

           FIND FIRST tit_acr WHERE 
                      tit_acr.cod_empresa = '100' AND 
                      tit_acr.cdn_cliente = tt-titcon.cod-emit AND 
                      tit_acr.cod_tit_acr = tt-titcon.numtit AND 
                      tit_acr.cod_parcela = tt-titcon.parcela  NO-LOCK NO-ERROR.

           FIND FIRST movto_tit_acr OF tit_acr WHERE
                      movto_tit_acr.ind_trans_acr = 'implantacao'  AND
                      movto_tit_acr.cod_espec_docto = 'DP' 
                      NO-LOCK NO-ERROR.

           IF NOT AVAIL movto_tit_acr THEN DO:
              RUN pi-acompanhar IN  h-acomp (INPUT "Exportando t¡tulo 500 " + tt-titcon.numtit + "/" + tt-titcon.parcela).

              FIND FIRST tit_acr WHERE 
                         tit_acr.cod_empresa = '500' AND 
                         tit_acr.cdn_cliente = tt-titcon.cod-emit AND 
                         tit_acr.cod_tit_acr = tt-titcon.numtit AND 
                         tit_acr.cod_parcela = tt-titcon.parcela  NO-LOCK NO-ERROR.

              FIND FIRST movto_tit_acr OF tit_acr WHERE
                         movto_tit_acr.ind_trans_acr = 'implantacao'  AND
                         movto_tit_acr.cod_espec_docto = 'DP' 
                         NO-LOCK NO-ERROR.
           END.

           IF NOT AVAIL movto_tit_acr THEN DO:
              RUN pi-acompanhar IN  h-acomp (INPUT "Exportando t¡tulo 200 " + tt-titcon.numtit + "/" + tt-titcon.parcela).

              FIND FIRST tit_acr WHERE 
                         tit_acr.cod_empresa = '200' AND 
                         tit_acr.cdn_cliente = tt-titcon.cod-emit AND 
                         tit_acr.cod_tit_acr = tt-titcon.numtit AND 
                         tit_acr.cod_parcela = tt-titcon.parcela  NO-LOCK NO-ERROR.

              FIND FIRST movto_tit_acr OF tit_acr WHERE
                         movto_tit_acr.ind_trans_acr = 'implantacao'  AND
                         movto_tit_acr.cod_espec_docto = 'DP' 
                         NO-LOCK NO-ERROR.
           END.
            
           IF AVAIL movto_tit_acr THEN DO:    
              ASSIGN i-cont = i-cont + 1.
              RUN pi-exporta-tit_acr (INPUT movto_tit_acr.cod_empresa,
                                      INPUT movto_tit_acr.cod_estab,
                                      INPUT movto_tit_acr.cod_espec_docto,
                                      INPUT tit_acr.cod_ser_docto,
                                      INPUT tit_acr.cod_tit_acr,
                                      INPUT tit_acr.cod_parcela,
                                      INPUT 2).
           END.
       END.
       RUN pi-exporta (INPUT YES).
    END.
    ELSE DO:
        FIND FIRST tt-rethdr NO-ERROR.
    
        IF AVAIL tt-rethdr THEN DO:
           RUN esapi/um-espaco.p (INPUT tt-rethdr.texto,
                                  OUTPUT c-mens).
    
            ASSIGN c-mens = CAPS(c-mens).
    
            IF SUBSTRING(c-mens, 1, 20) = "REMESSA PARCIALMENTE" OR SUBSTRING(c-mens, 1, 12) = "TODA REMESSA" THEN DO:
               ASSIGN lg-houve-erros = YES.
    
               RUN pi-acompanhar IN  h-acomp (INPUT "Processando erros encontrados").
    
               FOR EACH tt-retdet WHERE
                        tt-retdet.erros <> "" NO-LOCK:
    
                    DO i-ind = 1 TO LENGTH(TRIM(tt-retdet.erros)) BY 3:
                        FIND FIRST tt-reterr
                            WHERE tt-reterr.numero = SUBSTRING(tt-retdet.erros, i-ind, 3)
                            NO-LOCK NO-ERROR.
    
                        IF AVAIL tt-reterr THEN DO:
                            CREATE tt-retimp.
                            ASSIGN tt-retimp.cnpj    = SUBSTRING(tt-retdet.cnpj, 1, 2) + "." +
                                                       SUBSTRING(tt-retdet.cnpj, 3, 3) + "." +
                                                       SUBSTRING(tt-retdet.cnpj, 6, 3) + "/" +
                                                       SUBSTRING(tt-retdet.cnpj, 9, 4) + "-" +
                                                       SUBSTRING(tt-retdet.cnpj, 13, 2)
                                   tt-retimp.numtit  = tt-retdet.numtit
                                   tt-retimp.valor   = DECIMAL(tt-retdet.valor) / 100
                                   tt-retimp.coderr  = SUBSTRING(tt-retdet.erros, i-ind, 3)
                                   tt-retimp.deserr  = IF AVAIL tt-reterr THEN tt-reterr.descri ELSE "*** ERRO " + SUBSTRING(tt-retdet.erros, i-ind, 3) + " NAO CADASTRADO ***"
                                   tt-retimp.parcela = tt-retdet.parcela.
    
                            IF tt-retdet.emissao <> "?" AND tt-retdet.emissao <> "" THEN
                               ASSIGN tt-retimp.emissao = DATE(SUBSTRING(tt-retdet.emissao, 7, 2) + "/" +
                                                               SUBSTRING(tt-retdet.emissao, 5, 2) + "/" +
                                                               SUBSTRING(tt-retdet.emissao, 1, 4)).
    
                            IF tt-retdet.vencto <> "?" AND tt-retdet.vencto <> "" THEN
                               ASSIGN tt-retimp.vencto  = DATE(SUBSTRING(tt-retdet.vencto, 7, 2) + "/" +
                                                               SUBSTRING(tt-retdet.vencto, 5, 2) + "/" +
                                                               SUBSTRING(tt-retdet.vencto, 1, 4)).
    
                            IF tt-retdet.pagto <> "?" AND tt-retdet.pagto <> "" THEN
                               ASSIGN tt-retimp.pagto   = DATE(SUBSTRING(tt-retdet.pagto, 7, 2) + "/" +
                                                               SUBSTRING(tt-retdet.pagto, 5, 2) + "/" +
                                                               SUBSTRING(tt-retdet.pagto, 1, 4)).
                        END.
                    END.
               END.
    
               FOR EACH tt-retimp
                        BREAK BY tt-retimp.numtit:
    
                   IF FIRST-OF(tt-retimp.numtit) THEN DO:
                      DISPLAY tt-retimp.numtit
                              tt-retimp.parcela
                              tt-retimp.cnpj
                              tt-retimp.emissao
                              tt-retimp.valor
                              tt-retimp.vencto
                              tt-retimp.pagto
                              WITH FRAME f-detalhe.
                  END.
                  DISPLAY tt-retimp.deserr
                          WITH FRAME f-detalhe.
                  DOWN WITH FRAME f-detalhe.
               END.
            END.
        END.
    END.
END PROCEDURE.

PROCEDURE pi-acento.
    DEFINE INPUT  PARAMETER p-texto  AS CHARACTER.
    DEFINE OUTPUT PARAMETER p-result AS CHARACTER.

    DEFINE VARIABLE i-pos AS INTEGER    NO-UNDO.

    DEFINE VARIABLE c-char AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE c-tab1 AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE c-tab2 AS CHARACTER  NO-UNDO.

    ASSIGN c-tab1 = " …Æƒ‚ˆ¡¢ä“£¤‡µ·Ç¶ÒÖàåâéš¥€"
           c-tab2 = "aaaaeeiooouuncAAAAEEIOOOUUNC".

    ASSIGN p-result = "".

    DO i-pos = 1 TO LENGTH(p-texto):
        IF INDEX(c-tab1, SUBSTRING(p-texto, i-pos, 1)) > 0 THEN
            ASSIGN c-char = SUBSTRING(c-tab2, INDEX(c-tab1, SUBSTRING(p-texto, i-pos, 1)), 1).
        ELSE
            ASSIGN c-char = SUBSTRING(p-texto, i-pos, 1).

        ASSIGN p-result = p-result + c-char.
    END.
END.

PROCEDURE pi-tirachar.
    DEFINE INPUT  PARAMETER p-texto  AS CHARACTER.
    DEFINE INPUT  PARAMETER p-substr AS CHARACTER.
    DEFINE OUTPUT PARAMETER p-result AS CHARACTER.

    ASSIGN p-result = p-texto.

    IF INDEX(p-result, p-substr) > 0 THEN DO:
        DO WHILE INDEX(p-result, p-substr) > 0:
            ASSIGN p-result = SUBSTRING(p-result, 1, INDEX(p-result, p-substr) - 1) +
                              SUBSTRING(p-result, INDEX(p-result, p-substr) + LENGTH(p-substr)).
        END.
    END.
END.
    
PROCEDURE pi-exporta-tit_acr:
    DEFINE INPUT PARAMETER p-ep-codigo   LIKE tit_acr.cod_empresa.
    DEFINE INPUT PARAMETER p-cod-estabel LIKE tit_acr.cod_estab.
    DEFINE INPUT PARAMETER p-cod-esp     LIKE tit_acr.cod_espec_docto.
    DEFINE INPUT PARAMETER p-serie       LIKE tit_acr.cod_ser_docto.
    DEFINE INPUT PARAMETER p-nr-docto    LIKE tit_acr.cod_tit_acr.
    DEFINE INPUT PARAMETER p-parcela     LIKE tit_acr.cod_parcela.
    DEFINE INPUT PARAMETER p-tipo        AS INTEGER.

    FIND FIRST emitente WHERE 
               emitente.cod-emitente = tit_acr.cdn_cliente
               NO-LOCK NO-ERROR.

    IF NOT AVAIL emitente THEN LEAVE.

    RUN pi-acompanhar IN  h-acomp (INPUT "Exportando t¡tulo " + p-nr-docto + "/" + p-parcela).

    IF p-tipo = 1 THEN DO: /* EXPORTA€ÇO */
       IF emitente.natureza <> 2 THEN LEAVE.
       IF emitente.cgc = c-cnpj THEN LEAVE.
    END.

    FIND FIRST tt-cliente WHERE 
               tt-cliente.cod-emitente = tit_acr.cdn_cliente NO-ERROR.

    IF NOT AVAIL tt-cliente THEN DO:
       CREATE tt-cliente.
       ASSIGN tt-cliente.cod-emitente = tit_acr.cdn_cliente.
    END.


    IF p-tipo = 1 THEN DO:
        FIND FIRST ima-titulo-ext
            WHERE ima-titulo-ext.ep-codigo   = int(movto_tit_acr.cod_empresa)    AND
                  ima-titulo-ext.cod-estabel = movto_tit_acr.cod_estab           AND
                  ima-titulo-ext.cod-esp     = movto_tit_acr.cod_espec_docto     AND
                  ima-titulo-ext.serie       = tit_acr.cod_ser_docto             AND
                  ima-titulo-ext.nr-docto    = tit_acr.cod_tit_acr               AND
                  ima-titulo-ext.parcela     = tit_acr.cod_parcela
            NO-ERROR.

        IF AVAIL ima-titulo-ext AND
           movto_tit_acr.ind_trans_acr = "Implantacao" THEN NEXT.

        IF NOT AVAIL ima-titulo-ext THEN DO.
           CREATE ima-titulo-ext.
           ASSIGN ima-titulo-ext.ep-codigo   = int(movto_tit_acr.cod_empresa)
                  ima-titulo-ext.cod-estabel = movto_tit_acr.cod_estab
                  ima-titulo-ext.cod-esp     = movto_tit_acr.cod_espec_docto
                  ima-titulo-ext.serie       = tit_acr.cod_ser_docto
                  ima-titulo-ext.nr-docto    = tit_acr.cod_tit_acr  
                  ima-titulo-ext.parcela     = tit_acr.cod_parcela.
        END.
        ASSIGN ima-titulo-ext.dt-exporta  = TODAY
               ima-titulo-ext.ordem       = i-ordem
               ima-titulo-ext.period      = c-tipmov
               ima-titulo-ext.sit-retorno = 0.
    END.
    
    FIND FIRST ima-titulo-ext WHERE 
               ima-titulo-ext.ep-codigo   = int(p-ep-codigo)   AND
               ima-titulo-ext.cod-estabel = p-cod-estabel AND
               ima-titulo-ext.cod-esp     = p-cod-esp     AND
               ima-titulo-ext.serie       = p-serie       AND
               ima-titulo-ext.nr-docto    = p-nr-docto    AND
               ima-titulo-ext.parcela     = p-parcela NO-ERROR.

    FIND FIRST tt-remdet WHERE 
               tt-remdet.cnpj   = emitente.cgc AND
               tt-remdet.tit_acr = STRING(INTEGER(tit_acr.cod_tit_acr), "9999999999") + tit_acr.cod_parcela
               NO-ERROR.

    IF NOT AVAIL tt-remdet THEN DO:
        CREATE tt-remdet.
        ASSIGN tt-remdet.cnpj    = emitente.cgc
               tt-remdet.numtit  = ""
               tt-remdet.reserva = "#D" + STRING(INTEGER(tit_acr.cod_tit_acr), "9999999999") +
                                   "/" + tit_acr.cod_parcela
               tt-remdet.emissao = SUBSTRING(STRING(tit_acr.dat_emis_docto, "99/99/9999"), 7, 4) +
                                   SUBSTRING(STRING(tit_acr.dat_emis_docto, "99/99/9999"), 4, 2) +
                                   SUBSTRING(STRING(tit_acr.dat_emis_docto, "99/99/9999"), 1, 2)
               tt-remdet.vencto  = SUBSTRING(STRING(tit_acr.dat_vencto_tit_acr, "99/99/9999"), 7, 4) +
                                   SUBSTRING(STRING(tit_acr.dat_vencto_tit_acr, "99/99/9999"), 4, 2) +
                                   SUBSTRING(STRING(tit_acr.dat_vencto_tit_acr, "99/99/9999"), 1, 2)
               tt-remdet.tit_acr  = STRING(INTEGER(tit_acr.cod_tit_acr), "9999999999") + tit_acr.cod_parcela.

        IF AVAIL ima-titulo-ext THEN DO:
            IF ima-titulo-ext.sit-retorno = 3 THEN
                ASSIGN tt-remdet.valor = "9999999999999".
            ELSE
                ASSIGN tt-remdet.valor   = STRING((tit_acr.val_origin_tit_acr * 100), "9999999999999").
        END.
        ELSE
            ASSIGN tt-remdet.valor   = STRING((tit_acr.val_origin_tit_acr * 100), "9999999999999").
    END.

    IF AVAIL ima-titulo-ext THEN
        ASSIGN ima-titulo-ext.sit-retorno = 0.

    IF tit_acr.dat_liquidac_tit_acr <= TODAY AND
       (tit_acr.dat_liquidac_tit_acr <= dt-datfim OR lg-concilia) THEN DO.

       IF tt-remdet.pagto = "" THEN DO: 
           ASSIGN tt-remdet.pagto = SUBSTRING(STRING(tit_acr.dat_liquidac_tit_acr, "99/99/9999"), 7, 4) +
                                    SUBSTRING(STRING(tit_acr.dat_liquidac_tit_acr, "99/99/9999"), 4, 2) +
                                    SUBSTRING(STRING(tit_acr.dat_liquidac_tit_acr, "99/99/9999"), 1, 2).
       END.
    END.
END.
