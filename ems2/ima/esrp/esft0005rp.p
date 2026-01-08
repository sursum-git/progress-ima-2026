/******************************************************************************
** PROGRAMA: ESFT0005RP.P                                                    **
** DATA    : SETEMBRO/2015                                                   **
** AUTOR   : Toninho                                                         **
** OBJETIVO: Exportaá∆o para o Serasa Experian Positivo                      **
******************************************************************************/
/* Programa de controle de versao e seguranªa do Datasul EMS */

{include/i-prgvrs.i ESFT0005 2.04.00.001}

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

DEF BUFFER empresa FOR mgcad.empresa.

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

FIND FIRST tt-param.

DEFINE VARIABLE i-qtd-lin-lote AS INTEGER NO-UNDO.
DEFINE VARIABLE c-endereco     AS CHAR.
DEFINE VARIABLE c-aux          AS CHAR.
DEFINE VARIABLE i-aux          AS INTEGER.
DEFINE VARIABLE c-num          AS CHAR.
DEFINE VARIABLE c-compl        AS CHAR.
DEFINE VARIABLE h-acomp     AS HANDLE     NO-UNDO.
DEFINE STREAM s-arq.

{include/i-rpvar.i}   

FIND empresa WHERE
     empresa.ep-codigo = '1' NO-LOCK NO-ERROR.
IF NOT AVAIL empresa THEN
   RETURN "ADM-ERROR":U.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.

RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).
RUN pi-exporta.

RUN pi-finalizar in h-acomp.


/*************************** PROCEDURES *****************************/
PROCEDURE pi-exporta.

    RUN pi-acompanhar IN  h-acomp (INPUT "Buscando Titulo").

    FIND im-param WHERE
         im-param.cod-param = "ULT_LOTE_SERASA" SHARE-LOCK NO-ERROR.

    IF NOT AVAIL im-param THEN DO.
       MESSAGE "Parametro do Ultimo Lote Enviado ao Serasa n∆o Encontrado," SKIP
               "Verifique o programa esim001.w (PARAM = ULT_LOTE_SERASA) "
               VIEW-AS ALERT-BOX.
       RETURN.
    END.

    ASSIGN im-param.val-param = STRING(INTEGER(im-param.val-param) + 1,"9999").

    OUTPUT STREAM s-arq TO VALUE(tt-param.c-nomarq).
        /* Header do Lote */
        PUT STREAM s-arq                                      
            "L000"                                          FORMAT "x(04)"
            STRING(INTEGER(im-param.val-param) + 1,"9999")  FORMAT "x(04)"
            STRING(YEAR(TODAY),"9999")                      FORMAT "x(04)"
            STRING(MONTH(TODAY),"99")                       FORMAT "x(02)"
            STRING(DAY(TODAY),"99")                         FORMAT "x(02)"
            REPLACE(STRING(TIME,"HH:MM:SS"),":","")         FORMAT "x(06)"
            TRIM(empresa.cgc)                               FORMAT "x(15)"
            "81915140"                                      FORMAT "x(08)"
            "N"                                             FORMAT "x"     
            "T"                                             FORMAT "x"     
            "C"                                             FORMAT "x(06)" 
            "01"                                            FORMAT "x(02)"   /*Modalidade*/
            FILL(" ",43)                                    FORMAT "x(43)"
            "S"                                             FORMAT "x"     
            FILL(" ",16)                                    FORMAT "x(16)"
            "0000001"                                       FORMAT "x(07)" 
            SKIP.  
    

        ASSIGN i-qtd-lin-lote = 2.  /* linha 1 Ç do Lote */
    
        FOR EACH tit_acr WHERE 
                 tit_acr.dat_vencto_tit_acr < TODAY - 7 AND
                 tit_acr.cod_espec_docto    = "DP" AND
                 tit_acr.val_sdo_tit_acr    <> 0 NO-LOCK 
                 BREAK BY tit_acr.cdn_cliente.
    
            IF FIRST-OF(tit_acr.cdn_cliente) THEN DO.
               FIND emitente WHERE
                    emitente.cod-emit = tit_acr.cdn_cliente NO-LOCK NO-ERROR.
    
    
               IF emitente.natureza = 1 THEN DO.  /* Pessao Fisica */
                  /* Header do Bloco */
                  ASSIGN i-qtd-lin-lote = i-qtd-lin-lote + 1.
                  PUT STREAM s-arq                                      
                      "P000"                                          FORMAT "x(04)"
                      "F"                                             FORMAT "x"
                      emitente.cgc                                    FORMAT "x(15)"
                      "0000001"                                       FORMAT "x(07)" 
                      "C"                                             FORMAT "x(06)"
                      "01"                                            FORMAT "x(02)"   /*Modalidade*/
                      FILL(" ",12)                                    FORMAT "x(12)"
                      FILL(" ",25)                                    FORMAT "x(25)"
                      "000000000"                                     FORMAT "x(09)"
                      "S"                                             FORMAT "x"    
                      FILL(" ",33)                                    FORMAT "x(33)"
                      STRING(i-qtd-lin-lote, "9999999")               FORMAT "x(07)"
                      SKIP.

                  /* Dados Pessoa Fisica */
                  ASSIGN i-qtd-lin-lote = i-qtd-lin-lote + 1.
                  PUT STREAM s-arq                                      
                      "B001"                                       FORMAT "x(04)"
                      emitente.nome-emit                           FORMAT "x(46)"
                      emitente.cgc                                 FORMAT "x(11)"
                      FILL(" ",55)                                 FORMAT "x(55)"
                      STRING(i-qtd-lin-lote, "9999999")            FORMAT "x(07)"
                      SKIP.

                  /* Telefones Pessao Fisica */
                  ASSIGN i-qtd-lin-lote = i-qtd-lin-lote + 1.
                  PUT STREAM s-arq                                      
                      "B003"                                       FORMAT "x(04)"
                      FILL(" ",64)                                 FORMAT "x(64)"
                      FILL("0",3)                                  FORMAT "x(03)"
                      FILL("0",9)                                  FORMAT "x(09)"
                      INTEGER(emitente.telefone[1])                FORMAT "999"
                      INTEGER(emitente.telefone[1])                FORMAT "999999999"
                      INTEGER(emitente.ramal[1])                   FORMAT "9999"
                      FILL("0",9)                                  FORMAT "x(09)"
                      FILL("0",3)                                  FORMAT "x(03)"
                      STRING(i-qtd-lin-lote, "9999999")            FORMAT "x(07)"
                      SKIP.

                  /* Endereáos Pessoa Fisica */
                  ASSIGN c-endereco = ""
                         c-num = "0"
                         c-compl = "".

                  IF emitente.endereco MATCHES "*KM*" THEN
                     ASSIGN c-endereco = emitente.endereco.
                  ELSE IF NUM-ENTRIES(emitente.endereco) >= 2 THEN DO.
                     ASSIGN c-endereco = ENTRY(1,emitente.endereco)
                            c-aux = TRIM(ENTRY(2,emitente.endereco)). 

                     IF NUM-ENTRIES(c-aux," ") >= 2 THEN DO.
                        ASSIGN c-num = ENTRY(1,c-aux," ").

                        ASSIGN i-aux = INTEGER(c-num) NO-ERROR.
                        IF i-aux = 0 THEN
                           ASSIGN c-num = "0"
                                  c-compl = c-aux.
                        ELSE
                           ASSIGN c-compl = REPLACE(c-aux,c-num,"").
                     END.
                     ELSE DO.
                        ASSIGN c-num = c-aux.

                        ASSIGN i-aux = INTEGER(c-num) NO-ERROR.
                        IF i-aux = 0 THEN
                           ASSIGN c-num = "0"
                                  c-compl = c-aux.
                     END.
                  END.
                  ELSE DO.
                     ASSIGN c-num = ENTRY(NUM-ENTRIES(emitente.endereco," "),emitente.endereco," ").

                     ASSIGN i-aux = INTEGER(c-num) NO-ERROR.
                     IF i-aux = 0 THEN
                        ASSIGN c-endereco = emitente.endereco
                               c-num = '0'.
                     ELSE
                        ASSIGN c-endereco = REPLACE(emitente.endereco,c-num,"").
                  END.

                  ASSIGN i-qtd-lin-lote = i-qtd-lin-lote + 1.
                  PUT STREAM s-arq                                      
                      "B004"                                       FORMAT "x(04)"
                      c-endereco                                   FORMAT "x(30)"
                      c-num                                        FORMAT "x(05)"
                      c-compl                                      FORMAT "x(10)"
                      emitente.bairro                              FORMAT "x(20)"
                      emitente.cidade                              FORMAT "x(25)"
                      emitente.estado                              FORMAT "x(02)"
                      STRING(emitente.cep,"99999999")              FORMAT "x(08)"
                      STRING(YEAR(emitente.data-implant),"9999")   FORMAT "x(04)"
                      STRING(MONTH(emitente.data-implant),"99")    FORMAT "x(02)"
                      FILL(" ",5)                                  FORMAT "x(05)"
                      STRING(i-qtd-lin-lote, "9999999")            FORMAT "x(07)"
                      SKIP.
               END.
               ELSE DO.  /* Pessoa juridica */
                  /* Header do Bloco */
                  ASSIGN i-qtd-lin-lote = i-qtd-lin-lote + 1.
                  PUT STREAM s-arq                                      
                      "P000"                                          FORMAT "x(04)"
                      "J"                                             FORMAT "x"
                      emitente.cgc                                    FORMAT "x(15)"
                      "0000001"                                       FORMAT "x(07)" 
                      "C"                                             FORMAT "x(06)"
                      "01"                                            FORMAT "x(02)"   /*Modalidade*/
                      FILL(" ",12)                                    FORMAT "x(12)"
                      FILL(" ",25)                                    FORMAT "x(25)"
                      "000000000"                                     FORMAT "x(09)"
                      "S"                                             FORMAT "x"    
                      FILL(" ",33)                                    FORMAT "x(33)"
                      STRING(i-qtd-lin-lote, "9999999")               FORMAT "x(07)"
                      SKIP.

               END.

               /* Trailer do Bloco */
               ASSIGN i-qtd-lin-lote = i-qtd-lin-lote + 1.
               PUT STREAM s-arq                                      
                   "T999"                                          FORMAT "x(04)"
                   FILL(" ",111)                                   FORMAT "x(111)"
                   STRING(i-qtd-lin-lote, "9999999")               FORMAT "x(07)"
                   SKIP.
            END.
        END.

        /* Trailer do Lote */
        ASSIGN i-qtd-lin-lote = i-qtd-lin-lote + 1.
        PUT STREAM s-arq                                      
            "L999"                                          FORMAT "x(04)"
            FILL(" ",111)                                   FORMAT "x(111)"
            STRING(i-qtd-lin-lote, "9999999")               FORMAT "x(07)"
            SKIP.

    OUTPUT STREAM s-arq CLOSE.
END PROCEDURE.

PROCEDURE pi-acento.
    DEFINE INPUT  PARAMETER p-texto  AS CHARACTER.
    DEFINE OUTPUT PARAMETER p-result AS CHARACTER.

    DEFINE VARIABLE i-pos AS INTEGER    NO-UNDO.

    DEFINE VARIABLE c-char AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE c-tab1 AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE c-tab2 AS CHARACTER  NO-UNDO.

    ASSIGN c-tab1 = "†Ö∆ÉÇà°¢‰ì£Å§áµ∑«∂ê“÷‡Â‚Èö•Ä"
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
