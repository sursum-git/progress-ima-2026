/* Programa: ESSP0112RP.P
** Sistema.: Sistemas Internos.
** Objetivo: Gerar arquivos de Detalhamento de Liga‡äes Telef“nicas e
**           enviar e-mail aos respons veis pelos ramais. 
** Autor...: Gilvando de Souza Araujo - Abril/2006
** Obs.....: Especifico da TEAR TÒXTIL INDéSTRIA E COMRCIO LTDA.
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESSP0112RP 2.04.00.000}

define temp-table tt-param no-undo
       field destino          as integer
       field arquivo          as char format "x(35)"
       field usuario          as char format "x(12)"
       field data-exec        as date
       field hora-exec        as integer
       field classifica       as integer
       field desc-classifica  as char format "x(40)"
       FIELD ini-responsavel  AS CHAR FORMAT "x(20)"
       FIELD fin-responsavel  AS CHAR FORMAT "x(20)"
       FIELD ini-dt-chamada   AS DATE FORMAT 99/99/9999
       FIELD fin-dt-chamada   AS DATE FORMAT 99/99/9999
       FIELD ini-ramal        AS INTEGER
       FIELD fin-ramal        AS INTEGER
       FIELD arq-cham-loc     AS CHAR FORMAT "x(55)"
       FIELD arq-cham-nloc    AS CHAR FORMAT "x(55)"
       FIELD arq-ramais       AS CHAR FORMAT "x(55)"
       FIELD arq-tmp-email    AS CHAR FORMAT "x(55)"
       FIELD vlr-fat-loc      AS DEC
       FIELD vlr-fat-nloc     AS DEC
       FIELD gerar-resumo     AS LOG FORMAT "Sim/NÆo"
       FIELD arq-resumo       AS CHAR FORMAT "x(55)"
       FIELD enviar-e-mail    AS LOG FORMAT "Sim/NÆo"
       FIELD enviar-resp      AS LOG FORMAT "Sim/NÆo"
       FIELD enviar-telefon   AS LOG FORMAT "Sim/NÆo"
       FIELD enviar-info      AS LOG FORMAT "Sim/NÆo"
       FIELD email-telefon    AS CHAR FORMAT "x(40)"
       FIELD email-info       AS CHAR FORMAT "x(40)"
       FIELD subject-e-mail   AS CHAR FORMAT "x(40)"
       FIELD texto-e-mail     AS CHAR FORMAT "x(2000)"
       FIELD imp-param        AS LOG.

DEFINE TEMP-TABLE tt-raw-digita
       FIELD raw-digita as raw.

DEF INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEF INPUT PARAMETER TABLE FOR tt-raw-digita.

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* defini‡Æo de vari veis  */
DEFINE VAR h-acomp AS HANDLE NO-UNDO.

DEF TEMP-TABLE tt-ramais
    FIELD ramal         AS INT
    FIELD setor         AS CHAR
    FIELD complemento   AS CHAR
    FIELD responsavel   AS CHAR
    FIELD e-mail        AS CHAR
    INDEX ch-ramais ramal.

DEF TEMP-TABLE tt-locais
    FIELD data-chamada  AS DATE FORMAT 99/99/9999
    FIELD hora-chamada  AS CHAR
    FIELD tel-origem    AS CHAR
    FIELD local-origem  AS CHAR
    FIELD duracao       AS DEC 
    FIELD tipo          AS CHAR
    FIELD degrau        AS CHAR
    FIELD grupo         AS CHAR
    FIELD tel-destino   AS CHAR
    FIELD valor         AS DEC
    FIELD vlr-com-imp   AS DEC
    FIELD vlr-rateio    AS DEC.

DEF TEMP-TABLE tt-nlocais
    FIELD cps           AS CHAR
    FIELD conta         AS CHAR
    FIELD subconta      AS CHAR
    FIELD cnpj          AS CHAR
    FIELD razao-soc     AS CHAR
    FIELD sequencia     AS CHAR
    FIELD servico       AS CHAR
    FIELD tipo          AS CHAR
    FIELD data-chamada  AS DATE FORMAT 99/99/9999 
    FIELD hora-chamada  AS CHAR
    FIELD tel-origem    AS CHAR
    FIELD local-origem  AS CHAR
    FIELD tel-destino   AS CHAR
    FIELD local-dest    AS CHAR
    FIELD pais          AS CHAR
    FIELD unidade       AS CHAR
    FIELD duracao       AS DEC 
    FIELD vlr-sem-imp   AS DEC
    FIELD vlr-com-imp   AS DEC
    FIELD vlr-rateio    AS DEC.

DEF TEMP-TABLE tt-resumo
    FIELD data-chamada  AS DATE FORMAT 99/99/9999
    FIELD hora-chamada  AS CHAR
    FIELD tel-origem    AS CHAR
    FIELD tel-destino   AS CHAR
    FIELD duracao       AS DEC
    FIELD valor         AS DEC
    INDEX ch-resumo tel-origem
                    data-chamada
                    hora-chamada
                    tel-destino.

DEF VAR i-lin-excel AS INT INIT 1.
DEF VAR de-tot-aux  AS DEC.
DEF VAR c-destinatario AS CHAR.

DEF STREAM str-saida.
DEF STREAM str-entrada.

FORM
    tt-param.ini-responsavel LABEL "Responsavel...." AT  1
    "a"                                              AT 39
    tt-param.fin-responsavel NO-LABELS
    tt-param.ini-dt-chamada  LABEL "Data da Chamada" AT  1
    "a"                                              AT 39
    tt-param.fin-dt-chamada  NO-LABELS
    tt-param.ini-ramal       LABEL "Ramal.........." AT  1
    "a"                                              AT 39
    tt-param.fin-ramal       NO-LABELS
    tt-resumo.duracao        LABEL "Dura‡Æo Total.." AT  1
    tt-resumo.valor          LABEL "Valor Total...." AT  1  
    with no-box side-labels width 132 STREAM-IO frame f-parlis.


/* include padrÆo para output de relat¢rios */
{include/i-rpout.i &STREAM="stream str-rp"}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i &STREAM="str-rp"}

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i ESPECIFICO * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Detalhamento_de_Liga‡äes_Telef“nicas * r}
assign c-titulo-relat = trim(return-value).

view stream str-rp frame f-cabec.
view stream str-rp frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

/* -------------- Importacao de Ramais --------------* */
RUN pi-acompanhar IN h-acomp (INPUT "Importando Ramais...").

INPUT STREAM str-entrada FROM VALUE(tt-param.arq-ramais) CONVERT SOURCE "ibm850" NO-ECHO.
/*SET STREAM str-entrada ^.*/
REPEAT:
   CREATE tt-ramais.
   IMPORT STREAM str-entrada DELIMITER ";" tt-ramais.
   RUN pi-acompanhar IN h-acomp (INPUT "Importando Ramais " + string(tt-ramais.ramal)).
END.
INPUT STREAM str-entrada CLOSE.

/* ---------- Importacao de Ligacoes Locais ----------* */
RUN pi-acompanhar IN h-acomp (INPUT "Importando Liga‡äes Locais...").

INPUT STREAM str-entrada FROM VALUE(tt-param.arq-cham-loc) CONVERT SOURCE "ibm850".
/*SET STREAM str-entrada ^.*/
REPEAT:
   CREATE tt-locais.
   IMPORT STREAM str-entrada DELIMITER ";" tt-locais.
END.
INPUT STREAM str-entrada CLOSE.

/* Calcula impostos e rateia conta */
ASSIGN de-tot-aux = 0.
FOR EACH tt-locais:
    ASSIGN tt-locais.vlr-com-imp = TRUNCATE(TRUNCATE(tt-locais.valor * 100 ,0) * 0.01 / 0.7135 * 100 ,0) * 0.01
           de-tot-aux            = de-tot-aux + tt-locais.vlr-com-imp.
END.
FOR EACH tt-locais:
    ASSIGN tt-locais.vlr-rateio = (tt-param.vlr-fat-loc - de-tot-aux) / de-tot-aux * tt-locais.vlr-com-imp.
END.

/* ------- Importacao de Ligacoes NÆo Locais ---------* */
RUN pi-acompanhar IN h-acomp (INPUT "Importando nÆo Locais...").

INPUT STREAM str-entrada FROM VALUE(tt-param.arq-cham-nloc) CONVERT SOURCE "ibm850".
/*SET STREAM str-entrada ^.*/
REPEAT:
   CREATE tt-nlocais.
   IMPORT STREAM str-entrada DELIMITER ";" tt-nlocais.
END.
INPUT STREAM str-entrada CLOSE.

/* Rateia conta */
RUN pi-acompanhar IN h-acomp (INPUT "Rateando Conta...").

ASSIGN de-tot-aux = 0.
FOR EACH tt-nlocais:
    ASSIGN de-tot-aux = de-tot-aux + tt-nlocais.vlr-com-imp.
END.
FOR EACH tt-nlocais:
    ASSIGN tt-nlocais.vlr-rateio = (tt-param.vlr-fat-nloc - de-tot-aux) / de-tot-aux * tt-nlocais.vlr-com-imp.
END.

FOR EACH tt-locais:
    IF tt-locais.data-chamada = ? THEN NEXT.
    FIND tt-resumo WHERE tt-resumo.tel-origem   = tt-locais.tel-origem
                     AND tt-resumo.data-chamada = tt-locais.data-chamada
                     AND tt-resumo.hora-chamada = tt-locais.hora-chamada
                     AND tt-resumo.tel-destino  = tt-locais.tel-destino
                   NO-LOCK NO-ERROR.
    IF NOT AVAIL tt-resumo THEN DO:
       CREATE tt-resumo.
       ASSIGN tt-resumo.tel-origem   = tt-locais.tel-origem
              tt-resumo.data-chamada = tt-locais.data-chamada
              tt-resumo.hora-chamada = tt-locais.hora-chamada
              tt-resumo.tel-destino  = tt-locais.tel-destino. 
    END.
    ASSIGN tt-resumo.duracao = tt-locais.duracao
           tt-resumo.valor   = tt-resumo.valor + tt-locais.vlr-com-imp + tt-locais.vlr-rateio.
END.

FOR EACH tt-nlocais:
    IF tt-nlocais.cps = "" THEN NEXT.
    FIND tt-resumo WHERE tt-resumo.tel-origem   = tt-nlocais.tel-origem
                     AND tt-resumo.data-chamada = tt-nlocais.data-chamada
                     AND tt-resumo.hora-chamada = tt-nlocais.hora-chamada
                     AND tt-resumo.tel-destino  = tt-nlocais.tel-destino
                   NO-LOCK NO-ERROR.
    IF NOT AVAIL tt-resumo THEN DO:
       CREATE tt-resumo.
       ASSIGN tt-resumo.tel-origem   = tt-nlocais.tel-origem
              tt-resumo.data-chamada = tt-nlocais.data-chamada
              tt-resumo.hora-chamada = tt-nlocais.hora-chamada
              tt-resumo.tel-destino  = tt-nlocais.tel-destino. 
    END.
    ASSIGN tt-resumo.duracao = tt-nlocais.duracao
           tt-resumo.valor   = tt-resumo.valor + tt-nlocais.vlr-com-imp + tt-nlocais.vlr-rateio.
END.

FOR EACH tt-resumo:
    FIND tt-ramais WHERE tt-ramais.ramal = INT(SUBSTR(tt-resumo.tel-origem,8,3)) NO-LOCK NO-ERROR.
    IF NOT AVAIL tt-ramais THEN DO:
       MESSAGE "Ramal: " INT(SUBSTR(tt-resumo.tel-origem,8,3)) "NÆo cadastrado" VIEW-AS ALERT-BOX.
    END.
    ACCUMULATE tt-resumo.duracao(TOTAL).
    ACCUMULATE tt-resumo.valor(TOTAL).
END.

IF tt-param.gerar-resumo THEN DO:
   OUTPUT STREAM str-saida TO VALUE(tt-param.arq-resumo) CONVERT SOURCE "ibm850". 
   PUT STREAM str-saida
              "Responsavel;"
              "Data;"
              "Hora;"
              "Tel-Orig;"  
              "Tel-Dest;" 
              "Duracao;"     
              "Valor"
              SKIP.
END.

RUN pi-acompanhar IN h-acomp (INPUT "Gerando Planilha...").
FOR EACH tt-resumo NO-LOCK, 
    EACH tt-ramais WHERE tt-ramais.ramal = INT(SUBSTR(tt-resumo.tel-origem,8,3))
                     AND tt-ramais.ramal        >= tt-param.ini-ramal
                     AND tt-ramais.ramal        <= tt-param.fin-ramal
                     AND tt-ramais.responsavel  >= tt-param.ini-responsavel
                     AND tt-ramais.responsavel  <= tt-param.fin-responsavel
                     AND tt-resumo.data-chamada >= tt-param.ini-dt-chamada
                     AND tt-resumo.data-chamada <= tt-param.fin-dt-chamada
                   NO-LOCK
                   BREAK BY tt-ramais.responsavel
                         BY tt-ramais.ramal
                         BY tt-resumo.data-chamada
                         BY tt-resumo.hora-chamada:
                         
    ACCUMULATE tt-resumo.duracao (TOTAL BY tt-ramais.responsavel).
    ACCUMULATE tt-resumo.valor (TOTAL BY tt-ramais.responsavel).
    ACCUMULATE tt-resumo.duracao (TOTAL BY tt-ramais.ramal).
    ACCUMULATE tt-resumo.valor (TOTAL BY tt-ramais.ramal).
    
    IF tt-param.enviar-e-mail THEN DO:
       IF FIRST-OF(tt-ramais.responsavel) THEN DO:
          {utp/utapi003.i}
          OS-DELETE VALUE(tt-param.arq-tmp-email).
          CREATE tt-configuracao.
          ASSIGN tt-configuracao.versao-integracao   = 2
                 tt-configuracao.arquivo-num         = 1
                 tt-configuracao.arquivo             = tt-param.arq-tmp-email
                 tt-configuracao.total-planilha      = 1
                 tt-configuracao.exibir-construcao   = NO 
                 tt-configuracao.abrir-excel-termino = NO.
       
          CREATE tt-planilha.
          ASSIGN tt-planilha.arquivo-num       = 1
                 tt-planilha.planilha-num      = 1
                 tt-planilha.planilha-nome     = "Detalhamento"
                 tt-planilha.linhas-grade      = NO
                 tt-planilha.largura-coluna    = 14
                 tt-planilha.formatar-planilha = YES.
       
          CREATE tt-dados.
          ASSIGN tt-dados.arquivo-num   = 1
                 tt-dados.planilha-num  = 1
                 tt-dados.celula-coluna = 1
                 tt-dados.celula-linha  = 1
                 tt-dados.celula-valor  = "TEAR TÒXTIL INDéSTRIA E COMRCIO LTDA."
                 tt-dados.celula-fonte-tamanho = 20
                 tt-dados.celula-fonte-negrito = YES
                 tt-dados.celula-fonte-cor     = 3.
          CREATE tt-dados.
          ASSIGN tt-dados.arquivo-num   = 1
                 tt-dados.planilha-num  = 1
                 tt-dados.celula-coluna = 1
                 tt-dados.celula-linha  = 3
                 tt-dados.celula-valor  = "DETALHAMENTO DE LIGA€åES TELEFâNICAS DOS SEUS RAMAIS"
                 tt-dados.celula-fonte-tamanho = 14
                 tt-dados.celula-fonte-negrito = YES.
          CREATE tt-dados.
          ASSIGN tt-dados.arquivo-num   = 1
                 tt-dados.planilha-num  = 1
                 tt-dados.celula-coluna = 1
                 tt-dados.celula-linha  = 5
                 tt-dados.celula-valor  = "RESPONSµVEL: " + tt-ramais.responsavel
                 tt-dados.celula-fonte-tamanho = 12
                 tt-dados.celula-fonte-negrito = YES.
          ASSIGN i-lin-excel = 7.
       
          CREATE tt-dados.
          ASSIGN tt-dados.arquivo-num   = 1
                 tt-dados.planilha-num  = 1
                 tt-dados.celula-coluna = 1
                 tt-dados.celula-cor-interior = 10
                 tt-dados.celula-linha  = i-lin-excel
                 tt-dados.celula-valor  = "Data"
                 tt-dados.celula-fonte-negrito = YES
                 tt-dados.celula-fonte-cor = 2.
          CREATE tt-dados.
          ASSIGN tt-dados.arquivo-num   = 1
                 tt-dados.planilha-num  = 1
                 tt-dados.celula-coluna = 2
                 tt-dados.celula-cor-interior = 10
                 tt-dados.celula-linha  = i-lin-excel
                 tt-dados.celula-valor  = "Hora"
                 tt-dados.celula-fonte-negrito = YES
                 tt-dados.celula-fonte-cor = 2.
          CREATE tt-dados.
          ASSIGN tt-dados.arquivo-num    = 1
                 tt-dados.planilha-num   = 1
                 tt-dados.celula-coluna  = 3
                 tt-dados.celula-cor-interior = 10
                 tt-dados.celula-linha   = i-lin-excel
                 tt-dados.celula-valor   = "Tel-Origem"
                 tt-dados.celula-fonte-negrito = YES
                 tt-dados.celula-fonte-cor = 2.
          CREATE tt-dados.
          ASSIGN tt-dados.arquivo-num   = 1
                 tt-dados.planilha-num  = 1
                 tt-dados.celula-coluna = 4
                 tt-dados.celula-cor-interior = 10
                 tt-dados.celula-linha  = i-lin-excel
                 tt-dados.celula-valor  = "Tel-Destino"
                 tt-dados.celula-fonte-negrito = YES
                 tt-dados.celula-fonte-cor = 2.
          CREATE tt-dados.
          ASSIGN tt-dados.arquivo-num   = 1
                 tt-dados.planilha-num  = 1
                 tt-dados.celula-coluna = 5
                 tt-dados.celula-cor-interior = 10
                 tt-dados.celula-linha  = i-lin-excel
                 tt-dados.celula-alinhamento-horizontal = 5
                 tt-dados.celula-valor  = "Dura‡Æo"
                 tt-dados.celula-fonte-negrito = YES
                 tt-dados.celula-fonte-cor = 2.
          CREATE tt-dados.
          ASSIGN tt-dados.arquivo-num   = 1
                 tt-dados.planilha-num  = 1
                 tt-dados.celula-coluna = 6
                 tt-dados.celula-cor-interior = 10
                 tt-dados.celula-linha  = i-lin-excel
                 tt-dados.celula-alinhamento-horizontal = 5
                 tt-dados.celula-valor  = "Custo R$"
                 tt-dados.celula-fonte-negrito = YES
                 tt-dados.celula-fonte-cor = 2.
          ASSIGN i-lin-excel = i-lin-excel + 1.
       END.
       
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num   = 1
              tt-dados.planilha-num  = 1
              tt-dados.celula-coluna = 1
              tt-dados.celula-linha  = i-lin-excel
              tt-dados.celula-valor  = string(tt-resumo.data-chamada).
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num   = 1
              tt-dados.planilha-num  = 1
              tt-dados.celula-coluna = 2
              tt-dados.celula-linha  = i-lin-excel
              tt-dados.celula-valor  = tt-resumo.hora-chamada.
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num    = 1
              tt-dados.planilha-num   = 1
              tt-dados.celula-coluna  = 3
              tt-dados.celula-linha   = i-lin-excel
              tt-dados.celula-valor   = tt-resumo.tel-origem.
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num   = 1
              tt-dados.planilha-num  = 1
              tt-dados.celula-coluna = 4
              tt-dados.celula-linha  = i-lin-excel
              tt-dados.celula-valor  = tt-resumo.tel-destino.
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num   = 1
              tt-dados.planilha-num  = 1
              tt-dados.celula-coluna = 5
              tt-dados.celula-linha  = i-lin-excel
              tt-dados.celula-formato = "###.##0,0"
              tt-dados.celula-alinhamento-horizontal = 5
              tt-dados.celula-valor  = string(tt-resumo.duracao).
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num   = 1
              tt-dados.planilha-num  = 1
              tt-dados.celula-coluna = 6
              tt-dados.celula-linha  = i-lin-excel
              tt-dados.celula-formato = "###.##0,00"
              tt-dados.celula-alinhamento-horizontal = 5
              tt-dados.celula-valor  = string(tt-resumo.valor).
       ASSIGN i-lin-excel = i-lin-excel + 1.
    END.
    /* --- Gera Planilha com Resumo Mensal --- */
    IF tt-param.gerar-resumo THEN
       PUT STREAM str-saida
                  tt-ramais.responsavel      FORMAT "x(20)" ";"
                  tt-resumo.data-chamada ";"
                  tt-resumo.hora-chamada ";"
                  tt-resumo.tel-origem       FORMAT "x(10)" ";"  
                  tt-resumo.tel-destino      FORMAT "x(15)" ";" 
                  tt-resumo.duracao          FORMAT ">>>,>>9.9" ";"     
                  tt-resumo.valor            FORMAT ">>>,>>9.9999999999"      
                  SKIP.

    IF tt-param.enviar-e-mail THEN DO:
       IF LAST-OF(tt-ramais.ramal) THEN DO:
          CREATE tt-dados.
          ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 1
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-valor  = "Total do Ramal".
          CREATE tt-dados.
          ASSIGN tt-dados.arquivo-num   = 1
                 tt-dados.planilha-num  = 1
                 tt-dados.celula-coluna = 5
                 tt-dados.celula-linha  = i-lin-excel
                 tt-dados.celula-formato = "###.##0,0"
                 tt-dados.celula-alinhamento-horizontal = 5
                 tt-dados.celula-valor  = STRING((ACCUM TOTAL BY tt-ramais.ramal tt-resumo.duracao)).
          CREATE tt-dados.
          ASSIGN tt-dados.arquivo-num   = 1
                 tt-dados.planilha-num  = 1
                 tt-dados.celula-coluna = 6
                 tt-dados.celula-linha  = i-lin-excel
                 tt-dados.celula-formato = "###.##0,00"
                 tt-dados.celula-alinhamento-horizontal = 5
                 tt-dados.celula-valor  = STRING((ACCUM TOTAL BY tt-ramais.ramal tt-resumo.valor)).
          ASSIGN i-lin-excel = i-lin-excel + 2.
       END.
       
       IF LAST-OF(tt-ramais.responsavel) THEN DO:
          CREATE tt-dados.
          ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 1
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-valor  = "Total do Respons vel".
          CREATE tt-dados.
          ASSIGN tt-dados.arquivo-num   = 1
                 tt-dados.planilha-num  = 1
                 tt-dados.celula-coluna = 5
                 tt-dados.celula-linha  = i-lin-excel
                 tt-dados.celula-formato = "###.##0,0"
                 tt-dados.celula-alinhamento-horizontal = 5
                 tt-dados.celula-valor  = STRING((ACCUM TOTAL BY tt-ramais.responsavel tt-resumo.duracao)).
          CREATE tt-dados.
          ASSIGN tt-dados.arquivo-num   = 1
                 tt-dados.planilha-num  = 1
                 tt-dados.celula-coluna = 6
                 tt-dados.celula-linha  = i-lin-excel
                 tt-dados.celula-formato = "###.##0,00"
                 tt-dados.celula-alinhamento-horizontal = 5
                 tt-dados.celula-valor  = STRING((ACCUM TOTAL BY tt-ramais.responsavel tt-resumo.valor)).
          
          RUN utp/utapi003.p (INPUT-OUTPUT TABLE tt-configuracao,
                              INPUT-OUTPUT TABLE tt-planilha,
                              INPUT-OUTPUT TABLE tt-dados,
                              INPUT-OUTPUT TABLE tt-grafico,
                              INPUT-OUTPUT TABLE tt-erros).
       
          /*--- Enviar e-mail para o respons vel ---*/
          IF tt-param.enviar-e-mail and
             (tt-ramais.e-mail MATCHES "*@*" OR
              tt-param.email-telefon MATCHES "*@*" OR
              tt-param.email-info MATCHES "*@*") THEN DO:
             ASSIGN c-destinatario = "".
             IF tt-param.enviar-resp THEN
                ASSIGN c-destinatario = c-destinatario + tt-ramais.e-mail + ",".
             IF tt-param.enviar-telefon THEN
                ASSIGN c-destinatario = c-destinatario + tt-param.email-telefon + ",".
             IF tt-param.enviar-info THEN
                ASSIGN c-destinatario = c-destinatario + tt-param.email-info.
             IF c-destinatario <> "" AND substr(c-destinatario,LENGTH(c-destinatario),1) = "," THEN
                ASSIGN c-destinatario = SUBSTR(c-destinatario,1,LENGTH(c-destinatario) - 1).
             
             RUN esapi/esapi002.p (INPUT tt-param.email-info, /* e-mail remetente */
                                   INPUT c-destinatario, /* e-mail destinat rio */
                                   INPUT tt-param.subject-e-mail, /* Assunto */
                                   INPUT tt-param.texto-e-mail, /* Mensagem */
                                   INPUT tt-param.arq-tmp-email, /*arquivo anexo*/
                                   INPUT YES). /* Mostra Erros */
          END.
          FOR EACH tt-configuracao:
              DELETE tt-configuracao.
          END.  
          FOR EACH tt-planilha:
              DELETE tt-planilha.
          END.
          FOR EACH tt-dados:
              DELETE tt-dados.
          END.
       END.
    END.
END.
IF tt-param.gerar-resumo THEN
   OUTPUT STREAM str-saida CLOSE.

IF tt-param.imp-param THEN DO:
   PAGE STREAM str-rp.
   PUT STREAM str-rp "*---------------------- PAR¶METROS ------------------------*"
       SKIP.
   DISPLAY STREAM str-rp
           tt-param.ini-responsavel                                               
           tt-param.fin-responsavel     
           tt-param.ini-dt-chamada   
           tt-param.fin-dt-chamada      
           tt-param.ini-ramal
           tt-param.fin-ramal
           (ACCUM TOTAL tt-resumo.duracao) @ tt-resumo.duracao
           (ACCUM TOTAL tt-resumo.valor)   @ tt-resumo.valor 
           WITH FRAME f-parlis.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i &STREAM="stream str-rp"}

run pi-finalizar in h-acomp.
return "OK":U.



