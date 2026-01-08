/* Programa: imp-embratel.p
** Objetivo: Importar o arquivo "csv" com dados de ligaá‰es telefìnicas
**           da Embratel, para geraá∆o de relatorio por chefia.
*/

DEF TEMP-TABLE tt-embratel
    FIELD data-chamada  AS DATE FORMAT 99/99/9999
    FIELD hora-chamada  AS CHAR
    FIELD tel-origem    AS CHAR
    FIELD ramal         AS CHAR
    FIELD setor         AS CHAR
    FIELD responsavel   AS CHAR
    FIELD e-mail        AS CHAR
    FIELD tel-destino   AS CHAR
    FIELD duracao       AS DEC
    FIELD custo         AS DEC.
    
DEF TEMP-TABLE tt-resumo
    FIELD data-chamada  AS DATE FORMAT 99/99/9999
    FIELD hora-chamada  AS CHAR
    FIELD ramal         AS CHAR
    FIELD setor         AS CHAR
    FIELD responsavel   AS CHAR
    FIELD e-mail        AS CHAR
    FIELD tel-origem    AS CHAR
    FIELD tel-destino   AS CHAR
    FIELD duracao       AS DEC
    FIELD custo         AS DEC
    INDEX ch-resumo responsavel
                    data-chamada
                    hora-chamada
                    tel-origem
                    tel-destino.

DEF VAR c-temp-entr  AS CHAR FORMAT "x(45)".
DEF VAR c-temp-email AS CHAR FORMAT "x(45)".
DEF VAR i-lin-excel  AS INT INIT 1.

ASSIGN c-temp-entr  = "c:/temp/Mensal - 2006-03.csv".
ASSIGN c-temp-email = "c:/temp/Detalhamento.xls".

UPDATE c-temp-entr  LABEL "Arquivo de entrada"
       c-temp-email LABEL "Arquivo de e-mail."
       WITH SIDE-LABELS 1 COLUMN.

INPUT FROM VALUE(c-temp-entr) CONVERT SOURCE "ibm850".
SET ^.

REPEAT:
   CREATE tt-embratel.
   IMPORT DELIMITER ";" tt-embratel.
END.
INPUT CLOSE.

FOR EACH tt-embratel:
    IF tt-embratel.data-chamada = ? THEN NEXT.
    FIND tt-resumo WHERE tt-resumo.responsavel  = tt-embratel.responsavel
                     AND tt-resumo.data-chamada = tt-embratel.data-chamada
                     AND tt-resumo.hora-chamada = tt-embratel.hora-chamada
                     AND tt-resumo.tel-origem   = tt-embratel.tel-origem
                     AND tt-resumo.tel-destino  = tt-embratel.tel-destino
                   NO-LOCK NO-ERROR.
    IF NOT AVAIL tt-resumo THEN DO:
       CREATE tt-resumo.
       ASSIGN tt-resumo.data-chamada = tt-embratel.data-chamada
              tt-resumo.hora-chamada = tt-embratel.hora-chamada
              tt-resumo.tel-origem   = tt-embratel.tel-origem  
              tt-resumo.ramal        = tt-embratel.ramal
              tt-resumo.setor        = tt-embratel.setor
              tt-resumo.responsavel  = tt-embratel.responsavel
              tt-resumo.e-mail       = tt-embratel.e-mail
              tt-resumo.tel-destino  = tt-embratel.tel-destino. 
    END.
    ASSIGN tt-resumo.duracao = tt-embratel.duracao
           tt-resumo.custo   = tt-resumo.custo + tt-embratel.custo.
END.

/*
FOR EACH tt-resumo:
    ACCUMULATE tt-resumo.duracao(TOTAL).
    ACCUMULATE tt-resumo.custo(TOTAL).
END.
DISPLAY (ACCUM TOTAL tt-resumo.duracao)
        (ACCUM TOTAL tt-resumo.custo).
*/

FOR EACH tt-resumo /*WHERE tt-resumo.responsavel = "Gilvando"*/
                   BREAK BY tt-resumo.responsavel
                         BY tt-resumo.ramal
                         BY tt-resumo.data-chamada
                         BY tt-resumo.hora-chamada:
                         
    ACCUMULATE tt-resumo.duracao (TOTAL BY tt-resumo.responsavel).
    ACCUMULATE tt-resumo.custo (TOTAL BY tt-resumo.responsavel).
    ACCUMULATE tt-resumo.duracao (TOTAL BY tt-resumo.ramal).
    ACCUMULATE tt-resumo.custo (TOTAL BY tt-resumo.ramal).
    
    IF FIRST-OF(tt-resumo.responsavel) THEN DO:
       {utp/utapi003.i}
       OS-DELETE VALUE(c-temp-email).
       CREATE tt-configuracao.
       ASSIGN tt-configuracao.versao-integracao   = 2
              tt-configuracao.arquivo-num         = 1
              tt-configuracao.arquivo             = c-temp-email
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
              tt-dados.celula-valor  = "TEAR T“XTIL INDÈSTRIA E COMêRCIO LTDA."
              tt-dados.celula-fonte-tamanho = 24
              tt-dados.celula-fonte-negrito = YES
              tt-dados.celula-fonte-cor     = 3.
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num   = 1
              tt-dados.planilha-num  = 1
              tt-dados.celula-coluna = 1
              tt-dados.celula-linha  = 3
              tt-dados.celula-valor  = "DETALHAMENTO DE LIGAÄÂES TELEF‚NICAS DOS SEUS RAMAIS"
              tt-dados.celula-fonte-tamanho = 14
              tt-dados.celula-fonte-negrito = YES.
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num   = 1
              tt-dados.planilha-num  = 1
              tt-dados.celula-coluna = 1
              tt-dados.celula-linha  = 5
              tt-dados.celula-valor  = "RESPONSµVEL: " + tt-resumo.responsavel
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
              tt-dados.celula-valor  = "Duraá∆o"
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
           tt-dados.celula-valor  = string(tt-resumo.custo).
    ASSIGN i-lin-excel = i-lin-excel + 1.

    IF LAST-OF(tt-resumo.ramal) THEN DO:
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
              tt-dados.celula-valor  = STRING((ACCUM TOTAL BY tt-resumo.ramal tt-resumo.duracao)).
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num   = 1
              tt-dados.planilha-num  = 1
              tt-dados.celula-coluna = 6
              tt-dados.celula-linha  = i-lin-excel
              tt-dados.celula-formato = "###.##0,00"
              tt-dados.celula-alinhamento-horizontal = 5
              tt-dados.celula-valor  = STRING((ACCUM TOTAL BY tt-resumo.ramal tt-resumo.custo)).
       ASSIGN i-lin-excel = i-lin-excel + 2.
    END.
    
    IF LAST-OF(tt-resumo.responsavel) THEN DO:
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num   = 1
       tt-dados.planilha-num  = 1
       tt-dados.celula-coluna = 1
       tt-dados.celula-linha  = i-lin-excel
       tt-dados.celula-valor  = "Total do Respons†vel".
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num   = 1
              tt-dados.planilha-num  = 1
              tt-dados.celula-coluna = 5
              tt-dados.celula-linha  = i-lin-excel
              tt-dados.celula-formato = "###.##0,0"
              tt-dados.celula-alinhamento-horizontal = 5
              tt-dados.celula-valor  = STRING((ACCUM TOTAL BY tt-resumo.responsavel tt-resumo.duracao)).
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num   = 1
              tt-dados.planilha-num  = 1
              tt-dados.celula-coluna = 6
              tt-dados.celula-linha  = i-lin-excel
              tt-dados.celula-formato = "###.##0,00"
              tt-dados.celula-alinhamento-horizontal = 5
              tt-dados.celula-valor  = STRING((ACCUM TOTAL BY tt-resumo.responsavel tt-resumo.custo)).
       
       RUN utp/utapi003.p (INPUT-OUTPUT TABLE tt-configuracao,
                           INPUT-OUTPUT TABLE tt-planilha,
                           INPUT-OUTPUT TABLE tt-dados,
                           INPUT-OUTPUT TABLE tt-grafico,
                           INPUT-OUTPUT TABLE tt-erros).

       /*--- Enviar e-mail para o respons†vel ---*/
       /*
       MESSAGE tt-resumo.responsavel
               tt-resumo.e-mail VIEW-AS ALERT-BOX.
       */
       IF tt-resumo.e-mail MATCHES "*@*" THEN
          RUN esapi/esapi002.p (INPUT "gilvando@teartextil.com.br", /* e-mail remetente */
                                INPUT tt-resumo.e-mail + "," +
                                      "teartextil@teartextil.com.br" + "," +
                                      "graziela.pacheco@teartextil.com.br", /* e-mail destinat†rio */
                                INPUT "Detalhamento de ligaá‰es telefìnicas", /* Assunto */
                                INPUT "Anexo, arquivo de detalhamento das ligaá‰es telefìnicas " + 
                                      "efetuadas nos ramais de sua responsabilidade, para seu " +
                                      "acompanhamento." + CHR(13) + CHR(13) +
                                      "Tear Tàxtil Ind£stria e ComÇrcio Ltda." + CHR(13) +
                                      "Gilvando Souza Araujo - Inform†tica" + CHR(13) + 
                                      "(31)2191-4250 - gilvando@teartextil.com.br", /* Mensagem */
                                INPUT c-temp-email, /*arquivo anexo*/
                                INPUT YES). /* Mostra Erros */
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
