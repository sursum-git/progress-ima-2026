/* Programa: ESFT0013RP.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Faturamento
** Objetivo: Gerar arquivos de Faturamento e Clientes para integra‡Æo com o 
**           representante HERMANTEX (Sistema da Vtex)
** Autor...: Gilvando de Souza Araujo - Setembro/2004
** Obs.....: Especifico da TEAR TÒXTIL INDéSTRIA E COMRCIO LTDA.
**
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESFT0013RP 2.04.00.000}

define temp-table tt-param no-undo
       field destino          as integer
       field arquivo          as char format "x(35)"
       field usuario          as char format "x(12)"
       field data-exec        as date
       field hora-exec        as integer
       field classifica       as integer
       FIELD estab-ini        like nota-fiscal.cod-estabel              
       FIELD estab-fin        like nota-fiscal.cod-estabel   
       FIELD serie-ini        LIKE nota-fiscal.serie 
       FIELD serie-fin        LIKE nota-fiscal.serie
       FIELD espdoc-ini       like nota-fiscal.esp-docto
       FIELD espdoc-fin       like nota-fiscal.esp-docto
       FIELD dt-emis-ini      LIKE nota-fiscal.dt-emis-nota
       FIELD dt-emis-fin      LIKE nota-fiscal.dt-emis-nota
       FIELD cod-rep1         LIKE nota-fiscal.cod-rep
       FIELD cod-rep2         LIKE nota-fiscal.cod-rep
       FIELD arq-fatura       AS CHAR FORMAT "x(45)"
       FIELD enviar-e-mail    AS LOG FORMAT "Sim/NÆo"
       FIELD subject-e-mail   AS CHAR FORMAT "x(40)"
       FIELD texto-e-mail     AS CHAR FORMAT "x(2000)"
       FIELD l-batch          AS LOG
       FIELD impr-param       AS LOG.

define temp-table tt-raw-digita
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* Altera parƒmetros, quando execu‡Æo for em batch */
IF tt-param.l-batch THEN DO:
   assign tt-param.usuario          = "super"
          tt-param.destino          = 2 /* Arquivo */
          tt-param.arquivo          = "ESFT0013.LST"
          tt-param.data-exec        = TODAY
          tt-param.hora-exec        = TIME
          tt-param.estab-ini        = "1"
          tt-param.estab-fin        = "999"
          tt-param.serie-ini        = "3"
          tt-param.serie-fin        = "3"
          tt-param.espdoc-ini       = 22 /* NFS */
          tt-param.espdoc-fin       = 22 /* NFS */
          tt-param.cod-rep1         = 14 /* Hermantex */
          tt-param.cod-rep2         = 119 /* Herpin */
          tt-param.enviar-e-mail    = YES
          tt-param.subject-e-mail   = "Pedidos Faturados-Integra‡Æo Vtex"
          tt-param.texto-e-mail     = "Segue anexo Arquivo de Pedidos faturados no per¡odo de #PER-INI a #PER-FIN." +
                                      CHR(13) + CHR(13) + "Atenciosamente," + CHR(13) + CHR(13) +
                                      "TEAR TEXTIL INDUSTRIA E COMERCIO LTDA." + CHR(13) + 
                                      "Departamento Comercial."
          tt-param.impr-param       = NO.  

   /* O programa esta agendado para rodar de Seg a Sex …s 06:00, selecionando
   ** o dia anterior quando for Ter a Dom e, Sex, Sab e Dom, quando for Seg */
   IF WEEKDAY(TODAY) = 2 THEN /* Seg */
      ASSIGN tt-param.dt-emis-ini = TODAY - 3
             tt-param.dt-emis-fin = TODAY - 1.
   ELSE                       /* Ter a Dom */
      ASSIGN tt-param.dt-emis-ini = TODAY - 1
             tt-param.dt-emis-fin = TODAY - 1.

   assign tt-param.arq-fatura = SESSION:TEMP-DIRECTORY + 
                                string(year(tt-param.dt-emis-fin),"9999") +
                                string(month(tt-param.dt-emis-fin),"99") +
                                string(day(tt-param.dt-emis-fin),"99") + 
                                ".txt".
END.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* defini‡Æo de vari veis  */
DEFINE VAR h-acomp as handle no-undo.

DEFINE VAR c-saida      as char format "x(256)".
DEFINE VAR c-dt-saida   as char.
DEFINE VAR de-per-com1  as dec.
DEFINE VAR de-per-com2  as dec.
DEFINE VAR i-transp     like transporte.cod-transp.
DEFINE VAR i-tr-red     like transporte.cod-transp.
DEFINE VAR c-cnpj-cpf   as char.
DEFINE VAR c-cnpj-aux   as char.
DEFINE VAR i-cont       as int.
DEFINE VAR c-destinatar AS CHAR.
DEFINE VAR c-fone       LIKE emitente.telefone[1].
DEFINE VAR c-fax        LIKE emitente.telefax.

DEF STREAM fatur.

form
    tt-param.estab-ini    LABEL "Estabelecimento.." AT  1
    "a"                                             AT 31
    tt-param.estab-fin    NO-LABELS
    tt-param.serie-ini    LABEL "Serie............" AT  1
    "a"                                             AT 31
    tt-param.serie-fin    NO-LABELS
    tt-param.espdoc-ini   LABEL "Esp‚cie.........." AT  1
    "a"                                             AT 31
    tt-param.espdoc-fin   NO-LABELS
    tt-param.dt-emis-ini  LABEL "Data EmissÆo....." AT  1
    "a"                                             AT 31
    tt-param.dt-emis-fin  NO-LABELS
    tt-param.cod-rep1     LABEL "Representante1..." AT  1
    tt-param.cod-rep2     LABEL "Representante2..." AT  1
    tt-param.arq-fatura   LABEL "Arq.sa¡da Faturas" AT  1
    with no-box side-labels width 132 STREAM-IO frame f-parlis.

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i}

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i FATURAMENTO * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Faturamento/Clientes_para_integra‡Æo_com_a_Hermantex * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

/* ---- Representante 1 -------------------------------------------------*/
output stream fatur to value(tt-param.arq-fatura).

for each nota-fiscal where nota-fiscal.cod-estabel  >= tt-param.estab-ini
                       AND nota-fiscal.cod-estabel  <= tt-param.estab-fin
                       AND nota-fiscal.serie        >= tt-param.serie-ini
                       AND nota-fiscal.serie        <= tt-param.serie-fin
                       AND nota-fiscal.esp-doc      >= tt-param.espdoc-ini
                       AND nota-fiscal.esp-doc      <= tt-param.espdoc-fin
                       AND nota-fiscal.dt-emis-nota >= tt-param.dt-emis-ini
                       AND nota-fiscal.dt-emis-nota <= tt-param.dt-emis-fin
                       AND nota-fiscal.cod-rep      =  tt-param.cod-rep1
                       AND nota-fiscal.dt-cancela   = ?
                       AND nota-fiscal.emite-dup    = YES
                     no-lock:

    run pi-acompanhar in h-acomp (input "Nota Fiscal: " + nota-fiscal.nr-nota-fis).

    find emitente where emitente.cod-emitente = nota-fiscal.cod-emitente
                  no-lock no-error.
    if avail emitente then do:
       if emitente.natureza = 1 then
          assign c-cnpj-cpf = "CPF".
       else
       if emitente.natureza = 2 then
          assign c-cnpj-cpf = "cnpj".
       else
          assign c-cnpj-cpf = " ".
    
       assign c-cnpj-aux = ""
              c-fone     = ""
              c-fax      = "".
       DO i-cont = 1 TO LENGTH(emitente.cgc):
          IF SUBSTR(emitente.cgc,i-cont,1) >= "0" AND
             SUBSTR(emitente.cgc,i-cont,1) <= "9" THEN
             ASSIGN c-cnpj-aux = c-cnpj-aux + SUBSTR(emitente.cgc,i-cont,1).
       END.
       DO i-cont = 1 TO LENGTH(emitente.telefone[1]):
          IF SUBSTR(emitente.telefone[1],i-cont,1) >= "0" AND
             SUBSTR(emitente.telefone[1],i-cont,1) <= "9" THEN
             ASSIGN c-fone = c-fone + SUBSTR(emitente.telefone[1],i-cont,1).
       END.
       DO i-cont = 1 TO LENGTH(emitente.telefax):
          IF SUBSTR(emitente.telefax,i-cont,1) >= "0" AND
             SUBSTR(emitente.telefax,i-cont,1) <= "9" THEN
             ASSIGN c-fax = c-fax + SUBSTR(emitente.telefax,i-cont,1).
       END.

       c-saida = 
       "cliente" + "|" +
       c-cnpj-cpf + "|" +
       c-cnpj-aux + "|" +
       emitente.nome-emit + "|" +
       " " + "|" + /* Nome fantasia */
       string(emitente.cod-emitente) + "|" +
       emitente.ins-estadual + "|" +
       c-fone + "|" +
       c-fax + "|" +
       " " + "|" +  /* Site */
       string(emitente.cep) + "|" +
       emitente.endereco + "|" +
       " " + "|" + /* Complemento */
       emitente.bairro + "|" +
       emitente.cidade + "|" +
       emitente.estado + "|" +
       emitente.pais + "|" +
       string(emitente.lim-credito * 100,"9999999999999,99") + "|" +
       string(emitente.cod-rep) + "|" +
       " " + "|" + /* Data de fundacao */
       string(year(emitente.data-implant),"9999") + "-" +
       string(month(emitente.data-implant),"99") + "-" +
       string(day(emitente.data-implant),"99").
    
       put stream fatur c-saida
           skip.
    end.              

    if nota-fiscal.dt-saida = ? then
       assign c-dt-saida = " ".
    else 
       assign c-dt-saida = string(year(nota-fiscal.dt-saida),"9999") +
                           "-" + 
                           string(month(nota-fiscal.dt-saida),"99") +
                           "-" +
                           string(day(nota-fiscal.dt-saida),"99").
    find transporte 
    where transporte.nome-abrev = nota-fiscal.nome-transp
    no-lock no-error.
    if avail transporte then
       assign i-transp = transporte.cod-transp.
    else
       assign i-transp = 0.
    find transporte 
    where transporte.nome-abrev = nota-fiscal.nome-tr-red
                    no-lock no-error.
    if avail transporte then
       assign i-tr-red = transporte.cod-transp.
    else
       assign i-tr-red = 0.

    assign c-saida = 
           "fatura" +  "|" +
           string(nota-fiscal.nr-nota-fis) +  "|" +
           "001" + "|" +
           string(year(nota-fiscal.dt-emis-nota),"9999") + "-" +
           string(month(nota-fiscal.dt-emis-nota),"99") + "-" +
           string(day(nota-fiscal.dt-emis-nota),"99") + "|" +
           c-dt-saida + "|" +
           string(nota-fiscal.cod-emitente) + "|" +
           string(nota-fiscal.cod-rep) + "|" +
           string(i-transp) + "|" +
           string(i-tr-red) + "|" +
           "R$" + "|" +
           "0.00" + "|" +
           string(nota-fiscal.vl-tot-nota * 100,"9999999999999999,99").
    put stream fatur c-saida    
        skip.

    for each it-nota-fisc of nota-fiscal no-lock:
        find item where item.it-codigo = it-nota-fisc.it-codigo
                  no-lock no-error.
        FIND referencia WHERE referencia.cod-refer = it-nota-fisc.cod-refer
                        NO-LOCK NO-ERROR.
        assign c-saida = 
             "item" +  "|" +
             string(nota-fiscal.nr-nota-fis) +  "|" +
             it-nota-fisc.it-codigo + it-nota-fisc.cod-refer + "|" +
             item.descricao-1 + item.descricao-2 + " " +
             SUBSTR(referencia.descricao,1,3) + "|" +
             item.un + "|" +
             string(it-nota-fisc.qt-faturada[1] * 100,"9999999999999999,99")
                    + "|" +
                    string(it-nota-fisc.vl-preuni * 
                    100,"9999999999999999,99")
                    + "|" +
             string(it-nota-fisc.vl-tot-item * 
                    100,"9999999999999999,99").
        put stream fatur c-saida    
            skip.
    end.
    find repres where repres.cod-rep = nota-fiscal.cod-rep 
                no-lock no-error.
    assign de-per-com1 = repres.comis-direta * 
                         repres.comis-emis * 0.01
           de-per-com2 = repres.comis-direta - de-per-com1.
    for each fat-duplic 
        where fat-duplic.cod-estabel = nota-fiscal.cod-estabel
          and fat-duplic.serie       = nota-fiscal.serie
          and fat-duplic.nr-fatura   = nota-fiscal.nr-nota-fis
        no-lock:
        assign c-saida = 
               "duplicata" +  "|" +
               string(nota-fiscal.nr-nota-fis) +  "|" +
               string(fat-duplic.parcela,"99") + "|" +
               /* Substitui a data de emissÆo da duplicata que estava
                  na tabela fat-duplic, pela da tabela nota-fiscal
                  porque …s vezes a data de emissÆo nÆo est  correta
                  Gilvando - 10/03/2006.
               string(year(fat-duplic.dt-emissao),"9999") + "-" +
               string(month(fat-duplic.dt-emissao),"99") + "-" +
               string(day(fat-duplic.dt-emissao),"99") + "|" +
               */
               string(year(nota-fiscal.dt-emis-nota),"9999") + "-" +
               string(month(nota-fiscal.dt-emis-nota),"99") + "-" +
               string(day(nota-fiscal.dt-emis-nota),"99") + "|" +
               string(year(fat-duplic.dt-vencim),"9999") + "-" +
               string(month(fat-duplic.dt-vencim),"99") + "-" +
               string(day(fat-duplic.dt-vencim),"99") + "|" +
               string(fat-duplic.vl-parcela * 100,"9999999999999999,99")
               + "|" +
               string(de-per-com1 * 100,"9999999999999999,99")
               + "|" +
               string(de-per-com2 * 100,"9999999999999999,99").
        put stream fatur c-saida    
            skip.
    end.
    FIND ped-venda WHERE ped-venda.nome-abrev = nota-fiscal.nome-ab-cli
                     AND ped-venda.nr-pedcli  = nota-fiscal.nr-pedcli
                   NO-LOCK NO-ERROR.
    assign c-saida = 
           "pedido" +  "|" +
           string(nota-fiscal.nr-nota-fis) + "|" +
           string(int(nota-fiscal.nr-pedcli)) + "|" +
           ped-venda.nr-pedrep.
    put stream fatur c-saida    
        skip.
end.  

output stream fatur close.

IF tt-param.enviar-e-mail THEN DO:
   FIND repres WHERE repres.cod-rep = tt-param.cod-rep1 NO-LOCK NO-ERROR.
   ASSIGN tt-param.texto = REPLACE(tt-param.texto,"#PER-INI",STRING(tt-param.dt-emis-ini))
          tt-param.texto = REPLACE(tt-param.texto,"#PER-FIN",STRING(tt-param.dt-emis-fin))
          c-destinatar   = repres.e-mail + "," + "teartextil@teartextil.com.br".
   RUN esapi/esapi002.p (INPUT "vendas@teartextil.com.br", /* e-mail remetente */
                         INPUT c-destinatar, /* e-mail destinat rio */
                         INPUT "Pedidos Faturados-Integra‡Æo Vtex: " + string(tt-param.dt-emis-ini) +
                               " a " + string(tt-param.dt-emis-fin), /* Assunto */
                         INPUT "Anexo, arquivo de pedidos faturados-Integra‡Æo Vtex: " + 
                               string(tt-param.dt-emis-ini) +
                               " a " + string(tt-param.dt-emis-fin) + ".", /* Mensagem */
                         INPUT tt-param.arq-fatura, /*arquivo anexo*/
                         INPUT YES). /* Mostra Erros */
END.

/* ---- Representante 2 -------------------------------------------------*/
output stream fatur to value(tt-param.arq-fatura).

for each nota-fiscal where nota-fiscal.cod-estabel  >= tt-param.estab-ini
                       AND nota-fiscal.cod-estabel  <= tt-param.estab-fin
                       AND nota-fiscal.serie        >= tt-param.serie-ini
                       AND nota-fiscal.serie        <= tt-param.serie-fin
                       AND nota-fiscal.esp-doc      >= tt-param.espdoc-ini
                       AND nota-fiscal.esp-doc      <= tt-param.espdoc-fin
                       AND nota-fiscal.dt-emis-nota >= tt-param.dt-emis-ini
                       AND nota-fiscal.dt-emis-nota <= tt-param.dt-emis-fin
                       AND nota-fiscal.cod-rep      =  tt-param.cod-rep2
                       AND nota-fiscal.dt-cancela   = ?
                       AND nota-fiscal.emite-dup    = YES
                     no-lock:

    run pi-acompanhar in h-acomp (input "Nota Fiscal: " + nota-fiscal.nr-nota-fis).

    find emitente where emitente.cod-emitente = nota-fiscal.cod-emitente
                  no-lock no-error.
    if avail emitente then do:
       if emitente.natureza = 1 then
          assign c-cnpj-cpf = "CPF".
       else
       if emitente.natureza = 2 then
          assign c-cnpj-cpf = "cnpj".
       else
          assign c-cnpj-cpf = " ".
    
       assign c-cnpj-aux = ""
              c-fone     = ""
              c-fax      = "".
       DO i-cont = 1 TO LENGTH(emitente.cgc):
          IF SUBSTR(emitente.cgc,i-cont,1) >= "0" AND
             SUBSTR(emitente.cgc,i-cont,1) <= "9" THEN
             ASSIGN c-cnpj-aux = c-cnpj-aux + SUBSTR(emitente.cgc,i-cont,1).
       END.
       DO i-cont = 1 TO LENGTH(emitente.telefone[1]):
          IF SUBSTR(emitente.telefone[1],i-cont,1) >= "0" AND
             SUBSTR(emitente.telefone[1],i-cont,1) <= "9" THEN
             ASSIGN c-fone = c-fone + SUBSTR(emitente.telefone[1],i-cont,1).
       END.
       DO i-cont = 1 TO LENGTH(emitente.telefax):
          IF SUBSTR(emitente.telefax,i-cont,1) >= "0" AND
             SUBSTR(emitente.telefax,i-cont,1) <= "9" THEN
             ASSIGN c-fax = c-fax + SUBSTR(emitente.telefax,i-cont,1).
       END.

       c-saida = 
       "cliente" + "|" +
       c-cnpj-cpf + "|" +
       c-cnpj-aux + "|" +
       emitente.nome-emit + "|" +
       " " + "|" + /* Nome fantasia */
       string(emitente.cod-emitente) + "|" +
       emitente.ins-estadual + "|" +
       c-fone + "|" +
       c-fax + "|" +
       " " + "|" +  /* Site */
       string(emitente.cep) + "|" +
       emitente.endereco + "|" +
       " " + "|" + /* Complemento */
       emitente.bairro + "|" +
       emitente.cidade + "|" +
       emitente.estado + "|" +
       emitente.pais + "|" +
       string(emitente.lim-credito * 100,"9999999999999,99") + "|" +
       string(emitente.cod-rep) + "|" +
       " " + "|" + /* Data de fundacao */
       string(year(emitente.data-implant),"9999") + "-" +
       string(month(emitente.data-implant),"99") + "-" +
       string(day(emitente.data-implant),"99").
    
       put stream fatur c-saida
           skip.
    end.              

    if nota-fiscal.dt-saida = ? then
       assign c-dt-saida = " ".
    else 
       assign c-dt-saida = string(year(nota-fiscal.dt-saida),"9999") +
                           "-" + 
                           string(month(nota-fiscal.dt-saida),"99") +
                           "-" +
                           string(day(nota-fiscal.dt-saida),"99").
    find transporte 
    where transporte.nome-abrev = nota-fiscal.nome-transp
    no-lock no-error.
    if avail transporte then
       assign i-transp = transporte.cod-transp.
    else
       assign i-transp = 0.
    find transporte 
    where transporte.nome-abrev = nota-fiscal.nome-tr-red
                    no-lock no-error.
    if avail transporte then
       assign i-tr-red = transporte.cod-transp.
    else
       assign i-tr-red = 0.

    assign c-saida = 
           "fatura" +  "|" +
           string(nota-fiscal.nr-nota-fis) +  "|" +
           "001" + "|" +
           string(year(nota-fiscal.dt-emis-nota),"9999") + "-" +
           string(month(nota-fiscal.dt-emis-nota),"99") + "-" +
           string(day(nota-fiscal.dt-emis-nota),"99") + "|" +
           c-dt-saida + "|" +
           string(nota-fiscal.cod-emitente) + "|" +
           string(nota-fiscal.cod-rep) + "|" +
           string(i-transp) + "|" +
           string(i-tr-red) + "|" +
           "R$" + "|" +
           "0.00" + "|" +
           string(nota-fiscal.vl-tot-nota * 100,"9999999999999999,99").
    put stream fatur c-saida    
        skip.

    for each it-nota-fisc of nota-fiscal no-lock:
        find item where item.it-codigo = it-nota-fisc.it-codigo
                  no-lock no-error.
        FIND referencia WHERE referencia.cod-refer = it-nota-fisc.cod-refer
                        NO-LOCK NO-ERROR.
        assign c-saida = 
             "item" +  "|" +
             string(nota-fiscal.nr-nota-fis) +  "|" +
             it-nota-fisc.it-codigo + it-nota-fisc.cod-refer + "|" +
             item.descricao-1 + item.descricao-2 + " " +
             SUBSTR(referencia.descricao,1,3) + "|" +
             item.un + "|" +
             string(it-nota-fisc.qt-faturada[1] * 100,"9999999999999999,99")
                    + "|" +
                    string(it-nota-fisc.vl-preuni * 
                    100,"9999999999999999,99")
                    + "|" +
             string(it-nota-fisc.vl-tot-item * 
                    100,"9999999999999999,99").
        put stream fatur c-saida    
            skip.
    end.
    find repres where repres.cod-rep = nota-fiscal.cod-rep 
                no-lock no-error.
    assign de-per-com1 = repres.comis-direta * 
                         repres.comis-emis * 0.01
           de-per-com2 = repres.comis-direta - de-per-com1.
    for each fat-duplic 
        where fat-duplic.cod-estabel = nota-fiscal.cod-estabel
          and fat-duplic.serie       = nota-fiscal.serie
          and fat-duplic.nr-fatura   = nota-fiscal.nr-nota-fis
        no-lock:
        assign c-saida = 
               "duplicata" +  "|" +
               string(nota-fiscal.nr-nota-fis) +  "|" +
               string(fat-duplic.parcela,"99") + "|" +
               /* Substitui a data de emissÆo da duplicata que estava
                  na tabela fat-duplic, pela da tabela nota-fiscal
                  porque …s vezes a data de emissÆo nÆo est  correta
                  Gilvando - 10/03/2006.
               string(year(fat-duplic.dt-emissao),"9999") + "-" +
               string(month(fat-duplic.dt-emissao),"99") + "-" +
               string(day(fat-duplic.dt-emissao),"99") + "|" +
               */
               string(year(nota-fiscal.dt-emis-nota),"9999") + "-" +
               string(month(nota-fiscal.dt-emis-nota),"99") + "-" +
               string(day(nota-fiscal.dt-emis-nota),"99") + "|" +
               string(year(fat-duplic.dt-vencim),"9999") + "-" +
               string(month(fat-duplic.dt-vencim),"99") + "-" +
               string(day(fat-duplic.dt-vencim),"99") + "|" +
               string(fat-duplic.vl-parcela * 100,"9999999999999999,99")
               + "|" +
               string(de-per-com1 * 100,"9999999999999999,99")
               + "|" +
               string(de-per-com2 * 100,"9999999999999999,99").
        put stream fatur c-saida    
            skip.
    end.
    FIND ped-venda WHERE ped-venda.nome-abrev = nota-fiscal.nome-ab-cli
                     AND ped-venda.nr-pedcli  = nota-fiscal.nr-pedcli
                   NO-LOCK NO-ERROR.
    assign c-saida = 
           "pedido" +  "|" +
           string(nota-fiscal.nr-nota-fis) + "|" +
           string(int(nota-fiscal.nr-pedcli)) + "|" +
           ped-venda.nr-pedrep.
    put stream fatur c-saida    
        skip.
end.  

output stream fatur close.

IF tt-param.enviar-e-mail THEN DO:
   FIND repres WHERE repres.cod-rep = tt-param.cod-rep2 NO-LOCK NO-ERROR.
   ASSIGN tt-param.texto = REPLACE(tt-param.texto,"#PER-INI",STRING(tt-param.dt-emis-ini))
          tt-param.texto = REPLACE(tt-param.texto,"#PER-FIN",STRING(tt-param.dt-emis-fin))
          c-destinatar   = repres.e-mail + "," + "teartextil@teartextil.com.br".
   RUN esapi/esapi002.p (INPUT "vendas@teartextil.com.br", /* e-mail remetente */
                         INPUT c-destinatar, /* e-mail destinat rio */
                         INPUT "Pedidos Faturados-Integra‡Æo Vtex: " + string(tt-param.dt-emis-ini) +
                               " a " + string(tt-param.dt-emis-fin), /* Assunto */
                         INPUT "Anexo, arquivo de pedidos faturados-Integra‡Æo Vtex: " + 
                               string(tt-param.dt-emis-ini) +
                               " a " + string(tt-param.dt-emis-fin) + ".", /* Mensagem */
                         INPUT tt-param.arq-fatura, /*arquivo anexo*/
                         INPUT YES). /* Mostra Erros */
END.

IF tt-param.impr-param THEN DO:
   PAGE.
   PUT "*****----- PAR¶METROS ------*****"
       SKIP(1).

   DISPLAY tt-param.estab-ini     tt-param.estab-fin                                           
           tt-param.serie-ini     tt-param.serie-fin    
           tt-param.espdoc-ini    tt-param.espdoc-fin   
           tt-param.dt-emis-ini   tt-param.dt-emis-fin       
           tt-param.cod-rep1
           tt-param.cod-rep2
           tt-param.arq-fatura  
           WITH FRAME f-parlis.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.



