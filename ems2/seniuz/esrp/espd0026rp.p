/* Programa: ESPD0026RP.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Pedidos
** Objetivo: Listar Pedidos de Venda com Avaliaá∆o de CrÇdito
** Autor...: Gilvando Souza Araujo - Abril/2006
** Obs.....: Especifico da TEAR T“XTIL INDÈSTRIA E COMêRCIO LTDA.
*/

/* include de controle de vers∆o */
{include/i-prgvrs.i ESPD0026RP 2.04.00.000}

define temp-table tt-param no-undo
       field destino         as integer
       field arquivo         as char format "x(35)"
       field usuario         as char format "x(12)"
       field data-exec       as date
       field hora-exec       as integer
       field classifica      as integer
       FIELD desc-classifica AS CHAR FORMAT "x(45)"
       FIELD cliente-ini     LIKE ped-venda.nome-abrev
       FIELD cliente-fin     LIKE ped-venda.nome-abrev
       FIELD repres-ini      LIKE ped-venda.no-ab-reppri 
       FIELD repres-fin      LIKE ped-venda.no-ab-reppri
       FIELD dt-entr-ini     LIKE ped-venda.dt-entrega   
       FIELD dt-entr-fin     LIKE ped-venda.dt-entrega   
       FIELD dt-impl-ini     LIKE ped-venda.dt-implant
       FIELD dt-impl-fin     LIKE ped-venda.dt-implant
       FIELD cond-pagto      AS INTEGER
       FIELD desc-cond-pagto AS CHAR FORMAT "x(10)"
       FIELD abertos         AS LOGICAL FORMAT "Sim/N∆o"     
       FIELD at-parcial      AS LOGICAL FORMAT "Sim/N∆o"     
       FIELD at-total        AS LOGICAL FORMAT "Sim/N∆o"     
       FIELD pendentes       AS LOGICAL FORMAT "Sim/N∆o"     
       FIELD suspensos       AS LOGICAL FORMAT "Sim/N∆o"     
       FIELD cancelados      AS LOGICAL FORMAT "Sim/N∆o"     
       FIELD outros          AS LOGICAL FORMAT "Sim/N∆o"     
       FIELD avaliados       AS LOGICAL FORMAT "Sim/N∆o"
       FIELD n-avaliados     AS LOGICAL FORMAT "Sim/N∆o"
       FIELD aprovados       AS LOGICAL FORMAT "Sim/N∆o"
       FIELD n-aprovados     AS LOGICAL FORMAT "Sim/N∆o"
       FIELD pend-inform     AS LOGICAL FORMAT "Sim/N∆o"
       FIELD enviar-e-mail   AS LOGICAL FORMAT "Sim/N∆o"
       FIELD assunto-e-mail  AS CHAR FORMAT "x(40)"   
       FIELD texto-e-mail    AS CHAR FORMAT "x(2000)" 
       FIELD l-batch         AS LOGICAL
       FIELD impr-param      AS LOGICAL.

define temp-table tt-raw-digita
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* Altera parÉmetros, quando execuá∆o for em batch */
IF tt-param.l-batch THEN DO:
   assign tt-param.usuario          = "super"
          tt-param.destino          = 2 /* Arquivo */
          tt-param.arquivo          = "ESPD0026.LST"
          tt-param.data-exec        = TODAY
          tt-param.hora-exec        = TIME
          tt-param.classifica       = 1
          tt-param.desc-classifica  = "Por Representante/Cliente/Pedido"
          tt-param.cliente-ini      = ""
          tt-param.cliente-fin      = "ZZZZZZZZZZZZ"
          tt-param.repres-ini       = ""
          tt-param.repres-fin       = "ZZZZZZZZZZZZ"
          tt-param.dt-entr-ini      = 01/01/0001
          tt-param.dt-entr-fin      = 12/31/9999
          tt-param.dt-impl-ini      = 01/01/0001
          tt-param.dt-impl-fin      = 12/31/9999
          tt-param.cond-pagto       = 2
          tt-param.desc-cond-pagto  = "∑ Vista"
          tt-param.abertos          = YES
          tt-param.at-parcial       = YES
          tt-param.at-total         = NO
          tt-param.pendentes        = NO
          tt-param.suspensos        = YES
          tt-param.cancelados       = NO
          tt-param.outros           = NO
          tt-param.avaliados        = NO
          tt-param.n-avaliados      = NO
          tt-param.aprovados        = NO
          tt-param.n-aprovados      = YES
          tt-param.pend-inform      = NO
          tt-param.enviar-e-mail    = YES
          tt-param.assunto-e-mail   = "Pedidos de Vendas com Avaliaá∆o de CrÇdito"
          tt-param.texto-e-mail     = "Segue anexo Relaá∆o de Pedidos de Venda com Avaliaá∆o de CrÇdito" +
                                      CHR(13) + CHR(13) + "Atenciosamente," + CHR(13) + CHR(13) +
                                      "TEAR TEXTIL INDUSTRIA E COMERCIO LTDA." + CHR(13) + 
                                      "Departamento de CrÇdito/Cobranáa."
          tt-param.impr-param       = YES.
END.

DEFINE TEMP-TABLE tt-rep-email
       FIELD cod-rep LIKE repres.cod-rep
       INDEX ch-rep cod-rep.

/* include padr∆o para vari†veis de relat¢rio  */
{include/i-rpvar.i}

/* definiá∆o de vari†veis  */
def var h-acomp as handle no-undo.

DEF BUFFER b-emitente FOR emitente.

DEF VAR c-situacao        AS CHAR FORMAT "x(6)".
DEF VAR c-linha           AS CHAR FORMAT "x(132)".
DEF VAR c-cgc             AS CHAR FORMAT "x(18)".
DEF VAR c-endereco        LIKE emitente.endereco.
DEF VAR c-bairro          LIKE emitente.bairro.
DEF VAR c-cidade          LIKE emitente.cidade.
DEF VAR c-cep             LIKE emitente.cep.
DEF VAR c-estado          LIKE emitente.estado.
DEF VAR c-aux-e-mail      AS CHAR.
DEF VAR i-e-mail-enviado  AS INT.
DEF VAR i-e-mail-nenviado AS INT.
DEF VAR c-destinatar      AS CHAR.

form
    "*-------------- ParÉmetros/Seleá∆o --------------*" SKIP
    tt-param.desc-classifica LABEL "Classificaá∆o..." AT  1
    tt-param.cliente-ini     LABEL "Cliente........." AT  1
    "a"                                               AT 31
    tt-param.cliente-fin     NO-LABEL                
    tt-param.repres-ini      LABEL "Representante..." AT  1
    "a"                                               AT 31
    tt-param.repres-fin      NO-LABELS
    tt-param.dt-entr-ini     LABEL "Data Entrega...." AT  1 
    "a"                                               AT 31
    tt-param.dt-entr-fin     NO-LABELS
    tt-param.dt-impl-ini     LABEL "Data Implantaá∆o" AT  1 
    "a"                                               AT 31
    tt-param.dt-impl-fin     NO-LABELS
    tt-param.desc-cond-pagto LABEL "Condiá∆o Pagto.." AT  1
    tt-param.abertos         LABEL "Abertos........." AT  1
    tt-param.at-parcial      LABEL "Atend.Parcial..." AT  1
    tt-param.at-total        LABEL "Atend.Total....." AT  1
    tt-param.pendentes       LABEL "Pendentes......." AT  1
    tt-param.suspensos       LABEL "Suspensos......." AT  1
    tt-param.cancelados      LABEL "Cancelados......" AT  1  
    tt-param.outros          LABEL "Outros.........." AT  1  
    tt-param.avaliados       LABEL "Avaliados......." AT  1
    tt-param.n-avaliados     LABEL "N∆o Avaliados..." AT  1
    tt-param.aprovados       LABEL "Aprovados......." AT  1
    tt-param.n-aprovados     LABEL "N∆o Aprovados..." AT  1
    tt-param.pend-inform     LABEL "Pendentes Inform" AT  1
    tt-param.enviar-e-mail   LABEL "Enviar e-mail..." AT  1
    i-e-mail-enviado         LABEL "E-mail env......" AT  1
    i-e-mail-nenviado        LABEL "E-mail n/env...." AT  1
    with no-box side-labels width 132 STREAM-IO frame f-param.

FORM HEADER
    "Pedido      Valor Total SitCrd Implant  Entrega  Aprovador    Motivo" SKIP
    "------ ---------------- ------ -------- -------- ------------"
    "----------------------------------------------------------------------"
    WITH NO-LABELS NO-ATTR-SPACE NO-BOX PAGE-TOP WIDTH 132 STREAM-IO 1 DOWN FRAME f-cab-dados.

form
    ped-venda.nr-pedcli      FORMAT "x(6)"
    ped-venda.vl-tot-ped     
    c-situacao               
    ped-venda.dt-implant     FORMAT "99/99/99"
    ped-venda.dt-entrega     FORMAT "99/99/99"
    ped-venda.quem-aprovou   
    ped-venda.desc-bloq-cr   FORMAT "x(70)"
    WITH NO-LABEL NO-BOX 55 DOWN WIDTH 132 WITH STREAM-IO FRAME f-detalhe.

FORM
    "Representante: " 
    repres.cod-rep " - " 
    repres.nome
    WITH NO-LABEL 2 DOWN NO-BOX NO-ATTR-SPACE WIDTH 132 STREAM-IO FRAME f-repres.

FORM
    c-linha
    WITH NO-LABEL 1 DOWN NO-BOX NO-ATTR-SPACE WIDTH 132 STREAM-IO FRAME f-linha.

FORM
    "Cliente:"
    emitente.cod-emitente
    emitente.nome-emit
    emitente.cgc
    "Lim.Credito: "
    emitente.lim-credito format ">>,>>>,>>9"
    WITH NO-LABEL 2 DOWN NO-BOX NO-ATTR-SPACE WIDTH 132 STREAM-IO frame f-cliente.

/* include padr∆o para output de relat¢rios */
{include/i-rpout.i}

/* include com a definiá∆o da frame de cabeáalho e rodapÇ */
{include/i-rpcab.i}

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i PEDIDOS * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Pedidos_de_Vendas_com_Avaliaá∆o_de_CrÇdito * r}
assign c-titulo-relat = trim(return-value).

ASSIGN c-linha = FILL("-",132).

view frame f-cabec.
VIEW FRAME f-cab-dados.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

DEF STREAM email.
ASSIGN c-aux-e-mail = SESSION:TEMP-DIRECTORY + "aux-e-mail.txt".

IF tt-param.classifica = 1 THEN DO: /* Por Representante/Cliente/Pedido */
   FOR EACH ped-venda WHERE ped-venda.nome-abrev   >= tt-param.cliente-ini
                        AND ped-venda.nome-abrev   <= tt-param.cliente-fin
                        AND ped-venda.no-ab-reppri >= tt-param.repres-ini
                        AND ped-venda.no-ab-reppri <= tt-param.repres-fin
                        AND ped-venda.dt-implant   >= tt-param.dt-impl-ini
                        AND ped-venda.dt-implant   <= tt-param.dt-impl-fin
                        AND ped-venda.dt-entrega   >= tt-param.dt-entr-ini 
                        AND ped-venda.dt-entrega   <= tt-param.dt-entr-fin
                        AND ((tt-param.cond-pagto   = 1 AND ped-venda.cod-cond-pag >= 1 AND ped-venda.cod-cond-pag <= 2) OR
                             (tt-param.cond-pagto   = 2 AND (ped-venda.cod-cond-pag = 0 OR ped-venda.cod-cond-pag > 3)) OR
                             (tt-param.cond-pagto   = 3))
                        AND ((ped-venda.cod-sit-ped = 1 AND tt-param.abertos) OR
                             (ped-venda.cod-sit-ped = 2 AND tt-param.at-parcial) OR
                             (ped-venda.cod-sit-ped = 3 AND tt-param.at-total) OR
                             (ped-venda.cod-sit-ped = 4 AND tt-param.pendentes) OR
                             (ped-venda.cod-sit-ped = 5 AND tt-param.suspensos) OR
                             (ped-venda.cod-sit-ped = 6 AND tt-param.cancelados) OR
                             (ped-venda.cod-sit-ped > 6 AND tt-param.outros)) 
                        AND ((ped-venda.cod-sit-aval = 2 AND tt-param.avaliados) OR
                             (ped-venda.cod-sit-aval = 1 AND tt-param.n-avaliados) OR
                             (ped-venda.cod-sit-aval = 3 AND tt-param.aprovados) OR
                             (ped-venda.cod-sit-aval = 4 AND tt-param.n-aprovados) OR
                             (ped-venda.cod-sit-aval = 5 AND tt-param.pend-inform))
                      NO-LOCK
       BREAK BY ped-venda.no-ab-reppri
             BY ped-venda.nome-abrev
             BY ped-venda.nr-pedcli:
       
       RUN pi-acompanhar in h-acomp (input "Pedido: " + ped-venda.nr-pedcli).
      
       RUN pi-situacao.
      
       IF FIRST-OF(ped-venda.no-ab-reppri) THEN DO:
          find repres where repres.nome-abrev = ped-venda.no-ab-reppri
                      no-lock no-error.
          IF AVAIL repres THEN DO:
             DISPLAY repres.cod-rep 
                     repres.nome
                     with frame f-repres.
             DOWN(2) with frame f-repres.
              
             IF tt-param.enviar-e-mail THEN DO:
                OUTPUT STREAM email TO value(c-aux-e-mail) CONVERT SOURCE "ibm850".
                PUT STREAM email
                    c-empresa " - "
                    " PEDIDOS DE VENDA COM AVALIAÄ«O DE CRêDITO"
                    " - EXTRATO GERADO EM: " STRING(TODAY,"99/99/9999") FORMAT "x(10)" " " STRING(TIME,"HH:MM")
                    SKIP(1)
                    "Representante: " 
                    repres.cod-rep " - " 
                    repres.nome
                    " - e-mail: " repres.e-mail
                    SKIP(1)
                    "Pedido      Valor Total SitCrd Implant  Entrega  Aprovador    Motivo" SKIP
                    "------ ---------------- ------ -------- -------- ------------"         
                    " ----------------------------------------------------------------------"
                    SKIP.
             
                /* Atualiza temp-table com Representantes aos quais foram enviados e-mails */
                FIND tt-rep-email WHERE tt-rep-email.cod-rep = repres.cod-rep NO-LOCK NO-ERROR.
                IF NOT AVAIL tt-rep-email THEN DO:
                   CREATE tt-rep-email.
                   ASSIGN tt-rep-email.cod-rep = repres.cod-rep.
                END.
             END.
          END.
       END.
   
       IF FIRST-OF(ped-venda.nome-abrev) THEN DO:
          find emitente where emitente.cod-emitente = ped-venda.cod-emitente
                      no-lock no-error.
          IF AVAIL emitente THEN DO:
             IF emitente.natureza = 1 THEN 
                ASSIGN c-cgc = string(emitente.cgc,"999.999.999-99").
             ELSE
                IF emitente.natureza = 2 then
                   ASSIGN c-cgc = string(emitente.cgc,"99.999.999/9999-99").
                ELSE
                   ASSIGN c-cgc = "".

             assign c-endereco = emitente.endereco
                    c-bairro   = emitente.bairro
                    c-cidade   = emitente.cidade
                    c-estado   = emitente.estado
                    c-cep      = emitente.cep.
             if emitente.end-cobranca <> 0 and 
                emitente.end-cobranca <> emitente.cod-emit then do:
                find first b-emitente where b-emitente.cod-emit = emitente.end-cobranca
                                      no-lock no-error.

                assign c-endereco = b-emitente.endereco
                       c-bairro   = b-emitente.bairro
                       c-cidade   = b-emitente.cidade
                       c-estado   = b-emitente.estado
                       c-cep      = b-emitente.cep.
             END.
             ELSE DO:
                if emitente.endereco-cob <> "" then
                   assign c-endereco = emitente.endereco-cob
                          c-bairro   = emitente.bairro-cob
                          c-cidade   = emitente.cidade-cob
                          c-estado   = emitente.estado-cob
                          c-cep      = emitente.cep-cob.
             END.
   
             DISPLAY emitente.cod-emitente
                     emitente.nome-emit
                     c-cgc @ emitente.cgc
                     emitente.lim-credito
                     with frame f-cliente.
             DOWN(2) with frame f-cliente.

             IF tt-param.enviar-e-mail THEN DO:
                PUT STREAM email UNFORMAT
                    "Cliente: " AT 1
                    STRING(emitente.cod-emitente,"999999")
                    " " TRIM(emitente.nome-emit)
                    " " TRIM(c-endereco)
                    " " TRIM(c-bairro)
                    " " TRIM(c-cidade)
                    " " TRIM(c-estado)
                    SKIP(1).
             END.
          END.
       END.
   
       DISPLAY ped-venda.nr-pedcli
               ped-venda.vl-tot-ped
               c-situacao
               ped-venda.dt-implant
               ped-venda.dt-entrega
               ped-venda.desc-bloq-cr WHEN ped-venda.cod-sit-aval = 4
                   @ ped-venda.desc-bloq-cr
               ped-venda.desc-forc-cr WHEN ped-venda.cod-sit-aval = 3
                   @ ped-venda.desc-bloq-cr
               ped-venda.quem-aprovou
               WITH FRAME f-detalhe.
       DOWN WITH FRAME f-detalhe.
   
       IF tt-param.enviar-e-mail THEN DO:
          PUT STREAM email
              ped-venda.nr-pedcli  FORMAT "x(6)"             AT  1                
              ped-venda.vl-tot-ped FORMAT ">,>>>,>>>,>>9.99" AT  8
              c-situacao                                     AT 25       
              ped-venda.dt-implant FORMAT "99/99/99" AT 32
              ped-venda.dt-entrega FORMAT "99/99/99" AT 41
              ped-venda.quem-aprovou                 AT 50.
          IF ped-venda.cod-sit-aval = 4 THEN
             PUT STREAM email        
                 ped-venda.desc-bloq-cr              AT 63.
          IF ped-venda.cod-sit-aval = 3 THEN
             PUT STREAM email
                 ped-venda.desc-forc-cr              AT 63
              SKIP.
       END.

       IF LAST-OF(ped-venda.nome-abrev) THEN DO:
          DISPLAY c-linha WITH FRAME f-linha.
          DOWN WITH FRAME f-linha.
          
          IF tt-param.enviar-e-mail THEN
             PUT STREAM email
                 c-linha AT 1
                 SKIP.
       END.
   
       IF LAST-OF(ped-venda.no-ab-reppri) THEN DO:
          IF tt-param.enviar-e-mail THEN DO:
             FIND repres WHERE repres.nome-abrev = ped-venda.no-ab-reppri NO-LOCK NO-ERROR.
             IF repres.e-mail <> "" THEN DO:
                OUTPUT STREAM email CLOSE.
             
                ASSIGN c-destinatar = repres.e-mail + "," + "wellington.vale@teartextil.com.br," +
                                      "gisleno.ferreira@teartextil.com.br,teartextil@teartextil.com.br".
                RUN esapi/esapi002.p (INPUT "credito@teartextil.com.br", /* e-mail remetente */
                                      INPUT c-destinatar, /* e-mail destinat†rio */
                                      INPUT tt-param.assunto-e-mail, /* Assunto */
                                      INPUT tt-param.texto-e-mail, /* Mensagem */
                                      INPUT c-aux-e-mail, /* Arquivo Anexo */
                                      INPUT YES). /* Mostra Erros */
                ASSIGN i-e-mail-enviado = i-e-mail-enviado + 1.
             END.
             ELSE DO:
                OUTPUT STREAM email CLOSE.
                ASSIGN i-e-mail-nenviado = i-e-mail-nenviado + 1.
             END.
             OS-DELETE VALUE(c-aux-e-mail). 
          END.
       END.
   END.
END.
ELSE DO: /* Por Cliente/Pedido */
   FOR EACH ped-venda WHERE ped-venda.nome-abrev   >= tt-param.cliente-ini 
                        AND ped-venda.nome-abrev   <= tt-param.cliente-fin 
                        AND ped-venda.no-ab-reppri >= tt-param.repres-ini  
                        AND ped-venda.no-ab-reppri <= tt-param.repres-fin  
                        AND ped-venda.dt-implant   >= tt-param.dt-impl-ini 
                        AND ped-venda.dt-implant   <= tt-param.dt-impl-fin 
                        AND ped-venda.dt-entrega   >= tt-param.dt-entr-ini 
                        AND ped-venda.dt-entrega   <= tt-param.dt-entr-fin 
                        AND ((tt-param.cond-pagto  = 1 AND ped-venda.cod-cond-pag >= 1 AND ped-venda.cod-cond-pag <= 2) OR
                             (tt-param.cond-pagto  = 2 AND (ped-venda.cod-cond-pag = 0 OR ped-venda.cod-cond-pag > 3)) OR
                             (tt-param.cond-pagto  = 3))
                        AND ((ped-venda.cod-sit-ped = 1 AND tt-param.abertos) OR     
                             (ped-venda.cod-sit-ped = 2 AND tt-param.at-parcial) OR  
                             (ped-venda.cod-sit-ped = 3 AND tt-param.at-total) OR    
                             (ped-venda.cod-sit-ped = 4 AND tt-param.pendentes) OR   
                             (ped-venda.cod-sit-ped = 5 AND tt-param.suspensos) OR   
                             (ped-venda.cod-sit-ped = 6 AND tt-param.cancelados) OR  
                             (ped-venda.cod-sit-ped > 6 AND tt-param.outros))        
                        AND ((ped-venda.cod-sit-aval = 2 AND tt-param.avaliados) OR
                             (ped-venda.cod-sit-aval = 1 AND tt-param.n-avaliados) OR
                             (ped-venda.cod-sit-aval = 3 AND tt-param.aprovados) OR
                             (ped-venda.cod-sit-aval = 4 AND tt-param.n-aprovados) OR
                             (ped-venda.cod-sit-aval = 5 AND tt-param.pend-inform))
                      NO-LOCK
       BREAK BY ped-venda.nome-abrev
             BY ped-venda.nr-pedcli:

       RUN pi-acompanhar in h-acomp (input "Pedido: " + ped-venda.nr-pedcli).

       RUN pi-situacao.

       IF FIRST-OF(ped-venda.nome-abrev) THEN DO:
          find emitente where emitente.cod-emitente = ped-venda.cod-emitente
                      no-lock no-error.
          IF AVAIL emitente THEN DO:
             IF emitente.natureza = 1 THEN 
                ASSIGN c-cgc = string(emitente.cgc,"999.999.999-99").
             ELSE
             IF emitente.natureza = 2 then
                ASSIGN c-cgc = string(emitente.cgc,"99.999.999/9999-99").
             ELSE
                ASSIGN c-cgc = "".

             DISPLAY emitente.cod-emitente
                     emitente.nome-emit
                     c-cgc @ emitente.cgc
                     emitente.lim-credito
                     with frame f-cliente.
             DOWN(2) with frame f-cliente.
          END.
       END.

       DISPLAY ped-venda.nr-pedcli
               ped-venda.vl-tot-ped
               c-situacao
               ped-venda.dt-implant
               ped-venda.dt-entrega
               ped-venda.desc-bloq-cr WHEN ped-venda.cod-sit-aval = 4
                   @ ped-venda.desc-bloq-cr
               ped-venda.desc-forc-cr WHEN ped-venda.cod-sit-aval = 3
                   @ ped-venda.desc-bloq-cr
               ped-venda.quem-aprovou
               WITH FRAME f-detalhe.
       DOWN WITH FRAME f-detalhe.

       IF LAST-OF(ped-venda.nome-abrev) THEN DO:
          DISPLAY c-linha WITH FRAME f-linha.
          DOWN WITH FRAME f-linha.
       END.

   END.
END.

IF tt-param.impr-param THEN DO:
   PAGE.
   display tt-param.desc-classifica
           tt-param.cliente-ini    
           tt-param.cliente-fin    
           tt-param.repres-ini     
           tt-param.repres-fin     
           tt-param.dt-entr-ini 
           tt-param.dt-entr-fin 
           tt-param.dt-impl-ini 
           tt-param.dt-impl-fin 
           tt-param.desc-cond-pagto
           tt-param.abertos   
           tt-param.at-parcial
           tt-param.at-total  
           tt-param.pendentes 
           tt-param.suspensos 
           tt-param.cancelados
           tt-param.outros    
           tt-param.avaliados  
           tt-param.n-avaliados
           tt-param.aprovados  
           tt-param.n-aprovados
           tt-param.pend-inform
           tt-param.enviar-e-mail  
           i-e-mail-enviado        
           i-e-mail-nenviado       
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

PROCEDURE pi-situacao:
   CASE ped-venda.cod-sit-aval:
       WHEN 1 THEN
          ASSIGN c-situacao = "N/Aval".
       WHEN 2 THEN
          ASSIGN c-situacao = "Aval".
       WHEN 3 THEN
          ASSIGN c-situacao = "Aprv".
       WHEN 4 THEN
          ASSIGN c-situacao = "N/Aprv".
       OTHERWISE
          ASSIGN c-situacao = "Pend".
   END.
END PROCEDURE.
