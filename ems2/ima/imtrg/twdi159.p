/* /****************************************************************************  */
/* ** Programa: Twdi159 - Trigger de write na Tabela Ped-Venda                    */
/* ** Data    : Novembro de 2002                                                  */
/* ** Objetivo: Trigger de Write para a tabela Ped-Venda                          */
/* ** Empresa: IMA E INTERMALHAS                                                  */
/* ** Vers∆o:  2.04.001                                                           */
** Alterado: 16/01/2004 - FAL
** Fluxo:    Cadastrado na Oficial =>  Backup
*****************************************************************************/

DEFINE PARAMETER BUFFER b-ped-venda-new FOR ped-venda.
DEFINE PARAMETER BUFFER b-ped-venda-old FOR ped-venda.

{include/i-prgvrs.i twdi159 2.04.00.001}

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

DEF VAR l-ok AS LOG.


DEFINE VARIABLE h-registro-old  AS HANDLE     NO-UNDO.
DEFINE VARIABLE h-registro-new  AS HANDLE     NO-UNDO.
DEFINE VARIABLE h-campo-old     AS HANDLE     NO-UNDO.
DEFINE VARIABLE h-campo-new     AS HANDLE     NO-UNDO.
DEFINE VARIABLE c-campos        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE i-cont          AS INTEGER    NO-UNDO.
DEF VAR c-ocorrencia AS CHAR    FORMAT "x(500)".
DEF VAR c-arquivo AS CHAR.
ASSIGN c-arquivo = "P:\Log_PedVenda\ocorrencias-ped-venda-" + STRING(DAY(TODAY),"99") + STRING(MONTH(TODAY),"99") + ".txt".

      
IF NOT NEW b-ped-venda-new THEN DO.
   IF b-ped-venda-old.cod-portador <> 0 AND
      b-ped-venda-old.cod-portador <> b-ped-venda-new.cod-portador THEN DO:
      OUTPUT TO VALUE(c-arquivo) APPEND.

      ASSIGN c-ocorrencia = "Alterado Portador " + 
                            "De: " + STRING(b-ped-venda-old.cod-portador) + " "  +
                            "Para: " + STRING(b-ped-venda-new.cod-portador).

      PUT UNFORMATTED 
          b-ped-venda-new.nr-pedcli " " 
          b-ped-venda-new.nome-abrev " " 
          c-seg-usuario " " 
          STRING(TODAY,"99/99/9999") " " 
          STRING(TIME,"HH:MM:SS") " " 
          PROGRAM-NAME(1) " "
          PROGRAM-NAME(2) " "
          PROGRAM-NAME(3) " "
          PROGRAM-NAME(4) " "
          PROGRAM-NAME(5) " "
          PROGRAM-NAME(6) " "
          PROGRAM-NAME(7) " "
          PROGRAM-NAME(8) " "
          PROGRAM-NAME(9) " "
          c-ocorrencia
          SKIP.


      /*
      //BUFFER-COMPARE b-ped-venda-new TO b-ped-venda-old SAVE RESULT IN c-campos NO-ERROR.
      //IF c-campos <> "" THEN DO:
      
          ASSIGN h-registro-new = BUFFER b-ped-venda-new:HANDLE
                 h-registro-old = BUFFER b-ped-venda-old:HANDLE.
           
          DO i-cont = 1 TO NUM-ENTRIES(c-campos).
             ASSIGN h-campo-old = h-registro-old:BUFFER-FIELD(ENTRY(i-cont,c-campos))
                    h-campo-new = h-registro-new:BUFFER-FIELD(ENTRY(i-cont,c-campos)).
    
             ASSIGN c-ocorrencia = "Alterado o Campo: " + ENTRY(i-cont,c-campos) + " " + 
                                   "De: " + TRIM(h-campo-old:STRING-VALUE) + " "  +
                                   "Para: " + TRIM(h-campo-new:STRING-VALUE).
    
             PUT UNFORMATTED 
                 b-ped-venda-new.nr-pedcli " " 
                 b-ped-venda-new.nome-abrev " " 
                 c-seg-usuario " " 
                 STRING(TODAY,"99/99/9999") " " 
                 STRING(TIME,"HH:MM:SS") " " 
                 PROGRAM-NAME(1) " "
                 PROGRAM-NAME(2) " "
                 PROGRAM-NAME(3) " "
                 PROGRAM-NAME(4) " "
                 PROGRAM-NAME(5) " "
                 PROGRAM-NAME(6) " "
                 PROGRAM-NAME(7) " "
                 PROGRAM-NAME(8) " "
                 PROGRAM-NAME(9) " "
                 c-ocorrencia
                 SKIP.
          END.
      END.    
      */
      OUTPUT CLOSE.
   END.
END.

/*
IF b-ped-venda-new.completo = NO AND b-ped-venda-old.completo = YES THEN DO:
    ASSIGN c-ocorrencia = 'Pedido Completo mudou para incompleto->' + PROGRAM-NAME(1) + '->' + PROGRAM-NAME(2)  + '->' + PROGRAM-NAME(3) +  '->' + PROGRAM-NAME(4)
        + '->' + PROGRAM-NAME(5) + '->' + PROGRAM-NAME(6).
    PUT UNFORMATTED 
             b-ped-venda-new.nr-pedcli " " 
             b-ped-venda-new.nome-abrev " " 
             c-seg-usuario " " 
             STRING(TODAY,"99/99/9999") " " 
             STRING(TIME,"HH:MM:SS") " " 
             c-ocorrencia
             SKIP. 
   

END.

IF NOT NEW b-ped-venda-new AND 
   b-ped-venda-new.dt-entrega <> b-ped-venda-old.dt-entrega THEN DO.
   RUN esapi/cria-log-pedvenda.p (INPUT b-ped-venda-new.nr-pedcli,
                                  INPUT b-ped-venda-new.nome-abrev,
                                  INPUT "Alterado Data de Entrega (twdi159), para: " + STRING(b-ped-venda-new.dt-entrega),
                                   INPUT NO).
END.*/

/* Se Ç um novo Pedido ou 
   Se Alterou a Condiá∆o de Pagamento, Volta Pedido para Aprovaá∆o de Preáo */
/*IF NEW b-ped-venda-new OR
   b-ped-venda-new.cod-cond-pag <> b-ped-venda-old.cod-cond-pag THEN DO.

   IF b-ped-venda-new.tp-preco = 1 THEN
      RUN esapi/valida-preco.p (INPUT b-ped-venda-new.nr-pedcli,
                                OUTPUT l-ok).
END.*/

/*
/* 17/12/2013 - M†rcio, Este programa envia o pedido por e-mail */
IF NEW b-ped-venda-new THEN DO:
    FIND pp-ped-venda WHERE
         pp-ped-venda.nr-pedcli  = b-ped-venda-new.nr-pedcli AND
         pp-ped-venda.nome-abrev = b-ped-venda-new.nome-abrev
         NO-LOCK NO-ERROR.
    IF NOT AVAIL pp-ped-venda THEN
       RUN esrp\essp0155rp.p (INPUT ROWID(b-ped-venda-new)).
END.
*/

/*
COMENTADO POR TONINHO EM 10/06/2015

/* OS CODIGOS ABAIXO DEVER«O SER VALIDADOS, PARA VER SE AINDA S«O UTILIZADOS    */
DEF NEW GLOBAL SHARED VAR gc-nome-abrev  LIKE ped-venda.nome-abrev   NO-UNDO.
DEF NEW GLOBAL SHARED VAR gc-nr-pedcli   LIKE ped-venda.nr-pedcli    NO-UNDO.
DEF NEW GLOBAL SHARED VAR gc-cod-estabel LIKE ped-venda.cod-estabel  NO-UNDO.
DEF NEW GLOBAL SHARED VAR c-seg-usuario LIKE usuar_mestre.cod_usuario NO-UNDO.

DEF BUFFER b-IM-ITEM-PED-VENDA-PP FOR IM-ITEM-PED-VENDA-PP.

/* MESSAGE "Estou na trigger write 159" VIEW-AS ALERT-BOX.  */

/** verifica se o programa que estaÔ chamando esta trigger eÔ o PD4000 **/
def var j as int no-undo.
def var c-nome-programa as char no-undo.
    
assign j = 2
       c-nome-programa = "".
DO WHILE PROGRAM-NAME(j) <> ?:

   if program-name(j) matches "*pdp/pd4000*" then
      assign c-nome-programa = program-name(j).
    
   j = j + 1.
END.


ASSIGN gc-nome-abrev =  b-ped-venda-new.nome-abrev
       gc-nr-pedcli  =  b-ped-venda-new.nr-pedcli.

DEF VAR da-data AS DATE FORMAT "99/99/9999".
DEF VAR c-hora AS CHAR.
DEF VAR l-error AS LOGICAL INIT NO.

DEF VAR l-permissao AS LOGICAL NO-UNDO.

ASSIGN l-permissao = YES.

RUN pi-verifica-permissao (OUTPUT l-permissao).


/* message " b-ped-venda-new.cod-sit-ped"  b-ped-venda-new.cod-sit-ped view-as alert-box. */



/* Verifica se Ç uma alteraá∆o */
IF AVAIL b-ped-venda-old THEN DO:
    /* Verificar se o pedido foi efetivado para verdadeiro */
    IF b-ped-venda-old.completo = NO AND
       b-ped-venda-new.completo = YES THEN
       ASSIGN da-data = b-ped-venda-new.dt-implant
              c-hora = STRING(TIME,"HH:MM:SS")
              OVERLAY (b-ped-venda-new.char-1,70,10) = STRING(da-data,"99/99/9999")
              OVERLAY (b-ped-venda-new.char-1,85,8) = c-hora.
    
    /* Verifica se o pedido foi aprovado */
    IF b-ped-venda-new.cod-sit-aval = 3 AND 
       SUBSTRING(b-ped-venda-new.char-1,95,1) = "" THEN
       ASSIGN OVERLAY(b-ped-venda-new.char-1,95,1) = "*".

    IF b-ped-venda-old.tp-pedido <> 
       b-ped-venda-new.tp-pedido THEN ASSIGN  b-ped-venda-new.completo = NO.
END.
ELSE DO:
     IF b-ped-venda-new.completo = YES THEN DO:
        ASSIGN da-data = b-ped-venda-new.dt-implant
                         c-hora = STRING(TIME,"HH:MM:SS").
        ASSIGN OVERLAY (b-ped-venda-new.char-1,70,10) = STRING(da-data,"99/99/9999")
               OVERLAY (b-ped-venda-new.char-1,85,8) = c-hora.
     END.
END.

IF l-permissao = NO THEN
DO:
    MESSAGE  'Vocà n∆o tem permiss∆o para alterar este pedido. ' + chr(10) +
             'Cliente : ' + gc-nome-abrev + ' / ' + ' Pedido: ' + gc-nr-pedcli + CHR(10) + 
             'Solicite ao departamento de Vendas.'
            VIEW-AS ALERT-BOX ERROR TITLE 'Usu†rio sem permiss∆o para Alteraá∆o!' .

    ASSIGN l-error = YES.
END.

IF l-error = NO THEN DO:
    IF c-nome-programa <> "" /*Faz este tratamento so pelo programa PD4000*/ THEN DO:
       FIND FIRST im-ext-ped-venda OF b-ped-venda-new 
                  SHARE-LOCK NO-ERROR.
       IF NOT AVAIL im-ext-ped-venda THEN DO:
          CREATE im-ext-ped-venda.
          ASSIGN im-ext-ped-venda.nr-pedido       = b-ped-venda-new.nr-pedido
                 im-ext-ped-venda.cod-tipo-ped    = b-ped-venda-new.tp-pedido
                 im-ext-ped-venda.Dt-Implant      = b-ped-venda-new.dt-implant
                 im-ext-ped-venda.Log-transferido = NO.
       END.
       ELSE 
          ASSIGN im-ext-ped-venda.cod-tipo-ped = b-ped-venda-new.tp-pedido.
    
       if b-ped-venda-new.tp-pedido = "PI" then do:
          if b-ped-venda-new.cod-sit-ped = 5 /*Suspenso*/ or
             b-ped-venda-new.cod-sit-ped = 6 /*Cancelado*/ then do:
        
             /***Cancelamento ou Suspensao do Pedido***/
             FOR EACH ped-item WHERE 
                      ped-item.nome-abrev = b-ped-venda-new.nome-abrev and
                      ped-item.nr-pedcli  = b-ped-venda-new.nr-pedcli NO-LOCK:
            
                 FOR EACH IM-ITEM-VINC WHERE 
                          IM-ITEM-VINC.Num-Pedido-Venda = ped-item.nr-pedcli AND
                          IM-ITEM-VINC.Nome-Abrev-V     = ped-item.nome-abrev AND
                          IM-ITEM-VINC.It-Codigo        = ped-item.it-codigo AND
                          IM-ITEM-VINC.Cod-Refer        = ped-item.cod-refer
                          SHARE-LOCK:

                     FIND FIRST im-item-ped-compra-pi WHERE 
                                im-item-ped-compra-pi.num-pedido-compra = im-item-vinc.num-pedido-compra AND
                                im-item-ped-compra-pi.nome-abrev        = IM-ITEM-VINC.Nome-Abrev        AND
                                im-item-ped-compra-pi.it-codigo         = im-item-vinc.it-codigo         AND
                                im-item-ped-compra-pi.cod-refer         = im-item-vinc.cod-refer 
                                NO-LOCK NO-ERROR.
                     IF AVAIL im-item-ped-compra-pi THEN DO:
                        ASSIGN IM-ITEM-PED-COMPRA-PI.Qtd-Solic-Disponivel    = IM-ITEM-PED-COMPRA-PI.Qtd-Solic-Disponivel + IM-ITEM-VINC.Qtd-Solicitada
                               IM-ITEM-PED-COMPRA-PI.Qtd-Recebida-Disponivel = IM-ITEM-PED-COMPRA-PI.Qtd-Recebida-Disponivel + IM-ITEM-VINC.Qtd-Reservada.

                        if IM-ITEM-PED-COMPRA-PI.Qtd-Reservada-Venda > 0 and IM-ITEM-VINC.Qtd-Reservada > 0 then
                           ASSIGN IM-ITEM-PED-COMPRA-PI.Qtd-Reservada-Venda = IM-ITEM-PED-COMPRA-PI.Qtd-Reservada-Venda - IM-ITEM-VINC.Qtd-Reservada.
                     END.
                     DELETE IM-ITEM-VINC.
                 END.
             END.
             /************************************************/
          end.
        
          if b-ped-venda-old.cod-sit-ped = 5 and b-ped-venda-new.cod-sit-ped <> 5 /*Reativa o pedido*/ then do:
             /***Reativacao do Pedido***/
             DEF VAR de-qtd-disponivel       AS DEC FORMAT "9,999,999.9999" INIT 0.
             DEF VAR de-qtd-recebida         AS DEC FORMAT "9,999,999.9999" INIT 0.
             DEF VAR de-qtd-resto-disponivel AS DEC FORMAT "9,999,999.9999" INIT 0.
            
             DEF TEMP-TABLE tt-vinc
                 FIELD Sequencia         AS INT INIT 0                                   
                 FIELD Num-Pedido-Compra LIKE IM-ITEM-PED-COMPRA-PI.Num-Pedido-Compra
                 FIELD Nome-Abrev        LIKE IM-ITEM-PED-COMPRA-PI.Nome-Abrev       
                 FIELD Cod-Emitente      LIKE IM-ITEM-PED-COMPRA-PI.Cod-Emitente     
                 FIELD It-Codigo         AS CHAR FORMAT "X(16)"                LABEL "it-codigo"
                 FIELD cod-refer         AS CHAR FORMAT "X(8)"                 LABEL "Cod-refer"
                 FIELD Num-Pedido-Venda  AS CHAR FORMAT "X(12)"                                        
                 FIELD Nome-Abrev-V      AS CHAR FORMAT "X(14)"                LABEL "nome-abrev-V"
                 FIELD Cod-Emitente-V    LIKE ped-venda.cod-emitente           LABEL "Cod-emit-V"
                 FIELD Dt-Implant        AS DATE FORMAT "99/99/9999"
                 FIELD Qtd-Solicitada    AS DEC FORMAT "9,999,999.9999" INIT 0 LABEL "Qtd-Solic"
                 FIELD Qtd-Reservada     AS DEC FORMAT "9,999,999.9999" INIT 0 LABEL "Qtd-Reserv"
                 FIELD qtd-disponivel-orig AS DEC FORMAT "9,999,999.9999" INIT 0
                 FIELD qtd-recebida-orig   AS DEC FORMAT "9,999,999.9999" INIT 0
                 FIELD qtd-disponivel-max AS DEC FORMAT "9,999,999.9999" INIT 0 
                 FIELD qtd-recebida-max   AS DEC FORMAT "9,999,999.9999" INIT 0.
           
             DEF TEMP-TABLE tt-IM-ITEM-PED-COMPRA-PI
                 FIELD seq                   AS INT
                 FIELD Num-Pedido-Compra     LIKE IM-ITEM-PED-COMPRA-PI.Num-Pedido-Compra
                 FIELD Nome-Abrev            LIKE IM-ITEM-PED-COMPRA-PI.Nome-Abrev       
                 FIELD Cod-Emitente          LIKE IM-ITEM-PED-COMPRA-PI.Cod-Emitente     
                 FIELD It-Codigo             LIKE IM-ITEM-PED-COMPRA-PI.It-Codigo        
                 FIELD cod-refer             LIKE IM-ITEM-PED-COMPRA-PI.cod-refer
                 FIELD qtd-disponivel        LIKE im-item-ped-compra-pi.Qtd-Solic-Disponivel
                 FIELD qtd-recebida          LIKE im-item-ped-compra-pi.Qtd-recebida-estoque
                 FIELD qtd-disponivel-orig AS DEC FORMAT "9,999,999.9999" INIT 0
                 FIELD qtd-recebida-orig   AS DEC FORMAT "9,999,999.9999" INIT 0
                 FIELD qtd-disponivel-max AS DEC FORMAT "9,999,999.9999" INIT 0 
                 FIELD qtd-recebida-max   AS DEC FORMAT "9,999,999.9999" INIT 0.
            
             FOR EACH tt-vinc.
                 DELETE tt-vinc.
             END.
            
             FOR EACH tt-IM-ITEM-PED-COMPRA-PI.
                 DELETE tt-IM-ITEM-PED-COMPRA-PI.
             END.
        
             FOR EACH ped-item WHERE 
                      ped-item.nome-abrev = b-ped-venda-new.nome-abrev and
                      ped-item.nr-pedcli  = b-ped-venda-new.nr-pedcli NO-LOCK:
        
                 IF CAN-FIND (FIRST IM-ITEM-PED-COMPRA-PI WHERE 
                                    IM-ITEM-PED-COMPRA-PI.It-Codigo = ped-item.it-codigo AND
                                    IM-ITEM-PED-COMPRA-PI.cod-refer = ped-item.cod-refer NO-LOCK) THEN DO:
        
                    ASSIGN de-qtd-disponivel = 0
                           de-qtd-recebida   = 0.
        
                    FOR EACH IM-ITEM-PED-COMPRA-PI NO-LOCK WHERE 
                             IM-ITEM-PED-COMPRA-PI.It-Codigo = ped-item.it-codigo AND
                             IM-ITEM-PED-COMPRA-PI.cod-refer = ped-item.cod-refer,
                        FIRST IM-PED-COMPRA-PI NO-LOCK WHERE 
                              IM-PED-COMPRA-PI.Num-Pedido-Compra = IM-ITEM-PED-COMPRA-PI.Num-Pedido-Compra AND
                              IM-PED-COMPRA-PI.Nome-Abrev        = IM-ITEM-PED-COMPRA-PI.Nome-Abrev        AND
                              IM-PED-COMPRA-PI.Cod-Emitente      = IM-ITEM-PED-COMPRA-PI.Cod-Emitente      BY IM-PED-COMPRA-PI.Dt-Emissao-Pedido:
        
                        IF IM-ITEM-PED-COMPRA-PI.Qtd-Solic-Disponivel > 0 THEN DO:
                           CREATE tt-IM-ITEM-PED-COMPRA-PI.
                           ASSIGN tt-IM-ITEM-PED-COMPRA-PI.seq               = IM-ITEM-PED-COMPRA-PI.Sequencia 
                                  tt-IM-ITEM-PED-COMPRA-PI.Num-Pedido-Compra = IM-ITEM-PED-COMPRA-PI.Num-Pedido-Compra
                                  tt-IM-ITEM-PED-COMPRA-PI.Nome-Abrev        = IM-ITEM-PED-COMPRA-PI.Nome-Abrev
                                  tt-IM-ITEM-PED-COMPRA-PI.Cod-Emitente      = IM-ITEM-PED-COMPRA-PI.Cod-Emitente
                                  tt-IM-ITEM-PED-COMPRA-PI.It-Codigo         = IM-ITEM-PED-COMPRA-PI.It-Codigo
                                  tt-IM-ITEM-PED-COMPRA-PI.cod-refer         = IM-ITEM-PED-COMPRA-PI.cod-refer
                                  tt-IM-ITEM-PED-COMPRA-PI.qtd-disponivel    = IM-ITEM-PED-COMPRA-PI.Qtd-Solic-Disponivel
                                  tt-im-item-ped-compra-pi.qtd-recebida      = IM-ITEM-PED-COMPRA-PI.Qtd-recebida-estoque.
                           ASSIGN de-qtd-disponivel = de-qtd-disponivel + IM-ITEM-PED-COMPRA-PI.qtd-solic-disponivel
                                  de-qtd-recebida   = de-qtd-recebida + IM-ITEM-PED-COMPRA-PI.Qtd-recebida-estoque.
        
                           ASSIGN tt-IM-ITEM-PED-COMPRA-PI.qtd-disponivel-max = IM-ITEM-PED-COMPRA-PI.Qtd-Solicitada
                                  tt-IM-ITEM-PED-COMPRA-PI.qtd-recebida-max   = IM-ITEM-PED-COMPRA-PI.Qtd-Recebida-estoque.
        
                           IF de-qtd-disponivel >= ped-item.qt-pedida THEN
                              LEAVE.
                        END.
                    END.
        
                    IF ped-item.qt-pedida > de-qtd-disponivel THEN DO: /*A quantidade pedida para o °tem no ped. venda n∆o possui qtd suficiente em ped. compras para avalizar.*/
                       MESSAGE "A quantidade " ped-item.qt-pedida " pedida no item " ped-item.it-codigo " n∆o tem saldo o suficiente." SKIP
                               "Solicitar novo pedido de compras para o item." VIEW-AS ALERT-BOX INFO.
                       RETURN NO-APPLY.
                    END.
                    ELSE DO:
                       ASSIGN de-qtd-resto-disponivel = ped-item.qt-pedida.
        
                       FOR EACH tt-IM-ITEM-PED-COMPRA-PI NO-LOCK BREAK BY tt-IM-ITEM-PED-COMPRA-PI.seq:
                           CREATE tt-vinc.
                           ASSIGN tt-vinc.Sequencia         = tt-IM-ITEM-PED-COMPRA-PI.seq
                                  tt-vinc.Num-Pedido-Compra = tt-IM-ITEM-PED-COMPRA-PI.Num-Pedido-Compra
                                  tt-vinc.Nome-Abrev        = tt-IM-ITEM-PED-COMPRA-PI.Nome-Abrev       
                                  tt-vinc.Cod-Emitente      = tt-IM-ITEM-PED-COMPRA-PI.Cod-Emitente     
                                  tt-vinc.It-Codigo         = ped-item.it-codigo
                                  tt-vinc.cod-refer         = ped-item.cod-refer
                                  tt-vinc.Num-Pedido-Venda  = ped-item.nr-pedcli
                                  tt-vinc.Nome-Abrev-V      = ped-item.nome-abrev
                                  tt-vinc.Cod-Emitente-V    = b-ped-venda-new.cod-emitente
                                  tt-vinc.Dt-Implant        = b-ped-venda-new.dt-implant.
        
                           IF tt-IM-ITEM-PED-COMPRA-PI.qtd-disponivel < de-qtd-resto-disponivel THEN /*Se a quantidade do item no ped. compra for menor que a quant. pedida*/
                              ASSIGN tt-vinc.Qtd-Solicitada = tt-IM-ITEM-PED-COMPRA-PI.qtd-disponivel. /*Reserva toda a quantidade disponivel do item que se encontra no ped. compra*/
                           ELSE
                              ASSIGN tt-vinc.Qtd-Solicitada = de-qtd-resto-disponivel. /*Reserva o resto da quantidade pedida no ped. venda, que ser† retirada deste pedido de compra. Reserva s¢ a sobra pois este Ç o £ltimo p.compra (o que consegue fechar o ped.venda).*/
        
                           ASSIGN de-qtd-resto-disponivel = de-qtd-resto-disponivel - tt-IM-ITEM-PED-COMPRA-PI.qtd-disponivel. /*Retira a quantidade reservada do ped. compra da quant. pedida, para verificar se restou ainda qtd. pedida para ser retirada de outro ped. compra.*/
        
                           IF de-qtd-resto-disponivel > 0 THEN /*Se for maior do que Zero, a qtd-reservada ser† igual a qtd-recebida em estoque*/
                              ASSIGN tt-vinc.Qtd-Reservada = tt-IM-ITEM-PED-COMPRA-PI.qtd-recebida. /*Reserva toda a quantidade disponivel do item que se encontra no ped. compra*/
                           ELSE DO: /*Sen∆o, a qtd-reservada ser† o resto da quantidade solicitada, pois Ç o £ltimo registro de ped. compra para gravar */
                              IF tt-vinc.Qtd-Solicitada > tt-IM-ITEM-PED-COMPRA-PI.qtd-recebida-max THEN
                                 ASSIGN tt-vinc.Qtd-Reservada = tt-IM-ITEM-PED-COMPRA-PI.qtd-recebida-max. /*Reserva toda a quantidade pedida no ped. venda, que ser† retirada deste pedido de compra*/
                              ELSE
                                 ASSIGN tt-vinc.Qtd-Reservada = tt-vinc.Qtd-Solicitada. /*Reserva toda a quantidade pedida no ped. venda, que ser† retirada deste pedido de compra*/
                              LEAVE.
                           END.
                       END.
                    END.
                 END.
                 ELSE DO:
                    MESSAGE "N∆o existe pedido de compra para vincular ao pedido de venda. Criar pedido de compras com saldo para o item." VIEW-AS ALERT-BOX INFO.
                    RETURN NO-APPLY.
                 END.
        
                 /*Criando V°nculo do pedido de venda com pedido de compra*/
                 CREATE  im-item-vinc.
                 ASSIGN  IM-ITEM-VINC.Num-Pedido-Compra = tt-vinc.Num-Pedido-Compra
                         IM-ITEM-VINC.Nome-Abrev        = tt-vinc.Nome-Abrev
                         IM-ITEM-VINC.Cod-Emitente      = tt-vinc.Cod-Emitente
                         IM-ITEM-VINC.It-Codigo         = tt-vinc.It-Codigo
                         IM-ITEM-VINC.Cod-Refer         = tt-vinc.cod-refer
                         IM-ITEM-VINC.Sequencia         = tt-vinc.Sequencia
                         IM-ITEM-VINC.Num-Pedido-Venda  = tt-vinc.Num-Pedido-Venda
                         IM-ITEM-VINC.Nome-Abrev-V      = tt-vinc.Nome-Abrev-V
                         IM-ITEM-VINC.Cod-Emitente-V    = tt-vinc.Cod-Emitente-V
                         IM-ITEM-VINC.Dt-Implant        = tt-vinc.Dt-Implant
                         IM-ITEM-VINC.Qtd-Solicitada    = tt-vinc.Qtd-Solicitada
                         IM-ITEM-VINC.Qtd-Reservada     = tt-vinc.Qtd-Reservada.
        
                 /*Atualizando os pedidos de compra que tiveram baixa atravÇs do pedido de venda.*/
                 FIND FIRST IM-ITEM-PED-COMPRA-PI WHERE 
                            IM-ITEM-PED-COMPRA-PI.Num-Pedido-Compra = tt-vinc.Num-Pedido-Compra AND
                            IM-ITEM-PED-COMPRA-PI.Nome-Abrev        = tt-vinc.Nome-Abrev        AND
                            IM-ITEM-PED-COMPRA-PI.Cod-Emitente      = tt-vinc.Cod-Emitente      AND
                            IM-ITEM-PED-COMPRA-PI.It-Codigo         = tt-vinc.It-Codigo         AND
                            IM-ITEM-PED-COMPRA-PI.Cod-Refer         = tt-vinc.cod-refer        
                            NO-LOCK NO-ERROR.
                 IF AVAIL IM-ITEM-PED-COMPRA-PI THEN DO:
                    ASSIGN IM-ITEM-PED-COMPRA-PI.Qtd-Solic-Disponivel    = IF IM-ITEM-PED-COMPRA-PI.Qtd-Solic-Disponivel - tt-vinc.Qtd-Solicitada > 0 THEN IM-ITEM-PED-COMPRA-PI.Qtd-Solic-Disponivel - tt-vinc.Qtd-Solicitada ELSE 0.

                    if im-item-vinc.qtd-reservada > 0 then
                       ASSIGN IM-ITEM-PED-COMPRA-PI.Qtd-Reservada-Venda     = IM-ITEM-VINC.Qtd-Reservada
                                    IM-ITEM-PED-COMPRA-PI.Qtd-Recebida-Disponivel = IM-ITEM-PED-COMPRA-PI.Qtd-Recebida-Estoque - IM-ITEM-VINC.Qtd-Reservada.
                 END.
             END.
             /*************************************/
          end.
       end.
    
       ELSE if b-ped-venda-new.tp-pedido = "PP" then do:
    
          IF b-ped-venda-new.cod-sit-ped = 5 /*Suspenso*/ OR 
             b-ped-venda-new.cod-sit-ped = 6 /*Cancelado*/ then do:
        
             /***Cancelamento ou Suspensao do Pedido***/
             FOR EACH ped-item WHERE 
                      ped-item.nome-abrev = b-ped-venda-new.nome-abrev and
                      ped-item.nr-pedcli  = b-ped-venda-new.nr-pedcli NO-LOCK:
            
                 FIND FIRST IM-ITEM-PED-VENDA-PP WHERE 
                            IM-ITEM-PED-VENDA-PP.Num-Pedido-Venda = ped-item.nr-pedcli AND
                            IM-ITEM-PED-VENDA-PP.Nome-Abrev       = ped-item.nome-abrev AND
                            IM-ITEM-PED-VENDA-PP.It-Codigo        = ped-item.it-codigo AND
                            IM-ITEM-PED-VENDA-PP.Cod-Refer        = ped-item.cod-refer
                            NO-LOCK NO-ERROR.
                 IF AVAIL IM-ITEM-PED-VENDA-PP THEN
                    DELETE IM-ITEM-PED-VENDA-PP.
             END.
             /************************************************/
          END.
        
          IF b-ped-venda-old.cod-sit-ped = 5 AND 
             b-ped-venda-new.cod-sit-ped <> 5 /*Reativa o pedido*/ then do:
             /***Reativacao do Pedido***/
    
             FIND FIRST ped-item WHERE 
                        ped-item.nome-abrev = b-ped-venda-new.nome-abrev AND
                        ped-item.nr-pedcli  = b-ped-venda-new.nr-pedcli NO-LOCK NO-ERROR.
    
             FIND FIRST b-IM-ITEM-PED-VENDA-PP WHERE 
                        b-IM-ITEM-PED-VENDA-PP.Num-Pedido-Venda = ped-item.nr-pedcli AND
                        b-IM-ITEM-PED-VENDA-PP.Nome-Abrev       = ped-item.nome-abrev AND
                        b-IM-ITEM-PED-VENDA-PP.It-Codigo        = ped-item.it-codigo AND
                        b-IM-ITEM-PED-VENDA-PP.Cod-Refer        = ped-item.cod-refer
                        SHARE-LOCK NO-ERROR.
    
             IF NOT AVAIL b-im-item-ped-venda-pp THEN DO:
                find first IM-TIPO-PED where 
                           IM-TIPO-PED.Cod-Tipo-Ped = "PP" no-lock no-error.
            
                CREATE im-item-ped-venda-pp.
                ASSIGN im-item-ped-venda-pp.It-Codigo          = ped-item.It-Codigo
                       im-item-ped-venda-pp.Cod-Refer          = ped-item.cod-refer
                       im-item-ped-venda-pp.Num-Pedido-Venda   = ped-item.nr-Pedcli
                       im-item-ped-venda-pp.Nome-Abrev         = ped-item.Nome-Abrev
                       im-item-ped-venda-pp.Cod-Emitente       = b-ped-venda-new.Cod-Emitente
                       im-item-ped-venda-pp.Dt-Implant         = b-ped-venda-new.dt-emissao
                       im-item-ped-venda-pp.Qtd-Solicitada     = ped-item.qt-pedida
                       im-item-ped-venda-pp.Qtd-Reservada      = ped-item.qt-pedida
                       im-item-ped-venda-pp.Cod-Estabel-Estoq  = IM-TIPO-PED.cod-estabel
                       im-item-ped-venda-pp.Cod-Deposito-Estoq = IM-TIPO-PED.cod-depos
                       im-item-ped-venda-pp.Cod-Local-Estoq    = IM-TIPO-PED.cod-localiz.
             END.
             /*************************************/
          end.
       end.
       
       RETURN 'OK'.
    END.
END.
ELSE
   RETURN 'Nok'.

/*  Fim da UPC */ 


PROCEDURE pi-verifica-permissao.
    DEF VAR l-grupo-ven                     AS LOG.
    DEF OUTPUT PARAM l-pode-modificar       AS LOG.

    ASSIGN l-pode-modificar = YES.

    DEFINE BUFFER b-ped-venda FOR ped-venda.

    FIND FIRST b-ped-venda WHERE 
               b-ped-venda.nome-abrev = gc-nome-abrev AND
               b-ped-venda.nr-pedcli  = gc-nr-pedcli  NO-LOCK NO-ERROR.

    IF AVAIL b-ped-venda AND b-ped-venda.tp-preco = 1 /* Preco Informado */ THEN DO:
        /******************************************************************************
        **  Objetivo.: Programa UPC do pd4000 - Verifica a seguranáa de pedidos
        *******************************************************************************/
        ASSIGN l-grupo-ven = NO.
        FOR EACH usuar_grp_usuar WHERE usuar_grp_usuar.cod_usuario = c-seg-usuario NO-LOCK.
            IF usuar_grp_usuar.cod_grp_usuar = "PED" THEN    
            DO:
                ASSIGN l-grupo-ven = YES. 
                LEAVE.
            END.
        END.

        IF l-grupo-ven = YES THEN 
            ASSIGN l-pode-modificar = YES.
            ELSE   l-pode-modificar = NO.

        /******************************************************************************
        **  Objetivo.: Programa UPC do pd4000 - Verifica a seguranáa de pedidos
        *******************************************************************************/
    END.  /* AVAIL b-ped-venda */
END PROCEDURE.

FIM COMENTARIO DO TONINHO 10/06/2015  
*/
