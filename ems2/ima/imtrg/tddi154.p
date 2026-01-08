/*****************************************************************************
**
**     Programa: TDDI154.P
**     Data....: Outubro DE 2002
**     Objetivo: Deleta o item na tabela de aloca‡äes do pedidos 
**
*****************************************************************************/

DEFINE PARAMETER BUFFER b-ped-item FOR ped-item.

DEFINE NEW GLOBAL SHARED VAR r-rowid-ped-item-del AS ROWID NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR r-rowid-ped-item     AS ROWID NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR r-rowid-ped-item-old AS ROWID NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR de-qtd-del AS DEC NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR de-qtd-write-old AS DEC NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR de-qtd-write-new AS DEC NO-UNDO.
DEF NEW GLOBAL SHARED VAR lg-add-modifica   AS LOGICAL.

DEF BUFFER b-ped-venda FOR ped-venda.
def buffer b-IM-ITEM-VINC for IM-ITEM-VINC.

define var somatorio like IM-ITEM-VINC.Qtd-Solicitada.

    {include/i-prgvrs.i TDDI154 2.04.00.001}

/* MESSAGE "tddi154-------hoje" VIEW-AS ALERT-BOX. */


ASSIGN r-rowid-ped-item-del = ROWID(b-ped-item)
       r-rowid-ped-item     = ?
       r-rowid-ped-item-old = ?
       de-qtd-del           = b-ped-item.qt-pedida
       de-qtd-write-old     = ?
       de-qtd-write-new     = ?.

FIND FIRST ped-venda 
     WHERE ped-venda.nr-pedcli  = b-ped-item.nr-pedcli
       AND ped-venda.nome-abrev = b-ped-item.nome-abrev NO-LOCK NO-ERROR.
IF AVAIL ped-venda 
THEN DO:
    FIND FIRST ima-ped-item
         WHERE ima-ped-item.cod-estabel  = ped-venda.cod-estabel
           AND ima-ped-item.nome-abrev   = b-ped-item.nome-abrev
           AND ima-ped-item.nr-pedcli    = b-ped-item.nr-pedcli
           AND ima-ped-item.nr-sequencia = b-ped-item.nr-sequencia
           AND ima-ped-item.it-codigo    = b-ped-item.it-codigo
           AND ima-ped-item.cod-refer    = b-ped-item.cod-refer SHARE-LOCK  NO-ERROR.
    IF AVAIL ima-ped-item
    THEN DELETE ima-ped-item NO-ERROR.
END.

IF AVAIL b-ped-item THEN DO:
   FIND FIRST b-ped-venda where
        b-ped-venda.nome-abrev = b-ped-item.nome-abrev and
        b-ped-venda.nr-pedcli  = b-ped-item.nr-pedcli no-lock no-error.
   
   IF b-ped-venda.tp-pedido = "PI" THEN DO:

/*        MESSAGE "PI" VIEW-AS ALERT-BOX.  */

       FOR EACH IM-ITEM-VINC WHERE 
               IM-ITEM-VINC.Num-Pedido-Venda = b-ped-item.nr-pedcli AND
               IM-ITEM-VINC.Nome-Abrev-V        = b-ped-item.nome-abrev AND
               IM-ITEM-VINC.It-Codigo                = b-ped-item.it-codigo AND
               IM-ITEM-VINC.Cod-Refer               = b-ped-item.cod-refer.
          
           FIND FIRST im-item-ped-compra-pi WHERE 
                     im-item-ped-compra-pi.num-pedido-compra = im-item-vinc.num-pedido-compra AND
                     im-item-ped-compra-pi.nome-abrev             = IM-ITEM-VINC.Nome-Abrev        AND
                     im-item-ped-compra-pi.it-codigo                 = im-item-vinc.it-codigo         AND
                     im-item-ped-compra-pi.cod-refer                 = im-item-vinc.cod-refer NO-ERROR.
           IF AVAIL im-item-ped-compra-pi THEN DO:   

/*               message "DELETE " SKIP                                                                                               */
/*                  " im-item-vinc.num-pedido-compra"     im-item-vinc.num-pedido-compra SKIP                                         */
/*                   "IM-ITEM-VINC.Nome-Abrev"            IM-ITEM-VINC.Nome-Abrev         SKIP                                        */
/*                   "im-item-vinc.it-codigo"                     im-item-vinc.it-codigo                  SKIP                        */
/*                   "im-item-vinc.cod-refer"                     im-item-vinc.cod-refer                  SKIP                        */
/*                   "solic-disponivel" IM-ITEM-PED-COMPRA-PI.Qtd-Solic-Disponivel  IM-ITEM-VINC.Qtd-Solicitada skip                  */
/*                   "receb-disponivel" IM-ITEM-PED-COMPRA-PI.Qtd-Recebida-Disponivel  IM-ITEM-VINC.Qtd-Reservada skip                */
/*                   "reservada"            IM-ITEM-PED-COMPRA-PI.Qtd-Reservada-Venda  IM-ITEM-VINC.Qtd-Reservada view-as alert-box.  */

    
               ASSIGN IM-ITEM-PED-COMPRA-PI.Qtd-Solic-Disponivel        = IM-ITEM-PED-COMPRA-PI.Qtd-Solic-Disponivel + IM-ITEM-VINC.Qtd-Solicitada
                            IM-ITEM-PED-COMPRA-PI.Qtd-Recebida-Disponivel  = IM-ITEM-PED-COMPRA-PI.Qtd-Recebida-Disponivel + IM-ITEM-VINC.Qtd-Reservada
                            IM-ITEM-PED-COMPRA-PI.Qtd-Reservada-Venda     = IM-ITEM-PED-COMPRA-PI.Qtd-Reservada-Venda - IM-ITEM-VINC.Qtd-Reservada.
     
           END.
           
           DELETE IM-ITEM-VINC.
       END.
   END.

   ELSE IF b-ped-venda.tp-pedido = "PP" THEN DO:
      FIND FIRST IM-ITEM-PED-VENDA-PP WHERE 
           IM-ITEM-PED-VENDA-PP.Num-Pedido-Venda = b-ped-item.nr-pedcli AND
           IM-ITEM-PED-VENDA-PP.Nome-Abrev       = b-ped-item.nome-abrev AND
           IM-ITEM-PED-VENDA-PP.It-Codigo        = b-ped-item.it-codigo AND
           IM-ITEM-PED-VENDA-PP.Cod-Refer        = b-ped-item.cod-refer NO-ERROR.
      IF AVAIL IM-ITEM-PED-VENDA-PP THEN
         DELETE IM-ITEM-PED-VENDA-PP.
   END.  
END.

ASSIGN lg-add-modifica = NO.
     
RETURN 'OK'.

/* FIM TDDI154 */
