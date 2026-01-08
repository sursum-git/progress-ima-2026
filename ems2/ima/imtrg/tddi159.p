/****************************************************************************
** Programa: TDDI159 - Trigger de Delete na Tabela Ped-Venda
** Data    : Agosto de 2002
** Objetivo: trigger de delete para a tabela Ped-venda
** Empresa: IMA E INTERMALHAS 
** Vers∆o:   2.04.001
** Alterado: 21 de agosto de 2002
*****************************************************************************/

DEF PARAMETER BUFFER b-ped-venda FOR ped-venda.

DEF NEW GLOBAL SHARED VAR lg-add-modifica   AS LOGICAL.

FIND ped-venda-ext WHERE
     ped-venda-ext.cod-estabel = b-ped-venda.cod-estabel AND 
     ped-venda-ext.nr-pedido = b-ped-venda.nr-pedido EXCLUSIVE-LOCK NO-ERROR.

IF AVAIL ped-venda-ext THEN
   DELETE ped-venda-ext.


/* Comentado por Toninho em 29/07/2009 Ap¢s convers∆o para progress 10

/* MESSAGE "tddi159" VIEW-AS ALERT-BOX. */
DEF VAR v-nr-pedcli  LIKE b-venda.nr-pedcli.
DEF VAR v-nome-abrev LIKE b-venda.nome-abrev.

IF AVAIL b-venda  THEN DO:
   ASSIGN v-nr-pedcli = b-venda.nr-pedcli
          v-nome-abrev = b-venda.nome-abrev.
        
    /* Apagando Registros da IMA OFICIAL */

        FOR EACH W-PEDIDO
            WHERE w-pedido.nr-pedcli   = v-nr-pedcli
              AND w-pedido.nome-abrev  = v-nome-abrev 
              AND w-pedido.cod-estabel = b-venda.cod-estabel
            SHARE-LOCK.
            DELETE W-PEDIDO NO-ERROR.
        END.
        FOR EACH WF000 
            WHERE wf000.tb-name = "PED-VENDA"
              AND wf000.char1 = v-nr-pedcli
              AND wf000.char2 = v-nome-abrev 
              AND wf000.cod-estabel =  b-venda.cod-estabel
            SHARE-LOCK.
              DELETE WF000 NO-ERROR.
        END.
        FOR EACH W-PED-VEND
            WHERE w-ped-vend.nr-pedcli  = v-nr-pedcli
             AND w-ped-vend.nome-abrev  = v-nome-abrev 
             AND w-ped-vend.cod-estabel = b-venda.cod-estabel
            SHARE-LOCK.
            DELETE W-PED-VEND NO-ERROR.
        END.
        FOR EACH W-PED-REPRE
            WHERE w-ped-repre.nr-pedido = INT(v-nr-pedcli) SHARE-LOCK.
            DELETE W-PED-REPRE NO-ERROR.
        END.
        FOR EACH W-PED-ENT
            WHERE w-ped-ent.nr-pedcli = v-nr-pedcli
            AND w-ped-ent.nome-abrev  = v-nome-abrev 
            AND w-ped-ent.cod-estabel = b-venda.cod-estabel
            SHARE-LOCK.
            DELETE W-PED-ENT NO-ERROR.
        END.
        FOR EACH W-PED-ITEM
            WHERE w-ped-item.nr-pedcli = v-nr-pedcli
             AND w-ped-item.nome-abrev = v-nome-abrev 
             AND w-ped-item.cod-estabel = b-venda.cod-estabel
            SHARE-LOCK.

            DELETE W-PED-ITEM NO-ERROR.
        END.

        /* Apagando registros da Backup */

        FOR EACH  g-PEDIDO
            WHERE g-pedido.nr-pedcli   = v-nr-pedcli
              AND g-pedido.nome-abrev  = v-nome-abrev 
              AND g-pedido.cod-estabel = b-venda.cod-estabel 
        SHARE-LOCK.
        DELETE g-PEDIDO NO-ERROR.
    END.
    FOR EACH   g-wF000 
        WHERE  g-wf000.tb-name = "PED-VENDA"
          AND  g-wf000.char1   = v-nr-pedcli
          AND  g-wf000.char2   = v-nome-abrev 
          AND  g-wf000.cod-estabel = b-venda.cod-estabel SHARE-LOCK.
        DELETE g-wF000 NO-ERROR.
    END.
    FOR EACH   g-PED-VEND
        WHERE  g-ped-vend.nr-pedcli = v-nr-pedcli
         AND   g-ped-vend.nome-abrev = v-nome-abrev 
         AND   g-ped-vend.cod-estabel = b-venda.cod-estabel
        SHARE-LOCK.
        DELETE g-PED-VEND NO-ERROR.
    END.
    FOR EACH   g-PED-REPRE
        WHERE  g-ped-repre.nr-pedido = INT(v-nr-pedcli) SHARE-LOCK.
        DELETE g-PED-REPRE NO-ERROR.
    END.
    FOR EACH  g-PED-ENT
        WHERE g-ped-ent.nr-pedcli  = v-nr-pedcli
         AND  g-ped-ent.nome-abrev  = v-nome-abrev 
         AND  g-ped-ent.cod-estabel = b-venda.cod-estabel
        SHARE-LOCK.
        DELETE g-PED-ENT NO-ERROR.
    END.
    FOR EACH  g-PED-ITEM
        WHERE g-ped-item.nr-pedcli  = v-nr-pedcli
         AND  g-ped-item.nome-abrev  = v-nome-abrev 
         AND  g-ped-item.cod-estabel = b-venda.cod-estabel
      SHARE-LOCK.
      DELETE  g-PED-ITEM NO-ERROR.
    END.        
    /*****/
    FIND FIRST im-ext-ped-venda OF b-venda NO-ERROR.
    IF AVAIL im-ext-ped-venda THEN DO:
       DELETE im-ext-ped-venda.
    END.

    FOR EACH ped-item WHERE 
             ped-item.nome-abrev = v-nome-abrev and
             ped-item.nr-pedcli  = v-nr-pedcli NO-LOCK.

        IF b-venda.tp-pedido = "PI" THEN DO:
           FOR EACH IM-ITEM-VINC WHERE 
                    IM-ITEM-VINC.Num-Pedido-Venda = ped-item.nr-pedcli AND
                    IM-ITEM-VINC.Nome-Abrev-V     = ped-item.nome-abrev AND
                    IM-ITEM-VINC.It-Codigo        = ped-item.it-codigo AND
                    IM-ITEM-VINC.Cod-Refer        = ped-item.cod-refer.
    
              FIND FIRST im-item-ped-compra-pi WHERE 
                         im-item-ped-compra-pi.num-pedido-compra = im-item-vinc.num-pedido-compra AND
                         im-item-ped-compra-pi.nome-abrev        = IM-ITEM-VINC.Nome-Abrev        AND
                         im-item-ped-compra-pi.it-codigo         = im-item-vinc.it-codigo         AND
                         im-item-ped-compra-pi.cod-refer         = im-item-vinc.cod-refer NO-ERROR.
              IF AVAIL im-item-ped-compra-pi THEN DO:
    
                 ASSIGN IM-ITEM-PED-COMPRA-PI.Qtd-Solic-Disponivel    = IM-ITEM-PED-COMPRA-PI.Qtd-Solic-Disponivel + IM-ITEM-VINC.Qtd-Solicitada
                        IM-ITEM-PED-COMPRA-PI.Qtd-Recebida-Disponivel = IM-ITEM-PED-COMPRA-PI.Qtd-Recebida-Disponivel + IM-ITEM-VINC.Qtd-Reservada
                        IM-ITEM-PED-COMPRA-PI.Qtd-Reservada-Venda     = IM-ITEM-PED-COMPRA-PI.Qtd-Reservada-Venda - IM-ITEM-VINC.Qtd-Reservada.
    
              END.

              DELETE IM-ITEM-VINC.
           END.
        END.

        ELSE IF b-venda.tp-pedido = "PP" THEN DO:
           FIND FIRST IM-ITEM-PED-VENDA-PP WHERE 
                IM-ITEM-PED-VENDA-PP.Num-Pedido-Venda = ped-item.nr-pedcli AND
                IM-ITEM-PED-VENDA-PP.Nome-Abrev       = ped-item.nome-abrev AND
                IM-ITEM-PED-VENDA-PP.It-Codigo        = ped-item.it-codigo AND
                IM-ITEM-PED-VENDA-PP.Cod-Refer        = ped-item.cod-refer NO-ERROR.
           IF AVAIL IM-ITEM-PED-VENDA-PP THEN
              DELETE IM-ITEM-PED-VENDA-PP.
        END.
    END.
    /*****/
END.

Fim do Coment†rio pelo Toninho em 29/07/2009 */

ASSIGN lg-add-modifica = NO.
RETURN 'OK'.
