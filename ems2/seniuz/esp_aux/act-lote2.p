   DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.
   DEF VAR de-qtidade LIKE saldo-estoq.qtidade-atu.

   /* Includes para gerar Movto Estoq */
   {cdp/cd0666.i}
   {cep/ceapi001.i}

   FIND FIRST param-estoq NO-LOCK NO-ERROR.

   FOR EACH saldo-estoq WHERE
            saldo-estoq.cod-estabel = '1' AND 
            saldo-estoq.qtidade-atu > 0  AND
            saldo-estoq.cod-refer <> saldo-estoq.lote SHARE-LOCK,
       FIRST item OF saldo-estoq WHERE
             item.ge-codigo >= 50 AND
             item.ge-codigo <= 60 NO-LOCK.

      ASSIGN saldo-estoq.qt-alocada = 0.

      ASSIGN de-qtidade = saldo-estoq.qtidade-atu.

      RUN pi-cria-movto (INPUT 2,
                         INPUT saldo-estoq.lote).   /* Saida */
      IF RETURN-VALUE = 'ADM-ERROR' THEN
         UNDO, RETURN "ADM-ERROR".

      RUN pi-cria-movto (INPUT 1,
                         INPUT saldo-estoq.cod-refer).   /* Entrada */
      IF RETURN-VALUE = 'ADM-ERROR' THEN 
         UNDO, RETURN "ADM-ERROR".
   END.


   PROCEDURE pi-cria-movto.
       DEF INPUT PARAMETER p-tipo-trans AS INTEGER.
       DEF INPUT PARAMETER p-lote AS CHAR.

       FOR EACH tt-movto.
           DELETE tt-movto.
       END.

       CREATE tt-movto.
       ASSIGN tt-movto.cod-versao-integracao   = 1                       /* acrescentado*/
              tt-movto.cod-prog-orig           = "ESSP0102"              /* acrescentado*/
              tt-movto.usuario                 = c-seg-usuario           /* acrescentado*/
              tt-movto.cod-estabel             = saldo-estoq.cod-estabel
              tt-movto.ct-codigo               = param-estoq.ct-tr-transf
              tt-movto.sc-codigo               = param-estoq.sc-tr-transf
              tt-movto.conta-contabil          = param-estoq.conta-transf 
              tt-movto.esp-docto               = 33 
              tt-movto.tipo-trans              = p-tipo-trans
              tt-movto.dt-trans                = 12.31.2014
              tt-movto.it-codigo               = saldo-estoq.it-codigo
              tt-movto.cod-refer               = saldo-estoq.cod-refer
              tt-movto.lote                    = p-lote
              tt-movto.cod-depos               = saldo-estoq.cod-depos
              tt-movto.quantidade              = de-qtidade
              tt-movto.un                      = item.un
              tt-movto.dt-vali-lote            = 12.31.9999
              tt-movto.descricao-db            = 'Acerto de Lote Diferente da Referencia'.

       RUN cep/ceapi001.p (INPUT-OUTPUT TABLE tt-movto,
                           INPUT-OUTPUT TABLE tt-erro,
                           INPUT YES). /* Deleta erros? */

       FIND FIRST tt-erro NO-LOCK NO-ERROR.
       IF NOT AVAIL tt-erro THEN
          RETURN 'OK'.
       ELSE DO.
          MESSAGE tt-erro.mensagem
              VIEW-AS ALERT-BOX INFO BUTTONS OK.

          RETURN 'ADM-ERROR'.
       END.
   END PROCEDURE.

