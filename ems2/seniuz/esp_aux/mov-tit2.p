DEF VAR de-sld-titulo LIKE titulo.vl-saldo.
DEF VAR l-pagar-com AS LOG.
DEF VAR c-mov AS CHAR.
DEF VAR c-nr-docto LIKE cheque.nr-docto-mvto INIT "0100549".
DEF VAR c-parcela  LIKE cheque.parcela-mvto  INIT "02".
DEF VAR da-data-ini AS DATE INIT 07/01/2006.
DEF VAR da-data-fin AS DATE INIT 07/31/2006.

UPDATE c-nr-docto
       c-parcela
       da-data-ini
       da-data-fin.

DEF BUFFER b-cheque FOR cheque.

FOR EACH cheque WHERE cheque.ep-codigo-mvto   =  1
                  AND cheque.cod-estabel-mvto =  "2"
                  AND cheque.cod-esp-mvto     =  "dp"
                  AND cheque.nr-docto-mvto    =  c-nr-docto
                  AND cheque.parcela-mvto     =  c-parcela
               NO-LOCK:
    /*- Verifica a data do 1ß dep¢sito do cheque -*/
    FIND FIRST movto-cheque USE-INDEX num-id-cheque
         WHERE movto-cheque.num-id-cheque = cheque.num-id-cheque
           AND movto-cheque.transacao     = 6 /* Dep¢sito */
        NO-LOCK NO-ERROR.
    IF NOT AVAIL movto-cheque THEN
       NEXT.
    ELSE DO.
       MESSAGE "Cheque: " movto-cheque.nr-cheque "1ß Dep¢sito: " movto-cheque.dt-movto
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
       IF movto-cheque.dt-movto > da-data-fin THEN
          NEXT.
    END.

    FIND titulo WHERE titulo.ep-codigo   = cheque.ep-codigo-mvto  
                  AND titulo.cod-estabel = cheque.cod-estabel-mvto
                  AND titulo.cod-esp     = cheque.cod-esp-mvto    
                  AND titulo.serie       = cheque.serie-mvto      
                  AND titulo.nr-docto    = cheque.nr-docto-mvto   
                  AND titulo.parcela     = cheque.parcela-mvto
                NO-LOCK NO-ERROR.
    ASSIGN de-sld-titulo = 0.
    IF AVAIL titulo THEN DO:
       ASSIGN de-sld-titulo = titulo.vl-original.
       FOR EACH mov-tit OF titulo
          WHERE mov-tit.dt-movto <= da-data-fin NO-LOCK:
          IF mov-tit.transacao =  2 OR /* Bax */
             mov-tit.transacao =  3 OR /* Dev */
             mov-tit.transacao =  4 OR /* Ima */
             mov-tit.transacao = 13    /* Ava */ THEN DO:
             IF mov-tit.lancamento = 2 THEN
                ASSIGN de-sld-titulo = de-sld-titulo + mov-tit.vl-baixa.
             ELSE
                ASSIGN de-sld-titulo = de-sld-titulo - mov-tit.vl-baixa.
          END.
          {esinc/i-dsrb.i mov-tit.transacao mov-tit.transacao c-mov}
          DISP mov-tit.lancamento VIEW-AS FILL-IN
               c-mov
               mov-tit.vl-baixa
               mov-tit.dt-baixa
               de-sld-titulo.
       END.
    END.

    /*--- Verifica se pelo menos um dos cheques teve o 1ßdep¢sito no intervalo de data selecionado e 
                   se n∆o h† nenhum cheque pendente ---*/
    ASSIGN l-pagar-com = NO.
    FOR EACH b-cheque WHERE b-cheque.ep-codigo-mvto   = cheque.ep-codigo-mvto
                        AND b-cheque.cod-estabel-mvto = cheque.cod-estabel-mvto
                        AND b-cheque.cod-esp-mvto     = cheque.cod-esp-mvto
                        AND b-cheque.serie-mvto       = cheque.serie-mvto
                        AND b-cheque.nr-docto-mvto    = cheque.nr-docto-mvto
                        AND b-cheque.parcela-mvto     = cheque.parcela-mvto
                      NO-LOCK:
        FIND FIRST movto-cheque USE-INDEX num-id-cheque
             WHERE movto-cheque.num-id-cheque = b-cheque.num-id-cheque
               AND movto-cheque.transacao     = 6 /* Dep¢sito */
            NO-LOCK NO-ERROR.
        IF AVAIL movto-cheque THEN DO:
           IF movto-cheque.num-id-cheque =  b-cheque.num-id-cheque AND
              movto-cheque.dt-movto      >= da-data-ini AND
              movto-cheque.dt-movto      <= da-data-fin THEN
              ASSIGN l-pagar-com = YES.
           IF movto-cheque.num-id-cheque = b-cheque.num-id-cheque AND   
              movto-cheque.dt-movto      > da-data-fin THEN DO:
              ASSIGN l-pagar-com = NO.
              LEAVE.
           END.
        END.
        ELSE DO:
           ASSIGN l-pagar-com = NO.
           LEAVE.
        END.
    END.
END.
MESSAGE "Saldo do t°tulo: " de-sld-titulo SKIP
        "Pagar comiss∆o? " l-pagar-com
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

