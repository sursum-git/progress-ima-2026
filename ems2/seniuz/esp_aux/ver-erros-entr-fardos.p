DEF VAR da-dt-ini  AS DATE FORMAT "99/99/9999".
DEF VAR da-dt-fin  AS DATE FORMAT "99/99/9999".
DEF VAR de-peso-fardos AS DEC FORMAT "->>,>>9.99".
DEF VAR de-tot-dif     AS DEC FORMAT "->>,>>9.99".
DEF VAR i-qtd-fardos   AS INT FORMAT ">>>9".
DEF VAR l-fardo    AS LOG FORMAT "Sim/NÆo" LABEL "Mostra Fardos".
DEF VAR l-ok       AS LOG FORMAT "Sim/NÆo" LABEL "Confirma‡Æo".
DEF VAR p-fardo AS INT.
DEF VAR u-fardo AS INT.

ASSIGN da-dt-fin = TODAY.
ASSIGN da-dt-ini = DATE("01" + "/" + STRING(MONTH(TODAY)) + "/" + STRING(YEAR(TODAY))).
REPEAT:
   UPDATE da-dt-ini LABEL "Periodo Inicial"
          da-dt-fin LABEL "Periodo Final"
          l-fardo
          l-ok.
   IF l-ok = YES  THEN LEAVE.

END.

IF l-fardo THEN
   RUN mostra-fardo.
ELSE
   RUN mostra-erro.


/* ==================================================================== */

PROCEDURE mostra-fardo.

  FOR EACH mp-entr-mat WHERE mp-entr-mat.dt-recebimento >= da-dt-ini 
                            AND mp-entr-mat.dt-recebimento <= da-dt-fin NO-LOCK.
      ASSIGN de-peso-fardos = 0
             i-qtd-fardos   = 0.
      FOR EACH mp-fardo WHERE mp-fardo.nr-cdr = mp-entr-mat.nr-cdr NO-LOCK.
          IF i-qtd-fardos = 0 THEN
             ASSIGN p-fardo = mp-fardo.nr-fardo.
          ASSIGN de-peso-fardos = de-peso-fardos + mp-fardo.peso
                 i-qtd-fardos   = i-qtd-fardos + 1
                 u-fardo        = mp-fardo.nr-fardo.
     END.
     DISP  mp-entr-mat.nro-docto FORMAT ">>>>9"
           mp-entr-mat.peso-nf FORMAT ">>,>>9.99"
           (mp-entr-mat.qtd-fardos[1] + mp-entr-mat.qtd-fardos[2] +
           mp-entr-mat.qtd-fardos[3] + mp-entr-mat.qtd-fardos[4] +
           mp-entr-mat.qtd-fardos[5]) LABEL "Fardos" FORMAT ">>>9"
           de-peso-fardos  LABEL "Peso Fardos"
           i-qtd-fardos    LABEL "QTD"
           p-fardo LABEL "Prim" FORMAT "9999,9999"
           u-fardo LABEL "Ultimo" FORMAT "9999,9999".
  END.
END.

/* =============================================================== */

PROCEDURE mostra-erro.

 FOR EACH mp-entr-mat WHERE mp-entr-mat.dt-recebimento >= da-dt-ini 
                           AND mp-entr-mat.dt-recebimento <= da-dt-fin NO-LOCK.
     ASSIGN de-peso-fardos = 0
            i-qtd-fardos   = 0.
     FOR EACH mp-fardo WHERE mp-fardo.nr-cdr = mp-entr-mat.nr-cdr NO-LOCK.
         IF i-qtd-fardos = 0 THEN
            ASSIGN p-fardo = mp-fardo.nr-fardo.
         ASSIGN de-peso-fardos = de-peso-fardos + mp-fardo.peso
                i-qtd-fardos   = i-qtd-fardos + 1
                u-fardo        = mp-fardo.nr-fardo.
    END.
    DISP  mp-entr-mat.dt-recebimento 
       mp-entr-mat.nro-docto FORMAT ">>>>9"
       mp-entr-mat.peso-nf FORMAT ">>,>>9.99"
       (mp-entr-mat.qtd-fardos[1] + mp-entr-mat.qtd-fardos[2] +
       mp-entr-mat.qtd-fardos[3] + mp-entr-mat.qtd-fardos[4] +
       mp-entr-mat.qtd-fardos[5]) LABEL "Fardos" FORMAT ">>>9"
       de-peso-fardos  LABEL "Peso Fardos"
       i-qtd-fardos    LABEL "QTD"
       (mp-entr-mat.peso-nf - de-peso-fardos) LABEL "Diferen‡a" WHEN mp-entr-mat.peso-nf <> de-peso-fardos. 
    IF mp-entr-mat.peso-nf <> de-peso-fardos AND de-peso-fardos <> 0 THEN
       ASSIGN de-tot-dif = de-tot-dif + (mp-entr-mat.peso-nf - de-peso-fardos).

 END.
 DISP de-tot-dif LABEL "Total Diferen‡a". 
END.
