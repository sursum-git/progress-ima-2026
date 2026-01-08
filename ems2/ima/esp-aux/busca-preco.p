DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

DEF BUFFER  b-preco-item FOR preco-item.

DEF VAR i-ind-finan         AS INTEGER.
DEF VAR  c-const            AS CHAR FORMAT "x(80)".
DEF VAR  c-desc-item        AS CHAR FORMAT "x(47)".
DEF VAR  c-un               AS CHAR FORMAT "x(02)".
DEF VAR  primvez            AS CHAR FORMAT "x(01)".
DEF VAR  ii                 AS INT FORMAT "99".
DEF VAR  jj                 AS INT FORMAT "99".
DEF VAR  kk                 AS INT FORMAT "99".
DEF VAR  de-ind             AS DEC EXTENT 12 FORMAT "9.9999".
DEF VAR  de-saldo           AS DEC FORMAT "->>>>>,>>9.99".
DEF VAR  de-qt-min          LIKE saldo-estoq.qtidade-atu.

ASSIGN i-ind-finan = 4.

FIND FIRST tab-finan WHERE 
           tab-finan.dt-ini-val <= TODAY AND 
           tab-finan.dt-fim-val >= TODAY NO-LOCK NO-ERROR.
IF AVAIL tab-finan THEN DO:
   DO ii = 1 TO i-ind-finan:
      IF  ii = 1  THEN DO:
          c-const = "     Base ".
      END.
      ELSE DO:
         ASSIGN c-const = c-const + "       " + string(tab-dia-fin[ii]) +  " d " .
      END.
      ASSIGN de-ind[ii] = tab-finan.tab-ind-fin[ii].
   END.
END.

FIND FIRST para-ped NO-LOCK NO-ERROR.
IF NOT AVAIL para-ped THEN RETURN 'adm-error':u.

FIND FIRST tb-preco WHERE
           tb-preco.nr-tabpre = 'TAB E12' NO-LOCK no-error.
  IF AVAIL tb-preco THEN do:
    FOR EACH preco-item  NO-LOCK WHERE preco-item.nr-tabpre = tb-preco.nr-tabpre AND
             preco-item.it-codigo >= '390000' AND
             preco-item.it-codigo <= '390000' AND
             preco-item.situacao = 1
            BREAK BY PRECO-ITEM.IT-CODIGO:
      
      IF FIRST-OF(preco-item.it-codigo)   THEN DO:
         FIND FIRST ITEM WHERE 
                    item.it-codigo = preco-item.it-codigo NO-LOCK no-error.

         IF AVAIL item THEN 
           ASSIGN c-desc-item  = item.desc-item
                  c-un         = ITEM.un.
         ELSE 
             ASSIGN c-desc-item  = ""
                    c-un         = "".
      END.

      IF LAST-OF(preco-item.it-codigo)   THEN DO:
          DISP Preco-item.it-codigo 
               c-desc-item       
               c-un.        
          DO ii =  1 TO i-ind-finan:
             DISP preco-item.preco-venda 
                  de-ind[ii]
                  (preco-item.preco-venda * de-ind[ii]) FORMAT ">>>,>>9.99".
             PAUSE.
          END.
       END.
    END.
END.

