DEF VAR de-ent LIKE movto-estoq.quantidade.
DEF VAR de-sai LIKE movto-estoq.quantidade.
DEF VAR de-total   AS DEC FORMAT "->>,>>>,>>9.99".
DEF VAR de-meses   AS DEC FORMAT "->>,>>>,>>9.99" EXTENT 12.
DEF VAR de-aux     AS DEC FORMAT "->>,>>>,>>9.99" EXTENT 12.
DEF VAR i-pto     AS INT.
DEF VAR i-cont    AS INT.
DEF VAR i-kkk     AS INT.

FOR each  movto-estoq USE-INDEX data-item
    where movto-estoq.dt-trans >= 11/01/2004
      and movto-estoq.dt-trans <= 10/31/2005
      and movto-estoq.it-codigo >= "130107"
      and movto-estoq.it-codigo <= "130107"
    no-lock:

    if movto-estoq.cod-estabel <> "2" then  NEXT.


    if movto-estoq.esp-docto <> 28 and 
       movto-estoq.esp-docto <> 31 and 
       movto-estoq.esp-docto <>  5 THEN NEXT. 

    find item where item.it-codigo     = movto-estoq.it-codigo
                and item.cod-obsoleto >= 1
                and item.cod-obsoleto <= 4
              no-lock no-error.
    if not avail ITEM then NEXT.
 
    ASSIGN i-pto = MONTH(movto-estoq.dt-trans).

    IF movto-estoq.tipo-trans = 1 THEN
       ASSIGN de-ent = movto-estoq.quantidade
              de-meses[i-pto] = de-meses[i-pto]  - movto-estoq.quantidade.
    ELSE
       ASSIGN de-sai = movto-estoq.quantidade
              de-meses[i-pto] = de-meses[i-pto] + movto-estoq.quantidade.
END. 

ASSIGN i-pto = 11.
IF i-pto > 1 THEN DO:
    ASSIGN i-kkk = 1.
    DO i-cont = i-pto   TO 12:
       ASSIGN de-aux[i-kkk] = de-meses[i-cont]
              de-total      = de-total + de-meses[i-cont]
              i-kkk         = i-kkk + 1.
    END.
    DO  i-cont = 1 TO i-pto - 1:
        ASSIGN de-aux[i-kkk] = de-meses[i-cont]
               de-total      = de-total + de-meses[i-cont]
               i-kkk         = i-kkk + 1.
    END.
END.
ELSE
    DO i-cont = 1 TO 12:
       ASSIGN de-aux[i-cont] = de-meses[i-cont]
              de-total       = de-total + de-aux[i-cont].
    END.


DISP  de-aux[1]
      de-aux[2]
      de-aux[3]
      de-aux[4]
      de-aux[5]
      de-aux[6]
      de-aux[7]
      de-aux[8]
      de-aux[9]
      de-aux[10]
      de-aux[11]
      de-aux[12]
      WITH SIDE-LABEL 1 COLUMN.
