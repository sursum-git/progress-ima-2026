DEF VAR de-saldo AS DEC.
DEF VAR c-mov AS CHAR FORMAT "x(3)".

def var h-prog as handle no-undo.
run utp/ut-utils.p persistent set h-prog.

OUTPUT TO "c:/lixo/lixo.txt".

FOR EACH titulo WHERE titulo.ep-codigo   = 1
                  AND titulo.cod-estabel = "2"
                  AND titulo.cod-esp     = "dp"
                  AND titulo.serie       = "3"
                  AND titulo.nr-docto    = "0007658"
                NO-LOCK.
    RUN pi-saldo.
END.

PROCEDURE pi-saldo.
   ASSIGN de-saldo = titulo.vl-original.
   FOR EACH mov-tit OF titulo 
       WHERE mov-tit.vl-liquido <> 0 NO-LOCK.
       IF mov-tit.lancamento = 2 THEN
          ASSIGN de-saldo = de-saldo + mov-tit.vl-baixa.
       ELSE
          ASSIGN de-saldo = de-saldo - mov-tit.vl-baixa.

       {esinc/i-dsrb.i mov-tit.transacao mov-tit.transacao c-mov} 

       DISP mov-tit.nr-docto
            mov-tit.parcela
            mov-tit.cod-esp
            mov-tit.dt-trans
            mov-tit.doc-antecip
            mov-tit.parc-antecip
            c-mov
            mov-tit.lancamento VIEW-AS FILL-IN
            mov-tit.vl-liquido
            mov-tit.vl-baixa
            de-saldo WITH WIDTH 200.
   END.
END PROCEDURE.

OUTPUT CLOSE.

run Execute in h-prog(input "wordpad.exe", input "c:\lixo\lixo.txt").
delete procedure h-prog.
