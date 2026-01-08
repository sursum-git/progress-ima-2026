def var h-prog as handle no-undo.
run utp/ut-utils.p persistent set h-prog.

OUTPUT TO c:\temp\xxxx.txt.
FOR EACH movto-estoq WHERE movto-estoq.it-codigo = "500091"
                       AND movto-estoq.dt-trans  >= 08/01/2006
                       AND movto-estoq.dt-trans  <= 08/10/2006.
    DISP movto-estoq WITH 1 COL WIDTH 550.
END.
OUTPUT CLOSE.

run Execute in h-prog(input "notepad.exe",
 			          input "c:\temp\xxxx.txt").
delete procedure h-prog.


