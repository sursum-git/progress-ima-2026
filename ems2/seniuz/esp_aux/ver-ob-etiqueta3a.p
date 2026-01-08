def var h-prog as handle no-undo.
run utp/ut-utils.p persistent set h-prog.

OUTPUT TO c:\temp\lixo.txt CONVERT SOURCE "ibm850".

FOR EACH movto-etq WHERE
         movto-etq.num-etiqueta = 127972 NO-LOCK.
    DISP movto-etq.num-etiqueta
         movto-etq.dt-trans
         movto-etq.nro-docto
         movto-etq.esp-docto
         movto-etq.tipo-trans
         substr(movto-etq.char-1,1,80) FORMAT "x(80)"
         SKIP
         FILL("-",145) FORMAT "x(145)"
         WITH WIDTH 151.
END.
OUTPUT CLOSE.

run Execute in h-prog(input "notepad.exe",
 			          input "c:\temp\lixo.txt").
delete procedure h-prog.

