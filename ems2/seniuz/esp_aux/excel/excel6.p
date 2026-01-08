def var h-prog as handle no-undo.
run utp/ut-utils.p persistent set h-prog.

run Execute in h-prog(input "excel.exe",
 			          input ?).
delete procedure h-prog.


