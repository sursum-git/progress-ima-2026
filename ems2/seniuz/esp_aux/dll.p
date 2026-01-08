def var h-Inst as int  no-undo.

DEF VAR c-arq-image AS CHAR.
DEF VAR c-exec AS CHAR.

ASSIGN c-arq-image = "T:\especificos\ems2\ima\image\super.jpg".

OS-COPY VALUE(c-arq-image) "c:\temp".


ASSIGN c-exec = "RunDLL32.exe C:\Windows\System32\Shimgvw.dll, ImageView_Fullscreen c:\temp\super.jpg".
OS-COMMAND SILENT VALUE(c-exec).

/*
run ShellExecuteA (input 0,
                   input "open",
                   input "rundll32.exe",
                   input "shell32.dll,OpenAs_RunDLL " + c-doc,
                   input "",
                   input 1,
                   output h-inst).
*/

/*
assign c-exec = fill("x",255).
run FindExecutableA (input c-arq-image,
                     input "",
                     input-output c-exec,
                     output h-inst).


if h-inst >= 0 and h-inst <= 32 then
    RUN ShellExecuteA(INPUT 0, 
                      INPUT "open", 
                      INPUT "rundll32.exe", 
                      INPUT "shimgvw.dll,ImageView_Fullscreen '"+ c-arq-image,
                      INPUT "",
                      INPUT 1, 
                      OUTPUT h-inst). 


run ShellExecuteA (input 0,
                       input "open",
                       input c-arq-image,
                       input "",
                       input "",
                       input 1,
                       output h-inst).


*/

PROCEDURE ShellExecuteA EXTERNAL "Shell32.dll" persistent:

    define input parameter hwnd as long.
    define input parameter lpOperation as char  no-undo.
    define input parameter lpFile as char  no-undo.
    define input parameter lpParameters as char  no-undo.
    define input parameter lpDirectory as char  no-undo.
    define input parameter nShowCmd as long.
    define return parameter hInstance as long.

END PROCEDURE.

PROCEDURE FindExecutableA EXTERNAL "Shell32.dll" persistent:

    define input parameter lpFile as char  no-undo.
    define input parameter lpDirectory as char  no-undo.
    define input-output parameter lpResult as char  no-undo.
    define return parameter hInstance as long.

END.
