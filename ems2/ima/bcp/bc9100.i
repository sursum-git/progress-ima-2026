/**************************************************************************************************
** Programa: bc9100.i                                                                            **
** Objetivo: Template padrao DC para interfaces caracter                                         **
**    Autor: Carlos Alberto Soares Pereira                                                       **
**     Data: 25/05/00                                                                            **
**************************************************************************************************/
/* Variable Definitions ---                                                            */

Define New Global Shared Var lMenu          As Logical                               No-undo.
Define Variable vDesErro                    As Character View-as Editor Size 5 By 2  No-undo.
Define Variable vTransDetail                As Character                             No-undo.
Define Variable vFrameList                  As Character                             No-undo.
Define Variable vTransaction                As Character                             No-undo.

Define Variable vConteudoRaw                As Raw                                   No-undo.

Define Variable wgWindow                    As Handle                                No-undo.
Define Variable wgapi010                    As Handle                                No-undo.

Define Variable vNumErro                    As Integer Format '>>>>>>9'              No-undo.
Define Variable vNumFrame                   As Integer Format '>>>>>>9'              No-undo.

Define Variable vLogFinaliza                As Logical Init No                       No-undo.
Define Variable vLogUpdateFrame             As Logical Init Yes                      No-undo.
Define Variable vLogCancela                 As Logical Init No                       No-undo.
Define Variable vLogSai                     As Logical Init No                       No-undo.
Define Variable vLogErro                    As Logical Init No                       No-undo.
Define Variable vLogEtiqueta                As Logical Init No                       No-undo.
/* dako */
DEF VAR h_bcapi001  AS HANDLE NO-UNDO.
/* dako */
/* tt-erro Definitions ---                                                             */
{bcp/bc9102.i}
Define temp-table tt-erro-tipo-trans NO-UNDO like tt-erro{&Ext}.

/* tt-trans Definitions ---                                                            */
{bcp/bcapi001.i}

/* tt-bc-tipo-trans Definitions ---                                                    */
{bcp/bcapi010.i}

/* Definitions Frames ---                                                              */
{bcp/bc9103.i} 

Find First bc-prog-trans no-lock 
     where bc-prog-trans.cod_prog_dtsul = '{&ProgramName}' No-error.

If  Not AvailAble bc-prog-trans Then Do:
    Hide All.
    Display 'Programa ({&ProgramName})'     At Row 01 Col 01
            'n∆o possue Relacio'            At Row 02 Col 01
            'namento Programa X'            At Row 03 Col 01
            'Transaá∆o (bc0111)'            At Row 04 Col 01
            With Font 3 Size {&FrameSize} No-box.
    Pause 1 No-message.
    Return Error.
End.

/* dako */
     
&IF Defined(UsingBcTransfilho)
&THEN

     &IF '{&UsingBcTransfilho}' = 'YES'
     &THEN
     
     RUN bcp/bcapi001.p     PERSISTENT SET h_bcapi001 (INPUT-OUTPUT TABLE TT-TRANS, input-output table tt-erro{&Ext}).
     
     &ENDIF
     
&ENDIF
     
/* dako */

Assign vTransaction = bc-prog-trans.cd-trans.

Assign Session:data-entry-return = Yes.

If  lMenu = No Then Do:
    If Session:window-system <> 'TTY' Then Do:
          Create Window wgWindow Assign
                Status-area  = No
                Message-area = No
                Width        = Frame Frame01:Width
                Height       = Frame Frame01:Height
                Title        = vTransaction.
          Assign Current-window = wgWindow.
    End.
End.

If Session:window-system <> 'TTY' Then
    Assign Current-window:Title = vTransaction.

/* Triggers Open Program inicio ---            */
{&TriggersOpenProgram}
/* Triggers Open Program inicio fim ---        */

/* Triggers do usuario inicio ---              */
{&UserTriggers}
/* Triggers do usuario fim ---                 */

ON 'F2':U Anywhere 
DO:
    Assign vLogFinaliza = Yes.
END.

ON 'F4' Anywhere 
DO:
    If  vNumFrame = 0 Then Do:
        Assign vLogCancela  = Yes
               vLogFinaliza = Yes
               vLogSai      = Yes.
    End.
    Assign vNumFrame    = vNumFrame - 1.
END.

Create {&TempTable}.

/* Update dos campos da Interface */
{bcp/bc9104.i}

Delete {&TempTable}.

Assign Session:data-entry-return = No.

/* Triggers Close Program inicio ---            */
{&TriggersCloseProgram}
/* Triggers Close Program inicio fim ---        */

If  lMenu = No Then Do:
    If Valid-handle(wgWindow) Then Do:
        Assign wgWindow:visible = NO .
        Delete Object wgWindow.
    End.
End.

/* dako */

IF  VALID-HANDLE(h_bcapi001)  = YES
AND h_bcapi001:TYPE           = "PROCEDURE":U  
AND h_bcapi001:FILE-NAME      = "bcp/bcapi001.p":U 
THEN DO:
    DELETE OBJECT h_bcapi001.
END.
/* dako */

&IF Defined(AtciveObject1) &THEN
    Run destroy In {&ActiveObject1}
    If Valid-handle({&ActiveObject1}) Then Delete Object {&ActiveObject1}.
&ENDIF
&IF Defined(AtciveObject2) &THEN
    Run destroy In {&ActiveObject2}
    If Valid-handle({&ActiveObject2}) Then Delete Object {&ActiveObject2}.
&ENDIF
&IF Defined(AtciveObject3) &THEN
    Run destroy In {&ActiveObject3}
    If Valid-handle({&ActiveObject3}) Then Delete Object {&ActiveObject3}.
&ENDIF
&IF Defined(AtciveObject4) &THEN
    Run destroy In {&ActiveObject4}
    If Valid-handle({&ActiveObject4}) Then Delete Object {&ActiveObject4}.
&ENDIF
&IF Defined(AtciveObject5) &THEN
    Run destroy In {&ActiveObject5}
    If Valid-handle({&ActiveObject5}) Then Delete Object {&ActiveObject5}.
&ENDIF
&IF Defined(AtciveObject6) &THEN
    Run destroy In {&ActiveObject6}
    If Valid-handle({&ActiveObject6}) Then Delete Object {&ActiveObject6}.
&ENDIF
&IF Defined(AtciveObject7) &THEN
    Run destroy In {&ActiveObject7}
    If Valid-handle({&ActiveObject7}) Then Delete Object {&ActiveObject7}.
&ENDIF
&IF Defined(AtciveObject8) &THEN
    Run destroy In {&ActiveObject8}
    If Valid-handle({&ActiveObject8}) Then Delete Object {&ActiveObject8}.
&ENDIF
&IF Defined(AtciveObject9) &THEN
    Run destroy In {&ActiveObject9}
    If Valid-handle({&ActiveObject9}) Then Delete Object {&ActiveObject9}.
&ENDIF
&IF Defined(AtciveObject10) &THEN
    Run destroy In {&ActiveObject10}
    If Valid-handle({&ActiveObject10}) Then Delete Object {&ActiveObject10}.
&ENDIF
&IF Defined(AtciveObject11) &THEN
    Run destroy In {&ActiveObject11}
    If Valid-handle({&ActiveObject11}) Then Delete Object {&ActiveObject11}.
&ENDIF
&IF Defined(AtciveObject12) &THEN
    Run destroy In {&ActiveObject12}
    If Valid-handle({&ActiveObject12}) Then Delete Object {&ActiveObject12}.
&ENDIF
&IF Defined(AtciveObject13) &THEN
    Run destroy In {&ActiveObject13}
    If Valid-handle({&ActiveObject13}) Then Delete Object {&ActiveObject13}.
&ENDIF
&IF Defined(AtciveObject14) &THEN
    Run destroy In {&ActiveObject14}
    If Valid-handle({&ActiveObject14}) Then Delete Object {&ActiveObject14}.
&ENDIF
&IF Defined(AtciveObject15) &THEN
    Run destroy In {&ActiveObject15}
    If Valid-handle({&ActiveObject15}) Then Delete Object {&ActiveObject15}.
&ENDIF
&IF Defined(AtciveObject16) &THEN
    Run destroy In {&ActiveObject16}
    If Valid-handle({&ActiveObject16}) Then Delete Object {&ActiveObject16}.
&ENDIF
&IF Defined(AtciveObject17) &THEN
    Run destroy In {&ActiveObject17}
    If Valid-handle({&ActiveObject17}) Then Delete Object {&ActiveObject17}.
&ENDIF
&IF Defined(AtciveObject18) &THEN
    Run destroy In {&ActiveObject18}
    If Valid-handle({&ActiveObject18}) Then Delete Object {&ActiveObject18}.
&ENDIF
&IF Defined(AtciveObject19) &THEN
    Run destroy In {&ActiveObject19}
    If Valid-handle({&ActiveObject19}) Then Delete Object {&ActiveObject19}.
&ENDIF
&IF Defined(AtciveObject20) &THEN
    Run destroy In {&ActiveObject20}
    If Valid-handle({&ActiveObject20}) Then Delete Object {&ActiveObject20}.
&ENDIF

Return.

