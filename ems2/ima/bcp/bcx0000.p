/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
/********************************************************************************************
**   Programa..: bc9112.p                                                                  **
**                                                                                         **
**   Versao....: 2.00.00.000 - jun/2000 - John Cleber Jaraceski                            **
**                                                                                         **
**   Objetivo..: Template DB Interface Menu para transacoes                                **
**                                                                                         **
**   Includes..: bc9105.i, bc9102.i                                                        **
**                                                                                         **
********************************************************************************************/

/***************************************************************************************************
** SECAO DE PRE-PROCESSADORES DA TEMPLATE                                                         **
** Nesta secao sao definidos os pre-processadores que serao usados na montagem da interface.      **
**                                                                                                **
** DESCRICAO DOS PREPROCESSADORES:                                                                **
** ProgramName          - Nome do programa e, tambem, do Codigo da transacao do Data Collection   **
**                        que sera acionada pela interface.                                       **
***************************************************************************************************/

Define New Global Shared Var lMenu          As Logical                               No-undo.
/* Definicao global do nome da transacao ---                */
&global-define ProgramName bc9112
/************************************************************/
{utp/ut-glob.i}
/* Definicao Temp-Table tt-erro ---                         */

/*{bcp/bc9102.i}*/
/************************************************************/

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.
IF c-seg-usuario = "" THEN
   RUN btb\btb910za.r.
/* Definicao de variaveis do menu ---                       */

{bcp/bc9112.i0} /* Login */
Define Variable c-opcao  As Character Format 'x(01)'              No-Undo.
Define Variable vDesErro As Character View-as Editor Size 5 By 2  No-Undo.
Define Variable vLogErro As Logical Init No                       No-Undo.
Define Variable vNumErro As Integer Format '>>>>>>9'              No-Undo.
Define Variable wgWindow As Handle                                No-Undo.
/************************************************************/

DEFINE VARIABLE l-banco  AS LOG                                   NO-UNDO.

def new global shared var v_cod_usuar_corren as character format "x(12)" LABEL "Usu rio Corrente" COLUMN-LABEL "Usu rio Corrente" no-undo.



/***************************************** Frames Inicio ******************************************/
/* Definicao da Frame01 ---                                 */
Define Frame Frame01
    'US:'                  At Row 01 Col 01
     C-SEG-USUARIO         At Row 01 Col 04 NO-LABEL
    '1.EMBARCAR ITENS'     At Row 02 Col 01
    '2.DESEMBARCAR ITENS'  At Row 03 Col 01
    '3.ROMANEIO'           At Row 04 Col 01
    '4.INVENTARIO'         At Row 05 Col 01
    '9.Fim'                At Row 06 Col 01
    'DIGITE OPCAO[ ]'      At Row 10 Col 01
     c-opcao               At Row 10 Col 14 NO-LABEL
     With 1 down font 3 Size 40 By 15 NO-BOX.

/************************************************************/

/* Definicao da _Error ---                                  */
Define Frame _Error
    'Erro:'         At Row 1 Col 1 
    vNumErro        At Row 1 Col 6 No-label
    vDesErro        At Row 2 Col 1 No-label
        With 1 down font 3 Size 18 By 8 NO-BOX .

Assign vDesErro:Width  = Frame _Error:Width
       vDesErro:Height = Frame _Error:Height - 1.
/************************ACESSO************************************/

{esinc/i-acesso.i "bcx0000"}

/*****************************************   Frames Fim ******************************************/

/***********************************   Codigo Principal   ****************************************/
Assign Session:data-entry-return = Yes.

If Session:window-system <> 'TTY' Then Do:

    Create Window wgWindow Assign
          Status-area  = No
          Message-area = No
          Width        = 40
          Height       = 15
          Title        = 'Menu'.
    Assign Current-window = wgWindow.

    ASSIGN FRAME frame01:WIDTH   = 40
           FRAME frame01:HEIGHT  = 15.

End.

Assign lMenu = Yes.

find first param-bc no-lock no-error.

IF NOT AVAIL param-bc 
THEN DO:

    Hide All.
    Display "**** ERRO ****" At Row 01 Col 01
            'PARAM DC'       At Row 02 Col 01
            'NAO CADASTRADO' At Row 03 Col 01 With Font 3 Size 18 By 8 No-box.
    Pause 3 No-message.    

    RETURN.
END.

find first param-GLOBAL no-lock no-error.

IF param-GLOBAL.MODULO-CL = NO
THEN DO:

    Hide All.
    Display "**** ERRO *****"  At Row 01 Col 01
            'DATA COLLECITON' At Row 02 Col 01
            'NAO IMPLANTADO'  At Row 03 Col 01 With Font 3 Size 18 By 8 No-box.

    Pause 3 No-message.    

    RETURN.
END.


Repeat  On Error Undo, Retry
        On EndKey Undo, RETRY:

    HIDE ALL NO-PAUSE.  
    
    {bcp/bc9112.i}  /* Login do Produto */      

    if l-banco = no 
    then do:
        l-banco = yes.
        run pi-login.  
        
        if return-value = "NOK" THEN QUIT.
                    
    end.    

    DISP CAPS(C-SEG-USUARIO) @ C-SEG-USUARIO WITH FRAME FRAME01.
            
    Assign c-opcao = ''.
    
    If Session:window-system <> 'TTY' Then
        Assign wgWindow:Title = "Menu".
    
    View frame Frame01.


    readkey.
    if  keyfunction(lastkey) = 'end-error' then NEXT.
    
    ASSIGN c-opcao = keyfunction(lastkey).
    
    /*
        Update c-opcao With Frame Frame01.
    */

    Run ExecutaOpcaoFrame01.
    
    HIDE ALL NO-PAUSE.

    If c-opcao = '9' Then DO :

        If Session:window-system <> 'TTY' 
        Then DO:

            APPLY "CLOSE" TO THIS-PROCEDURE.

            Assign Session:data-entry-return = No.

            If Valid-handle(wgWindow) Then Do:
                Assign wgWindow:visible = No.
                Delete  Object wgWindow.
            End.

            RETURN NO-APPLY.

        END.

        RETURN.
    END.

End.

Assign Session:data-entry-return = No.

If Valid-handle(wgWindow) Then Do:
    Assign wgWindow:visible = No.
    Delete  Object wgWindow.
End.

Return.
/**************************************************************************************************/

/*********************************** Procedures Internas ******************************************/
Procedure ExecutaOpcaoFrame01:
    Assign vLogErro = No.
    Hide Frame Frame01.

    HIDE ALL NO-PAUSE. 
    HIDE ALL NO-PAUSE. 
    HIDE ALL NO-PAUSE. 
    
    Case c-opcao:
        When '1' Then       
            run 'bcp/bcx0003a.p'.
        When '2' Then                 
            run 'bcp/bcx0003b.p'.  
        When '3' Then                 
            run 'bcp/bcx0003f.w'.
        When '4' Then                 
            run 'bcp/bcx0005a.p'.
        When '9' Then do:
            Hide All.
/*             Display 'Datasul S.A.'    At Row 01 Col 01                                  */
/*                     'Data Collection' At Row 02 Col 01 With Font 3 Size 18 By 8 No-box. */
/*             Pause 1 No-message.                                                         */
        End.
        Otherwise do:
            Assign vLogErro = Yes.
            MESSAGE 'opcao invalida'
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            /*{bcp/bc9105.i "0" "Opcao invalida"}*/
        End.
    End Case.
    
    If  Frame Frame01:Visible = Yes Then
        Hide Frame Frame01.
        
    Assign lMenu = No.

End Procedure.

procedure pi-login:
def var c-senha as char no-undo. 

{bcp/bc9112.i}  /* Login do Produto */      

assign c-senha = entry(5,session:parameter) no-error.

if v_cod_usuar_corren = "" then do:

    
   If Session:window-system <> 'TTY' Then Do:
             
      RUN btb/btb910za.w.    
   
   END.
   ELSE do: 
      pause 0.
      hide all.    
     
      disp "Nao foi efetuado " SKIP 
           "o Login no produto," SKIP 
           "nao foi informado o" SKIP
           "usuÿrio" with frame fff1 NO-BOX.

      hide all.
      PAUSE 0.
               
               
   end.
end.
 
IF  V_COD_USUAR_CORREN <> "" 
THEN DO:
    
    RETURN "OK".
        
END.

return "NOK".
  
end PROCEDURE.
