Def New Global Shared Var c-seg-usuario as char format "x(12)" no-undo.
DEF NEW GLOBAL SHARED VAR lTraceDataCollection AS LOG NO-UNDO.

Procedure _GenerateDCTransaction:
    Define Input Param pCd-trans              As Character   Format "x(8)":U     No-undo.
    Define Input Param pConteudo-trans        As Raw                             No-undo.
    Define Input Param pDetalhe               As Character   Format "x(60)":U    No-undo.
    Define Input Param pUsuario               As Character   Format "x(12)":U    No-undo.

    Define Variable vLocalSeconds             As Integer                         No-undo.

    Assign vLocalSeconds = Int('{&ErrorDisplaySeconds}') No-error.

    If  vLocalSeconds = 0 Or 
        vLocalSeconds = ? Then Do:
        vLocalSeconds = 1.
    End.

    for each tt-erro-after-GenerateDC:
        delete tt-erro-after-GenerateDC.
    end.

    Assign vNumFrame = vNumFrame - 1.

    Hide All no-pause.

    &IF Defined(ShowMessageWaiting) 
    &THEN
    IF '{&ShowMessageWaiting}' = "YES"
    THEN
        Display {&MessageWaiting} At Row 01 Col 01 With Font 3 Size {&FrameSize} No-box.
    &ELSE
        Display 'Aguarde...' At Row 01 Col 01 With Font 3 Size {&FrameSize} No-box.
    &ENDIF

    Pause 0 No-message.

    Run 'bcp/bcapi010.p':U Persistent Set wgapi010.

    Create tt-bc-tipo-trans.
    Assign tt-bc-tipo-trans.cd-trans              = vTransaction
           tt-bc-tipo-trans.cod-versao-integracao = 001.

    Run Pi-bc-tipo-trans in wgapi010 (Input-Output  table tt-bc-tipo-trans,
                                      Input-Output  table tt-erro-tipo-trans).

    if valid-handle(wgapi010) then Delete Object wgapi010.

    Find First tt-erro-tipo-trans No-lock No-error.
    Find First tt-bc-tipo-trans   No-lock No-error.

    If  Available tt-erro-tipo-trans Then Do:
        Hide All no-pause.
        For Each tt-erro-tipo-trans No-lock:
            Run 'bcp/bc9115.p':U (Input tt-erro-tipo-trans.cd-erro,
                                  Input "ERRO: " + tt-erro-tipo-trans.mensagem,

                                  &IF Defined(FrameSizeError)
                                  &THEN
                                  Input Int(Entry(3,'{&FrameSizeError}',' ')), /*Linhas*/
                                  Input Int(Entry(1,'{&FrameSizeError}',' ')), /*Colunas*/
                                  &ELSE
                                  Input Int(Entry(3,'{&FrameSize}',' ')), /*Linhas*/
                                  Input Int(Entry(1,'{&FrameSize}',' ')), /*Colunas*/
                                  &ENDIF

                                  Input vLocalSeconds   /*Segundos*/).
        End.

        Hide All No-pause.
        Pause 0 No-message.

        For each tt-erro-tipo-trans:

            create tt-erro-after-GenerateDC.
            buffer-copy tt-erro-tipo-trans to tt-erro-after-GenerateDC.

            Delete tt-erro-tipo-trans.
        End. /* for each tt-erro-tipo-trans */

        Return Error.
    End. /* if avail tt-erro-tipo-trans */

    IF  lTraceDataCollection THEN DO:

        IF SUBSTRING(SESSION:TEMP-DIRECTORY,LENGTH(SESSION:TEMP-DIRECTORY),1) = "/"
        OR SUBSTRING(SESSION:TEMP-DIRECTORY,LENGTH(SESSION:TEMP-DIRECTORY),1) = "\"
        THEN
            OUTPUT TO value(SESSION:TEMP-DIRECTORY + "Trace-dc-" + pCd-trans + ".TXT") APPEND.
        ELSE 
            OUTPUT TO value(SESSION:TEMP-DIRECTORY + "/" + "Trace-dc-" + pCd-trans + ".TXT") APPEND.

        PUT UNFORMATTED SKIP ">>> " STRING(TODAY,"99/99/9999") " - " STRING(TIME,"HH:MM:SS") " INICIO ATIALIZACAO "  SKIP.

    END.

    Create  tt-trans.
    Assign  
            tt-trans.i-sequen               = 1
            tt-trans.cd-trans               = pCd-trans
            tt-trans.detalhe                = pDetalhe
            tt-trans.usuario                = pUsuario
            tt-trans.conteudo-trans         = pConteudo-trans
            tt-trans.atualizada             = No
            tt-trans.etiqueta               = vLogEtiqueta.


    &IF Defined(UsingBcTransfilho)
    &THEN

         &IF '{&UsingBcTransfilho}' = 'NO'
         &THEN
         ASSIGN tt-trans.cod-versao-integracao  = 1.
         Run bcp/bcapi001.p (input-output table tt-trans,
                             input-output table tt-erro{&Ext}).

         &ELSE

         ASSIGN tt-trans.cod-versao-integracao  = 1.

         IF VALID-HANDLE(H_BCAPI001) = NO
         THEN
            run bcp/bcapi001.p persistent set H_BCAPI001 (input-output table tt-trans,
                                                          input-output table tt-erro). 

         RUN atualizaTransacao in H_BCAPI001 (INPUT-OUTPUT TABLE tt-trans,
                                              INPUT-OUTPUT TABLE tt-trans-ext,
                                              INPUT-OUTPUT TABLE tt-trans-filho,
                                              INPUT-OUTPUT TABLE tt-erro).

         EMPTY TEMP-TABLE TT-TRANS-EXT.
         EMPTY TEMP-TABLE TT-TRANS-FILHO.

        &ENDIF

    &ELSE
         ASSIGN tt-trans.cod-versao-integracao  = 1.
         Run bcp/bcapi001.p (input-output table tt-trans,
                             input-output table tt-erro{&Ext}).

    &ENDIF

    For Each tt-trans:
        Delete tt-trans.
    End.

    Find First tt-erro{&Ext} No-error.

    IF  lTraceDataCollection THEN DO:


        PUT UNFORMATTED SKIP ">>> " STRING(TODAY,"99/99/9999") " - " STRING(TIME,"HH:MM:SS") " FIM ATIALIZACAO " SKIP(1).

        OUTPUT CLOSE.

    END.

    If  Available tt-erro{&Ext}    And 
        Available tt-bc-tipo-trans And
        tt-bc-tipo-trans.erros-on-line Then Do:
        Hide All no-pause.
        For Each tt-erro{&Ext} No-lock:
            Run 'bcp/bc9115.p':U (Input tt-erro{&Ext}.cd-erro,
                                  Input "ERRO: " +  tt-erro{&Ext}.mensagem,

                                  &IF Defined(FrameSizeError)
                                  &THEN
                                  Input Int(Entry(3,'{&FrameSizeError}',' ')), /*Linhas*/
                                  Input Int(Entry(1,'{&FrameSizeError}',' ')), /*Colunas*/
                                  &ELSE
                                  Input Int(Entry(3,'{&FrameSize}',' ')), /*Linhas*/
                                  Input Int(Entry(1,'{&FrameSize}',' ')), /*Colunas*/
                                  &ENDIF

                                  Input vLocalSeconds   /*Segundos*/).
        End.

        Hide All No-pause.
        Pause 0 No-message.

    End. /* if avail tt-erro{&Ext} */
    For each tt-erro{&Ext}:

        create tt-erro-after-GenerateDC.
        buffer-copy tt-erro{&Ext} to tt-erro-after-GenerateDC.

        Delete tt-erro{&Ext}.
    End. /* for each tt-erro{&Ext} */

    Hide All No-pause.
    Pause 0 No-message.


    If Can-find(first tt-erro-after-GenerateDC) Then Return 'NOK'.

End Procedure.

/************************************************************************************************************/
Procedure _RetornaUsuario:
Define OutPut Param vNomeUsuario    as Char No-Undo.

Find First Param-Bc No-Lock No-Error.

If  Avail Param-Bc
/* **** Erro detectado na Vipal - campo ind-tipo-integracao nao deve ser utilizado *****
&IF "{&mgcld_version}" >= "2.02" &THEN
And Param-Bc.Ind-Tipo-Integracao <> 1 /* EMS */
&ELSE
*/
And Param-Bc.Int-2 <> 1 /* EMS */
/* &ENDIF */
Then do:
    /* Solicitacao Vanessa - Alteracao Assistencia Tecnica */
    if c-seg-usuario = "" then
        Assign vNomeUsuario = userid("mgadm").
    else
        assign vNomeUsuario = c-seg-usuario.
end.
Else
    Assign vNomeUsuario = c-seg-usuario.

End Procedure.
