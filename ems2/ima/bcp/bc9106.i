/***************************************************************************************
** Copyright DATASUL S.A. (1999)                                                      **
** Todos os Direitos Reservados.                                                      ** 
**                                                                                    **
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao                   **
** parcial ou total por qualquer meio, so podera ser feita mediante                   **
** autorizacao expressa.                                                              ** 
****************************************************************************************/
/***************************************************************************************
** Program   : bc9106.i                                                               **
** Purpose   : Faz a manutencao nas temp-tables das transacoes do coleta de           **
**             dados.                                                                 **
** Parameters: Variavel global vAcessType determina o tipo de acesso ao               **
**             programa.                                                              **
**             1 - Alteracao                                                          **
**             2 - Consulta                                                           **
** Notes     : None.                                                                  **
** Author    : Carlos Alberto Soares Pereira - 20/12/1999                             **
****************************************************************************************/

/* {&Table} Definitions ---                                                            */
{{&NameOfTTInclude}}

/* tt-erro Definitions ---                                                             */
{cdp/cd0666.i}

/* tt-trans Definitions ---                                                            */
{bcp/bcapi001.i}

/* tt-etiquera Definitions ---                                                         */
{bcp/bcapi002.i}

/* tt-prog-bc Definitions ---                                                          */
{bcp/bcapi004.i}

/* Definitions Procedures Proxy ---                                                    */
{bcp/bcproxy.i2}

/*************************************************/

/* Input-Output Parameters ---                                                         */
Define Input        Param       pNumTransacao            as inte         no-undo.
Define Input-output Param       pDesDetalhe              as char         no-undo.
Define Input-Output Param       pRawContent              as raw          no-undo.
Define Input-Output Param       pLogApaga                as logi         no-undo.

/* Global Variable Definitions ---                                                     */
Define New Global Shared Var vAcessType as integer                              no-undo.

/* Local Variable Definitions ---                                                      */
Define Variable wgWindow            as widget-handle                            no-undo.
Define Variable wgFocus             as widget-handle                            no-undo.

Define Variable vCodTransaction     as Character                                no-undo.

/* Rectangle Definitions ---                                                           */
Define Rectangle rtButton size 1 by 1.3  EDGE-PIXELS 2 GRAPHIC-EDGE BGCOLOR 7 ToolTip 'Modura dos Botoes. Obj:rtButton'.
Define Rectangle rtMoldur size 1 by .1   EDGE-PIXELS 2 GRAPHIC-EDGE NO-FILL   ToolTip 'Modura dos Campos. Obj:rtMoldur'.

/* Buttons Definitions ---                                                             */
Define button btOk                  Label '&Ok'         size 10 by 0.9 ToolTip 'Confirma e Sai do Programa. Obj:btOk'.
Define button btCancela             Label '&Cancela'    size 10 by 0.9 ToolTip 'Confirma e Sai do Programa. Obj:btCancela'.

/* Windows Definitions ---                                                             */
CREATE WINDOW wgWindow ASSIGN
    HIDDEN             = YES
    TITLE              = 'Transacao ' + '{&ProgramTitle}'
    HEIGHT             = 1
    WIDTH              = 1
    MAX-HEIGHT         = 1
    MAX-WIDTH          = 1
    VIRTUAL-HEIGHT     = 1
    VIRTUAL-WIDTH      = 1
    RESIZE             = no
    SCROLL-BARS        = no
    STATUS-AREA        = no
    BGCOLOR            = ?
    FGCOLOR            = ?
    THREE-D            = yes
    MESSAGE-AREA       = no
    SENSITIVE          = yes.

/* Frames Definitions ---                                                               */
Define Frame fFields 
            {&FrameFields}
        with {&FrameNumOfColumns} columns 1 down three-d font 2 no-box.

Define Frame fObjects 
             rtMoldur
             rtButton btOk at row 01 col 01 
             btCancela     at row 01 col 01
                with three-d font 1  no-box.

IF  vAcessType = 1
Then do:
    Find First bc-prog-trans no-lock 
         where bc-prog-trans.cod_prog_dtsul = '{&TransactionProgram}' No-error.

    If  Not AvailAble bc-prog-trans Then Do:
        Return Error.
    End.

    Assign vCodTransaction = bc-prog-trans.cd-trans.
End.

/* Triggers Definitions Begin ---                                                       */
&IF   '{&TriggerObject01}' <> '' &THEN
    ON {&TriggerEvent01} Of {&TriggerObject01} in Frame f{&TriggerFrame01}
    Do:
        {&TriggerCode01}
    End. 
&ENDIF
&IF   '{&TriggerObject02}' <> '' &THEN
    ON {&TriggerEvent02} Of {&TriggerObject02} in Frame f{&TriggerFrame02}
    Do:
        {&TriggerCode02}
    End. 
&ENDIF
&IF   '{&TriggerObject03}' <> '' &THEN
    ON {&TriggerEvent03} Of {&TriggerObject03} in Frame f{&TriggerFrame03}
    Do:
        {&TriggerCode03}
    End. 
&ENDIF
&IF   '{&TriggerObject04}' <> '' &THEN
    ON {&TriggerEvent04} Of {&TriggerObject04} in Frame f{&TriggerFrame04}
    Do:
        {&TriggerCode04}
    End. 
&ENDIF
&IF   '{&TriggerObject05}' <> '' &THEN
    ON {&TriggerEvent05} Of {&TriggerObject05} in Frame f{&TriggerFrame05}
    Do:
        {&TriggerCode05}
    End. 
&ENDIF
&IF   '{&TriggerObject06}' <> '' &THEN
    ON {&TriggerEvent06} Of {&TriggerObject06} in Frame f{&TriggerFrame06}
    Do:
        {&TriggerCode06}
    End. 
&ENDIF
&IF   '{&TriggerObject07}' <> '' &THEN
    ON {&TriggerEvent07} Of {&TriggerObject07} in Frame f{&TriggerFrame07}
    Do:
        {&TriggerCode07}
    End. 
&ENDIF
&IF   '{&TriggerObject08}' <> '' &THEN
    ON {&TriggerEvent08} Of {&TriggerObject08} in Frame f{&TriggerFrame08}
    Do:
        {&TriggerCode08}
    End. 
&ENDIF
&IF   '{&TriggerObject09}' <> '' &THEN
    ON {&TriggerEvent09} Of {&TriggerObject09} in Frame f{&TriggerFrame09}
    Do:
        {&TriggerCode09}
    End. 
&ENDIF
&IF   '{&TriggerObject10}' <> '' &THEN
    ON {&TriggerEvent10} Of {&TriggerObject10} in Frame f{&TriggerFrame10}
    Do:
        {&TriggerCode10}
    End. 
&ENDIF
&IF   '{&TriggerObject11}' <> '' &THEN
    ON {&TriggerEvent11} Of {&TriggerObject11} in Frame f{&TriggerFrame11}
    Do:
        {&TriggerCode11}
    End. 
&ENDIF
&IF   '{&TriggerObject12}' <> '' &THEN
    ON {&TriggerEvent12} Of {&TriggerObject12} in Frame f{&TriggerFrame12}
    Do:
        {&TriggerCode12}
    End. 
&ENDIF
&IF   '{&TriggerObject13}' <> '' &THEN
    ON {&TriggerEvent13} Of {&TriggerObject13} in Frame f{&TriggerFrame13}
    Do:
        {&TriggerCode13}
    End. 
&ENDIF
&IF   '{&TriggerObject14}' <> '' &THEN
    ON {&TriggerEvent14} Of {&TriggerObject14} in Frame f{&TriggerFrame14}
    Do:
        {&TriggerCode14}
    End. 
&ENDIF
&IF   '{&TriggerObject15}' <> '' &THEN
    ON {&TriggerEvent15} Of {&TriggerObject15} in Frame f{&TriggerFrame15}
    Do:
        {&TriggerCode15}
    End. 
&ENDIF

/* Triggers Definitions End   ---                                                       */

/* Aplica formatos customizados aos campos da tela */
Run CustomizeFormats.

/* Aplica propriedades customizadas aos campos da tela */
Run CustomizeFieldProperties.

/* Ajusta os objetos na tela ---                                                        */
Assign Frame fFields:Width   = Frame fFields:Width  + 2.0
       Frame fObjects:Width  = Frame fFields:Width  + 2.0
       Frame fObjects:Height = Frame fFields:Height + 2.0
       Frame fObjects:Row    = 1
       Frame fObjects:Col    = 1.6
       Frame fFields:Frame   = Frame fObjects:Handle
       Frame fFields:Row     = 1.3
       Frame fFields:Col     = 2.0
       wgWindow:Width        = Frame fObjects:Width  + 0.5
       wgWindow:Height       = Frame fObjects:Height + 0.2
       wgWindow:Max-Width    = Frame fObjects:Width  + 0.5
       wgWindow:Max-Height   = Frame fObjects:Height + 0.2
       rtMoldur:row         = 1.15
       rtMoldur:col         = 1.0
       rtMoldur:Width       = Frame fObjects:Width  - 0.5
       rtMoldur:Height      = Frame fObjects:Height - 1.65
       rtButton:row         = Frame fObjects:Height - 0.35
       rtButton:col         = 1.0
       rtButton:Width       = Frame fObjects:Width  - 0.5
       btOk:Row             = rtButton:Row          + 0.25
       btOk:Col             = rtButton:Col          + 0.8
       btCancela:Row        = rtButton:Row          + 0.25
       btCancela:Col        = btOk:Col              + 10.5.


/* Nomeia Titulo do programa conforme tipo de acesso */
Case vAcessType:
    when 1 then Assign wgWindow:Title = 'Modifica ' + wgWindow:Title. /* Alteracao   */
    when 2 then Assign wgWindow:Title = 'Consulta ' + wgWindow:Title. /* Consulta    */
    OtherWise   Assign wgWindow:Title = 'Consulta ' + wgWindow:Title. /* Consulta    */
End Case.

Assign wgWindow:Visible = Yes.

View Frame fObjects in Window wgWindow.

/* Habilita frame de acordo com o tipo de acesso */

Case vAcessType:
    when 1 then do:
        Run EnableToUpdate. /* Alteracao   */
        raw-transfer pRawContent to {&Table}.
        find first {&Table} no-error.
        Display {&FrameFields} with frame fFields no-error.
        Run DisplayCustomFields.
    End.
    when 2 then do:
        Run EnableToView.   /* Consulta    */
        raw-transfer pRawContent to {&Table}.
        find first {&Table} no-error.
        Display {&FrameFields} with frame fFields no-error.
        Run DisplayCustomFields.
    End.
    OtherWise   Run EnableToView.   /* Stand Alone */
End Case.


On End-Error Anywhere 
Do:
    Apply 'Close' To This-procedure.
End.

On Close of This-Procedure
Do:
    Assign wgWindow:visible = no.
    Delete widget wgWindow.
End.


On Choose of btCancela in frame fObjects 
Do:
    Apply 'Close' To This-Procedure.
End.


On Window-Close of wgWindow
Do:
    Apply 'Close' To This-Procedure.
End.


On Choose of btOK in frame fObjects 
Do:
    /* Verifica como proceder na saida do programa */
    Case vAcessType:
        when 1 then Do:
            Run LocalAssignFields.
            Assign pDesDetalhe = {&TransDetail}.
            Apply 'Close' To This-Procedure.
        End.
        when 2 then Do:
            Apply 'Close' To This-Procedure.
        End.
        OtherWise Do:
            Apply 'Close' To This-Procedure.
        End.
    End Case.
    Raw-transfer {&Table} to pRawContent no-error.
    Assign  pLogApaga   = yes
            pDesDetalhe = {&TransDetail}.

End.

Assign wgFocus = frame fFields:First-Child.

if wgWindow:Move-To-Top() Then.

Wait-for Close of This-Procedure Focus wgFocus.

Return.

Procedure DisplayCustomFields:
/*********************************************************************************************
** Purpose   : Faz Display de campos customizados pelo programador                          **
** Parameters: None                                                                         **
** Notes     : None.                                                                        **
**********************************************************************************************/
Assign 
        &IF  '{&CustomDisplayField01}' <> '' &THEN
             {&CustomDisplayField01}:screen-value in frame fFields = {&CustomValueField01}
        &ENDIF
        &IF  '{&CustomDisplayField02}' <> '' &THEN
             {&CustomDisplayField02}:screen-value in frame fFields = {&CustomValueField02}
        &ENDIF
        &IF  '{&CustomDisplayField03}' <> '' &THEN
             {&CustomDisplayField03}:screen-value in frame fFields = {&CustomValueField03}
        &ENDIF
        &IF  '{&CustomDisplayField04}' <> '' &THEN
             {&CustomDisplayField04}:screen-value in frame fFields = {&CustomValueField04}
        &ENDIF
        &IF  '{&CustomDisplayField05}' <> '' &THEN
             {&CustomDisplayField05}:screen-value in frame fFields = {&CustomValueField05}
        &ENDIF
        &IF  '{&CustomDisplayField06}' <> '' &THEN
             {&CustomDisplayField06}:screen-value in frame fFields = {&CustomValueField06}
        &ENDIF
        &IF  '{&CustomDisplayField07}' <> '' &THEN
             {&CustomDisplayField07}:screen-value in frame fFields = {&CustomValueField07}
        &ENDIF
        &IF  '{&CustomDisplayField08}' <> '' &THEN
             {&CustomDisplayField08}:screen-value in frame fFields = {&CustomValueField08}
        &ENDIF
        &IF  '{&CustomDisplayField09}' <> '' &THEN
             {&CustomDisplayField09}:screen-value in frame fFields = {&CustomValueField09}
        &ENDIF
        &IF  '{&CustomDisplayField10}' <> '' &THEN
             {&CustomDisplayField10}:screen-value in frame fFields = {&CustomValueField10}
        &ENDIF
            .
End Procedure.


Procedure LocalAssignFields:
/*********************************************************************************************
** Purpose   : Gravar os campos da tela.                                                    **
** Parameters: None                                                                         **
** Notes     : None.                                                                        **
**********************************************************************************************/
    Do With Frame fFields:
        Assign {&FrameFields}.
    End.
End Procedure.


Procedure EnableToUpdate:
/*********************************************************************************************
** Purpose   : Habilitar a frame para alteracao.                                            **
** Parameters: None                                                                         **
** Notes     : None.                                                                        **
**********************************************************************************************/
    Enable all with frame fFields.
    Enable btOK btCancela rtButton with frame fObjects.
End Procedure.


Procedure EnableToView:
/*********************************************************************************************
** Purpose   : Habilitar a frame somente para leitura.                                      **
** Parameters: None                                                                         **
** Notes     : None.                                                                        **
**********************************************************************************************/
    Enable btCancela rtButton with frame fObjects.
End Procedure.


Procedure CustomizeFormats:
/*********************************************************************************************
** Purpose   : Alterar o formado de campos da tela para um formato predefinido pelo         **
**             programador.                                                                 **
** Parameters: None                                                                         **
** Notes     : Definir pre-processor para cada campo.                                       **
**********************************************************************************************/
    Assign 
            &IF  '{&CustomFormatField01}' <> '' &Then
                  {&CustomFormatField01}:auto-resize in Frame fFields = yes
                  {&CustomFormatField01}:format      in Frame fFields = {&FieldFormat01}
            &ENDIF
            &IF   '{&CustomFormatField02}' <> '' &Then
                  {&CustomFormatField02}:auto-resize in Frame fFields = yes
                  {&CustomFormatField02}:format      in Frame fFields = {&FieldFormat02}
            &ENDIF
            &IF  '{&CustomFormatField03}' <> '' &Then
                  {&CustomFormatField03}:auto-resize in Frame fFields = yes
                  {&CustomFormatField03}:format      in Frame fFields = {&FieldFormat03}
            &ENDIF
            &IF  '{&CustomFormatField04}' <> '' &Then
                  {&CustomFormatField04}:auto-resize in Frame fFields = yes
                  {&CustomFormatField04}:format      in Frame fFields = {&FieldFormat04}
            &ENDIF
            &IF  '{&CustomFormatField04}' <> '' &Then
                  {&CustomFormatField04}:auto-resize in Frame fFields = yes
                  {&CustomFormatField04}:format      in Frame fFields = {&FieldFormat04}
            &ENDIF
            &IF  '{&CustomFormatField05}' <> '' &Then
                  {&CustomFormatField05}:auto-resize in Frame fFields = yes
                  {&CustomFormatField05}:format      in Frame fFields = {&FieldFormat05}
            &ENDIF
            &IF  '{&CustomFormatField06}' <> '' &Then
                  {&CustomFormatField06}:auto-resize in Frame fFields = yes
                  {&CustomFormatField06}:format      in Frame fFields = {&FieldFormat06}
            &ENDIF
            &IF  '{&CustomFormatField07}' <> '' &Then
                  {&CustomFormatField07}:auto-resize in Frame fFields = yes
                  {&CustomFormatField07}:format      in Frame fFields = {&FieldFormat07}
            &ENDIF
            &IF  '{&CustomFormatField08}' <> '' &Then
                 {&CustomFormatField08}:auto-resize in Frame fFields = yes
                  {&CustomFormatField08}:format      in Frame fFields = {&FieldFormat08}
            &ENDIF
            &IF  '{&CustomFormatField09}' <> '' &Then
                  {&CustomFormatField09}:auto-resize in Frame fFields = yes
                  {&CustomFormatField09}:format      in Frame fFields = {&FieldFormat09}
            &ENDIF
            &IF  '{&CustomFormatField10}' <> '' &Then
                  {&CustomFormatField10}:auto-resize in Frame fFields = yes
                  {&CustomFormatField10}:format      in Frame fFields = {&FieldFormat10}
            &ENDIF
            &IF  '{&CustomFormatField11}' <> '' &Then
                  {&CustomFormatField11}:auto-resize in Frame fFields = yes
                  {&CustomFormatField11}:format      in Frame fFields = {&FieldFormat11}
            &ENDIF
            &IF  '{&CustomFormatField12}' <> '' &Then
                  {&CustomFormatField12}:auto-resize in Frame fFields = yes
                  {&CustomFormatField12}:format      in Frame fFields = {&FieldFormat12}
            &ENDIF
            &IF  '{&CustomFormatField13}' <> '' &Then
                  {&CustomFormatField13}:auto-resize in Frame fFields = yes
                  {&CustomFormatField13}:format      in Frame fFields = {&FieldFormat13}
            &ENDIF
            &IF  '{&CustomFormatField14}' <> '' &Then
                  {&CustomFormatField14}:auto-resize in Frame fFields = yes
                  {&CustomFormatField14}:format      in Frame fFields = {&FieldFormat14}
            &ENDIF
            &IF  '{&CustomFormatField15}' <> '' &Then
                  {&CustomFormatField15}:auto-resize in Frame fFields = yes
                  {&CustomFormatField15}:format      in Frame fFields = {&FieldFormat15}
            &ENDIF
            .
End Procedure.

Procedure CustomizeFieldProperties:
/*********************************************************************************************
** Purpose   : Alterar as propriedades dos campos da tela conforme desejado pelos           **
**             programador.                                                                 **
** Parameters: None                                                                         **
** Notes     : Esta procedure foi implementada no final do desenvolvimento, entao para que  **
**             que nao houvesse retrabalho foram mantidas as procedures CustomizeFormats e  **
**             DisplayCustomFields que tem suas funcoes atendida por esta.                  **
**********************************************************************************************/
    Assign 
            &IF  '{&CustomFieldProperty01}' <> '' &Then
                  {&CustomFieldProperty01}:{&FieldProperty01} in Frame fFields = {&PropertyValue01} .
            &ENDIF
            &IF  '{&CustomFieldProperty02}' <> '' &Then
                  {&CustomFieldProperty02}:{&FieldProperty02} in Frame fFields = {&PropertyValue02} .
            &ENDIF
            &IF  '{&CustomFieldProperty03}' <> '' &Then
                  {&CustomFieldProperty03}:{&FieldProperty03} in Frame fFields = {&PropertyValue03} .
            &ENDIF
            &IF  '{&CustomFieldProperty04}' <> '' &Then
                  {&CustomFieldProperty04}:{&FieldProperty04} in Frame fFields = {&PropertyValue04} .
            &ENDIF
            &IF  '{&CustomFieldProperty05}' <> '' &Then
                  {&CustomFieldProperty05}:{&FieldProperty05} in Frame fFields = {&PropertyValue05} .
            &ENDIF
            &IF  '{&CustomFieldProperty06}' <> '' &Then
                  {&CustomFieldProperty06}:{&FieldProperty06} in Frame fFields = {&PropertyValue06} .
            &ENDIF
            &IF  '{&CustomFieldProperty07}' <> '' &Then
                  {&CustomFieldProperty07}:{&FieldProperty07} in Frame fFields = {&PropertyValue07} .
            &ENDIF
            &IF  '{&CustomFieldProperty08}' <> '' &Then
                  {&CustomFieldProperty08}:{&FieldProperty08} in Frame fFields = {&PropertyValue08} .
            &ENDIF
            &IF  '{&CustomFieldProperty09}' <> '' &Then
                  {&CustomFieldProperty09}:{&FieldProperty09} in Frame fFields = {&PropertyValue09} .
            &ENDIF
            &IF  '{&CustomFieldProperty10}' <> '' &Then
                  {&CustomFieldProperty10}:{&FieldProperty10} in Frame fFields = {&PropertyValue10} .
            &ENDIF
            &IF  '{&CustomFieldProperty11}' <> '' &Then
                  {&CustomFieldProperty11}:{&FieldProperty11} in Frame fFields = {&PropertyValue11} .
            &ENDIF
            &IF  '{&CustomFieldProperty12}' <> '' &Then
                  {&CustomFieldProperty12}:{&FieldProperty12} in Frame fFields = {&PropertyValue12} .
            &ENDIF
            &IF  '{&CustomFieldProperty13}' <> '' &Then
                  {&CustomFieldProperty13}:{&FieldProperty13} in Frame fFields = {&PropertyValue13} .
            &ENDIF
            &IF  '{&CustomFieldProperty14}' <> '' &Then
                  {&CustomFieldProperty14}:{&FieldProperty14} in Frame fFields = {&PropertyValue14} .
            &ENDIF
            &IF  '{&CustomFieldProperty15}' <> '' &Then
                  {&CustomFieldProperty15}:{&FieldProperty15} in Frame fFields = {&PropertyValue15} .
            &ENDIF
            .

End Procedure.
