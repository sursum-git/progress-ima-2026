/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
/********************************************************************************************
**   Programa..: bc9008.p                                                                  **
**                                                                                         **
**   Versao....: 2.00.00.000 - jun/2000 - John Cleber Jaraceski                            **
**                                                                                         **
**   Objetivo..: Template DB Interface para transacao de Aviso de Embarque                 **
**                                                                                         **
**   Includes..: bc9105.i, bc9101.i, bc9100.i, bc9008.i                                    **
**                                                                                         **
********************************************************************************************/

/***************************************************************************************************
** SECAO DE PRE-PROCESSADORES DA TEMPLATE                                                         **
** Nesta secao sao definidos os pre-processadores que serao usados na montagem da interface.      **
**                                                                                                **
** DESCRICAO DOS PREPROCESSADORES:                                                                **
** ProgramName          - Nome do programa e, tambem, do Codigo da transacao do Data Collection   **
**                        que sera acionada pela interface.                                       **
** TempTable            - Nome da Temp-Table da transacao de negocio para comunicacao com ERP.    **
**                        Ex: tt-transfere-bc.                                                    **
** FrameSize            - Tamanho da tela da interface. Deve-se observar este parametro quando da **
**                        definicao das telas, o espaco utilizado nao pode ultrapassar o limite   **
**                        estabelecido por este pre-processador.                                  **
** Frame99Name          - Nome da Tela. Sugestao "Frame" mais sequencia.                          **
**                        Ex. Frame01, Frame02, Frame03, etc.                                     **
** Frame99Defs          - Definicoes da tela onde a mesma deve conter:                            **
**                        .Titulo da Tela quando necessario.                                      **
**                        .Campos com a opcao no-label e sua localizacao Ex. At Row x Col y.      **
**                        .Literais que substituem os labels dos campos e sua localizacao.        **
**                        .Instrucoes de Navegacao quando houver espaco livre para o mesmo.       **
**                        Ex. "F4=Sair","ESC=Voltar","F2=Gravar" etc.                             **
** Frame99Repeat        - Indica se a tela tem caracteristica de repeticao. O Valor YES indica    **
**                        que ha repeticao e o valor NO indica que nao ha repeticao.              **
** Update99Fields       - Relaciona os campos que serao solicitados na tela.                      **
**                        OBS. Nao deve conter formatacao. Ex. ttWork.cod-item no-label.         **
** TriggerBeforeFrame99 - Gatilho para ser executado antes da execucao da tela. Neste gatilho     **
**                        devem ser colocadas chamadas as procedures de inicializacao dos campos  **
**                        da tela.                                                                **
** TriggerAfterFrame99  - Gatilho para ser executado apos a execucao da tela. Neste gatilho       **
**                        dever ser colocada a chamada a procedures que devera armazenar  na      **
**                        temp-table {&TempTable} os campos da ttWork, que sao os campos          **
**                        solicitados em tela. Neste gatilho tambem, quando o mesmo se referir a  **
**                        ultima tela de entrada de dados, devera existir uma chamada a procedure **
**                        que executa a procedure _GenerateDCTransaction que eï responsavel por   **
**                        gerar a transacao no Coleta de Dados.                                   **
** UserTriggers         - Este pro-processador destina-se as triggers customizadas do usuario.    **
**                        Ex: Alimentar um campo da dela conforme o informado em outro campo.     **
**                            On Leave of ttWork.cod-depos in Frame Frame02                       **
**                            Do:                                                                 **
**                                Case ttWork.cod-depos:                                          **
**                                  When 'Pro' Then Assign ttWork.cod-local = 'Pro01'.            **
**                                  When 'Alm' Then Assign ttWork.cod-local = 'Alm01'.            **
**                                  When 'Exp' Then Assign ttWork.cod-local = 'Exp01'.            **
**                            End.                                                                **
***************************************************************************************************/

/* Definicao global do nome da transacao ---                */
&global-define ProgramName bcx0003b
/************************************************************/

/* Definicao da temp-table de integracao ---                */
&global-define TempTable tt-embarque-bc
{bcp/bc9008.i " "}              
Define Temp-table ttWork No-Undo Like {&TempTable}.
Create ttWork.
{bcp/bcx1000.i}
/************************************************************/

/* Propriedades globais para frames ---                     */              
&global-define FrameSize    50 By 15
/************************************************************/
DEF VAR h_bcx1000   AS HANDLE NO-UNDO.  
DEF VAR h_bcx014bo  AS HANDLE NO-UNDO.

DEF VAR C-EMBARQUE-RESUMO AS CHAR FORMAT "X(10)" NO-UNDO.
DEF VAR C-ITEM            AS CHAR FORMAT "X(18)" NO-UNDO.
DEF VAR C-ITEM-FALTA      AS CHAR FORMA  "X(16)" NO-UNDO.
DEF VAR DE-QT-LIDO        AS DEC  DECIMALS 10    NO-UNDO.
DEF VAR DE-QT-TOTAL       AS DEC  DECIMALS 10    NO-UNDO. 
DEF VAR L-ETIQUETA        AS LOG                 NO-UNDO.
DEF VAR l-retorna         AS LOG                 NO-UNDO.
DEF VAR c-cod-estabel     AS CHAR                NO-UNDO.
DEF VAR R-IT-PRE-FAT      AS CHAR                NO-UNDO.
DEF VAR i-prioridade      AS INT                 NO-UNDO.
DEF VAR i-ult-prioridade  AS INT                 NO-UNDO.
DEF VAR C-MSG             AS CHAR                NO-UNDO.
DEF VAR de-qt-at          AS DEC FORMAT "99999.9999" NO-UNDO INIT 0.
DEF VAR de-qt-pd          AS DEC FORMAT "99999.9999" NO-UNDO INIT 0.

DEF VAR c-arq-texto AS CHAR.
DEF VAR OKpressed AS LOGICAL INITIAL TRUE.

DEF STREAM s-coletor.
DEF STREAM e-coletor.
DEF STREAM s-bat.


/***************************************** Frames Inicio ******************************************/
/* Definicao da Frame01 ---                                 */
&global-define Frame01Name   Frame01
&global-define Frame01Defs   'DESEMBARCAR '          At Row 01 Col 01          ~
                             'EMBARQUE:'               At Row 02 Col 01          ~
                             C-EMBARQUE-RESUMO         At Row 03 Col 01 FORMAT "x(16)" No-Label ~
                             'RESUMO:'                 At Row 04 Col 01          ~
                             ttWork.NR-RESUMO          At Row 04 Col 10 FORMAT ">,>>9" No-Label ~
                             'PRIORIDADE:'             AT ROW 05 COL 01 ~
                             i-prioridade              AT ROW 05 COL 12 FORMAT ">>>9"  NO-LABEL 

&global-define Frame01Repeat  No
/************************************************************/

/* Definicao da Frame02 ---                                 */
&global-define Frame02Name   Frame02
&global-define Frame02Defs   'DESEMBARCAR '     At Row 01 Col 01          ~
                             'EMBARQUE:'          At Row 02 Col 01          ~
                             ttWork.NUM-AVISO-EMBARQUE  At Row 02 Col 10 FORMAT ">>>,>>9" No-label ~
                             'RESUMO:'            At Row 03 Col 01          ~
                             ttWork.NR-RESUMO     At Row 03 Col 08 FORMAT ">>>,>>9" No-label ~
                             'CODIGO DO ITEM:'    At Row 04 Col 01          ~
                             C-ITEM               At Row 05 Col 01 No-label ~
                             '< F5 > Ler Coletor    < F6 > Ler Arquivo Texto' At Row 06 Col 01    ~
                             'IT:'                At Row 07 Col 01          ~
                             ttWork.cod-item      At Row 07 Col 04 No-Label ~
                             'RF:'                At Row 08 Col 01          ~
                             ttWork.cod-REFER     At Row 08 Col 04 No-Label ~
                             'QT PD:'             At Row 09 Col 01          ~
                             de-qt-pd             At Row 09 Col 07 NO-LABEL ~
                             'QT AT:'             At Row 10 Col 01          ~
                             de-qt-at             At Row 10 Col 07 NO-LABEL ~

&global-define Frame02Repeat Yes
/************************************************************/

/* Definicao dos campos a serem recebidos ---               */
&global-define Update01Fields C-EMBARQUE-RESUMO             
&global-define Update02Fields C-ITEM 
/************************************************************/

/* Definicao das trigger de interacao com a tela ---        */ 
&global-define TriggerBeforeFrame01 Run InicializaCamposFrame01. 
&global-define TriggerBeforeFrame02 IF l-retorna THEN DO: HIDE ALL NO-PAUSE. ASSIGN l-retorna = NO. LEAVE _frame02. END. ELSE Run InicializaCamposFrame02. 
&global-define TriggerAfterFrame01  Run GravaCamposFrame01. 
&global-define TriggerAfterFrame02  Run GravaCamposFrame02. ~
                                    Run GravaTransacao.
/************************************************************/

&global-define TriggersOpenProgram RUN bcp/bcx1000.p PERSISTENT SET h_bcx1000.                                                  

&global-define TriggersCloseProgram IF VALID-HANDLE(h_bcx1000) THEN DO:  DELETE OBJECT h_bcx1000. END.


/* Definicao das trigger de usuario ---                     */ 
&global-define UserTriggers        
/************************************************************/

/*****************************************   Frames Fim ******************************************/

/**************************************************************************************************
** SECAO DO CODIGO PRINCIPAL DO PROGRAMA                                                         **
** Esta secao contem includes com codigos de execucao das interfaces.                      .     **
** Nao eï necessario efetuar alteracoes nesta sessao.                                            **
***************************************************************************************************/
PROCEDURE FindWindowA EXTERNAL "USER32.DLL":
    DEFINE INPUT  PARAMETER intClassName AS LONG.
    DEFINE INPUT  PARAMETER chrCaption   AS CHARACTER.
    DEFINE RETURN PARAMETER intHandle    AS LONG.
END PROCEDURE.

ON 'F5':U ANYWHERE DO:
    IF SELF:NAME <> "c-item" THEN NEXT.

    RUN pi-ler-coletor.
    APPLY 'end-error' TO SELF.
END.

ON 'F6' ANYWHERE DO:
   IF SELF:NAME <> "c-item" THEN NEXT.

   ASSIGN c-arq-texto = ''.
   SYSTEM-DIALOG GET-FILE c-arq-texto
       TITLE      "Informe Arquivo Texto para Importar..."
       FILTERS    "Arquivos Coletor (*.txt)" "*.txt"
       MUST-EXIST
       USE-FILENAME
       UPDATE OKpressed.

   IF OKpressed = NO OR c-arq-texto = '' THEN
      NEXT.

   RUN pi-ler-arquivo.

   APPLY 'end-error' TO SELF.
END.


{bcp/bc9100.i} /* Gerador da interface caracter do coleta de dados */
{bcp/bc9101.i} /* Gerador de transa‡Æo do coleta de dados          */

/**************************************************************************************************/

/************************************* Codigo do Usuario Inicio ************************************
** Este local ‚ destinado ao codigo do usuario.                                                   **
** Para efeitos de escalabilidade entre versoes de produto recomenda-se que o acesso as tabelas   **
** do ERP seja feita atraves de um proxy, caso contrario poderao haver retrabalhos na migracao    **
****************************************************************************************************/

/*************************************************************************************************** 
** Esta procedure esta inicializando o campo estabelecimento da tela Frame01 com o valor 1        **
** Os outros campos nao estao sendo inicializados pois os mesmos devem apresentar valor           **
** caso seja retornado a proxima tela para esta.                                                  **
** Esta procedure eï executada pelo pre-processador {&TriggerBeforeFrame01}.                      **
****************************************************************************************************/
Procedure InicializaCamposFrame01:
    HIDE ALL NO-PAUSE.
    Assign  C-EMBARQUE-RESUMO         = ""
            ttWork.num-aviso-embarque = 0
            ttWork.nr-resumo          = 0
            i-prioridade              = 0.  
End Procedure.

/*************************************************************************************************** 
** Esta procedure esta inicializando os campos da tela Frame02 com valores em branco              **
** Esta procedure eï executada pelo pre-processador {&TriggerBeforeFrame02}.                      **
****************************************************************************************************/
Procedure InicializaCamposFrame02:
    HIDE ALL NO-PAUSE.
    Assign  C-ITEM              = '' 
            ttwork.chave-unica  = "".

    DISP ttWork.NUM-AVISO-EMBARQUE ttWork.NR-RESUMO
         de-qt-at de-qt-pd
         ttwork.cod-item ttwork.cod-refer
         WITH FRAME frame02.

End Procedure.

/*************************************************************************************************** 
** Esta procedure esta armazenando na temp-table {&Temp-Table} os valores recebidos por ttWork    **
** na tela Frame 01.                                                                              **
** Esta procedure eï executada pelo pre-processador {&TriggerAfterFrame01}.                       **
****************************************************************************************************/
Procedure GravaCamposFrame01:       

DEF VAR h_bcx14bo AS HANDLE NO-UNDO.

/* Validacoes Frame 01 Inicio --- */
    HIDE ALL NO-PAUSE.
    
    Assign vLogErro                  = No
           ttWork.num-aviso-embarque = INT(SUBSTRING(C-EMBARQUE-RESUMO,1,6)).
/*            ttWork.NR-RESUMO          = INT(SUBSTRING(C-EMBARQUE-RESUMO,7,6))   */
/*            i-prioridade              = INT(SUBSTRING(C-EMBARQUE-RESUMO,13,4)). */

    If  ttWork.num-aviso-embarque = 0 Then do:
        Assign vLogErro = Yes.
        {bcp/bc9105.i "0" "Numero do Aviso de Embarque deve ser informado"}
        RETURN ERROR.
    End.
                                    
    if  ttWork.NR-RESUMO = 0 then do on endkey undo,return error:
    
        UPDATE ttWork.NR-RESUMO 
               WITH FRAME frame01.
                                  
    end.
    
    /*                                  
    If  ttWork.nr-resumo = 0 Then do:
         Assign vLogErro = Yes.
         {bcp/bc9105.i "0" "Numero do Resumo do Embarque deve ser informado"}
        
         RETURN ERROR.
    End.
    */

    /* BUSCA EMBARQUE */
    FOR EACH TT-EMBARQUE:
        DELETE TT-EMBARQUE.
    END.

    RUN pi-busca-embarque IN h_bcx1000 (ttwork.num-aviso-embarque,OUTPUT TABLE TT-EMBARQUE).
    
    IF  RETURN-VALUE = 'NOK' THEN DO:
        Assign vLogErro = Yes.

        {bcp/bc9105.i "0" "EMBARQUE NAO EXISTE"}
        
        Return Error.
    END.               

    FIND FIRST TT-EMBARQUE.

    /* BUSCA EMBARQUE */

    IF  ttwork.nr-resumo <> 0 THEN DO:
    
        RUN pi-busca-RESUMO IN h_bcx1000 (ttwork.num-aviso-embarque,ttwork.nr-resumo).
    
        IF  RETURN-VALUE = 'NOK' THEN DO:
            Assign vLogErro = Yes.
    
            {bcp/bc9105.i "0" "RESUMO NAO EXISTE"}
            
            Return Error.
        END.                    
    END.

    /* EMBARQUE ABERTO OU FECHADO */
    RUN PI-EMBARQUE-ABERTO IN h_bcx1000 (ttwork.num-aviso-embarque,ttwork.nr-resumo).

    IF  RETURN-VALUE = 'NOK' THEN DO:
        Assign vLogErro = Yes.
        {bcp/bc9105.i "0" "EMBARQUE JA ESTA FECHADO"}
        
        Return Error.
    END.                     
    
    /*
    /* EMBARQUE ABERTO OU FECHADO */
    RUN PI-EMBARQUE-ABERTO IN h_bcx1000 (ttwork.num-aviso-embarque,ttwork.nr-resumo).

    IF  RETURN-VALUE = 'NOK' THEN DO:
        Assign vLogErro = Yes.
        {bcp/bc9105.i "0" "EMBARQUE JA ESTA FECHADO"}
        
        Return Error.
    END.                       

    /* EMBARQUE Jµ TOTALMENTE ATENDIDO */
                               
    RUN PI-ITEM-ATENDIDO IN h_bcx1000 (ttwork.num-aviso-embarque,
                                       (IF ttwork.nr-resumo = 0 THEN 0 ELSE ttwork.nr-resumo),
                                       (IF ttwork.nr-resumo = 0 THEN 999999 ELSE ttwork.nr-resumo),
                                       "", 
                                       "ZZZZZZZZZZZZZZZZZZZZ", 
                                       "",
                                       "ZZZZZZZZZZZZZZZZZZZZ",
                                       OUTPUT C-ITEM-FALTA, 
                                       OUTPUT DE-QT-TOTAL,
                                       OUTPUT DE-QT-LIDO).

    IF  RETURN-VALUE = 'OK' THEN DO:
         {bcp/bc9105.i "0" "ALERTA - EMBARQUE JA ESTA TOTALMENTE ATENDIDO - TECLE <ENTER>"}
    END.                    
                                 
    /********************************/
    
    IF  i-prioridade = 0  THEN DO:

        UPDATE i-prioridade 
               WITH FRAME frame01.

    END.
    

    RUN bcp/bcx014bo.p PERSISTENT SET h_bcx014bo.

    RUN pi-busca-prioridade IN h_bcx014bo (INPUT ttwork.num-aviso-embarque,
                                           INPUT ttwork.nr-resumo,
                                           OUTPUT i-ult-prioridade).

    IF  i-ult-prioridade = 0 THEN DO:

        DELETE OBJECT h_bcx014bo.
    
        ASSIGN vLogErro = YES
        C-MSG =  'EMBARQUE NAO FOI LIBERADO PARA EMBARQUE - NAO FOI DEFINIDA SUA PRIORIDADE' .
        {bcp/bc9105.i "0" " " " " "+ C-MSG" }
        Return Error.            
    END.
    
    IF  i-prioridade <> i-ult-prioridade THEN DO:

        DELETE OBJECT h_bcx014bo.

        ASSIGN vLogErro = YES
        C-MSG =  'FOI ALTERADA A PRIORIDADE DO EMBARQUE - ANTERIOR (' 
               + STRING(I-PRIORIDADE) 
               + ') - NOVA (' 
               + STRING(I-ULT-PRIORIDADE) 
               + ') - ENTRAR EM CONTATO COM FATURAMENTO'.

         {bcp/bc9105.i "0" " " " " "+ C-MSG" }
         Return Error.            
    END.
    
    DELETE OBJECT h_bcx014bo.            
    
    */
    
    If   vLogErro = Yes Then Return Error.
    /* Validacoes Frame 01 Fim    --- */

    Assign  {&TempTable}.num-aviso-embarque = ttWork.num-aviso-embarque
            {&TempTable}.nr-resumo          = ttWork.nr-resumo
            .

End Procedure.

/*************************************************************************************************** 
** Esta procedure esta armazenando na temp-table {&Temp-Table} os valores recebidos por ttWork    **
** na tela Frame 02.                                                                              **
** Esta procedure eï executada pelo pre-processador {&TriggerAfterFrame02}.                       **
****************************************************************************************************/
Procedure GravaCamposFrame02:
DEF VAR l-etiq-lida AS LOG NO-UNDO.
DEF VAR C-MSG AS CHAR NO-UNDO.
DEF VAR c-documento AS CHAR NO-UNDO.
DEF VAR C-COD-ESTABEL-ETQ AS CHAR NO-UNDO.
DEF VAR C-DESC-ESTADO-ETIQ AS CHAR NO-UNDO.
DEF VAR i-nr-embarque AS INT NO-UNDO.
DEF VAR i-nr-resumo   AS INT NO-UNDO.

    HIDE ALL NO-PAUSE.
    Assign vLogErro                  = No.
    
    /** CASO SEJA UM CODIDOG DA ETIQEUTA **/

    FOR EACH TT-CAMPO:
        DELETE TT-CAMPO.
    END.
    
    RUN PI-BUSCA-ETIQUETA IN h_bcx1000 (INPUT  C-ITEM, 
                                        OUTPUT L-ETIQUETA, 
                                        OUTPUT TABLE TT-CAMPO). 
    
    IF RETURN-VALUE = "NOK" 
    THEN DO:
        Assign vLogErro = Yes.
        C-MSG = "ETIQUETA (" + c-item + ") NAO ENCONTRADA".
        {bcp/bc9105.i "0" " " " " " + C-MSG" }
        Return Error.
    END.
    
    FOR EACH TT-CAMPO:

        IF TT-CAMPO.CAMPO = "DESC-ESTADO"
        THEN DO: 
        
            ASSIGN C-DESC-ESTADO-ETIQ = TT-CAMPO.CARACTER.

        END.

        IF TT-CAMPO.CAMPO = "IT-CODIGO" THEN DO:

           ASSIGN ttwork.cod-item = TT-CAMPO.CARACTER
                  .

        END.

        IF TT-CAMPO.CAMPO = "REFERENCIA" THEN DO:

           ASSIGN ttwork.cod-refer     = TT-CAMPO.CARACTER
                  .

        END.

        IF TT-CAMPO.CAMPO = "LOTE" THEN DO:

           ASSIGN ttwork.cod-lote      = TT-CAMPO.CARACTER
                  .

        END.

        IF TT-CAMPO.CAMPO = "QTD-PECA" THEN DO:

           ASSIGN ttwork.qtd-embarcada = TT-CAMPO.DECIMAL
                  .

        END.
        
        IF  TT-CAMPO.CAMPO = "CHAVE-UNICA" THEN DO:

            ASSIGN ttwork.chave-unica = TT-CAMPO.caracter.

        END.

        IF  TT-CAMPO.CAMPO = "COD-ESTABEL" THEN DO:

            ASSIGN C-COD-ESTABEL-ETQ = TT-CAMPO.caracter.
        
        END.

        IF  TT-CAMPO.CAMPO = "NR-EMBARQUE" THEN DO:

            ASSIGN I-NR-EMBARQUE = TT-CAMPO.INTEIRO.
        
        END.

        IF  TT-CAMPO.CAMPO = "NR-RESUMO" THEN DO:

            ASSIGN I-NR-RESUMO = TT-CAMPO.INTEIRO.
        
        END.
        
    END.
        

    RUN PI-ITEM-ATENDIDO IN h_bcx1000 (ttwork.num-aviso-embarque,
                                       (IF ttwork.nr-resumo = 0 THEN 0      ELSE ttwork.nr-resumo),
                                       (IF ttwork.nr-resumo = 0 THEN 999999 ELSE ttwork.nr-resumo),
                                       ttwork.cod-item, 
                                       ttwork.cod-item,
                                       ttwork.cod-refer,
                                       ttwork.cod-refer,
                                       OUTPUT C-ITEM-FALTA, 
                                       OUTPUT DE-QT-TOTAL,
                                       OUTPUT DE-QT-LIDO).
    ASSIGN de-qt-at = DE-QT-LIDO
           de-qt-pd = DE-QT-TOTAL.
    
    DISP ttwork.cod-item
         ttwork.cod-refer
         de-qt-at 
         de-qt-pd
         WITH FRAME frame02.
    
    FOR EACH TT-CAMPO:
        
        IF  TT-CAMPO.CAMPO   =  "COD-ESTADO" 
        AND TT-CAMPO.INTEIRO <> 3 /* SEPARADA */
        THEN DO:

            ASSIGN vLOGerro = YES.           
            C-MSG = "SITUACAO DA ETIQUETA (" + c-item + ") NAO PERMITE MOVIMENTACAO (SITUACAO:" + C-DESC-ESTADO-ETIQ + ")".
            {bcp/bc9105.i "0" " " " " "+ C-MSG" }
            Return Error.
        END.
    END.
    
    IF  C-COD-ESTABEL-ETQ <> TT-EMBARQUE.COD-ESTABEL
    THEN DO:
        Assign vLogErro = YES
               C-MSG = "ESTABELICIMENTO DA ETIQUETA (" + c-item + ") DIRERENTE DO EMBARQUE (EST ETQ:" + C-COD-ESTABEL-ETQ + " - EST EMB: "
                       + TT-EMBARQUE.COD-ESTABEL + ")".
    
        {bcp/bc9105.i "0" " " " " "+ C-MSG" }
        Return Error.
    END.

    IF I-NR-EMBARQUE <> ttwork.num-aviso-embarque
    THEN DO:

        Assign vLogErro = YES
              C-MSG = "ETIQUETA (" + c-item + ") NAO PERTENCE A ESTE EMBARQUE - (EMB ETQ: "
                      + STRING(I-NR-EMBARQUE) + ")".

        {bcp/bc9105.i "0" " " " " "+ C-MSG" }
        Return Error.

    END.

    IF  I-NR-RESUMO    <> ttwork.NR-RESUMO
    AND ttwork.nr-resumo <> 0
    THEN DO:
        Assign vLogErro = YES
               C-MSG = "ETIQUETA (" + c-item + ") NAO PERTENCE A ESTE RESUMO - (RES ETQ: "
                       + STRING(I-NR-RESUMO) + ")".

        {bcp/bc9105.i "0" " " " " "+ C-MSG" }
        Return Error.
    END.

    IF  DE-QT-LIDO = 0 
    THEN DO:

        Assign vLogErro = YES
        C-MSG = "ITEM " + ttwork.cod-item + " TOTALMENTE DESEMBARCADO".

        {bcp/bc9105.i "0" " " " " "+ C-MSG" }
        Return Error.

    END.

    IF  ttwork.qtd-embarcada = 0 THEN DO:
        Assign vLogErro = Yes.        
        
        {bcp/bc9105.i "0" "QUANTIDADE EMBARCADA DEVE SER DIFERENTE DE ZERO"}

        Return Error.
        
    END.
    
    IF  ttwork.qtd-embarcada > DE-QT-LIDO 
    OR  DE-QT-LIDO = 0 
    THEN DO:
        ASSIGN vLogErro = YES
               C-MSG =  'QUANTIDADE INFORMADA EXEDEU A QUANTIDADE EMBARCADA (QT EMBARCADA:' 
                      + STRING(DE-QT-LIDO)
                      + ')'.

        {bcp/bc9105.i "0" " " " " "+ C-MSG" }
        Return Error.
    END.
             
    Assign  {&TempTable}.cod-item      = ttWork.cod-item
            {&TempTable}.cod-lote      = ttWork.cod-lote
            {&temptable}.cod-refer     = ttwork.cod-refer
            {&TempTable}.qtd-embarcada = ttWork.qtd-embarcada
            {&TempTable}.chave-unica   = ttWork.chave-unica.

    ASSIGN de-qt-at = de-qt-at - ttwork.qtd-embarcada.

    DISP de-qt-at
         de-qt-pd
         WITH FRAME frame02.

            
End Procedure.

/*************************************************************************************************** 
** Esta procedure esta gerando a transacao no Data Collection atraves da chamada a procedure      **
** _GenerateDCTransaction.                                                                        **
** Esta procedure eï executada pelo pre-processador {&TriggerAfterFrame02}.                       **
****************************************************************************************************/
Procedure GravaTransacao:
    Def Var vNomeUsuario            As Character                No-undo.
    HIDE ALL NO-PAUSE.
    If   vLogErro = Yes Then Return Error.
   
    Assign vTransDetail =  "PRG:"  + ttWork.chave-unica 
                         + "ITEM:" + {&TempTable}.cod-item 
                         + "QTD:"  + STRING({&TempTable}.qtd-embarcada) 
                         + "EMB:"  + String({&TempTable}.num-aviso-embarque) 
                         + "RES:"  + String({&TempTable}.nr-resumo).

    Run _RetornaUsuario (Output vNomeUsuario).

    Assign {&TempTable}.data-transacao = Today
           {&TempTable}.cod-livre-1    = STRING(TIME,"hh:mm:ss")
           {&TempTable}.cod-usuario    = vNomeUsuario
           l-retorna                   = NO.

    Raw-transfer {&TempTable} To vConteudoRaw.

    Run _GenerateDCTransaction (Input vTransaction,            /* Codigo da Transacao                   */
                                Input vConteudoRaw,            /* Conteudo da temp-table {&TempTable}   */
                                Input vTransDetail,            /* Cabecalho de detalhes da transacao    */
                                Input vNomeUsuario).           /* Usuario responsavel pela transacao    */


End Procedure.
/*************************************  Codigo do Usuario Fim   **********************************/

PROCEDURE pi-zera-return:

    RETURN.

END.

/****************** Importa Dados do Coletor ******************************/

PROCEDURE pi-ler-coletor.
   DEF VAR c-id          AS CHAR FORMAT "x(20)".
   DEF VAR c-arq-coletor AS CHAR FORMAT "x(30)".
   DEF VAR c-arq-saida   AS CHAR FORMAT "x(30)".
   DEF VAR c-arq-bat     AS CHAR.
   DEF VAR i-handle      AS INTEGER NO-UNDO.

   ASSIGN c-arq-coletor = SESSION:TEMP-DIRECTORY + 
                          "DES-COLETOR" + SUBSTRING(C-EMBARQUE-RESUMO,1,6) + ".TXT"
          c-arq-saida = SESSION:TEMP-DIRECTORY + '\DES-SAIDA' +
                        SUBSTRING(C-EMBARQUE-RESUMO,1,6) + ".TXT".
         /* c-arq-saida = 'm:\ems206\coletor\DES' +
                        SUBSTRING(C-EMBARQUE-RESUMO,1,6) + ".TXT".*/
   
   /* Baixa os dados do Coletor */
   IF SESSION:SET-WAIT-STATE("general":U) THEN.

   OS-DELETE SILENT VALUE(c-arq-coletor).
   IF SEARCH("C:\IMPROTEC\P220\P220.EXE") <> ? THEN DO.
      ASSIGN c-arq-bat = SESSION:TEMP-DIRECTORY + "p220.bat".
      OUTPUT STREAM s-bat TO VALUE(c-arq-bat).
         PUT STREAM s-bat
             "c:" SKIP
             "cd " SESSION:TEMP-DIRECTORY SKIP
             "C:\IMPROTEC\P220\P220.EXE R 1 1 "
             c-arq-coletor FORMAT "x(40)" SKIP.
      OUTPUT STREAM s-bat CLOSE.

      IF SEARCH(c-arq-bat) <> ? THEN DO.
         OS-COMMAND SILENT VALUE(c-arq-bat).
         PAUSE 5 NO-MESSAGE.
         REPEAT. 
            RUN FindWindowA (0, "IMODEM", OUTPUT i-handle).
            IF i-handle = 0 THEN LEAVE.
         END. 
         OS-DELETE SILENT VALUE(c-arq-bat). 
      END.
   END.
   IF SESSION:SET-WAIT-STATE("") THEN.

   IF SEARCH(c-arq-coletor) = ? THEN DO.
      MESSAGE "Arquivo nÆo foi Baixado do Coletor..." VIEW-AS ALERT-BOX.
      RETURN ERROR.
   END.

   OUTPUT STREAM s-coletor TO VALUE(c-arq-saida).
   INPUT STREAM e-coletor FROM VALUE(c-arq-coletor) NO-ECHO.
   REPEAT.                 
       SET STREAM e-coletor c-id.

       IF c-id = '' THEN NEXT.

       FIND bc-etiqueta WHERE
            bc-etiqueta.progressivo = c-id NO-LOCK NO-ERROR.
       IF NOT AVAIL bc-etiqueta THEN DO.
          PUT STREAM s-coletor
              c-id
              "N£mero da Etiqueta invalido"
              SKIP.
          NEXT.
       END.

       PUT STREAM s-coletor
           bc-etiqueta.it-codigo ";" 
           bc-etiqueta.referencia ";"
           bc-etiqueta.qt-item ";" 
           bc-etiqueta.progressivo ";" 
           SKIP.

       ASSIGN c-item = c-id.

       DISP c-item WITH FRAME {&Frame02Name}.

       {&TriggerAfterFrame02}
       Run InicializaCamposFrame02.

   END.
   OUTPUT STREAM s-coletor CLOSE.
   INPUT STREAM e-coletor CLOSE.
END PROCEDURE.


PROCEDURE pi-ler-arquivo.
    DEF VAR c-arq-saida   AS CHAR FORMAT "x(30)".
    DEF VAR c-id          AS CHAR FORMAT "x(20)".

    IF SEARCH(c-arq-texto) = ? THEN DO.
       MESSAGE "Arquivo nÆo Texto para Informado..." VIEW-AS ALERT-BOX.
       RETURN ERROR.
    END.

    OUTPUT STREAM s-coletor TO VALUE(c-arq-saida).
    INPUT STREAM e-coletor FROM VALUE(c-arq-texto) NO-ECHO.
    REPEAT.                 
        SET STREAM e-coletor c-id.

        IF c-id = '' THEN NEXT.

        FIND bc-etiqueta WHERE
             bc-etiqueta.progressivo = c-id NO-LOCK NO-ERROR.

        IF NOT AVAIL bc-etiqueta THEN DO.
           PUT STREAM s-coletor
               c-id
               "N£mero da Etiqueta invalido"
               SKIP.
           NEXT.
        END.

        PUT STREAM s-coletor
            bc-etiqueta.it-codigo ";" 
            bc-etiqueta.referencia ";"
            bc-etiqueta.qt-item ";" 
            bc-etiqueta.progressivo ";" 
            SKIP.

        ASSIGN c-item = c-id.

        DISP c-item WITH FRAME {&Frame02Name}.

        {&TriggerAfterFrame02}
        Run InicializaCamposFrame02.
    END.
    OUTPUT STREAM s-coletor CLOSE.
    INPUT STREAM e-coletor CLOSE.

END PROCEDURE.

