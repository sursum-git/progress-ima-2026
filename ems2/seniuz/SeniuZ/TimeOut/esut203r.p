/********************************************************************************

    Programa    : 
    
    Objetivo    : 
    
    Criado em   : 
    
    Autor       : 
    
    Alteraá∆o   : 
    
********************************************************************************/

    /* Programa de controle de vers∆o e seguranáa do Datasul EMS */
    
    {include/i-prgvrs.i esut203r 2.02.00.001}
    
    /* Definiá∆o de variaveis globais comuns a todos os programas do EMS */
    
    {utp/ut-glob.i}
    
    /* Definiá∆o das temp-tables TT-Param e TT-digita 
       na area de definitions do programa chamador */
       
    DEF TEMP-TABLE tt-param    NO-UNDO
        FIELD destino          AS INT
        FIELD arquivo          AS CHAR FORMAT "x(35)"
        FIELD usuario          AS CHAR FORMAT "x(12)"
        FIELD data-exec        AS DATE
        FIELD hora-exec        AS INT
        FIELD cod-usuario      AS CHAR
        FIELD cod-usua-solic   AS CHAR
        FIELD dat-solic        AS DATE
        FIELD hor-solic        AS INT.

    DEFINE TEMP-TABLE tt-digita NO-UNDO
        FIELD ordem            AS INTEGER   FORMAT ">>>>9"
        FIELD exemplo          AS CHARACTER FORMAT "x(30)"
        INDEX id ordem.

/*    {utp/utapi019.i} */
    {esp/utp/esut205.i}
    {utp/ut-glob.i}

    /* Transfer Definitions */
    
    /* Parametros de entrada logica obrigatoria */
    DEF TEMP-TABLE tt-raw-digita
        FIELD raw-digita       AS RAW.
    
    DEF INPUT PARAMETER raw-param AS RAW NO-UNDO.
    DEF INPUT PARAMETER TABLE FOR tt-raw-digita.
    
    CREATE tt-param.
    RAW-TRANSFER raw-param to tt-param.
    
    FOR EACH tt-raw-digita NO-LOCK:
        CREATE tt-digita.
        RAW-TRANSFER tt-raw-digita.raw-digita TO tt-digita.
    END.

    /* Definicao das variaveis do cabecalho padrao corresponde a include cdp/cd9500.i  */
    
    {include/i-rpvar.i}   
    
    /* Elementos da p†gina final - Impress∆o opcional   */
    
    /* DEFINIÄ«O DE TABELA TEMPORµRIA DE USO ESPEC÷FICO */

    /* LOCALIZAÄ«O DO NOME DA EMPRESA PARA COMPOR O CABEÄALHO PADR«O */
    FIND empresa NO-LOCK 
        WHERE empresa.ep-codigo = i-ep-codigo-usuario NO-ERROR.
    IF NOT AVAIL empresa THEN 
        RETURN "ADM-ERROR":U.

    /* VALORIZAÄ«O DAS OUTRAS VARIµVEIS QUE COMPOEM O CABEÄALHO PADR«O */
    ASSIGN c-programa     = "esut203":U
           c-versao       = "2.02":U
           c-revisao      = ".00.001":U
           c-empresa      = empresa.razao-social
           c-sistema      = "EMS":U
           c-titulo-relat = "".
    
    /* FORMATAÄ«O DO CABEÄALHO E DO RODAPê - EXISTEM DOIS TIPOS DE CABEÄALHOS: f-cabecPER E O f-cabecper */

    {include/i-rpcab.i}

    DEF NEW GLOBAL SHARED VAR v_cod_usuar_corren LIKE usuar_mestre.cod_usuario NO-UNDO.
    DEF VAR                   c-bat              AS CHAR    NO-UNDO.
    DEF VAR                   c-ftp              AS CHAR    NO-UNDO.
    DEF VAR                   cProshut           AS CHAR    NO-UNDO.
    
    /* EXIBIÄ«O DA CAIXA DE BARRA DE ROLAGEM */

    
    /* ABERTURA DO ARQUIVO DE SA÷DA (ARQUIVO/IMPRESSORA) CORREPONDE A INCLUDE CDP/CD9520.I (MAGNUS) */
/*     {include/i-rpout.i}  */
/*                          */
/*     VIEW FRAME f-cabec.  */
/*     VIEW FRAME f-rodape. */
    
    /* INICIO PROCESSAMENTO DO RELAT‡RIO */

    DEF VAR i-Bancos  AS INT.

    IF NOT CONNECTED("finmov") THEN DO:
        IF PDBNAME("mguni") = "tems2cad" THEN                                                              
           RUN esp/funcoes/conecta-bco-ext.p (INPUT "tems5mov",  /* Nome Fisico do Banco */                
                                              INPUT "finmov",  /* Nome Logico do Banco */                  
                                              INPUT ""). /* Outros Parametros - Usuario, Senha, Mm */      
        ELSE                                                                                               
           RUN esp/funcoes/conecta-bco-ext.p (INPUT "ems5mov",  /* Nome Fisico do Banco */                 
                                              INPUT "finmov",  /* Nome Logico do Banco */                  
                                              INPUT ""). /* Outros Parametros - Usuario, Senha, Mm */      

    END.
    IF NOT CONNECTED("fincad") THEN DO:
    IF PDBNAME("mguni") = "tems2cad" THEN
       RUN esp/funcoes/conecta-bco-ext.p (INPUT "tems5cad",  /* Nome Fisico do Banco */
                                          INPUT "fincad",  /* Nome Logico do Banco */
                                          INPUT ""). /* Outros Parametros - Usuario, Senha, Mm */
    ELSE
       RUN esp/funcoes/conecta-bco-ext.p (INPUT "ems5cad",  /* Nome Fisico do Banco */
                                          INPUT "fincad",  /* Nome Logico do Banco */
                                          INPUT ""). /* Outros Parametros - Usuario, Senha, Mm */
    END.


    CONNECT VALUE("-db hr209    -ld hr209    -S 30000 -H 172.19.0.20") NO-ERROR.
    CONNECT VALUE("-db dthrtosh -ld dthrtosh -S 30100 -H 172.19.0.20") NO-ERROR.
    CONNECT VALUE("-db ems2uni  -ld ems2uni  -S 30200 -H 172.19.0.20") NO-ERROR.

    IF tt-param.cod-usuario <> "" THEN DO:
        DO i-Bancos = 1 TO NUM-DBS:
            CREATE ALIAS DICTDB FOR DATABASE VALUE(LDBNAME(i-Bancos)).
            RUN esp/utp/esut203d.p (INPUT tt-param.cod-usuario,
                                INPUT "c:/datasul/temp/logs/" + arquivo,
                                INPUT c-bat).
        END.
    
    END.

    DISCONNECT fincad   NO-ERROR.
    DISCONNECT finmov   NO-ERROR.
    DISCONNECT hr209    NO-ERROR.
    DISCONNECT dthrtosh NO-ERROR.
    DISCONNECT ems2uni  NO-ERROR.

    /* FIM DO PROCESSAMENTO DO RELAT‡RIO */
        
    /* FINALIZAR ACOMPANHAMENTO */

    /* FECHAMENTO DO ARQUIVO DE SA÷DA E TAMBêM
       DE CARACTERES DE CONTROLE DA IMPRESSORA.
       CORRESPONDE ∑ INCLUDE CDP/CD9540.I (MAGNUS) */
    
/*     HIDE FRAME f-cabec.  */
/*     HIDE FRAME f-rodape. */
/*                          */
/*     {include/i-rpclo.i}  */

