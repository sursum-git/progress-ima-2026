/**************************************************************************
PADRAO DE BO DE CONSULTA
Programa: esbo/boSlItPer.p
Autor:  Tadeu Silva
Objetivo: Consulta a tabela sl-it-per com mais informa‡äes de produtos
Data: 09/2024
Modificacoes:
*****************************************************************************/


&SCOPED-DEFINE ttParam  ttParam
&SCOPED-DEFINE boMsg    HBoMsg
&SCOPED-DEFINE ttResult ttResult
&SCOPED-DEFINE tabela   sl-it-per
DEFINE TEMP-TABLE {&ttparam}
    FIELD itCodigo      AS CHAR EXTENT 2
    FIELD periodo       AS DATE EXTENT 2
    FIELD codEstabel    AS CHAR EXTENT 2
    FIELD depos         AS CHAR EXTENT 2
    FIELD geCodigo      AS INT  EXTENT 2
    FIELD logApenasFat  AS LOGICAL .

DEFINE VARIABLE {&boMsg}   AS HANDLE      NO-UNDO.
{esbo/boSlItPer.i {&ttResult} {&tabela}}


{esp/util.i}
{esp/setProp.i  {&ttparam} }

DEFINE VARIABLE h-acomp AS HANDLE      NO-UNDO.

PROCEDURE iniciar:
RUN esbo/boMsg.p PERSIST SET {&boMsg}.
RUN esbo/boAcomp.p PERSIST SET h-acomp.
CREATE {&ttparam}.

    
END PROCEDURE.

PROCEDURE finalizar:
    IF VALID-HANDLE({&boMsg}) THEN
       DELETE PROCEDURE {&boMsg}.
    IF VALID-HANDLE(h-acomp) THEN
       RUN finalizar IN h-acomp.
    DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

PROCEDURE setValsIni:

    FIND FIRST {&ttParam} NO-ERROR.
    ASSIGN {&ttParam}.itCodigo[1]   = ''
           {&ttParam}.itCodigo[2]   = 'zzzzzz'
           {&ttParam}.codEstabel[1] = ''
           {&ttParam}.codEstabel[2] = 'zzzzz'
           {&ttParam}.depos[1]      = ''
           {&ttParam}.depos[2]      = 'zzzzz'
           {&ttParam}.geCodigo[1]   = 0
           {&ttParam}.geCodigo[2]   = 99999
           .

END PROCEDURE.

PROCEDURE setTTParam:

    DEFINE INPUT PARAMETER TABLE FOR {&ttParam}.

END PROCEDURE.

PROCEDURE setAcomp:

    DEFINE INPUT  PARAMETER logHabilita AS LOGICAL     NO-UNDO.
    DEFINE INPUT  PARAMETER pHAComp     AS HANDLE      NO-UNDO.

    RUN setHabilita IN h-acomp(logHabilita).
    IF valid-handle(phAcomp) THEN DO:
       RUN setHandle IN h-acomp(phAComp).
    END.
    ELSE DO:
       RUN setTitulo IN h-acomp('Saldos Por Per¡odo').
    END.

    

END PROCEDURE.


PROCEDURE setBoMsg:

    DEFINE INPUT  PARAMETER pHBoMsg AS HANDLE      NO-UNDO.
    ASSIGN {&boMsg} = pHBoMsg.

END PROCEDURE.

PROCEDURE exec:
    RUN limparTTMsg IN {&boMsg}.
    FIND FIRST {&ttparam} NO-ERROR.
    IF NOT AVAIL {&ttParam} THEN DO:
       RUN setMsg IN {&bomsg}(1,'NÆo foram passados parametros').
       RETURN 'nok'.
    END.

    FOR EACH {&tabela}
        WHERE {&tabela}.it-codigo   >= {&ttParam}.itCodigo[1]
        AND   {&tabela}.it-codigo   <= {&ttParam}.itCodigo[2]
        AND   {&tabela}.cod-estabel >= {&ttParam}.codEstabel[1]
        AND   {&tabela}.cod-estabel <= {&ttParam}.codEstabel[2]
        AND   {&tabela}.cod-depos   >= {&ttParam}.depos[1]
        AND   {&tabela}.cod-estabel <= {&ttParam}.depos[2]
        AND   {&tabela}.periodo     >= {&ttParam}.periodo[1]
        AND   {&tabela}.periodo     <= {&ttParam}.periodo[2]
        ,
        EACH ITEM OF {&tabela} NO-LOCK 
        WHERE ITEM.ge-codigo >= {&ttParam}.geCodigo[1]
        AND   ITEM.ge-codigo <= {&ttParam}.geCodigo[2]
        .
        IF ITEM.ind-item-fat = NO AND {&ttParam}.logApenasFat THEN NEXT.

        //evitar periodos intermediarios, buscando pegar o ultimo de cada mˆs
        IF DAY({&tabela}.periodo) < 30 AND MONTH({&tabela}.periodo) <> 2  THEN NEXT.

        RUN acomp IN h-acomp('Produto:' + {&tabela}.it-codigo + "- periodo:" + STRING({&tabela}.periodo) ).

        FIND item-ext OF ITEM NO-LOCK NO-ERROR.
        FIND familia OF ITEM NO-LOCK NO-ERROR.
        FIND fam-comerc OF ITEM NO-LOCK NO-ERROR.

        CREATE {&ttResult}.
        BUFFER-COPY {&tabela} TO {&ttResult}.
        ASSIGN {&ttResult}.desfamComerc   = IF AVAIL fam-comerc THEN fam-comerc.descricao ELSE ''
               {&ttResult}.desFamMaterial = IF AVAIL familia    THEN familia.descricao ELSE ''
               {&ttResult}.tipoProduto    = IF AVAIL item-ext AND  item-ext.cod_tipo_Item = 1 THEN 'estampado' ELSE 'liso'
               {&ttResult}.un             = IF AVAIL ITEM THEN ITEM.un ELSE ''
               {&ttResult}.descItem       = IF AVAIL ITEM THEN ITEM.DESC-item ELSE '' 
               .          
    END.
        
   

END PROCEDURE.


PROCEDURE exportarTTParam:

OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY +  'PARAMETROS_' + PROGRAM-NAME(1) + '.txt').
    FOR EACH {&ttParam}.
        DISP {&ttParam} WITH 1 COL WIDTH 550.
    END.
OUTPUT CLOSE.


END PROCEDURE.

PROCEDURE getErro:

    DEFINE OUTPUT PARAMETER pErro AS CHARACTER   NO-UNDO.
    RUN getErro IN {&boMsg}.

END PROCEDURE.

PROCEDURE getTTResult:

    DEFINE OUTPUT PARAMETER TABLE FOR ttResult.

END PROCEDURE.

PROCEDURE exportarTTResult:

      DEFINE INPUT  PARAMETER pArquivo      AS CHARACTER   NO-UNDO.
      DEFINE INPUT  PARAMETER pDelimitador  AS CHARACTER   NO-UNDO.
      OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY + pArquivo) NO-CONVERT.
      PUT UNFORM "Produto"            pDelimitador 
           "Periodo"            pDelimitador 
           "Estabelecimento"    pDelimitador 
           "Deposito"           pDelimitador 
           "Qt.Saldo M‚dio"     pDelimitador 
           "Vl.Unit rio"        pDelimitador
           "Vl.Saldo Total"     pDelimitador
           "Familia Comercial"  pDelimitador
           "Familia Material"   pDelimitador
           "Tipo Produto"       pDelimitador
           "U.M."               pDelimitador
           "Descri‡Æo Produto"  pDelimitador
           "Quantidade"         SKIP .
      FOR EACH ttResult:
          PUT UNFORM   
              {&ttResult}.it-codigo            pDelimitador 
              {&ttResult}.periodo              pDelimitador 
              {&ttResult}.cod-estabel          pDelimitador 
              {&ttResult}.cod-depos            pDelimitador 
              {&ttResult}.saldo-medio          pDelimitador 
              {&ttResult}.val-unit-mat-m[1]    pDelimitador 
              {&ttResult}.valor-mat-m[1]       pDelimitador 
              {&ttResult}.desfamComerc         pDelimitador 
              {&ttResult}.desFamMaterial       pDelimitador 
              {&ttResult}.tipoProduto          pDelimitador 
              {&ttResult}.un                   pDelimitador 
              {&ttResult}.descItem             pDelimitador             
              {&ttResult}.quantidade             
              SKIP .
      END.

      OUTPUT CLOSE.

END PROCEDURE.
