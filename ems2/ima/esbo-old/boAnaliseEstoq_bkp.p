/**************************************************************************
PADRAO DE BO DE CONSULTA
Programa: esbo/boAnaliseEstoq.p 
Autor: Tadeu Silva Parreiras
Objetivo: Extrair dados para analise de estoque
Data: 
Modificacoes:
*****************************************************************************/


&SCOPED-DEFINE ttParam  ttParam
&SCOPED-DEFINE boMsg    HBoMsg
&SCOPED-DEFINE ttResult ttResult
&SCOPED-DEFINE tabela   movto-estoq
DEFINE TEMP-TABLE {&ttparam}
    FIELD codEstabel       AS CHAR EXTENT 2
    FIELD itCodigo         AS CHAR EXTENT 2
    FIELD codRefer         AS CHAR EXTENT 2
    FIELD geCodigo         AS  INT EXTENT 2
    FIELD dtTrans          AS DATE EXTENT 2 FORMAT '99/99/9999'
    FIELD logApenasFat     AS LOGICAL
    .

DEFINE VARIABLE {&boMsg}   AS HANDLE      NO-UNDO.



{esp/util.i}
{esp/setProp.i  {&ttparam} }
{esbo/boAnaliseEstoq.i}

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
    ASSIGN 
    {&ttParam}.codEstabel[1] = ''
    {&ttParam}.codEstabel[2] = 'zzzz'
    {&ttParam}.itCodigo[1]   = ''  
    {&ttParam}.itCodigo[2]   = 'zzzzzzzzzzzzzz'  
    {&ttParam}.codRefer[1]   = ''   
    {&ttParam}.codRefer[2]   = 'zzzzz'
    {&ttParam}.geCodigo[1]   = 0   
    {&ttParam}.geCodigo[2]   = 999999
    {&ttParam}.dtTrans[1]    = 01.01.2001
    {&ttParam}.dtTrans[2]    = 01.01.2999
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
       RUN setTitulo IN h-acomp('Extra‡Æo Dados Movimento Estoque').
    END.

    

END PROCEDURE.


PROCEDURE setBoMsg:

    DEFINE INPUT  PARAMETER pHBoMsg AS HANDLE      NO-UNDO.
    ASSIGN {&boMsg} = pHBoMsg.

END PROCEDURE.

PROCEDURE gerarTtSaldo:
    RUN limparTTMsg IN {&boMsg}.

    FIND FIRST {&ttparam} NO-ERROR.
    IF NOT AVAIL {&ttParam} THEN DO:
      RUN setMsg IN {&bomsg}(1,'NÆo foram passados parametros').
      RETURN 'nok'.
    END.

    EMPTY TEMP-TABLE ttSaldo.

    MESSAGE {&ttParam}.codEstabel[1]  "-" {&ttParam}.codEstabel[2]  SKIP
            {&ttParam}.itCodigo[1]    "-" {&ttParam}.itCodigo[2]    SKIP
            {&ttParam}.codRefer[1]    "-" {&ttParam}.codRefer[2]    SKIP
            {&ttParam}.geCodigo[1]    "-" {&ttParam}.geCodigo[2]    SKIP
            {&ttParam}.logApenasFat SKIP


        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    FOR EACH saldo-estoq NO-LOCK
        WHERE saldo-estoq.cod-estabel   >= {&ttParam}.codEstabel[1]
        AND   saldo-estoq.cod-estabel   <= {&ttParam}.codEstabel[2]
        AND   saldo-estoq.it-codigo     >= {&ttParam}.itCodigo[1]
        AND   saldo-estoq.it-codigo     <= {&ttParam}.itCodigo[2]
        AND   saldo-estoq.cod-refer     >= {&ttParam}.codRefer[1]
        AND   saldo-estoq.cod-refer     <= {&ttParam}.codRefer[2],
        ITEM NO-LOCK OF saldo-estoq
        WHERE ITEM.ge-codigo            >= {&ttParam}.geCodigo[1]
        AND   ITEM.ge-codigo            <= {&ttParam}.geCodigo[2].

        IF {&ttParam}.logApenasFat AND item.ind-item-fat = NO  THEN NEXT.
        RUN acomp IN h-acomp('Saldo Estoq. Item:'  + saldo-estoq.it-codigo + " - Ref:" + saldo-estoq.cod-refer).
        FIND ttSaldo 
            WHERE ttSaldo.itCodigo      = saldo-estoq.it-codigo
            AND   ttSaldo.codRefer      = saldo-estoq.cod-refer
            NO-ERROR.
        IF NOT AVAIL ttSaldo THEN DO:
           CREATE ttSaldo.
           ASSIGN ttSaldo.itCodigo         = saldo-estoq.it-codigo
                  ttSaldo.codRefer         = saldo-estoq.cod-refer .
        END.
        ASSIGN ttSaldo.qtSaldoAtual        = ttSaldo.qtSaldoAtual + saldo-estoq.qtidade-atu .
    END.

END PROCEDURE.

PROCEDURE exec:
    
    DEFINE VARIABLE iSinal     AS INTEGER     NO-UNDO.

    FIND FIRST {&ttparam} NO-ERROR.
    IF NOT AVAIL {&ttParam} THEN DO:
       RUN setMsg IN {&bomsg}(1,'NÆo foram passados parametros').
       RETURN 'nok'.
    END.
    EMPTY TEMP-TABLE ttMovto.
    FOR EACH ttSaldo:
        RUN acomp IN h-acomp('Movimento:'  + ttSaldo.itCodigo + " - Ref:" + ttSaldo.codRefer).
        FOR EACH movto-estoq NO-LOCK
            WHERE movto-estoq.it-codigo = ttSaldo.itCodigo
            AND   movto-estoq.cod-refer = ttSaldo.codRefer
            USE-INDEX item-estab .
            FIND ITEM OF movto-estoq NO-LOCK NO-ERROR.
            ASSIGN iSinal = IF movto-estoq.tipo-Trans = 1 THEN 1 ELSE -1 .
            CREATE ttMovto.
            ASSIGN ttMovto.itCodigo          =  ttSaldo.itCodigo
                   ttMovto.codRefer          =  ttSaldo.codRefer
                   ttMovto.descItem          =  IF AVAIL ITEM THEN ITEM.desc-item ELSE ''
                   ttMovto.un                =  IF AVAIL ITEM THEN ITEM.un ELSE ''
                   ttMovto.especDocto        =  movto-estoq.esp-docto
                   ttMovto.desEspecDocto     =  {ininc/i03in218.i 4 ttMovto.especDocto }
                   ttMovto.nrDocto           =  movto-estoq.nro-docto
                   ttMovto.serie             =  movto-estoq.serie-docto
                   ttMovto.codEmitente       =  movto-estoq.cod-emitente
                   ttMovto.tipoTrans         =  movto-estoq.tipo-trans
                   ttMovto.desTipoTrans      =  {ininc/i01in218.i 4 ttMovto.tipoTrans}
                   ttMovto.natOperacao       =  movto-estoq.nat-operacao
                   ttMovto.codDepos          =  movto-estoq.cod-depos
                   ttMovto.codLocaliz        =  movto-estoq.cod-localiz
                   ttMovto.qtMovto           =  movto-estoq.quantidade
                   ttMovto.dtTrans           =  movto-estoq.dt-trans
                   ttMovto.ano               =  YEAR(movto-estoq.dt-trans)
                   ttMovto.mes               =  MONTH(movto-estoq.dt-trans)
                   ttMovto.dia               =  DAY(movto-estoq.dt-trans)
                   ttMovto.vlMovto           = (movto-estoq.valor-mat-m[1] + movto-estoq.valor-mat-m[2] + movto-estoq.valor-mat-m[3] +
                                                movto-estoq.valor-mat-o[1] + movto-estoq.valor-mat-o[2] + movto-estoq.valor-mat-o[3] +
                                                movto-estoq.valor-mat-p[1] + movto-estoq.valor-mat-p[2] + movto-estoq.valor-mat-p[3] +
                                                movto-estoq.valor-mob-m[1] + movto-estoq.valor-mob-m[2] + movto-estoq.valor-mob-m[3] +
                                                movto-estoq.valor-mob-o[1] + movto-estoq.valor-mob-o[2] + movto-estoq.valor-mob-o[3] +
                                                movto-estoq.valor-mob-p[1] + movto-estoq.valor-mob-p[2] + movto-estoq.valor-mob-p[3] + 
                                                movto-estoq.valor-nota     + movto-estoq.val-cofins     + movto-estoq.valor-icm      + 
                                                movto-estoq.valor-ipi ) * iSinal
                   .
            RUN classificarMovtoEstoq(INPUT  ttMovto.codEmitente,
                                      INPUT  ttMovto.especDocto,
                                      INPUT  ttMovto.desEspecDocto,  
                                      INPUT  ttMovto.tipoTrans,     
                                      INPUT  ttMovto.natOperacao,   
                                      OUTPUT ttMovto.desClassifMovto,
                                      OUTPUT ttMovto.codClassifMovto).
            IF movto-estoq.tipo-trans = 1 THEN //entrada
               ASSIGN ttSaldo.qtEntrada = ttSaldo.qtEntrada + movto-estoq.quantidade.
            ELSE
               ASSIGN ttSaldo.qtSaida    = ttSaldo.qtSaida   + movto-estoq.quantidade * iSinal.

            RUN _getAgrup(ttMovto.codClassifMovto, OUTPUT ttMovto.agrup).
            

        END.
    END.
    RUN _gerarMovtoSaldoAnterior.    
   

END PROCEDURE.


PROCEDURE exportarTTParam:

OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY +  'PARAMETROS_BO_ANALISE_ESTOQ_' +    STRING(TIME) + '.txt').

    FOR EACH {&ttParam}.
        DISP {&ttParam} WITH 1 COL WIDTH 550.
    END.
OUTPUT CLOSE.


END PROCEDURE.

PROCEDURE getErro:

    DEFINE OUTPUT PARAMETER pErro AS CHARACTER   NO-UNDO.
    RUN getErro IN {&boMsg}.

END PROCEDURE.

PROCEDURE getTTMovto:

    DEFINE OUTPUT PARAMETER TABLE FOR ttMovto.

END PROCEDURE.

PROCEDURE getTTSaldo:

    DEFINE OUTPUT PARAMETER TABLE FOR ttSaldo.

END PROCEDURE.


PROCEDURE exportarTTResult:

      DEFINE INPUT  PARAMETER pArquivo      AS CHARACTER   NO-UNDO.
      DEFINE INPUT  PARAMETER pDelimitador  AS CHARACTER   NO-UNDO.
      OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY + pArquivo) NO-CONVERT.
      PUT UNFORM "Produto"              pDelimitador 
                 "Referˆncia"           pDelimitador 
                 "Descri‡Æo"            pDelimitador 
                 "U.M."                 pDelimitador 
                 "Esp‚cie"              pDelimitador 
                 "Descr.Esp‚cie"        pDelimitador
                 "Nr.Docto"             pDelimitador
                 "S‚rie"                pDelimitador
                 "Cod.Emitente"         pDelimitador
                 "Tp.Trans"             pDelimitador
                 "Descr.Tp.Trans"       pDelimitador
                 "Nat.Opera‡Æo"         pDelimitador
                 "Dep¢sito"             pDelimitador
                 "Localiz."             pDelimitador   
                 "Quantidade"           pDelimitador   
                 "Dt.Transa‡Æo"         pDelimitador   
                 "Ano"                  pDelimitador   
                 "Mˆs"                  pDelimitador   
                 "Dia"                  pDelimitador   
                 "Vl.Movto"             pDelimitador   
                 "Agrupamento"          pDelimitador   
                 "Classif.Movto"        SKIP .
      FOR EACH ttMovto:
          PUT UNFORM   
              ttMovto.itCodigo              pDelimitador
              ttMovto.codRefer              pDelimitador
              ttMovto.descItem              pDelimitador
              ttMovto.un                    pDelimitador
              ttMovto.especDocto            pDelimitador 
              ttMovto.desEspecDocto         pDelimitador
              ttMovto.nrDocto               pDelimitador
              ttMovto.serie                 pDelimitador
              ttMovto.codEmitente           pDelimitador
              ttMovto.tipoTrans             pDelimitador
              ttMovto.desTipoTrans          pDelimitador
              ttMovto.natOperacao           pDelimitador
              ttMovto.codDepos              pDelimitador
              ttMovto.codLocaliz            pDelimitador
              ttMovto.qtMovto               pDelimitador
              ttMovto.dtTrans               pDelimitador
              ttMovto.ano                   pDelimitador
              ttMovto.mes                   pDelimitador
              ttMovto.dia                   pDelimitador
              ttMovto.vlMovto               pDelimitador
              ttMovto.agrup                 pDelimitador
              ttMovto.desClassifMovto       SKIP .
           


      END.

      OUTPUT CLOSE.

END PROCEDURE.




PROCEDURE classificarMovtoEstoq:

    DEFINE INPUT  PARAMETER pCodEmitente    AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER pEspDocto       AS INT   NO-UNDO.
    DEFINE INPUT  PARAMETER pDesEspecDocto  AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pTipoTrans      AS INT   NO-UNDO.
    DEFINE INPUT  PARAMETER pNatOperacao    AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER cClassif        AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER iClassif        AS INTEGER     NO-UNDO.

    DEFINE VARIABLE lGerarDp     AS LOGICAL     NO-UNDO.
    DEFINE VARIABLE lTerceiros   AS LOGICAL     NO-UNDO.
    DEFINE VARIABLE descEspDocto AS CHARACTER   NO-UNDO.

    

    FIND natur-oper NO-LOCK
        WHERE natur-oper.nat-operacao   = pNatOperacao NO-ERROR.
    IF AVAIL natur-oper THEN DO:
       ASSIGN lGerarDp    = natur-oper.emite-duplic
              lTerceiros  = natur-oper.terceiros
              .     
    END.

    CASE pEspDocto:
        WHEN 2 OR  //act
        WHEN 6     //div
        THEN
          ASSIGN cClassif = "Acerto"
                 iClassif = 1.
        WHEN 15  THEN //inv
          ASSIGN cClassif = "Invent rio"
                 iClassif = 2.
        WHEN 20 THEN  //NFD
          ASSIGN cClassif = "Devolu‡Æo de Venda"
                 iClassif = 3.
        WHEN 21 THEN DO: //NFE
          IF lTerceiros THEN
             ASSIGN cClassif = "Movto Terceiros-Entrada"
                    iClassif = 4.
          IF AVAIL natur-oper AND SUBSTR(natur-oper.nat-operacao,1,1) = '3' THEN //importacao
             ASSIGN cClassif = "NF Importa‡Æo"
                    iClassif = 5.
        END.
        WHEN 22 THEN DO: //NFS
          IF lGerarDP THEN
             ASSIGN cClassif = "Venda"
                    iClassif = 6.
          ELSE DO:
             IF lTerceiros THEN
                ASSIGN cClassif = "Movto Terceiros-Saida"
                       iClassif = 7.
          END.
        END.
        WHEN 23 OR //NFT - transferencia externa 
        WHEN 33    //TRA - transferencia interna
        THEN
          ASSIGN cClassif = "Transferencia"
                 iClassif = 8.
        OTHERWISE DO:
          IF pTipoTrans = 1 THEN
            ASSIGN cClassif = "Outros-Entrada-" + pDesEspecDocto
                   iClassif = pEspDocto * 10  + 9.

         IF pTipoTRans = 2 THEN
            ASSIGN cClassif = "Outros-Saida-" + pDesEspecDocto
                   iClassif = pEspDocto * 10 + 10.
        END.
    END CASE.

    FIND estabelec  NO-LOCK
        WHERE estabelec.cod-emitente = pCodEmitente NO-ERROR.
    IF AVAIL estabelec THEN DO:
       ASSIGN cClassif = cClassif + "- Empresas Grupo"
              iClassif = iClassif + 1000
              .
    END.
    


END PROCEDURE.

PROCEDURE _gerarMovtoSaldoAnterior:

  DEFINE VARIABLE dtCustoMedio AS DATE        NO-UNDO.
  DEFINE VARIABLE vlCustoMedio AS DECIMAL     NO-UNDO.

  FIND FIRST ttParam NO-ERROR.

  FOR EACH ttSaldo:
      RUN acomp IN h-acomp('Calc.Sld.Ant.Item:'  + ttSaldo.itCodigo + " - Ref:" + ttSaldo.codRefer).
      ASSIGN ttSaldo.qtSaldoAnterior = (ttSaldo.qtEntrada - ttSaldo.qtSaida - ttSaldo.qtSaldoAtual) * -1 .
      FIND FIRST ttMovto
          WHERE ttMovto.itCodigo    = ttSaldo.itCodigo
          AND   ttMovto.codRefer    = ttSaldo.codRefer
          USE-INDEX ind-data NO-ERROR.
      IF AVAIL ttMovto THEN
         ASSIGN ttSaldo.dtInicial = ttMovto.dtTrans.
      ELSE
         ASSIGN ttSaldo.dtInicial = ttParam.dtTrans[1].
      IF ttSaldo.qtSaldoAnterior <> 0 THEN DO:
          FIND ITEM NO-LOCK
              WHERE ITEM.it-codigo = ttSaldo.itCodigo NO-ERROR.
          
          CREATE ttMovto.
          ASSIGN ttMovto.itCodigo          = ttSaldo.itCodigo
                 ttMovto.codRefer          = ttSaldo.codRefer
                 ttMovto.descItem          = IF AVAIL ITEM THEN ITEM.desc-item ELSE ''
                 ttMovto.un                = IF AVAIL ITEM THEN ITEM.un ELSE ''
                 ttMovto.dtTrans           = ttSaldo.dtInicial - 1
                 ttMovto.nrDocto           = "SALDO ANTERIOR"
                 ttMovto.ano               =  YEAR(ttMovto.dtTrans)
                 ttMovto.mes               =  MONTH(ttMovto.dtTrans)
                 ttMovto.dia               =  DAY(ttMovto.dtTrans)
                 ttMovto.desClassifMovto   =  "SALDO ANTERIOR"
                 ttMovto.codClassifMovto   =  0
                 ttMovto.especDocto        =  0
                 ttMovto.desEspecDocto     = "SLD"
                 ttMovto.tipoTrans         = IF ttSaldo.qtSaldoAnterior < 0 THEN  2 ELSE 1
                 ttMovto.qtMovto           = IF ttMovto.TipoTrans = 1 THEN ttSaldo.qtSaldoAnterior ELSE ttSaldo.qtSaldoAnterior * - 1
                 ttMovto.agrup             = "SALDO ANTERIOR"
                 .

           RUN esapi/getCustoMedioItemData.p(ttMovto.dtTrans, ttMovto.itCodigo, OUTPUT vlCustoMedio, OUTPUT dtCustoMedio).
           ASSIGN ttMovto.vlMovto          = ttMovto.qtMovto * vlCustoMedio.

      END.
  END.


END PROCEDURE.


PROCEDURE _getAgrup:

     DEFINE INPUT  PARAMETER pClassif AS INTEGER     NO-UNDO.
     DEFINE OUTPUT PARAMETER cAgrup   AS CHARACTER   NO-UNDO.

     IF pClassif = 1 OR (pClassif > 100 AND pClassif < 1000) THEN
        ASSIGN cAgrup = "Acertos".

     IF pClassif > 100 THEN
        ASSIGN cAgrup = "Movto Grupo Empresa".

     IF pClassif = 2 THEN
        ASSIGN cAgrup = "Inventario".

     IF pClassif = 3 THEN
        ASSIGN cAgrup = "Devolucao".


     IF pClassif = 4 OR pClassif = 7 THEN
        ASSIGN cAgrup = "Movto Terceiros".
   
     IF pClassif = 5 THEN
        ASSIGN cAgrup = "Importacao".

     IF pClassif = 6 THEN
        ASSIGN cAgrup = "Venda".

     IF pClassif = 8 THEN
        ASSIGN cAgrup = "Transferencia".

     IF pClassif = 0 THEN
        ASSIGN cAgrup = "Saldo Anterior".


END PROCEDURE.
