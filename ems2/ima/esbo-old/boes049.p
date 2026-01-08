/**************************************************************************
Programa: boes049.p
Autor: Tadeu silva
Objetivo: Centralizar todas as manutená‰es da tabela ob-etiqueta
Data: 12/2023
Modificacoes:
*****************************************************************************/
&SCOPED-DEFINE ttReg  ttReg 
&SCOPED-DEFINE boMsg   HBoMsg

DEFINE BUFFER bf FOR ob-etiqueta.



DEFINE TEMP-TABLE {&ttReg}  LIKE ob-etiqueta .
DEFINE TEMP-TABLE ttEtqLisa LIKE etiqueta_lisa.


DEFINE VARIABLE {&boMsg}    AS HANDLE      NO-UNDO.
DEFINE VARIABLE lErro       AS LOGICAL     NO-UNDO.
DEFINE VARIABLE numEtq      AS INTEGER     NO-UNDO.
DEFINE VARIABLE hBoEtqLisa  AS HANDLE      NO-UNDO.

DEFINE var  etqLisa   AS CHARACTER   NO-UNDO.
DEFINE var  nrPedido  AS INTEGER     NO-UNDO.
DEFINE var  prePedido AS INTEGER     NO-UNDO.
DEFINE var  numOrigem AS INTEGER     NO-UNDO.



{esp/util.i}
{esp/setProp.i  {&ttReg} }
{esinc/verifIntegrLISA.i}
PROCEDURE iniciar:
RUN esbo/boMsg.p PERSIST SET {&boMsg}.
RUN esbo/boLisa13.p PERSIST SET hBoEtqLisa.
RUN iniciar IN hBoEtqLisa.
END PROCEDURE.

PROCEDURE finalizar:

    IF VALID-HANDLE({&boMsg}) THEN
       DELETE PROCEDURE {&boMsg}.

     IF VALID-HANDLE(hBoEtqLisa) THEN
        RUN finalizar IN hBoEtqLisa.

    DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.


PROCEDURE setTTReg:

    DEFINE INPUT PARAMETER TABLE FOR {&ttReg}.

END PROCEDURE.

//propriedades especificas para inclus∆o das etiquetas Lisa
PROCEDURE setEtqLisa:
    DEFINE INPUT  PARAMETER pEtqLisa AS CHARACTER   NO-UNDO.
    ASSIGN etqLisa = pEtqLisa.
END PROCEDURE.

PROCEDURE setNrPedido:
    DEFINE INPUT  PARAMETER pNrPedido AS INTEGER     NO-UNDO.
    ASSIGN nrPedido = pNrPedido.
END PROCEDURE.

PROCEDURE setPrePedido:
    DEFINE INPUT  PARAMETER pPrePedido AS INTEGER     NO-UNDO.
    ASSIGN prePedido = prePedido.
END PROCEDURE.

PROCEDURE setNumOrigem:
    DEFINE INPUT  PARAMETER pNumOrigem AS INTEGER     NO-UNDO.
    ASSIGN numOrigem = pNumOrigem.
END PROCEDURE.

// final propriedade especificas da inclusá∆o das etiquetas Lisa

PROCEDURE incluir:

    DEFINE VARIABLE lIntegraLISA AS LOGICAL     NO-UNDO.
    
    RUN validacoes('incluir').
    RUN validacoes('comum').
    RUN getErro IN {&boMsg}(OUTPUT lErro) .
    IF lErro THEN RETURN 'nok'.


     //no caso de estabelecimentos que tenham integraá∆o com a Lisa 
   RUN verifIntegrLisa({&ttReg}.cod-estabel,OUTPUT lIntegraLISA).

   IF lIntegraLISA THEN DO:
      RUN incluirEtqLisa.
      RUN getErro IN {&boMsg}(OUTPUT lErro) .
      IF lErro  THEN RETURN 'nok'.
      CREATE bf.
      ASSIGN bf.num-etiqueta = NEXT-VALUE(seq-etq-estoq-itj).
      
   END.
   ELSE DO:
      CREATE bf.
      ASSIGN bf.num-etiqueta = NEXT-VALUE(seq-etq-estoq-ima).
   END.

   FIND FIRST {&ttReg} NO-ERROR.
   FIND ITEM WHERE
         ITEM.it-codigo = {&ttReg}.it-codigo NO-LOCK NO-ERROR.

    
   ASSIGN bf.cod-estabel     = {&ttReg}.cod-estabel
           bf.dt-emissao      = TODAY
           bf.hr-emissao      = STRING(TIME,"HH:MM")
           bf.acondic         = ""
           bf.it-codigo       = {&ttReg}.it-codigo
           bf.num-rolo-imp    = {&ttReg}.num-rolo-imp
           bf.cod-refer       = {&ttReg}.cod-refer
           bf.nr-container    = {&ttReg}.nr-container
           bf.quantidade      = {&ttReg}.quantidade
           bf.nr-lote         = {&ttReg}.nr-lote
           bf.cod-qualid      = {&ttReg}.cod-qualid
           bf.corte-comerc    = {&ttReg}.corte-comerc
           bf.localizacao     = {&ttReg}.localizacao
           bf.situacao        = {&ttReg}.situacao
           bf.cod-depos       = {&ttReg}.cod-depos
           bf.ob-origem       = {&ttReg}.ob-origem
           .
   RELEASE bf NO-ERROR.
   
END PROCEDURE.


PROCEDURE incluirEtqLisa:

    DEFINE VARIABLE cErros AS CHARACTER   NO-UNDO.
    RUN limparTTMsg IN {&boMsg} .
    FIND FIRST {&ttreg} NO-ERROR.

    //incluindo primeiramente a TT para ser passada para a BO de Etq LISA
    CREATE ttEtqLisa.
    ASSIGN ttEtqLisa.cod_estabel    = {&ttreg}.cod-Estabel
           ttEtqLisa.num_etiqueta   = {&ttReg}.num-etiqueta
           ttEtqLisa.num_origem     = numOrigem
           ttEtqLisa.id_etq_lisa    = etqLisa
           ttEtqLisa.nr_pedido      = nrPedido
           ttEtqLisa.pre_pedido     = prePedido
        .

    
    RUN setTTReg IN hBoEtqLisa(INPUT TABLE ttEtqLisa).
    RUN incluir  IN hBoEtqLisa.
    RUN getErros IN hBoEtqLisa(OUTPUT cErros).
    IF cErros <> '' THEN DO:
       RUN SETMsg IN {&boMsg}(91,'Erros na inclus∆o de Etiqueta LISA:' + CHR(13) +
                              cErros).
    END.

    //verificar erros



END PROCEDURE.




PROCEDURE alterar:
    RUN limparTTMsg IN {&boMsg} .
    FIND FIRST {&ttReg} NO-ERROR.

    RUN validacoes('alterar').
    RUN validacoes('comum').
    RUN getErro(OUTPUT lErro) .
    IF lErro THEN RETURN 'nok'.
    FIND CURRENT bf EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL bf THEN DO:
       BUFFER-COPY {&ttReg} TO bf.
       RELEASE bf NO-ERROR.
    END.

END PROCEDURE.

PROCEDURE alterarQuantidade:

    RUN limparTTMsg IN {&boMsg} .
    FIND FIRST {&ttReg} NO-ERROR.

    RUN validacoes('alterar').
    RUN validacoes('comum').
    RUN getErro(OUTPUT lErro) .
    IF lErro THEN RETURN 'nok'.
    FIND CURRENT bf EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL bf THEN DO:
       ASSIGN bf.quantidade = {&ttReg}.quantidade .
       RELEASE bf NO-ERROR.
    END.

END PROCEDURE.


PROCEDURE alterarLocaliz:

    RUN limparTTMsg IN {&boMsg} .
    FIND FIRST {&ttReg} NO-ERROR.

    RUN validacoes('alterar').
    RUN validacoes('comum').
    RUN getErro(OUTPUT lErro) .
    IF lErro THEN RETURN 'nok'.
    FIND CURRENT bf EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL bf THEN DO:
       ASSIGN bf.localiz = {&ttReg}.localiz .
       RELEASE bf NO-ERROR.
    END.

END PROCEDURE.



PROCEDURE excluir:

    RUN limparTTMsg IN {&boMsg} .
    FIND FIRST {&ttReg} NO-ERROR.

    RUN validacoes('excluir').
    RUN validacoes('comum').
    RUN getErro IN {&boMsg}(OUTPUT lErro) .
    IF lErro THEN RETURN 'nok'.
    FIND CURRENT bf EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL bf THEN DO:
       DELETE bf.
       RELEASE bf NO-ERROR.
    END.


END PROCEDURE.


PROCEDURE validacoes:


   DEFINE INPUT  PARAMETER pAcao AS CHARACTER   NO-UNDO. //incluir,alterar,excluir,comum
   
   FIND FIRST {&ttReg} NO-ERROR.
   IF NOT AVAIL {&ttReg} THEN DO:
      RUN setMsg IN {&bomsg}(1,'Dados n∆o inseridos na tabela Tempor†ria').
      RETURN 'nok'.
   END.
  
   RUN verifExist.
   

   CASE pAcao:
       WHEN 'incluir' THEN DO:
           IF numEtq > 0 THEN
              RUN setMsg IN {&bomsg}(2,'Etiqueta j† existe com o Id:' + STRING(numEtq)).

       END.
       WHEN 'alterar' THEN DO:
           IF numEtq = 0 THEN
              RUN setMsg IN {&bomsg}(3,"N∆o foi encontrada Etiqueta para Alteraá∆o").

       END.
       WHEN 'excluir' THEN DO:
           IF numEtq = 0 THEN
              RUN setMsg IN {&bomsg}(4,"N∆o foi encontrada Etiqueta para Exclus∆o").
       END.
       WHEN 'comum' THEN DO:

       END.


   END CASE.


END PROCEDURE.

PROCEDURE getErro:

    DEFINE OUTPUT PARAMETER pErro AS CHARACTER   NO-UNDO.
    RUN getErro IN {&boMsg}(OUTPUT pErro).

END PROCEDURE.


PROCEDURE verifExist:

   DEFINE VARIABLE lIntegra AS LOGICAL     NO-UNDO.

   FIND FIRST {&ttReg} NO-ERROR.
   //verificaá∆o de duplicaá∆o por chave primaria
   IF {&ttreg}.num-etiqueta > 0 THEN DO:
       FIND  bf NO-LOCK
          WHERE bf.cod-estabel     = {&ttReg}.cod-estabel
          AND   bf.num-etiqueta    = {&ttReg}.num-etiqueta NO-ERROR.
       IF AVAIL bf THEN DO:
          ASSIGN numEtq = bf.num-etiqueta .
          RETURN 'ok'.
       END.
   END.

   //no caso de estabelecimentos que tenham integraá∆o com a Lisa 
   RUN verifIntegrLisa({&ttReg}.cod-estabel,OUTPUT lIntegra).
   IF lIntegra THEN DO:
      FIND LAST bf NO-LOCK
           WHERE bf.it-codigo       =  {&ttReg}.it-codigo
           AND   bf.cod-refer       =  {&ttReg}.cod-refer
           AND   bf.nr-container    =  {&ttReg}.nr-container
           AND   bf.num-rolo        =  {&ttReg}.num-rolo-imp NO-ERROR.
      IF AVAIL bf THEN DO:
         ASSIGN  numEtq = bf.num-etiqueta.
         RETURN 'ok'.
      END.           
   END.
   RETURN 'nok'.
END PROCEDURE.


/*PROCEDURE  sincronizar:

    RUN verifExist.
    IF RETURN-VALUE = 'NOK' THEN
       RUN incluir.
    ELSE
       RUN alterar.

END PROCEDURE.*/
