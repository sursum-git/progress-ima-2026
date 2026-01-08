/**************************************************************************
Programa: esbo/boCalcFreteDestacado.p
Autor: Tadeu Silva Parreiras
Objetivo: Retornar o valor de frete a a ser destacado, quando for o caso
Data: 07/2023
Modificacoes:
06/2025 - altera‡äes de percentuais de destaque por estado   - tsp001
*****************************************************************************/


&SCOPED-DEFINE ttParam ttParam
&SCOPED-DEFINE boMsg   HBoMsg
DEFINE VARIABLE hBoEmitente       AS HANDLE      NO-UNDO.
DEFINE VARIABLE vlFreteDestacado  AS DECIMAL     NO-UNDO.
DEFINE VARIABLE logFreteDestacado AS LOGICAL     NO-UNDO.

DEFINE TEMP-TABLE {&ttparam}
    FIELD codEstabel        AS CHAR
    FIELD codCliente        AS INT
    FIELD codClienteTriang  AS INT
    FIELD codTipoFrete      AS CHAR
    FIELD vlTotPed          AS DECIMAL 
    FIELD nrPedido          AS INT.


DEFINE VARIABLE {&boMsg}   AS HANDLE      NO-UNDO.
{utp/ut-glob.i}
{esp/util.i}
{esp/setProp.i  {&ttparam} }



FUNCTION getPercDestacFreteCIFTotalPorUF RETURNS DECIMAL (
    INPUT pc_UF AS CHARACTER ) :

    DEFINE VARIABLE ld_Return AS DECIMAL NO-UNDO.

    /* Normaliza para mai£sculas */
    ASSIGN pc_UF = UPPER(pc_UF).

    CASE pc_UF:

      /* UFs apenas CIF PARCIAL at‚ SP e UFs com CIF TOTAL sem cobran‡a adicional */
      WHEN "AC" THEN ASSIGN ld_Return = 0.0.
      WHEN "AP" THEN ASSIGN ld_Return = 0.0.
      WHEN "AM" THEN ASSIGN ld_Return = 0.0.
      WHEN "PA" THEN ASSIGN ld_Return = 0.0.
      WHEN "RO" THEN ASSIGN ld_Return = 0.0.
      WHEN "RR" THEN ASSIGN ld_Return = 0.0.
      WHEN "TO" THEN ASSIGN ld_Return = 0.0.
      WHEN "MA" THEN ASSIGN ld_Return = 0.0.
      WHEN "PI" THEN ASSIGN ld_Return = 0.0.
      WHEN "MT" THEN ASSIGN ld_Return = 0.0.
      WHEN "MS" THEN ASSIGN ld_Return = 0.0.
      WHEN "SP" THEN ASSIGN ld_Return = 0.0.
      WHEN "RJ" THEN ASSIGN ld_Return = 0.0.
      WHEN "MG" THEN ASSIGN ld_Return = 0.0.
      WHEN "ES" THEN ASSIGN ld_Return = 0.0.
      WHEN "PR" THEN ASSIGN ld_Return = 0.0.
      WHEN "SC" THEN ASSIGN ld_Return = 0.0.
      WHEN "RS" THEN ASSIGN ld_Return = 0.0.

      /* Nordeste com CIF TOTAL e cobran‡a adicional de 2% */
      WHEN "CE" THEN ASSIGN ld_Return = 2.0.
      WHEN "RN" THEN ASSIGN ld_Return = 2.0.
      WHEN "PB" THEN ASSIGN ld_Return = 2.0.
      WHEN "PE" THEN ASSIGN ld_Return = 2.0.
      WHEN "AL" THEN ASSIGN ld_Return = 2.0.
      WHEN "SE" THEN ASSIGN ld_Return = 2.0.
      WHEN "BA" THEN ASSIGN ld_Return = 2.0.

      /* Centro-Oeste com CIF TOTAL e cobran‡a adicional de 1% */
      WHEN "GO" THEN ASSIGN ld_Return = 1.0.
      WHEN "DF" THEN ASSIGN ld_Return = 1.0.

      /* Qualquer outro valor de UF */
      OTHERWISE ASSIGN ld_Return = 0.0.

    END CASE.

    RETURN ld_Return.

END FUNCTION.




PROCEDURE iniciar:
RUN esbo/boMsg.p PERSIST SET {&boMsg}.
RUN esbo/boEmitente.p PERSIST SET hBoEmitente.
RUN iniciarBos IN hBoEmitente.

CREATE {&ttparam}.
    
END PROCEDURE.

PROCEDURE finalizar:
    IF VALID-HANDLE({&boMsg}) THEN
       DELETE PROCEDURE {&boMsg}.
    IF VALID-HANDLE(hBoEmitente) THEN DO:
        RUN iniciarBos IN hBoEmitente.
        DELETE PROCEDURE hBoEmitente.
    END.
       

    DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

PROCEDURE setValsIni:

    FIND FIRST {&ttParam} NO-ERROR.
    //setar os valores iniciais


END PROCEDURE.

PROCEDURE setTTParam:

    DEFINE INPUT PARAMETER TABLE FOR {&ttParam}.

END PROCEDURE.

PROCEDURE validarParametros:

    DEFINE OUTPUT PARAMETER lErro AS LOGICAL     NO-UNDO.
    FIND FIRST {&ttparam} NO-ERROR.

    IF {&ttparam}.codEstabel = '' THEN DO:
      RUN setMsg IN hBoMsg(1,' necess rio informar o Estabelecimento','erro').
      ASSIGN lErro = YES.
    END.
    IF {&ttparam}.codCliente = 0 THEN DO:
      RUN setMsg IN hBoMsg(2,' necess rio informar o Cliente Ou o Cliente Triangular','erro').
      ASSIGN lErro = YES.
    END.
    IF {&ttparam}.codTipoFrete ='' THEN DO:
      RUN setMsg IN hBoMsg(3,' necess rio informar o Tipo de Frete','erro').
      ASSIGN lErro = YES.
    END.




END PROCEDURE.


PROCEDURE exec:

    DEFINE VARIABLE cListaUfs   AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE codCliente  AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cUF         AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE dPerc       AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE lErro       AS LOGICAL     NO-UNDO.
    DEFINE VARIABLE cListaEstab AS CHARACTER   NO-UNDO.
    FIND FIRST {&ttparam} NO-ERROR.
    RUN validarParametros(OUTPUT lErro).
    IF lErro = NO THEN DO:
       RUN getEstabsFreteDestacado(OUTPUT cListaEstab).
       RUN setMsg IN hBoMsg(80,'Lista de Estabs que destacam o Frete:' + cListaEstab,'aviso').
       RUN setMsg IN hBoMsg(90,'Estab. do parametro:' + {&ttParam}.codEstabel,'aviso').
       IF LOOKUP({&ttParam}.codEstabel,cListaEstab) > 0 THEN DO:                                      
          IF {&ttparam}.codClienteTriang <> 0 THEN                                    
             ASSIGN codCliente = {&ttparam}.codClienteTriang .                        
          ELSE                                                                        
             ASSIGN codCliente = {&ttparam}.codCliente .                             
                                                                                      
          IF {&ttParam}.codTipoFrete = 'CIF TOTAL' OR {&ttParam}.codtipoFrete = 'Cif Destaque NF' THEN DO:                          
             RUN setMsg IN hBoMsg(100,'Tipo de Frete CIF Total ou CIF destaque NF','aviso').
             RUN getUfsCifParcial(OUTPUT cListaUfs) .                             
             RUN setCodEmitente IN hBoEmitente(codCliente).                          
             RUN getUf IN hBoEmitente(OUTPUT cUF).  
             RUN setMsg IN hBoMsg(110,'Lista de UFs CIF Parcial:' + cListaUFs,'aviso').
             RUN setMsg IN hBoMsg(120,'UF Cliente:' + cUf,'aviso').
             
             /*IF LOOKUP(cUf,cListaUFs) > 0 THEN DO:    
                ASSIGN logFreteDestacado  = YES.
                RUN getPercFreteDestacado(cUf,OUTPUT dPerc).           
                RUN setMsg IN hBoMsg(130,'Percentual a ser destacado:' + STRING(dPerc),'aviso').
                ASSIGN vlFreteDestacado  = {&ttParam}.vlTotPed * dPerc / 100.        
                RUN setMsg IN hBoMsg(140,'Valor de Frete Destacado:' + STRING(vlFreteDestacado),'aviso').
             END.       
             */
             //tsp001 - sempre busca o percentual destacado
             RUN getPercFreteDestacado(cUf,OUTPUT dPerc).           
             RUN setMsg IN hBoMsg(130,'Percentual a ser destacado:' + STRING(dPerc),'aviso').
             ASSIGN vlFreteDestacado  = {&ttParam}.vlTotPed * dPerc / 100.        
             RUN setMsg IN hBoMsg(140,'Valor de Frete Destacado:' + STRING(vlFreteDestacado),'aviso').
             ASSIGN  logFreteDestacado = dPerc > 0 .
             
             
          END.    
          ELSE DO:
              RUN setMsg IN hBoMsg(101,'Tipo de Frete DIFERENTE de  CIF Total e de  Cif Destaque NF:' + {&ttParam}.codTipoFrete ,'aviso').
          END.
       END.                                                                           
       ELSE DO:                                                                       
           ASSIGN vlFreteDestacado     = 0.                                           
       END.
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


PROCEDURE getUfsCifParcial:
    FIND FIRST {&ttparam} NO-ERROR.
    DEFINE OUTPUT PARAMETER  pValor AS CHARACTER   NO-UNDO.
    RUN getVlParametro('ufs_cif_parcial_' + {&ttParam}.codEstabel ,OUTPUT pValor).
    IF pValor = '' THEN DO:
       ASSIGN pValor = 'AC,AL,AP,AM,BA,CE,DF,MA,MT,MS,GO,PA,PB,PE,PI,RN,RO,RR,SE,TO'.
    END.     

END PROCEDURE.




PROCEDURE getPercFreteDestacado:

    DEFINE INPUT  PARAMETER pUf   AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER dPerc AS DECIMAL     NO-UNDO.
    
    ASSIGN dPerc = getPercDestacFreteCIFTotalPorUF(pUF).
    
    /*DEFINE VARIABLE regiao        AS CHARACTER   NO-UNDO.

    RUN getRegiao(INPUT pUF,OUTPUT regiao ).
    CASE regiao:
        
        WHEN 'centro-oeste' THEN DO:
            ASSIGN dPerc = 1 .
        END.
        WHEN 'nordeste' THEN DO:
            ASSIGN dPerc = 2 .
        END.
        WHEN 'sul' THEN DO:
            ASSIGN dPerc = 0 .
        END.
        WHEN 'sudeste' THEN DO:
            ASSIGN dPerc = 0 .
        END.
        WHEN 'norte' THEN DO:
            ASSIGN dPerc = 50 .
        END.

    END CASE.*/
    //tsp001
    

END PROCEDURE.


PROCEDURE getEstabsFreteDestacado:
    DEFINE OUTPUT PARAMETER  pValor AS CHARACTER   NO-UNDO.
    FIND FIRST {&ttparam} NO-ERROR.
    RUN getVlParametro('estabs_frete_destacado_' + {&ttParam}.codEstabel,OUTPUT pValor).
    IF pValor = '' THEN DO:
       ASSIGN pValor = '505'.
    END.
END PROCEDURE.

PROCEDURE getVlFreteDestacado:

    DEFINE OUTPUT PARAMETER pPerc AS DECIMAL     NO-UNDO.
    ASSIGN pPerc = vlFreteDestacado .

END PROCEDURE.

PROCEDURE gravarLogCalculo:
    FIND FIRST {&ttparam} NO-ERROR.
    DEFINE VARIABLE hBoTransacoes AS HANDLE      NO-UNDO.
    DEFINE VARIABLE iTransacao    AS INTEGER     NO-UNDO.
    RUN esbo/boTransacoes.p  PERSIST SET hBoTransacoes.
    RUN gerarTransacao IN hboTransacoes('boTransacoes',
                                       '',
                                        15,
                                        {&ttParam}.nrPedido,
                                        OUTPUT iTransacao).
    RUN setTransacaoLogCalculo  IN hBoMsg(iTransacao).
    RUN gravarLogCalculo IN hBoMsg(15).
    RUN finalizarTransacao IN hBoTransacoes(1).
    IF VALID-HANDLE(hBoTransacoes) THEN
       DELETE PROCEDURE hBoTransacoes.
    


END PROCEDURE.


PROCEDURE getLogFreteDestacado:

    DEFINE OUTPUT PARAMETER pLog AS LOGICAL     NO-UNDO.
    ASSIGN pLog = logFreteDestacado .

END PROCEDURE.
