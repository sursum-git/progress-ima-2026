/*********************
programa: esbo/boBoleto.p
objetivo: Gera‡Æo de Boleto por meio do ACBR

***********************/

DEFINE TEMP-TABLE ttTitulo
    FIELD codEstab AS CHAR
    FIELD idTitulo AS INT
    FIELD numeroDocumento      AS CHAR
    FIELD nossoNumero          AS CHAR
    FIELD carteira             AS CHAR
    FIELD valorDocumento       AS DECIMAL
    FIELD vencimento           AS DATE
    FIELD dataDocumento        AS DATE
    FIELD dataProcessamento    AS DATE
    FIELD dataAbatimento       AS DATE
    FIELD dataDesconto         AS DATE
    FIELD dataMoraJuros        AS DATE
    FIELD diasMoraJuros        AS INT
    FIELD diasDeProtesto       AS INT
    FIELD diasDeNegativacao    AS INT
    FIELD dataProtesto         AS DATE
    FIELD dataNegativacao      AS DATE
    FIELD dataMulta            AS DATE 
    FIELD dataBaixa            AS DATE
    FIELD valorAbatimento      AS DECIMAL
    FIELD valorDesconto        AS DECIMAL
    FIELD valorMoraJuros       AS DECIMAL
    FIELD valorIOF             AS DECIMAL
    FIELD valorOutrasDespesas  AS DECIMAL
    FIELD multaValorFixo       AS DECIMAL
    FIELD percentualMulta      AS DECIMAL
    FIELD localPagamento       AS CHAR
    FIELD especie              AS CHAR
    FIELD especieMod           AS CHAR
    FIELD sacadoNomeSacado     AS CHAR
    FIELD sacadoCNPJCPF        AS CHAR
    FIELD sacadoPessoa         AS CHAR
    FIELD sacadoLogradouro     AS CHAR
    FIELD sacadoNumero         AS CHAR
    FIELD sacadoBairro         AS CHAR
    FIELD sacadoComplemento    AS CHAR
    FIELD sacadoCidade         AS CHAR
    FIELD sacadoUF             AS CHAR
    FIELD sacadoCEP            AS CHAR
    FIELD sacadoEmail          AS CHAR
    FIELD mensagem             AS CHAR
    FIELD detalhamento         AS CHAR
    FIELD competencia          AS CHAR
    FIELD arquivoLogEmp        AS CHAR
    FIELD verso                AS CHAR
    FIELD instrucao1           AS CHAR
    FIELD instrucao2           AS CHAR
    FIELD aceite               AS CHAR
    FIELD tipoOcorrencia       AS CHAR
    FIELD parcela              AS CHAR
    FIELD totalParcelas        AS CHAR
    FIELD seuNumero            AS CHAR
    FIELD tipoDiasProtesto     AS CHAR
    FIELD tipoDiasNegativacao  AS CHAR
    FIELD tipoImpressao        AS CHAR
    FIELD codigoMora           AS CHAR
    FIELD tipoDesconto         AS CHAR
    FIELD tipoDesconto2        AS CHAR
    FIELD carteiraEnvio        AS CHAR
    INDEX ind AS PRIMARY UNIQUE codEstab idTitulo. // indice titacr_token

/*
RUN sincrProp(sessao,subsessao,'NumeroDocumento','',0).
    RUN sincrProp(sessao,subsessao,'NossoNumero','',0).
    RUN sincrProp(sessao,subsessao,'Carteira','',0).
    RUN sincrProp(sessao,subsessao,'ValorDocumento','',0).
    RUN sincrProp(sessao,subsessao,'Vencimento','',0).
    RUN sincrProp(sessao,subsessao,'DataDocumento','',0).
    RUN sincrProp(sessao,subsessao,'DataProcessamento','',0).
    RUN sincrProp(sessao,subsessao,'DataAbatimento','',0).
    RUN sincrProp(sessao,subsessao,'DataDesconto','',0).
    RUN sincrProp(sessao,subsessao,'DataMoraJuros','',0).
    RUN sincrProp(sessao,subsessao,'DiasDeProtesto','',0).
    RUN sincrProp(sessao,subsessao,'DiasDeNegativacao','',0).
    RUN sincrProp(sessao,subsessao,'DataProtesto','',0).
    RUN sincrProp(sessao,subsessao,'DataNegativacao','',0).
    RUN sincrProp(sessao,subsessao,'DataMulta','',0).
    RUN sincrProp(sessao,subsessao,'DataBaixa','',0).
    RUN sincrProp(sessao,subsessao,'ValorAbatimento','0,0',0).
    RUN sincrProp(sessao,subsessao,'ValorDesconto','0,0',0).
    RUN sincrProp(sessao,subsessao,'ValorMoraJuros','0,0',0).
    RUN sincrProp(sessao,subsessao,'ValorIOF','0,0',0).
    RUN sincrProp(sessao,subsessao,'ValorOutrasDespesas','0,0',0).
    RUN sincrProp(sessao,subsessao,'MultaValorFixo','0,0',0).
    RUN sincrProp(sessao,subsessao,'PercentualMulta','0,0',0).
    RUN sincrProp(sessao,subsessao,'LocalPagamento','',0).
    RUN sincrProp(sessao,subsessao,'Especie','DP',0).
    RUN sincrProp(sessao,subsessao,'EspecieMod','R$',0).
    RUN sincrProp(sessao,subsessao,'Sacado.NomeSacado','',0).
    RUN sincrProp(sessao,subsessao,'Sacado.CNPJCPF','',0).
    RUN sincrProp(sessao,subsessao,'Sacado.Pessoa','1',0).
    RUN sincrProp(sessao,subsessao,'Sacado.Logradouro','',0).
    RUN sincrProp(sessao,subsessao,'Sacado.Numero','',0).
    RUN sincrProp(sessao,subsessao,'Sacado.Bairro','',0).
    RUN sincrProp(sessao,subsessao,'Sacado.Complemento','',0).
    RUN sincrProp(sessao,subsessao,'Sacado.Cidade','',0).
    RUN sincrProp(sessao,subsessao,'Sacado.UF','',0).
    RUN sincrProp(sessao,subsessao,'Sacado.CEP','',0).
    RUN sincrProp(sessao,subsessao,'Sacado.Email','',0).
    RUN sincrProp(sessao,subsessao,'Mensagem','',0).
    RUN sincrProp(sessao,subsessao,'Detalhamento','',0).
    RUN sincrProp(sessao,subsessao,'Competencia','',0).
    RUN sincrProp(sessao,subsessao,'ArquivoLogoEmp','',0).
    RUN sincrProp(sessao,subsessao,'Verso','False',0).
    RUN sincrProp(sessao,subsessao,'Instrucao1','01',0).
    RUN sincrProp(sessao,subsessao,'Instrucao2','02',0).
    RUN sincrProp(sessao,subsessao,'Aceite','1',0).
    RUN sincrProp(sessao,subsessao,'OcorrenciaOriginal.TipoOcorrencia','0',0).
    RUN sincrProp(sessao,subsessao,'Parcela','',0).
    RUN sincrProp(sessao,subsessao,'TotalParcelas','',0).
    RUN sincrProp(sessao,subsessao,'SeuNumero','',0).
    RUN sincrProp(sessao,subsessao,'TipoDiasProtesto','1',0).
    RUN sincrProp(sessao,subsessao,'TipoDiasNegativacao','0',0).
    RUN sincrProp(sessao,subsessao,'TipoImpressao','1',0).
    RUN sincrProp(sessao,subsessao,'CodigoMora','1',0).
    RUN sincrProp(sessao,subsessao,'TipoDesconto','0',0).
    RUN sincrProp(sessao,subsessao,'TipoDesconto2','0',0).
    RUN sincrProp(sessao,subsessao,'CarteiraEnvio','0',0).
*/






DEFINE TEMP-TABLE ttDadosACBR
    FIELD sessao    AS CHAR FORMAT 'x(15)'
    FIELD subsessao AS CHAR FORMAT 'x(25)'
    FIELD chave     AS CHAR FORMAT 'x(50)'
    FIELD valor     AS CHAR FORMAT 'x(500)'
    FIELD ordem     AS INT
    INDEX ind AS PRIMARY UNIQUE sessao chave ordem .


PROCEDURE inserirProp:

    DEFINE INPUT  PARAMETER sessao      AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER subSessao   AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pChave      AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pValor      AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pOrdem      AS INTEGER     NO-UNDO.

    DEFINE BUFFER bf FOR ttDadosACBR .
    FIND LAST bf  NO-ERROR.

    CREATE ttDadosACBR.
    ASSIGN ttDadosACBR.chave   =   pChave
           ttDadosACBR.valor   =   pValor 
           ttDadosACBR.ordem   = IF pOrdem  > 0 THEN pOrdem 
                              ELSE 
                                IF AVAIL bf THEN 
                                   bf.ordem + 1 
                                ELSE
                                  0 .
END PROCEDURE.

PROCEDURE sincrProp:

    DEFINE INPUT  PARAMETER psessao         AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER psubSessao      AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pChave          AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pValor          AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pOrdem          AS INTEGER     NO-UNDO.
    DEFINE VARIABLE rRowid AS ROWID       NO-UNDO.
    RUN getProp(pSessao,pSubSessao,pChave,OUTPUT rRowid).
    IF rRowid <> ? THEN DO:
       RUN atuProp(rRowid,pValor).
    END.
    ELSE DO:
       RUN inserirProp(
            psessao       ,
            psubSessao    ,
            pChave        ,
            pValor        ,
            pOrdem        
           
           ).
    END.

END PROCEDURE.

PROCEDURE atuProp:

    DEFINE INPUT  PARAMETER rRowid      AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER pValor      AS CHARACTER   NO-UNDO.

    FIND ttDadosACBR
        WHERE ROWID(ttDAdosACBR) = rRowid
        NO-ERROR.
    IF AVAIL ttDadosACBR THEN DO:
       ASSIGN ttDAdosACBR.valor = pValor .
    END.






END PROCEDURE.

PROCEDURE getProp:

    DEFINE INPUT  PARAMETER pSessao      AS CHARACTER   NO-UNDO.  
    DEFINE INPUT  PARAMETER pSubSessao   AS CHARACTER   NO-UNDO.  
    DEFINE INPUT  PARAMETER pChave       AS CHARACTER   NO-UNDO.  
    DEFINE OUTPUT PARAMETER rRowid       AS ROWID       NO-UNDO.

    FIND ttDadosACBR
        WHERE ttDadosACBR.sessao    = pSessao
        AND   ttDadosACBR.subSessao = pSubSessao
        AND   ttDadosACBR.chave     = pChave
        NO-ERROR.
    IF AVAIL ttDadosACBR THEN
       ASSIGN rRowid = ROWID(ttDadosACBR).


END PROCEDURE.


PROCEDURE inserirProps:
    
    DEFINE VARIABLE sessao      AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE subSessao   AS CHARACTER   NO-UNDO.



    ASSIGN sessao    = 'header'
           subsessao = 'Cedente'.

    RUN sincrProp(sessao,subsessao,'Nome','',0).
    RUN sincrProp(sessao,subsessao,'CNPJCPF','',0).
    RUN sincrProp(sessao,subsessao,'Logradouro','',0).
    RUN sincrProp(sessao,subsessao,'Numero','',0).
    RUN sincrProp(sessao,subsessao,'Bairro','',0).
    RUN sincrProp(sessao,subsessao,'Cidade','',0).
    RUN sincrProp(sessao,subsessao,'CEP','',0).
    RUN sincrProp(sessao,subsessao,'Complemento','',0).
    RUN sincrProp(sessao,subsessao,'UF','',0).
    RUN sincrProp(sessao,subsessao,'Telefone','',0).
    RUN sincrProp(sessao,subsessao,'RespEmis','',0).
    RUN sincrProp(sessao,subsessao,'TipoPessoa','',0).
    RUN sincrProp(sessao,subsessao,'CodigoCedente','',0).
    RUN sincrProp(sessao,subsessao,'LayoutBol','0',0).
    RUN sincrProp(sessao,subsessao,'CaracTitulo','2',0).
    RUN sincrProp(sessao,subsessao,'TipoCarteira','',0).
    RUN sincrProp(sessao,subsessao,'TipoDocumento','1',0).
    RUN sincrProp(sessao,subsessao,'Modalidade','',0).
    RUN sincrProp(sessao,subsessao,'CodTransmissao','10',0).
    RUN sincrProp(sessao,subsessao,'Convenio','',0).

    ASSIGN subsessao = 'Conta'.                           
    RUN sincrProp(sessao,subsessao,'Conta','',0).
    RUN sincrProp(sessao,subsessao,'DigitoConta','',0).
    RUN sincrProp(sessao,subsessao,'Agencia','',0).
    RUN sincrProp(sessao,subsessao,'DigitoAgencia','',0).
    RUN sincrProp(sessao,subsessao,'DigitoVerificadorAgenciaConta','',0).

    ASSIGN subsessao = 'Banco'. 

    RUN sincrProp(sessao,subsessao,'Numero','',0).
    RUN sincrProp(sessao,subsessao,'CNAB','1',0).
    RUN sincrProp(sessao,subsessao,'IndiceACBr','',0).
    RUN sincrProp(sessao,subsessao,'NumeroCorrespondente','0',0).
    RUN sincrProp(sessao,subsessao,'VersaoArquivo','0',0).
    RUN sincrProp(sessao,subsessao,'VersaoLote','0',0).

    RUN sincrProp(sessao,subsessao,'NumeroDocumento','',0).
    RUN sincrProp(sessao,subsessao,'NossoNumero','',0).
    RUN sincrProp(sessao,subsessao,'Carteira','',0).
    RUN sincrProp(sessao,subsessao,'ValorDocumento','',0).
    RUN sincrProp(sessao,subsessao,'Vencimento','',0).
    RUN sincrProp(sessao,subsessao,'DataDocumento','',0).
    RUN sincrProp(sessao,subsessao,'DataProcessamento','',0).
    RUN sincrProp(sessao,subsessao,'DataAbatimento','',0).
    RUN sincrProp(sessao,subsessao,'DataDesconto','',0).
    RUN sincrProp(sessao,subsessao,'DataMoraJuros','',0).
    RUN sincrProp(sessao,subsessao,'DiasDeProtesto','',0).
    RUN sincrProp(sessao,subsessao,'DiasDeNegativacao','',0).
    RUN sincrProp(sessao,subsessao,'DataProtesto','',0).
    RUN sincrProp(sessao,subsessao,'DataNegativacao','',0).
    RUN sincrProp(sessao,subsessao,'DataMulta','',0).
    RUN sincrProp(sessao,subsessao,'DataBaixa','',0).
    RUN sincrProp(sessao,subsessao,'ValorAbatimento','0,0',0).
    RUN sincrProp(sessao,subsessao,'ValorDesconto','0,0',0).
    RUN sincrProp(sessao,subsessao,'ValorMoraJuros','0,0',0).
    RUN sincrProp(sessao,subsessao,'ValorIOF','0,0',0).
    RUN sincrProp(sessao,subsessao,'ValorOutrasDespesas','0,0',0).
    RUN sincrProp(sessao,subsessao,'MultaValorFixo','0,0',0).
    RUN sincrProp(sessao,subsessao,'PercentualMulta','0,0',0).
    RUN sincrProp(sessao,subsessao,'LocalPagamento','',0).
    RUN sincrProp(sessao,subsessao,'Especie','DP',0).
    RUN sincrProp(sessao,subsessao,'EspecieMod','R$',0).
    RUN sincrProp(sessao,subsessao,'Sacado.NomeSacado','',0).
    RUN sincrProp(sessao,subsessao,'Sacado.CNPJCPF','',0).
    RUN sincrProp(sessao,subsessao,'Sacado.Pessoa','1',0).
    RUN sincrProp(sessao,subsessao,'Sacado.Logradouro','',0).
    RUN sincrProp(sessao,subsessao,'Sacado.Numero','',0).
    RUN sincrProp(sessao,subsessao,'Sacado.Bairro','',0).
    RUN sincrProp(sessao,subsessao,'Sacado.Complemento','',0).
    RUN sincrProp(sessao,subsessao,'Sacado.Cidade','',0).
    RUN sincrProp(sessao,subsessao,'Sacado.UF','',0).
    RUN sincrProp(sessao,subsessao,'Sacado.CEP','',0).
    RUN sincrProp(sessao,subsessao,'Sacado.Email','',0).
    RUN sincrProp(sessao,subsessao,'Mensagem','',0).
    RUN sincrProp(sessao,subsessao,'Detalhamento','',0).
    RUN sincrProp(sessao,subsessao,'Competencia','',0).
    RUN sincrProp(sessao,subsessao,'ArquivoLogoEmp','',0).
    RUN sincrProp(sessao,subsessao,'Verso','False',0).
    RUN sincrProp(sessao,subsessao,'Instrucao1','01',0).
    RUN sincrProp(sessao,subsessao,'Instrucao2','02',0).
    RUN sincrProp(sessao,subsessao,'Aceite','1',0).
    RUN sincrProp(sessao,subsessao,'OcorrenciaOriginal.TipoOcorrencia','0',0).
    RUN sincrProp(sessao,subsessao,'Parcela','',0).
    RUN sincrProp(sessao,subsessao,'TotalParcelas','',0).
    RUN sincrProp(sessao,subsessao,'SeuNumero','',0).
    RUN sincrProp(sessao,subsessao,'TipoDiasProtesto','1',0).
    RUN sincrProp(sessao,subsessao,'TipoDiasNegativacao','0',0).
    RUN sincrProp(sessao,subsessao,'TipoImpressao','1',0).
    RUN sincrProp(sessao,subsessao,'CodigoMora','1',0).
    RUN sincrProp(sessao,subsessao,'TipoDesconto','0',0).
    RUN sincrProp(sessao,subsessao,'TipoDesconto2','0',0).
    RUN sincrProp(sessao,subsessao,'CarteiraEnvio','0',0).
    
    
    


END PROCEDURE.





PROCEDURE sincrIniHeader:


END PROCEDURE.

PROCEDURE gerarIniHeader:


END PROCEDURE.

PROCEDURE getIniHeader:


END PROCEDURE.


PROCEDURE incluirTitulo:

    DEFINE INPUT  PARAMETER pCodEstab AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pIdTitulo AS INTEGER     NO-UNDO.

    CREATE ttTitulo.
    ASSIGN ttTitulo.codEstab = pCodEstab
           ttTitulo.idTitulo = pIdTitulo .



END PROCEDURE.

PROCEDURE gerarTitulos:
    
    FOR EACH ttTitulo ,
        EACH tit_acr NO-LOCK
        WHERE ttTitulo.codEstab = tit_acr.cod_estab
        AND   ttTitulo.idTitulo = tit_acr.num_id_tit_acr .                   

        //posicionamento nas tabelas que cont‚m os dados necess rios a gera‡Æo 
        FIND estabelec
            WHERE estabelec.cod-estabel = tit_acr.cod_estab NO-LOCK NO-ERROR.

        FIND emitente 
            WHERE emitente.cod-emitente =  tit_acr.cdn_cliente NO-LOCK NO-ERROR.

        FIND ems5.portador              OF tit_acr  NO-LOCK NO-ERROR.  
        FIND ems5.portad_edi            OF portador NO-LOCK NO-ERROR.
        FIND ems5.portad_finalid_econ   OF portador NO-LOCK NO-ERROR.
        FIND ems5.portad_bco            OF portador NO-LOCK NO-ERROR.








    END.



END PROCEDURE.


PROCEDURE setDtsEmissao:

END PROCEDURE.


PROCEDURE setDtsVencimento:

END PROCEDURE.


PROCEDURE buscarTitulosFiltro:


END PROCEDURE.




