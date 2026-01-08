DEFINE TEMP-TABLE ttDadosACBR
    FIELD sessao    AS CHAR FORMAT 'x(15)'
    FIELD subsessao AS CHAR FORMAT 'x(25)'
    FIELD chave     AS CHAR FORMAT 'x(50)'
    FIELD valor     AS CHAR FORMAT 'x(500)'
    FIELD ordem     AS INT
    INDEX ind AS PRIMARY UNIQUE sessao chave ordem
    INDEX ind-ordem sessao subsessao ordem .

DEFINE TEMP-TABLE ttTitulo
    FIELD codEstab             AS CHAR
    FIELD idTitulo             AS INT
    FIELD codPortador          AS CHAR
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
    INDEX ind AS PRIMARY UNIQUE codEstab idTitulo 
    INDEX indestabport codEstab codPortador. // indice titacr_token


DEFINE TEMP-TABLE ttArqHeader
    FIELD tipo          AS CHAR
    FIELD codEstab      AS CHAR
    FIELD codPortador   AS CHAR
    FIELD nomeArquivo   AS CHAR
    INDEX ind AS PRIMARY UNIQUE tipo codEstab codPortador .

DEFINE TEMP-TABLE ttEstabPortador
    FIELD codEstab    AS CHAR
    FIELD codPortador AS CHAR
    INDEX primario AS PRIMARY UNIQUE codEstab codPortador .
    

