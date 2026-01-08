DEFINE TEMP-TABLE ttEmitente LIKE emitente
    FIELD r-rowid AS ROWID
    FIELD LOG_integrado AS LOGICAL.

DEFINE TEMP-TABLE ttEmitenteExt LIKE ext-emitente
    FIELD nome_comprador        AS CHAR FORMAT 'x(50)'
    FIELD cpf_comprador         AS CHAR FORMAT 'x(20)'
    FIELD celular_waths         AS CHAR FORMAT 'x(30)'
    FIELD cnpj_contabilidade    AS CHAR FORMAT 'x(30)'
    FIELD aplicacao             AS int.

//DEFINE TEMP-TABLE ttEmitenteCnaes LIKE emitente_cnae.

DEFINE TEMP-TABLE ttAtividades NO-UNDO
    FIELD id                AS CHARACTER
    FIELD transacao_id      AS CHARACTER
    FIELD ativ_principal    AS LOGICAL
    FIELD cod_atividade     AS CHARACTER
    FIELD desc_atividade    AS CHARACTER
    FIELD cod_emitente      AS INT .


