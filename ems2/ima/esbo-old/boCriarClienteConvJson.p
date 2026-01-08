USING Progress.Json.ObjectModel.ObjectModelParser.
USING Progress.Json.ObjectModel.JsonArray.
USING Progress.Json.ObjectModel.JsonObject.
{esbo/BoClienteConvJson.i}
{esapi/analisarJsonObject2.i}
 
DEFINE VARIABLE oJsonObject         AS jsonObject.
DEFINE VARIABLE cNivel              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTipoDados          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cNomeArquivo        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE oJsonObjectPayload  AS JsonObject  NO-UNDO.
DEFINE VARIABLE hBoMsg              AS HANDLE      NO-UNDO.


PROCEDURE setHandleBoMsg:

    DEFINE INPUT  PARAMETER pHBoMsg AS HANDLE      NO-UNDO.
    ASSIGN hBoMsg = pHBoMsg .

END PROCEDURE.

PROCEDURE setObjJson:
    DEFINE INPUT PARAMETER poJsonObject AS jsonObject.
    DEFINE INPUT  PARAMETER pNivelPrinc AS CHARACTER   NO-UNDO.
    
    ASSIGN oJsonObject = poJsonObject 
           cTipoDados = 'objeto'.
    IF pNivelPrinc <> '' THEN
       oJsonObjectPayLoad  = CAST(oJsonObject:GetJsonObject(pNivelPrinc),JsonObject). 

END PROCEDURE.

PROCEDURE getObjetoJsonPrinc:
     DEFINE OUTPUT PARAMETER pObj AS jsonObject.
     ASSIGN pObj = oJsonObjectPayLoad.

END PROCEDURE.


//esta procedure s¢ tem sentido quanto por utilizado o tipo de dado jsonobject 
PROCEDURE extrairJsonParaArquivo:
    DEFINE INPUT PARAMETER pArquivo     AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER pObj         AS jsonObject.
    DEFINE INPUT PARAMETER cPropriedade AS CHARACTER   NO-UNDO.
    
    DEFINE VARIABLE lcJson AS LONGCHAR  NO-UNDO.

    IF cPropriedade = '' THEN
       lcJson  = pObj:getJsonText().
    ELSE
       lcJson  = pObj:getJsonText(cPropriedade).
    COPY-LOB lcJson TO FILE pArquivo.
END PROCEDURE.



PROCEDURE setArqJson:
    DEFINE INPUT  PARAMETER pNomeArquivo AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE oParser              AS ObjectModelParser NO-UNDO.
    DEFINE VARIABLE lcJson               AS LONGCHAR          NO-UNDO.
    ASSIGN cNomeArquivo = pNomeArquivo
           cTipoDados   = 'arquivo'.
    COPY-LOB FILE cNomeArquivo TO lcJson CONVERT TARGET CODEPAGE "UTF-8".
    oParser = NEW ObjectModelParser().
    oJsonObjectPayload = CAST(oParser:Parse(lcJson), JsonObject).
    DELETE OBJECT oParser      NO-ERROR.
END PROCEDURE.

/*
 NAO ê MAIS NECESSµRIO O NIVEL DEPOIS DA API ANALIZARJSONOBJECT, POIS ESTA API ANALISA TODOS OS NIVEIS NECESSµRIO TRAZENDO S‡ OS ÈLTIMOS N÷VEIS.
PROCEDURE setNivel:
    DEFINE INPUT  PARAMETER cNivel     AS CHARACTER   NO-UNDO.
END PROCEDURE.*/

PROCEDURE getTtEmitente:
    DEFINE OUTPUT PARAMETER TABLE FOR ttEmitente.
END PROCEDURE.

PROCEDURE getTTEmitenteExt:
    DEFINE OUTPUT PARAMETER TABLE FOR ttEmitenteExt.
END PROCEDURE.

PROCEDURE getTTAtividades:
    DEFINE OUTPUT PARAMETER TABLE FOR ttAtividades.
END PROCEDURE.


PROCEDURE exportTtEmitente:

    DEFINE INPUT  PARAMETER pArquivo AS CHARACTER   NO-UNDO.

    OUTPUT TO VALUE(pArquivo).
        FOR EACH ttEmitente:
            EXPORT DELIMITER "|" ttEmitente  EXCEPT  r-rowid.
        END.

    OUTPUT CLOSE.

END PROCEDURE.

PROCEDURE exportTTEmitenteExt:
    
    DEFINE INPUT  PARAMETER pArquivo AS CHARACTER   NO-UNDO.

    OUTPUT TO VALUE(pArquivo).
        FOR EACH ttEmitenteExt:
            EXPORT DELIMITER "|" ttEmitenteExt.
        END.

    OUTPUT CLOSE.

END PROCEDURE.

PROCEDURE exportTTAtividades:
    
    DEFINE INPUT  PARAMETER pArquivo AS CHARACTER   NO-UNDO.

    OUTPUT TO VALUE(pArquivo).
        FOR EACH ttAtividades:
            EXPORT DELIMITER "|" ttAtividades.
        END.                                 
    OUTPUT CLOSE.
END PROCEDURE.



PROCEDURE getDados:     
    DEFINE VARIABLE iCont AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cnpj  AS CHARACTER   NO-UNDO.

    RUN esapi/analisarJsonObject2.p(oJsonObjectPayload, OUTPUT TABLE ttJson).


    //extraindo dados para a tabela de emitente
    OUTPUT TO c:\temp\LOGvls1.txt.
    FOR EACH ttJson
        WHERE ttJson.tag_pai = 'principal'
        BREAK BY ttJson.agrup.
        IF FIRST-OF(ttJson.agrup) THEN DO:
           CREATE ttEmitente.
           CREATE ttEmitenteExt.
           ASSIGN iCont = iCont + 1
                  ttEmitente.cod-emitente      = iCont
                  ttEmitenteExt.cod-emitente   = ttEmitente.cod-emitente
                  ttEmitente.data-implant      = TODAY
                  ttemitente.identific         = 1.
        END.
        
        /*FIND ttEmitente
            WHERE ttEmitente.cod-emitente = iCont 
            NO-ERROR.
        FIND ttEmitenteExt 
            WHERE ttEmitenteExt.cod-emitente = iCont 
            NO-ERROR.
        */
        PUT UNFORMAT iCont ttJson.tag ttJson.valor SKIP .

        CASE trim(ttJson.tag):
            WHEN 'nome_abrev' THEN
                ASSIGN ttEmitente.nome-abrev        = ttJson.valor .
            WHEN 'nome_cli' THEN
                ASSIGN ttEmitente.nome-emit         = ttJson.valor.
            WHEN 'logradouro' THEN
                ASSIGN ttEmitente.endereco          = ttJson.valor.
            WHEN 'num_logradouro' THEN
                ASSIGN ttEmitente.endereco          = ttEmitente.endereco + ' ' + ttJson.valor.
            WHEN 'complemento' THEN
                ASSIGN ttEmitente.endereco          = ttEmitente.endereco + ' ' + ttJson.valor.

            WHEN 'bairro' THEN
                ASSIGN ttEmitente.bairro            = ttJson.valor.
            WHEN 'cep' THEN
                ASSIGN ttEmitente.cep               = ttJson.valor.
            WHEN 'municipio' THEN
                ASSIGN ttEmitente.cidade            = ttJson.valor.
            WHEN 'email' THEN
                ASSIGN ttEmitente.e-mail            = ttJson.valor
                       ttEmitenteExt.e-mail-fisc    = ttJson.valor .
            WHEN 'telefone' THEN
                ASSIGN ttEmitente.telefone          = ttJson.valor.
            WHEN 'cnpj' THEN
                ASSIGN ttEmitente.cgc               = ttJson.valor
                       ttEmitente.cgc-cob           = ttJson.valor.
            WHEN 'repres_id' THEN
                ASSIGN ttEmitente.cod-rep           = int(ttJson.valor).
            WHEN 'grupo' THEN
                ASSIGN ttEmitente.cod-gr-cli            = int(ttJson.valor).
            WHEN 'categoria' THEN
                ASSIGN ttEmitente.categoria         = ttJson.valor.
            WHEN 'portador' THEN
                ASSIGN ttEmitente.portador          = int(ttJson.valor).
            WHEN 'port_preferencial' THEN
                ASSIGN ttEmitente.port-prefer       = int(ttJson.valor).
            WHEN 'banco' THEN
                ASSIGN ttEmitente.cod-banco         = int(ttJson.valor).
            WHEN 'agencia' THEN
                ASSIGN ttEmitente.agencia           = ttJson.valor.
            WHEN 'conta_corrente' THEN
                ASSIGN ttEmitente.conta-corren      = ttJson.valor.
            WHEN 'valor_minimo' THEN
                ASSIGN ttEmitente.valor-minimo      = decimal(ttJson.valor).
            WHEN 'dias_atraso' THEN
                ASSIGN ttEmitente.nr-dias-atraso    = int(ttJson.valor).
            WHEN 'instruc_banc_1' THEN
                ASSIGN ttEmitente.ins-banc[1]       = int(ttJson.valor).
            WHEN 'instruc_banc_2' THEN
                ASSIGN ttEmitente.ins-banc[2]      = INt(ttJson.valor).
            WHEN 'receita_pdr' THEN
                ASSIGN ttEmitente.tp-rec-padrao     = int(ttJson.valor).
            WHEN 'cond_pagto' THEN
                ASSIGN ttEmitente.cod-cond-pag     = int(ttJson.valor).
            WHEN 'parceiro_edi' THEN
                ASSIGN ttEmitente.cod-parceiro-edi  = int(ttJson.valor).
            WHEN 'cod_msg_obs_nf' THEN
                ASSIGN ttEmitente.cod-mensagem      =INT(ttJson.valor).
            WHEN 'trans_redespacho' THEN
                ASSIGN ttEmitente.nome-tr-red       = ttJson.valor.
            WHEN 'calcula_multa' THEN
                ASSIGN ttEmitente.calcula-multa     = ttJson.valor = '1'.
            WHEN 'inscr_estadual' THEN
                ASSIGN ttEmitente.ins-estadual      = ttJson.valor.
            WHEN 'obs' THEN
                ASSIGN ttEmitente.observacoes       = ttEmitente.observacoes + chr(13) + 'Referencias Comerciais:' + CHR(13) +  ttJson.valor.
            WHEN 'opiniao_repres' THEN
                ASSIGN ttEmitente.observacoes       = ttEmitente.observacoes + chr(13) + 'Opini∆o Representante:' + CHR(13) +  ttJson.valor.
            WHEN 'id' THEN
                ASSIGN ttEmitente.dec-2             = dec(ttJson.valor).
            WHEN 'ramo_ativ' THEN
                ASSIGN ttEmitenteExt.cod-ramo-ativ  = int(ttJson.valor).
            WHEN 'email_comercial' THEN
                ASSIGN ttEmitenteExt.e-mail-comerc  = ttJson.valor.
            WHEN 'email_financeiro' THEN
                ASSIGN ttEmitenteExt.e-mail-financ  = ttJson.valor.
            WHEN 'transportadora_id' THEN
                ASSIGN ttEmitente.cod-transp        = int(ttJson.valor).
            WHEN 'modalidade' THEN
                ASSIGN ttEmitente.modalidade        = INT(ttJson.valor).
            WHEN 'uf' THEN
               ASSIGN ttEmitente.estado             = ttJson.valor . 
            WHEN 'natureza' THEN
               ASSIGN ttEmitente.natureza           = int(ttJson.valor) . 
            WHEN 'pais' THEN
               ASSIGN ttEmitente.pais               = ttJson.valor . 
            WHEN 'nome_comprador' THEN
               ASSIGN ttEmitenteExt.nome_comprador  = ttJson.valor . 
            WHEN 'cpf_comprador' THEN 
               ASSIGN ttEmitenteExt.cpf_comprador   = ttJson.valor . 
            WHEN 'celular_waths' THEN 
               ASSIGN ttEmitenteExt.celular_waths    = ttJson.valor .
            WHEN 'receber_nfe' THEN
               ASSIGN ttEmitente.log-nf-eletro       =  ttJson.valor = '1'.
            WHEN 'mod_prefer' THEN
               ASSIGN ttEmitente.mod-prefer          = INT(ttJson.valor).
            WHEN 'aplicacao' THEN
               ASSIGN ttEmitenteExt.aplicacao        = INT(ttJson.valor).
           WHEN 'cnpj_contabilidade' THEN
               ASSIGN ttEmitenteExt.cnpj_contabilidade  = ttJson.valor.

        END CASE.
    END.

    FOR EACH ttJson
        WHERE ttJson.tag_pai = 'atividades'
        BREAK BY ttJson.agrup.
        IF FIRST-OF(ttJson.agrup) THEN DO:
           CREATE ttAtividades.           
        END.
        CASE ttJson.tag:
        WHEN 'id' THEN
            ASSIGN ttAtividades.id             = ttJson.valor .
        WHEN 'cod_atividade' THEN
            ASSIGN ttAtividades.cod_atividade  = ttJson.valor .
        WHEN 'atividade_principal' THEN
            ASSIGN ttAtividades.ativ_principal = ttJson.valor = '1'.
        WHEN 'cnpj' THEN DO:
            ASSIGN cnpj = ttJson.valor.
            FIND ttEmitente
                WHERE ttEmitente.cgc = cnpj NO-ERROR.
            IF AVAIL ttEmitente THEN DO:
               ASSIGN ttAtividades.cod_emitente = ttEmitente.cod-emitente.
            END.
        END.
        END CASE.
    END.
    OUTPUT CLOSE.
    
    
END PROCEDURE.
