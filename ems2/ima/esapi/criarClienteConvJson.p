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

    RUN esapi/analisarJsonObject2.p(oJsonObjectPayload, OUTPUT TABLE ttJson).


    //extraindo dados para a tabela de emitente
    FOR EACH ttJson
        WHERE ttJson.tag_pai = 'principal'
        BREAK BY ttJson.agrup.
        IF FIRST-OF(ttJson.agrup) THEN DO:
           CREATE ttEmitente.  
           CREATE ttEmitenteExt.
            
           ASSIGN iCont = iCont + 1
                  ttEmitente.cod-emitente      = iCont
                  ttEmitenteExt.cod-emitente   = ttEmitente.cod-emitente
                  ttEmitente.data-implant      = TODAY.
        END.


        CASE ttJson.tag:
            WHEN 'nome_abrev' THEN
                ASSIGN ttEmitente.nome-abrev        = ttJson.tag .
            WHEN 'nome_cli' THEN
                ASSIGN ttEmitente.nome-emit         = ttJson.tag.
            WHEN 'logradouro' THEN
                ASSIGN ttEmitente.endereco          = ttJson.tag.
            WHEN 'bairro' THEN
                ASSIGN ttEmitente.bairro            = ttJson.tag.
            WHEN 'cep' THEN
                ASSIGN ttEmitente.cep               = ttJson.tag.
            WHEN 'municipio' THEN
                ASSIGN ttEmitente.cidade            = ttJson.tag.
            WHEN 'email' THEN
                ASSIGN ttEmitente.e-mail            = ttJson.tag
                       ttEmitenteExt.e-mail-fisc    = ttJson.tag.
            WHEN 'telefone' THEN
                ASSIGN ttEmitente.telefone          = ttJson.tag.
            WHEN 'cnpj' THEN
                ASSIGN ttEmitente.cgc               = ttJson.tag
                       ttEmitente.cgc-cob           = ttJson.tag.
            WHEN 'repres_id' THEN
                ASSIGN ttEmitente.cod-rep           = int(ttJson.tag).
            WHEN 'grupo' THEN
                ASSIGN ttEmitente.cod-gr-cli        = int(ttJson.tag).
            WHEN 'categoria' THEN
                ASSIGN ttEmitente.categoria         = ttJson.tag.
            WHEN 'portador' THEN
                ASSIGN ttEmitente.portador          = int(ttJson.tag).
            WHEN 'port_preferencial' THEN
                ASSIGN ttEmitente.port-prefer       = int(ttJson.tag).
            WHEN 'banco' THEN
                ASSIGN ttEmitente.cod-banco         = int(ttJson.tag).
            WHEN 'agencia' THEN
                ASSIGN ttEmitente.agencia           = ttJson.tag.
            WHEN 'conta_corrente' THEN
                ASSIGN ttEmitente.conta-corren      = ttJson.tag.
            WHEN 'valor_minimo' THEN
                ASSIGN ttEmitente.valor-minimo      = decimal(ttJson.tag).
            WHEN 'dias_atraso' THEN
                ASSIGN ttEmitente.nr-dias-atraso    = int(ttJson.tag).
            WHEN 'instruc_banc_1' THEN
                ASSIGN ttEmitente.ins-banc[1]       = int(ttJson.tag).
            WHEN 'instruc_banc_2' THEN
                ASSIGN ttEmitente.ins-banc[2]       = int(ttJson.tag).
            WHEN 'receita_pdr' THEN
                ASSIGN ttEmitente.tp-rec-padrao     = int(ttJson.tag).
            WHEN 'cond_pagto' THEN
                ASSIGN ttEmitente.cod-cond-pag      = int(ttJson.tag).
            WHEN 'parceiro_edi' THEN
                ASSIGN ttEmitente.cod-parceiro-edi  = int(ttJson.tag).
            WHEN 'cod_msg_obs_nf' THEN
                ASSIGN ttEmitente.cod-mensagem      = int(ttJson.tag).
            WHEN 'trans_redespacho' THEN
                ASSIGN ttEmitente.nome-tr-red       = ttJson.tag.
            WHEN 'calcula_multa' THEN
                ASSIGN ttEmitente.calcula-multa     = ttJson.tag = '1'.
            WHEN 'inscr_estadual' THEN
                ASSIGN ttEmitente.ins-estadual      = ttJson.tag.
            WHEN 'obs' THEN
                ASSIGN ttEmitente.observacoes       = ttJson.tag.
            WHEN 'id' THEN
                ASSIGN ttEmitente.dec-2             = dec(ttJson.tag) .
            WHEN 'ramo_ativ' THEN
                ASSIGN ttEmitenteExt.cod-ramo-ativ  = int(ttJson.tag).
            WHEN 'email_comercial' THEN
                ASSIGN ttEmitenteExt.e-mail-comerc  = ttJson.tag.
            WHEN 'email_financeiro' THEN
                ASSIGN ttEmitenteExt.e-mail-financ  = ttJson.tag.
            WHEN 'transportadora_id' THEN
                ASSIGN ttEmitente.cod-transp        = int(ttJson.tag).
            WHEN 'modalidade' THEN
                ASSIGN ttEmitente.modalidade        = int(ttJson.tag).
            WHEN 'uf' THEN
                ASSIGN ttEmitente.estado            = trim(ttJson.tag).
            WHEN 'natureza' THEN DO:
                ASSIGN ttEmitente.natureza          = int(ttJson.tag) .
                MESSAGE 'natureza:' ttEmitente.natureza
                    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
            END.
                
            WHEN 'pais' THEN
                ASSIGN ttEmitente.pais              = ttJson.tag .

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
            ASSIGN ttAtividades.id             = ttJson.tag .
        WHEN 'cod_atividade' THEN
            ASSIGN ttAtividades.cod_atividade  = ttJson.tag .
        WHEN 'atividade_principal' THEN
            ASSIGN ttAtividades.ativ_principal = ttJson.tag = '1'.


        END CASE.
    END.
    /*
    CREATE ttAtividades.
          ASSIGN ttAtividades.cod_atividade    = oJsonObjectAtiv:GetCharacter("cod_atividade") NO-ERROR.
          ASSIGN ttAtividades.cod_emitente     = int(ttPrincipal.id) NO-ERROR.
          ASSIGN ttAtividades.ativ_principal   = oJsonObjectAtiv:GetCharacter("atividade_principal") = '1' NO-ERROR.
   
    */
END PROCEDURE.

