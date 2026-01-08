DEFINE VARIABLE cDominio        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iPorta          AS INTEGER     NO-UNDO.
DEFINE VARIABLE cDirJson        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cUrlJson        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTipoConteudo   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTenantId       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cAutorizacao    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE protocolo       AS CHARACTER   NO-UNDO EXTENT.
DEFINE VARIABLE cripto          AS CHARACTER   NO-UNDO EXTENT.
//GET
DEFINE VARIABLE cChave      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cCNPJ       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFilial     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cSenha      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cArmazem    AS CHARACTER   NO-UNDO.


//https://li195954.protheus.cloudtotvs.com.br:8401/rest
ASSIGN 
       cDominio                 =    'li195954.protheus.cloudtotvs.com.br'
       iPorta                   =    8401
       cDirJson                 =    '\\pv1\pv2\integracao_lisa'
       cUrlJson                 =    'http://imaonline.imatextil.com.br/integracao_lisa/'
       cTipoConteudo            =    'application/json'
       cTenantId                =    '03,08'
       cAutorizacao             =    'Basic YXBpd21zOmFwaXdtcyMyNDVA'
       cChave                   =    '86159501'
       cCNPJ                    =    '06013812000581'
       cFilial                  =    '08'
       cSENHA                   =    '86159501'
       cArmazem                 =    IF TODAY < 12.04.2025 THEN '01' ELSE '02'
       EXTENT(protocolo)        =     1
       EXTENT(cripto)           =     1
       protocolo[1]             =     'TLSv1.2'
       cripto[1]                =     'AES128-GCM-SHA256' 
       .
