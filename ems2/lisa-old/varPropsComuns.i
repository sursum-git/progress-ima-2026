DEFINE VARIABLE cDominio        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iPorta          AS INTEGER     NO-UNDO.
DEFINE VARIABLE cDirJson        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cUrlJson        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTipoConteudo   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTenantId       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cAutorizacao    AS CHARACTER   NO-UNDO.
//GET
DEFINE VARIABLE cChave      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cCNPJ       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFilial     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cSenha      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cArmazem    AS CHARACTER   NO-UNDO.



ASSIGN 
       cDominio                 =    'sobradocomercio114725.protheus.cloudtotvs.com.br'
       iPorta                   =    4050
       cDirJson                 =    '\\pv1\pv2\integracao_lisa'
       cUrlJson                 =    'http://imaonline.imatextil.com.br/integracao_lisa/'
       cTipoConteudo            =    'application/json'
       cTenantId                =    '03,08'
       cAutorizacao             =    'Basic YXBpd21zOmFwaXdtcyMyNDVA'
       cChave                   =    '86159501'
       cCNPJ                    =    '06013812000581'
       cFilial                  =    '08'
       cSENHA                   =    '86159501'
       cArmazem                 =    '01'
       .
