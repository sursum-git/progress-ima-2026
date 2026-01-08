{lisa/varPropsComunshttps.i}
/*1
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
//{esp/util.i}
{esp/params.i}
{lisa/ssl_lisa.i}      
/*DEFINE VARIABLE protocolo       AS CHARACTER   NO-UNDO EXTENT 10.
DEFINE VARIABLE cripto          AS CHARACTER   NO-UNDO EXTENT 10. */
DEFINE VARIABLE lHttps          AS LOGICAL     NO-UNDO.

//a partir de 09/01/2025
IF TODAY >= 09.01.2025 THEN
DO:
    ASSIGN   cDominio    =    'li195956.protheus.cloudtotvs.com.br'
             iPorta      =    11257
             //lHttps      =    YES
             .
    
END.
ELSE DO:
    ASSIGN cDominio      =    'sobradocomercio114725.protheus.cloudtotvs.com.br'
           iPorta        =    4050
           //lHttps        =    NO
           . 
END.

ASSIGN       
       cDirJson                 =    '\\pv1\pv2\integracao_lisa'
       cUrlJson                 =    'http://imaonline.imatextil.com.br/integracao_lisa/'
       cTipoConteudo            =    'application/json'
       cTenantId                =    '03,08'
       cAutorizacao             =    'Basic YXBpd21zOmFwaXdtcyMyNDVA'
       cChave                   =    '86159501'
       cCNPJ                    =    '06013812000581'
       cFilial                  =    '08'
       cSENHA                   =    '86159501'
       cArmazem                 =    getArmazemLisa()
       //EXTENT(protocolo)        =     1
       //EXTENT(cripto)           =     1
      /* protocolo[1]             =     'TLSv1.2'
       cripto[1]                =     'AES128-GCM-SHA256' */
  .
 1*/
       
 
