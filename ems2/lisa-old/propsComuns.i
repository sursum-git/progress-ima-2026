{lisa\varPropsComuns.i}

DEFINE VARIABLE hBo         AS HANDLE      NO-UNDO.
RUN esbo/boClienteAPI.p PERSISTENT SET hBo .
RUN iniciar               IN hBO.
RUN setDominio            IN hBO(cDominio).
RUN setPorta              IN hBO(iPorta).
RUN setDirJson            IN hBo(cDirJson).
RUN setUrlJson            IN hBo(cUrlJson).
RUN setHttps              IN hBo(NO).
RUN setTipoConteudo       IN hBo(cTipoConteudo).
RUN sincrChaveCab         IN hBo('tenantid',cTenantId).
RUN sincrChaveCab         IN hBo('authorization', cAutorizacao).





 
 
/*
CHAVE - 86159501 
CNPJ (CGC) - 06013812000581
FILIALLISA - 08
SENHA - 86159501
Armaz‚m - 01
*/
