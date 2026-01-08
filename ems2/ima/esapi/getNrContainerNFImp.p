/**************************************************************************
Programa: esapi/getNrContainerNFImp.p 
Autor: Tadeu Silva Parreiras
Objetivo: Buscar o numero do container pela tabela nfs_container do modulo
de importaá∆o.
ATENCAO: para que a informaá∆o fique correta, o setor de importaá∆o
deve associar a nota fiscal m∆e, logo ap¢s fazer a emiss∆o da mesma.
Caso n∆o seja encontrado o registro o programa retornara 999999 para que
fique clara a pendencia a ser corrigida.
Data: 03/2025
Modificacoes:
*****************************************************************************/
DEFINE INPUT  PARAMETER pEstab       AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pSerie       AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pDocto       AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pNatOperacao AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER pNrContainer AS INTEGER     NO-UNDO INIT 999999.


FIND nfs_container
    WHERE nfs_container.estab        = pEstab
    AND   nfs_container.serie        = pSerie
    AND   nfs_container.documento    = pDocto
    AND   nfs_container.nat_operacao = pNatOperacao
    NO-LOCK NO-ERROR.
IF AVAIL nfs_container THEN
   ASSIGN pNrContainer =  nfs_container.container_id.
