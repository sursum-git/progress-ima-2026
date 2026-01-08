/*****************************************************************************
Programa: esapi/getNfTriang.p
objetivo: a partir do rowid da nota de venda retornar o rowid da nota fiscal 
triangular caso a venda seja triangular.
autor:Tadeu Silva
Data: 04/2024    
***************************************************************************/

DEFINE INPUT  PARAMETER pRowidNF        AS ROWID       NO-UNDO.
DEFINE OUTPUT PARAMETER rowidTriang     AS ROWID       NO-UNDO.

DEFINE BUFFER b-nota-fiscal FOR nota-fiscal.

FIND nota-fiscal NO-LOCK
     WHERE ROWID(nota-fiscal) = pRowidNF NO-ERROR.
IF AVAIL nota-fiscal THEN DO:

    FIND ped-venda WHERE
         ped-venda.nr-pedcli = nota-fiscal.nr-pedcli AND
         ped-venda.nome-abrev = nota-fiscal.nome-ab-cli
         NO-LOCK NO-ERROR.
    IF NOT AVAIL ped-venda THEN
       RETURN 'nok'.
   
    IF ped-venda.nome-abrev-tri  = '' THEN
       RETURN 'ok'.

   // Localizar nota remessa
    FIND FIRST b-nota-fiscal 
        WHERE  b-nota-fiscal.nr-nota-fis >= nota-fiscal.nr-nota-fis 
        AND    b-nota-fiscal.cod-estabel = nota-fiscal.cod-estabel 
        AND    b-nota-fiscal.serie = nota-fiscal.serie 
        AND    b-nota-fiscal.nome-ab-cli = ped-venda.nome-abrev-tri 
        //AND    b-nota-fiscal.vl-tot-nota = nota-fiscal.vl-tot-nota  
        AND    b-nota-fiscal.dt-cancela = ? NO-ERROR. 
    IF AVAIL b-nota-fiscal THEN DO:
        ASSIGN rowidTriang = ROWID(b-nota-fiscal).
        RETURN 'ok'.
    END.            

END.








  
