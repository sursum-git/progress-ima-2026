/*********************************************************'*****************************************************************************
programa:esapi/imprime-nf-527.p
objetivo:possibilitar a impress∆o inndividual de uma nota fiscal pelo programa ft0527 passando apenas o rowid da nota fiscal
autor: Tadeu Silva
data:09/2025
***************************************************************************************************************************************/
{esp/util.i}
{esapi/ttFt0527.i}
{utp/ut-glob.i}
{esp/params.i}
DEFINE INPUT  PARAMETER rowidNF    AS ROWID       NO-UNDO.
DEFINE INPUT  PARAMETER logAbrePDF AS LOGICAL     NO-UNDO. 

DEFINE VARIABLE cArquivo AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cXML     AS CHARACTER   NO-UNDO.
FOR FIRST nota-fiscal FIELDS(cod-estabel serie nr-nota-fis) 
    WHERE rowid(nota-fiscal) = rowidNF NO-LOCK:    
END.

/*
ASSIGN cArquivo = getDirDanfe(nota-fiscal.cod-estabel) + "\" +
                   string(year(nota-fiscal.dt-emis-nota)) + "\" + 
                   string(MONTH(nota-fiscal.dt-emis-nota),'99') + "\" +                    
                   nota-fiscal.cod-estabel + "-" +
                   nota-fiscal.serie + "-" +
                   nota-fiscal.nr-nota-fis  + ".PDF"                   
*/

RUN esapi/getArqsNF.p(ROWID(nota-fiscal),OUTPUT cArquivo,OUTPUT cXML).    
.
IF SEARCH(cArquivo) <> ? THEN DO:
   IF logAbrePDF THEN DO:
       OS-COMMAND SILENT VALUE(' start ' + cArquivo).      
   END.   
   RETURN 'OK' .
END.  
ELSE DO:
   /* MESSAGE cArquivo
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
END.




CREATE tt-param-aux.
ASSIGN tt-param-aux.usuario              = c-seg-usuario
       tt-param-aux.destino              = IF logAbrePDF THEN 3  ELSE 2
       tt-param-aux.data-exec            = today
       tt-param-aux.hora-exec            = time
       tt-param-aux.v_num_tip_aces_usuar = 1 // padrao informado no ft0527
       tt-param-aux.ep-codigo            = i-ep-codigo-usuario
       tt-param-aux.c-cod-estabel        = nota-fiscal.cod-estabel
       tt-param-aux.c-serie              = nota-fiscal.serie
       tt-param-aux.c-nr-nota-fis-ini    = nota-fiscal.nr-nota-fis
       tt-param-aux.c-nr-nota-fis-fim    = nota-fiscal.nr-nota-fis
       tt-param-aux.de-cdd-embarq-ini    = 0
       tt-param-aux.de-cdd-embarq-fim    = 9999999999999999
       tt-param-aux.da-dt-saida          = ?
       tt-param-aux.c-hr-saida           = '' 
       tt-param-aux.nr-copias            = 1
       tt-param-aux.cod-febraban         = 0
       tt-param-aux.cod-portador         = 0
       tt-param-aux.rs-imprime           = nota-fiscal.ind-sit-nota 
       tt-param-aux.l-gera-danfe-xml     = FALSE
       tt-param-aux.c-dir-hist-xml       = ''
       tt-param-aux.imprime-bloq         = FALSE
       .

CREATE tt-digita.
ASSIGN tt-digita.ordem   = 1
       tt-digita.exemplo = ''.



RAW-TRANSFER tt-param-aux to raw-param.

for each tt-digita:
    create tt-raw-digita.
    raw-transfer tt-digita to tt-raw-digita.raw-digita.
end. 

SESSION:SET-WAIT-STATE("general":U).

/* Imprime a Nota Fiscal */

//{include/i-rprun.i ftp/ft051627.p}
RUN ftp/ft0527rp.p(INPUT raw-param, INPUT TABLE tt-raw-digita).

SESSION:SET-WAIT-STATE("":U).



