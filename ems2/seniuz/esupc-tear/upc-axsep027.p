/*******************************************************************************

 UPC para o programa -> axsep027 (Envio NF-e)
 
 Objetivo: Manipular informacoes geradas no XML das Notas Fiscais Referenciadas.
                       
*******************************************************************************/

{cdp/cdcfgdis.i}
{include/i-epc200.i1}

DEFINE INPUT        PARAM p-ind-event AS CHAR NO-UNDO.
DEFINE INPUT-OUTPUT PARAM TABLE FOR tt-epc.

DEFINE VARIABLE h-tabela     AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE h-ttNFRef    AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE hQueryBuffer AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE h-query      AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE h-serie      AS WIDGET-HANDLE NO-UNDO.

DEF VAR h-cod-estabel-ref AS WIDGET-HANDLE NO-UNDO.
DEF VAR h-serie-ref       AS WIDGET-HANDLE NO-UNDO.
DEF VAR h-nr-nota-fis-ref AS WIDGET-HANDLE NO-UNDO.

DEF VAR c-especie          AS CHAR NO-UNDO.
DEF VAR c-nome-transp      AS CHAR NO-UNDO.
DEF VAR c-cgc-transp-remet AS CHAR NO-UNDO.
DEF VAR c-endereco-transp  AS CHAR NO-UNDO.
DEF VAR c-cidade-transp    AS CHAR NO-UNDO.
DEF VAR c-estado-transp    AS CHAR NO-UNDO.
DEF VAR c-ie-transp        AS CHAR NO-UNDO.
DEF VAR c-placa            AS CHAR NO-UNDO.

IF  p-ind-event = "AtualizaDadosNFe":U THEN DO:
    FOR FIRST tt-epc NO-LOCK
        WHERE tt-epc.cod-event     = "AtualizaDadosNFe"
          AND tt-epc.cod-parameter = "ttNFRef" :  /* Tabela ttNFRef - Notas Fiscais Referenciadas */

        ASSIGN h-ttNFRef    = WIDGET-HANDLE(tt-epc.val-parameter)
               hQueryBuffer = h-ttNFRef:DEFAULT-BUFFER-HANDLE.

        IF  VALID-HANDLE(hQueryBuffer) THEN DO:

            CREATE QUERY h-query.
                         h-query:SET-BUFFERS(hQueryBuffer).
                         h-query:QUERY-PREPARE('for each ' + h-ttNFRef:NAME + ' no-lock').
                         h-query:QUERY-OPEN().

            REPEAT ON ERROR UNDO, LEAVE:

                h-query:GET-NEXT() NO-ERROR.
                IF  h-query:QUERY-OFF-END THEN LEAVE.

                ASSIGN h-serie = hQueryBuffer:BUFFER-FIELD('serie') NO-ERROR.

                /* Troca s‚rie U, UN, 01, 001 ou branca para "0" */
                IF  h-serie:BUFFER-VALUE = ""
                OR  h-serie:BUFFER-VALUE = "U"
                OR  h-serie:BUFFER-VALUE = "UN"
                OR  h-serie:BUFFER-VALUE = "01" 
                OR  h-serie:BUFFER-VALUE = "001" THEN
                    ASSIGN h-serie:BUFFER-VALUE = "0".

            END.
        END.
    END.
END.

RETURN "OK":U.

