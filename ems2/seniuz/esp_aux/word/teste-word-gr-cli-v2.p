DEF VAR c-arq-modelo      AS CHAR FORMAT "x(30)".         
DEF VAR c-arq-gerado-doc  AS CHAR FORMAT "x(30)".         
DEF VAR ChWord            AS COM-HANDLE.         
DEF VAR ChDoc             AS COM-HANDLE.

DEF VAR Tot-reg           AS INTEGER.

/******************************************************************************/
FOR EACH gr-cli
   WHERE gr-cli.cod-gr-cli >= 20
     AND gr-cli.cod-gr-cli <= 20
     NO-LOCK
     BREAK BY gr-cli.cod-gr-cli.

    IF FIRST-OF (gr-cli.cod-gr-cli) THEN DO:
        ASSIGN Tot-reg = 0. 

        RUN PI-GERAR-PEDIDO.
        RUN PI-GRAVAR-CABECALHO.
    END.

   FOR EACH emitente
     WHERE emitente.cod-gr-cli = gr-cli.cod-gr-cli
       AND emitente.cod-emitente <= 500
       NO-LOCK BY emitente.cod-gr-cli.

       RUN PI-GRAVAR-DETALHE.

       ASSIGN Tot-reg = Tot-reg + 1. 
   END.

   IF LAST-OF (gr-cli.cod-gr-cli) THEN DO:

        RUN PI-GRAVAR-RODAPE.

        RUN PI-ENCERRAR-PEDIDO.

   END.


END.

MESSAGE "Processo conclu¡do"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

/******************************************************************************/
PROCEDURE PI-GERAR-PEDIDO:

ASSIGN c-arq-modelo     = SEARCH("modelos/modelo-world-02.doc").
ASSIGN c-arq-gerado-doc = "c:\temp\ped" + STRING(gr-cli.cod-gr-cli) + ".DOC".

OS-COPY VALUE(c-arq-modelo) VALUE(c-arq-gerado-doc).

CREATE "word.application" ChWord.

ASSIGN ChWord:VISIBLE = TRUE
       ChWord:DisplayAlerts = FALSE
       ChDoc = ChWord:Documents:OPEN(c-arq-gerado-doc) NO-ERROR.

END PROCEDURE.

/******************************************************************************/
PROCEDURE PI-GRAVAR-CABECALHO:

ChWord:SELECTION:FONT:NAME = "CURRIER NEW". /*Tipo de letra*/
ChWord:SELECTION:FONT:SIZE = 13.            /*Tamanho da letra*/
ChWord:SELECTION:FONT:bold = YES.           /*Negrito */
ChWord:SELECTION:FONT:Italic = YES.         /*It lico*/       
ChWord:SELECTION:TypeText ("XXX Formul rio modelo para teste de impressÆo XXX"). /*Testo*/

chDoc:FormFields:ITEM("cod_gr_cli"):RESULT  = string(gr-cli.cod-gr-cli,"9999") NO-ERROR.
chDoc:FormFields:ITEM("cod_gr_cli2"):RESULT  = string(gr-cli.cod-gr-cli,"9999") NO-ERROR.
chDoc:FormFields:ITEM("cod_gr_cli3"):RESULT  = string(gr-cli.cod-gr-cli,"9999") NO-ERROR.
chDoc:FormFields:ITEM("descricao"):RESULT   = gr-cli.descricao NO-ERROR.
chDoc:FormFields:ITEM("lim_credito"):RESULT = gr-cli.lim-credito NO-ERROR.

ChWord:SELECTION:MoveDown(5,18).

END PROCEDURE.

/******************************************************************************/
PROCEDURE PI-GRAVAR-DETALHE:

ChWord:SELECTION:InsertRows.
ChWord:SELECTION:Collapse.

ChWord:SELECTION:TypeText (STRING(emitente.cod-emitente,"999999")).
ChWord:SELECTION:MoveRight.
ChWord:SELECTION:TypeText (emitente.nome-emi).
ChWord:SELECTION:MoveRight.
ChWord:SELECTION:TypeText (STRING (emitente.data-implant,"99/99/9999")).

END PROCEDURE.

/******************************************************************************/
PROCEDURE PI-GRAVAR-RODAPE:

chDoc:FormFields:ITEM("tot_reg"):RESULT = STRING (tot-reg,"9999") NO-ERROR.

END PROCEDURE.

/******************************************************************************/
PROCEDURE PI-ENCERRAR-PEDIDO:

ChDoc:SAVE().

RELEASE OBJECT ChDoc NO-ERROR.
ChWord:VISIBLE = true NO-ERROR.

/* ChWord:QUIT().                  */
/* RELEASE OBJECT ChWord NO-ERROR. */
/*                                                         */
/* RUN esapi/gera-pdf.p (INPUT c-arq-gerado-doc) NO-ERROR. */
/*                                                         */
/* OS-DELETE VALUE(c-arq-gerado-doc).                      */

END PROCEDURE.
