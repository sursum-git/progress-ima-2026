DEF VAR c-arq-modelo      AS CHAR FORMAT "x(30)".         
DEF VAR c-arq-gerado-doc  AS CHAR FORMAT "x(30)".         
DEF VAR ChWord            AS COM-HANDLE.         
DEF VAR ChDoc             AS COM-HANDLE.

DEF VAR Tot-reg           AS INTEGER.

/******************************************************************************/


       
FIND pedido-compr WHERE  
     pedido-compr.num-pedido = 32016 NO-LOCK.
IF AVAIL pedido-compr THEN DO:
   FIND emitente WHERE 
        emitente.cod-emitente = pedido.cod-emitente NO-LOCK.
   RUN PI-GERAR-PEDIDO.
   RUN PI-GRAVAR-CABECALHO.
   
   FOR EACH ordem-compra WHERE
            ordem-compra.num-pedido =  pedido-compr.num-pedido NO-LOCK,
       EACH prazo-compra WHERE
            prazo-compra.numero-ordem = ordem-compra.numero-ordem NO-LOCK:
       
       FIND item WHERE
            item.it-codigo = prazo-compra.it-codigo no-lock.
       RUN PI-GRAVAR-DETALHE. 
   END.
   
END.












/*
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
*/
MESSAGE "Processo conclu¡do"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

/******************************************************************************/
PROCEDURE PI-GERAR-PEDIDO:

ASSIGN c-arq-modelo     = SEARCH("m:\aula1.doc").
ASSIGN c-arq-gerado-doc = "c:\temp\aula1.DOC".

OS-COPY VALUE(c-arq-modelo) VALUE(c-arq-gerado-doc).

CREATE "word.application" ChWord.

ASSIGN ChWord:VISIBLE = TRUE
       ChWord:DisplayAlerts = FALSE
       ChDoc = ChWord:Documents:OPEN(c-arq-gerado-doc) NO-ERROR.

END PROCEDURE.

/******************************************************************************/
PROCEDURE PI-GRAVAR-CABECALHO:
/* usar o default do formulario 
ChWord:SELECTION:FONT:NAME = "CURRIER NEW". /*Tipo de letra*/
ChWord:SELECTION:FONT:SIZE = 13.            /*Tamanho da letra*/
ChWord:SELECTION:FONT:bold = YES.           /*Negrito */
ChWord:SELECTION:FONT:Italic = YES.         /*It lico*/       
*/
chDoc:FormFields:ITEM("t_forn"):RESULT  = emitente.nome-emit NO-ERROR.
chDoc:FormFields:ITEM("t_end"):RESULT  = emitente.endereco NO-ERROR.
chDoc:FormFields:ITEM("t_tel"):RESULT  = emitente.telefone[1] NO-ERROR.
chDoc:FormFields:ITEM("t_cont"):RESULT   = emitente.contato[1] NO-ERROR.
chDoc:FormFields:ITEM("t_ped"):RESULT = pedido-compr.num-pedido NO-ERROR.
chDoc:FormFields:ITEM("t_data"):RESULT = today NO-ERROR.

ChWord:SELECTION:MoveDown(5,12).

END PROCEDURE.

/******************************************************************************/
PROCEDURE PI-GRAVAR-DETALHE:

ChWord:SELECTION:InsertRows.
ChWord:SELECTION:Collapse.

ChWord:SELECTION:TypeText (STRING(item.it-codigo,"999999")).
ChWord:SELECTION:MoveRight.
ChWord:SELECTION:TypeText (item.desc-item).
ChWord:SELECTION:MoveRight.
ChWord:SELECTION:TypeText (prazo-compra.quantidade).
ChWord:SELECTION:MoveRight.
ChWord:SELECTION:TypeText (item.un).
ChWord:SELECTION:MoveRight.
ChWord:SELECTION:TypeText (ordem-compra.preco-fornec).
ChWord:SELECTION:MoveRight.
ChWord:SELECTION:TypeText (ordem-compra.aliquota-ipi).
ChWord:SELECTION:MoveRight.
ChWord:SELECTION:TypeText ( (prazo-compra.quantidade * ordem-compra.preco-fornec) +                                      
                            ((prazo-compra.quantidade * ordem-compra.preco-fornec) * ordem-compra.aliquota-ipi * 0.01) + 
                            ((prazo-compra.quantidade * ordem-compra.preco-fornec) * ordem-compra.valor-taxa * 0.01) -   
                            ((prazo-compra.quantidade * ordem-compra.preco-fornec) * ordem-compra.perc-descto * 0.01)).

ChWord:SELECTION:MoveRight.
ChWord:SELECTION:TypeText (STRING(item.it-codigo,"999999")).




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
