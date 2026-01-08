/* Programa: CC0305RP.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Compras              
** Objetivo: Gerar Pedido de Compra
** Autor...: PRODB-Antonio G. Souza (Nov 2003)
**
**           Foi substituido a gera‡Æo e impressÆo via REPORT BUILDER
**           por documento WORD e gera‡Æo de PDF via PDF995.
**           F bio Coelho Lanza (Novembro 2010)
** 
*/

 DEFINE TEMP-TABLE tt-param NO-UNDO
     FIELD destino          AS INTEGER
     FIELD arquivo          AS CHAR FORMAT "x(35)"
     FIELD usuario          AS CHAR FORMAT "x(12)"
     FIELD data-exec        AS DATE
     FIELD hora-exec        AS INTEGER
     FIELD pedido-ini       LIKE pedido-compr.num-pedido
     FIELD pedido-fin       LIKE pedido-compr.num-pedido
     FIELD comprador-ini    LIKE pedido-compr.responsavel
     FIELD comprador-fim    LIKE pedido-compr.responsavel
     FIELD num-copias       AS INT
     FIELD e-mail           AS LOG
     FIELD subject          AS CHAR FORMAT "x(40)"
     FIELD texto            AS CHAR FORMAT "x(2000)".

 DEFINE TEMP-TABLE tt-raw-digita 
        FIELD raw-digita AS RAW.

 DEF INPUT PARAMETER raw-param AS RAW NO-UNDO.
 DEF INPUT PARAMETER TABLE FOR tt-raw-digita.

 CREATE tt-param.
 RAW-TRANSFER raw-param TO tt-param.

/**************/
/** Includes **/
/**************/
{include/tt-edit.i}
{include/pi-edit.i}

DEF VAR h-acomp     AS HANDLE NO-UNDO.
DEF VAR h-prog      AS HANDLE NO-UNDO.
DEF VAR c-narrativa AS CHAR.
DEF VAR c-comando   AS CHAR.
DEF VAR de-total    AS DEC FORMAT ">>>,>>>,>>9.9999".

/* Variaveis da rotina do WORD */
 DEF VAR c-arq-modelo     AS CHAR.  
 DEF VAR c-arq-gerado-doc AS CHAR.
 DEF VAR c-arq-pdf        AS CHAR.
 DEF VAR c-arq-copia     AS CHAR.
 DEF VAR c-imp-padrao     AS CHAR.
 DEF VAR ChWord           AS COM-HANDLE NO-UNDO.         
 DEF VAR ChDoc            AS COM-HANDLE NO-UNDO.
 
/* Carrega Arquivo de Configura‡Æo ImpressÆo PDF995 */
 IF SEARCH("C:\PDF995\RES\PDF995.INI") = "" THEN DO.
     MESSAGE "O Utilitario PDF995 nÆo foi encontrado." SKIP
             "NÆo ‚ possivel a execu‡Æo do programa."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     RETURN.
 END. 
 LOAD "PDF995" DIR "C:\PDF995\RES". 
 USE "PDF995".

/* Abertura e Cria‡Æo de Arquivo do Word */
 ASSIGN c-arq-modelo     = SEARCH("modelos/ped-compras.doc")
        c-arq-gerado-doc = SESSION:TEMP-DIRECTORY + "ped-compras.doc"
        c-arq-copia     = SESSION:TEMP-DIRECTORY + "ped-ultimo.doc". 

 ASSIGN c-comando = "copy " + c-arq-gerado-doc + " " + c-arq-copia.

 OS-COPY VALUE(c-arq-modelo) VALUE(c-arq-gerado-doc).

 CREATE "Word.Application":U chWord NO-ERROR.
 IF ChWord = ? THEN DO:
    MESSAGE "O Aplicativo WORD nÆo foi encontrado." SKIP
            "NÆo ‚ possivel a execu‡Æo do programa."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
 END.

 ASSIGN ChWord:VISIBLE = FALSE    
        ChWord:DisplayAlerts = FALSE
        ChDoc = ChWord:Documents:OPEN(c-arq-gerado-doc) NO-ERROR.

 RUN utp/ut-utils.p PERSISTENT SET h-prog.

 RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
 {utp/ut-liter.i Imprimindo *}

 RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

 FOR EACH pedido-compr WHERE  
          pedido-compr.num-pedido >= tt-param.pedido-ini AND
          pedido-compr.num-pedido <= tt-param.pedido-fin NO-LOCK.

     FIND FIRST ordem-compra WHERE 
                ordem-compra.num-pedido = pedido-compr.num-pedido NO-LOCK. 
     FIND comprador WHERE
          comprador.cod-comprado = ordem-compra.cod-comprado NO-LOCK NO-ERROR.

     FIND estabelec WHERE
          estabelec.cod-estabel = pedido-compr.cod-estabel NO-LOCK NO-ERROR.
     FIND estab-distrib WHERE
          estab-distrib.cod-estab = estabelec.cod-estabel NO-LOCK NO-ERROR.

     FIND emitente WHERE 
          emitente.cod-emitente = pedido.cod-emitente NO-LOCK.

     FIND FIRST cont-emit WHERE
                cont-emit.cod-emitente =  emitente.cod-emitente NO-LOCK NO-ERROR.
        
     IF tt-param.e-mail THEN DO.
        IF NOT AVAIL cont-emit OR (AVAIL cont-emit AND cont-emit.e-mail = "") THEN DO.
            MESSAGE "Fornecedor sem e-mail Cadastrado, Imposs¡vel enviar e-mail"
                    VIEW-AS ALERT-BOX.
            RETURN.
        END.
     END.
     RUN pi-cabec-form.
     ASSIGN c-arq-pdf   = SESSION:TEMP-DIRECTORY + STRING(pedido-compr.num-pedido) + ".PDF".
            de-total    = 0.
     FOR EACH ordem-compra WHERE
              ordem-compra.num-pedido =  pedido.num-pedido AND
              ordem-compra.situacao   <> 4 NO-LOCK,
         EACH prazo-compra WHERE
              prazo-compra.numero-ordem = ordem-compra.numero-ordem AND
              prazo-compra.situacao <> 4 NO-LOCK
           BY prazo-compra.it-codigo +
              STRING(prazo-compra.parcela) DESCEND.

         RUN pi-acompanhar IN h-acomp (INPUT "Pedido de Compra: "    + STRING(ordem-compra.num-pedido) +
                                             " Item: " + prazo-compra.it-codigo).

         IF ordem-compra.it-codigo <> "" THEN
            FIND item WHERE
                 item.it-codigo = prazo-compra.it-codigo NO-LOCK NO-ERROR.

         /* Rotina quebrar a narrativa a cada 76 caracter */
         RUN pi-print-editor(INPUT ordem-compra.narrativa, INPUT 76).
         ASSIGN c-narrativa = "".
         FOR EACH tt-editor.
             IF tt-editor.conteudo <> "" THEN DO:
                IF c-narrativa = "" THEN
                   ASSIGN c-narrativa = tt-editor.conteudo.
                ELSE
                   ASSIGN c-narrativa = c-narrativa + CHR(10) + tt-editor.conteudo.
                   
             END.
         END.
         
         /* Insere Uma Linha Na Tabela do Documento */
         ChWord:SELECTION:InsertRows.
         ChWord:SELECTION:Collapse.
         /* Rotina para Imprimir Detalhes do Documento */
         ChWord:SELECTION:TypeText (prazo-compra.it-codigo).
         ChWord:SELECTION:MoveRight.
         ChWord:SELECTION:TypeText (item.desc-item + chr(10) + c-narrativa).
         ChWord:SELECTION:MoveRight.
         ChWord:SELECTION:TypeText (prazo-compra.quantidade).
         ChWord:SELECTION:MoveRight.
         ChWord:SELECTION:TypeText (prazo-compra.un).
         ChWord:SELECTION:MoveRight.
         ChWord:SELECTION:TypeText (STRING(ordem-compra.preco-fornec,">>>,>>9.9999")).
         ChWord:SELECTION:MoveRight.
         ChWord:SELECTION:TypeText (STRING(ordem-compra.aliquota-ipi, ">9.99") + "%" ).
         ChWord:SELECTION:MoveRight.
         ChWord:SELECTION:TypeText (STRING(((prazo-compra.quantidade * ordem-compra.preco-fornec) +                                      
                                    ((prazo-compra.quantidade * ordem-compra.preco-fornec) * ordem-compra.aliquota-ipi * 0.01) + 
                                    ((prazo-compra.quantidade * ordem-compra.preco-fornec) * ordem-compra.valor-taxa * 0.01) -   
                                    ((prazo-compra.quantidade * ordem-compra.preco-fornec) * ordem-compra.perc-descto * 0.01)),">>>>,>>9.99")).    
         ChWord:SELECTION:MoveRight.
         ChWord:SELECTION:TypeText (prazo-compra.data-entrega).
         ChWord:SELECTION:MoveRight.
         ChWord:SELECTION:TypeText (ordem-compra.requisitante).

         ChWord:SELECTION:MoveLeft. 
         ChWord:SELECTION:MoveLeft. 
         ChWord:SELECTION:MoveLeft. 
         ChWord:SELECTION:MoveLeft. 
         ChWord:SELECTION:MoveLeft. 
         ChWord:SELECTION:MoveLeft. 
         ChWord:SELECTION:MoveLeft. 
         ChWord:SELECTION:MoveLeft. 
         ASSIGN de-total =  de-total + ((prazo-compra.quantidade * ordem-compra.preco-fornec) +                                       
                                        ((prazo-compra.quantidade * ordem-compra.preco-fornec) * ordem-compra.aliquota-ipi * 0.01) +  
                                        ((prazo-compra.quantidade * ordem-compra.preco-fornec) * ordem-compra.valor-taxa * 0.01) -    
                                        ((prazo-compra.quantidade * ordem-compra.preco-fornec) * ordem-compra.perc-descto * 0.01)).   

     END.
   
     FIND cond-pagto OF pedido-compr NO-LOCK NO-ERROR.
     FIND transporte OF pedido-compr NO-LOCK NO-ERROR.
     /* ImpressÆo do Rodape do Formulario */
     chDoc:FormFields:ITEM("obs"):RESULT = pedido-compr.comentarios NO-ERROR.

     chDoc:FormFields:ITEM("total"):RESULT = STRING(de-total,">>>,>>>,>>9.99")  NO-ERROR.

     chDoc:FormFields:ITEM("pag_dias"):RESULT = IF AVAIL cond-pagto      
                                                THEN cond-pagto.descricao ELSE ""  NO-ERROR.

     chDoc:FormFields:ITEM("entrega"):RESULT = pedido-compr.compl-entrega NO-ERROR.

     chDoc:FormFields:ITEM("t_frete"):RESULT = IF pedido-compr.frete = 1      
                                               THEN "CIF" ELSE "FOB"  NO-ERROR.
      
     chDoc:FormFields:ITEM("transportadora"):RESULT = IF AVAIL transporte      
                                                      THEN transporte.nome ELSE ""  NO-ERROR.

     chDoc:FormFields:ITEM("trans_fone"):RESULT = IF AVAIL transporte      
                                                      THEN transporte.telefone ELSE ""  NO-ERROR.

     IF tt-param.e-mail THEN DO:
        /* Seta Para Gerar PDF */
        PUT-KEY-VALUE SECTION "Parameters" KEY "Output File" VALUE c-arq-pdf.

        /* Salva a Impressora Default */ 
        c-imp-padrao = ChWord:ActivePrinter. 

        /* Faz a Impressora PDF995 a Impressora Default */
        ChWord:ActivePrinter = "PDF995". 

        chDoc:PrintOut(). 
        PAUSE 2 NO-MESSAGE. 

        /* Restaura a Impressora Defautl do Windows */
        ChWord:ActivePrinter = c-imp-padrao. 

        /* Restaura o pdf995.ini original */
        PUT-KEY-VALUE SECTION "Parameters" KEY "Output File" VALUE " ".
     END.

     IF tt-param.destino = 1 THEN
        ChDoc:PrintOut(). /* ImpressÆo do Documento */  

     IF tt-param.e-mail THEN DO:
        PAUSE 15 NO-MESSAGE. /* Aguarda o final da gera‡Æo do arquivo PDF */
        
        ASSIGN tt-param.texto = REPLACE(tt-param.texto,"#PED",STRING(pedido-compr.num-pedido)).
        ASSIGN tt-param.texto = REPLACE(tt-param.texto,"#COMP",comprador.nome).
        ASSIGN tt-param.texto = REPLACE(tt-param.texto,"#FONE","Fone: " + comprador.telefone[1] + "   E-mail: " + comprador.e-mail).

        RUN esapi/esapi001.p (INPUT cont-emit.e-mail, /* e-mail */
                              INPUT tt-param.subject, /* Assunto */
                              INPUT tt-param.texto, /* Mensagem */
                              INPUT c-arq-pdf,  /*arquivo anexo*/
                              INPUT YES). /* Mostra Erros */
     END.
     OS-DELETE VALUE(c-arq-pdf).  
    
     /* Fecha Documento */
     chDoc:CLOSE().
     RELEASE OBJECT chDoc NO-ERROR.
     OS-COMMAND SILENT VALUE(c-comando). /* Salva Ultimo Documento Gerado */
     OS-DELETE VALUE(c-arq-gerado-doc). 

     OS-COPY VALUE(c-arq-modelo) VALUE(c-arq-gerado-doc).
     ASSIGN ChDoc = ChWord:Documents:OPEN(c-arq-gerado-doc) NO-ERROR. 
     
 END. /* pedido-compra */
 
 /* Restaura o pdf995.ini original */
 PUT-KEY-VALUE SECTION "Parameters" KEY "Output File" VALUE " ".
 UNLOAD "PDF995".

  /* Fecha Documento */
 chDoc:CLOSE().
 RELEASE OBJECT chDoc NO-ERROR.
 chWord:QUIT().
 RELEASE OBJECT chWord NO-ERROR.
 RUN pi-finalizar in h-acomp.

 IF tt-param.destino = 3 AND NOT tt-param.e-mail THEN DO.
   RUN EXECUTE IN h-prog(INPUT "winword.exe",
                         INPUT c-arq-copia).
   DELETE PROCEDURE h-prog.
 END.
 PAUSE 15 NO-MESSAGE.
 OS-DELETE VALUE(c-arq-gerado-doc). 
 OS-DELETE VALUE(c-arq-copia). 


/* P  R  O  C  E  D  I  M  E  N  T  O  S */
/* ------------------------------------- */

 PROCEDURE pi-cabec-form.

 DEF VAR c-cgc AS CHAR.

 /* Rotina para Gravar Campos de Controle Criados no Documento Word */
 RUN pi-formata-cgc (INPUT estabelec.cgc, OUTPUT c-cgc).
 chDoc:FormFields:ITEM("t_cnpj"):RESULT      = c-cgc  NO-ERROR.
 chDoc:FormFields:ITEM("t_ie"):RESULT        = estabelec.ins-estadual NO-ERROR.
 chDoc:FormFields:ITEM("t_end"):RESULT       = TRIM(estabelec.endereco) + " - " + TRIM(estabelec.bairro)  + 
                                               " CEP: " + STRING(estabelec.cep, "99999,999")  + " - " + TRIM(estabelec.cidade) + "/" +
                                               TRIM(estabelec.estado)  NO-ERROR.
 chDoc:FormFields:ITEM("t_fone"):RESULT      = comprador.telefone[2] NO-ERROR.
 chDoc:FormFields:ITEM("t_fax"):RESULT       = comprador.telefax NO-ERROR.
 chDoc:FormFields:ITEM("comprador"):RESULT   = comprador.nome NO-ERROR.
 chDoc:FormFields:ITEM("comp_fone"):RESULT   = comprador.telefone[1] NO-ERROR.
 chDoc:FormFields:ITEM("comp_email"):RESULT  = comprador.e-mail NO-ERROR.
 chDoc:FormFields:ITEM("fornecedor"):RESULT  = STRING(emitente.cod-emitente, ">>9,999") + " - " + emitente.nome-emit NO-ERROR.
 chDoc:FormFields:ITEM("f_end"):RESULT       = emitente.endereco + " - " + emitente.bairro + " - " +
                                               emitente.cidade + " - " + emitente.estado + " - " + "Cep: " + STRING(emitente.cep) NO-ERROR.
 chDoc:FormFields:ITEM("f_fone"):RESULT      = emitente.telefone[1] NO-ERROR.
 chDoc:FormFields:ITEM("f_fax"):RESULT       = emitente.telefax NO-ERROR.
 RUN pi-formata-cgc (INPUT emitente.cgc, OUTPUT c-cgc).
 chDoc:FormFields:ITEM("f_cnpj"):RESULT      = c-cgc  NO-ERROR.
 chDoc:FormFields:ITEM("f_contato"):RESULT   = IF AVAIL cont-emit          
                                               THEN cont-emit.nome ELSE ""  NO-ERROR.
 chDoc:FormFields:ITEM("f_ie"):RESULT        = emitente.ins-estadual NO-ERROR.
 chDoc:FormFields:ITEM("f_email"):RESULT     = emitente.e-mail NO-ERROR.
 chDoc:FormFields:ITEM("n_pedido"):RESULT    = STRING(pedido-compr.num-pedido, ">>>9,999") NO-ERROR.
 chDoc:FormFields:ITEM("data_pedido"):RESULT = pedido-compr.data-pedido  NO-ERROR.

 ChWord:SELECTION:MoveDown(5,10).   /* Move 10 Linhas para baixo  */

END PROCEDURE.

PROCEDURE pi-formata-cgc.

 DEF INPUT  PARAMETER c-cgc-ent AS CHAR.
 DEF OUTPUT PARAMETER c-cgc     AS CHAR.

 IF LENGTH(c-cgc-ent) = 14 THEN
    ASSIGN c-cgc = SUBSTR(c-cgc-ent,  1, 2) + "." + SUBSTR(c-cgc-ent, 3, 3) + "." +
                   SUBSTR(c-cgc-ent,  6, 3) + "/" + SUBSTR(c-cgc-ent, 9, 4) + "-" +
                   SUBSTR(c-cgc-ent, 13, 2).
 ELSE
    ASSIGN c-cgc = SUBSTR(c-cgc-ent,  1, 3) + "." + SUBSTR(c-cgc-ent, 4, 3) + "." +
                   SUBSTR(c-cgc-ent,  7, 3) + "-" + SUBSTR(c-cgc-ent, 10, 2).

END PROCEDURE.

PROCEDURE pi-tirar-carac.

 DEF INPUT-OUTPUT  PARAMETER c-texto AS CHAR.
 DEF VAR i AS INT.
 DEF VAR l AS INT.
 
 REPEAT i = 1 TO LENGTH(c-texto).
     IF SUBSTRING(c-texto,i,1) = CHR(10) THEN DO:
        OVERLAY(c-texto,i,1) = " ".
     END.            
 END.
 
 REPEAT:
     l = INDEX(c-texto,"  ").
     IF l = 0 THEN LEAVE.
     REPEAT i = 1 TO LENGTH(c-texto).                                                     
         IF SUBSTRING(c-texto,i,1) = " " AND SUBSTRING(c-texto,i + 1,1) = " " THEN DO:
            c-texto = SUBSTRING(c-texto,1,i) + SUBSTRING(c-texto,i + 2). 
         END.             
     END.
END.
 
END PROCEDURE.
