/******************************************************************************
** PROGRAMA : ESCC0305RP.P                                                   **
** DATA     : JUNHO - 2010                                                   **
** AUTOR    : LIASA - LIGAS DE ALUMINIO S.A  - Eduardo Ribeiro dos Santos    **
** OBJETIVO : RELATORIO DE EMISSAO DE PEDIDOS DE COMPRAS                     **
**                                                                           **
******************************************************************************/

    {utp/ut-glob.i}
  
    DEFINE TEMP-TABLE tt-param NO-UNDO
      FIELD destino          AS INTEGER
      FIELD arquivo          AS CHAR FORMAT "x(35)"
      FIELD usuario          AS CHAR FORMAT "x(12)"
      FIELD data-exec        AS DATE
      FIELD hora-exec        AS integer
      FIELD pedido-ini       LIKE pedido-compr.num-pedido
      FIELD pedido-fim       LIKE pedido-compr.num-pedido
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
  
    DEF BUFFER b-ordem-compra FOR ordem-compra.

    FIND FIRST param-compra NO-LOCK NO-ERROR.
  
    {PDFinclude/pdf_inc.i "THIS-PROCEDURE"}
    {include/tt-edit.i}
    {include/pi-edit.i}
    {utp/utapi019.i}

    DEF NEW GLOBAL SHARED VAR v_cod_usuar_corren AS CHAR NO-UNDO. 

    DEF VAR c-arq-modelo       AS CHAR FORMAT "x(300)" NO-UNDO.
    DEF VAR c-arq-salvo        AS CHAR FORMAT "x(300)" NO-UNDO.
    DEF VAR c-arq-logo         AS CHAR FORMAT "x(300)" NO-UNDO.
    DEF VAR i-linha            AS INT                  NO-UNDO.
    DEF VAR i-linha1           AS INT                  NO-UNDO.
    DEF VAR i-qt-linha         AS INT                  NO-UNDO.
    DEF VAR i-logo             AS INT                  NO-UNDO.

    DEF VAR c-cep-est        AS CHARACTER FORMAT "x(09)" NO-UNDO. 
    DEF VAR c-CNPJ-est       AS CHAR FORMAT "x(20)"      NO-UNDO.                                                                                                       
    DEF VAR c-endereco       LIKE estabelec.endereco     NO-UNDO.
    DEF VAR c-bairro         LIKE estabelec.bairro       NO-UNDO.
    DEF VAR c-estado         LIKE estabelec.estado       NO-UNDO.
    DEF VAR c-pais           LIKE estabelec.pais         NO-UNDO.
    DEF VAR c-cidade         LIKE estabelec.cidade       NO-UNDO.
    DEF VAR c-insc           AS CHAR FORMAT "x(20)"      NO-UNDO.
    DEF VAR c-empresa        LIKE estabelec.nome         NO-UNDO.
    DEF VAR c-cd-projeto     LIKE proj-oc.cd-projeto.
    DEF VAR c-num-ped-dig    AS CHAR                     NO-UNDO.

    DEF VAR l-liber-impr       AS LOG NO-UNDO. 
    DEF VAR l-confere-aprov    AS LOG NO-UNDO.
    DEF VAR l-tem-ordem-rec    AS LOG.

    DEF VAR c-contato          LIKE cotacao-item.contato.
    DEF VAR c-cond-pagto       AS CHAR FORMAT "x(26)" NO-UNDO.
    DEF VAR c-data-pagto       AS CHAR NO-UNDO.
    DEF VAR c-narrativa        AS CHAR FORMAT "x(76)"      EXTENT 28   NO-UNDO.
    DEF VAR c-descricao        AS CHARACTER FORMAT "x(60)"             NO-UNDO.
    DEF VAR c-item-loc         AS CHAR FORMAT "x(30)"                  NO-UNDO.
    DEF VAR c-requisitante     AS CHAR FORMAT "x(28)"                  NO-UNDO.
    DEF VAR c-arq-texto        AS CHAR NO-UNDO.
    DEF VAR c-arq-condicoes    AS CHAR NO-UNDO.
    DEF VAR c-destino          AS CHAR FORMAT "x(40)" NO-UNDO.
    DEF VAR c-remetente        AS CHAR FORMAT "x(40)" NO-UNDO.
    DEF VAR c-comando          AS CHAR FORMAT "x(40)" NO-UNDO.

    DEF VAR i-pos              AS INT NO-UNDO.
    DEF VAR c-impressora       AS CHAR NO-UNDO.
    DEF VAR l-achou            AS LOGICAL INIT NO.
    DEF VAR i-copia            AS INT NO-UNDO.

    DEF VAR i-cont             AS INT NO-UNDO.
    DEF VAR i-qte-ordem        AS INT NO-UNDO.

    DEF VAR i-num-seq-ped      LIKE ps-pedcomp-dig.num-seq-ped.
    DEF VAR de-preco-conv      LIKE ordem-compra.preco-unit FORMAT ">>>>>,>>>,>>9.999"   NO-UNDO.
    DEF VAR de-preco-conv1     LIKE ordem-compra.preco-unit FORMAT ">>>>>,>>>,>>9.999"   NO-UNDO.
    DEF VAR de-preco-total     AS DECIMAL                   NO-UNDO.
    DEF VAR de-valor-descto    AS DECIMAL                              no-undo.
    DEF VAR de-preco-tot-aux   LIKE de-preco-total          NO-UNDO.
    DEF VAR de-preco-tot-aux1  LIKE de-preco-total          NO-UNDO.
    DEF VAR de-desc-total      LIKE de-preco-total.
    DEF VAR de-enc             AS DECIMAL.
    DEF VAR de-ipi             AS DECIMAL                         NO-UNDO.
    DEF VAR de-desc-total1     LIKE de-preco-total.
    DEF VAR de-enc-total       LIKE ordem-compra.preco-unit.
    DEF VAR de-preco-sipi      AS DECIMAL                         NO-UNDO.
    DEF VAR de-total-geral     AS DECIMAL  FORMAT ">>,>>>,>>9.99" NO-UNDO.
    DEF VAR de-total-sipi      AS DECIMAL  FORMAT ">>,>>>,>>9.99" NO-UNDO.
    DEF VAR i-vl-unit-sipi     LIKE ordem-compra.preco-unit.
    DEF VAR i-total-sipi       AS DECIMAL  FORMAT ">>,>>>,>>9.99"      NO-UNDO.

    DEF BUFFER b-ped FOR pedido-compr. 

DO TRANSACTION WHILE TRUE ON ENDKEY UNDO,RETURN:
   FIND NEXT b-ped WHERE
          /*   b-ped.impr-pedido  = YES         AND   */
             b-ped.num-pedido  >= tt-param.pedido-ini  AND
             b-ped.num-pedido  <= tt-param.pedido-fim  AND
             b-ped.responsavel >= tt-param.comprador-ini AND
             b-ped.responsavel <= tt-param.comprador-fim NO-LOCK NO-ERROR.
   IF NOT AVAIL b-ped THEN LEAVE.

   FIND pedido-compr WHERE
        RECID(pedido-compr) = RECID(b-ped) NO-ERROR NO-WAIT.

   IF NOT AVAILABLE pedido-compr THEN NEXT.
   IF LOCKED pedido-compr THEN NEXT.

   /****************************************
   **  COMENTAR BLOCO ABAIXO PARA TESTAR  **
   ***************************************/
   FIND FIRST param-aprov NO-LOCK NO-ERROR.
   ASSIGN l-confere-aprov = YES.
   IF NOT param-aprov.aprova-solic AND
      NOT param-aprov.aprova-pedido AND
      NOT param-aprov.aprova-emerg THEN
      ASSIGN l-confere-aprov = NO.

   IF l-confere-aprov = YES THEN DO.
      ASSIGN l-liber-impr = YES.

      FOR EACH ordem-compra WHERE
               ordem-compra.num-pedido = pedido-compr.num-pedido NO-LOCK:

          FOR EACH doc-pend-aprov WHERE
                  (doc-pend-aprov.ind-situacao = 1 OR
                   doc-pend-aprov.ind-situacao = 3) NO-LOCK.
              IF doc-pend-aprov.numero-ordem = ordem-compra.numero-ordem THEN DO.
                 ASSIGN l-liber-impr = NO.
                 LEAVE.
              END.
          END.
          IF NOT l-liber-impr THEN
             LEAVE.
      END.
   END.

   /**** libera impressao pedidos - revisao 23/05/2008 */

   IF NOT l-liber-impr THEN DO:
      RUN utp/mensagem ( INPUT "Situaá∆o do Pedido de Compra!",
                                INPUT "Pedido n∆o poder† ser Impresso ou Enviado por e-mail, Ordem de Compra " + STRING(ordem-compra.numero-ordem) + " n∆o foi aprovada.").

      NEXT.
   END.
/*                                                                                                                                                                                   */
/*    /**************************************                                                                                                                                        */
/*    **  FIM DOS COMENTµRIOS PARA TESTES  **                                                                                                                                        */
/*    **************************************/   */

   ASSIGN c-arq-modelo   = SEARCH("\\ntems\ems206\liasa\BaseTST\modelo.PDF")
          c-arq-salvo    = "\\ntems\ems206\liasa\PedCompra\PED" + STRING(pedido-compr.num-pedido, "999999") + ".pdf"
          c-arq-logo     = SEARCH("\\ntems\ems206\liasa\BaseTST\logo.jpg").

   RUN pdf_new("Spdf", c-arq-salvo).
   RUN pdf_open_pdf("Spdf",c-arq-modelo,"Modelo").
   RUN pdf_load_font IN h_PDFinc ("Spdf","TimesBD","c:\WINDOWS\Fonts\timesbd.ttf","c:\WINDOWS\Fonts\timesbd.afm",""). /* Importando Fonte */
   RUN pdf_load_font IN h_PDFinc ("Spdf","Arial","c:\WINDOWS\Fonts\arial.ttf","c:\WINDOWS\Fonts\arial.afm","").  /* Importando Fonte */


   FIND FIRST emitente WHERE
              emitente.cod-emitente = pedido-compr.cod-emitente NO-LOCK NO-ERROR.
   IF NOT AVAIL emitente THEN NEXT.
   FIND FIRST cont-emit WHERE
              cont-emit.cod-emitente =  emitente.cod-emitente
           NO-LOCK NO-ERROR.
   IF tt-param.e-mail THEN DO.
   IF NOT AVAIL emitente OR
      (AVAIL emitente AND emitente.e-mail = "") THEN DO.
      RUN utp\mensagem (INPUT " Cadastro de E-mail!",
                        INPUT " Fornecedor sem e-mail cadastrado, Imposs°vel enviar e-mail." ).
       RETURN.
   END.
   RUN utp\mens_conf ( INPUT " Envio de Email !",
                       INPUT " O Pedido " + STRING(pedido-compr.num-pedido, "999,999") + " Fornecedor " + emitente.nome-emit + " Ser† enviado para o e-mail: " + emitente.e-mail + "." + " Confirma ?").
   IF RETURN-VALUE = "NOK" THEN NEXT.
   IF c-destino = " " THEN
       ASSIGN c-destino = emitente.e-mail.
   ELSE
       ASSIGN c-destino = c-destino + ";" + emitente.e-mail.
END.



   /* --------------------  CRIAR PAGE  ------------------------------------------------ */

   RUN pi-cria-page (INPUT "Logo").
   RUN pi-imprime-linhas. /* Imprime linhas */

   /*---------------------- FIM PAGE ----------------------------------------------------*/

   /* --------------------  IMPRIME CABEÄALHO  ----------------------------------------- */

   RUN pi-imprime-cabecalho ( INPUT pedido-compr.responsavel,
                              INPUT pedido-compr.num-pedido).

   /*---------------------- FIM CABEÄALHO -----------------------------------------------*/

   /* --------------------  IMPRIME FORNECEDOR  ---------------------------------------- */

   RUN pi-imprime-fornecedor (INPUT ROWID(pedido-compr)).

   /*---------------------- FIM FORNECEDOR ----------------------------------------------*/

   /*---------------------- ENDEREÄO P COBRANÄA -----------------------------------------*/

   RUN pi-imprime-end-cobranca (INPUT ROWID(pedido-compr)).

   /*---------------------- FIM END COBRANÄA---------------------------------------------*/

   /* ------------------  CONDICAO DE PAGAMENTO  ----------------------------------------*/

   RUN pi-cond-pagto (INPUT ROWID(pedido-compr)).

   /* ----------------------- FIM CONDICAO DE PAGAMENTO  --------------------------------*/

  /* ---------------------------  IMPRIME MENSAGEM/OBS/ASSNATURA ------------------------*/

   RUN pi-dados-obs (INPUT ROWID(pedido-compr)).

  /* ---------------------------  FIM IMPRIME MENSAGEM/OBS/ASSNATURA ------------------- */


  /*------------------    VERIFICAR ENVIO DE EMAIL ANTES DE LIBERAR O PROG ---*/

  /* ---------------------------  ORDEM DE COMPRA  --------------------------------------*/

   RUN pi-imprime-ordem (INPUT ROWID(pedido-compr)).

  /*---------------------- FIM ORDEM DE COMPRA ------------------------------------------*/


   RUN pdf_close("Spdf"). /* Fechar Arquivo pdf */

   RUN pi-formata-mensagem.

   IF SEARCH (c-arq-salvo) <> ? THEN DO.
      DO i-copia = 1 TO tt-param.num-copia.
        IF tt-param.destino = 3 THEN
           OS-COMMAND NO-WAIT VALUE(c-arq-salvo).
        ELSE IF tt-param.destino = 1 THEN DO.
           ASSIGN i-pos = 1.
           DO WHILE l-achou = NO.
              IF SUBSTRING(tt-param.arquivo,1,i-pos) MATCHES "*:*" THEN
                 ASSIGN l-achou = YES.
              ELSE
                 ASSIGN c-impressora = SUBSTRING(tt-param.arquivo,1,i-pos).
        
              ASSIGN i-pos = i-pos + 1.
           END.
        
           FIND FIRST imprsor_usuar WHERE
                      imprsor_usuar.cod_usuario     = v_cod_usuar_corren AND
                      imprsor_usuar.nom_impressora  = c-impressora NO-LOCK NO-ERROR.
           IF AVAIL imprsor_usuar THEN DO.
              RUN WinExec ("C:\Program Files (x86)\Adobe\Reader 9.0\Reader\AcroRd32.exe", INPUT 2). 
              RUN WinExec ("C:\Program Files (x86)\Adobe\Reader 9.0\Reader\AcroRd32.exe" + " " + "/n /t /h" + " " + c-arq-salvo + " " + imprsor_usuar.nom_disposit_so, INPUT 2). 
              /*RUN WinExec ("taskkill /im AcroRd32.exe /f", INPUT 2). */
           END.
           ELSE 
              RUN utp\mensagem ( INPUT "Impressora N∆o Encontrada",
                                  INPUT "A impressora N∆o foi Encontrada, Favor Entrar em Contato com o CPD ( Local )").
        END.
      END.
   END.
   ELSE
       RUN utp\mensagem ( INPUT "Arquivo Nío Encontrado!",
                          INPUT "Nío Foi Encontrado o Arquivo " + c-arq-salvo).

END.

PROCEDURE pi-formata-cpfCNPJ.
    DEF INPUT PARAMETER c-num AS CHARACTER FORMAT "x(14)" NO-UNDO.
    DEF OUTPUT PARAMETER c-cpfCNPJ AS CHARACTER FORMAT "x(18)" NO-UNDO.

    IF LENGTH(TRIM(c-num)) = 11 THEN
        ASSIGN c-cpfCNPJ = STRING(c-num, "999.999.999-99").
    ELSE
        IF LENGTH(TRIM(c-num)) = 14 THEN
            ASSIGN c-cpfCNPJ = STRING(c-num, "99.999.999/9999-99").
        ELSE
            ASSIGN c-cpfCNPJ = "".
END PROCEDURE.


PROCEDURE pi-dados-estabelec.
    DEF INPUT PARAMETER c-estab LIKE estabelec.cod-estabel.

    ASSIGN c-endereco = ""
           c-bairro   = ""
           c-cep-est  = ""
           c-cidade   = ""
           c-estado   = ""
           c-insc     = "".

    FIND FIRST estabelec WHERE
               estabelec.cod-estabel = c-estab NO-LOCK NO-ERROR.
    IF AVAIL estabelec THEN DO.
        RUN pi-formata-cpfCNPJ(INPUT estabelec.cgc, OUTPUT c-CNPJ-est).
        ASSIGN c-endereco = estabelec.endereco
               c-bairro   = estabelec.bairro
               c-cep-est  = SUBSTR(STRING(estabelec.cep, "99999999"), 1, 5) + "-" +
                            SUBSTR(STRING(estabelec.cep, "99999999"), 6, 3)
               c-cidade   = estabelec.cidade
               c-estado   = estabelec.estado
               c-insc     = estabelec.ins-estadual
               c-empresa  = estabelec.nome.
    END.
END PROCEDURE.

PROCEDURE pi-imprime-cabecalho.
    DEF INPUT PARAMETER p-resp          LIKE pedido-compr.responsavel.
    DEF INPUT PARAMETER p-pedido        LIKE pedido-compr.num-pedido.
    DEF VAR c-email-compr               AS CHAR FORMAT "x(30)" NO-UNDO.
    DEF VAR c-estab                     LIKE pedido-compr.end-cobranca.

    FIND FIRST usuar_mestre WHERE
               usuar_mestre.cod_usuario = p-resp NO-LOCK NO-ERROR.
    IF AVAIL usuar_mestre THEN
        ASSIGN c-email-compr = usuar_mestre.cod_e_mail_local.

    RUN pdf_set_font IN h_PDFinc ("Spdf", "Courier-bold", 11).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "PEDIDO DE COMPRA" , 100 , 780 ).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "LIGAS DE ALUM÷NIO S/A - LIASA" , 70 , 768 ).
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Arial", 8).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "ADMINISTRAÄ«O:" , 70 , 756 ).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "UNIDADE FABRIL:" , 175 , 756 ).
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Arial", 6).
    RUN pi-dados-estabelec (INPUT "bh").
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", c-endereco , 70 , 745 ).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Bairro " + c-bairro , 70 , 737 ).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", c-cep-est + " - " + c-cidade + " - " + c-estado , 70 , 729 ).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Fone: " + "(031) 3249-2000" , 70 , 721 ).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Fax: " + "(031) 3249-2050" , 70 , 713 ).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "CNPJ: " + c-CNPJ-est , 70 , 705 ).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Insc.Est: " + c-insc , 70 , 697 ).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "e-mail: " + c-email-compr, 70, 689).
    RUN pi-dados-estabelec (INPUT "pi").
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", c-endereco , 175 , 745 ).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Bairro " + c-bairro , 175 , 737 ).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", c-cep-est + " - " + c-cidade + " - " + c-estado , 175 , 729 ).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Fone: " + "(031) 3249-2000" , 175 , 721 ).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Fone: " + "(031) 3249-2050" , 175 , 713 ).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "CNPJ: " + c-CNPJ-est , 175 , 705 ).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Insc.Est: " + c-insc , 175 , 697 ).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "e-mail: " + c-email-compr, 175, 689).
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Courier-bold", 8).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "ENDEREÄO PARA FATURAMENTO E ENTREGA" , 310 , 780 ).
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Arial", 6).
    FIND FIRST pedido-compr WHERE
               pedido-compr.num-pedido = p-pedido NO-LOCK NO-ERROR.
    RUN pi-dados-estabelec (INPUT pedido-compr.end-entrega).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Raz∆o Social ", 298 , 766 ).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf",  c-empresa , 347 , 766 ).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "CNPJ " , 298 , 756 ).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf",  c-CNPJ-est , 347 , 756 ).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Insc.Est " , 298 , 746 ).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", c-insc , 347 , 746 ).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Endereáo ", 298 , 736 ).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", c-endereco , 347 , 736 ).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", c-bairro , 347 , 726 ).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", c-cidade , 347 , 716 ).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", c-estado + " " + c-cep-est , 347 , 706 ).
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Courier-bold", 8).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "PEDIDO DE" , 538 , 780 ).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "COMPRA" , 541 , 770 ).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", STRING(p-pedido, "999,999") , 546 , 757 ).
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Arial", 6).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Mencionar o n£mero deste" , 524 , 736 ).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "pedido nas Notas Fiscais" , 524 , 726 ).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "e demais documentos" , 528 , 716 ).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Vide condiá‰es gerais de" , 524 , 706 ).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "fornecimento abaixo" , 528 , 696 ).

    RUN pdf_line ("Spdf", 290,690,290,790,1).
    RUN pdf_line ("Spdf", 290,790,515,790,1).
    RUN pdf_line ("Spdf", 290,775,515,775,1).
    RUN pdf_line ("Spdf", 515,690,515,790,1).
    RUN pdf_line ("Spdf", 520,690,520,790,1).
    RUN pdf_line ("Spdf", 520,790,600,790,1).
    RUN pdf_line ("Spdf", 520,750,600,750,1).
    RUN pdf_line ("Spdf", 600,690,600,790,1).
    RUN pdf_line ("Spdf", 520,690,600,690,1).
    RUN pdf_line ("Spdf", 290,690,515,690,1).

END PROCEDURE.

PROCEDURE pi-dados-item.
DEF INPUT PARAMETER rw-ordem-compra AS ROWID NO-UNDO.

 FIND FIRST ordem-compra WHERE
      ROWID(ordem-compra) = rw-ordem-compra NO-LOCK.
 IF AVAIL ordem-compra THEN DO.


    IF ordem-compra.mo-codigo > 0 THEN DO:
       RUN cdp/cd0812.p (INPUT ordem-compra.mo-codigo,
                         INPUT 0,
                         INPUT ordem-compra.pre-unit-for,
                         INPUT pedido-compr.data-pedido,
                         OUTPUT de-preco-conv).
       IF de-preco-conv = ? THEN
          ASSIGN de-preco-conv = ordem-compra.pre-unit-for.
    END.
    ELSE
       ASSIGN de-preco-conv = ordem-compra.pre-unit-for.

    IF ordem-compra.mo-codigo > 0 THEN DO:
       RUN cdp/cd0812.p (INPUT ordem-compra.mo-codigo,
                         INPUT 0,
                         INPUT ordem-compra.preco-fornec,
                         INPUT pedido-compr.data-pedido,
                         OUTPUT de-preco-conv1).
       IF de-preco-conv1 = ? THEN
          ASSIGN de-preco-conv1 = ordem-compra.preco-fornec.
    END.
    ELSE
       ASSIGN de-preco-conv1 = ordem-compra.preco-fornec.

       /* Desconto */
   IF ordem-compra.mo-codigo > 0 THEN DO:
       RUN cdp/cd0812.p (INPUT ordem-compra.mo-codigo,
                         INPUT 0,
                         INPUT ordem-compra.valor-descto,
                         INPUT pedido-compr.data-pedido,
                         OUTPUT de-valor-descto).
       IF de-valor-descto = ? THEN
          ASSIGN de-valor-descto = ordem-compra.valor-descto.
   END.
   ELSE 
      ASSIGN de-valor-descto = ordem-compra.valor-descto.

   ASSIGN de-preco-conv1 = de-preco-conv1 - de-valor-descto.

   /* ---------------------------  PRAZO DE COMPRA  ---------------------------- */

    FOR EACH prazo-compra WHERE
             prazo-compra.numero-ordem  = ordem-compra.numero-ordem AND
             prazo-compra.situacao     <> 4                         AND
             prazo-compra.situacao     <> 6 NO-LOCK
             BREAK BY prazo-compra.numero-ordem
                   BY prazo-compra.parcela:

        ASSIGN i-logo = i-logo + 1.

        IF i-qte-ordem <= 24 THEN DO.
           RUN pdf_text_xy  IN h_PDFinc ("Spdf", STRING(prazo-compra.numero-ordem, "ZZZZZZ,99") , 20, i-linha ).
           RUN pdf_set_font IN h_PDFinc ("Spdf", "Arial", 6).
        END.
        ELSE DO.
            ASSIGN i-linha  = 558.  
            RUN pi-cria-page (INPUT STRING(i-logo)).
            RUN pdf_text_xy  IN h_PDFinc ("Spdf", STRING(prazo-compra.numero-ordem, "ZZZZZZ,99") , 20, i-linha ).
            RUN pdf_set_font IN h_PDFinc ("Spdf", "Arial", 6).
            RUN pi-imprime-linhas.
            RUN pi-imprime-cabecalho ( INPUT pedido-compr.responsavel,
                                       INPUT pedido-compr.num-pedido).
            RUN pi-imprime-fornecedor (INPUT ROWID(pedido-compr)).
            RUN pi-imprime-end-cobranca (INPUT ROWID(pedido-compr)).
            RUN pi-cond-pagto (INPUT ROWID(pedido-compr)).
            RUN pi-dados-obs (INPUT ROWID(pedido-compr)).
            ASSIGN i-qte-ordem = 0.
        END.

        ASSIGN i-qte-ordem = i-qte-ordem + 1.

        FIND FIRST ordem-compra OF prazo-compra NO-LOCK NO-ERROR.

        IF ordem-compra.num-pedido = 101395 OR
           ordem-compra.num-pedido = 102597 OR
           ordem-compra.num-pedido = 102906 OR
           ordem-compra.num-pedido = 103657 OR
           ordem-compra.num-pedido = 106650 OR
           ordem-compra.num-pedido = 108268 OR
           ordem-compra.num-pedido = 109253 THEN
           ASSIGN de-preco-tot-aux = de-preco-conv *  prazo-compra.quant-receb
                  de-preco-tot-aux1 = de-preco-conv1 * prazo-compra.quant-receb.
        ELSE
           ASSIGN de-preco-tot-aux = de-preco-conv *  prazo-compra.qtd-sal-forn
                  de-preco-tot-aux1 = de-preco-conv1 * prazo-compra.qtd-sal-forn.

        ASSIGN de-desc-total = 0
               de-enc        = 0
               de-ipi        = 0.

        IF param-compra.log-1 = NO THEN DO:

           /* ------ CALCULO I.P.I. SOBRE PRECO LIQUIDO ------ */

           IF ordem-compra.perc-descto > 0 THEN
              ASSIGN de-desc-total = de-preco-tot-aux * ordem-compra.perc-descto / 100
                     de-desc-total1 = de-preco-tot-aux1 * ordem-compra.perc-descto / 100.

           IF ordem-compra.taxa-financ = NO THEN DO:
              ASSIGN de-enc = de-preco-tot-aux - de-desc-total.

              RUN ccp/cc9020.p (INPUT YES,
                                INPUT ordem-compra.cod-cond-pag,
                                INPUT ordem-compra.valor-taxa,
                                input  ordem-compra.nr-dias-taxa,
                                INPUT de-enc,
                                OUTPUT de-enc-total).
              ASSIGN de-enc = round(de-enc-total - de-enc,1).
           END.
           ELSE
              ASSIGN de-enc-total = de-preco-tot-aux - de-desc-total.

           IF ordem-compra.aliquota-ipi > 0 THEN
              ASSIGN de-ipi = de-enc-total * ordem-compra.aliquota-ipi / 100.
        END.
        ELSE DO:
             /* ------ CALCULO I.P.I. SOBRE PRECO BRUTO ------ */

             IF ordem-compra.taxa-financ = NO THEN DO:
                RUN ccp/cc9020.p (INPUT YES,
                                  INPUT ordem-compra.cod-cond-pag,
                                  INPUT ordem-compra.valor-taxa,
                                  input  ordem-compra.nr-dias-taxa,
                                  INPUT de-preco-tot-aux,
                                  OUTPUT de-enc-total).

                ASSIGN de-enc = ROUND(de-enc-total - de-preco-tot-aux,1).
             END.
             ELSE
                ASSIGN de-enc-total = de-preco-tot-aux.

             IF ordem-compra.aliquota-ipi > 0 THEN
                ASSIGN de-ipi = de-enc-total * ordem-compra.aliquota-ipi / 100.

             IF ordem-compra.perc-descto > 0 THEN
                ASSIGN de-desc-total = (de-enc-total + de-ipi)
                                       * ordem-compra.perc-descto / 100.
        END.
        ASSIGN de-preco-total = de-preco-tot-aux + de-enc
               de-preco-sipi  = de-preco-tot-aux1 + de-enc
               de-total-sipi  = de-total-sipi  + de-preco-sipi
               de-total-geral = ROUND(de-total-geral + de-preco-total,2).

        IF ordem-compra.num-pedido = 101395 OR
           ordem-compra.num-pedido = 102597 OR
           ordem-compra.num-pedido = 102906 OR
           ordem-compra.num-pedido = 103657 OR
           ordem-compra.num-pedido = 106650 OR
           ordem-compra.num-pedido = 108268 OR
           ordem-compra.num-pedido = 109253 THEN
            ASSIGN i-vl-unit-sipi = de-preco-conv / ((100 + ordem-compra.aliquota-ipi) / 100)
                   i-total-sipi = i-total-sipi + (i-vl-unit-sipi * prazo-compra.quant-receb).
        ELSE
           ASSIGN i-vl-unit-sipi = de-preco-conv / ((100 + ordem-compra.aliquota-ipi) / 100)
                  i-total-sipi = i-total-sipi + (i-vl-unit-sipi * prazo-compra.qtd-sal-forn).


        RUN pdf_text_xy  IN h_PDFinc ("Spdf", CAPS(prazo-compra.un) , 357 , i-linha).
        RUN pdf_text_xy  IN h_PDFinc ("Spdf", STRING(prazo-compra.qtd-sal-forn, "ZZZ,ZZ9.99") , 368 , i-linha ).
        RUN pdf_text_xy  IN h_PDFinc ("Spdf", STRING(ordem-compra.aliquota-ipi, "ZZ9,99") , 513 , i-linha ).
        RUN pdf_text_xy  IN h_PDFinc ("Spdf", STRING(prazo-compra.data-entrega, "99/99/9999"), 548, i-linha).

        IF ordem-compra.num-pedido = 101395 OR
           ordem-compra.num-pedido = 102597 OR
           ordem-compra.num-pedido = 102906 OR
           ordem-compra.num-pedido = 103657 OR
           ordem-compra.num-pedido = 106650 OR
           ordem-compra.num-pedido = 108268 OR
           ordem-compra.num-pedido = 109253 THEN DO.
             RUN pdf_text_xy  IN h_PDFinc ("Spdf", STRING(i-vl-unit-sipi, "ZZ9,99") , 395 , i-linha).
             RUN pdf_text_xy  IN h_PDFinc ("Spdf", STRING(i-vl-unit-sipi * prazo-compra.quant-receb), 440, i-linha).
        END.
        ELSE DO.
           RUN pdf_text_xy  IN h_PDFinc ("Spdf", STRING(i-vl-unit-sipi, ">>>>>,>>>,>>9.999") , 395 , i-linha).
           RUN pdf_text_xy  IN h_PDFinc ("Spdf", STRING(i-vl-unit-sipi * prazo-compra.qtd-sal-forn, ">>>>>,>>>,>>9.999"), 440, i-linha).
        END.
        FIND FIRST cotacao-item WHERE
                   cotacao-item.numero-ordem = prazo-compra.numero-ordem AND
                   cotacao-item.it-codigo = prazo-compra.it-codigo NO-LOCK NO-ERROR.

        FIND item-fornec WHERE
             item-fornec.it-codigo    = prazo-compra.it-codigo AND
             item-fornec.cod-emitente = pedido-compr.cod-emitente NO-LOCK NO-ERROR.

        FIND ITEM WHERE
             ITEM.it-codigo = prazo-compra.it-codigo NO-LOCK NO-ERROR.

        FIND narrativa WHERE
             narrativa.it-codigo = prazo-compra.it-codigo NO-LOCK NO-ERROR.

        ASSIGN c-descricao = ""
               c-narrativa = ""
               i-cont      = 0
               i-qt-linha  = 0.

        IF ordem-compra.narrativa <> "" THEN DO.
           RUN pdf_text_xy  IN h_PDFinc ("Spdf", CAPS(ITEM.desc-item) , 70, i-linha ).
           ASSIGN i-qte-ordem = i-qte-ordem + 1.
           RUN pi-print-editor (TRIM(ordem-compra.narrativa), 76).
           FOR EACH tt-editor NO-LOCK.
               ASSIGN i-cont = i-cont + 1.
               IF i-cont > 28 THEN LEAVE.
               IF tt-editor.conteudo <> " " THEN DO.
                   ASSIGN  i-linha  = i-linha - 8
                           i-qt-linha = i-qt-linha + 8
                           i-qte-ordem  = i-qte-ordem + 1.
                   RUN pdf_text_xy  IN h_PDFinc ("Spdf", TRIM(tt-editor.conteudo) , 70, i-linha ).
               END.
           END.
        END.
        ELSE DO.
           IF AVAIL item-fornec AND item-fornec.narrativa <> "" THEN DO.
              RUN pdf_text_xy  IN h_PDFinc ("Spdf", TRIM(ITEM.desc-item) , 70, i-linha ).
              ASSIGN i-qte-ordem = i-qte-ordem + 1.
              RUN pi-print-editor (TRIM(item-fornec.narrativa), 76).
              FOR EACH tt-editor NO-LOCK.
                  ASSIGN i-cont   = i-cont + 1
                         i-linha  = i-linha - 8
                         i-qt-linha = i-qt-linha + 8
                         i-qte-ordem  = i-qte-ordem + 1.
                  IF i-cont > 28 THEN LEAVE.
                  RUN pdf_text_xy  IN h_PDFinc ("Spdf", TRIM(tt-editor.conteudo) , 70, i-linha ).
              END.
           END.
           ELSE DO.
              IF AVAIL narrativa AND narrativa.descricao <> "" THEN DO.
                 RUN pdf_text_xy  IN h_PDFinc ("Spdf", TRIM(ITEM.desc-item) , 70, i-linha ).
                 ASSIGN i-qte-ordem = i-qte-ordem + 1.
                 RUN pi-print-editor (TRIM(narrativa.descricao), 76).
                 FOR EACH tt-editor NO-LOCK.
                     ASSIGN i-cont   = i-cont + 1
                            i-linha  = i-linha - 8
                            i-qt-linha = i-qt-linha + 8
                            i-qte-ordem  = i-qte-ordem + 1.
                     IF i-cont > 28 THEN LEAVE.
                     RUN pdf_text_xy  IN h_PDFinc ("Spdf", TRIM(tt-editor.conteudo) , 70, i-linha ).
                 END.
              END.
              ELSE DO.
                  ASSIGN i-qte-ordem  = i-qte-ordem + 1.
                  RUN pdf_text_xy  IN h_PDFinc ("Spdf", CAPS(TRIM(ITEM.desc-item)) , 70, i-linha ).
              END.
           END.
        END.
        IF ITEM.cod-localiz <> "" THEN
           ASSIGN c-item-loc = TRIM(ordem-compra.it-codigo) + " - " +
                  ITEM.cod-localiz.
        ELSE
           ASSIGN c-item-loc = ordem-compra.it-codigo.

        FIND it-requisicao WHERE
             it-requisicao.numero-ordem = ordem-compra.numero-ordem
             NO-LOCK NO-ERROR.

        IF AVAIL it-requisicao THEN DO.
           FIND requisicao WHERE
                requisicao.nr-requisicao = it-requisicao.nr-requisicao
                NO-LOCK NO-ERROR.
           ASSIGN c-requisitante = "REQ.: " + requisicao.nome-abrev.
        END.
           ASSIGN c-requisitante = "".

        IF c-requisitante = "" AND ordem-compra.origem = 5 THEN
           ASSIGN c-requisitante = "REQ.: " + ordem-compra.requisitante.
        ASSIGN c-cd-projeto = "".
        FIND proj-oc WHERE
             proj-oc.numero-ordem = ordem-compra.numero-ordem
             NO-LOCK NO-ERROR.
        IF AVAIL proj-oc and proj-oc.cd-projeto <> "" THEN
           ASSIGN c-cd-projeto = proj-oc.cd-projeto.
        ELSE DO.
           FOR EACH proj-pc WHERE
                    proj-pc.num-pedido = ordem-compra.num-pedido NO-LOCK.
               IF proj-pc.cd-projeto <> "" then
                  ASSIGN c-cd-projeto = proj-pc.cd-projeto.
           END.
        END.

        IF c-cd-projeto = "" THEN DO.
           FIND proj-req WHERE
                proj-req.nr-requisicao = ordem-compra.nr-requisicao
                NO-LOCK NO-ERROR.
           IF AVAIL proj-req AND proj-req.cd-projeto <> "" THEN
              ASSIGN c-cd-projeto = proj-req.cd-projeto.
        END.

        IF c-cd-projeto = "" THEN DO.
            ASSIGN i-qte-ordem  = i-qte-ordem + 1.
            RUN pdf_text_xy  IN h_PDFinc ("Spdf", "ITEM/LOC.: " + c-item-loc + " " + CAPS(c-requisitante) , 70 , i-linha - 8 ).
        END.
        ELSE DO.
            ASSIGN i-qte-ordem  = i-qte-ordem + 1.
            RUN pdf_text_xy  IN h_PDFinc ("Spdf", "ITEM/LOC.: " + c-item-loc + " " + CAPS(c-requisitante) + "PROJETO.:" + c-cd-projeto , 70 , i-linha - 8 ).
        END.

        ASSIGN i-linha    = i-linha - 15.
    END.
 END.
END PROCEDURE.

PROCEDURE pi-dados-obs.
    DEF INPUT PARAMETER rw-pedido-compr AS ROWID.

    FIND FIRST pedido-compr WHERE
         ROWID(pedido-compr)  = rw-pedido-compr NO-LOCK NO-ERROR.

    FIND FIRST ordem-compra WHERE
               ordem-compra.num-pedido = pedido-compr.num-pedido NO-LOCK NO-ERROR.
    IF NOT AVAIL ordem-compr THEN NEXT.

    RUN pdf_set_font IN h_PDFinc ("Spdf", "Courier-bold", 8).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "OBS:", 20 , 198).
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Courier-bold", 5.8).
    FIND mensagem WHERE
        mensagem.cod-mensagem = pedido-compr.cod-mensagem NO-LOCK NO-ERROR.
    IF AVAIL mensagem THEN
        RUN pdf_text_xy  IN h_PDFinc ("Spdf", SUBSTRING((mensagem.texto-mensag),1,160), 42, 198).
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Courier-bold", 8).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "CONDIÄÂES GERAIS DE FORNECIMENTO", 225, 179).
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Arial", 6).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "1. O(s) pagamento(s) ser†(∆o) realizados de acordo com o prazo estabelecido na negociaá∆o e retratados neste pedido, sendo vetada a emiss∆o de boleto(s) banc†rio(s) contra a LIASA.", 20, 171).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "2. Dever† ser indicado na NF ou em documento h†bil (papel timbrado ou com carimbo de CNPJ) os dados banc†rios da empresa para que seja efetuado dep¢sito em conta corrente. O documento deve ser ", 20, 164).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "enviado no endereáo de cobranáa supra citado, com antecedància m°nima de 10 dias do vencimento. Os respectivos comprovantes de dep¢sitos, TED ou DOC, ser∆o v†lidos como recibo de quitaá∆o. ", 26, 157).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "3. Quaisquer eventuais alteraá‰es neste Pedido de Compra, n∆o ser∆o v†lidos sem prÇvia autorizaá∆o da LIASA", 20, 150).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "4. As mercadorias est∆o sujeitas a conferància no recebimento, levando em consideraá∆o controle de qualidade. Materiais em divergància com a especificaá∆o do PC ser∆o devolvidos sem ìnus para a LIASA.", 20, 143).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "5. O n∆o cumprimento das condiá‰es comerciais (prazo de entrega, pagamentos, etc...) estabelecidas no Pedido de Compra, poder† impactar no IQF (÷ndice de Qualificaá∆o de Fornecedor), podendo ocorrer", 20, 136).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Ö suspens∆o do cadastro de fornecedores da LIASA.", 26, 129).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "6. Favor confirmar o recebimento deste Pedido de Compra. Quaisquer divergància em relaá∆o a proposta apresentada dever ser enviada no prazo m†ximo de 2 (dois) dias, ap¢s este prazo considere-se ", 20, 122).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "que o Pedido de Compra foi aceito sem restriá‰es.", 26, 115).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "7. A(s) NF(s) de Prestaá∆o de Serviáos (que haja retená∆o do INSS) devem ser entregues na LIASA (Pirapora-MG) entre o 1ß atÇ 20ßdia do màs em curso, cuja data de emiss∆o tambÇm esteja dentro deste", 20, 108).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "per°odo. A(s) NF(s) de Prestaá∆o de Serviáos recebidas ap¢s per°odo acima mencionado, ser∆o devolvidas ao Prestador de Serviáo para refaturamento, sendo nova data 1ß do màs subsequente.", 26, 101).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "8. Para NF(s) de Prestaá∆o de Serviáos (que n∆o haja retená∆o do INSS), podem ser entregues na LIASA atÇ 25ß dia do màs em curso, mantendo demais condiá‰es do item 7.", 20, 94).

/* -----------------------  OBSERVACOES  ------------------------- */
    FIND transporte WHERE
        transporte.cod-transp = pedido-compr.cod-transp NO-LOCK NO-ERROR.
    IF AVAIL transporte THEN DO.
      RUN pdf_set_font IN h_PDFinc ("Spdf", "Arial", 6).
      RUN pdf_text_xy  IN h_PDFinc ("Spdf", "LOCAL DE ENTREGA: " + transporte.nome, 20, 76).
      RUN pdf_text_xy  IN h_PDFinc ("Spdf",  transporte.endereco, 85, 68).
      RUN pdf_text_xy  IN h_PDFinc ("Spdf",  transporte.bairro + " - " + transporte.cidade, 85, 60).
      RUN pdf_text_xy  IN h_PDFinc ("Spdf",  transporte.estado + " - " + STRING(transporte.cep) + " Fone: " + transporte.telefone, 85, 52).
    END.
    ELSE
       RUN pdf_text_xy  IN h_PDFinc ("Spdf", "LOCAL DE ENTREGA: Ao Portador ", 20, 76).

    RUN pdf_text_xy  IN h_PDFinc ("Spdf",  "Comprador: " + ordem-compra.cod-comprado, 85, 45).

    FIND comprador WHERE
        comprador.cod-comprado = ordem-compra.cod-comprado NO-LOCK NO-ERROR.
    IF AVAIL comprador THEN
        RUN pdf_text_xy  IN h_PDFinc ("Spdf",  CAPS(comprador.nome) + " - " + CAPS(comprador.e-mail), 160, 45).
    IF ordem-compra.cod-comprado = "EXP" THEN
        RUN pdf_text_xy  IN h_PDFinc ("Spdf", "COMPRAS5@LIASA.COM.BR,COMPRAS7@LIASA.COM.BR", 20, 37).

    IF AVAIL comprador THEN DO.
       IF c-destino = " " THEN
          ASSIGN c-destino = comprador.e-mail.
       ELSE
          ASSIGN c-destino = c-destino + ";" + comprador.e-mail.
    END.
    ELSE DO.
       IF c-destino = " " THEN
          ASSIGN c-destino = "compras5@liasa.com.br".
       ELSE
          ASSIGN c-destino = c-destino + ";" + "compras5@liasa.com.br".
    END.

    IF ordem-compra.cod-comprado = "EXP" THEN
       IF c-destino = " " THEN
           ASSIGN c-destino = "compras5@liasa.com.br,compras7@liasa.com.br".
       ELSE
           ASSIGN c-destino = c-destino + ";" + "compras5@liasa.com.br,compras7@liasa.com.br".

    IF pedido-compr.frete = 1 THEN DO.
      RUN pdf_text_xy  IN h_PDFinc ("Spdf", "FRETE: ", 20, 31).
      RUN pdf_text_xy  IN h_PDFinc ("Spdf", "PAGO", 85, 31).
    END.
    ELSE DO.
      RUN pdf_text_xy  IN h_PDFinc ("Spdf", "FRETE: ", 20, 31).
      RUN pdf_text_xy  IN h_PDFinc ("Spdf", "∑ PAGAR", 85, 31).
    END.

    /* 1 - Consumo     2 - Industrializaá∆o  */
    IF ordem-compra.codigo-icm = 1 THEN DO.
        RUN pdf_text_xy  IN h_PDFinc ("Spdf", "DESTINO: ", 20, 23).
        RUN pdf_text_xy  IN h_PDFinc ("Spdf", "CONSUMO", 85, 23).
    END.
    ELSE DO.
        RUN pdf_text_xy  IN h_PDFinc ("Spdf", "DESTINO: ", 20, 23).
        RUN pdf_text_xy  IN h_PDFinc ("Spdf", "INDUSTRIALIZAÄ«O", 85, 23).
    END.

    FIND LAST ps-pedcomp-dig NO-LOCK USE-INDEX seq-pedido.
    ASSIGN i-num-seq-ped = ps-pedcomp-dig.num-seq-ped + 1.

    FIND FIRST ps-pedcomp-dig WHERE
               ps-pedcomp-dig.num-pedido = pedido-compr.num-pedido
               NO-LOCK NO-ERROR.

    IF NOT AVAIL ps-pedcomp-dig THEN DO.
       CREATE ps-pedcomp-dig.
       ASSIGN ps-pedcomp-dig.num-pedido  = pedido-compr.num-pedido
              ps-pedcomp-dig.usuario = tt-param.usuario
              ps-pedcomp-dig.num-seq-ped = i-num-seq-ped
              ps-pedcomp-dig.dt-emissao  = TODAY
              ps-pedcomp-dig.hr-emissao  = SUBSTRING(STRING(TIME,"hh:mm:ss"),1,2)  +
                                           SUBSTRING(STRING(TIME,"hh:mm:ss"),4,2)  +
                                           SUBSTRING(STRING(TIME,"hh:mm:ss"),7,2)
              ps-pedcomp-dig.num-ped-dig = STRING(pedido-compr.num-pedido) +
                                           STRING(i-num-seq-ped) +
                                           STRING(DAY(pedido-compr.data-pedido),"99") +
                                           STRING(MONTH(pedido-compr.data-pedido),"99") +
                                           STRING(YEAR(pedido-compr.data-pedido),"9999") +
                                           SUBSTRING(STRING(TIME,"hh:mm:ss"),1,2) +
                                           SUBSTRING(STRING(TIME,"hh:mm:ss"),4,2) +
                                           SUBSTRING(STRING(TIME,"hh:mm:ss"),7,2).
    END.

    ASSIGN pedido-compr.situacao = 1.

    ASSIGN c-num-ped-dig = ps-pedcomp-dig.num-ped-dig.

    RUN include/CODE.p (INPUT-OUTPUT c-num-ped-dig ).
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Courier-bold", 8).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", c-num-ped-dig, 443, 40).
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Arial", 6).

  /* ----------------------- FIM OBSERVACOES  ------------------------- */
END PROCEDURE.

PROCEDURE pi-imprime-fornecedor.

    DEF INPUT PARAMETER rw-pedido-compr AS ROWID.

    FIND FIRST pedido-compr WHERE
         ROWID(pedido-compr)  = rw-pedido-compr NO-LOCK NO-ERROR.

    FIND emitente WHERE
         emitente.cod-emitente = pedido.cod-emitente NO-LOCK NO-ERROR.

    IF AVAIL emitente THEN DO.
       RUN pdf_set_font IN h_PDFinc ("Spdf", "Courier-bold", 11).
       RUN pdf_text_xy  IN h_PDFinc ("Spdf", "FORNECEDOR" , 140 , 670 ).
       RUN pdf_set_font IN h_PDFinc ("Spdf", "Arial", 8).
       RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Raz∆o Social" , 18 , 655 ).
       RUN pdf_text_xy  IN h_PDFinc ("Spdf", CAPS(emitente.nome-emit) , 85 , 655 ).
       RUN pdf_text_xy  IN h_PDFinc ("Spdf", "CNPJ" , 18 , 645 ).
       RUN pi-formata-cpfCNPJ(INPUT emitente.cgc, OUTPUT c-CNPJ-est).
       RUN pdf_text_xy  IN h_PDFinc ("Spdf", c-cnpj-est , 85 , 645 ).
       RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Insc.Estadual" , 180 , 645 ).
       RUN pdf_text_xy  IN h_PDFinc ("Spdf", emitente.ins-estadual , 240 , 645 ).
       RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Endereáo" , 18 , 635 ).
       RUN pdf_text_xy  IN h_PDFinc ("Spdf", emitente.endereco , 85 , 635 ).
       RUN pdf_text_xy  IN h_PDFinc ("Spdf", emitente.bairro , 85 , 625 ).
       RUN pdf_text_xy  IN h_PDFinc ("Spdf", CAPS(emitente.estado) + "  " + emitente.cep , 210 , 625 ).
       RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Telefone" , 18 , 615 ).
       RUN pdf_text_xy  IN h_PDFinc ("Spdf", emitente.telefone[1] , 85 , 615 ).
       RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Fax  " + emitente.telefax, 210 , 615 ).

       FIND FIRST ordem-compra WHERE
                  ordem-compra.num-pedido = pedido-compr.num-pedido AND
                  ordem-compra.situacao <> 4 NO-LOCK NO-ERROR.
       FIND FIRST cotacao-item WHERE
                  cotacao-item.numero-ordem = ordem-compra.numero-ordem AND
                  cotacao-item.it-codigo    = ordem-compra.it-codigo AND
                  cotacao-item.cot-aprovada NO-LOCK NO-ERROR.

       IF AVAIL cotacao-item THEN
          ASSIGN c-contato = cotacao-item.contato.
       ELSE
          ASSIGN c-contato = " ".

       RUN pdf_text_xy  IN h_PDFinc ("Spdf", "Contato  ", 18 , 605 ).
       RUN pdf_text_xy  IN h_PDFinc ("Spdf", CAPS(c-contato), 85 , 605 ).
       RUN pdf_text_xy  IN h_PDFinc ("Spdf", "E-mail  " + emitente.e-mail, 210 , 605 ).
    END.
END PROCEDURE.

PROCEDURE pi-imprime-end-cobranca.

    DEF INPUT PARAMETER rw-pedido-compr AS ROWID.

    FIND FIRST pedido-compr WHERE
         ROWID(pedido-compr)  = rw-pedido-compr NO-LOCK NO-ERROR.

    FIND estabelec WHERE
         estabelec.cod-estabel = pedido-compr.end-cobranca NO-LOCK NO-ERROR.
    IF AVAIL estabelec THEN DO.
        RUN pdf_set_font IN h_PDFinc ("Spdf", "Courier-bold", 11).
        RUN pdf_text_xy  IN h_PDFinc ("Spdf", "ENDEREÄO PARA COBRANÄA" , 405 , 670 ).
        RUN pdf_set_font IN h_PDFinc ("Spdf", "Arial", 8).
        RUN pdf_text_xy  IN h_PDFinc ("Spdf", estabelec.endereco, 375 , 653 ).
        RUN pdf_text_xy  IN h_PDFinc ("Spdf", estabelec.bairro, 510 , 653 ).
        RUN pdf_text_xy  IN h_PDFinc ("Spdf", estabelec.cidade, 375 , 642 ).
        RUN pdf_text_xy  IN h_PDFinc ("Spdf", estabelec.estado + "  " + STRING(estabelec.cep), 510 , 642 ).
    END.

END PROCEDURE.

PROCEDURE pi-cond-pagto.

    DEF INPUT PARAMETER rw-pedido-compr AS ROWID.

    FIND FIRST pedido-compr WHERE
         ROWID(pedido-compr)  = rw-pedido-compr NO-LOCK NO-ERROR.

    ASSIGN c-cond-pagto = " "
           c-data-pagto = ?.

    RUN pdf_set_font IN h_PDFinc ("Spdf", "Courier-bold", 8).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "DATA DE EMISS«O" , 375 , 618 ).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "CONDIÄÂES DE PAGAMENTO" , 467 , 618 ).
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Arial", 8).

    IF pedido-compr.cod-cond-pag = 0 THEN DO: /* Condiá∆o especifica */
       FIND FIRST cond-especif WHERE
                  cond-especif.num-pedido = pedido-compr.num-pedido
                   NO-LOCK NO-ERROR.

       IF AVAIL cond-especif THEN DO i-cont = 1 TO 6:
          IF cond-especif.perc-pagto[i-cont] > 0 THEN DO.
             ASSIGN c-data-pagto = STRING(cond-especif.data-pagto[i-cont], "99/99/9999")
                    c-cond-pagto = c-cond-pagto + " " + c-data-pagto + " - " +
                                   STRING(cond-especif.perc-pagto[i-cont]) + " % ".
          END.
       END.
       RUN pdf_text_xy  IN h_PDFinc ("Spdf", STRING(c-data-pagto, "99/99/9999") , 390 , 605 ).
       RUN pdf_set_font IN h_PDFinc ("Spdf", "Arial", 6).
       RUN pdf_text_xy  IN h_PDFinc ("Spdf", c-cond-pagto , 467 , 605 ).
    END.
    ELSE DO.
        FIND cond-pagto WHERE
             cond-pagto.cod-cond-pag = pedido-compr.cod-cond-pag NO-LOCK NO-ERROR.
        IF AVAIL cond-pagto THEN DO.
            RUN pdf_text_xy  IN h_PDFinc ("Spdf", STRING(pedido-compr.data-pedido, "99/99/9999") , 390 , 605 ).
            RUN pdf_set_font IN h_PDFinc ("Spdf", "Arial", 6).
            RUN pdf_text_xy  IN h_PDFinc ("Spdf", cond-pagto.descricao , 467 , 605 ).
        END.
    END.

END.

PROCEDURE pi-imprime-ordem.

    DEF INPUT PARAMETER rw-pedido-compr AS ROWID.

    FIND FIRST pedido-compr WHERE
         ROWID(pedido-compr)  = rw-pedido-compr NO-LOCK NO-ERROR.

    ASSIGN l-tem-ordem-rec = NO.

    ASSIGN i-linha  = 558
           i-qte-ordem = 0.

    FOR EACH ordem-compra WHERE
             ordem-compra.num-pedido = pedido-compr.num-pedido AND
             ordem-compra.situacao <> 4 AND
             ordem-compra.situacao <> 6  USE-INDEX pedido NO-LOCK
        BREAK BY ordem-compra.num-pedido
              BY ordem-compra.numero-ordem:

        RUN pi-dados-item (INPUT ROWID(ordem-compra)).

        ASSIGN l-tem-ordem-rec = YES.

        IF LAST-OF(ordem-compra.num-pedido) THEN DO.
            RUN pdf_set_font IN h_PDFinc ("Spdf", "Arial", 6).
            RUN pdf_text_xy  IN h_PDFinc ("Spdf", STRING(de-total-sipi, "ZZZ,ZZZ,ZZ9.99"), 520, 74).
            RUN pdf_text_xy  IN h_PDFinc ("Spdf", STRING(de-total-geral, "ZZZ,ZZZ,ZZ9.99"), 520, 62).
        END.
    END.
END PROCEDURE.

PROCEDURE pi-cria-page.
    DEF INPUT PARAMETER c-logo AS CHAR NO-UNDO.
    RUN pdf_new_page ("Spdf").
    RUN pdf_load_image ("Spdf",c-logo,c-arq-logo).   /* Importando logotipo */
    RUN pdf_place_image ("Spdf", c-logo,15,85,50,65). /*Redefinindo Logotipo */

END PROCEDURE.

PROCEDURE pi-imprime-linhas.
    RUN pdf_line ("Spdf", 15,683,15,600,1).
    RUN pdf_line ("Spdf", 15,683,600,683,1).
    RUN pdf_line ("Spdf", 15,665,600,665,1).
    RUN pdf_line ("Spdf", 370,600,370,682,1).
    RUN pdf_line ("Spdf", 370,635,600,635,1).
    RUN pdf_line ("Spdf", 370,630,600,630,1).
    RUN pdf_line ("Spdf", 370,615,600,615,1).
    RUN pdf_line ("Spdf", 15,600,600,600,1).
    RUN pdf_line ("Spdf", 600,600,600,630,1).
    RUN pdf_line ("Spdf", 459,600,459,630,1).
    RUN pdf_line ("Spdf", 600,635,600,682,1).
    RUN pdf_line ("Spdf", 15,88,15,595,1).
    RUN pdf_line ("Spdf", 15,595,600,595,1).
    RUN pdf_line ("Spdf", 15,580,600,580,1).
    RUN pdf_line ("Spdf", 61,579,61,210,1).
    RUN pdf_line ("Spdf", 350,579,350,210,1).
    RUN pdf_line ("Spdf", 370,579,370,210,1).
    RUN pdf_line ("Spdf", 405,579,405,210,1).
    RUN pdf_line ("Spdf", 450,579,450,210,1).
    RUN pdf_line ("Spdf", 510,579,510,210,1).
    RUN pdf_line ("Spdf", 535,579,535,210,1).
    RUN pdf_line ("Spdf", 600,595,600,88,1).
    RUN pdf_line ("Spdf", 15,210,600,210,1).
    RUN pdf_line ("Spdf", 15,190,600,190,1).
    RUN pdf_line ("Spdf", 15,88,600,88,1).
    RUN pdf_line ("Spdf", 15,82,400,82,1).
    RUN pdf_line ("Spdf", 15,20,400,20,1).
    RUN pdf_line ("Spdf", 410.6,82,600,82,1).
    RUN pdf_line ("Spdf", 410.6,70,600,70,1).
    RUN pdf_line ("Spdf", 410.6,58,600,58,1).
    RUN pdf_line ("Spdf", 410.6,55,600,55,1).
    RUN pdf_line ("Spdf", 418,35,592,35,1).
    RUN pdf_line ("Spdf", 412,20,600,20,1).
    RUN pdf_line ("Spdf", 401,82,401,20,1).
    RUN pdf_line ("Spdf", 411,82,411,58,1).
    RUN pdf_line ("Spdf", 600,82,600,58,1).
    RUN pdf_line ("Spdf", 411,55,411,20,1).
    RUN pdf_line ("Spdf", 600,55,600,20,1).
    RUN pdf_line ("Spdf", 15,20,15,81,1).

    RUN pdf_set_font IN h_PDFinc ("Spdf", "Arial", 8).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "PEDIMOS FORNECER, NAS CONDIÄÂES INDICADAS, AS MERCADORIAS ABAIXO DISCRIMINADAS: " , 117 , 584 ).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "ORDEM" , 20 , 570 ).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "DESCRIÄ«O" , 70 , 570 ).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "UN" , 355 , 570 ).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "QTDE" , 375 , 570 ).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "VL.UNIT." , 410, 570 ).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "VALOR TOTAL" , 453 , 570 ).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "%IPI" , 513 , 570 ).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "DT.ENTREGA" , 540, 570 ).
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Arial", 6).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "VALOR DAS MERCADORIAS R$" , 415 , 74 ).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "VALOR TOTAL COM IPI R$" , 415 , 62 ).
    RUN pdf_text_xy  IN h_PDFinc ("Spdf", "ASSINATURA" , 485 , 26 ).

END PROCEDURE.

PROCEDURE pi-formata-mensagem.
    IF tt-param.e-mail THEN DO.
       IF SEARCH(c-arq-salvo) = ? THEN DO.
           RUN utp\mensagem ( INPUT "Arquivo Inexistente!",
                              INPUT "Imposs°vel enviar e-mail, arquivo " + c-arq-salvo + " n∆o foi gerado....").
              NEXT.
       END.

       ASSIGN tt-param.texto = REPLACE(tt-param.texto,"#PED",STRING(pedido-compr.num-pedido)).
       ASSIGN tt-param.texto = REPLACE(tt-param.texto,"#FONE","(038) 3749-6810").

       ASSIGN c-arq-texto = "D:\Datasul10\ems206\liasa\PedCompra\PED" + STRING(pedido-compr.num-pedido) + ".TXT".
       OUTPUT TO VALUE(c-arq-texto) CONVERT SOURCE "ibm850".
          PUT tt-param.texto
              SKIP.
       OUTPUT CLOSE.

       ASSIGN tt-param.subject = tt-param.subject + " - " + STRING(pedido-compr.num-pedido, "999,999").

       ASSIGN c-arq-condicoes = "CONDIÄÂES GERAIS PARA PEDIDOS DE COMPRAS DA LIGAS DE ALUM÷NIO S.A. - LIASA - PC " + STRING(TRIM(STRING(pedido-compr.num-pedido))) + "-DATASUL SC".

       RUN env-email.

       OS-DELETE VALUE(c-arq-texto).
    END.
END PROCEDURE.

PROCEDURE env-email.

/*     FIND FIRST usuar_mestre WHERE                                              */
/*                usuar_mestre.cod_usuario = v_cod_usuar_corren NO-LOCK NO-ERROR. */
/*     IF AVAIL usuar_mestre THEN                                                 */
/*         ASSIGN c-destino = c-destino + "," + usuar_mestre.cod_e_mail_local.    */

    OS-COPY VALUE("D:\Datasul10\ems206\liasa\PedCompra\COND-PED-COMPRAS.PDF") VALUE("D:\Datasul10\ems206\liasa\PedCompra\" + c-arq-condicoes + ".PDF").

    RUN utp/utapi019.p PERSISTENT SET h-utapi019.

    FIND FIRST param-global NO-LOCK NO-ERROR.

    FOR EACH tt-envio2.
        DELETE tt-envio2.
    END.
    FOR EACH tt-mensagem.
        DELETE tt-mensagem.
    END.
    FOR EACH tt-erros.
        DELETE tt-erros.
    END.

    FIND usuar_mestre WHERE
         usuar_mestre.cod_usuario = c-seg-usuario NO-LOCK NO-ERROR.
    IF AVAILABLE usuar_mestre THEN DO.
       IF usuar_mestre.cod_e_mail_local <> "" THEN DO:
          ASSIGN c-remetente = usuar_mestre.cod_e_mail_local.
       END.
    END.

    CREATE tt-envio2.
    ASSIGN tt-envio2.versao-integracao = 1
           tt-envio2.exchange          = YES
           tt-envio2.servidor          = IF AVAIL param-global THEN param-global.serv-mail ELSE ""
           tt-envio2.porta             = IF AVAIL param-global THEN param-global.porta-mail ELSE 0
           tt-envio2.destino           = c-destino
           tt-envio2.remetente         = c-remetente
           tt-envio2.assunto           = tt-param.subject
           tt-envio2.importancia       = 2
           tt-envio2.arq-anexo         = c-arq-salvo + ",D:\Datasul10\ems206\liasa\PedCompra\COND-PED-COMPRAS.PDF".
           tt-envio2.arq-anexo         = c-arq-salvo + ",D:\Datasul10\ems206\liasa\PedCompra\COND-PED-COMPRAS.PDF".

    /*** Verifica se o pedido de compra Ç de SERVIÄO para enviar mais um arquivo em anexo*/

    IF AVAIL pedido-compr AND pedido-compr.natureza = 2 THEN
        ASSIGN tt-envio2.arq-anexo     = tt-envio2.arq-anexo + ",D:\Datasul10\ems206\liasa\PedCompra\GERENCIAMENTO_DE_PRESTADOR_DE_SERVIÄO.PDF".

    /*** fim da verificaá∆o ***/

    CREATE tt-mensagem.
    ASSIGN tt-mensagem.seq-mensagem = 1
           tt-mensagem.mensagem = tt-param.texto.

    ASSIGN i-cont = 2.

    RUN pi-execute2 IN h-utapi019 (INPUT  TABLE tt-envio2,
                                   INPUT  TABLE tt-mensagem,
                                   OUTPUT TABLE tt-erros).

    IF  RETURN-VALUE = "NOK" THEN DO:
        FOR EACH tt-erros:
            RUN utp\mensagem ( INPUT "Erros!!!",
                               INPUT tt-erros.desc-erro ).
        END.
    END.
    DELETE PROCEDURE h-utapi019.

    OS-DELETE VALUE("D:\Datasul10\ems206\liasa\PedCompra\" + c-arq-condicoes + ".PDF").
END PROCEDURE.

PROCEDURE WinExec EXTERNAL "kernel32.dll":
    DEF INPUT PARAMETER prog_name    AS CHAR.
    DEF INPUT PARAMETER visual_style AS SHORT.
END PROCEDURE



