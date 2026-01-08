DEFINE VARIABLE email-validado AS LOGICAL     NO-UNDO.

DEF SHARED TEMP-TABLE tt-sel
    FIELD fi-data-ini      AS DATE
    FIELD fi-data-fim      AS DATE
    FIELD fi-cliente-ini   AS INT
    FIELD fi-cliente-fim   AS INT
    FIELD fi-repres-ini    AS INT
    FIELD fi-repres-fim    AS INT
    FIELD fi-cidade-ini    AS CHAR
    FIELD fi-cidade-fim    AS CHAR
    FIELD fi-estado-ini    AS CHAR
    FIELD fi-estado-fim    AS CHAR
    FIELD rs-estab         AS INT
    FIELD tg-repres-ima    AS LOG
    FIELD tg-suspenso      AS INT
    FIELD fi-ramo-ativ-ini AS INT
    FIELD fi-ramo-ativ-fim AS INT
    FIELD rs-email         AS INT
    FIELD tg-repres-dif    AS LOG.

DEF SHARED TEMP-TABLE tt-conteudo
    FIELD cod-emitente   AS INT FORMAT ">>>>>9"
    FIELD nome-ab-cli    AS CHAR FORMAT "x(25)"          
    FIELD nome-emit      AS CHAR FORMAT "x(45)"          
    FIELD endereco       AS CHAR FORMAT "x(45)"          
    FIELD cidade         AS CHAR FORMAT "x(45)"          
    FIELD bairro         AS CHAR FORMAT "x(30)"          
    FIELD estado         AS CHAR FORMAT "X(2)" 
    FIELD CEP            AS CHAR FORMAT "x(20)"
    FIELD endereco-cob   AS CHAR FORMAT "x(45)"          
    FIELD cidade-cob     AS CHAR FORMAT "x(45)"          
    FIELD bairro-cob     AS CHAR FORMAT "x(30)"          
    FIELD estado-cob     AS CHAR FORMAT "X(2)"           
    FIELD CEP-cob        AS CHAR FORMAT "x(20)"
    FIELD email          AS CHAR FORMAT "x(60)" 
    FIELD ind-cre-cli    AS INT FORMAT "9"
    FIELD cod-ramo-ativ  AS INT FORMAT ">>>>>9"
    FIELD desc-ramo-ativ AS CHAR FORMAT "x(45)"
    FIELD telefone       AS CHAR FORMAT "x(40)"
    FIELD dt-ult-compra  AS DATE
    FIELD tot-venda      AS DEC
    FIELD cod-rep-ven    AS INT
    FIELD nom-rep-ven    AS CHAR FORMAT "x(45)"
    FIELD cod-rep-cad    AS INT
    FIELD nom-rep-cad    AS CHAR FORMAT "x(45)"
    FIELD cgc            AS CHAR FORMAT "x(45)".

DEFINE VARIABLE rep1 AS INTEGER    NO-UNDO.
DEFINE VARIABLE rep11212 AS INTEGER    NO-UNDO.
DEFINE VARIABLE c-destinatario LIKE mgadm.emitente.e-mail.
def var h-acomp as handle no-undo.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

FIND tt-sel NO-LOCK NO-ERROR.

IF tg-repres-ima THEN
   ASSIGN rep1     = 1
          rep11212 = 11212.
ELSE
   ASSIGN rep1     = ?
          rep11212 = ?.

RUN pi-acompanhar IN  h-acomp (INPUT "IMA").

FOR EACH movadm.nota-fiscal WHERE 
         movadm.nota-fiscal.dt-emis-nota >= tt-sel.fi-data-ini AND 
         movadm.nota-fiscal.dt-emis-nota <= tt-sel.fi-data-fim AND 
         movadm.nota-fiscal.dt-cancela    = ? AND 
         movadm.nota-fiscal.cod-rep      <> rep1 AND 
         movadm.nota-fiscal.cod-rep      <> rep11212 AND 
         movadm.nota-fiscal.cod-rep      >= tt-sel.fi-repres-ini AND 
         movadm.nota-fiscal.cod-rep      <= tt-sel.fi-repres-fim AND 
         movadm.nota-fiscal.cod-emitente <> 1 AND 
         movadm.nota-fiscal.cod-emitente <> 11212 AND 
         movadm.nota-fiscal.cod-emitente <> 10535 AND 
         movadm.nota-fiscal.cod-emitente >= tt-sel.fi-cliente-ini AND 
         movadm.nota-fiscal.cod-emitente <= tt-sel.fi-cliente-fim AND 
         movadm.nota-fiscal.cidade       >= tt-sel.fi-cidade-ini AND 
         movadm.nota-fiscal.cidade       <= tt-sel.fi-cidade-fim AND 
         movadm.nota-fiscal.estado       >= tt-sel.fi-estado-ini AND 
         movadm.nota-fiscal.estado       <= tt-sel.fi-estado-fim 
         /*
         AND 
        (SUBSTRING(movadm.nota-fiscal.nat-operacao,1,3) = "512" OR
         SUBSTRING(movadm.nota-fiscal.nat-operacao,1,3) = "612" OR
         SUBSTRING(movadm.nota-fiscal.nat-operacao,1,3) = "619" OR
         SUBSTRING(movadm.nota-fiscal.nat-operacao,1,3) = "712") 
         */
         USE-INDEX ch-sit-nota NO-LOCK,
    EACH mgadm.emitente WHERE 
         mgadm.emitente.nome-abrev = movadm.nota-fiscal.nome-ab-cli AND 
         mgadm.emitente.ind-cre-cli <> tt-sel.tg-suspenso NO-LOCK,
    EACH ext-emitente WHERE 
         ext-emitente.cod-emitente = mgadm.emitente.cod-emitente AND 
         ext-emitente.cod-ramo-ativ >= tt-sel.fi-ramo-ativ-ini AND 
         ext-emitente.cod-ramo-ativ <= tt-sel.fi-ramo-ativ-fim NO-LOCK 
    BREAK BY movadm.nota-fiscal.dt-emis-nota DESC.

    FIND mgind.natur-oper OF movadm.nota-fiscal NO-LOCK NO-ERROR.
    IF NOT mgind.natur-oper.emite-duplic THEN NEXT.

   FIND FIRST tt-conteudo WHERE 
              tt-conteudo.cod-emitente = movadm.nota-fiscal.cod-emitente NO-LOCK NO-ERROR.
   
   IF AVAIL tt-conteudo THEN DO:
      ASSIGN tt-conteudo.tot-venda = tt-conteudo.tot-venda + movadm.nota-fiscal.vl-tot-nota.
      NEXT.
   END.
   
   /*IF tt-sel.tg-repres-dif AND movadm.nota-fiscal.cod-rep = emitente.cod-rep THEN NEXT.*/

   ASSIGN c-destinatario = "".
   FIND mgadm.cont-emit OF emitente WHERE
        mgadm.cont-emit.area = 'COMERCIAL' NO-LOCK NO-ERROR.
   IF AVAIL mgadm.cont-emit THEN
      ASSIGN c-destinatario = mgadm.cont-emit.e-mail.
   ELSE
      ASSIGN c-destinatario = mgadm.emitente.e-mail.

   IF tt-sel.rs-email <> 1 THEN DO:
      IF c-destinatario = "" THEN NEXT.

      RUN esapi\email-valida.p (INPUT  c-destinatario, 
                                OUTPUT email-validado ).
      IF email-validado = NO  AND tt-sel.rs-email = 2 THEN NEXT.
      IF email-validado = YES AND tt-sel.rs-email = 3 THEN NEXT.
   END.
   
   RUN pi-acompanhar IN  h-acomp (INPUT "IMA" + "-" + STRING(movadm.nota-fiscal.dt-emis-nota) + '-' + movadm.nota-fiscal.nome-ab-cli).

   FIND ramo-ativ WHERE 
        ramo-ativ.cod-ramo-ativ = ext-emitente.cod-ramo-ativ NO-LOCK NO-ERROR.
   
   CREATE tt-conteudo.
   ASSIGN tt-conteudo.cod-emitente     = movadm.nota-fiscal.cod-emitente
          tt-conteudo.nome-ab-cli      = movadm.nota-fiscal.nome-ab-cli
          tt-conteudo.nome-emit        = mgadm.emitente.nome-emit
          tt-conteudo.endereco         = mgadm.emitente.endereco                                                                                                                                                                                     
          tt-conteudo.cidade           = mgadm.emitente.cidade                                                                                                                                                                                       
          tt-conteudo.bairro           = mgadm.emitente.bairro                                                                                                                                                                                       
          tt-conteudo.estado           = mgadm.emitente.estado                                                                                             
          tt-conteudo.CEP              = substring(mgadm.emitente.CEP,1,2) + "." + substring(emitente.CEP,3,3) + "-" + substring(emitente.CEP,6,3) 
          tt-conteudo.endereco-cob     = mgadm.emitente.endereco-cob                                                                                           
          tt-conteudo.cidade-cob       = mgadm.emitente.cidade-cob                                                                                             
          tt-conteudo.bairro-cob       = mgadm.emitente.bairro-cob                                                                                             
          tt-conteudo.estado-cob       = mgadm.emitente.estado-cob    
          tt-conteudo.telefone         = mgadm.emitente.telefone[1] + " " + emitente.telefone[2]
          tt-conteudo.CEP-cob          = substring(mgadm.emitente.CEP-cob,1,2) + "." + substring(emitente.CEP-cob,3,3) + "-" + substring(emitente.CEP-cob,6,3) 
          tt-conteudo.email            = c-destinatario
          tt-conteudo.ind-cre-cli      = mgadm.emitente.ind-cre-cli
          tt-conteudo.cod-ramo-ativ    = ext-emitente.cod-ramo-ativ WHEN AVAIL ext-emitente
          tt-conteudo.desc-ramo-ativ   = ramo-ativ.descricao WHEN AVAIL ramo-ativ
          tt-conteudo.dt-ult-compra    = movadm.nota-fiscal.dt-emis-nota
          tt-conteudo.tot-venda        = movadm.nota-fiscal.vl-tot-nota
          tt-conteudo.cgc              = mgadm.emitente.cgc.
   
   FIND mgadm.repres WHERE 
        mgadm.repres.cod-rep = movadm.nota-fiscal.cod-rep NO-LOCK NO-ERROR.
          
   ASSIGN tt-conteudo.cod-rep-ven = movadm.nota-fiscal.cod-rep
          tt-conteudo.nom-rep-ven = mgadm.repres.nome-abrev WHEN AVAIL mgadm.repres.

   FIND mgadm.repres WHERE 
        mgadm.repres.cod-rep = mgadm.emitente.cod-rep NO-LOCK NO-ERROR.

   ASSIGN tt-conteudo.cod-rep-cad = mgadm.emitente.cod-rep
          tt-conteudo.nom-rep-cad = mgadm.repres.nome-abrev WHEN AVAIL mgadm.repres.
END.

IF tt-sel.rs-estab = 1 THEN DO:  /* Todos */
   RUN pi-acompanhar IN  h-acomp (INPUT "MED").

   FOR EACH dbaux.nota-fiscal WHERE 
            dbaux.nota-fiscal.dt-emis-nota >= tt-sel.fi-data-ini AND 
            dbaux.nota-fiscal.dt-emis-nota <= tt-sel.fi-data-fim AND 
            dbaux.nota-fiscal.dt-cancela    = ? AND 
            dbaux.nota-fiscal.cod-rep      <> rep1 AND 
            dbaux.nota-fiscal.cod-rep      <> rep11212 AND 
            dbaux.nota-fiscal.cod-rep      >= tt-sel.fi-repres-ini AND 
            dbaux.nota-fiscal.cod-rep      <= tt-sel.fi-repres-fim AND 
            dbaux.nota-fiscal.cod-emitente <> 1 AND 
            dbaux.nota-fiscal.cod-emitente <> 11212 AND 
            dbaux.nota-fiscal.cod-emitente <> 10535 AND 
            dbaux.nota-fiscal.cod-emitente >= tt-sel.fi-cliente-ini AND 
            dbaux.nota-fiscal.cod-emitente <= tt-sel.fi-cliente-fim AND 
            dbaux.nota-fiscal.cidade       >= tt-sel.fi-cidade-ini AND 
            dbaux.nota-fiscal.cidade       <= tt-sel.fi-cidade-fim AND 
            dbaux.nota-fiscal.estado       >= tt-sel.fi-estado-ini AND 
            dbaux.nota-fiscal.estado       <= tt-sel.fi-estado-fim
            /*
            AND 
           (SUBSTRING(dbaux.nota-fiscal.nat-operacao,1,3) = "512" OR
            SUBSTRING(dbaux.nota-fiscal.nat-operacao,1,3) = "612" OR
            SUBSTRING(dbaux.nota-fiscal.nat-operacao,1,3) = "619" OR
            SUBSTRING(dbaux.nota-fiscal.nat-operacao,1,3) = "712") 
            */
            USE-INDEX ch-sit-nota NO-LOCK,
       EACH emitente WHERE 
            emitente.nome-abrev = dbaux.nota-fiscal.nome-ab-cli AND 
            emitente.ind-cre-cli <> tt-sel.tg-suspenso NO-LOCK,
       EACH ext-emitente WHERE 
            ext-emitente.cod-emitente = emitente.cod-emitente AND 
            ext-emitente.cod-ramo-ativ >= tt-sel.fi-ramo-ativ-ini AND 
            ext-emitente.cod-ramo-ativ <= tt-sel.fi-ramo-ativ-fim NO-LOCK 
       BREAK  BY dbaux.nota-fiscal.dt-emis-nota DESC.

       FIND mgind.natur-oper OF dbaux.nota-fiscal NO-LOCK NO-ERROR.
       IF NOT mgind.natur-oper.emite-duplic THEN NEXT.

       FIND FIRST tt-conteudo WHERE 
                  tt-conteudo.cod-emitente = dbaux.nota-fiscal.cod-emitente NO-LOCK NO-ERROR.
       
       IF AVAIL tt-conteudo THEN DO:
          ASSIGN tt-conteudo.tot-venda = tt-conteudo.tot-venda + dbaux.nota-fiscal.vl-tot-nota.
          
          IF dbaux.nota-fiscal.dt-emis-nota > tt-conteudo.dt-ult-compra THEN DO:
             ASSIGN tt-conteudo.dt-ult-compra = dbaux.nota-fiscal.dt-emis-nota.
             
             FIND repres WHERE 
                  repres.cod-rep = dbaux.nota-fiscal.cod-rep NO-LOCK NO-ERROR.

             ASSIGN tt-conteudo.cod-rep-ven = dbaux.nota-fiscal.cod-rep
                    tt-conteudo.nom-rep-ven = repres.nome-abrev WHEN AVAIL repres.
          END.
          NEXT.
       END.
       
       /*IF tt-sel.tg-repres-dif AND 
           dbaux.nota-fiscal.cod-rep = emitente.cod-rep THEN NEXT.*/

       ASSIGN c-destinatario = "".
       FIND cont-emit OF emitente WHERE
            cont-emit.area = 'COMERCIAL' NO-LOCK NO-ERROR.
       IF AVAIL cont-emit THEN
          ASSIGN c-destinatario = cont-emit.e-mail.
       ELSE
          ASSIGN c-destinatario = emitente.e-mail.

       IF tt-sel.rs-email <> 1 THEN DO:
          IF c-destinatario = "" THEN NEXT.

          RUN esapi\email-valida.p (INPUT  c-destinatario, 
                                    OUTPUT email-validado ).

          IF email-validado = NO  AND tt-sel.rs-email = 2 THEN NEXT.
          IF email-validado = YES AND tt-sel.rs-email = 3 THEN NEXT.
       END.

       RUN  pi-acompanhar IN  h-acomp (INPUT "MED" + "-" + STRING(dbaux.nota-fiscal.dt-emis-nota) + '-' + dbaux.nota-fiscal.nome-ab-cli).

       FIND ramo-ativ WHERE 
            ramo-ativ.cod-ramo-ativ = ext-emitente.cod-ramo-ativ NO-LOCK NO-ERROR.
       FIND repres WHERE 
            repres.cod-rep = dbaux.nota-fiscal.cod-rep NO-LOCK NO-ERROR.
   
       CREATE tt-conteudo.
       ASSIGN tt-conteudo.cod-emitente     = dbaux.nota-fiscal.cod-emitente
              tt-conteudo.nome-ab-cli      = dbaux.nota-fiscal.nome-ab-cli
              tt-conteudo.nome-emit        = emitente.nome-emit
              tt-conteudo.endereco         = emitente.endereco                                                                                                                                                                                     
              tt-conteudo.cidade           = emitente.cidade                                                                                                                                                                                       
              tt-conteudo.bairro           = emitente.bairro                                                                                                                                                                                       
              tt-conteudo.estado           = emitente.estado                                                                                             
              tt-conteudo.cep              = SUBSTRING(emitente.CEP,1,2) + "." + substring(emitente.CEP,3,3) + "-" + substring(emitente.CEP,6,3) 
              tt-conteudo.endereco-cob     = emitente.endereco-cob                                                                                           
              tt-conteudo.cidade-cob       = emitente.cidade-cob                                                                                             
              tt-conteudo.bairro-cob       = emitente.bairro-cob                                                                                             
              tt-conteudo.estado-cob       = emitente.estado-cob    
              tt-conteudo.telefone         = emitente.telefone[1] + " " + emitente.telefone[2]
              tt-conteudo.cep-cob          = SUBSTRING(emitente.CEP-cob,1,2) + "." + substring(emitente.CEP-cob,3,3) + "-" + substring(emitente.CEP-cob,6,3) 
              tt-conteudo.email            = c-destinatario
              tt-conteudo.ind-cre-cli      = emitente.ind-cre-cli
              tt-conteudo.cod-ramo-ativ    = ext-emitente.cod-ramo-ativ WHEN AVAIL ext-emitente
              tt-conteudo.desc-ramo-ativ   = ramo-ativ.descricao WHEN AVAIL ramo-ativ
              tt-conteudo.dt-ult-compra    = dbaux.nota-fiscal.dt-emis-nota
              tt-conteudo.tot-venda        = dbaux.nota-fiscal.vl-tot-nota
              tt-conteudo.cgc              = emitente.cgc.

       FIND repres WHERE 
            repres.cod-rep = dbaux.nota-fiscal.cod-rep NO-LOCK NO-ERROR.

       ASSIGN tt-conteudo.cod-rep-ven = dbaux.nota-fiscal.cod-rep
              tt-conteudo.nom-rep-ven = repres.nome-abrev WHEN AVAIL repres.

       FIND repres WHERE 
            repres.cod-rep = emitente.cod-rep NO-LOCK NO-ERROR.

       ASSIGN tt-conteudo.cod-rep-cad = emitente.cod-rep
              tt-conteudo.nom-rep-cad = repres.nome-abrev WHEN AVAIL repres.
   END.
END.

IF tt-sel.tg-repres-dif THEN DO:
   FOR EACH tt-conteudo WHERE tt-conteudo.cod-rep-ven = tt-conteudo.cod-rep-cad NO-LOCK.
       RUN pi-acompanhar IN  h-acomp (INPUT "Vendas Corretas" + "-" + STRING(tt-conteudo.cod-emitente) + '-' + tt-conteudo.nome-ab-cli).
       DELETE tt-conteudo.
   END.
END.

IF CONNECTED ("dbaux") THEN
   DISCONNECT dbaux.

RUN pi-finalizar IN h-acomp.
