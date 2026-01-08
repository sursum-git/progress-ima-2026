/*****************************************************************************
programa: esapi/getEtqsRetornoISF.p
objetivo: retornar os dados das etiquetas separadas na lisa
autor: Tadeu Silva
data:07/2024                                                               
****************************************************************************/

{esapi/getEtqsRetornoISF.i}

DEFINE INPUT  PARAMETER pNrPedido AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER pItem     AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pRef      AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR ttEtq.


DEFINE VARIABLE cChave      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cSituacao   AS CHARACTER   NO-UNDO.

ASSIGN cChave = string(pNrPedido)   + "|" +
                pItem               + "|" +
                pRef        .


FOR EACH lisa-integra NO-LOCK
    WHERE lisa-integra.cod-trans = 'RetornoISF'
    AND   lisa-integra.chave     = cChave
    .
    
    CREATE ttEtq.
    ASSIGN ttEtq.numEtq     = INT(lisa-integra.conteudo)
           ttEtq.qtLisa     = DECIMAL(replace(lisa-integra.val-livre-5,".",","))
           ttEtq.idEtqLisa  = lisa-integra.val-livre-4
           .
          
    FIND ob-etiqueta NO-LOCK
        WHERE ob-etiqueta.cod-estabel   = '505'
        AND   ob-etiqueta.num-etiqueta  = INT(lisa-integra.conteudo)
        NO-ERROR.
    IF NOT AVAIL ob-etiqueta THEN DO:
       ASSIGN cSituacao     = "Etq. N∆o Encontrada"
              ttEtq.qtEtq   = 0.
    END.
    ELSE DO:
       RUN esapi/getDescrSitEtq.p(ob-etiqueta.situacao,OUTPUT cSituacao).
       ASSIGN ttEtq.qtEtq   = ob-etiqueta.quantidade .
    END.
    ASSIGN ttEtq.situacao = cSituacao.
END.

FOR EACH ttEtq.

     IF CAN-FIND( FIRST lisa-integra 
                  WHERE lisa-integra.cod-trans      = "ConfEtiquetas" 
                  AND   lisa-integra.ind-situacao   = 1 // Aguardando Integraáao
                  AND   lisa-integra.chave          = string(ttEtq.numEtq)
                  ) THEN
        ASSIGN ttEtq.observ = "Ajuste ETQ Pendente.".

     IF ttEtq.qtEtq <> ttEtq.qtLisa AND ttEtq.qtLisa > 0  THEN 
        ASSIGN ttEtq.observ = ttEtq.observ + " Quantidade Etiqueta LISA Diferente da Etiqueta MED ".
     IF ttEtq.qtLisa = 0 THEN
        ASSIGN ttEtq.observ = ttEtq.observ + " Quantidade Etiqueta LISA n∆o extraida do JSON ".
        
     IF ttEtq.idEtqLisa = '' THEN DO:
        FIND etiqueta_lisa NO-LOCK
            WHERE etiqueta_lisa.cod_estabel  = '505'
            AND   etiqueta_lisa.num_etiqueta =  ttEtq.numEtq
            NO-ERROR.
        IF AVAIL etiqueta_lisa THEN
           ASSIGN ttEtq.idEtqLisa = etiqueta_Lisa.id_etq_lisa .
     END.

END.
