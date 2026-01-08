{lisa/EtqLisa.i}
DEFINE INPUT  PARAMETER pCodEstabel AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pitCodigo   AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pCodRefer   AS CHARACTER   NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR   ttEtq .

DEFINE VARIABLE itCodigoIni AS CHARACTER   NO-UNDO INIT ''.
DEFINE VARIABLE itCodigoFim AS CHARACTER   NO-UNDO INIT 'zzzzzzzzzzzz'.

DEFINE VARIABLE codReferIni AS CHARACTER   NO-UNDO INIT ''.
DEFINE VARIABLE codReferFim AS CHARACTER   NO-UNDO INIT 'zzzzzz'.

DEFINE VARIABLE codEstabelIni AS CHARACTER   NO-UNDO INIT ''.
DEFINE VARIABLE codEstabelFim AS CHARACTER   NO-UNDO INIT 'zzzzzz'.

DEFINE VARIABLE iUltId AS INTEGER     NO-UNDO.

IF pItCodigo <> '' THEN
   ASSIGN itCodigoIni   = pItCodigo
          itCodigoFim   = pItCodigo.

IF pCodRefer <> '' THEN
   ASSIGN codReferIni   = pCodRefer
          codReferFim   = pCodRefer.

IF pCodEstabel <> '' THEN
   ASSIGN codEstabelIni   = pCodEstabel
          codEstabelFim   = pCodEstabel.

FIND LAST ttEtq USE-INDEX unico NO-ERROR.

ASSIGN iUltId = IF AVAIL ttEtq THEN ttEtq.id ELSE 0.

FUNCTION getDescrSit RETURNS CHAR(sit AS INT):

    CASE sit:
        WHEN 1 THEN
            RETURN 'Impressa'.
        WHEN 2 THEN
            RETURN 'Produ‡Æo'.
        WHEN 3 THEN
            RETURN 'Estoque'.
        WHEN 4 THEN
            RETURN 'Alocada'.
        WHEN 5 THEN
            RETURN 'Faturada'.
        WHEN 6 THEN
            RETURN 'Reprocesso'.
        WHEN 7 THEN
            RETURN 'Consumo Corte'.
        WHEN 8 THEN
            RETURN 'Bloqueado'.
        WHEN 9 THEN
            RETURN 'Consumido'.

    END CASE.



END FUNCTION.

/*MESSAGE codEstabelIni
        codEstabelFim SKIP
        itCodigoIni  
        itCodigoFim  SKIP
        codReferIni  
        codReferFim
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
FOR EACH ob-etiqueta FIELDS(cod-estabel it-codigo cod-refer num-etiqueta nr-container num-rolo-imp situacao localiz quantidade) NO-LOCK
    WHERE ob-etiqueta.cod-estabel   >= codEstabelIni
    AND   ob-etiqueta.cod-estabel   <= codEstabelFim
    AND   ob-etiqueta.it-codigo     >= itCodigoIni
    AND   ob-etiqueta.it-codigo     <= itCodigoFim
    AND   ob-etiqueta.cod-refer     >= codReferIni
    AND   ob-etiqueta.cod-refer     <= codReferFim.
    ASSIGN iUltId = iUltId + 1.

    FOR FIRST ITEM FIELDS(desc-item) OF ob-etiqueta  NO-LOCK .
    END.
    FIND ped-item-rom 
        where ped-item-rom.cod-estabel  = ob-etiqueta.cod-estabel
        AND ped-item-rom.num-etiqueta =  ob-etiqueta.num-etiqueta
         NO-LOCK NO-ERROR.
    FIND nota-fiscal NO-LOCK
        WHERE nota-fiscal.nome-ab-cli  = ped-item-rom.nome-abrev
        AND   nota-fiscal.nr-pedcli    = ped-item-rom.nr-pedcli
        AND   nota-fiscal.dt-cancel    = ? NO-ERROR.
    CREATE ttEtq.
    ASSIGN ttEtq.id             = iUltId
           ttEtq.itCodigo       = ob-etiqueta.it-Codigo
           ttEtq.descricao      = IF AVAIL ITEM THEN ITEM.desc-item ELSE ''
           ttEtq.codRefer       = ob-etiqueta.cod-refer
           ttEtq.nrContainer    = ob-etiqueta.nr-container
           ttEtq.numRolo        = ob-etiqueta.num-rolo-imp
           ttEtq.idEtq          = string(ob-etiqueta.num-etiqueta)
           ttEtq.pedido         = IF AVAIL nota-fiscal THEN nota-fiscal.nr-nota-fis ELSE ''
           ttEtq.pedidoCliente  = IF AVAIL ped-item-rom THEN ped-item-rom.nr-pedcli ELSE ''
           ttEtq.prePedido      = IF AVAIL ped-item-rom THEN ped-item-rom.nr-pedcli ELSE '' 
           ttEtq.quantidade     = ob-etiqueta.quantidade * -1
           ttEtq.qtPeca         = -1
           ttEtq.codSituacao    = getDescrSit(ob-etiqueta.situacao)
           ttEtq.origem         = 'med'
           ttEtq.localiz        = ob-etiqueta.localiz
        .

END.





