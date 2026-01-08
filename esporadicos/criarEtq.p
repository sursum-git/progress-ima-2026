DEFINE VARIABLE iTcodigo    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE codRefer    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE nrContainer AS INTEGER     NO-UNDO.
DEFINE VARIABLE numrolo     AS INTEGER     NO-UNDO.
DEFINE VARIABLE qt          AS DECIMAL     NO-UNDO.


REPEAT:
UPDATE itCodigo codrefer nrContainer numrolo qt.
CREATE ob-etiqueta.
ASSIGN ob-etiqueta.cod-estabel     = '505'
       ob-etiqueta.dt-emissao      = TODAY
       ob-etiqueta.hr-emissao      = STRING(TIME,"HH:MM")
       ob-etiqueta.acondic         = ""
       ob-etiqueta.it-codigo       = itCodigo
       ob-etiqueta.cod-refer       = codRefer
       ob-etiqueta.nr-lote         = 'CA'
       ob-etiqueta.cod-qualid      = 'D' 
       ob-etiqueta.corte-comerc    = ''
       ob-etiqueta.quantidade      = qt
       ob-etiqueta.localizacao     = ''
       ob-etiqueta.situacao        = 3
       ob-etiqueta.cod-depos       = 'arm'.

 ASSIGN ob-etiqueta.num-etiqueta = NEXT-VALUE(seq-etq-estoq-itj).

 ASSIGN ob-etiqueta.nr-container    = nrContainer
        ob-etiqueta.num-rolo-imp    = numrolo
        ob-etiqueta.ob-origem       = ''.
    DISP ob-etiqueta.num-etiqueta.


END.
