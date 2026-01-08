DEF VAR de-peso-nf      LIKE mp-entr-cam.peso-nf.
DEF VAR de-peso-liquido LIKE mp-entr-cam.peso-liquido.
DEF VAR da-dt-entrada   LIKE mp-entr-cam.dt-entrada.
DEF VAR de-peso-tara    LIKE mp-entr-cam.peso-tara.
DEF VAR de-peso-bruto   LIKE mp-entr-cam.peso-bruto.
DEF VAR c-placa         LIKE mp-entr-cam.placa.

REPEAT:
  CLEAR ALL.
  UPDATE da-dt-entrada
         c-placa.
  
  FIND mp-entr-cam WHERE 
       mp-entr-cam.dt-entrada = da-dt-entrada  AND
       mp-entr-cam.placa      = c-placa NO-ERROR.
    
  IF NOT AVAIL mp-entr-cam THEN DO:
     MESSAGE "NÆo Achei o Veiculo ! ! "
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     UNDO,RETRY.
  END.
    
  ASSIGN de-peso-liquido = mp-entr-cam.peso-liquido
         de-peso-nf      = mp-entr-cam.peso-nf
         de-peso-bruto   = mp-entr-cam.peso-bruto
         de-peso-tara    = mp-entr-cam.peso-tara.
    

  FIND mp-entr-mat WHERE
       mp-entr-mat.nr-cdr = mp-entr-cam.nr-cdr NO-LOCK.

  DISP mp-entr-mat.nro-docto
       mp-entr-mat.cod-emit
       mp-entr-cam.peso-bruto
       mp-entr-cam.peso-tara
       mp-entr-cam.peso-liquido
       mp-entr-cam.peso-nf
       mp-entr-cam.peso-tara
       WITH SIDE-LABEL 1 COLUMN.


  UPDATE de-peso-liquido
         de-peso-nf
         de-peso-bruto
         de-peso-tara.

  ASSIGN mp-entr-cam.peso-liquido = de-peso-liquido
         mp-entr-cam.peso-nf      = de-peso-nf
         mp-entr-cam.peso-bruto   = de-peso-bruto
         mp-entr-cam.peso-tara    = de-peso-tara.

END.
