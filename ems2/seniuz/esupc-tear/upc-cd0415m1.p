DEF NEW GLOBAL SHARED VAR wh-variante     AS WIDGET-HANDLE EXTENT 9 NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-cod-refer     AS WIDGET-HANDLE.
DEF NEW GLOBAL SHARED VAR h-query         AS HANDLE.

DEF BUFFER b-referencia FOR referencia.
DEF BUFFER b-referencia-ext FOR referencia-ext.

IF SELF:IMAGE = "image\im-copy" THEN DO.
   SELF:LOAD-IMAGE-UP("image\imt-chck1").
    
   ASSIGN wh-variante[1]:SENSITIVE = YES
          wh-variante[2]:SENSITIVE = YES
          wh-variante[3]:SENSITIVE = YES
          wh-variante[4]:SENSITIVE = YES
          wh-variante[5]:SENSITIVE = YES
          wh-variante[6]:SENSITIVE = YES
          wh-variante[7]:SENSITIVE = YES
          wh-variante[8]:SENSITIVE = YES
          wh-variante[9]:SENSITIVE = YES.
    
   APPLY 'entry' TO wh-variante[1].
END.
ELSE DO.
   FIND b-referencia WHERE 
        b-referencia.cod-refer = h-cod-refer:INPUT-VALUE NO-LOCK NO-ERROR.

   IF LENGTH(b-referencia.cod-refer) = 7 THEN DO.
      RUN pi-copia-ref (INPUT wh-variante[1]). 
      RUN pi-copia-ref (INPUT wh-variante[2]). 
      RUN pi-copia-ref (INPUT wh-variante[3]). 
      RUN pi-copia-ref (INPUT wh-variante[4]). 
      RUN pi-copia-ref (INPUT wh-variante[5]). 
      RUN pi-copia-ref (INPUT wh-variante[6]). 
      RUN pi-copia-ref (INPUT wh-variante[7]). 
      RUN pi-copia-ref (INPUT wh-variante[8]). 
      RUN pi-copia-ref (INPUT wh-variante[9]). 
   END.
   
   SELF:LOAD-IMAGE-UP("image\im-copy").

   ASSIGN wh-variante[1]:SENSITIVE = NO
          wh-variante[2]:SENSITIVE = NO
          wh-variante[3]:SENSITIVE = NO
          wh-variante[4]:SENSITIVE = NO
          wh-variante[5]:SENSITIVE = NO
          wh-variante[6]:SENSITIVE = NO
          wh-variante[7]:SENSITIVE = NO
          wh-variante[8]:SENSITIVE = NO
          wh-variante[9]:SENSITIVE = NO.

    IF VALID-HANDLE(h-query) THEN DO.
       RUN adm-open-query-cases IN h-query.
       RUN pi-reposiciona-query IN h-query (INPUT ROWID(referencia)).
    END.
END.

PROCEDURE pi-copia-ref.
    DEF INPUT PARAMETER p-variante AS HANDLE.
    DEF VAR c-var-ori AS CHAR.
    DEF VAR c-var-new AS CHAR.
    DEF VAR c-cod-refer LIKE referencia.cod-refer.

    ASSIGN c-var-new = p-variante:SCREEN-VALUE.
    IF c-var-new = '' THEN NEXT.

    ASSIGN c-cod-refer = b-referencia.cod-refer
           c-var-ori = "-" + SUBSTR(b-referencia.cod-refer,7,1).

    OVERLAY(c-cod-refer,7,1) = c-var-new.  /* Substitui a variante na referencia) */

    ASSIGN c-var-new = "-" + c-var-new.   /* acrescenta o "-" para a Descri‡Æo */

    FIND referencia WHERE
         referencia.cod-refer = c-cod-refer NO-LOCK NO-ERROR.
    IF NOT AVAIL referencia THEN DO.
       CREATE referencia.
       BUFFER-COPY b-referencia TO referencia
              ASSIGN referencia.cod-refer = c-cod-refer
                     referencia.descricao = IF INDEX(b-referencia.descricao,c-var-ori) > 0
                                            THEN REPLACE(b-referencia.descricao,c-var-ori,c-var-new)
                                            ELSE b-referencia.descricao.
    END.

    FIND referencia-ext WHERE
         referencia-ext.cod-refer = referencia.cod-refer NO-LOCK NO-ERROR.
    IF NOT AVAIL referencia-ext THEN DO.
       FIND b-referencia-ext WHERE
            b-referencia-ext.cod-refer = b-referencia.cod-refer NO-LOCK NO-ERROR.
       IF AVAIL b-referencia-ext THEN DO.
          CREATE referencia-ext.
          BUFFER-COPY b-referencia-ext TO referencia-ext
                      ASSIGN referencia-ext.cod-refer = referencia.cod-refer.
       END.
    END.

    ASSIGN p-variante:SCREEN-VALUE = "".  /* Limpa conteudo da Tela */

END PROCEDURE.

