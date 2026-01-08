DEFINE VARIABLE iPed AS INTEGER     NO-UNDO.
PROMPT-FOR iPed WITH SIDE-LABELS CENTERED.
FOR EACH peds_web WHERE peds_web.ped_web_id = INPUT iPed:
    UPDATE peds_web
    EXCEPT comentario descr_rejeicao
    WITH 1 COL CENTERED WIDTH 600.
END.
