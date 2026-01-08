DEFINE VARIABLE repeat_loop AS INTEGER.
DEFINE VARIABLE animation_loop AS INTEGER.
DEFINE VARIABLE ok AS LOGICAL.
DEFINE BUTTON animate LABEL "Animate".
DEFINE IMAGE trashcan FILE "image/ii-cancel.bmp".
DISPLAY trashcan WITH FRAME y TITLE "**  Animation Sample  **".
/*
ON CHOOSE OF animate IN FRAME y DO:
 /* Begin Animation */
  DO repeat_loop = 1 TO 5:
     DO animation_loop = 1 TO 14:
        ok = trashcan:LOAD-IMAGE("ANI" + STRING(animation_loop,"99"))
             IN FRAME y.
     END.
  END.
END.
UPDATE animate WITH FRAME y.
*/
