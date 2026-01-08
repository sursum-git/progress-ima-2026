DEFINE VARIABLE txt1 AS WIDGET-HANDLE.

CREATE TEXT txt1
       ASSIGN FORMAT = "x(12)"
               WIDTH = 13
        SCREEN-VALUE = "{2}"
                 ROW = {1}:ROW IN FRAME f-relat + 0.2
              COLUMN = {1}:COL IN FRAME f-relat + 1.64
              HEIGHT = 0.55
               FRAME = FRAME f-relat:HANDLE
             VISIBLE = TRUE. 
