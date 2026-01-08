/* menu.p */
def var vItemCost-width as decimal no-undo.

def var vstart-x        as decimal no-undo.
def var vctr            as integer no-undo.

def temp-table TT_menu
  FIELD MenuItem  AS CHARACTER
  FIELD ItemCost  AS DECIMAL.

CREATE TT_Menu.
ASSIGN MenuItem = "Carp"
       ItemCost = 12.20.

CREATE TT_Menu.
ASSIGN MenuItem = "Mackeral"
       ItemCost = 14.00.

CREATE TT_Menu.
ASSIGN MenuItem = "Bang Island Mussels"
       ItemCost = 3.75.

CREATE TT_Menu.
ASSIGN MenuItem = "New Zealand Rock Shrimp Tails"
       ItemCost = 54.00.

CREATE TT_Menu.
ASSIGN MenuItem = "Sole"
       ItemCost = 14.00.

{PDFinclude/pdf_inc.i "THIS-PROCEDURE"}


RUN pdf_new("Spdf","c:\temp\menu.pdf").

RUN pdf_new_page("Spdf").
RUN pdf_set_font("Spdf","Helvetica",10).

RUN pdf_set_dash("Spdf",1,2).
FOR EACH TT_menu.

  vctr = vctr + 1.
  IF vctr MODULO 2 = 0 THEN DO:
    RUN pdf_text_color ("Spdf",1.0,0.0,0.0).
    RUN pdf_stroke_color ("Spdf",1.0,0.0,0.0).
  END.
  ELSE DO:
    RUN pdf_text_color ("Spdf",0.0,0.0,0.0).
    RUN pdf_stroke_color ("Spdf",0.0,0.0,0.0).
  END.

  vItemCost-width = pdf_text_widthdec("Spdf",STRING(ItemCost,"Z9.99")).

  RUN pdf_text_at ("Spdf",MenuItem,10).
  vstart-x = pdf_TextX("Spdf").
  RUN pdf_text_to ("Spdf",STRING(ItemCost,">9.99"),100).

  RUN pdf_line_dec("Spdf",Vstart-X + 5,
                      pdf_TextY("Spdf"),
                      250,
                      pdf_TextY("Spdf"), 0.5).

  RUN pdf_skip("Spdf").
END.

RUN pdf_close("Spdf").

/* end of menu.p */

