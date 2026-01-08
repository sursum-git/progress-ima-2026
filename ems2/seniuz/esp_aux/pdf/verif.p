/* Auto-Mate PDF Statement Program */       

def var li-Line         as int  no-undo.
def var li-PageLength   as int  no-undo initial 35.
def var li-Page         as int  no-undo.
def var li-PageCount    as int  no-undo.
def var li-LineCount    as int  no-undo.
def var li-Time         as int  no-undo.
def var lc-FilenameIn   as char no-undo initial "c:\pdfinc\working\stattemplate.pdf". 
def var lc-FilenameOut  as char no-undo format "x(40)" INITIAL "c:\pdfinc\working\statout.pdf".
def var li-PageHeight   as dec  no-undo initial 842.
def var li-PageWidth    as dec  no-undo initial 595.
def var li-ImageHeight  as int no-undo.
def var li-ImageWidth   as int no-undo.
def var hDocstate       as handle no-undo.
def var hField          as handle extent 100.
def var hTmp            as handle.

etime(yes).

define temp-table TMP
field fld as char
field pos as char
index idx1 fld. 
function getpos returns integer ( input lc-Text as char, input lc-Field as char, input lc-XY as char) forward.

/*
/* Auto-Mate PDF Statement Program */       

def var li-Line         as int  no-undo.
def var li-PageLength   as int  no-undo initial 35.
def var li-Page         as int  no-undo.
def var li-PageCount    as int  no-undo.
def var li-LineCount    as int  no-undo.
def var li-Time         as int  no-undo.
def var lc-FilenameIn   as char no-undo initial "c:\pdfinc\working\stattemplate.pdf". 
def var lc-FilenameOut  as char no-undo format "x(40)" INITIAL "c:\pdfinc\working\statout.pdf".
def var li-PageHeight   as dec  no-undo initial 842.
def var li-PageWidth    as dec  no-undo initial 595.
def var li-ImageHeight  as int no-undo.
def var li-ImageWidth   as int no-undo.
def var hDocstate       as handle no-undo.
def var hField          as handle extent 100.
def var hTmp            as handle.

etime(yes).

define temp-table TMP
field fld as char
field pos as char
index idx1 fld. 
function getpos returns integer ( input lc-Text as char, input lc-Field as char, input lc-XY as char) forward.

{ pdf_inc.i PERSISTENT}
*/
run buildtmp.

find first dochead where dochead.sys-cono = 61 and 
                         dochead.gc-accmth = 200808 and
                         dochead.DocAccount = "611ADA01" /*"611ALS01" */
                         no-lock no-error.
               
find drsmas where drsmas.dm-accno = "611ABH01" no-lock no-error.

find sysctl where sysctl.sys-cono = 61.

run GetPages(output li-PageCount).

run pdf_new                 in h_PDFinc ("Spdf",lc-FileNameOut).   
run pdf_set_PaperType       in h_PDFinc ("Spdf","A4").
run pdf_set_Orientation     in h_PDFinc ("Spdf","Portrait").
run pdf_open_pdf            in h_PDFinc ("Spdf",lc-FileNameIn,"stat").
run pdf_load_image          in h_PDFinc ("Spdf","Logo","C:\pdfinc\jpg\DTSALogo.jpg").
li-ImageHeight = pdf_ImageDim("Spdf","Logo","Height").
li-ImageWidth = pdf_ImageDim("Spdf","Logo","Width").
pdf_PageHeader ("Spdf",this-procedure:handle,"PageHeader"). 
pdf_PageFooter ("Spdf",this-procedure:handle,"PageFooter"). 

/* Actual Statement begins here */
li-Page = 1.
li-LineCount = 0.
li-Line = 0.

run pdf_new_page            in h_PDFinc ("Spdf"). 
run pdf_use_pdf_page        in h_PDFinc ("Spdf","stat","1"). 

for each DocDetail where Docdetail.sys-cono = DocHead.sys-cono and
                         DocDetail.gc-accmth = DocHead.gc-accmth and
                         Docdetail.DocAccount = DocHead.DocAccount
                         no-lock:
    run pdf_set_font in h_PDFinc ("Spdf","Helvetica",7).
    run writetext(string(DocDetail.DocDetDate,"99/99/9999"),36,290 + li-line).
    run writetext(DocDetail.DocDetCode,86,290 + li-line).
    run writetext(DocDetail.DocDetDetail,110,290 + li-line).
    run writetext(DocDetail.DocDetReference,200,290 + li-line).
    run writetext(DocDetail.DocDetOrder,236,290 + li-line).
    run writetextR(string(DocDetail.DocDetAmount,">>>,>>>,>>9.99"),390,290 + li-line).
    run writetext(DocDetail.DocDetReference,410,290 + li-line).
    run writetextR(string(DocDetail.DocDetAmount,">>>,>>>,>>9.99"),535,290 + li-line).
    li-Line = li-Line + 10.
    li-LineCount = li-LineCount + 1.
    if li-LineCount >= 38 then
    do:
        li-LineCount = 0.
        li-Page = li-Page + 1.
        run pdf_new_page            in h_PDFinc ("Spdf"). 
        run pdf_use_pdf_page        in h_PDFinc ("Spdf","stat","1"). 
        li-Line = 0.
    end.
end.

run pdf_close in h_PDFinc ("Spdf").
/*message etime view-as alert-box. */             
/*run value("viewxmldialog.w") (lc-FileNameOut,"Auto-Mate Demo Statement"). */


procedure PageHeader:
    /* find first docstate where sys-cono = 61 no-lock no-error. */
    run pdf_place_image    in h_PDFinc ("Spdf","logo",200,150,li-ImageWidth,li-ImageHeight).

    run pdf_set_font in h_PDFinc ("Spdf","Helvetica-Bold",18    ). 
    run writetext1(sysctl.sys-name,"StConame").
                                                                
    run pdf_set_font in h_PDFinc ("Spdf","Helvetica",14). 
    run writetext1(sysctl.sys-add1,"StCoAddr1").
    run writetext(sysctl.sys-add2,35,100).
    run writetext(sysctl.sys-add3,35,115).
    run writetext(sysctl.sys-pstcode,35,130).
    
    run pdf_set_font in h_PDFinc ("Spdf","Helvetica",10). 
    run writetext("Tel: " + sysctl.sys-tel,35,150).
    run writetext("Fax: " + sysctl.sys-faxno,35,165).
    
    run pdf_set_font in h_PDFinc ("Spdf","Helvetica",8). 
    run writetext(drsmas.dm-name,38,198).
    run writetext(drsmas.dm-addr1,38,210).
    run writetext(drsmas.dm-addr2,38,222).
    run writetext(drsmas.dm-addr3,38,235).
    run writetext(drsmas.dm-pstcd,38,248).
    
    run pdf_set_font in h_PDFinc ("Spdf","Helvetica",8). 
    run writetext(sysctl.sys-name,410,125).
    run writetext(sysctl.sys-add1,410,135).
    run writetext(sysctl.sys-add2,410,145).
    run writetext(sysctl.sys-add3,410,155).
    run writetext(sysctl.sys-pstcode,410,165).
    
    run pdf_set_font in h_PDFinc ("Spdf","Helvetica",5). 
    run writetext(drsmas.dm-name,410,183).
    run writetext(drsmas.dm-addr1,410,190).
    run writetext(drsmas.dm-addr2,410,197).
    run writetext(drsmas.dm-addr3,410,204).
    run writetext(drsmas.dm-pstcd,410,210).
    
    run pdf_set_font in h_PDFinc ("Spdf","Helvetica",8). 
    run writetext(drsmas.dm-accno,340,221).
    run writetext(drsmas.dm-accno,480,221).
    run writetext(string(dochead.DocDate,"99/99/9999"),340,233).
    run writetext(string(dochead.DocDate,"99/99/9999"),480,233).
    run writetext(string(li-Page,">>9"),340,244).
    run writetext(string(li-Page,">>9"),480,244).
    
end procedure.

procedure PageFooter:
    run pdf_set_font in h_PDFinc ("Spdf","Helvetica",8).
    if li-Page = li-PageCount then
    do:
        run writetext(string(DocHead.Doc90Days + DocHead.Doc120Days,">>>,>>>,>>9.99"),60,692).
        run writetext(string(DocHead.Doc60Days,">>>,>>>,>>9.99"),145,692).
        run writetext(string(DocHead.Doc30Days,">>>,>>>,>>9.99"),215,692).
        run writetext(string(DocHead.DocCurrent,">>>,>>>,>>9.99"),285,692).
        run writetext(string(DocHead.DocTotBal,">>>,>>>,>>9.99"),347,692).
        run writetext(string(DocHead.DocTotBal,">>>,>>>,>>9.99"),488,692).
    end.
    run writetext("Bank: " + sysctl.sys-bankacc,110,718).
    run writetext("Branch: " + sysctl.sys-bank[1],110,728).
    run writetext("Bank Account: " + sysctl.sys-bank[2],110,736).
    
end procedure.

procedure WriteText:
    def input parameter lc-Text as char.
    def input parameter li-Column as int.
    def input parameter li-Row as int.
    run pdf_text_xy     in h_PDFinc ("Spdf",lc-Text,li-Column, li-PageHeight - li-Row).
end procedure.

procedure WriteText1:
    def input parameter lc-Text as char.
    def input parameter lc-Field as char.
    run pdf_text_xy     in h_PDFinc ("Spdf",lc-Text,getpos(lc-Text,lc-Field,"x"), li-PageHeight - getpos(lc-Text,lc-Field,"y")).
end procedure.

procedure WriteTextR:
    def input parameter lc-Text as char.
    def input parameter li-Column as int.
    def input parameter li-Row as int.
    run pdf_text_xy     in h_PDFinc ("Spdf",lc-Text,li-Column - pdf_text_width("Spdf",lc-Text), li-PageHeight - li-Row).
end procedure.

procedure GetPages:
    def output parameter li-PageCount   as int no-undo.
    def var              li-i           as int no-undo.
    li-PageCount = 1.
    for each DocDetail where Docdetail.sys-cono = DocHead.sys-cono and
                             DocDetail.gc-accmth = DocHead.gc-accmth and
                             Docdetail.DocAccount = DocHead.DocAccount
                             no-lock:
        li-i = li-i + 1.
        if li-i mod li-pageLength = 0 then li-PageCount = li-PageCount + 1.
    end.
    if li-i = 0 then li-PageCount = 0.
end.

procedure buildtmp:

    def var li-i as int.
    def var li-Fields as int.
    def var q1 as handle.
    create query q1.
    
    
    find _file where _file-name = "DocState".
    for each _field of _file:
        li-Fields = li-Fields + 1.
    end.
    hDocstate = buffer docstate:handle.
    create query q1.
    q1:set-buffers(hdocstate).
    q1:QUERY-PREPARE("for each DocState where sys-cono = 61 no-lock").
    q1:QUERY-OPEN.
    hTMP = buffer TMP:handle.
    repeat li-i = 1 to li-Fields:
        q1:get-first.
        hTMP:buffer-create.
        hField[li-i] = hDocstate:BUFFER-FIELD(li-i) no-error.
        if error-status:error then leave.
        TMP.fld = hfield[li-i]:name.
        TMP.pos = hfield[li-i]:buffer-value.
    end.
end.

function getpos returns integer:
    /*define input parameter lc-Text as char.
    define input parameter lc-Field as char no-undo.
    define input parameter lc-XY as char no-undo. */
    def var lc-Align as char.
    lc-Align = entry(3,TMP.pos) no-error.
    
    find TMP where TMP.fld = lc-Field no-error.
    if not available TMP then return 0.
    if TMP.pos = "" then return 0.
    if lc-Align = "R" then
    do:
        return integer(entry(1,TMP.pos)) - pdf_text_width("Spdf",lc-Text).
        if error-Status:error then return 0.
    end.
    if lc-XY = "X" then return integer(entry(1,TMP.pos)).
    if error-Status:error then return 0.
    if lc-XY = "Y" then return integer(entry(2,TMP.pos)).
    if error-Status:error then return 0.
end.
run buildtmp.

find first dochead where dochead.sys-cono = 61 and 
                         dochead.gc-accmth = 200808 and
                         dochead.DocAccount = "611ADA01" /*"611ALS01" */
                         no-lock no-error.
               
find drsmas where drsmas.dm-accno = "611ABH01" no-lock no-error.

find sysctl where sysctl.sys-cono = 61.

run GetPages(output li-PageCount).

run pdf_new                 in h_PDFinc ("Spdf",lc-FileNameOut).   
run pdf_set_PaperType       in h_PDFinc ("Spdf","A4").
run pdf_set_Orientation     in h_PDFinc ("Spdf","Portrait").
run pdf_open_pdf            in h_PDFinc ("Spdf",lc-FileNameIn,"stat").
run pdf_load_image          in h_PDFinc ("Spdf","Logo","C:\pdfinc\jpg\DTSALogo.jpg").
li-ImageHeight = pdf_ImageDim("Spdf","Logo","Height").
li-ImageWidth = pdf_ImageDim("Spdf","Logo","Width").
pdf_PageHeader ("Spdf",this-procedure:handle,"PageHeader"). 
pdf_PageFooter ("Spdf",this-procedure:handle,"PageFooter"). 

/* Actual Statement begins here */
li-Page = 1.
li-LineCount = 0.
li-Line = 0.

run pdf_new_page            in h_PDFinc ("Spdf"). 
run pdf_use_pdf_page        in h_PDFinc ("Spdf","stat","1"). 

for each DocDetail where Docdetail.sys-cono = DocHead.sys-cono and
                         DocDetail.gc-accmth = DocHead.gc-accmth and
                         Docdetail.DocAccount = DocHead.DocAccount
                         no-lock:
    run pdf_set_font in h_PDFinc ("Spdf","Helvetica",7).
    run writetext(string(DocDetail.DocDetDate,"99/99/9999"),36,290 + li-line).
    run writetext(DocDetail.DocDetCode,86,290 + li-line).
    run writetext(DocDetail.DocDetDetail,110,290 + li-line).
    run writetext(DocDetail.DocDetReference,200,290 + li-line).
    run writetext(DocDetail.DocDetOrder,236,290 + li-line).
    run writetextR(string(DocDetail.DocDetAmount,">>>,>>>,>>9.99"),390,290 + li-line).
    run writetext(DocDetail.DocDetReference,410,290 + li-line).
    run writetextR(string(DocDetail.DocDetAmount,">>>,>>>,>>9.99"),535,290 + li-line).
    li-Line = li-Line + 10.
    li-LineCount = li-LineCount + 1.
    if li-LineCount >= 38 then
    do:
        li-LineCount = 0.
        li-Page = li-Page + 1.
        run pdf_new_page            in h_PDFinc ("Spdf"). 
        run pdf_use_pdf_page        in h_PDFinc ("Spdf","stat","1"). 
        li-Line = 0.
    end.
end.

run pdf_close in h_PDFinc ("Spdf").
/*message etime view-as alert-box. */             
/*run value("viewxmldialog.w") (lc-FileNameOut,"Auto-Mate Demo Statement"). */


procedure PageHeader:
    /* find first docstate where sys-cono = 61 no-lock no-error. */
    run pdf_place_image    in h_PDFinc ("Spdf","logo",200,150,li-ImageWidth,li-ImageHeight).

    run pdf_set_font in h_PDFinc ("Spdf","Helvetica-Bold",18    ). 
    run writetext1(sysctl.sys-name,"StConame").
                                                                
    run pdf_set_font in h_PDFinc ("Spdf","Helvetica",14). 
    run writetext1(sysctl.sys-add1,"StCoAddr1").
    run writetext(sysctl.sys-add2,35,100).
    run writetext(sysctl.sys-add3,35,115).
    run writetext(sysctl.sys-pstcode,35,130).
    
    run pdf_set_font in h_PDFinc ("Spdf","Helvetica",10). 
    run writetext("Tel: " + sysctl.sys-tel,35,150).
    run writetext("Fax: " + sysctl.sys-faxno,35,165).
    
    run pdf_set_font in h_PDFinc ("Spdf","Helvetica",8). 
    run writetext(drsmas.dm-name,38,198).
    run writetext(drsmas.dm-addr1,38,210).
    run writetext(drsmas.dm-addr2,38,222).
    run writetext(drsmas.dm-addr3,38,235).
    run writetext(drsmas.dm-pstcd,38,248).
    
    run pdf_set_font in h_PDFinc ("Spdf","Helvetica",8). 
    run writetext(sysctl.sys-name,410,125).
    run writetext(sysctl.sys-add1,410,135).
    run writetext(sysctl.sys-add2,410,145).
    run writetext(sysctl.sys-add3,410,155).
    run writetext(sysctl.sys-pstcode,410,165).
    
    run pdf_set_font in h_PDFinc ("Spdf","Helvetica",5). 
    run writetext(drsmas.dm-name,410,183).
    run writetext(drsmas.dm-addr1,410,190).
    run writetext(drsmas.dm-addr2,410,197).
    run writetext(drsmas.dm-addr3,410,204).
    run writetext(drsmas.dm-pstcd,410,210).
    
    run pdf_set_font in h_PDFinc ("Spdf","Helvetica",8). 
    run writetext(drsmas.dm-accno,340,221).
    run writetext(drsmas.dm-accno,480,221).
    run writetext(string(dochead.DocDate,"99/99/9999"),340,233).
    run writetext(string(dochead.DocDate,"99/99/9999"),480,233).
    run writetext(string(li-Page,">>9"),340,244).
    run writetext(string(li-Page,">>9"),480,244).
    
end procedure.

procedure PageFooter:
    run pdf_set_font in h_PDFinc ("Spdf","Helvetica",8).
    if li-Page = li-PageCount then
    do:
        run writetext(string(DocHead.Doc90Days + DocHead.Doc120Days,">>>,>>>,>>9.99"),60,692).
        run writetext(string(DocHead.Doc60Days,">>>,>>>,>>9.99"),145,692).
        run writetext(string(DocHead.Doc30Days,">>>,>>>,>>9.99"),215,692).
        run writetext(string(DocHead.DocCurrent,">>>,>>>,>>9.99"),285,692).
        run writetext(string(DocHead.DocTotBal,">>>,>>>,>>9.99"),347,692).
        run writetext(string(DocHead.DocTotBal,">>>,>>>,>>9.99"),488,692).
    end.
    run writetext("Bank: " + sysctl.sys-bankacc,110,718).
    run writetext("Branch: " + sysctl.sys-bank[1],110,728).
    run writetext("Bank Account: " + sysctl.sys-bank[2],110,736).
    
end procedure.

procedure WriteText:
    def input parameter lc-Text as char.
    def input parameter li-Column as int.
    def input parameter li-Row as int.
    run pdf_text_xy     in h_PDFinc ("Spdf",lc-Text,li-Column, li-PageHeight - li-Row).
end procedure.

procedure WriteText1:
    def input parameter lc-Text as char.
    def input parameter lc-Field as char.
    run pdf_text_xy     in h_PDFinc ("Spdf",lc-Text,getpos(lc-Text,lc-Field,"x"), li-PageHeight - getpos(lc-Text,lc-Field,"y")).
end procedure.

procedure WriteTextR:
    def input parameter lc-Text as char.
    def input parameter li-Column as int.
    def input parameter li-Row as int.
    run pdf_text_xy     in h_PDFinc ("Spdf",lc-Text,li-Column - pdf_text_width("Spdf",lc-Text), li-PageHeight - li-Row).
end procedure.

procedure GetPages:
    def output parameter li-PageCount   as int no-undo.
    def var              li-i           as int no-undo.
    li-PageCount = 1.
    for each DocDetail where Docdetail.sys-cono = DocHead.sys-cono and
                             DocDetail.gc-accmth = DocHead.gc-accmth and
                             Docdetail.DocAccount = DocHead.DocAccount
                             no-lock:
        li-i = li-i + 1.
        if li-i mod li-pageLength = 0 then li-PageCount = li-PageCount + 1.
    end.
    if li-i = 0 then li-PageCount = 0.
end.

procedure buildtmp:

    def var li-i as int.
    def var li-Fields as int.
    def var q1 as handle.
    create query q1.
    
    
    find _file where _file-name = "DocState".
    for each _field of _file:
        li-Fields = li-Fields + 1.
    end.
    hDocstate = buffer docstate:handle.
    create query q1.
    q1:set-buffers(hdocstate).
    q1:QUERY-PREPARE("for each DocState where sys-cono = 61 no-lock").
    q1:QUERY-OPEN.
    hTMP = buffer TMP:handle.
    repeat li-i = 1 to li-Fields:
        q1:get-first.
        hTMP:buffer-create.
        hField[li-i] = hDocstate:BUFFER-FIELD(li-i) no-error.
        if error-status:error then leave.
        TMP.fld = hfield[li-i]:name.
        TMP.pos = hfield[li-i]:buffer-value.
    end.
end.

function getpos returns integer:
    /*define input parameter lc-Text as char.
    define input parameter lc-Field as char no-undo.
    define input parameter lc-XY as char no-undo. */
    def var lc-Align as char.
    lc-Align = entry(3,TMP.pos) no-error.
    
    find TMP where TMP.fld = lc-Field no-error.
    if not available TMP then return 0.
    if TMP.pos = "" then return 0.
    if lc-Align = "R" then
    do:
        return integer(entry(1,TMP.pos)) - pdf_text_width("Spdf",lc-Text).
        if error-Status:error then return 0.
    end.
    if lc-XY = "X" then return integer(entry(1,TMP.pos)).
    if error-Status:error then return 0.
    if lc-XY = "Y" then return integer(entry(2,TMP.pos)).
    if error-Status:error then return 0.
end.
