 /*------------------------------------------------------------------------
    File        : inicio
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : sursum_corda
    Created     : Tue Sep 26 19:08:09 BRT 2017
    Notes       : 
  ----------------------------------------------------------------------*/
USING Progress.Windows.Form.
    
ROUTINE-LEVEL ON ERROR UNDO, THROW.
CLASS inicio INHERITS Form : 
    DEFINE PRIVATE VARIABLE bindingSource1 AS Progress.Data.BindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE childFormNumber AS INTEGER INITIAL 0 NO-UNDO.
    
    DEFINE PRIVATE VARIABLE openFileDialog AS System.Windows.Forms.OpenFileDialog NO-UNDO. 
    DEFINE PRIVATE VARIABLE radGridView1 AS Telerik.WinControls.UI.RadGridView NO-UNDO.
    DEFINE PRIVATE VARIABLE saveFileDialog AS System.Windows.Forms.SaveFileDialog NO-UNDO. 
    DEFINE PRIVATE VARIABLE menuStrip AS System.Windows.Forms.MenuStrip NO-UNDO.
    DEFINE PRIVATE VARIABLE splitContainer1 AS System.Windows.Forms.SplitContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE toolStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
    DEFINE PRIVATE VARIABLE statusStrip AS System.Windows.Forms.StatusStrip NO-UNDO.
    DEFINE PRIVATE VARIABLE toolStripButton1 AS System.Windows.Forms.ToolStripButton NO-UNDO.
    DEFINE PRIVATE VARIABLE toolStripSeparator1 AS System.Windows.Forms.ToolStripSeparator NO-UNDO.
    DEFINE PRIVATE VARIABLE toolStripSeparator2 AS System.Windows.Forms.ToolStripSeparator NO-UNDO.
    DEFINE PRIVATE VARIABLE toolStripSeparator3 AS System.Windows.Forms.ToolStripSeparator NO-UNDO.
    DEFINE PRIVATE VARIABLE toolStripSeparator4 AS System.Windows.Forms.ToolStripSeparator NO-UNDO.
    DEFINE PRIVATE VARIABLE toolStripSeparator5 AS System.Windows.Forms.ToolStripSeparator NO-UNDO.
    DEFINE PRIVATE VARIABLE toolStripSeparator6 AS System.Windows.Forms.ToolStripSeparator NO-UNDO.
    
    DEFINE PRIVATE VARIABLE printSetupToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE toolStripSeparator7 AS System.Windows.Forms.ToolStripSeparator NO-UNDO.
    DEFINE PRIVATE VARIABLE toolStripSeparator8 AS System.Windows.Forms.ToolStripSeparator NO-UNDO.
    DEFINE PRIVATE VARIABLE toolStripStatusLabel AS System.Windows.Forms.ToolStripStatusLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE aboutToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE tileHorizontalToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE fileMenu AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE newToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE openToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE saveToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE saveAsToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE printToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE printPreviewToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE exitToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE editMenu AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    
    DEFINE PRIVATE VARIABLE undoToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE redoToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE cutToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE copyToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE pasteToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE selectAllToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE viewMenu AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE toolBarToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE statusBarToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE toolsMenu AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE optionsToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE windowsMenu AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    
    DEFINE PRIVATE VARIABLE newWindowToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE cascadeToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE tileVerticalToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE closeAllToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE arrangeIconsToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE helpMenu AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE contentsToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE indexToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE searchToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    
    DEFINE PRIVATE VARIABLE newToolStripButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
    DEFINE PRIVATE VARIABLE openToolStripButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
    DEFINE PRIVATE VARIABLE saveToolStripButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
    DEFINE PRIVATE VARIABLE printToolStripButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
    DEFINE PRIVATE VARIABLE printPreviewToolStripButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
    DEFINE PRIVATE VARIABLE helpToolStripButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
    DEFINE PRIVATE VARIABLE toolTip AS System.Windows.Forms.ToolTip NO-UNDO.

    
    CONSTRUCTOR PUBLIC inicio( ):
    
        SUPER().
        InitializeComponent ( ).
        THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END CONSTRUCTOR.    

 

    @VisualDesigner.
    METHOD PRIVATE VOID ShowNewForm(sender AS System.Object, e AS System.EventArgs):
    
        /* Create a NEW instance of the child form. */
        DEFINE VARIABLE childForm AS Progress.Windows.Form NO-UNDO. 
        childForm = NEW Progress.Windows.Form( ).
        /* Make it a child of this MDI form before showing it. */
        childForm:MdiParent = THIS-OBJECT.
        childFormNumber = childFormNumber + 1.
        childForm:Text = "Window " + STRING(childFormNumber).
        childForm:FormClosed:SUBSCRIBE(Form_Closed).
        childForm:Show( ).
    END.
    
    
    @VisualDesigner.
    METHOD PRIVATE VOID OpenFile_Ok(sender AS System.Object, e AS System.ComponentModel.CancelEventArgs):
        DEFINE VARIABLE fileName AS CHARACTER NO-UNDO.
        fileName = openFileDialog:FileName.
        /* TODO: Add code here to open the file. */
    END.    

    @VisualDesigner.
    METHOD PRIVATE VOID OpenFile(sender AS System.Object, e AS System.EventArgs):
    
        openFileDialog = NEW System.Windows.Forms.OpenFileDialog( ).
        DEFINE VARIABLE x AS CHARACTER NO-UNDO.
        x = System.Environment:GetFolderPath(System.Environment+SpecialFolder:Personal).
        openFileDialog:InitialDirectory = System.Environment:GetFolderPath(System.Environment+SpecialFolder:Personal).
        openFileDialog:RestoreDirectory = TRUE.
        openFileDialog:Filter = "Text Files (*.txt)|*.txt|All Files (*.*)|*.*".
        openFileDialog:FileOk:Subscribe(OpenFile_Ok).
        WAIT-FOR openFileDialog:ShowDialog().
    END.

    @VisualDesigner.
    METHOD PRIVATE VOID SaveAsToolStripMenuItem_Click(sender AS System.Object, e AS System.EventArgs):
    
        saveFileDialog = NEW System.Windows.Forms.SaveFileDialog( ).
        saveFileDialog:InitialDirectory = System.Environment:GetFolderPath(System.Environment+SpecialFolder:Personal).
        saveFileDialog:RestoreDirectory = TRUE.
        saveFileDialog:Filter = "Text Files (*.txt)|*.txt|All Files (*.*)|*.*".
        saveFileDialog:FileOk:Subscribe(SaveAs_Ok).
        WAIT-FOR saveFileDialog:ShowDialog(THIS-OBJECT).

     END.

    @VisualDesigner.
    METHOD PRIVATE VOID SaveAs_Ok(o AS System.Object, e AS System.ComponentModel.CancelEventArgs):
        DEFINE VARIABLE fileName AS CHARACTER NO-UNDO.
        fileName = saveFileDialog:FileName.
        /* TODO: Add code here to save the current contents of the form to a file. */
    END.

    @VisualDesigner.
    METHOD PRIVATE VOID ExitToolsStripMenuItem_Click(sender AS System.Object, e AS System.EventArgs):
    
        THIS-OBJECT:Close().
    END.

    @VisualDesigner.
    METHOD PRIVATE VOID CutToolStripMenuItem_Click(sender AS System.Object, e AS System.EventArgs):
    
        /* TODO: Use System.Windows.Forms.Clipboard to insert the selected text or images into the clipboard */
    END.

    @VisualDesigner.
    METHOD PRIVATE VOID CopyToolStripMenuItem_Click(sender AS System.Object, e AS System.EventArgs):
    
        /* TODO: Use System.Windows.Forms.Clipboard to insert the selected text or images into the clipboard */
    END.

    @VisualDesigner.
    METHOD PRIVATE VOID PasteToolStripMenuItem_Click(sender AS System.Object, e AS System.EventArgs):
    
        /* TODO: Use System.Windows.Forms.Clipboard:GetText( ) or System.Windows.Forms:GetData to retrieve information from the clipboard. */
    END.

    @VisualDesigner.
    METHOD PRIVATE VOID ToolBarToolStripMenuItem_Click(sender AS System.Object, e AS System.EventArgs):
    
        toolStrip:Visible = toolBarToolStripMenuItem:Checked.
    END.

    @VisualDesigner.
    METHOD PRIVATE VOID StatusBarToolStripMenuItem_Click(sender AS System.Object, e AS System.EventArgs):
    
        statusStrip:Visible = statusBarToolStripMenuItem:Checked.
    END.

    @VisualDesigner.
    METHOD PRIVATE VOID CascadeToolStripMenuItem_Click(sender AS System.Object, e AS System.EventArgs):
    
        THIS-OBJECT:LayoutMdi(System.Windows.Forms.MdiLayout:Cascade).
    END.

    @VisualDesigner.
    METHOD PRIVATE VOID TileVerticleToolStripMenuItem_Click(sender AS System.Object, e AS System.EventArgs):
    
        THIS-OBJECT:LayoutMdi(System.Windows.Forms.MdiLayout:TileVertical).
    END.

    @VisualDesigner.
    METHOD PRIVATE VOID TileHorizontalToolStripMenuItem_Click(sender AS System.Object, e AS System.EventArgs):
    
        THIS-OBJECT:LayoutMdi(System.Windows.Forms.MdiLayout:TileHorizontal).
    END.

    @VisualDesigner.
    METHOD PRIVATE VOID ArrangeIconsToolStripMenuItem_Click(sender AS System.Object, e AS System.EventArgs):
    
        THIS-OBJECT:LayoutMdi(System.Windows.Forms.MdiLayout:ArrangeIcons).
    END.

    @VisualDesigner.
    METHOD PRIVATE VOID CloseAllToolStripMenuItem_Click(sender AS System.Object, e AS System.EventArgs):
    
        DEFINE VARIABLE formArray AS System.Array NO-UNDO.
        DEFINE VARIABLE formx AS System.Windows.Forms.Form NO-UNDO.
        formArray = THIS-OBJECT:MdiChildren.
        DEFINE VARIABLE icnt AS INTEGER NO-UNDO.
        DO icnt = 0 TO formArray:LENGTH - 1:
            formx = CAST( formArray:GetValue(icnt), System.Windows.Forms.Form ).
            formx:Close( ).
        END.
    END.

      
    METHOD PRIVATE VOID InitializeComponent ( ):

        /* NOTE: The following method is automatically generated.
           
           We strongly suggest that the contents of this method only be modified using the 
           Visual Designer to avoid any incompatible modifications.
           
           Modifying the contents of this method using a code editor will invalidate any support for this file. */


        THIS-OBJECT:components = NEW System.ComponentModel.Container().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("inicio").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc1 = NEW Progress.Data.TableDesc("param_comp").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE gridViewTextBoxColumn1 AS Telerik.WinControls.UI.GridViewTextBoxColumn NO-UNDO.
        gridViewTextBoxColumn1 = NEW Telerik.WinControls.UI.GridViewTextBoxColumn().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE gridViewTextBoxColumn2 AS Telerik.WinControls.UI.GridViewTextBoxColumn NO-UNDO.
        gridViewTextBoxColumn2 = NEW Telerik.WinControls.UI.GridViewTextBoxColumn().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE gridViewTextBoxColumn3 AS Telerik.WinControls.UI.GridViewTextBoxColumn NO-UNDO.
        gridViewTextBoxColumn3 = NEW Telerik.WinControls.UI.GridViewTextBoxColumn().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE gridViewTextBoxColumn4 AS Telerik.WinControls.UI.GridViewTextBoxColumn NO-UNDO.
        gridViewTextBoxColumn4 = NEW Telerik.WinControls.UI.GridViewTextBoxColumn().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE gridViewTextBoxColumn5 AS Telerik.WinControls.UI.GridViewTextBoxColumn NO-UNDO.
        gridViewTextBoxColumn5 = NEW Telerik.WinControls.UI.GridViewTextBoxColumn().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE gridViewCheckBoxColumn1 AS Telerik.WinControls.UI.GridViewCheckBoxColumn NO-UNDO.
        gridViewCheckBoxColumn1 = NEW Telerik.WinControls.UI.GridViewCheckBoxColumn().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE gridViewTextBoxColumn6 AS Telerik.WinControls.UI.GridViewTextBoxColumn NO-UNDO.
        gridViewTextBoxColumn6 = NEW Telerik.WinControls.UI.GridViewTextBoxColumn().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE tableViewDefinition1 AS Telerik.WinControls.UI.TableViewDefinition NO-UNDO.
        tableViewDefinition1 = NEW Telerik.WinControls.UI.TableViewDefinition().
        THIS-OBJECT:menuStrip = NEW System.Windows.Forms.MenuStrip().
        THIS-OBJECT:fileMenu = NEW System.Windows.Forms.ToolStripMenuItem().
        THIS-OBJECT:newToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
        THIS-OBJECT:openToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
        THIS-OBJECT:toolStripSeparator3 = NEW System.Windows.Forms.ToolStripSeparator().
        THIS-OBJECT:saveToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
        THIS-OBJECT:saveAsToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
        THIS-OBJECT:toolStripSeparator4 = NEW System.Windows.Forms.ToolStripSeparator().
        THIS-OBJECT:printToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
        THIS-OBJECT:printPreviewToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
        THIS-OBJECT:printSetupToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
        THIS-OBJECT:toolStripSeparator5 = NEW System.Windows.Forms.ToolStripSeparator().
        THIS-OBJECT:exitToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
        THIS-OBJECT:editMenu = NEW System.Windows.Forms.ToolStripMenuItem().
        THIS-OBJECT:undoToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
        THIS-OBJECT:redoToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
        THIS-OBJECT:toolStripSeparator6 = NEW System.Windows.Forms.ToolStripSeparator().
        THIS-OBJECT:cutToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
        THIS-OBJECT:copyToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
        THIS-OBJECT:pasteToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
        THIS-OBJECT:toolStripSeparator7 = NEW System.Windows.Forms.ToolStripSeparator().
        THIS-OBJECT:selectAllToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
        THIS-OBJECT:viewMenu = NEW System.Windows.Forms.ToolStripMenuItem().
        THIS-OBJECT:toolBarToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
        THIS-OBJECT:statusBarToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
        THIS-OBJECT:toolsMenu = NEW System.Windows.Forms.ToolStripMenuItem().
        THIS-OBJECT:optionsToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
        THIS-OBJECT:windowsMenu = NEW System.Windows.Forms.ToolStripMenuItem().
        THIS-OBJECT:newWindowToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
        THIS-OBJECT:cascadeToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
        THIS-OBJECT:tileVerticalToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
        THIS-OBJECT:tileHorizontalToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
        THIS-OBJECT:closeAllToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
        THIS-OBJECT:arrangeIconsToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
        THIS-OBJECT:helpMenu = NEW System.Windows.Forms.ToolStripMenuItem().
        THIS-OBJECT:contentsToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
        THIS-OBJECT:indexToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
        THIS-OBJECT:searchToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
        THIS-OBJECT:toolStripSeparator8 = NEW System.Windows.Forms.ToolStripSeparator().
        THIS-OBJECT:aboutToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
        THIS-OBJECT:toolStrip = NEW System.Windows.Forms.ToolStrip().
        THIS-OBJECT:newToolStripButton = NEW System.Windows.Forms.ToolStripButton().
        THIS-OBJECT:openToolStripButton = NEW System.Windows.Forms.ToolStripButton().
        THIS-OBJECT:saveToolStripButton = NEW System.Windows.Forms.ToolStripButton().
        THIS-OBJECT:toolStripSeparator1 = NEW System.Windows.Forms.ToolStripSeparator().
        THIS-OBJECT:printToolStripButton = NEW System.Windows.Forms.ToolStripButton().
        THIS-OBJECT:printPreviewToolStripButton = NEW System.Windows.Forms.ToolStripButton().
        THIS-OBJECT:toolStripSeparator2 = NEW System.Windows.Forms.ToolStripSeparator().
        THIS-OBJECT:helpToolStripButton = NEW System.Windows.Forms.ToolStripButton().
        THIS-OBJECT:toolStripButton1 = NEW System.Windows.Forms.ToolStripButton().
        THIS-OBJECT:statusStrip = NEW System.Windows.Forms.StatusStrip().
        THIS-OBJECT:toolStripStatusLabel = NEW System.Windows.Forms.ToolStripStatusLabel().
        THIS-OBJECT:toolTip = NEW System.Windows.Forms.ToolTip(THIS-OBJECT:components).
        THIS-OBJECT:splitContainer1 = NEW System.Windows.Forms.SplitContainer().
        THIS-OBJECT:bindingSource1 = NEW Progress.Data.BindingSource(THIS-OBJECT:components).
        THIS-OBJECT:radGridView1 = NEW Telerik.WinControls.UI.RadGridView().
        THIS-OBJECT:menuStrip:SuspendLayout().
        THIS-OBJECT:toolStrip:SuspendLayout().
        THIS-OBJECT:statusStrip:SuspendLayout().
        CAST(THIS-OBJECT:splitContainer1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:splitContainer1:Panel2:SuspendLayout().
        THIS-OBJECT:splitContainer1:SuspendLayout().
        CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:radGridView1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:radGridView1:MasterTemplate, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* menuStrip */
        /*  */
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar0 AS System.Windows.Forms.ToolStripItem EXTENT 6 NO-UNDO.
        arrayvar0[1] = THIS-OBJECT:fileMenu.
        arrayvar0[2] = THIS-OBJECT:editMenu.
        arrayvar0[3] = THIS-OBJECT:viewMenu.
        arrayvar0[4] = THIS-OBJECT:toolsMenu.
        arrayvar0[5] = THIS-OBJECT:windowsMenu.
        arrayvar0[6] = THIS-OBJECT:helpMenu.
        THIS-OBJECT:menuStrip:Items:AddRange(arrayvar0).
        THIS-OBJECT:menuStrip:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:menuStrip:MdiWindowListItem = THIS-OBJECT:windowsMenu.
        THIS-OBJECT:menuStrip:Name = "menuStrip".
        THIS-OBJECT:menuStrip:Size = NEW System.Drawing.Size(873, 24).
        THIS-OBJECT:menuStrip:TabIndex = 0.
        THIS-OBJECT:menuStrip:Text = "MenuStrip".
        /*  */
        /* fileMenu */
        /*  */
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar1 AS System.Windows.Forms.ToolStripItem EXTENT 11 NO-UNDO.
        arrayvar1[1] = THIS-OBJECT:newToolStripMenuItem.
        arrayvar1[2] = THIS-OBJECT:openToolStripMenuItem.
        arrayvar1[3] = THIS-OBJECT:toolStripSeparator3.
        arrayvar1[4] = THIS-OBJECT:saveToolStripMenuItem.
        arrayvar1[5] = THIS-OBJECT:saveAsToolStripMenuItem.
        arrayvar1[6] = THIS-OBJECT:toolStripSeparator4.
        arrayvar1[7] = THIS-OBJECT:printToolStripMenuItem.
        arrayvar1[8] = THIS-OBJECT:printPreviewToolStripMenuItem.
        arrayvar1[9] = THIS-OBJECT:printSetupToolStripMenuItem.
        arrayvar1[10] = THIS-OBJECT:toolStripSeparator5.
        arrayvar1[11] = THIS-OBJECT:exitToolStripMenuItem.
        THIS-OBJECT:fileMenu:DropDownItems:AddRange(arrayvar1).
        THIS-OBJECT:fileMenu:ImageTransparentColor = System.Drawing.SystemColors:ActiveBorder.
        THIS-OBJECT:fileMenu:Name = "fileMenu".
        THIS-OBJECT:fileMenu:Size = NEW System.Drawing.Size(37, 20).
        THIS-OBJECT:fileMenu:Text = "&File".
        /*  */
        /* newToolStripMenuItem */
        /*  */
        THIS-OBJECT:newToolStripMenuItem:Image = CAST(resources:GetObject("newToolStripMenuItem.Image"), System.Drawing.Image).
        THIS-OBJECT:newToolStripMenuItem:ImageTransparentColor = System.Drawing.Color:Black.
        THIS-OBJECT:newToolStripMenuItem:Name = "newToolStripMenuItem".
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.Keys NO-UNDO.
        nestedvar0 = System.Windows.Forms.Keys:Control.
        THIS-OBJECT:newToolStripMenuItem:ShortcutKeys = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.Keys:N), System.Windows.Forms.Keys).
        THIS-OBJECT:newToolStripMenuItem:Size = NEW System.Drawing.Size(146, 22).
        THIS-OBJECT:newToolStripMenuItem:Text = "&New".
        THIS-OBJECT:newToolStripMenuItem:Click:Subscribe(THIS-OBJECT:ShowNewForm).
        /*  */
        /* openToolStripMenuItem */
        /*  */
        THIS-OBJECT:openToolStripMenuItem:Image = CAST(resources:GetObject("openToolStripMenuItem.Image"), System.Drawing.Image).
        THIS-OBJECT:openToolStripMenuItem:ImageTransparentColor = System.Drawing.Color:Black.
        THIS-OBJECT:openToolStripMenuItem:Name = "openToolStripMenuItem".
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.Keys NO-UNDO.
        nestedvar1 = System.Windows.Forms.Keys:Control.
        THIS-OBJECT:openToolStripMenuItem:ShortcutKeys = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.Keys:O), System.Windows.Forms.Keys).
        THIS-OBJECT:openToolStripMenuItem:Size = NEW System.Drawing.Size(146, 22).
        THIS-OBJECT:openToolStripMenuItem:Text = "&Open".
        THIS-OBJECT:openToolStripMenuItem:Click:Subscribe(THIS-OBJECT:OpenFile).
        /*  */
        /* toolStripSeparator3 */
        /*  */
        THIS-OBJECT:toolStripSeparator3:Name = "toolStripSeparator3".
        THIS-OBJECT:toolStripSeparator3:Size = NEW System.Drawing.Size(143, 6).
        /*  */
        /* saveToolStripMenuItem */
        /*  */
        THIS-OBJECT:saveToolStripMenuItem:Image = CAST(resources:GetObject("saveToolStripMenuItem.Image"), System.Drawing.Image).
        THIS-OBJECT:saveToolStripMenuItem:ImageTransparentColor = System.Drawing.Color:Black.
        THIS-OBJECT:saveToolStripMenuItem:Name = "saveToolStripMenuItem".
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.Keys NO-UNDO.
        nestedvar2 = System.Windows.Forms.Keys:Control.
        THIS-OBJECT:saveToolStripMenuItem:ShortcutKeys = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.Keys:S), System.Windows.Forms.Keys).
        THIS-OBJECT:saveToolStripMenuItem:Size = NEW System.Drawing.Size(146, 22).
        THIS-OBJECT:saveToolStripMenuItem:Text = "&Save".
        /*  */
        /* saveAsToolStripMenuItem */
        /*  */
        THIS-OBJECT:saveAsToolStripMenuItem:Name = "saveAsToolStripMenuItem".
        THIS-OBJECT:saveAsToolStripMenuItem:Size = NEW System.Drawing.Size(146, 22).
        THIS-OBJECT:saveAsToolStripMenuItem:Text = "Save &As".
        THIS-OBJECT:saveAsToolStripMenuItem:Click:Subscribe(THIS-OBJECT:SaveAsToolStripMenuItem_Click).
        /*  */
        /* toolStripSeparator4 */
        /*  */
        THIS-OBJECT:toolStripSeparator4:Name = "toolStripSeparator4".
        THIS-OBJECT:toolStripSeparator4:Size = NEW System.Drawing.Size(143, 6).
        /*  */
        /* printToolStripMenuItem */
        /*  */
        THIS-OBJECT:printToolStripMenuItem:Image = CAST(resources:GetObject("printToolStripMenuItem.Image"), System.Drawing.Image).
        THIS-OBJECT:printToolStripMenuItem:ImageTransparentColor = System.Drawing.Color:Black.
        THIS-OBJECT:printToolStripMenuItem:Name = "printToolStripMenuItem".
        DEFINE VARIABLE nestedvar3 AS System.Windows.Forms.Keys NO-UNDO.
        nestedvar3 = System.Windows.Forms.Keys:Control.
        THIS-OBJECT:printToolStripMenuItem:ShortcutKeys = CAST(Progress.Util.EnumHelper:Or(nestedvar3, System.Windows.Forms.Keys:P), System.Windows.Forms.Keys).
        THIS-OBJECT:printToolStripMenuItem:Size = NEW System.Drawing.Size(146, 22).
        THIS-OBJECT:printToolStripMenuItem:Text = "&Print".
        /*  */
        /* printPreviewToolStripMenuItem */
        /*  */
        THIS-OBJECT:printPreviewToolStripMenuItem:Image = CAST(resources:GetObject("printPreviewToolStripMenuItem.Image"), System.Drawing.Image).
        THIS-OBJECT:printPreviewToolStripMenuItem:ImageTransparentColor = System.Drawing.Color:Black.
        THIS-OBJECT:printPreviewToolStripMenuItem:Name = "printPreviewToolStripMenuItem".
        THIS-OBJECT:printPreviewToolStripMenuItem:Size = NEW System.Drawing.Size(146, 22).
        THIS-OBJECT:printPreviewToolStripMenuItem:Text = "Print Pre&view".
        /*  */
        /* printSetupToolStripMenuItem */
        /*  */
        THIS-OBJECT:printSetupToolStripMenuItem:Name = "printSetupToolStripMenuItem".
        THIS-OBJECT:printSetupToolStripMenuItem:Size = NEW System.Drawing.Size(146, 22).
        THIS-OBJECT:printSetupToolStripMenuItem:Text = "Print Setup".
        /*  */
        /* toolStripSeparator5 */
        /*  */
        THIS-OBJECT:toolStripSeparator5:Name = "toolStripSeparator5".
        THIS-OBJECT:toolStripSeparator5:Size = NEW System.Drawing.Size(143, 6).
        /*  */
        /* exitToolStripMenuItem */
        /*  */
        THIS-OBJECT:exitToolStripMenuItem:Name = "exitToolStripMenuItem".
        THIS-OBJECT:exitToolStripMenuItem:Size = NEW System.Drawing.Size(146, 22).
        THIS-OBJECT:exitToolStripMenuItem:Text = "E&xit".
        THIS-OBJECT:exitToolStripMenuItem:Click:Subscribe(THIS-OBJECT:ExitToolsStripMenuItem_Click).
        /*  */
        /* editMenu */
        /*  */
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar2 AS System.Windows.Forms.ToolStripItem EXTENT 8 NO-UNDO.
        arrayvar2[1] = THIS-OBJECT:undoToolStripMenuItem.
        arrayvar2[2] = THIS-OBJECT:redoToolStripMenuItem.
        arrayvar2[3] = THIS-OBJECT:toolStripSeparator6.
        arrayvar2[4] = THIS-OBJECT:cutToolStripMenuItem.
        arrayvar2[5] = THIS-OBJECT:copyToolStripMenuItem.
        arrayvar2[6] = THIS-OBJECT:pasteToolStripMenuItem.
        arrayvar2[7] = THIS-OBJECT:toolStripSeparator7.
        arrayvar2[8] = THIS-OBJECT:selectAllToolStripMenuItem.
        THIS-OBJECT:editMenu:DropDownItems:AddRange(arrayvar2).
        THIS-OBJECT:editMenu:Name = "editMenu".
        THIS-OBJECT:editMenu:Size = NEW System.Drawing.Size(39, 20).
        THIS-OBJECT:editMenu:Text = "&Edit".
        /*  */
        /* undoToolStripMenuItem */
        /*  */
        THIS-OBJECT:undoToolStripMenuItem:Image = CAST(resources:GetObject("undoToolStripMenuItem.Image"), System.Drawing.Image).
        THIS-OBJECT:undoToolStripMenuItem:ImageTransparentColor = System.Drawing.Color:Black.
        THIS-OBJECT:undoToolStripMenuItem:Name = "undoToolStripMenuItem".
        DEFINE VARIABLE nestedvar4 AS System.Windows.Forms.Keys NO-UNDO.
        nestedvar4 = System.Windows.Forms.Keys:Control.
        THIS-OBJECT:undoToolStripMenuItem:ShortcutKeys = CAST(Progress.Util.EnumHelper:Or(nestedvar4, System.Windows.Forms.Keys:Z), System.Windows.Forms.Keys).
        THIS-OBJECT:undoToolStripMenuItem:Size = NEW System.Drawing.Size(164, 22).
        THIS-OBJECT:undoToolStripMenuItem:Text = "&Undo".
        /*  */
        /* redoToolStripMenuItem */
        /*  */
        THIS-OBJECT:redoToolStripMenuItem:Image = CAST(resources:GetObject("redoToolStripMenuItem.Image"), System.Drawing.Image).
        THIS-OBJECT:redoToolStripMenuItem:ImageTransparentColor = System.Drawing.Color:Black.
        THIS-OBJECT:redoToolStripMenuItem:Name = "redoToolStripMenuItem".
        DEFINE VARIABLE nestedvar5 AS System.Windows.Forms.Keys NO-UNDO.
        nestedvar5 = System.Windows.Forms.Keys:Control.
        THIS-OBJECT:redoToolStripMenuItem:ShortcutKeys = CAST(Progress.Util.EnumHelper:Or(nestedvar5, System.Windows.Forms.Keys:Y), System.Windows.Forms.Keys).
        THIS-OBJECT:redoToolStripMenuItem:Size = NEW System.Drawing.Size(164, 22).
        THIS-OBJECT:redoToolStripMenuItem:Text = "&Redo".
        /*  */
        /* toolStripSeparator6 */
        /*  */
        THIS-OBJECT:toolStripSeparator6:Name = "toolStripSeparator6".
        THIS-OBJECT:toolStripSeparator6:Size = NEW System.Drawing.Size(161, 6).
        /*  */
        /* cutToolStripMenuItem */
        /*  */
        THIS-OBJECT:cutToolStripMenuItem:Image = CAST(resources:GetObject("cutToolStripMenuItem.Image"), System.Drawing.Image).
        THIS-OBJECT:cutToolStripMenuItem:ImageTransparentColor = System.Drawing.Color:Black.
        THIS-OBJECT:cutToolStripMenuItem:Name = "cutToolStripMenuItem".
        DEFINE VARIABLE nestedvar6 AS System.Windows.Forms.Keys NO-UNDO.
        nestedvar6 = System.Windows.Forms.Keys:Control.
        THIS-OBJECT:cutToolStripMenuItem:ShortcutKeys = CAST(Progress.Util.EnumHelper:Or(nestedvar6, System.Windows.Forms.Keys:X), System.Windows.Forms.Keys).
        THIS-OBJECT:cutToolStripMenuItem:Size = NEW System.Drawing.Size(164, 22).
        THIS-OBJECT:cutToolStripMenuItem:Text = "Cu&t".
        THIS-OBJECT:cutToolStripMenuItem:Click:Subscribe(THIS-OBJECT:CutToolStripMenuItem_Click).
        /*  */
        /* copyToolStripMenuItem */
        /*  */
        THIS-OBJECT:copyToolStripMenuItem:Image = CAST(resources:GetObject("copyToolStripMenuItem.Image"), System.Drawing.Image).
        THIS-OBJECT:copyToolStripMenuItem:ImageTransparentColor = System.Drawing.Color:Black.
        THIS-OBJECT:copyToolStripMenuItem:Name = "copyToolStripMenuItem".
        DEFINE VARIABLE nestedvar7 AS System.Windows.Forms.Keys NO-UNDO.
        nestedvar7 = System.Windows.Forms.Keys:Control.
        THIS-OBJECT:copyToolStripMenuItem:ShortcutKeys = CAST(Progress.Util.EnumHelper:Or(nestedvar7, System.Windows.Forms.Keys:C), System.Windows.Forms.Keys).
        THIS-OBJECT:copyToolStripMenuItem:Size = NEW System.Drawing.Size(164, 22).
        THIS-OBJECT:copyToolStripMenuItem:Text = "&Copy".
        THIS-OBJECT:copyToolStripMenuItem:Click:Subscribe(THIS-OBJECT:CopyToolStripMenuItem_Click).
        /*  */
        /* pasteToolStripMenuItem */
        /*  */
        THIS-OBJECT:pasteToolStripMenuItem:Image = CAST(resources:GetObject("pasteToolStripMenuItem.Image"), System.Drawing.Image).
        THIS-OBJECT:pasteToolStripMenuItem:ImageTransparentColor = System.Drawing.Color:Black.
        THIS-OBJECT:pasteToolStripMenuItem:Name = "pasteToolStripMenuItem".
        DEFINE VARIABLE nestedvar8 AS System.Windows.Forms.Keys NO-UNDO.
        nestedvar8 = System.Windows.Forms.Keys:Control.
        THIS-OBJECT:pasteToolStripMenuItem:ShortcutKeys = CAST(Progress.Util.EnumHelper:Or(nestedvar8, System.Windows.Forms.Keys:V), System.Windows.Forms.Keys).
        THIS-OBJECT:pasteToolStripMenuItem:Size = NEW System.Drawing.Size(164, 22).
        THIS-OBJECT:pasteToolStripMenuItem:Text = "&Paste".
        THIS-OBJECT:pasteToolStripMenuItem:Click:Subscribe(THIS-OBJECT:PasteToolStripMenuItem_Click).
        /*  */
        /* toolStripSeparator7 */
        /*  */
        THIS-OBJECT:toolStripSeparator7:Name = "toolStripSeparator7".
        THIS-OBJECT:toolStripSeparator7:Size = NEW System.Drawing.Size(161, 6).
        /*  */
        /* selectAllToolStripMenuItem */
        /*  */
        THIS-OBJECT:selectAllToolStripMenuItem:Name = "selectAllToolStripMenuItem".
        DEFINE VARIABLE nestedvar9 AS System.Windows.Forms.Keys NO-UNDO.
        nestedvar9 = System.Windows.Forms.Keys:Control.
        THIS-OBJECT:selectAllToolStripMenuItem:ShortcutKeys = CAST(Progress.Util.EnumHelper:Or(nestedvar9, System.Windows.Forms.Keys:A), System.Windows.Forms.Keys).
        THIS-OBJECT:selectAllToolStripMenuItem:Size = NEW System.Drawing.Size(164, 22).
        THIS-OBJECT:selectAllToolStripMenuItem:Text = "Select &All".
        /*  */
        /* viewMenu */
        /*  */
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar3 AS System.Windows.Forms.ToolStripItem EXTENT 2 NO-UNDO.
        arrayvar3[1] = THIS-OBJECT:toolBarToolStripMenuItem.
        arrayvar3[2] = THIS-OBJECT:statusBarToolStripMenuItem.
        THIS-OBJECT:viewMenu:DropDownItems:AddRange(arrayvar3).
        THIS-OBJECT:viewMenu:Name = "viewMenu".
        THIS-OBJECT:viewMenu:Size = NEW System.Drawing.Size(44, 20).
        THIS-OBJECT:viewMenu:Text = "&View".
        /*  */
        /* toolBarToolStripMenuItem */
        /*  */
        THIS-OBJECT:toolBarToolStripMenuItem:Checked = TRUE.
        THIS-OBJECT:toolBarToolStripMenuItem:CheckOnClick = TRUE.
        THIS-OBJECT:toolBarToolStripMenuItem:CheckState = System.Windows.Forms.CheckState:Checked.
        THIS-OBJECT:toolBarToolStripMenuItem:Name = "toolBarToolStripMenuItem".
        THIS-OBJECT:toolBarToolStripMenuItem:Size = NEW System.Drawing.Size(126, 22).
        THIS-OBJECT:toolBarToolStripMenuItem:Text = "&Toolbar".
        THIS-OBJECT:toolBarToolStripMenuItem:Click:Subscribe(THIS-OBJECT:ToolBarToolStripMenuItem_Click).
        /*  */
        /* statusBarToolStripMenuItem */
        /*  */
        THIS-OBJECT:statusBarToolStripMenuItem:Checked = TRUE.
        THIS-OBJECT:statusBarToolStripMenuItem:CheckOnClick = TRUE.
        THIS-OBJECT:statusBarToolStripMenuItem:CheckState = System.Windows.Forms.CheckState:Checked.
        THIS-OBJECT:statusBarToolStripMenuItem:Name = "statusBarToolStripMenuItem".
        THIS-OBJECT:statusBarToolStripMenuItem:Size = NEW System.Drawing.Size(126, 22).
        THIS-OBJECT:statusBarToolStripMenuItem:Text = "&Status Bar".
        THIS-OBJECT:statusBarToolStripMenuItem:Click:Subscribe(THIS-OBJECT:StatusBarToolStripMenuItem_Click).
        /*  */
        /* toolsMenu */
        /*  */
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar4 AS System.Windows.Forms.ToolStripItem EXTENT 1 NO-UNDO.
        arrayvar4[1] = THIS-OBJECT:optionsToolStripMenuItem.
        THIS-OBJECT:toolsMenu:DropDownItems:AddRange(arrayvar4).
        THIS-OBJECT:toolsMenu:Name = "toolsMenu".
        THIS-OBJECT:toolsMenu:Size = NEW System.Drawing.Size(47, 20).
        THIS-OBJECT:toolsMenu:Text = "&Tools".
        /*  */
        /* optionsToolStripMenuItem */
        /*  */
        THIS-OBJECT:optionsToolStripMenuItem:Name = "optionsToolStripMenuItem".
        THIS-OBJECT:optionsToolStripMenuItem:Size = NEW System.Drawing.Size(116, 22).
        THIS-OBJECT:optionsToolStripMenuItem:Text = "&Options".
        /*  */
        /* windowsMenu */
        /*  */
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar5 AS System.Windows.Forms.ToolStripItem EXTENT 6 NO-UNDO.
        arrayvar5[1] = THIS-OBJECT:newWindowToolStripMenuItem.
        arrayvar5[2] = THIS-OBJECT:cascadeToolStripMenuItem.
        arrayvar5[3] = THIS-OBJECT:tileVerticalToolStripMenuItem.
        arrayvar5[4] = THIS-OBJECT:tileHorizontalToolStripMenuItem.
        arrayvar5[5] = THIS-OBJECT:closeAllToolStripMenuItem.
        arrayvar5[6] = THIS-OBJECT:arrangeIconsToolStripMenuItem.
        THIS-OBJECT:windowsMenu:DropDownItems:AddRange(arrayvar5).
        THIS-OBJECT:windowsMenu:Name = "windowsMenu".
        THIS-OBJECT:windowsMenu:Size = NEW System.Drawing.Size(68, 20).
        THIS-OBJECT:windowsMenu:Text = "&Windows".
        /*  */
        /* newWindowToolStripMenuItem */
        /*  */
        THIS-OBJECT:newWindowToolStripMenuItem:Name = "newWindowToolStripMenuItem".
        THIS-OBJECT:newWindowToolStripMenuItem:Size = NEW System.Drawing.Size(151, 22).
        THIS-OBJECT:newWindowToolStripMenuItem:Text = "&New Window".
        THIS-OBJECT:newWindowToolStripMenuItem:Click:Subscribe(THIS-OBJECT:ShowNewForm).
        /*  */
        /* cascadeToolStripMenuItem */
        /*  */
        THIS-OBJECT:cascadeToolStripMenuItem:Name = "cascadeToolStripMenuItem".
        THIS-OBJECT:cascadeToolStripMenuItem:Size = NEW System.Drawing.Size(151, 22).
        THIS-OBJECT:cascadeToolStripMenuItem:Text = "&Cascade".
        THIS-OBJECT:cascadeToolStripMenuItem:Click:Subscribe(THIS-OBJECT:CascadeToolStripMenuItem_Click).
        /*  */
        /* tileVerticalToolStripMenuItem */
        /*  */
        THIS-OBJECT:tileVerticalToolStripMenuItem:Name = "tileVerticalToolStripMenuItem".
        THIS-OBJECT:tileVerticalToolStripMenuItem:Size = NEW System.Drawing.Size(151, 22).
        THIS-OBJECT:tileVerticalToolStripMenuItem:Text = "Tile &Vertical".
        THIS-OBJECT:tileVerticalToolStripMenuItem:Click:Subscribe(THIS-OBJECT:TileVerticleToolStripMenuItem_Click).
        /*  */
        /* tileHorizontalToolStripMenuItem */
        /*  */
        THIS-OBJECT:tileHorizontalToolStripMenuItem:Name = "tileHorizontalToolStripMenuItem".
        THIS-OBJECT:tileHorizontalToolStripMenuItem:Size = NEW System.Drawing.Size(151, 22).
        THIS-OBJECT:tileHorizontalToolStripMenuItem:Text = "Tile &Horizontal".
        THIS-OBJECT:tileHorizontalToolStripMenuItem:Click:Subscribe(THIS-OBJECT:TileHorizontalToolStripMenuItem_Click).
        /*  */
        /* closeAllToolStripMenuItem */
        /*  */
        THIS-OBJECT:closeAllToolStripMenuItem:Name = "closeAllToolStripMenuItem".
        THIS-OBJECT:closeAllToolStripMenuItem:Size = NEW System.Drawing.Size(151, 22).
        THIS-OBJECT:closeAllToolStripMenuItem:Text = "C&lose All".
        THIS-OBJECT:closeAllToolStripMenuItem:Click:Subscribe(THIS-OBJECT:CloseAllToolStripMenuItem_Click).
        /*  */
        /* arrangeIconsToolStripMenuItem */
        /*  */
        THIS-OBJECT:arrangeIconsToolStripMenuItem:Name = "arrangeIconsToolStripMenuItem".
        THIS-OBJECT:arrangeIconsToolStripMenuItem:Size = NEW System.Drawing.Size(151, 22).
        THIS-OBJECT:arrangeIconsToolStripMenuItem:Text = "&Arrange Icons".
        THIS-OBJECT:arrangeIconsToolStripMenuItem:Click:Subscribe(THIS-OBJECT:ArrangeIconsToolStripMenuItem_Click).
        /*  */
        /* helpMenu */
        /*  */
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar6 AS System.Windows.Forms.ToolStripItem EXTENT 5 NO-UNDO.
        arrayvar6[1] = THIS-OBJECT:contentsToolStripMenuItem.
        arrayvar6[2] = THIS-OBJECT:indexToolStripMenuItem.
        arrayvar6[3] = THIS-OBJECT:searchToolStripMenuItem.
        arrayvar6[4] = THIS-OBJECT:toolStripSeparator8.
        arrayvar6[5] = THIS-OBJECT:aboutToolStripMenuItem.
        THIS-OBJECT:helpMenu:DropDownItems:AddRange(arrayvar6).
        THIS-OBJECT:helpMenu:Name = "helpMenu".
        THIS-OBJECT:helpMenu:Size = NEW System.Drawing.Size(44, 20).
        THIS-OBJECT:helpMenu:Text = "&Help".
        /*  */
        /* contentsToolStripMenuItem */
        /*  */
        THIS-OBJECT:contentsToolStripMenuItem:Name = "contentsToolStripMenuItem".
        DEFINE VARIABLE nestedvar10 AS System.Windows.Forms.Keys NO-UNDO.
        nestedvar10 = System.Windows.Forms.Keys:Control.
        THIS-OBJECT:contentsToolStripMenuItem:ShortcutKeys = CAST(Progress.Util.EnumHelper:Or(nestedvar10, System.Windows.Forms.Keys:F1), System.Windows.Forms.Keys).
        THIS-OBJECT:contentsToolStripMenuItem:Size = NEW System.Drawing.Size(168, 22).
        THIS-OBJECT:contentsToolStripMenuItem:Text = "&Contents".
        /*  */
        /* indexToolStripMenuItem */
        /*  */
        THIS-OBJECT:indexToolStripMenuItem:Image = CAST(resources:GetObject("indexToolStripMenuItem.Image"), System.Drawing.Image).
        THIS-OBJECT:indexToolStripMenuItem:ImageTransparentColor = System.Drawing.Color:Black.
        THIS-OBJECT:indexToolStripMenuItem:Name = "indexToolStripMenuItem".
        THIS-OBJECT:indexToolStripMenuItem:Size = NEW System.Drawing.Size(168, 22).
        THIS-OBJECT:indexToolStripMenuItem:Text = "&Index".
        /*  */
        /* searchToolStripMenuItem */
        /*  */
        THIS-OBJECT:searchToolStripMenuItem:Image = CAST(resources:GetObject("searchToolStripMenuItem.Image"), System.Drawing.Image).
        THIS-OBJECT:searchToolStripMenuItem:ImageTransparentColor = System.Drawing.Color:Black.
        THIS-OBJECT:searchToolStripMenuItem:Name = "searchToolStripMenuItem".
        THIS-OBJECT:searchToolStripMenuItem:Size = NEW System.Drawing.Size(168, 22).
        THIS-OBJECT:searchToolStripMenuItem:Text = "&Search".
        /*  */
        /* toolStripSeparator8 */
        /*  */
        THIS-OBJECT:toolStripSeparator8:Name = "toolStripSeparator8".
        THIS-OBJECT:toolStripSeparator8:Size = NEW System.Drawing.Size(165, 6).
        /*  */
        /* aboutToolStripMenuItem */
        /*  */
        THIS-OBJECT:aboutToolStripMenuItem:Name = "aboutToolStripMenuItem".
        THIS-OBJECT:aboutToolStripMenuItem:Size = NEW System.Drawing.Size(168, 22).
        THIS-OBJECT:aboutToolStripMenuItem:Text = "&About...".
        /*  */
        /* toolStrip */
        /*  */
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar7 AS System.Windows.Forms.ToolStripItem EXTENT 9 NO-UNDO.
        arrayvar7[1] = THIS-OBJECT:newToolStripButton.
        arrayvar7[2] = THIS-OBJECT:openToolStripButton.
        arrayvar7[3] = THIS-OBJECT:saveToolStripButton.
        arrayvar7[4] = THIS-OBJECT:toolStripSeparator1.
        arrayvar7[5] = THIS-OBJECT:printToolStripButton.
        arrayvar7[6] = THIS-OBJECT:printPreviewToolStripButton.
        arrayvar7[7] = THIS-OBJECT:toolStripSeparator2.
        arrayvar7[8] = THIS-OBJECT:helpToolStripButton.
        arrayvar7[9] = THIS-OBJECT:toolStripButton1.
        THIS-OBJECT:toolStrip:Items:AddRange(arrayvar7).
        THIS-OBJECT:toolStrip:Location = NEW System.Drawing.Point(0, 24).
        THIS-OBJECT:toolStrip:Name = "toolStrip".
        THIS-OBJECT:toolStrip:Size = NEW System.Drawing.Size(873, 25).
        THIS-OBJECT:toolStrip:TabIndex = 1.
        THIS-OBJECT:toolStrip:Text = "ToolStrip".
        /*  */
        /* newToolStripButton */
        /*  */
        THIS-OBJECT:newToolStripButton:DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle:Image.
        THIS-OBJECT:newToolStripButton:Image = CAST(resources:GetObject("newToolStripButton.Image"), System.Drawing.Image).
        THIS-OBJECT:newToolStripButton:ImageTransparentColor = System.Drawing.Color:Black.
        THIS-OBJECT:newToolStripButton:Name = "newToolStripButton".
        THIS-OBJECT:newToolStripButton:Size = NEW System.Drawing.Size(23, 22).
        THIS-OBJECT:newToolStripButton:Text = "New".
        THIS-OBJECT:newToolStripButton:Click:Subscribe(THIS-OBJECT:ShowNewForm).
        /*  */
        /* openToolStripButton */
        /*  */
        THIS-OBJECT:openToolStripButton:DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle:Image.
        THIS-OBJECT:openToolStripButton:Image = CAST(resources:GetObject("openToolStripButton.Image"), System.Drawing.Image).
        THIS-OBJECT:openToolStripButton:ImageTransparentColor = System.Drawing.Color:Black.
        THIS-OBJECT:openToolStripButton:Name = "openToolStripButton".
        THIS-OBJECT:openToolStripButton:Size = NEW System.Drawing.Size(23, 22).
        THIS-OBJECT:openToolStripButton:Text = "Open".
        THIS-OBJECT:openToolStripButton:Click:Subscribe(THIS-OBJECT:OpenFile).
        /*  */
        /* saveToolStripButton */
        /*  */
        THIS-OBJECT:saveToolStripButton:DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle:Image.
        THIS-OBJECT:saveToolStripButton:Image = CAST(resources:GetObject("saveToolStripButton.Image"), System.Drawing.Image).
        THIS-OBJECT:saveToolStripButton:ImageTransparentColor = System.Drawing.Color:Black.
        THIS-OBJECT:saveToolStripButton:Name = "saveToolStripButton".
        THIS-OBJECT:saveToolStripButton:Size = NEW System.Drawing.Size(23, 22).
        THIS-OBJECT:saveToolStripButton:Text = "Save".
        /*  */
        /* toolStripSeparator1 */
        /*  */
        THIS-OBJECT:toolStripSeparator1:Name = "toolStripSeparator1".
        THIS-OBJECT:toolStripSeparator1:Size = NEW System.Drawing.Size(6, 25).
        /*  */
        /* printToolStripButton */
        /*  */
        THIS-OBJECT:printToolStripButton:DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle:Image.
        THIS-OBJECT:printToolStripButton:Image = CAST(resources:GetObject("printToolStripButton.Image"), System.Drawing.Image).
        THIS-OBJECT:printToolStripButton:ImageTransparentColor = System.Drawing.Color:Black.
        THIS-OBJECT:printToolStripButton:Name = "printToolStripButton".
        THIS-OBJECT:printToolStripButton:Size = NEW System.Drawing.Size(23, 22).
        THIS-OBJECT:printToolStripButton:Text = "Print".
        /*  */
        /* printPreviewToolStripButton */
        /*  */
        THIS-OBJECT:printPreviewToolStripButton:DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle:Image.
        THIS-OBJECT:printPreviewToolStripButton:Image = CAST(resources:GetObject("printPreviewToolStripButton.Image"), System.Drawing.Image).
        THIS-OBJECT:printPreviewToolStripButton:ImageTransparentColor = System.Drawing.Color:Black.
        THIS-OBJECT:printPreviewToolStripButton:Name = "printPreviewToolStripButton".
        THIS-OBJECT:printPreviewToolStripButton:Size = NEW System.Drawing.Size(23, 22).
        THIS-OBJECT:printPreviewToolStripButton:Text = "Print Preview".
        /*  */
        /* toolStripSeparator2 */
        /*  */
        THIS-OBJECT:toolStripSeparator2:Name = "toolStripSeparator2".
        THIS-OBJECT:toolStripSeparator2:Size = NEW System.Drawing.Size(6, 25).
        /*  */
        /* helpToolStripButton */
        /*  */
        THIS-OBJECT:helpToolStripButton:DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle:Image.
        THIS-OBJECT:helpToolStripButton:Image = CAST(resources:GetObject("helpToolStripButton.Image"), System.Drawing.Image).
        THIS-OBJECT:helpToolStripButton:ImageTransparentColor = System.Drawing.Color:Black.
        THIS-OBJECT:helpToolStripButton:Name = "helpToolStripButton".
        THIS-OBJECT:helpToolStripButton:Size = NEW System.Drawing.Size(23, 22).
        THIS-OBJECT:helpToolStripButton:Text = "Help".
        /*  */
        /* toolStripButton1 */
        /*  */
        THIS-OBJECT:toolStripButton1:DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle:Image.
        THIS-OBJECT:toolStripButton1:Image = CAST(resources:GetObject("toolStripButton1.Image"), System.Drawing.Image).
        THIS-OBJECT:toolStripButton1:ImageTransparentColor = System.Drawing.Color:Magenta.
        THIS-OBJECT:toolStripButton1:Name = "toolStripButton1".
        THIS-OBJECT:toolStripButton1:Size = NEW System.Drawing.Size(23, 22).
        THIS-OBJECT:toolStripButton1:Text = "toolStripButton1".
        /*  */
        /* statusStrip */
        /*  */
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar8 AS System.Windows.Forms.ToolStripItem EXTENT 1 NO-UNDO.
        arrayvar8[1] = THIS-OBJECT:toolStripStatusLabel.
        THIS-OBJECT:statusStrip:Items:AddRange(arrayvar8).
        THIS-OBJECT:statusStrip:Location = NEW System.Drawing.Point(0, 431).
        THIS-OBJECT:statusStrip:Name = "statusStrip".
        THIS-OBJECT:statusStrip:Size = NEW System.Drawing.Size(873, 22).
        THIS-OBJECT:statusStrip:TabIndex = 2.
        THIS-OBJECT:statusStrip:Text = "StatusStrip".
        /*  */
        /* toolStripStatusLabel */
        /*  */
        THIS-OBJECT:toolStripStatusLabel:Name = "toolStripStatusLabel".
        THIS-OBJECT:toolStripStatusLabel:Size = NEW System.Drawing.Size(39, 17).
        THIS-OBJECT:toolStripStatusLabel:Text = "Status".
        /*  */
        /* splitContainer1 */
        /*  */
        THIS-OBJECT:splitContainer1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:splitContainer1:Location = NEW System.Drawing.Point(0, 49).
        THIS-OBJECT:splitContainer1:Name = "splitContainer1".
        /*  */
        /* splitContainer1.Panel2 */
        /*  */
        THIS-OBJECT:splitContainer1:Panel2:Controls:Add(THIS-OBJECT:radGridView1).
        THIS-OBJECT:splitContainer1:Size = NEW System.Drawing.Size(873, 382).
        THIS-OBJECT:splitContainer1:SplitterDistance = 167.
        THIS-OBJECT:splitContainer1:TabIndex = 4.
        /*  */
        /* bindingSource1 */
        /*  */
        THIS-OBJECT:bindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:bindingSource1:NoLOBs = FALSE.
        THIS-OBJECT:bindingSource1:Position = 0.
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar9 AS "Progress.Data.TableDesc[]" NO-UNDO.
        arrayvar9 = NEW "Progress.Data.TableDesc[]"(0).
        tableDesc1:ChildTables = arrayvar9.
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar10 AS Progress.Data.ColumnPropDesc EXTENT 7 NO-UNDO.
        arrayvar10[1] = NEW Progress.Data.ColumnPropDesc("arquivo_alias", "Selecione o arquivo Alias", Progress.Data.DataType:CHARACTER).
        arrayvar10[2] = NEW Progress.Data.ColumnPropDesc("arquivo_pf", "Selecione o arquivo PF", Progress.Data.DataType:CHARACTER).
        arrayvar10[3] = NEW Progress.Data.ColumnPropDesc("caminho_agent_ransack", "Caminho Agent Ransack", Progress.Data.DataType:CHARACTER).
        arrayvar10[4] = NEW Progress.Data.ColumnPropDesc("caminho_dir_comp", "?", Progress.Data.DataType:CHARACTER).
        arrayvar10[5] = NEW Progress.Data.ColumnPropDesc("caminho_progress", "?", Progress.Data.DataType:CHARACTER).
        arrayvar10[6] = NEW Progress.Data.ColumnPropDesc("log_conexao_fnd", "Conexão dos bancos pelo banco emsfnd?", Progress.Data.DataType:LOGICAL).
        arrayvar10[7] = NEW Progress.Data.ColumnPropDesc("serial", "Serial", Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar10.
        THIS-OBJECT:bindingSource1:TableSchema = tableDesc1.
        /*  */
        /* radGridView1 */
        /*  */
        THIS-OBJECT:radGridView1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:radGridView1:Location = NEW System.Drawing.Point(0, 0).
        /*  */
        /*  */
        /*  */
        gridViewTextBoxColumn1:FieldName = "arquivo_alias".
        gridViewTextBoxColumn1:HeaderText = "Selecione o arquivo Alias".
        gridViewTextBoxColumn1:IsAutoGenerated = TRUE.
        gridViewTextBoxColumn1:Name = "arquivo_alias".
        gridViewTextBoxColumn2:FieldName = "arquivo_pf".
        gridViewTextBoxColumn2:HeaderText = "Selecione o arquivo PF".
        gridViewTextBoxColumn2:IsAutoGenerated = TRUE.
        gridViewTextBoxColumn2:Name = "arquivo_pf".
        gridViewTextBoxColumn3:FieldName = "caminho_agent_ransack".
        gridViewTextBoxColumn3:HeaderText = "Caminho Agent Ransack".
        gridViewTextBoxColumn3:IsAutoGenerated = TRUE.
        gridViewTextBoxColumn3:Name = "caminho_agent_ransack".
        gridViewTextBoxColumn4:FieldName = "caminho_dir_comp".
        gridViewTextBoxColumn4:HeaderText = "?".
        gridViewTextBoxColumn4:IsAutoGenerated = TRUE.
        gridViewTextBoxColumn4:Name = "caminho_dir_comp".
        gridViewTextBoxColumn5:FieldName = "caminho_progress".
        gridViewTextBoxColumn5:HeaderText = "?".
        gridViewTextBoxColumn5:IsAutoGenerated = TRUE.
        gridViewTextBoxColumn5:Name = "caminho_progress".
        gridViewCheckBoxColumn1:FieldName = "log_conexao_fnd".
        gridViewCheckBoxColumn1:HeaderText = "Conexão dos bancos pelo banco emsfnd?".
        gridViewCheckBoxColumn1:IsAutoGenerated = TRUE.
        gridViewCheckBoxColumn1:Name = "log_conexao_fnd".
        gridViewTextBoxColumn6:FieldName = "serial".
        gridViewTextBoxColumn6:HeaderText = "Serial".
        gridViewTextBoxColumn6:IsAutoGenerated = TRUE.
        gridViewTextBoxColumn6:Name = "serial".
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar11 AS Telerik.WinControls.UI.GridViewDataColumn EXTENT 7 NO-UNDO.
        arrayvar11[1] = gridViewTextBoxColumn1.
        arrayvar11[2] = gridViewTextBoxColumn2.
        arrayvar11[3] = gridViewTextBoxColumn3.
        arrayvar11[4] = gridViewTextBoxColumn4.
        arrayvar11[5] = gridViewTextBoxColumn5.
        arrayvar11[6] = gridViewCheckBoxColumn1.
        arrayvar11[7] = gridViewTextBoxColumn6.
        THIS-OBJECT:radGridView1:MasterTemplate:Columns:AddRange(arrayvar11).
        THIS-OBJECT:radGridView1:MasterTemplate:DataSource = THIS-OBJECT:bindingSource1.
        THIS-OBJECT:radGridView1:MasterTemplate:ViewDefinition = tableViewDefinition1.
        THIS-OBJECT:radGridView1:Name = "radGridView1".
        THIS-OBJECT:radGridView1:Size = NEW System.Drawing.Size(702, 382).
        THIS-OBJECT:radGridView1:TabIndex = 0.
        THIS-OBJECT:radGridView1:Text = "radGridView1".
        THIS-OBJECT:radGridView1:ThemeName = "ControlDefault".
        /*  */
        /* inicio */
        /*  */
        THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
        THIS-OBJECT:AutoScaleMode = System.Windows.Forms.AutoScaleMode:Font.
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(873, 453).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:splitContainer1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:statusStrip).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:toolStrip).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:menuStrip).
        THIS-OBJECT:IsMdiContainer = TRUE.
        THIS-OBJECT:MainMenuStrip = THIS-OBJECT:menuStrip.
        THIS-OBJECT:Name = "inicio".
        THIS-OBJECT:Text = "inicio".
        THIS-OBJECT:FormClosed:Subscribe(THIS-OBJECT:Form_Closed).
        THIS-OBJECT:menuStrip:ResumeLayout(FALSE).
        THIS-OBJECT:menuStrip:PerformLayout().
        THIS-OBJECT:toolStrip:ResumeLayout(FALSE).
        THIS-OBJECT:toolStrip:PerformLayout().
        THIS-OBJECT:statusStrip:ResumeLayout(FALSE).
        THIS-OBJECT:statusStrip:PerformLayout().
        THIS-OBJECT:splitContainer1:Panel2:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:splitContainer1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:splitContainer1:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:radGridView1:MasterTemplate, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:radGridView1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    DESTRUCTOR PUBLIC inicio( ):
    
    END DESTRUCTOR.
    
    
    @VisualDesigner.
    METHOD PRIVATE VOID Form_Closed(sender AS System.Object, e AS System.EventArgs):
       
    END.     

END CLASS.
