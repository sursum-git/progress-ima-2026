USING Progress.Lang.*.
USING Progress.Windows.Form.

CLASS Menu11 INHERITS Form   : 

    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
	DEFINE PRIVATE VARIABLE windowContainer1 AS Progress.Windows.WindowContainer NO-UNDO.

    DEFINE PRIVATE VARIABLE txtBusca     AS System.Windows.Forms.TextBox NO-UNDO.
    DEFINE PRIVATE VARIABLE btBuscar     AS System.Windows.Forms.Button NO-UNDO.
    DEFINE PRIVATE VARIABLE btTarefas    AS System.Windows.Forms.Button NO-UNDO.
    DEFINE PRIVATE VARIABLE btRelatorios AS System.Windows.Forms.Button NO-UNDO.
    DEFINE PRIVATE VARIABLE btConsultas  AS System.Windows.Forms.Button NO-UNDO.
    DEFINE PRIVATE VARIABLE btCadastros  AS System.Windows.Forms.Button NO-UNDO.
    DEFINE PRIVATE VARIABLE btTrocarEmpresa     AS System.Windows.Forms.Button NO-UNDO.
    DEFINE PRIVATE VARIABLE btExecutarPrograma  AS System.Windows.Forms.Button NO-UNDO.

    DEFINE PRIVATE VARIABLE tree AS System.Windows.Forms.TreeView NO-UNDO.
    DEFINE PRIVATE VARIABLE node AS System.Windows.Forms.TreeNode NO-UNDO.

    /* Fonte de dados */
	DEFINE PRIVATE VARIABLE bindingSource1 AS Progress.Data.BindingSource NO-UNDO.
    /* Query */
    DEFINE PRIVATE VARIABLE hQuery AS HANDLE NO-UNDO.
    /* DataGrid (Equivalente ao browse no pogress) */
    DEFINE PRIVATE VARIABLE dataGridView1 AS System.Windows.Forms.DataGridView NO-UNDO.
    /* Colunas do DataGrid */
    DEFINE PRIVATE VARIABLE colNome AS System.Windows.Forms.DataGridViewTextBoxColumn NO-UNDO.
    DEFINE PRIVATE VARIABLE colExec AS System.Windows.Forms.DataGridViewTextBoxColumn NO-UNDO.

    DEFINE PRIVATE PROPERTY cGrupos   AS CHARACTER  NO-UNDO
        PRIVATE GET. PRIVATE SET.
    DEFINE PRIVATE PROPERTY nivelApp  AS INTEGER    NO-UNDO
        PRIVATE GET. PRIVATE SET.
    DEFINE PRIVATE PROPERTY globalVar AS HANDLE     NO-UNDO
        PRIVATE GET. PRIVATE SET.
    DEFINE PRIVATE PROPERTY moduloAtual   AS CHARACTER  NO-UNDO
        PRIVATE GET. PRIVATE SET.
    DEFINE PRIVATE PROPERTY qtTarefas    AS INTEGER NO-UNDO
        PRIVATE GET. PRIVATE SET.
    DEFINE PRIVATE PROPERTY qtRelatorios AS INTEGER NO-UNDO
        PRIVATE GET. PRIVATE SET.
    DEFINE PRIVATE PROPERTY qtConsultas  AS INTEGER NO-UNDO
        PRIVATE GET. PRIVATE SET.
    DEFINE PRIVATE PROPERTY qtCadastros  AS INTEGER NO-UNDO
        PRIVATE GET. PRIVATE SET.
    DEFINE PRIVATE TEMP-TABLE ttProgramas
        FIELD nom_prog_menu_verbal LIKE emsfnd.prog_dtsul.nom_prog_menu_verbal
        FIELD nom_prog_ext         LIKE emsfnd.prog_dtsul.nom_prog_ext
        FIELD idi_tip_prog_dtsul   LIKE emsfnd.prog_dtsul.idi_tip_prog_dtsul.

    CONSTRUCTOR PUBLIC Menu11 (  ):
        SUPER().
        RUN GlobalVar.p PERSISTENT SET globalVar.
        buscarGrupos().
        InitializeComponent().
        CREATE QUERY hQuery.
        hQuery:SET-BUFFERS(BUFFER ttProgramas:HANDLE).
        buscarProgramas().
        bindingSource1:HANDLE = hQuery.
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
        
	END CONSTRUCTOR.
    
    METHOD PRIVATE VOID InitializeComponent(  ):

        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc1 = NEW Progress.Data.TableDesc("ttProgramas").

        THIS-OBJECT:windowContainer1 = NEW Progress.Windows.WindowContainer().
        THIS-OBJECT:txtBusca     = NEW System.Windows.Forms.TextBox().
        THIS-OBJECT:btBuscar     = NEW System.Windows.Forms.Button().
        THIS-OBJECT:btTarefas    = NEW System.Windows.Forms.Button().
        THIS-OBJECT:btRelatorios = NEW System.Windows.Forms.Button().
        THIS-OBJECT:btConsultas  = NEW System.Windows.Forms.Button().
        THIS-OBJECT:btCadastros  = NEW System.Windows.Forms.Button().
        THIS-OBJECT:btTrocarEmpresa    = NEW System.Windows.Forms.Button().
        THIS-OBJECT:btExecutarPrograma = NEW System.Windows.Forms.Button().
        THIS-OBJECT:tree = NEW System.Windows.Forms.TreeView().    
        THIS-OBJECT:dataGridView1 = NEW System.Windows.Forms.DataGridView().
        THIS-OBJECT:colNome = NEW System.Windows.Forms.DataGridViewTextBoxColumn().
        THIS-OBJECT:colExec = NEW System.Windows.Forms.DataGridViewTextBoxColumn().
        THIS-OBJECT:bindingSource1 = NEW Progress.Data.BindingSource().
        CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:dataGridView1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        
        /* windowContainer1 */
        THIS-OBJECT:windowContainer1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:windowContainer1:Name = "windowContainer1".
        THIS-OBJECT:windowContainer1:Size = NEW System.Drawing.Size(800, 600).
        THIS-OBJECT:windowContainer1:TabIndex = 0.
        THIS-OBJECT:windowContainer1:TabStop = FALSE.

        /* txtBusca */
        THIS-OBJECT:txtBusca:Location = NEW System.Drawing.Point(270, 10).
        THIS-OBJECT:txtBusca:Size = NEW System.Drawing.Size(400, 20).
        THIS-OBJECT:txtBusca:KeyPress:SUBSCRIBE(txtBuscaKeyPress).

        /* btBuscar */
        THIS-OBJECT:btBuscar:Location = NEW System.Drawing.Point(675,10).
        THIS-OBJECT:btBuscar:Size = NEW System.Drawing.Size(80, 20).
        THIS-OBJECT:btBuscar:TEXT = "Buscar".
        THIS-OBJECT:btBuscar:Click:SUBSCRIBE(clickBtBuscar).
        
        /* btTarefas */
        THIS-OBJECT:btTarefas:Location = NEW System.Drawing.Point(270,30).
        THIS-OBJECT:btTarefas:Size = NEW System.Drawing.Size(100, 25).
        THIS-OBJECT:btTarefas:TEXT = "Tarefas (0)".
        THIS-OBJECT:btTarefas:Click:SUBSCRIBE(clickBtTarefas).

        /* btRelatorios */
        THIS-OBJECT:btRelatorios:Location = NEW System.Drawing.Point(375,30). 
        THIS-OBJECT:btRelatorios:Size = NEW System.Drawing.Size(100, 25).     
        THIS-OBJECT:btRelatorios:TEXT = "Relatorios (0)".
        THIS-OBJECT:btRelatorios:Click:SUBSCRIBE(clickBtRelatorios).

        /* btConsultas  */
        THIS-OBJECT:btConsultas:Location = NEW System.Drawing.Point(480,30).
        THIS-OBJECT:btConsultas:Size = NEW System.Drawing.Size(100, 25).    
        THIS-OBJECT:btConsultas:TEXT = "Consultas (0)".                               
        THIS-OBJECT:btConsultas:Click:SUBSCRIBE(clickBtConsultas).

        /* btCadastros */
        THIS-OBJECT:btCadastros:Location = NEW System.Drawing.Point(585,30).
        THIS-OBJECT:btCadastros:Size = NEW System.Drawing.Size(100, 25).    
        THIS-OBJECT:btCadastros:TEXT = "Cadastros (0)".
        THIS-OBJECT:btCadastros:Click:SUBSCRIBE(clickBtCadastros).
        
        /* btTrocarEmpresa */
        THIS-OBJECT:btTrocarEmpresa:Location = NEW System.Drawing.Point(580,565).
        THIS-OBJECT:btTrocarEmpresa:Size = NEW System.Drawing.Size(100, 25).    
        THIS-OBJECT:btTrocarEmpresa:TEXT = "Trocar Empresa".                     
        THIS-OBJECT:btTrocarEmpresa:Click:SUBSCRIBE(trocarEmpresa).          
                    
        /* btExecutarPrograma */
        THIS-OBJECT:btExecutarPrograma:Location = NEW System.Drawing.Point(690,565).
        THIS-OBJECT:btExecutarPrograma:Size = NEW System.Drawing.Size(100, 25).    
        THIS-OBJECT:btExecutarPrograma:TEXT = "Executar Prog".
        THIS-OBJECT:btExecutarPrograma:Click:SUBSCRIBE(executarPrograma).          


        /* Tree */
        THIS-OBJECT:tree:Location = NEW System.Drawing.Point(10, 10).
        THIS-OBJECT:tree:Name = "dtIni".
        THIS-OBJECT:tree:Size = NEW System.Drawing.Size(250, 580).
        THIS-OBJECT:tree:TabIndex = 1.
        THIS-OBJECT:tree:BeginUpdate().
        THIS-OBJECT:tree:Nodes:Clear().
        
        tableDesc1:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar0 AS Progress.Data.ColumnPropDesc EXTENT 2 NO-UNDO.
        arrayvar0[1] = NEW Progress.Data.ColumnPropDesc("nom_prog_menu_verbal", "Programa", Progress.Data.DataType:CHARACTER).
        arrayvar0[2] = NEW Progress.Data.ColumnPropDesc("nom_prog_ext", "Exec", Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar0.
        THIS-OBJECT:bindingSource1:TableSchema = tableDesc1.
        
        /* dataGridView1 */
        THIS-OBJECT:dataGridView1:Location = NEW System.Drawing.Point(270, 60).
        THIS-OBJECT:dataGridView1:Size = NEW System.Drawing.Size(520, 500).
        THIS-OBJECT:dataGridView1:AutoGenerateColumns = FALSE.
        THIS-OBJECT:dataGridView1:ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode:AutoSize.
        THIS-OBJECT:dataGridView1:RowHeadersVisible = FALSE.
        THIS-OBJECT:dataGridView1:AllowUserToResizeColumns = FALSE.
        THIS-OBJECT:dataGridView1:AllowUserToResizeRows = FALSE.
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar1 AS System.Windows.Forms.DataGridViewColumn EXTENT 2 NO-UNDO.
        arrayvar1[1] = THIS-OBJECT:colNome.
        arrayvar1[2] = THIS-OBJECT:colExec.
        THIS-OBJECT:dataGridView1:Columns:AddRange(arrayvar1).
        THIS-OBJECT:dataGridView1:DataSource = THIS-OBJECT:bindingSource1.
        THIS-OBJECT:dataGridView1:Name = "gridProgramas".
        
        
        /* colNome */
        THIS-OBJECT:colNome:DataPropertyName = "nom_prog_menu_verbal".
        THIS-OBJECT:colNome:HeaderText = "Programa".
        THIS-OBJECT:colNome:Name = "colNome".
        THIS-OBJECT:colNome:ReadOnly = TRUE.
        THIS-OBJECT:colNome:WIDTH = 500.
        
        /* colExec */
        THIS-OBJECT:colExec:DataPropertyName = "nom_prog_ext".
        THIS-OBJECT:colExec:HeaderText = "Exec".
        THIS-OBJECT:colExec:Name = "colExec".
        THIS-OBJECT:colExec:ReadOnly = TRUE.
        THIS-OBJECT:colExec:VISIBLE = FALSE.
        THIS-OBJECT:dataGridView1:CellDoubleClick:SUBSCRIBE(THIS-OBJECT:clickRow).
        
        ASSIGN nivelApp  = 0.

        FOR EACH emsfnd.aplicat_dtsul NO-LOCK ,
            FIRST emsfnd.aplicat_dtsul_segur NO-LOCK WHERE
            aplicat_dtsul_segur.cod_aplicat_dtsul = aplicat_dtsul.cod_aplicat_dtsul AND
            LOOKUP(aplicat_dtsul_segur.cod_grp_usuar,cgrupos) > 0
            BY aplicat_dtsul.des_aplicat_dtsul:
            
            THIS-OBJECT:tree:Nodes:ADD(NEW System.Windows.Forms.TreeNode(aplicat_dtsul.des_aplicat_dtsul)).
            
            FOR EACH emsfnd.sist_dtsul NO-LOCK WHERE
                sist_dtsul.cod_aplicat_dtsul = aplicat_dtsul.cod_aplicat_dtsul,
                EACH emsfnd.modul_dtsul NO-LOCK WHERE
                modul_dtsul.cod_sist_dtsul = sist_dtsul.cod_sist_dtsul
                BY modul_dtsul.des_modul_dtsul:

                node = NEW System.Windows.Forms.TreeNode(modul_dtsul.des_modul_dtsul).
                node:tag = STRING(ROWID(modul_dtsul)).
				/*node:NodeMouseClick:SUBSCRIBE(THIS-OBJECT:click).*/
                THIS-OBJECT:tree:Nodes[nivelApp]:Nodes:ADD(node) NO-ERROR.
                
            END.
            nivelApp = nivelApp + 1.
        END.
        THIS-OBJECT:tree:EndUpdate().
        THIS-OBJECT:tree:AfterSelect:SUBSCRIBE(THIS-OBJECT:clickNode).

        /* Window */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(800, 600).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tree).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:dataGridView1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:txtBusca).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btBuscar).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btTarefas).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btRelatorios).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btConsultas). 
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btCadastros).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btTrocarEmpresa).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btExecutarPrograma).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:windowContainer1).
        THIS-OBJECT:Name = "wMenu".
        THIS-OBJECT:Text = "Menu Totvs 11".
        CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:dataGridView1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:Resize:SUBSCRIBE(THIS-OBJECT:redimensionar).
        THIS-OBJECT:ResumeLayout(FALSE).

    END METHOD.
    
    METHOD PRIVATE VOID txtBuscaKeyPress (sender AS System.Object, e AS System.EventArgs):
        /* Se pressionar a tecla enter, executa a rotina do botao Buscar */
        IF CAST(e,System.Windows.Forms.KeyPressEventArgs):KeyChar  = CHR(13) THEN
            clickBtBuscar(sender,e).
    END METHOD.

    METHOD PRIVATE VOID clickBtBuscar (sender AS System.Object, e AS System.EventArgs):
        buscarProgramas(txtBusca:TEXT).
    END METHOD.
    METHOD PRIVATE VOID clickBtConsultas (sender AS System.Object, e AS System.EventArgs):
        setQuery(1).
    END METHOD.
    METHOD PRIVATE VOID clickBtCadastros (sender AS System.Object, e AS System.EventArgs):
        setQuery(2).
    END METHOD.
    METHOD PRIVATE VOID clickBtRelatorios (sender AS System.Object, e AS System.EventArgs):
        setQuery(3).
    END METHOD.
    METHOD PRIVATE VOID clickBtTarefas (sender AS System.Object, e AS System.EventArgs):
        setQuery(4).
    END METHOD.

    METHOD PRIVATE VOID modoProgramas (iTipo AS INTE):
        ASSIGN
            btConsultas:ENABLED = NOT iTipo = 1
            btCadastros:ENABLED = NOT iTipo = 2
            btRelatorios:ENABLED = NOT iTipo = 3
            btTarefas:ENABLED = NOT iTipo = 4.
    END METHOD.

    METHOD PRIVATE VOID buscarGrupos():
        FOR EACH emsfnd.usuar_grp_usuar NO-LOCK WHERE
            usuar_grp_usuar.cod_usuario = getUser().
            ASSIGN cGrupos = cGrupos + usuar_grp_usuar.cod_grp_usuar + ','.
        END.
    END METHOD.

    METHOD PRIVATE VOID clickNode (sender AS System.Object, e AS System.EventArgs):
        DEFINE VARIABLE strRowidModulo AS CHARACTER       NO-UNDO.
        strRowidModulo = CAST(sender,System.Windows.Forms.TreeView):SelectedNode:Tag.
        
        moduloAtual = "".
        IF strRowidModulo = ? THEN RETURN.
		FOR FIRST emsfnd.modul_dtsul NO-LOCK WHERE
			ROWID(modul_dtsul) = TO-ROWID(strRowidModulo):
            moduloAtual = modul_dtsul.cod_modul_dtsul.
        END.

        buscarProgramas().
        
    END METHOD.

    METHOD PRIVATE VOID clickRow (sender AS System.Object, e AS System.EventArgs):
        RUN VALUE(CAST(sender,System.Windows.Forms.DataGridView):CurrentRow:Cells["colExec"]:VALUE:ToString()).
    END METHOD.

    METHOD PRIVATE CHAR getUser ():
        RETURN DYNAMIC-FUNCTION("getGlobalVar" IN globalVar,"c-seg-usuario").
    END METHOD.

    METHOD PRIVATE CHAR getNomUsuar ():
        FOR FIRST emsfnd.usuar_mestre NO-LOCK WHERE
            emsfnd.usuar_mestre.cod_usuar = getUser().
            RETURN emsfnd.usuar_mestre.nom_usuar.
        END.
    END METHOD.

    METHOD PRIVATE VOID setQuery (tipoPrograma AS INTE):
        
        hQuery:QUERY-PREPARE(
            SUBSTITUTE("FOR EACH ttProgramas WHERE"
                       + "    ttProgramas.idi_tip_prog_dtsul = &1 "
                       + "    BY ttProgramas.nom_prog_menu_verbal",tipoPrograma)).
        hQuery:QUERY-OPEN ().
        modoProgramas(tipoPrograma).

    END METHOD.


    /* Buscar programas do modulo atual */
    METHOD PRIVATE VOID buscarProgramas ():

        zerarTipos().

        EMPTY TEMP-TABLE ttProgramas.
        
        FOR EACH emsfnd.procedimento FIELDS(cod_modul_dtsul cod_proced) NO-LOCK WHERE
            procedimento.cod_modul_dtsul = moduloAtual,
            EACH emsfnd.prog_dtsul FIELDS (cod_prog_dtsul cod_proced log_visualiz_menu 
                                           nom_prog_menu_verbal idi_tip_prog_dtsul nom_prog_ext) 
            NO-LOCK WHERE
            emsfnd.prog_dtsul.cod_proced = procedimento.cod_proced AND
            emsfnd.prog_dtsul.log_visualiz_menu AND
            emsfnd.prog_dtsul.nom_prog_menu_verbal <> '',
            FIRST emsfnd.prog_dtsul_segur FIELDS(cod_prog_dtsul cod_grp_usuar) NO-LOCK WHERE
            prog_dtsul_segur.cod_prog_dtsul = emsfnd.prog_dtsul.cod_prog_dtsul AND
            LOOKUP(prog_dtsul_segur.cod_grp_usuar,cGrupos) > 0 :
            
            CREATE ttProgramas.
            BUFFER-COPY emsfnd.prog_dtsul TO ttProgramas.
            contarTipos(ttProgramas.idi_tip_prog_dtsul).

        END.
        setTextButtons().
        setQuery(2).

    END METHOD.

    /* Buscar programas que contenham a string a ser buscada */
    METHOD PRIVATE VOID buscarProgramas (strBuscar AS CHAR):

        zerarTipos().

        strBuscar = REPLACE(TRIM(strBuscar),' ','*').

        EMPTY TEMP-TABLE ttProgramas.

        FOR EACH emsfnd.prog_dtsul FIELDS (cod_prog_dtsul cod_proced log_visualiz_menu 
                                           nom_prog_menu_verbal idi_tip_prog_dtsul nom_prog_ext) 
            NO-LOCK WHERE
            emsfnd.prog_dtsul.log_visualiz_menu AND
            (emsfnd.prog_dtsul.nom_prog_menu_verbal MATCHES '*' + strBuscar + '*' OR
             emsfnd.prog_dtsul.nom_prog_ext MATCHES '*' + strBuscar + '*'),
            FIRST emsfnd.prog_dtsul_segur FIELDS(cod_prog_dtsul cod_grp_usuar) NO-LOCK WHERE
            prog_dtsul_segur.cod_prog_dtsul = emsfnd.prog_dtsul.cod_prog_dtsul AND
            LOOKUP(prog_dtsul_segur.cod_grp_usuar,cGrupos) > 0:

            CREATE ttProgramas.
            BUFFER-COPY emsfnd.prog_dtsul TO ttProgramas.
            contarTipos(ttProgramas.idi_tip_prog_dtsul).
            
        END.
        setTextButtons().
        setQuery(2).
        
    END METHOD.
    
    METHOD PRIVATE VOID trocarEmpresa (sender AS System.Object, e AS System.EventArgs):
        RUN btb/btb962ad.r ('').
    END METHOD.

    METHOD PRIVATE VOID executarPrograma (sender AS System.Object, e AS System.EventArgs):
        RUN men/men702dc.r.
    END METHOD.

    METHOD PRIVATE VOID redimensionar (sender AS System.Object, e AS System.EventArgs):
        ASSIGN
            THIS-OBJECT:dataGridView1:Size = NEW System.Drawing.Size(THIS-OBJECT:ClientSize:width - 280, THIS-OBJECT:ClientSize:HEIGHT - 100)
            THIS-OBJECT:tree:HEIGHT = THIS-OBJECT:ClientSize:HEIGHT - 20
            THIS-OBJECT:btTrocarEmpresa:Location = NEW System.Drawing.Point(THIS-OBJECT:ClientSize:WIDTH - 220,THIS-OBJECT:ClientSize:HEIGHT - 35)
            THIS-OBJECT:btExecutarPrograma:Location = NEW System.Drawing.Point(THIS-OBJECT:ClientSize:WIDTH - 110,THIS-OBJECT:ClientSize:HEIGHT - 35)
            THIS-OBJECT:colNome:Width = THIS-OBJECT:dataGridView1:WIDTH - 20.
    END METHOD.

    METHOD PRIVATE VOID zerarTipos ():
        ASSIGN  qtConsultas  = 0    qtCadastros  = 0
                qtRelatorios = 0    qtTarefas    = 0.
    END METHOD.

    METHOD PRIVATE VOID contarTipos (tipo AS INTEGER):
        CASE ttProgramas.idi_tip_prog_dtsul:
            WHEN 1 THEN qtConsultas  = qtConsultas  + 1. 
            WHEN 2 THEN qtCadastros  = qtCadastros  + 1. 
            WHEN 3 THEN qtRelatorios = qtRelatorios + 1. 
            WHEN 4 THEN qtTarefas    = qtTarefas    + 1. 
        END CASE.
    END METHOD.

    METHOD PRIVATE VOID setTextButtons ():
        ASSIGN
            btConsultas :TEXT = SUBSTITUTE("Consultas (&1)", qtConsultas)
            btCadastros :TEXT = SUBSTITUTE("Cadastros (&1)", qtCadastros) 
            btRelatorios:TEXT = SUBSTITUTE("Relatorios (&1)", qtRelatorios)
            btTarefas   :TEXT = SUBSTITUTE("Tarefas (&1)", qtTarefas).
    END METHOD.


    DESTRUCTOR PUBLIC Menu11 ( ):

		IF VALID-OBJECT(components) THEN 
			CAST(components, System.IDisposable):Dispose().

		IF VALID-HANDLE(globalVar) THEN
            DELETE PROCEDURE globalVar.

	END DESTRUCTOR.


END CLASS.


