
/*
**
*/

//
// RowObjectAux2 - Used to generate Strongly Typed DataSet xsd file
//


namespace StrongTypesNS
{
    using System;
    using System.Data;


    public class RowObjectAux2DS
    {

        static void Main (string[] args)
        {
            DataSet ds = new DataSet();
            DataRelation drel;
            DataColumn[] parentCols = null, childCols = null;
            DataColumn[] keyCols = null;

            ds.DataSetName = "RowObjectAux2" + "DataSet";
            ds.Namespace = "RowObjectAux2" + "NS";

            
	    DataTable RowObjectAux2 = ds.Tables.Add("RowObjectAux2");
	    RowObjectAux2.Columns.Add("r-rowid", typeof(byte[]));
	    RowObjectAux2.Columns.Add("cgc", typeof(string));
	    RowObjectAux2.Columns.Add("cod-emitente", typeof(int));
	    RowObjectAux2.Columns.Add("cod-gr-forn", typeof(int));
	    RowObjectAux2.Columns.Add("identific", typeof(int));
	    RowObjectAux2.Columns.Add("natureza", typeof(int));
	    RowObjectAux2.Columns.Add("nome-abrev", typeof(string));
	    RowObjectAux2.Columns.Add("nome-emit", typeof(string));
	    RowObjectAux2.Columns.Add("nome-matriz", typeof(string));


            ds.WriteXmlSchema("RowObjectAux2.xsd");

        }


    }
}
