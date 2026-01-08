
/*
**
*/

//
// RowErrors - Used to generate Strongly Typed DataSet xsd file
//


namespace StrongTypesNS
{
    using System;
    using System.Data;


    public class RowErrorsDS
    {

        static void Main (string[] args)
        {
            DataSet ds = new DataSet();
            DataRelation drel;
            DataColumn[] parentCols = null, childCols = null;
            DataColumn[] keyCols = null;

            ds.DataSetName = "RowErrors" + "DataSet";
            ds.Namespace = "RowErrors" + "NS";

            
	    DataTable RowErrors = ds.Tables.Add("RowErrors");
	    RowErrors.Columns.Add("ErrorSequence", typeof(int));
	    RowErrors.Columns.Add("ErrorNumber", typeof(int));
	    RowErrors.Columns.Add("ErrorDescription", typeof(string));
	    RowErrors.Columns.Add("ErrorParameters", typeof(string));
	    RowErrors.Columns.Add("ErrorType", typeof(string));
	    RowErrors.Columns.Add("ErrorHelp", typeof(string));
	    RowErrors.Columns.Add("ErrorSubType", typeof(string));


            ds.WriteXmlSchema("RowErrors.xsd");

        }


    }
}
