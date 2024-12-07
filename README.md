# Drug Screen Analyzer

The Drug Screen Analyzer tool uses the GR Metrics Library [Biocondutor GR Metrics](https://www.bioconductor.org/packages/release/bioc/html/GRmetrics.html). This tool expects the file to be in Case A format. I have attached an example in the data folder.

**Note - The column is called agent not treatment. In case you download the example from [GR Calculator](http://www.grcalculator.org/grcalculator/)**

### GR Metrics and GR Values Output Files

You can select the input file and output directory. Then select the checkbox for the file you want. Click the button and files will be generated in the output directory.
![Screenshot](https://github.com/TDeepanshPandey/Drug_Screen_Analyzer/blob/main/screenshot/generate_excel_files.png)

### Graphs

You can select the "Generate Graph" tab and it will automatically show the agents name. You can select multiple checkboxes and select option "Generate Individual Agent Graphs" to generate graphs per agent or "Generate Combined Agent Graph" to make one graph for selected agents.
![Screenshot](https://github.com/TDeepanshPandey/Drug_Screen_Analyzer/blob/main/screenshot/generate_graphs.png)

### Display GR Values

The agents will be automatically loaded. You can select the interesting column name by default the value is select to - GR50, GR_AOC, AUC. You can also select Copy to clipboard option to easily paste it to Excel.
![Screenshot](https://github.com/TDeepanshPandey/Drug_Screen_Analyzer/blob/main/screenshot/display_gr_values.png)
