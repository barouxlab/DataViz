# DataViz

DataViz is an R-Shiny application that visualizes image segmentation data from [Imaris](https://imaris.oxinst.com/). 
Originally developed to analyse datasets from segmented 3D nuclei images, it can also be applied to other Imaris data with similar objects and statistics.
It can be used by anyone with a computer capable of running the [R language for Statistical Computing](https://www.r-project.org/about.html).

DataViz was conceived by [Dr. Célia Baroux](https://www.botinst.uzh.ch/en/research/development/celiabaroux.html) and developed by [Devin Routh](https://www.zi.uzh.ch/en/teaching-and-research/science-it.html) at the University of Zürich.

The development process of DataViz is ongoing, so check back again for updates.

## Application Summary

DataViz has two principal parts:
- a data cleaning, harmonization, and processing pathway;
- a graphical user interface for sub-selecting and visualizing Imaris segmented data;

DataViz can thus be used to clean, harmonize, subset/filter data as well as produce publication quality plots and graphics with state-of-the-art plotting packages.

### Data Cleaning
The data cleaning part uses a custom written R script that can accept either a Zip or CSV file input. The inputted files must include Imaris data, and both input formats require a specific structure/schema. See the example files to ascertain the required structure and schema.

### Graphical User Interface (GUI)
The graphical user interface allows a user to produce 1-D and 2-D plots (e.g., boxplots and scatterplots, respectively) from the image segmentation data. The GUI also guides users into data subsetting and filtering for custom analyses.

## Running the Application
The application can be run via the following two lines of code inputted from within an R session:
```
if(!"shiny" %in% rownames(installed.packages())) install.packages("shiny"); library(shiny)
runUrl("https://github.com/barouxlab/DataViz/archive/main.zip")
```
To run DataViz from downloaded files, clone the repository using:
```
git clone https://github.com/barouxlab/DataViz.git
```
Then run the following lines from an R session located in the directory that contains the repo:
```
if(!"shiny" %in% rownames(installed.packages())) install.packages("shiny"); library(shiny)
runApp("DataViz")
```
If you have any questions, please contact [Devin Routh](mailto:devin.routh@uzh.ch).

## File Notes
If interacting with the repository files, the following notes may be helpful:
- All `.R` extension files are necessary for the RShiny application to run. This includes:
    - global.R
    - server.R
    - ui.R
    - cleaningFunction.R
    - processingFunction.R
- "example_data_S3IT.zip" is an example ZIP formatted collection of Imaris exported tabular files; in order to use the Data Cleaning/Harmonization pipeline, ensure that your Imaris tabular data matches the directory format of this ZIP.
    - N.B. directory names are used by the data cleaning pipeline to create column names and metadata; ensure directories are named appropriately and, when at all possible, as they are outputted from Imaris.
- "exampleCSV.csv" is an example CSV formatted dataset that can be imported without the cleaning / harmonization step.

🌱
