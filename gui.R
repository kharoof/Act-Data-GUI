require(gWidgets)
require(tcltk2)
options("guiToolkit"="tcltk")


##############################################################################################################
## GUI.R ----
## 18/10/2015
## This program lets the user select a csv file to load and choose the variables to import and the types ----
##############################################################################################################
## The file is designed to create an environment to store the various overhead need to import the file
## This has not been designed to be especially efficient and is currently only function and is intended
## to be a proof of concept rather than a production program
## The first section will let the user pick the data to load
## The second section will show the loaded data variables and their current
## formats etc.

## Structure ----
## The purpose of this GUI is to let the user
## 1. Specify a file to load
## 2. Let user choose the variable types and edit names if desired (this is not working yet)
## 3. Load the data to a R data structure (data frame or data table, at the moment this just loads a data.frame)




## Environment ----
## Set up an environment to store the variables ----
## This environment will basically be used to store temporary variables which will be
## specified by the user to format the data for import.
## We initially also store the imported data in the enviroment but this will be returned in a later
## version of the program.
env <- new.env()

## Variable types
env$types <- c("integer", "character", "factor")

## Variable names and selected types
env$z_names <- list()
env$z_types <- list()


## Set up the GUI window ----
env$win <- gwindow("Load Data", visible=TRUE, height=400, width=800)

# The main window group ----
env$group <- ggroup(horizontal = FALSE, container=env$win)

## Button for file chooser dialog ----
env$file_button <- gbutton("Select File", handler = function(h, ...)
                             {
                               #Retrieve the selected file and store the details
                               env$data_filepath <- env$fileChoose(text="Select a file...")
                               env$selectData(x=env$data_filepath)
                               
                             }, container=env$group
)

## Add file path and some commentary to the dialog ----
env$path <- gedit("No File Selected", container=env$group, width=50)

## Frame to store the variable names and types, add a group for the variable names and types ----
env$data <- gframe("Chosse data to import and format", container=env$win, horizontal=T)

## Names will be checkboxes initiall so that you can select which fields to keep
## types will be radio boxes so that you can specify the type for import
env$variables_names_list <- ggroup(container=env$data, horizontal=F)
env$variables_types_list <- ggroup(container=env$data, horizontal=F)

## Create a customised file chooser dialog ----
env$fileChoose <- function( text = "Select a file...",
                            type="open", ...) {
  gfile(text=text, type=type, ...)
}


## Probe and specify the data to import..... ----
env$selectData <- function(x,...) {
  ## Get head of file to return variable names and guess the types
  data.head <- read.csv(x, header=T, nrows=10)
  data.names <- colnames(data.head)
  data.types <- sapply(data.head, class)
  ## Loop through the variables and return checkboxes and radio boxes for the user to inspect
  for(i in 1:length(data.names)){
    env$z_names[i] <- gcheckbox(text=data.names[i],container=env$variables_names_list)
    env$z_types[i] <- gradio(items=env$types,container=env$variables_types_list, horizontal=T, selected=match(data.types[i],env$types))
  }
  
  # Button to start the import ----
  env$file_button2 <- gbutton("Import Data", handler = 
                                function(h, ...) {
                                  col.names.logical <- sapply(env$z_names, svalue)
                                  col.classes <- sapply(env$z_types, svalue)[col.names.logical]
                                  env$raw.data <- read.csv(file=env$data_filepath, colClasses=col.classes)
                                  env$raw.data <- env$raw.data[,col.names.logical]
                                  print(nrow(env$raw.data))
                                  print(" rows imported")
                                  print("Data saved to env$raw.data....")
                                  dispose(h$obj)
                                }, container=env$group
  )
  
  
}

