require(gWidgets)
require(tcltk2)
options("guiToolkit"="tcltk")


##############################################################################################################
## GUI.R ----
## 18/10/2015
## This program lets the user select a csv file to load and choose the variables to import and the types ----
##############################################################################################################
## The file is designed to create an environment to store the various overhead need to import the file
## This has not been designed to be especially efficient and is currently only intended
## to be a proof of concept rather than a production program
## The first section will let the user pick the data to load
## The second section will present the loaded data variables and their current
## formats etc in a GUI
## The user can then select which variables to import from the file and add a data type to them
## This GUI does not contain any error handling. This needs extensive error handling e.g. only allow
## one origin etc
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

## Variable types (R native types and Actuarial Types typical for importing Actuarail data)
env$types <- c("integer", "character", "factor", "date")
env$act_types <- c("Incurred", "Paid", "Count","Origin", "Development", "Class")

## Variable names and selected types
env$z_names <- list()
env$z_types <- list()
env$z_act_types <- list()

## Set up the GUI window ----
env$win <- gwindow("Load Data", visible=TRUE, height=400, width=800)

## The main window group ----
env$group <- ggroup(horizontal = FALSE, container=env$win)

## Button for file chooser dialog ----
env$file_button <- gbutton("Select File", handler = function(h, ...)
    {
        ##Retrieve the selected file and store the details
        env$data_filepath <- env$fileChoose(text="Select a file...")
        env$selectData(x=env$data_filepath)
        
    }, container=env$group
                           )

## Add file path and some commentary to the dialog ----
env$path <- gedit("No File Selected", container=env$group, width=50)


## Create a customised file chooser dialog ----
env$fileChoose <- function( text = "Select a file...",
                           type="open", ...) {
    gfile(text=text, type=type, ...)
}


## Probe and specify the data to import..... ----
env$selectData <- function(x,...) {
    ## Get head of file to return variable names and guess the types
    data.head <- read.csv(x, header=T, nrows=10)
    svalue(env$path) <- x
    data.names <- colnames(data.head)
    data.types <- sapply(data.head, class)
    ## Frame to store the variable names and types,
    ## add groups for the variable names and types and only make this visible after a
    ## file has been selected
    env$data <- gframe("Chosse data to import and format", container=env$win, horizontal=F)
    helpTxt <- "Select variables to load from import file
Select Actuarial Type/Role. The raw data variables will be renamed accordingly
Select Import data type which is native R type used when importing (Choose character if not sure or if you get an error)
     Factors should be imported as factors
     Dates (European format) should be imported as dates
     Everything else can be impoted as a character
"
    glabel(helpTxt, container=env$data)
    env$table <- gframe( container=env$data, horizontal=T)
    env$var_name <- gframe("Variable", container=env$table)
    env$var_act_type <- gframe("Actuarial Type", container=env$table)
    env$var_type <- gframe("Import Type (R Type)", container=env$table)


    
    
    ## Names will be checkboxes initially so that you can select which fields to keep

    ## types will be radio boxes so that you can specify the type for import
    ## Both R data types and Actuarial data types will be sepcified
    env$variables_names_list <- ggroup(container=env$var_name, horizontal=F)
    env$variables_data_type_list <- ggroup(container=env$var_type, horizontal=F)


    ## To do.......
    ## Make this more automated, for dates have only date options, amounts only amounts etc...
    env$variables_actuarial_data_type <- ggroup(container=env$var_act_type, horizontal=F)

    
    ## Loop through the variables and return checkboxes and radio boxes for the user to inspect
    for(i in 1:length(data.names)){
        env$z_names[i] <- gcheckbox(text=data.names[i],container=env$variables_names_list)
        env$z_types[i] <- gradio(items=env$types,container=env$variables_data_type_list, horizontal=T, selected=match(data.types[i],env$types))
        env$z_act_types[i] <- gradio(items=env$act_types,container=env$variables_actuarial_data_type, horizontal=T, selected=0)
    }
    
    ## Button to start the import ----
    env$file_button2 <- gbutton("Import Data", handler = 
                                    function(h, ...) {
                                        ## sub date for character for import then convert to required date format
                                        ## not possible to specify date format in read.csv
                                        ## remove complexity of dates from the user
                                        ## create  a local variable to store this
                                        z_types <- gsub("date", "character",sapply(env$z_types, svalue))
                                        col.names.logical <- sapply(env$z_names, svalue)
                                        ## Drop unselected cols
                                        col.classes <- ifelse(col.names.logical == FALSE,"NULL" , z_types)
                                        env$raw.data <- read.csv(file=env$data_filepath, colClasses=col.classes)
                                        ## rename the variables
                                        names(env$raw.data) <- sapply(env$z_act_types, svalue)[col.names.logical]
                                        ##env$raw.data <- env$raw.data[,which(col.names.logical)==1]
                                        ## Format dates
                                        z_types <- sapply(env$z_types,svalue)[col.names.logical]
                                        for( i in 1:length(z_types) ){
                                            if(z_types[i] == "date") {
                                                env$raw.data[,i] <- as.Date(env$raw.data[,i],"%d/%m/%Y")
                                            }
                                                     }
                                        #env$raw.data <- env$raw.data[,col.names.logical]
                                        assign("raw.data", env$raw.data, envir=.GlobalEnv)
                                        print(nrow(env$raw.data))
                                        print(" rows imported")
                                        print("Data saved to env$raw.data....")
                                        dispose(h$obj)
                                    }, container=env$group
                                )
    
    
}

