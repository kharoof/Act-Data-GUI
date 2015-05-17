## The purpose of this GUI is to let the user
## 1. Specify a file to load
## 2. Let user choose the variable types and edit names if desired
## 3. Load the data to a R data structure (data frame or data table)
## Any further functionality will be provided in further GUI implementations


#library(RGtk2)
require(gWidgets)
 ##options("guiToolkit"="RGtk2")
options("guiToolkit"="tcltk")

## The GUI should be made up of two sections
## The first section will let the user pick the data to load
## The second section will show the loaded data variables and their current
## formats etc.

######################################
## SECTION 1 - Select the File to load
######################################

## Set up the window
env <- environment() #we will store our variables in this one
env$win <- gwindow("Load Data", visible=TRUE, height=400, width=800)
env$group <- ggroup(horizontal = FALSE, container=win)

## Define the components and add to the window
## Add a button that creates a file chooser dialog
env$file_button <- gbutton("Select File", handler = function(h, ...)
    {
        #Retrieve the selected file and store the details
        env$x <- fileChoose(text="Select a file...")
        addSummary(x=env$x)
        addPath(x = env$x)        
    }, container=env$group
                       )

env$path <- gedit("No File Selected", container=group, width=50)
env$frame <- gframe("Summary of Selected File", container=group)
env$summary <- gtext("No File Selected...", container=env$frame, font.attr=list(style="bold", size="xx-small"))




## Customised File Chooser Dialog ----
fileChoose <- function( text = "Select a file...",
                         type="open", ...) {
    gfile(text=text, type=type, ...)
}

## Update the selected file path
addPath <- function(x, ...) {
    svalue(env$path) <- x
}

## Add Summary of Selected data ----
addSummary <- function(x,...) {  
  data.head <- read.csv(x, header=T, nrows=10)
  data.names <- names(data.head)
  data.summary <- summary(data.head)
  data.types <- sapply(data.head, typeof)
  text <- rbind(data.names,data.types, data.summary, "\n")
    
     svalue(env$summary) <- text
    
   # svalue(env$summary) <- data.summary
    
    
}

