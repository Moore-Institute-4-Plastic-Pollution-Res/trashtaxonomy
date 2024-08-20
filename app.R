# Load packages
library(shiny)
library(dplyr)
library(data.table)
library(shinyjs)
library(shinythemes)
library(DT)
library(shinyhelper)
library(shinyTree)
library(data.tree)
library(tidyr)

timeoutSeconds <- 3000

# Workaround for Chromium Issue 468227
downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}

inactivity <- sprintf("function idleTimer() {
var t = setTimeout(logout, %s);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
Shiny.setInputValue('timeOut', '%ss')
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, %s);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();", timeoutSeconds*1000, timeoutSeconds, timeoutSeconds*1000)


ui <- fluidPage(
  tags$script(inactivity),    
  theme=shinytheme("cyborg"),
  titlePanel("Trash Taxonomy"),
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
      body {
        background-color: black;
        color: white;
      }
                    ")), 
    HTML('<script async src="https://media.ethicalads.io/media/client/ethicalads.min.js"></script>')
  ),
  #About ----
  tabsetPanel(
    tabPanel("About",
             fluidRow(
               column(3),
               column(6,
                      
                      shiny::HTML("<br><br><center> <h1>Overview</h1> </center><br>"),
                      h6("*** ATTENTION, This page has now been superceeded for all things new with the Trash Taxonomy, see MaTCH Github (https://github.com/hannahhapich/MaTCH) and webtool (https://hannahhapich.shinyapps.io/match/) ***"),
                      shiny::HTML("<h5>Trash Taxonomy website is a portal for relational tables that relate trash survey nomenclature. It includes 7 relational tables and a tool that can be used to query the relational tables with trash survey sheets.
                                  This tool was developed by collating and comparing categories and terms used from over 50 commonly used in trash survey sheets.</h5>"),
                      shiny::HTML("<h5>We are grateful for the funding for this project provided by the National Marine Sanctuary Foundation, the National Oceanic and Atmospheric Administration Marine Debris Program, and the Benioff Ocean Initiative</h5>"),
                      HTML('<div class = "dark raised" data-ea-publisher="openanalysisorg" id="trashtaxonomy_about" data-ea-type="image" data-ea-style="stickybox"></div>')
                      
                      
               ),
               column(3)
             ),
             
             fluidRow(
               
               style = "height:50px;"),
             
             # PAGE BREAK
             tags$hr(),
             
             
             
             fluidRow(
               column(3),
               column(6,
                      shiny::HTML("<br><br><center> <h1>About the Relational Tables</h1> </center><br>"),
                      align = "center",
                      tags$img(width = "100%", src = "https://github.com/wincowgerDEV/TrashTaxonomy/blob/master/www/RelationalStructure.png?raw=true"),
                      #HTML('<iframe width="560" height="315" src='https://dbdiagram.io/embed/5f3d9342cf48a141ff557dfe'> </iframe>'),
                      shiny::HTML("<h5>These relational tables describe alias relationships (words that mean the same thing) and hierarchical relationships (words that are nested groups within one another). You can view or download these tables using the relational table tab above!</h5>")
               ),
               column(3)
             ),
             
             fluidRow(
               
               style = "height:50px;"),
             
             # PAGE BREAK
             tags$hr(),
             
             # HOW
             fluidRow(
               column(3),
               column(6,
                      shiny::HTML("<br><br><center> <h1>About the Query Tool</h1> </center><br>"),
                      shiny::HTML("<h5>This tool queries the relational tables with an uploaded trash survey list. To use the tool, upload a csv file to the upload file tab. 
                                  The file needs to be a csv with one column named -material- and another named -items-. 
                                  The material should correspond to the item names in the same row.</h5>")
               ),
               column(3)
             ),
             tags$hr(),
             
             
             fluidRow(
               column(3),
               column(6,
                      
                      shiny::HTML("<br><br><center> <h1>How To Use</h1> </center><br>"),
                      shiny::HTML("<h5>In order to assist your navigation through both the relational tables and query tool functions of this app, please refer to the video tutorial below.</h5>"),
                      shiny::HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/sqeLaJKyol8" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen require-corp credentialless (Chrome > 96)></iframe>')
                      
               ),
               column(3)
             ),
             
             fluidRow(
               
               style = "height:50px;"),
             
             tags$hr(),
             
             
             fluidRow(
               
               style = "height:50px;"),
             
             # PAGE BREAK
             tags$hr(),
             
             
             fluidRow(
               column(3),
               column(6,
                      shiny::HTML("<br><br><center> <h1>Funded By</h1> </center><br>"),
                      align = "center",
                      tags$img(src= "https://github.com/wincowgerDEV/TrashTaxonomy/blob/master/www/NOAA.png?raw=true", width = "50%"),
                      tags$img(src="https://github.com/wincowgerDEV/TrashTaxonomy/blob/master/www/NMSF.png?raw=true", width = "50%"),
                      tags$img(src="https://github.com/wincowgerDEV/TrashTaxonomy/blob/master/www/boi.png?raw=true", width = "50%")
                      
                      #downloadButton(NOAA, label="Download")
                      #tags$div(align = "center",
                      #        tags$a("Sample Data",
                      #              onclick = "window.open('https://drive.google.com/file/d/1YKyEDf4VbZaeSlV6yxgh0XVqstvel6LQ/view', '_blank')",
                      #             class="btn btn-primary btn-lg")
                      #     )
                      
               ),
               column(3)
             ),
             
             fluidRow(
               
               style = "height:50px;"),
             
             # PAGE BREAK
             tags$hr(),
             
             # INSTRUCTIONAL SECTION
             fluidRow(
               column(3),
               column(6,
                      shiny::HTML("<br><br><center> <h1>Citation</h1> </center>
                                  <br>"),
                      shiny::HTML("<h5> H. Hapich, W. Cowger, A. Gray, Jambeck Research Group. 2020. Trash Taxonomy. https://trashtaxonomy.shinyapps.io/trashtaxonomy/</h5>")
               ),
               column(3)
             )
             
             
             #end of about panel
    ),
    
    #Relational Tables ----
    tabPanel("Relational Tables",
             titlePanel(tags$h4("View and Download Relational Tables")),
             
             # PAGE BREAK
             tags$hr(),
             
             fluidRow(
               column(3
               ),
               column(6,
                      shiny::HTML("<br><br><center> <h1>Materials Alias Table</h1> </center><br>"),
                      shiny::HTML("<h5>This table describes the aliases that can be used to describe material types and links them to a key term. Each row represents a unique material and each column is an alias for that material.</h5>"),
                      checkboxInput("show1", "Show Table", width = '100%')
                      
               ),
               column(3
               )
             ),
             
             # PAGE BREAK
             tags$hr(),
             
             fluidRow(
               column(3),
               column(6,
                      
                      tags$div(align = "center", 
                               downloadButton('downloadData1', 'Download'),
                               HTML('<div class = "dark raised" data-ea-publisher="openanalysisorg" id="trashtaxonomy_relational" data-ea-type="image" data-ea-style="stickybox"></div>')
                               
                      )
                      
               ),
               column(3)),
             
             
             fluidRow(
               column(1),
               column(10,
                      
                      conditionalPanel(condition = "input.show1 == true",
                                       DT::dataTableOutput('table1')
                      )
               ), 
               column(1)
             ),
             
             
             fluidRow(
               
               style = "height:50px;"),
             
             # PAGE BREAK
             tags$hr(),
             
             fluidRow(
               column(1),
               column(10,
                      shiny::HTML("<br><br><center> <h1>Materials Hierarchy Table</h1> </center><br>"),
                      shiny::HTML("<h5>This table describes how the unique material types relate to one another in a hierarchical structure (ex: foam and rubber are a subset of plastic).</h5>"),
                      #shinyTree(outputId = "materialhierarchy"),
                      
                      #collapsibleTreeOutput(outputId = "material_tree", width = "100%", height = "500px")
                      checkboxInput("show2", "Show Table", width = '50%'),
                      shinyTree::shinyTree(outputId = "materialhierarchy", dragAndDrop=F, sort = F, wholerow = T, theme = "default-dark", themeIcons = F, search = F)
                      
               ),
               column(1)
             ),
             
             # PAGE BREAK
             tags$hr(),
             
             fluidRow(
               column(3),
               column(6,
                      
                      tags$div(align = "center", 
                               downloadButton('downloadData2', 'Download')
                      )
                      
               ),
               column(3)
             ),
             
             fluidRow(
               column(1),
               column(10,
                      
                      conditionalPanel(condition = "input.show2 == true",
                                       DT::dataTableOutput('table2')
                      )
               ), 
               column(1)
             ),
             
             fluidRow(
               
               style = "height:50px;"),
             
             # PAGE BREAK
             tags$hr(),
             
             fluidRow(
               column(3),
               column(6,
                      shiny::HTML("<br><br><center> <h1>Items Alias Table</h1> </center><br>"),
                      shiny::HTML("<h5>This table describes the aliases that can be used to describe item types and links them to a key term. Each row represents a unique item and each column is an alias for that item.</h5>"),
                      checkboxInput("show3", "Show Table", width = '50%')
               ),
               column(3)
             ),
             
             # PAGE BREAK
             tags$hr(),
             
             fluidRow(
               column(3),
               column(6,
                      
                      tags$div(align = "center", 
                               downloadButton('downloadData3', 'Download')
                      )
                      
               ),
               column(3)
             ),
             
             fluidRow(
               column(1),
               column(10,
                      
                      conditionalPanel(condition = "input.show3 == true",
                                       DT::dataTableOutput('table3')
                      )
               ), 
               column(1)
             ),
             
             fluidRow(
               
               style = "height:50px;"),
             
             # PAGE BREAK
             tags$hr(),
             
             fluidRow(
               column(1),
               column(10,
                      shiny::HTML("<br><br><center> <h1>Items Hierarchy Table</h1> </center><br>"),
                      shiny::HTML("<h5>This table describes how the unique items relate to one another in a hierarchical structure (ex: forks, knives, and spoons all fall under utensils).</h5>"),
                      #shinyTree(outputId = "itemhierarchy"),
                      #div(style = "background-color: white;",
                      #    collapsibleTreeOutput(outputId = "item_tree", width = "100%", height = "500px")
                      #),
                      checkboxInput("show4", "Show Table", width = '50%'),
                      shinyTree::shinyTree(outputId = "itemshierarchy", dragAndDrop=F, sort = F, wholerow = T, theme = "default-dark", themeIcons = F, search = F)
                      
                      
               ),
               column(1)
             ),
             
             # PAGE BREAK
             tags$hr(),
             
             fluidRow(
               column(3),
               column(6,
                      
                      tags$div(align = "center", 
                               downloadButton('downloadData4', 'Download')
                      )
                      
               ),
               column(3)
             ),
             
             fluidRow(
               column(1),
               column(10,
                      
                      conditionalPanel(condition = "input.show4 == true",
                                       DT::dataTableOutput('table4')
                      )
               ), 
               column(1)
             ),
             
             fluidRow(
               
               style = "height:50px;"),
             
             # PAGE BREAK
             tags$hr(),
             
             fluidRow(
               column(3),
               column(6,
                      shiny::HTML("<br><br><center> <h1>Material-Item Relational Table</h1> </center><br>"),
                      shiny::HTML("<h5>This table relates the items, materials, and survey sheets used to make the other relational tables.</h5>"),
                      checkboxInput("show5", "Show Table", width = '50%')
                      
               ),
               column(3)
             ),
             
             # PAGE BREAK
             tags$hr(),
             
             fluidRow(
               column(3),
               column(6,
                      
                      tags$div(align = "center", 
                               downloadButton('downloadData5', 'Download')
                      )
                      
               ),
               column(3)
             ),
             
             fluidRow(
               column(1),
               column(10,
                      
                      conditionalPanel(condition = "input.show5 == true",
                                       DT::dataTableOutput('table5')
                      )
               ), 
               column(1)
             ),
             
             fluidRow(
               
               style = "height:50px;"),
             
             # PAGE BREAK
             tags$hr(),
             
             fluidRow(
               column(3),
               column(6,
                      shiny::HTML("<br><br><center> <h1>Manufacturer Brand Relational Table</h1> </center><br>"),
                      shiny::HTML("<h5>This table relates brand types to their respective manufacturer.</h5>"),
                      checkboxInput("show6", "Show Table", width = '50%')
                      
               ),
               column(3)
             ),
             
             # PAGE BREAK
             tags$hr(),
             
             fluidRow(
               column(3),
               column(6,
                      
                      tags$div(align = "center", 
                               downloadButton('downloadData6', 'Download')
                      )
                      
               ),
               column(3)
             ),
             
             fluidRow(
               column(1),
               column(10,
                      
                      conditionalPanel(condition = "input.show6 == true",
                                       DT::dataTableOutput('table6')
                      )
               ), 
               column(1)
             ),
             
             fluidRow(
               
               style = "height:50px;"),
             
             # PAGE BREAK
             tags$hr(),
             
             fluidRow(
               column(3),
               column(6,
                      shiny::HTML("<br><br><center> <h1>Item-Brand Relational Table</h1> </center><br>"),
                      shiny::HTML("<h5>This table relates brands to items.</h5>"),
                      checkboxInput("show7", "Show Table", width = '50%')
                      
               ),
               column(3)
             ),
             
             # PAGE BREAK
             tags$hr(),
             
             fluidRow(
               column(3),
               column(6,
                      
                      tags$div(align = "center", 
                               downloadButton('downloadData7', 'Download')
                      )
                      
               ),
               column(3)
             ),
             
             fluidRow(
               column(1),
               column(10,
                      
                      conditionalPanel(condition = "input.show7 == true",
                                       DT::dataTableOutput('table7')
                      )
               ), 
               column(1)
             ),
             fluidRow(
               
               style = "height:50px;"),
             
             # PAGE BREAK
             tags$hr(),
             
             fluidRow(
               column(3),
               column(6,
                      shiny::HTML("<br><br><center> <h1>Misaligned Categories Table</h1> </center><br>"),
                      shiny::HTML("<h5>This table displays all categories that did not fit our item-material framework.</h5>"),
                      checkboxInput("show8", "Show Table", width = '50%')
                      
               ),
               column(3)
             ),
             
             # PAGE BREAK
             tags$hr(),
             
             fluidRow(
               column(3),
               column(6,
                      
                      tags$div(align = "center", 
                               downloadButton('downloadData8', 'Download')
                      )
                      
               ),
               column(3)
             ),
             
             fluidRow(
               column(1),
               column(10,
                      
                      conditionalPanel(condition = "input.show8 == true",
                                       DT::dataTableOutput('table8')
                      )
               ), 
               column(1)
             ),
             
             fluidRow(
               column(3),
               column(6,
                      shiny::HTML("<br><br><center> <h1>Citation</h1> </center>
                                  <br>"),
                      shiny::HTML("<h5> H. Hapich, W. Cowger, A. Gray, Jambeck Research Group. 2020. Trash Taxonomy. https://trashtaxonomy.shinyapps.io/trashtaxonomy/</h5>")
               ),
               column(3)
             )
             
    ),
    
    #Tool ----
    tabPanel("Query Tool",
             titlePanel(tags$h4("Query the Relational Tables with Trash Survey Sheets")),
             fluidRow(
               column(2, 
                      fileInput('df', "Choose CSV File", multiple = FALSE, accept = c(".csv")),
                      
                      
                      downloadButton('downloadtest', 'Download Test Data'),
                      
                      checkboxGroupInput('variable', "Functions:",
                                         c("More Specific Materials"="MoreSpecificMaterial",
                                           "Less Specific Materials"="LessSpecificMaterial",
                                           "More Specific Items"="MoreSpecificItem",
                                           "Less Specific Items"="LessSpecificItem")),
                      HTML('<div class = "dark raised" data-ea-publisher="openanalysisorg" id="trashtaxonomy_tool" data-ea-type="image" data-ea-style="stickybox"></div>')
               ),
               column(10, 
                      dataTableOutput('contents')
               )
               
             ),
             hr(),
             fluidRow(
               column(3), 
               column(6,shiny::HTML("<br><br><center> <h4>View Key Alias Matches</h4> </center><br>")
               ),
               column(3)
               
             ),
             fluidRow(
               column(1),
               column(5, 
                      dataTableOutput('contents1')
               ),
               column(5, 
                      dataTableOutput('contents2')
               ), 
               column(1)
             ),
             fluidRow(
               align="center",
               hr(),
               tags$p("Citation: H. Hapich, W. Cowger, A. Gray, Jambeck Research Group. 2020. Trash Taxonomy. https://trashtaxonomy.shinyapps.io/trashtaxonomy/")
             )
    )
  )
)



#setwd("/Users/hannahhapich/desktop/Trash_Taxonomy/TrashTaxonomy")

cleantext <- function(x) {
  x <- tolower(gsub("[[:space:]]", "", x))
  ifelse(x == "", NA, x)
}

removeslash <- function(x){
  gsub("/", " OR ", x)
}


cleanmaterials <- function(x) {
  x <- x[!names(x) %in% c("X1", "X2", "X3", "X4")]
  if(is.list(x)) lapply(x, cleanmaterials)
}


cleanitems <- function(x) {
  x <- x[!names(x) %in% c("X1", "X2", "X3", "X4", "X5", "X6")]
  if(is.list(x)) lapply(x, cleanitems)
}


#Files for tool
alius <- read.csv("data/PrimeMaterials.csv")
hierarchy <- read.csv("data/MaterialsHierarchyLower.csv")
aliusi <- read.csv("data/PrimeItems.csv")
hierarchyi <- read.csv("data/ITEMSHierarchyLower.csv")
aliasclean <- mutate_all(alius, cleantext)
aliascleani <- mutate_all(aliusi, cleantext)
hierarchyclean <- mutate_all(hierarchy, cleantext)
hierarchycleani <- mutate_all(hierarchyi, cleantext)

Materials <- hierarchy
Materials[is.na(Materials)] <- ""
Materials <- mutate_all(Materials, removeslash)
Materials$pathString <- paste("Trash", Materials$X1, Materials$X2, Materials$X3, Materials$X4, sep = "/")
Materials <- as.Node(Materials[,])
Materials <- as.list(Materials)
Materials <- Materials[-1]
Materials <- cleanmaterials(Materials)

Materials_hierarchy <- hierarchy
Materials_hierarchy[is.na(Materials_hierarchy)] <- ""
Materials_hierarchy <- mutate_all(Materials_hierarchy, removeslash) %>%
  mutate(key = "trash") %>%
  relocate(key) %>%
  unite(pathString, sep = "/")
Materials_hierarchy <- as.Node(Materials_hierarchy, pathDelimiter = "/")
Materials_hierarchy <- as.list(Materials_hierarchy)
Materials_hierarchy <- Materials_hierarchy[-1]

Items <- hierarchyi
Items[is.na(Items)] <- ""
Items <- mutate_all(Items, removeslash)
Items$pathString <- paste("Trash", Items$X1, Items$X2, Items$X3, Items$X4, Items$X5, Items$X6, sep = "/")
Items <- as.Node(Items)
Items <- as.list(Items)
Items <- Items[-1]
Items <- cleanitems(Items)

Items_hierarchy <- hierarchyi
Items_hierarchy[is.na(Items_hierarchy)] <- ""
Items_hierarchy <- mutate_all(Items_hierarchy, removeslash) %>%
  mutate(key = "trash") %>%
  relocate(key) %>%
  unite(pathString, sep = "/")
Items_hierarchy <- as.Node(Items_hierarchy, pathDelimiter = "/")
Items_hierarchy <- as.list(Items_hierarchy)
Items_hierarchy <- Items_hierarchy[-1]


#Files for display
Materials_Alias <- read.csv("data/PrimeMaterials.csv")
Materials_Hierarchy <- read.csv("data/MaterialsHierarchyLower.csv")
Items_Alias <- read.csv("data/PrimeItems.csv")
Items_Hierarchy <- read.csv("data/ITEMSHierarchyLower.csv")
Material_Item_Relation <- read.csv("data/MaterialItemRelationship.csv")
Brand_Manufacturer_Relation <- read.csv("data/BrandManufacturer.csv")
Brand_Item_Relation <- read.csv("data/BrandItem.csv")
NOAA <- read.csv("data/NOAA.csv")
PrimeUnclassifiable <- read.csv("data/PrimeUnclassifiable.csv")


server <- function(input,output,session) {
  
  observeEvent(input$timeOut, { 
    print(paste0("Session (", session$token, ") timed out at: ", Sys.time()))
    showModal(modalDialog(
      title = "Timeout",
      paste("Session timeout due to", input$timeOut, "inactivity -", Sys.time()),
      footer = NULL
    ))
    session$close()
  })
  
  df <- reactive({
    req(input$df)
    infile <- input$df
    df <- fread(infile$datapath)
    dataframe<- as.data.frame(df)%>%
      select(material, items)
    dataframe$material <- as.character(dataframe$material)
    dataframe$items <- as.character(dataframe$items)
    dataframeclean <- mutate_all(dataframe, cleantext) 
    
    
    for(row in 1:nrow(dataframeclean)) { 
      
      if(is.na(dataframeclean[row,"material"]) | dataframeclean[row,"material"] == "") {
        dataframe[row, "PrimeMaterial"] <- NA
        dataframe[row, "MoreSpecificMaterial"] <- NA
        next #Corrects for cases when there is no val. 
      }
      
      if(any(PrimeUnclassifiable == dataframeclean[row,"material"])) {
        dataframe[row, "MoreSpecificMaterial"] <- "Unclassifiable"
        dataframe[row, "PrimeMaterial"] <- NA
        next #Corrects for cases when unclassifiable
      }
      
      #Identify Alias Row and Alias name
      Primename <- unique(aliasclean[unname(unlist(apply(aliasclean, 2, function(x) which(x == dataframeclean[row,"material"], arr.ind = T)))), "Material"])
      
      if(length(Primename) == 0 ){
        dataframe[row, "PrimeMaterial"] <- "NO VAL IN DATABASE"
      }
      
      else{
        dataframe[row, "PrimeMaterial"] <- Primename
      }
      
      
      #Identify Column Name in Hierarchy
      hierarchycolumnnum <- as.numeric(gsub("[[:alpha:]]", "", unique(names(unlist(apply(hierarchyclean, 1, function(x) which(x == dataframe[row, "PrimeMaterial"], arr.ind = T)))))))
      if(length(hierarchycolumnnum) == 0) {
        dataframe[row, "MoreSpecificMaterial"] <- "NO VAL IN DATABASE"
        next #Corrects for cases when there is no val. 
      }
      
      
      if(hierarchycolumnnum == ncol(hierarchyclean)) next #Corrects for cases when the value is already the most specific material.
      hierarchyrows <- unname(unlist(apply(hierarchyclean, 2, function(x) which(x == dataframe[row, "PrimeMaterial"], arr.ind = T))))
      
      #Get higher values
      vals <- tolower(unique(unname(unlist(hierarchy[hierarchyrows, (hierarchycolumnnum + 1):ncol(hierarchyclean)]))))
      
      #Push values into datatable
      dataframe[row, "MoreSpecificMaterial"] <- paste(vals[!is.na(vals)], collapse = " | ")
      
    }
    
    #Less Specific Materials
    for(row in 1:nrow(dataframeclean)) { 
      
      if(is.na(dataframeclean[row,"material"]) | dataframeclean[row,"material"] == "") {
        dataframe[row, "LessSpecificMaterial"] <- NA
        next #Corrects for cases when there is no val. 
      }
      
      if(any(PrimeUnclassifiable == dataframeclean[row,"material"])) {
        dataframe[row, "LessSpecificMaterial"] <- "Unclassifiable"
        next #Corrects for cases when unclassifiable
      }
      
      
      hierarchycolumnnum <- as.numeric(gsub("[[:alpha:]]", "", unique(names(unlist(apply(hierarchyclean, 1, function(x) which(x == dataframe[row, "PrimeMaterial"], arr.ind = T)))))))
      
      if(length(hierarchycolumnnum) == 0) {
        dataframe[row, "LessSpecificMaterial"] <- "NO VAL IN DATABASE"
        next #Corrects for cases when there is no value.
      }
      
      if(hierarchycolumnnum == 1) next #Corrects for cases when the value is already the least specific.
      hierarchyrows <- unname(unlist(apply(hierarchyclean, 2, function(x) which(x == dataframe[row, "PrimeMaterial"], arr.ind = T))))
      
      #Get higher values
      vals <- tolower(unique(unname(unlist(hierarchy[hierarchyrows, 1:(hierarchycolumnnum - 1)]))))
      
      #Push values into datatable
      dataframe[row, "LessSpecificMaterial"] <- paste(vals[!is.na(vals)], collapse = " | ")
      
    }
    
    #row = 1
    
    #find all more specific items
    for(row in 1:nrow(dataframe)) { 
      
      if(is.na(dataframeclean[row,"items"]) | dataframeclean[row,"items"] == "") {
        dataframe[row, "PrimeItem"] <- NA
        dataframe[row, "MoreSpecificItem"] <- NA
        next #Corrects for cases when there is no val. 
      }
      
      if(any(PrimeUnclassifiable == dataframeclean[row,"items"])) {
        dataframe[row, "MoreSpecificItem"] <- "Unclassifiable"
        dataframe[row, "PrimeItem"] <- NA
        next #Corrects for cases when unclassifiable
      }
      
      #Identify Alias Row and Alias name
      Primename <- unique(aliascleani[unname(unlist(apply(aliascleani, 2, function(x) which(x == dataframeclean[row,"items"], arr.ind = T)))), "Item"])
      
      if(length(Primename) == 0){
        dataframe[row, "PrimeItem"] <- "NO VAL IN DATABASE"
      }
      
      else{
        dataframe[row, "PrimeItem"] <- Primename
      }
      
      
      
      #Identify Column Name in Hierarchy
      hierarchycolumnnum <- as.numeric(gsub("[[:alpha:]]", "", unique(names(unlist(apply(hierarchycleani, 1, function(x) which(x == dataframe[row, "PrimeItem"], arr.ind = T)))))))
      if(length(hierarchycolumnnum) == 0) {
        dataframe[row, "MoreSpecificItem"] <- "NO VAL IN DATABASE"
        
        next #Corrects for cases when there is no value.
      }
      
      if(hierarchycolumnnum == ncol(hierarchycleani)) next #Corrects for cases when the value is already the most specific.
      
      hierarchyrows <- unname(unlist(apply(hierarchycleani, 2, function(x) which(x == dataframe[row, "PrimeItem"], arr.ind = T))))
      
      #Get higher values
      vals <- tolower(unique(unname(unlist(hierarchyi[hierarchyrows, (hierarchycolumnnum + 1):ncol(hierarchyclean)]))))
      
      #Push values into datatable
      dataframe[row, "MoreSpecificItem"] <- paste(vals[!is.na(vals)], collapse = " | ")
      
    }
    
    #find all less specific items
    for(row in 1:nrow(dataframe)) { 
      if(is.na(dataframeclean[row,"items"]) | dataframeclean[row,"items"] == "") {
        dataframe[row, "LessSpecificItem"] <- NA
        next #Corrects for cases when there is no val. 
      }
      
      if(any(PrimeUnclassifiable == dataframeclean[row,"items"])) {
        dataframe[row, "LessSpecificItem"] <- "Unclassifiable"
        next #Corrects for cases when unclassifiable
      }
      
      #Identify Alias Row and Alias name
      #Identify Column Name in Hierarchy
      hierarchycolumnnum <- as.numeric(gsub("[[:alpha:]]", "", unique(names(unlist(apply(hierarchycleani, 1, function(x) which(x == dataframe[row, "PrimeItem"], arr.ind = T)))))))
      if(length(hierarchycolumnnum) == 0) {
        dataframe[row, "LessSpecificItem"] <- "NO VAL IN DATABASE"
        next #Corrects for cases when there is no value.
      }
      
      if(hierarchycolumnnum == 1) next #Corrects for cases when the value is already the least specific.
      hierarchyrows <- unname(unlist(apply(hierarchycleani, 2, function(x) which(x == dataframe[row, "PrimeItem"], arr.ind = T))))
      
      #Get higher values
      vals <- tolower(unique(unname(unlist(hierarchyi[hierarchyrows, 1:(hierarchycolumnnum - 1)]))))
      
      #Push values into datatable
      dataframe[row, "LessSpecificItem"] <- paste(vals[!is.na(vals)], collapse = " | ")
      
    }
    
    
    return(dataframe)
  })
  
  columnnames <- reactive({
    return(input$variable)
  })
  
  output$contents <- renderDataTable({
    df()[, c("material","items",  input$variable)]
  }, style="bootstrap")
  
  output$contents1 <- renderDataTable({
    df()[, c("material","PrimeMaterial")] %>% distinct()
  }, selection=list(mode="single", target="row"), style="bootstrap")
  
  output$contents2 <- renderDataTable({
    df()[, c("items","PrimeItem")] %>% distinct()
  }, selection=list(mode="single", target="row"), style="bootstrap")
  
  output$downloadData1 <- downloadHandler(    
    filename = function() {
      paste(deparse(substitute(Materials_Alias)), '.csv', sep='')
    },
    content = function(file) {
      write.csv(Materials_Alias, file, row.names=FALSE)
    }
  )
  
  output$downloadData2 <- downloadHandler(    
    filename = function() {
      paste(deparse(substitute(Materials_Hierarchy)), '.csv', sep='')
    },
    content = function(file) {
      write.csv(Materials_Hierarchy, file, row.names=FALSE)
    }
  )
  
  #output$material_tree <- renderCollapsibleTree(collapsibleTree(Materials_Hierarchy,
  #                                                              root = "Materials Hierarchy",
  #                                                              hierarchy = names(Materials_Hierarchy), 
  #                                                              #width = 800,
  #                                                              fontSize = 14))
  
  output$materialhierarchy <- renderTree({
    #shiny::validate(shiny::need(input$file1, "Please upload a dataset to get started")) 
    Materials_hierarchy
  }
  )
  
  output$itemshierarchy <- renderTree({
    #shiny::validate(shiny::need(input$file1, "Please upload a dataset to get started")) 
    Items_hierarchy
  }
  )
  
  output$downloadData3 <- downloadHandler(    
    filename = function() {
      paste(deparse(substitute(Items_Alias)), '.csv', sep='')
    },
    content = function(file) {
      write.csv(Items_Alias, file, row.names=FALSE)
    }
  )
  
  output$downloadData4 <- downloadHandler(    
    filename = function() {
      paste(deparse(substitute(Items_Hierarchy)), '.csv', sep='')
    },
    content = function(file) {
      write.csv(Items_Hierarchy, file, row.names=FALSE)
    }
  )
  
  
  output$downloadData5 <- downloadHandler(    
    filename = function() {
      paste(deparse(substitute(Material_Item_Relation)), '.csv', sep='')
    },
    content = function(file) {
      write.csv(Material_Item_Relation, file, row.names=FALSE)
    }
  )
  
  output$downloadData6 <- downloadHandler(    
    filename = function() {
      paste(deparse(substitute(Brand_Manufacturer_Relation)), '.csv', sep='')
    },
    content = function(file) {
      write.csv(Brand_Manufacturer_Relation, file, row.names=FALSE)
    }
  )
  
  output$downloadData7 <- downloadHandler(    
    filename = function() {
      paste(deparse(substitute(Brand_Item_Relation)), '.csv', sep='')
    },
    content = function(file) {
      write.csv(Brand_Item_Relation, file, row.names=FALSE)
    }
  )
  
  output$downloadData8 <- downloadHandler(    
    filename = function() {
      paste(deparse(substitute(PrimeUnclassifiable)), '.csv', sep='')
    },
    content = function(file) {
      write.csv(PrimeUnclassifiable, file, row.names=FALSE)
    }
  )
  
  output$downloadtest <- downloadHandler(    
    filename = function() {
      paste(deparse(substitute(NOAA)), '.csv', sep='')
    },
    content = function(file) {
      write.csv(NOAA, file, row.names=FALSE)
    }
  )
  
  output$table1 = DT::renderDataTable({
    Materials_Alias
  }, style="bootstrap")
  
  output$table2 = DT::renderDataTable({
    Materials_Hierarchy
  }, style="bootstrap")
  
  output$table3 = DT::renderDataTable({
    Items_Alias
  }, style="bootstrap")
  
  output$table4 = DT::renderDataTable({
    Items_Hierarchy
  }, style="bootstrap")
  
  output$table5 = DT::renderDataTable({
    Material_Item_Relation
  }, style="bootstrap")
  
  output$table6 = DT::renderDataTable({
    Brand_Manufacturer_Relation
  }, style="bootstrap")
  
  output$table7 = DT::renderDataTable({
    Brand_Item_Relation
  }, style="bootstrap")
  
  output$table8 = DT::renderDataTable({
    PrimeUnclassifiable
  }, style="bootstrap")
  
}







shinyApp(ui = ui, server = server)