library(shiny)
library(diffr)

#css<-readtext("render.css")$text
# Define UI for application
fluidPage(
  tags$style(HTML("
    .scrollable-sidebar {
      height: 80vh; /* Set the height of the sidebar */
      overflow-y: auto; /* Enable vertical scrolling */
      overflow-x: hidden; /* Disable horizontal scrolling */
      padding-right: 10px; /* Add some padding for better appearance */
    }
  ")),
  tags$head(
    #tags$head(
      # Dynamically insert dependencies
      uiOutput("dynamic_head"),
    tags$style(HTML('
     /* .td.code.replace.after {
       white-space: pre !important;
        overflow-x: auto !important;
      }*/
      /*td[class~="code"][class~="replace"][class~="after"] {
  white-space: pre !important;
        overflow-x: auto !important;
      }*/ /*wks!*/
      .diff-container { 
        border: 1px solid #ddd; 
        border-radius: 5px; 
        padding: 10px; 
        margin: 10px 0; 
        background: #f8f9fa;
        overflow-x: auto;
        width:100%;
        height:100vH;
      }
      .diff-header {
        background: #e9ecef;
        padding: 10px;
        border-radius: 3px;
        margin-bottom: 10px;
      }
      .ace_editor {
        border: 1px solid #ddd;
        border-radius: 4px;
      }
    ')),
  # tags$style(HTML(css)),

  # Application title
  titlePanel("dracor TEI refactoring"),
  
  # Main layout
  sidebarLayout(
    sidebarPanel(
      class = "scrollable-sidebar",  # Apply the custom CSS class
      helpText("SNC:15307.save issue.2.3.4"),
      h4("CONFIGURATION"),
      helpText("get transcript file..."),
      textInput("transcript","transcript from transkribus db","iwanette"),
      actionButton("submit.doc","fetch transcript"),
      fileInput("upload_tr","upload local transcript",accept = ".txt",buttonLabel = "browse..."),
      fileInput("upload_ezd","upload ezd marked-up transcript",accept = ".txt",buttonLabel = "browse..."),
      fileInput("upload_repl","upload replacements",accept = ".csv",buttonLabel = "browse..."),
      helpText("set title and author"),
      textInput("title","Title",""),
      textInput("subtitle","SubTitle, if vorhanden",""),
      textInput("author","Author",""),
      textInput("cast","castlist declaration:","Personen."),
      helpText("set body begin and act definitions"),
      textInput("h1","act header declarations:","Act|Akt|Handlung|.ufzug"),
      #actionButton("submit.h1","apply act definitions"),
      textInput("h2","scene header declarations:","Scene|Szene|.uftritt"),
      actionButton("submit.h","apply act|scene definitions"),
      
      helpText('enter the commonly used expressions that introduce a new act (header level 1) and scene (header level 2), like "Handlung|Akt|Act" or "Scene|Szene". This will be used as regex query for defining the sections.'),
     # helpText('we have found the following acts declarations:'),
      #verbatimTextOutput("acts"),
     # actionButton("sumbit.keep.act","use act definitions"),
      helpText("declare speaker"),
     actionButton("guess.sp","guess speakers"),
      textInput(
        "speaker",
        "speaker names:",
        ""
      ),
     
     
      actionButton("submit.sp", "Process Names"),
      hr(),
      helpText("Enter speaker names separated by commas, then click the button to process them."),
    textInput("id.defaults.save","ID to save settings"),
    actionButton("defaults.save","save settings"),
    textInput("id.defaults.load","load settings from ID"),
    actionButton("defaults.load","load settings"),
    actionButton("compare", "Compare Texts", class = "btn-primary", icon = icon("code-compare")),
    
    actionButton("submit.xml", "create XML"),
    downloadButton("downloadXML","downdload .xml"),
    downloadButton("downloadEZD","downdload ezd-markup text"),
    hr(),
    helpText("if you satisfied with the preprocessed text, start transformation.")
  ),
  mainPanel(
    verbatimTextOutput("proutput"),
    tabsetPanel(id="tabset",
      
      tabPanel("progress",
      h4("processing"),
      verbatimTextOutput("pr-progress"),
                ),
      tabPanel("raw",
               h4("raw text"),
               
               uiOutput("apidoc")),
      tabPanel("processed",
              h4("output"),
              
      uiOutput("processed")
      ),
      tabPanel("render",h4("rendered xml view"),
               uiOutput("xmlrendered")
      ),
      tabPanel("diff",
      div(class = "diff-container",
          h4("diff compare"),
          diffrOutput("diff_output")
      )),
      tabPanel("about",
               htmlOutput("md_html")
      ),
      tabPanel("helper",
               htmlOutput("nb")
      )
      
      # tabPanel("render",h4("rendered view"),
      #          uiOutput("xmlrendered")
      #          )
    ))
  
  )
)
)