library(shiny)

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
  # Application title
  titlePanel("dracor TEI refactoring"),
  
  # Main layout
  sidebarLayout(
    sidebarPanel(
      class = "scrollable-sidebar",  # Apply the custom CSS class
      helpText("SNC:15307.save issue.2.3.4"),
      h4("CONFIGURATION"),
      helpText("get transcript file..."),
      textInput("transcript","transcript","iwanette"),
      actionButton("submit.doc","fetch transcript"),
      helpText("set title and author"),
      textInput("title","Title",""),
      textInput("subtitle","SubTitle, if vorhanden",""),
      textInput("author","Author",""),
      textInput("cast","castlist declaration:","Personal."),
      helpText("set body begin and act definitions"),
      textInput("h1","act header declarations:","Act|Akt|Handlung|.ufzug"),
      #actionButton("submit.h1","apply act definitions"),
      textInput("h2","scene header declarations:","Scene|Szene|.uftritt"),
      actionButton("submit.h","apply act|scene definitions"),
      
      helpText('enter the commonly used expressions that introduce a new act (header level 1) and scene (header level 2), like "Handlung|Akt|Act" or "Scene|Szene". This will be used as regex query for defining the sections.'),
      helpText('we have found the following acts declarations:'),
      verbatimTextOutput("acts"),
     # actionButton("sumbit.keep.act","use act definitions"),
      helpText("declare speaker"),
      textInput(
        "speaker",
        "speaker names:",
        ""
      ),
     
     
      actionButton("submit.sp", "Process Names"),
      hr(),
      helpText("Enter speaker names separated by commas, then click the button to process them."),
    textInput("id.defaults.save","ID to save settings"),
    actionButton("defaults.save","save defaults"),
    textInput("id.defaults.load","load settings from ID"),
    actionButton("defaults.load","load defaults"),
    actionButton("submit.xml", "create XML"),
    hr(),
    helpText("if you satisfied with the preprocessed text, start transformation.")
  ),
    mainPanel(
      h4("Processed Output"),
      verbatimTextOutput("spoutput"),
      uiOutput("apidoc")
    )
  )
)