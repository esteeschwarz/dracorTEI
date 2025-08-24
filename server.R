library(shiny)
library(readr)
library(httr)
library(jsonlite)
source("ezd2tei.R")
source("functions.R")
#sp.default<-"Iwanette,Golowin,Wolsey,Stormond,Bender"
transcript<-"iwanette"
output_file<-"www/r-tempxmlout.xml"
# get.transcript<-function(transcript){
#   r<-GET(paste0("https://ids.dh-index.org/api/trans?transcript=",transcript))
#   t<-content(r,"text")
#  # t<-clean.t(t)
#   txtemp<-tempfile("transtemp.txt")
#   writeLines(t,txtemp)
#   t1<-readLines(txtemp)
#   
#   empty<-c(""," ","  ")
#   m<-t1%in%empty
#   
#   t1<-t1[!m]
#   #t1<-t
#   return(list(txraw=t,tlines=t1))
# }
# tlist<-get.transcript("iwanette")
# text<-tlist$tlines
# traw<-tlist$txraw
# # defaults<-data.frame(id=1,h1="Act|Akt|Handlung",h2="Szene|Scene",speaker="Stormond,Iwanette,Golowin,Bender,Wolsey")
# # save(defaults,file = "default-values.RData")
# load_defaults <- function(id=F) {
#   # Replace this with your actual database query
#   # Example database connection and query:
#   idx<-id
#   tryCatch({
#     # con <- dbConnect(RSQLite::SQLite(), "your_database.db")
#     # result <- dbGetQuery(con, "SELECT speaker_names FROM defaults WHERE id = 1")
#     # dbDisconnect(con)
#     # return(result$speaker_names[1])
#     load("default-values.RData")
#     print("loaded...")
#     print(id)
#     cat("----\n")
#     
#     print(head(defaults))
#     #ifelse(!id,idx<-1:length(defaults$id),idx<-id)
#     print(idx)
#     cat("----\n")
#     return(defaults[idx,])
#     # For demonstration, returning a mock default value
#     return("Character1, Character2, Narrator, Chorus")
#   }, error = function(e) {
#     # Fallback default if database is unavailable
#     defaults<-data.frame(id=1,h1="Act|Akt|Handlung",h2="Szene|Scene",speaker="Stormond,Iwanette,Golowin,Bender,Wolsey")
#     return(defaults[idx,])
#     return("Speaker1, Speaker2, Speaker3")
#   })
# }

# Load defaults when app starts
observe({
  # Load default speaker names from database
  #sp_default <- load_default_speakers()
  defaults<-load_defaults(1)
  # Update the text input with the loaded default
  updateTextInput(session, "speaker", value = defaults$speaker)
  updateTextInput(session, "h1", value = defaults$h1)
  updateTextInput(session, "h2", value = defaults$h2)
})
# save_defaults<-function(rvdf){
#   load("default-values.RData")
#   print(head(defaults))
#   defaults[rvdf$id,]<-rvdf
#   save(defaults,file="default-values.RData")
#   print("saved")
#   
# }
# transform.ezd<-function(ezd){
#   #ezdtemp<-tempfile("ezd.txt")
#   #writeLines(ezd,ezdtemp)
#   #xmlout<-tempfile("xmlout.xml")
#   xmlout<-"r-tempxmlout.xml"
#   writeLines(ezd,"ezdmarkup.txt")
#   parse_drama_text(ezd,xmlout)
#   return(readLines(xmlout))
# }
# get.heads<-function(t1,headx="(Akt|Act"){
#   numer<-c("(Erst|Zweyt|Zweit|Dritt|Viert|Fünfte|Fuenft|Sechs|Sieben|Acht|Neun|Zehn|Elf|Zwoelf|Zwölf|Dreizehn|Dreyzehn)")
#   regx<-paste0("^.+?",numer,".+(",headx,")\\.")
#   m<-grep(regx,t1)
#   t2<-t1
#   t2[m]<-paste0("#ACT ",t2[m])
#   #return(t1)
#   return(list(vario=t1[m],text=t2))
# }
# sp<-"eins,zwei,drei"
# get.speakers<-function(t1,sp){
#   sp01<-unlist(strsplit(sp,","))
#   sp1<-paste0("(",paste0(sp01,collapse = "|"),")")
#   sp2<-paste0(sp01,".")
#   print(sp1)
#   regx<-paste0("^.+?",sp1,"\\.")
#   regx<-paste0("^",sp1,"\\.$")
#   print(regx)
#   m<-grep(regx,t1)
#   print(m)
#   t2<-t1
#   
#   crit<-t2[m]%in%sp2
#   print(crit)
#   crit.sp<-t2[m][!crit]
#   crit.sp<-crit.sp[!is.na(crit.sp)]
#   t2[m]<-paste0("@",t2[m])
#   
#   print(crit.sp)
#   wc<-which(!crit)
#   print(wc)
#   sp.move<-function(){
#   for (k in wc){
#     p<-m[k]
#     s<-strsplit(t2[p],"\\.")
#     print(s)
#     t2[p]<-paste0(s[[1]][1],".")
#     s[[1]][1]<-""
#     rest<-paste0(s)
#     
#     t2<-append(t2,rest,after = p)
# 
# 
#   }
#   }
#   sp.move()
#   # unlist(s)
#   # t<-lapply(s,function(x){
#   #   st<-x[1]
#   #   nd<-x[2]
#   # })  
#   crit.m<-length(m)-sum(!crit)
#   print(crit.m)
#   #return(t1)
#   return(list(vario=t1[m][crit],text=t2,eval=crit.sp))
#   
# }
# Define server logic
function(input, output, session) {
  # Reactive values to store intermediate states
  rv <- reactiveValues(
    t1 = "input docname...",
    t2 = NULL,
    t3 = NULL,
    heads = "not defined...",
    speaker = array(),
    speaker.crit = array(),
    h1.sf = "",
    h2.sf = "",
    sp.sf = "",
    id.sf = NULL,
    cast = NULL
  )
  output$downloadXML<-downloadHandler(
    filename="ezdxmlout.xml",
    content=function(file){writeLines(readLines(output_file),file)}
  )
  observeEvent(input$defaults.save,{
    rv$id.sf<-input$id.defaults.save
    rv$sp.sf<-input$speaker
    rv$h1.sf<-input$h1
    rv$h2.sf<-input$h2
    rv$cast<-input$cast
    rvdf<-data.frame(id=rv$id.sf,cast=rv$cast,h1=rv$h1.sf,h2=rv$h2.sf,speaker=rv$sp.sf)
    cat("observe sf...\n")
    print(head(rvdf))
    save_defaults(rvdf)
    updateTextInput(session, "id.defaults.save", value = "settings saved...")
    
  })
  observeEvent(input$defaults.load,{
    defaults<-load_defaults(input$id.defaults.load)
    cat("oberserve load...\n")
    print(input$id.defaults.load)
    print(head(defaults))
    updateTextInput(session, "speaker", value = defaults$speaker)
    updateTextInput(session, "h1", value = defaults$h1)
    updateTextInput(session, "h2", value = defaults$h2)
    updateTextInput(session, "cast", value = defaults$cast)
    
  })
  # Observe the submit button for fetching the transcript
  observeEvent(input$submit.doc, {
    output$apidoc <- renderUI({ div(tags$pre("Processing...")) })  # Show a loading message
    
    # Fetch the transcript
    transcript <- input$transcript
    tlist <- get.transcript(transcript)  # Store the transcript in reactiveValues
    t1<-tlist$txraw
    t4<-tlist$tlines
    t4
    t3<-repl.um(t4)
    t3
    t3<-clean.t(t3,1)
    t3
    #rv$t3<-clean.t(t1,2)
    rv$t1<-t3
    # Update the UI with the fetched transcript
    output$apidoc <- renderUI({
      div(
        style = "height: 70vh; overflow-y: auto; background: #f8f8f8; padding: 10px;",
        tags$pre(style = "white-space: pre-wrap; word-wrap: break-word; font-family: monospace;",
                 paste(rv$t1, collapse = "\n"))
      )
    })
  })
  
  # Observe the submit button for processing act headers
  observeEvent(input$submit.h, {
    vario.1 <- input$h1
    vario.2<-input$h2
    print("getting H1")
    print(vario.1)
    print(vario.2)
    t <- get.heads.s(rv$t1, vario.1,vario.2)  # Use the transcript stored in reactiveValues
    rv$t2 <- t$text  # Store the updated text in reactiveValues
    rv$heads <- t$vario  # Store the act headers in reactiveValues
    rv$h1.sf<-vario.1
    rv$h2.sf<-vario.2
    # Update the UI with the processed act headers
    output$spoutput <- renderText(paste("level 1 headers found:\n",paste(rv$heads, collapse = "\n"),collapse = "\n"))
    output$apidoc <- renderUI({
      div(
        style = "height: 70vh; overflow-y: auto; background: #f8f8f8; padding: 10px;",
        tags$pre(style = "white-space: pre-wrap; word-wrap: break-word; font-family: monospace;",
                 paste(rv$t2, collapse = "\n"))
      )
    })
  })
  # observeEvent(input$submit.h2, {
  #   vario <- input$h2
  #   print("getting H2")
  #   t <- get.heads.s(rv$t2, 2,vario)  # Use the transcript stored in reactiveValues
  #   rv$t2 <- t$text  # Store the updated text in reactiveValues
  #   rv$heads <- t$vario  # Store the act headers in reactiveValues
  #   
  #   # Update the UI with the processed act headers
  #   output$spoutput <- renderText(paste("level 2 headers found:\n",paste(rv$heads, collapse = "\n"),collapse = "\n"))
  #   output$apidoc <- renderUI({
  #     div(
  #       style = "height: 70vh; overflow-y: auto; background: #f8f8f8; padding: 10px;",
  #       tags$pre(style = "white-space: pre-wrap; word-wrap: break-word; font-family: monospace;",
  #                paste(rv$t2, collapse = "\n"))
  #     )
  #   })
  # })
  observeEvent(input$submit.sp, {
    vario <- input$speaker
    print("getting speaker")
    #t <- get.speakers(t3, vario)  # Use the transcript stored in reactiveValues
    
    # t <- get.speakers(rv$t2, vario)  # Use the transcript stored in reactiveValues
    t4 <- get.castlist(rv$t2,input$cast)
    print("got cast...")
    t5<-get.front(t4)
    print("got front...")
    t6 <- get.speakers(t5, vario)# Use the transcript stored in reactiveValues
    t2 <- t6  # Store the updated text in reactiveValues
    #t2<-t4
    print("got speakers...")
    rv$speaker <- append(rv$speaker,t2$vario,after = length(rv$vario)) # Store the act headers in reactiveValues
    rv$speaker<-unique(rv$speaker)
    rv$speaker<-rv$speaker[!is.na(rv$speaker)]
    rv$speaker.crit<-append(rv$speaker.crit,t2$eval,after=length(rv$speaker.crit))
    rv$speaker.crit<-rv$speaker.crit[!is.na(rv$speaker.crit)]
    rv$sp.sf<-input$speaker
    # Update the UI with the processed act headers
   # output$acts <- renderText(paste(rv$heads, collapse = "\n"))
    ### remove linebreaks
    sp6<-gsub("%front%","",t2$text)
   # sp6<-t2$text
    t3<-clean.t(sp6,F)
    print("clean.t...")
   # t3<-get.front(t3)
    rv$t3<-t3
    output$apidoc <- renderUI({
      div(
        style = "height: 70vh; overflow-y: auto; background: #f8f8f8; padding: 10px;",
        tags$pre(style = "white-space: pre-wrap; word-wrap: break-word; font-family: monospace;",
                 paste(rv$t3, collapse = "\n"))
      )
    })
    output$spoutput<- renderText(paste("SPEAKERS found:\n",paste(rv$speaker,collapse = "\n"),
                                       "\ncritical lines:\n",paste(rv$speaker.crit,collapse = "\n")))
  })
  observeEvent(input$submit.xml, {
    xml.t<-transform.ezd(rv$t3,output_file)
    xml.test<-c("<p>testxmlrender</p>","<h1>head1</h1><p><stage>stages</stage>paragraph</p>")
   # xml.test<-list.files(".")
#    xml.str<-paste0("<div>",paste0(xml.t),"</div>")
    xml.str<-paste0(xml.t,collapse = "")
    print("----- xmlstr ------")
    print(xml.str)
    b64 <- jsonlite::base64_enc(charToRaw(xml.str))
    
  #  valid<-validate_tei(output_file,"dracor-scheme.rng") # not on M7, cant install jing
    #t2<-xml.t
   # print(valid$ok)
    output$apidoc <- renderUI({
      div(
        style = "height: 70vh; overflow-y: auto; background: #f8f8f8; padding: 10px;",
        tags$pre(style = "white-space: pre-wrap; word-wrap: break-word; font-family: monospace;",
                 paste(xml.t, collapse = "\n"))
      )
    })
    output$xmlrendered <- renderUI({
   # div(id="xml",
     # style="width:100%; height:100%;",
      tags$iframe(
#        src = paste0("data:application/xml;base64,", b64),
       src = "r-tempxmlout.xml",
        style="width:100%; height:100vH; border:none;"
      )
    })
    #)
    
    # output$xmlrendered <- renderUI({
    #   tags$div(id="xml", HTML(paste(xml.t, collapse = "\n")))
    # })
    
    # output$spoutput<-renderText({
    #       paste(paste0("validation success: ",valid$ok),valid$log, collapse = "\n")
    # })
  # })
  })
  # Initialize the outputs
  output$spoutput<- renderText("configure variables left...")
  output$acts <- renderText(paste(rv$heads, collapse = "\n"))
  output$apidoc <- renderUI({
    div(
      style = "height: 70vh; background: #f8f8f8; padding: 10px; color: #888;",
      "Output will appear here after processing."
    )
  })
}