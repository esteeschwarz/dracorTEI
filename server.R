library(shiny)
library(readr)
library(httr)
library(jsonlite)
#library(diffobj)
library(diffr)
library(xml2)
#library(tools)
# takes 9.40min to install packages on silver
source("ezd2tei.R")
source("functions.R")
#sp.default<-"Iwanette,Golowin,Wolsey,Stormond,Bender"
transcript<-"iwanette"
output_file<-"www/r-tempxmlout.xml"
output_file_ezd<-"www/ezdmarkup.txt"
# Load defaults when app starts
observe({
  # Load default speaker names from database
  #sp_default <- load_default_speakers()
  defaults<-load_defaults(1)
  # Update the text input with the loaded default
  updateTextInput(session, "speaker", value = defaults$speaker)
  updateTextInput(session, "h1", value = defaults$h1)
  updateTextInput(session, "h2", value = defaults$h2)
  updateTextInput(session, "cast", value = defaults$cast)
  
})
# Define server logic
function(input, output, session) {
  # Reactive values to store intermediate states
  rv <- reactiveValues(
    t1 = "input docname...",
    repl = NULL,
    t2 = NULL,
    t3 = NULL,
    heads = "not defined...",
    speaker = array(),
    speaker.crit = array(),
    h1.sf = "",
    h2.sf = "",
    sp.sf = "",
    id.sf = NULL,
    cast = NULL,
    nb.tags = list()
  )
  metadf<-fromJSON("repldf.json",flatten = T)
  repldf<-metadf$repl
  rv$repl<-repldf
  
  output$md_html <- renderUI({
    md_file <- "about-md.md"
    html <- markdown::markdownToHTML(md_file, fragment.only = TRUE)
    HTML(html)
  })
  output$nb <- renderUI({
    # div(id="xml",
    # style="width:100%; height:100%;",
    tags$iframe(
      #        src = paste0("data:application/xml;base64,", b64),
      src = "about-nb.nb.html",
      style="width:100%; height:100vH; border:none;"
    )
  })
  output$nb <- renderUI({
    rmd_file <- "about-nb.Rmd"
    html_file <- tempfile(fileext = ".nb.html")
    html_file <- "www/about-nb.html"
    rmarkdown::render(rmd_file, output_file = html_file, output_format = "html_notebook", quiet = TRUE)
#    html <- paste(readLines(html_file, warn = FALSE), collapse = "\n")
    #html <- paste(readLines(, warn = FALSE), collapse = "\n")
    tags$iframe(
 #     src = paste0("data:application/xml;base64,", html),
      src = "about-nb.nb.html",
      style="width:100%; height:100vH; border:none;"
    )
    # html_file<-"about-nb.nb.html"
    # ht2<-read_html(html_file)
    # nodes <- xml_find_all(ht2, "//*[self::script or self::link]")
    # #all.scr<-xml_find_all(ht2,"//script")
    # #all.link<-xml_find_all(ht2,"//link")
    # rv$nb.tags<-nodes
    # #rv$nb.tags$link<-all.link
    #all.scr[1]
   # HTML(html)
  })
  # output$dynamic_head <- renderUI({
  #   deps<-rv$nb.tags
  #   deps <- extract_head_nodes("about-nb.nb.html")
  #  # print(deps)
  #   tagList(deps)
  # })
  
  output$downloadXML<-downloadHandler(
    filename="ezdxmlout.xml",
    content=function(file){writeLines(readLines(output_file),file)}
  )
  output$downloadEZD<-downloadHandler(
    filename="ezdmarkup.txt",
    content=function(file){writeLines(readLines(output_file_ezd),file)}
  )
  observeEvent(input$defaults.save,{
    rv$id.sf<-input$id.defaults.save
    rv$sp.sf<-input$speaker
    rv$h1.sf<-input$h1
    rv$h2.sf<-input$h2
    rv$cast<-input$cast
#    rvdf<-data.frame(id=rv$id.sf,h1=rv$h1.sf,h2=rv$h2.sf,speaker=rv$sp.sf,cast=rv$cast)
    #rvdf<-c(id=1,bla1="zwei")
    #rvdf[["id"]]
    rvdf<-c(id=rv$id.sf,h1=rv$h1.sf,h2=rv$h2.sf,speaker=rv$sp.sf,cast=rv$cast)
    cat("observe sf...\n")
    print(rvdf)
    save_defaults(rvdf)
    updateTextInput(session, "id.defaults.save", value = "settings saved...")
    # defaults$h1[4]<-".ufzug"
     #defaults$h2[4]<-".uftritt"
    # defaults$cast<-"Personen."
  #    save(defaults,file = "default-values.RData")
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
    rv$sp.sf<-defaults$speaker
    rv$h1.sf<-defaults$h1
    rv$h2.sf<-defaults$h2
    rv$cast<-defaults$cast
  })
  observeEvent(input$upload_repl,{
    file<-input$upload_repl$datapath
    repldf<-read.csv(file)
    print(repldf)
    repldf$replace<-gsub("\\\\n","\\\\\n",repldf$replace)
    #  repldf$replace<-gsub("W","dummy",repldf$replace)
    
    print(repldf$replace)
    #    rv$repl<-repldf
    # metadf<-fromJSON("repldf.json",flatten = T)
    repl1<-rv$repl
    print(colnames(repl1)[1:3])
    print(head(repl1))
    repldf$id<-1
    mode(repl1$id)<-"double"
    mode(repl1$string1)<-"character"
    mode(repl1$string2)<-"character"    
    colnames(repl1)[2:3]<-c("find","replace")
    
    repl2<-bind_rows(repldf,repl1[,1:3])
    colnames(repl2)[2:3]<-c("string1","string2")
    print(head(repl2))
    rv$repl<-repl2
    t3<-clean.t(rv$t1,1,rv$repl)
    #t3
    rv$t1<-t3
  })
#   observeEvent(input$upload_repl,{
#     file<-input$upload_repl$datapath
#     repldf<-read.csv(file)
#     print(head(repldf))
# #    rv$repl<-repldf
#     # metadf<-fromJSON("repldf.json",flatten = T)
#     repl1<-rv$repl
#     print(colnames(repl1)[1:3])
#     print(head(repl1))
#     repldf$id<-1
#     mode(repl1$id)<-"double"
#     mode(repl1$string1)<-"character"
#     mode(repl1$string2)<-"character"    
#     colnames(repl1)[2:3]<-c("find","replace")
#     
#     repl2<-bind_rows(repl1[,1:3],repldf)
#     colnames(repl2)[2:3]<-c("string1","string2")
#     print(head(repl2))
#     rv$repl<-repl2
#   })
  #test
 # repldf<-read.csv("~/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/actuel/breithaupt/repldf.csv")
  # mode(repl1$id)<-"double"
  # mode(repl1$string1)<-"character"
  # mode(repl1$string2)<-"character"
  observeEvent(input$upload_tr,{
    output$proutput <- renderText("processing...\n")
    file<-input$upload_tr
    # ext<-tools::file_ext(file$datapath)
    # req(file)
    # validate(need(ext=="txt","please upload a plain text file"))
    t4<-readLines(file$datapath)
    print(t4)
    t3<-repl.um(t4)
    t3
    t3<-clean.t(t3,1,rv$repl)
    t3
    rv$t1<-t3
    output$apidoc <- renderUI({
      div(
        style = "height: 70vh; overflow-y: auto; background: #f8f8f8; padding: 10px;",
        tags$pre(style = "white-space: pre-wrap; word-wrap: break-word; font-family: monospace;",
                 paste(rv$t1, collapse = "\n"))
      )
    })
    output$proutput <- renderText("transcript fetched...\n")
  })
  observeEvent(input$upload_ezd,{
    output$proutput <- renderText("processing...\n")
    file<-input$upload_ezd
    # ext<-tools::file_ext(file$datapath)
    # req(file)
    # validate(need(ext=="txt","please upload a plain text file"))
    t4<-readLines(file$datapath)
    #print(t4)
   # t3<-repl.um(t4)
    #t3
    #t3<-clean.t(t3,1,rv$repl)
    #t3
    rv$t3<-t4
    output$processed <- renderUI({
      div(
        style = "height: 70vh; overflow-y: auto; background: #f8f8f8; padding: 10px;",
        tags$pre(style = "white-space: pre-wrap; word-wrap: break-word; font-family: monospace;",
                 paste(rv$t3, collapse = "\n"))
      )
    })
    output$proutput <- renderText("markup transcript fetched...\n")
  })
  # Observe the submit button for fetching the transcript
  observeEvent(input$submit.doc, {
    output$apidoc <- renderUI({ div(tags$pre("Processing...")) })  # Show a loading message
    output$proutput <- renderText("processing...\n")
    
    # Fetch the transcript
    transcript <- input$transcript
    tlist <- get.transcript(transcript)  # Store the transcript in reactiveValues
    t1<-tlist$txraw
    t4<-tlist$tlines
    t4
    t3<-repl.um(t4)
    t3
    t3<-clean.t(t3,1,rv$repl)
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
    output$proutput <- renderText("transcript fetched...\n")
    
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
    writeLines(rv$t2,"www/ezdmarkup.txt")
    # Update the UI with the processed act headers
    output$proutput <- renderText(paste("level 1 headers found:\n",paste(rv$heads, collapse = "\n"),collapse = "\n"))
    output$processed <- renderUI({
      div(
        style = "height: 70vh; overflow-y: auto; background: #f8f8f8; padding: 10px;",
        tags$pre(style = "white-space: pre-wrap; word-wrap: break-word; font-family: monospace;",
                 paste(rv$t2, collapse = "\n"))
      )
    })
  })
  observeEvent(input$submit.sp, {
    vario <- input$speaker
    print("getting speaker")
    #t <- get.speakers(t3, vario)  # Use the transcript stored in reactiveValues
    print(rv$cast)
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
    t3<-clean.t(sp6,F,rv$repl)
    print("clean.t...")
   # t3<-get.front(t3)
    rv$t3<-t3
    writeLines(rv$t3,"www/ezdmarkup.txt")
    
    output$processed <- renderUI({
      div(
        style = "height: 70vh; overflow-y: auto; background: #f8f8f8; padding: 10px;",
        tags$pre(style = "white-space: pre-wrap; word-wrap: break-word; font-family: monospace;",
                 paste(rv$t3, collapse = "\n"))
      )
    })
    output$proutput<- renderText(paste("SPEAKERS found:\n",paste(rv$speaker,collapse = "\n"),
                                       "\ncritical lines:\n",paste(rv$speaker.crit,collapse = "\n")))
  })
  observeEvent(input$submit.xml, {
    xml.t<-transform.ezd(rv$t3,output_file)
    xml.test<-c("<p>testxmlrender</p>","<h1>head1</h1><p><stage>stages</stage>paragraph</p>")
   # xml.test<-list.files(".")
#    xml.str<-paste0("<div>",paste0(xml.t),"</div>")
    xml.str<-paste0(xml.t,collapse = "")
    #print("----- xmlstr ------")
   # print(xml.str)
    b64 <- jsonlite::base64_enc(charToRaw(xml.str))
    
  #  valid<-validate_tei(output_file,"dracor-scheme.rng") # not on M7, cant install jing
    #t2<-xml.t
   # print(valid$ok)
    output$processed <- renderUI({
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
  })
  
 observeEvent(input$compare, {
    text1 <- rv$t1
    text1<-paste0(text1,collapse = " ")
    text2 <- paste0(rv$t3,collapse = "<nl>")
    tempapi<-tempfile("api.txt")
    writeLines(text1,tempapi)
    tempproc<-tempfile("proc.txt")
    writeLines(text2,tempproc)
    # Split into lines for better diff display
    # lines1 <- unlist(strsplit(text1, "\n"))
    # lines2 <- unlist(strsplit(text2, "\n"))
    # 
?    renderDiffr
    tryCatch(({
      output$diff_output <- renderDiffr({
       # input$compare
        #text1 <- rv$t1
        #text2 <- rv$t3
        # tempapi<-tempfile("api.txt")
        # writeLines(rv$t1,tempapi)
        # tempproc<-tempfile("proc.txt")
        # writeLines(rv$t3,tempproc)
        # div(id="diff",style="width:100%;height:100vH;",
        isolate({
          diffr(
            file1 = tempapi,
            file2 = tempproc,
            before = "Original",
            after = "Corrected",
            contextSize = 3,
            wordWrap = TRUE
          )
        })
        # )
      })
    }))
  })
    # Create diff object
    # tryCatch({
    #   diff <- diffobj::diffChr(
    #     lines1, 
    #     lines2,
    #     mode = "sidebyside",
    #     format = "html",
    #     #contextSize = input$context_size,
    #     style = list(html.output = "page")
    #   )
    #   
  #     # Convert to HTML
  #     htmltools::HTML(as.character(diff))
  #   }, error = function(e) {
  #     HTML(paste0("<div class='alert alert-danger'>Error: ", e$message, "</div>"))
  #   })
  # })
  # 
  # output$diff_output <- renderUI({
  #   if (input$compare == 0) {
  #     return(HTML("<div class='alert alert-info'>Click 'Compare Texts' to see the differences</div>"))
  #   }
  #   
  #   diff_result()
  # })
  # Initialize the outputs
  output$proutput<- renderText("configure variables left...")
  output$acts <- renderText(paste(rv$heads, collapse = "\n"))
  output$apidoc <- renderUI({
    div(
      style = "height: 70vh; background: #f8f8f8; padding: 10px; color: #888;",
      "Output will appear here after processing."
    )
  })
}