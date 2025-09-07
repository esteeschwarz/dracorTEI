library(shiny)
library(readr)
library(httr)
library(jsonlite)
#library(diffobj)
library(diffr)
library(xml2)
library(dplyr)
library(shinycssloaders)
#library(DT)
#library(tools)
# takes 9.40min to install packages on silver
source("ezd2tei.R")
source("functions.R")
load("default-values.RData")
#sp.default<-"Iwanette,Golowin,Wolsey,Stormond,Bender"
transcript<-"iwanette"
output_file_s_www<-"r-tempxmlout.xml"
output_file<-paste0("www/",output_file_s_www)
output_file<-tempfile("tempxmlout.xml")
output_file_ezd<-"www/ezdmarkup.txt"
output_file_ezd<-tempfile("ezdmarkup.txt")
output_file_pb<-"www/r-tempxmlout_pb.xml"
# Load defaults when app starts
# observe({
#   # Load default speaker names from database
#   #sp_default <- load_default_speakers()
#   defaults<-load_defaults(1)
#   # Update the text input with the loaded default
#   updateTextInput(session, "speaker", value = defaults$speaker)
#   updateTextInput(session, "h1", value = defaults$h1)
#   updateTextInput(session, "h2", value = defaults$h2)
#   updateTextInput(session, "cast", value = defaults$cast)
#   
# })
# Define server logic
function(input, output, session) {
  # Reactive values to store intermediate states
  copyrighted<-TRUE
  copyvc<-!copyrighted
  rv <- reactiveValues(
    t1 = "%empty%input docname...",
    repl = NULL,
    t2 = NULL,
    t3 = NULL,
    heads = "not defined...",
    speaker = array(),
    sp.guess = NULL,
    speaker.crit = array(),
    h1.sf = "",
    h2.sf = "",
    sp.sf = "",
    id.sf = NULL,
    cast = NULL,
    nb.tags = list(),
    selected_id = NULL,
    df = defaults,
    title = "",
    subtitle = "",
    author = "",
    h1.set = FALSE,
    ezd = "no text processed",
    copyrighted = TRUE,
    copyvc = copyvc,
    xmlout = "you have not yet created an xml file or it is copyrighted",
    b64 = jsonlite::base64_enc(charToRaw("<p>no content yet</p>")),
    xmlprocessed = F
  )
  metadf<-fromJSON("repldf.json",flatten = T)
  repldf<-metadf$repl
  rv$repl<-repldf
  
  
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
  
  #ui <- fluidPage(
   # titlePanel("Dataframe Dropdown with Save Functionality"),
  observe({
    updatePickerInput(
     # copyvc<-!rv$copyrighted
      
      session,
      "copy",
      choices = c(rv$copyrighted,rv$copyvc)
    )
  }) 
  observeEvent(input$copy, {
    rv$copyrighted<-input$copy
    print(rv$copyrighted)
    copyrighted<-rv$copyrighted
    print(copyrighted)
    copyvc<-ifelse(copyrighted,FALSE,TRUE)
    print(copyvc)
    rv$copyvc<-copyvc
  })
    # Update dropdown choices when dataframe changes
    observe({
      updatePickerInput(
        session,
        "id_select",
        choices = c("Create new..." = "new", rv$df$id)
      )
    })
    
    # Update name input when selection changes
    observeEvent(input$id_select, {
      if (input$id_select != "new" && !is.null(input$id_select)) {
        selected_row <- rv$df[rv$df$id == input$id_select, ]
       # updateTextInput(session, "cast", value = selected_row$cast)
        rv$selected_id <- input$id_select
      } else {
        updateTextInput(session, "cast", value = "empty")
        rv$selected_id <- NULL
      }
    })
    
    # Save to existing ID
    observeEvent(input$save_btn, {
      req(input$id_select, input$id_select != "new")
      print("saving...")
      row_to_save<-c(input$id_select,input$h1,input$h2,input$speaker,input$cast,input$author,input$title,input$subtitle,input$copy)
      print(row_to_save)
      print(rv$df$id)
      print(rv$df)
      id=input$id_select
      # Update the dataframe
      defaults<-data.frame(rv$df)
      defaults[id,] <- row_to_save
      print(defaults)
      save(defaults,file="default-values.RData")
      
      showNotification("Data saved successfully!", type = "message")
      
      # Your save routine would go here, e.g.:
      # saveRDS(rv$df, "data.rds")
      # write.csv(rv$df, "data.csv", row.names = FALSE)
    })
    observeEvent(input$load_btn, {
      req(input$id_select)
      id<-input$id_select
      print(id)
      cat("observe load...\n")
      print(rv$df[id,"speaker"])
     # print(input$id.defaults.load)
    #  print(head(defaults))
      print(rv$df$speaker[id])
      updateTextInput(session, "speaker", value = rv$df[id,"speaker"])
      updateTextInput(session, "title", value = rv$df[id,"title"])
      updateTextInput(session, "subtitle", value = rv$df[id,"subtitle"])
      updateTextInput(session, "author", value = rv$df[id,"author"])
      
      updateTextInput(session, "h1", value = rv$df[id,"h1"])
      updateTextInput(session, "h2", value = rv$df[id,"h2"])
      updateTextInput(session, "cast", value = rv$df[id,"cast"])
      updateTextInput(session, "copy", value = rv$df[id,"copyrighted"])
      rv$sp.sf<-rv$df[id,"speaker"]
      rv$h1.sf<-rv$df[id,"h1"]
      rv$h2.sf<-rv$df[id,"h2"]
      rv$cast<-rv$df[id,"cast"]
      rv$author<-rv$df[id,"author"]
      rv$title<-rv$df[id,"title"]
      rv$subtitle<-rv$df[id,"subtitle"]
      rv$copyrighted<-rv$df[id,"copyrighted"]
      print(rv$df)
      showNotification("Data loaded successfully!", type = "message")
      
      # Your save routine would go here, e.g.:
      # saveRDS(rv$df, "data.rds")
      # write.csv(rv$df, "data.csv", row.names = FALSE)
    })
    
    # Create new ID
    observeEvent(input$new_btn, {
      req(input$id_select)
      
      # Generate new ID (you can customize this logic)
      new_id <- sprintf("%03d", as.numeric(max(rv$df$id)) + 1)
      new_id <- as.numeric(max(rv$df$id)) + 1
      
      print(new_id)
      print("new id created")
      # Add new row to dataframe
      new_row <- c(id=new_id,h1 = input$h1,h2 = input$h2,speaker = input$speaker,cast = input$cast,
                   author = input$author,title = input$title,subtitle = input$subtitle)
      new_row <- c(id=new_id,h1 = NA,h2 = NA,speaker = NA,cast = NA,
                   author = NA,title = NA,subtitle = NA,copyrighted=TRUE)
      print(new_row)
      ldf<-length(rv$df)
      ln<-length(new_row)
      if(ln>ldf)
        rv$df<-cbind(rv$df,copyrighted=new_row[length(new_row)])
      rv$df <- rbind(rv$df, new_row)
      print("rbind done")
      # Update selection to the new ID
      updatePickerInput(session, "id_select", selected = new_id)
      print(rv$df)
      showNotification(paste("New ID", new_id, "created!"), type = "message")
      
      # Your save routine would go here
    })
    
    # Refresh data (optional - if you want to reset or reload from source)
    observeEvent(input$refresh_btn, {
      rv$df <- defaults
      updateTextInput(session, "cast", value = "")
      updatePickerInput(session, "id_select", selected = "new")
      showNotification("Data refreshed!", type = "message")
    })
    
    # Display the dataframe
    # output$data_table <- renderDT({
    #   datatable(
    #     rv$defaults,
    #     options = list(
    #       pageLength = 10,
    #       autoWidth = TRUE
    #     ),
    #     selection = 'none'
    #   )
    # })
    # 
    # Debug output (optional)
    output$debug_output <- renderPrint({
      cat("Selected ID:", input$id_select, "\n")
      cat("Current name input:", input$cast, "\n")
      cat("Dataframe dimensions:", dim(rv$df), "\n")
    })
  
  

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
    # content=function(file){writeLines(readLines(output_file_ezd),file)}
    content=function(file){writeLines(rv$ezd,file)}
  )
  observeEvent(input$defaults.save,{
    req(input$id_select)
    
    save(rv$df,file="default-values.RData")
#     
#     rv$id.sf<-input$id.defaults.save
#     rv$sp.sf<-input$speaker
#     rv$h1.sf<-input$h1
#     rv$h2.sf<-input$h2
#     rv$cast<-input$cast
# #    rvdf<-data.frame(id=rv$id.sf,h1=rv$h1.sf,h2=rv$h2.sf,speaker=rv$sp.sf,cast=rv$cast)
#     #rvdf<-c(id=1,bla1="zwei")
#     #rvdf[["id"]]
#     rvdf<-c(id=rv$id.sf,h1=rv$h1.sf,h2=rv$h2.sf,speaker=rv$sp.sf,cast=rv$cast)
#     cat("observe sf...\n")
#     print(rvdf)
#     save_defaults(rvdf)
    updateTextInput(session, "id.defaults.save", value = "settings saved...")
#     # defaults$h1[4]<-".ufzug"
     #defaults$h2[4]<-".uftritt"
    # defaults$cast<-"Personen."
  #    save(defaults,file = "default-values.RData")
  })
  observeEvent(input$defaults.load,{
    defaults<-load_defaults(input$id.defaults.load)
    cat("observe load...\n")
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
    res <- check_regex(repldf)
    if (!res$success) {
      print("regex error...")
      showNotification(res$error, type = "error")
      output$proutput<- renderText(res$error)
      
    } else {
      # proceed with res$result
    print("regex okay...")
    # replchk<-check_regex(repldf)
    # if(typeof(replchk)=="character"){
    #   print(replchk)
    #   output$proutput<- renderText(replchk)
    # }
    # if(typeof(replchk)!="character"){
    repldf<-res$result
    output$proutput<- renderText("replacements loaded...")
    showNotification("replacements loaded...", type = "message")
    
    repldf$replace<-gsub("\\\\n","\\\\\n",repldf$replace)
    # repldf$replace<-gsub("[\\\\]([1-9])","\\\\\\\\\1",repldf$replace)
    repldf$replace<-gsub("[\\]([1-9])","\\\\1",repldf$replace)
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
    }
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
    showNotification("processing transcript...", type = "message")
    
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
    showNotification("transcript loaded...", type = "message")
    
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
    rv$t1<-"%ezd%"
    rv$t3<-t4
    output$processed <- renderUI({
      div(
        style = "height: 70vh; overflow-y: auto; background: #f8f8f8; padding: 10px;",
        tags$pre(style = "white-space: pre-wrap; word-wrap: break-word; font-family: monospace;",
                 paste(rv$t3, collapse = "\n"))
      )
    })
    showNotification("ezd markup transcript loaded...", type = "message")
    
    output$proutput <- renderText("markup transcript uploaded\n")
  })
  # Observe the submit button for fetching the transcript
  observeEvent(input$submit.doc, {
    showNotification("fetching transcript from transkribus DB...", type = "message")
    
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
    showNotification("transcript fetched...", type = "message")
    
#    output$proutput <- renderText("transcript fetched...\n")
    
  })
  
  # Observe the submit button for processing act headers
  observeEvent(input$submit.h, {
    showNotification("processing headers...", type = "message")
#    output$proutput <- renderText("processing headers...\n")
    
    vario.1 <- input$h1
    vario.2<-input$h2
    print("getting H1")
    print(vario.1)
    print(vario.2)
    t <- get.heads.s(rv$t1, vario.1,vario.2)  # Use the transcript stored in reactiveValues
    rv$t2 <- t$text  # Store the updated text in reactiveValues
    rv$heads <- t$vario  # Store the act headers in reactiveValues
   # heads<-data.frame(found=1:length(rv$heads),head=rv$heads)
    rv$h1.sf<-vario.1
    rv$h2.sf<-vario.2
    # heads$h1<-rv$h1.sf
    # heads$h2=rv$h2.sf
    print(rv$heads)
    if(length(rv$heads)>0){
      rv$h1.set<-TRUE
      rv$ezd<-rv$t2
    }
   # writeLines(rv$t2,"www/ezdmarkup.txt")
    # Update the UI with the processed act headers
   # output$proutput <- renderText(paste("level 1/2 headers found:\n",paste(rv$heads, collapse = "\n"),collapse = "\n"))
    #output$proutput<-renderDT(rv$heads)
    output$processed <- renderUI({
      div(
        style = "height: 70vh; overflow-y: auto; background: #f8f8f8; padding: 10px;",
        tags$pre(style = "white-space: pre-wrap; word-wrap: break-word; font-family: monospace;",
                 paste(rv$t2, collapse = "\n"))
      )
    })
    output$pr_progress<-renderUI({
      div(
        style = "height: 70vh; overflow-y: auto; background: #f8f8f8; padding: 10px;",
        tags$pre(style = "white-space: pre-wrap; word-wrap: break-word; font-family: monospace;",
                 paste(rv$heads, collapse = "\n"))
      )
    })
    showNotification("headers processed...", type = "message")
    
    # output$pr_progress<-renderDT(
    #   iris
    #   )
  })
  observeEvent(input$guess.sp, {
    print("guess speakers")
    showNotification("guessing speakers...", type = "message")
    
    sp.guess<-guess_speaker(rv$t2,input$cast)
    rv$sp.guess<-sp.guess
    output$proutput<- renderText(paste("SPEAKERS guessed:\n",paste(sp.guess,collapse = "\n")))
    updateTextInput(session, "speaker", value = paste0(sp.guess,collapse = ","))
  })
  #################################
  
    observeEvent(input$submit.sp, {
    vario <- input$speaker
    rswitch<-input$rswitch
    req(rv$h1.set)
    showNotification("processing speakers...", type = "message")
    
    print("getting speaker")
    #t <- get.speakers(t3, vario)  # Use the transcript stored in reactiveValues
    print(rv$cast)
    # t <- get.speakers(rv$t2, vario)  # Use the transcript stored in reactiveValues
    t4 <- get.castlist(rv$t2,rv$cast)
    t4<-t4$lines
    print("got cast...")
    t5<-get.front(t4)
    print("got front...")
    t6 <- get.speakers(t5, vario,rswitch)# Use the transcript stored in reactiveValues
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
   # sp6<-gsub("%hnl%","",t2$text)
    # sp6<-t2$text
    t3<-clean.t(sp6,F,rv$repl)
    t3<-gsub("%hnl%","",t3)
    t3<-gsub("^\\((.+)\\)$","$\\1",t3)
    print("clean.t...")
   # t3<-get.front(t3)
    rv$t3<-t3
    rv$ezd<-t3
   # writeLines(rv$t3,"www/ezdmarkup.txt")
    
    output$processed <- renderUI({
      div(
        style = "height: 70vh; overflow-y: auto; background: #f8f8f8; padding: 10px;",
        tags$pre(style = "white-space: pre-wrap; word-wrap: break-word; font-family: monospace;",
                 paste(rv$t3, collapse = "\n"))
      )
    })
    output$pr_progress<-renderUI({
      div(
        style = "height: 70vh; overflow-y: auto; background: #f8f8f8; padding: 10px;",
        tags$pre(style = "white-space: pre-wrap; word-wrap: break-word; font-family: monospace;",
                 paste("SPEAKERS found:\n",paste(rv$speaker,collapse = "\n"),
                       "\ncritical lines:\n",paste(rv$speaker.crit,collapse = "\n")))
      )
    })
    showNotification("speaker processed...", type = "message")
    
    # output$proutput<- renderText(paste("SPEAKERS found:\n",paste(rv$speaker,collapse = "\n"),
    #                                    "\ncritical lines:\n",paste(rv$speaker.crit,collapse = "\n")))
  })
  observeEvent(input$submit.xml, {
    author<-rv$author
    title<-rv$title
    subtitle<-rv$subtitle
    meta<-list(author=author,title=title,subtitle=subtitle)
    #    output$proutput <- renderText("processing ezd > TEI...\n")
    showNotification("transform ezd > TEI...", type = "message")
  #   output_text <- capture.output({
  #     print("Starting process...")
  #     Sys.sleep(1)
  #     print("Step 1 complete")
  #     Sys.sleep(1)
  #     print("Step 2 complete")
  #     xml.t<-transform.ezd(rv$t3,output_file,meta)
  #     print("finished...")
  #   })
  #   
  #   # Update the reactive value
  #   console_text(paste(output_text, collapse = "\n"))
  # 
  # 
  # output$pr_progress <- renderText({
  #   console_text()
  # })
    xml.t<-transform.ezd(rv$t3,output_file,meta)
    xml.test<-c("<p>testxmlrender</p>","<h1>head1</h1><p><stage>stages</stage>paragraph</p>")
   # xml.test<-list.files(".")
  # xml.str<-paste0("<div>",paste0(xml.test),"</div>")
     xml.str<-paste0(xml.t,collapse = "")
    #print("----- xmlstr ------")
   # print(xml.str)
   b64 <- jsonlite::base64_enc(charToRaw(xml.str))
    rv$b64<-b64
    rv$xmlout<-xml.t
    
  #  valid<-validate_tei(output_file,"dracor-scheme.rng") # not on M7, cant install jing
    #t2<-xml.t
   # print(valid$ok)
#    output$proutput <- renderText("ezd > TEI processed...\n")
    showNotification("TEI process finished...", type = "message")
    rv$xmlprocessed<-T
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
       src = paste0("data:application/xml;base64,", rv$b64),
       # src = paste0("data:text/html,", rv$xmlout),
       
       # src = "r-tempxmlout.xml",
        # src = output_file_s_www, # this wks.
        # src = output_file,
        style="width:100%; height:100vH; border:none;"
      )
    })
  })
  
 observeEvent(input$compare, {
    req(rv$xmlprocessed,rv$xmlprocessed==TRUE)
    ifelse(rv$t1=="%ezd%",text1<-rv$t3,
           text1 <- rv$t1)
   print(text1)
   
    #text2 <- paste0(rv$t3,collapse = "<nl>")
   doc<-read_xml(output_file)
   texts <- xml_text(xml_find_all(doc, "//text()"))
   
    text2 <- texts
    tempapi<-tempfile("api.txt")
    writeLines(text1,tempapi)
    tempproc<-tempfile("proc.txt")
    writeLines(text2,tempproc)
    # Split into lines for better diff display
    # lines1 <- unlist(strsplit(text1, "\n"))
    # lines2 <- unlist(strsplit(text2, "\n"))
    # 
#?    renderDiffr
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
            before = "uploaded",
            after = "processed",
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