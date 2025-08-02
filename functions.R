get.transcript<-function(transcript){
  r<-GET(paste0("https://ids.dh-index.org/api/trans?transcript=",transcript))
  t<-content(r,"text")
  # t<-clean.t(t)
  txtemp<-tempfile("transtemp.txt")
  writeLines(t,txtemp)
  t1<-readLines(txtemp)
  
  empty<-c(""," ","  ")
  m<-t1%in%empty
  
  t1<-t1[!m]
  #t1<-t
  return(list(txraw=t,tlines=t1))
}
# tlist<-get.transcript("iwanette")
# text<-tlist$tlines
# traw<-tlist$txraw
# defaults<-data.frame(id=1,h1="Act|Akt|Handlung",h2="Szene|Scene",speaker="Stormond,Iwanette,Golowin,Bender,Wolsey")
# save(defaults,file = "default-values.RData")
load_defaults <- function(id=F) {
  # Replace this with your actual database query
  # Example database connection and query:
  idx<-id
  tryCatch({
    # con <- dbConnect(RSQLite::SQLite(), "your_database.db")
    # result <- dbGetQuery(con, "SELECT speaker_names FROM defaults WHERE id = 1")
    # dbDisconnect(con)
    # return(result$speaker_names[1])
    load("default-values.RData")
    print("loaded...")
    print(id)
    cat("----\n")
    
    print(head(defaults))
    #ifelse(!id,idx<-1:length(defaults$id),idx<-id)
    print(idx)
    cat("----\n")
    return(defaults[idx,])
    # For demonstration, returning a mock default value
    return("Character1, Character2, Narrator, Chorus")
  }, error = function(e) {
    # Fallback default if database is unavailable
    defaults<-data.frame(id=1,cast="Personal.",h1="Act|Akt|Handlung|.ufzug",h2="Szene|Scene|.uftritt",speaker="Stormond,Iwanette,Golowin,Bender,Wolsey")
    return(defaults[idx,])
    return("Speaker1, Speaker2, Speaker3")
  })
}



repl.um<-function(t){
umrepl<-c(A="Ä",O="Ö",U="Ü",a="ä",o="ö",u="ü")
t3<-t
t3s<-t3
lim<-867
r<-4
for(r in 1:length(umrepl)){
  x<-umrepl[r]
  k<-47
  for (k in 1:length(t3)){
    
    ltint<-utf8ToInt(t3[k])
    ltint
    t3[k]
    mu<-ltint>lim
    #mu<-ltint>lim
    mu
    mup<-which(mu)-1
    u<-intToUtf8(ltint[mup])
    u<-unlist(strsplit(u,""))
    u
    am<-u==names(x)|u==x
    u<-u[am]
    am<-u==names(x)|u==x
    if(sum(am)>0) {
     # mp
      for (mp in mup){  
        if(ltint[mp+1]!=1000&(intToUtf8(ltint[mp])%in%names(x)|intToUtf8(ltint[mp])%in%x)) {
          ltint[mp]<-utf8ToInt(x)
          ltint[mp+1]<-1000
        }
      }
      mout<-ltint==1000
      lt2<-ltint[!mout]
      t3[k]<-intToUtf8(lt2)
    }
  }
  
}
return(t3)
#t3[548:570]
}
r<-F
#t<-t3
r<-F
clean.t<-function(t,r){
  txtemp<-tempfile("txraw.txt")
  writeLines(t,txtemp)
  library(readtext)
  t2<-readtext(txtemp)$text
  # metadf<-read.table("metadf.csv",sep = "\t")
  metadf<-fromJSON("repldf.json",flatten = T)
  repldf<-metadf$repl
  repldf
  #repldf[11,]
  if(sum(r)>0)
    repldf<-repldf[repldf$id==r,]
  repldf
  #r<-11
  m1<-grepl("%cast%",t2)
  print(m1)
  for(r in 1:length(repldf$id)){
    t2<-gsub(repldf$string1[r],repldf$string2[r],t2,perl = T)
  }
  writeLines(t2,txtemp)
  
  t3<-readLines(txtemp)
  t3[1:150]
  t3<-gsub("%spknl%|%cast%","",t3)
  return(t3)
}
save_defaults<-function(rvdf){
  load("default-values.RData")
  print(head(defaults))
  defaults[rvdf$id,]<-rvdf
  save(defaults,file="default-values.RData")
  print("saved")
  
}
transform.ezd<-function(ezd){
  #ezdtemp<-tempfile("ezd.txt")
  #writeLines(ezd,ezdtemp)
  #xmlout<-tempfile("xmlout.xml")
  xmlout<-"r-tempxmlout.xml"
  writeLines(ezd,"ezdmarkup.txt")
  parse_drama_text(ezd,xmlout)
  return(readLines(xmlout))
}
### preprocess raw text
headx.1<-"Aufzug"
headx.2<-"Auftritt"
#t1<-t3
get.heads.s<-function(t1,headx.1="(Akt|Act|Handlung)",headx.2="(Szene|Scene)"){
  numer<-c("(Erst|Zweyt|Zweit|Dritt|Viert|Fünfte|Fuenft|Sechs|Sieben|Acht|Neun|Zehn|Elf|Zwoelf|Zwölf|Dreizehn|Dreyzehn)")
  #  ifelse(level==1,headx<-headx.1,headx<-headx.2)
  # ifelse(level==1,ph<-"#",ph<-"##")
  regx.1<-paste0("^.+?",numer,".+(",headx.1,")\\.")
  regx.1<-paste0("^+?",numer,".+(",headx.1,")\\.")
  regx.1
  regx.2<-paste0("^.+?",numer,".+(",headx.2,")\\.")
  regx.2<-paste0("^+?",numer,".+(",headx.2,")\\.")
  regx.2
  m1<-grep(regx.1,t1)
  t1
  t2<-t1
  t2[m1]<-paste0("# ",t2[m1])
  m2<-grep(regx.2,t1)
  #t2<-t1
  t2[m2]<-paste0("## ",t2[m2])
  #return(t1)
  return(list(vario=t1[m1],text=t2))
}
get.heads.dep<-function(t1,headx="(Akt|Act"){
  numer<-c("(Erst|Zweyt|Zweit|Dritt|Viert|Fünfte|Fuenft|Sechs|Sieben|Acht|Neun|Zehn|Elf|Zwoelf|Zwölf|Dreizehn|Dreyzehn)")
  regx<-paste0("^.+?",numer,".+(",headx,")\\.")
  m<-grep(regx,t1)
  t2<-t1
  t2[m]<-paste0("#ACT ",t2[m])
  #return(t1)
  return(list(vario=t1[m],text=t2))
}
#sp<-"Der ältere Stormond,Stormond,Bender,Der Medicus,Medicus,Wolsey,Iwanette,Golowin"
#t1<-t3
#t3
# line<-text[15]
# line<-"Personen"
# lines<-sp1$text
 k<-89
# # t3
# # cast<-"Personen."
# lines[1:100]
# line<-lines[k]
#line
 l<-89
 get.front<-function(lines){
  for (l in 1:length(lines)){
    line<-lines[l]
    line
    if(str_detect(line,"^@front.*",)){
      r1<-l+1
      r<-r1:length(lines)
      m<-str_detect(lines[r],"^[$#^]",)
      mw<-which(m)
      mwl<-r[mw]
      mw<-mwl[1]-1
      mw<-r1:mw
      lines[mw]
      lines[mw]<-paste0(lines[mw],"%front%")
      m2<-grepl("^@",lines[mw])
      lines[mw]<-gsub("@","",lines[mw])
      #castlist.t<-lines[mw]
      #castlist.t
   }
  }
  return(lines)
 }
# lines<-readLines("ezdmarkup.txt")
 l<-97
#line<-lines[l]
# cast<-"Personal"
 # lines<-t2
 # t2[1:100]
 # line<-lines[l]
 # line
 # lines[1:150]
 get.castlist<-function(lines,cast){
  for (l in 1:length(lines)){
    line<-lines[l]
    if(str_detect(line,paste0("^",cast,".?$"),)){
      parts<-str_match(line,"\\^(.*)")
      parts
      write(parts,"debug.txt",append = T)
      desc<-parts[2]
      desc
      r<-l:length(lines)
      m<-str_detect(lines[r],"^[$#]",)
      mw<-which(m)
      mw<-mw[1]
      mw<-l:r[mw-1]
      mw
      lines[mw]<-paste0(lines[mw],"%cast%")
      lines[mw]<-gsub("%spknl%","",lines[mw])
      write(mw,"debug.txt",append = T)
      m2<-str_detect(lines[mw],"^@",)
      sum(m2)
      lines[mw[m2]]
      lines[mw[m2]]<-gsub("@|%spknl%","",lines[mw[m2]])
    }
    lines[1:100]
    if(str_detect(line,"\\^",)){
    parts<-str_match(line,"\\^(.*)")
    parts
    write(parts,"debug.txt",append = T)
    desc<-parts[2]
    desc
    r<-l:length(lines)
    m<-str_detect(lines[r],"^[$#]",)
    mw<-which(m)
    mw<-mw[1]
    mw<-l:r[mw-1]
    mw
    lines[mw]<-paste0(lines[mw],"%cast%")
    lines[mw]<-gsub("%spknl%","",lines[mw])
    write(mw,"debug.txt",append = T)
    m2<-str_detect(lines[mw],"^@",)
    sum(m2)
    lines[mw[m2]]
    lines[mw[m2]]<-gsub("@|%spknl%","",lines[mw[m2]])
  }
  }
   lines[1:100]
  return(lines)
 }
 #lines[1:150]
# t6<-get.castlist(text)
# t6
get.speakers<-function(t1,sp){
  sp01<-unlist(strsplit(sp,","))
  sp1<-paste0("(",paste0(sp01,collapse = "|"),")")
  sp2<-paste0(sp01,".")
  print(sp1)
  #regx<-paste0("^.+?",sp1,"\\.")
  regx<-paste0("^",sp1,"\\.$")
  print(regx)
  m<-grep(regx,t1)
  print(m)
  t2<-t1
  sp2
  crit<-t2[m]%in%sp2 # if {speaker}. appears in text
  print(crit)
  crit.sp<-t2[m][!crit]
  crit.sp
  t2[m]
  crit.sp<-crit.sp[!is.na(crit.sp)]
  t2[m][crit]<-paste0("@",t2[m][crit],"%spknl%")
  t2[m]
  #t2<-gsub("%cast%","",t2)
  print(crit.sp)
  wc<-which(!crit)
  print(wc)
  sp.move<-function(){
    for (k in wc){
      p<-m[k]
      s<-strsplit(t2[p],"\\.")
      print(s)
      t2[p]<-paste0(s[[1]][1],".")
      s[[1]][1]<-""
      rest<-paste0(s)
      
      t2<-append(t2,rest,after = p)
      
      
    }
  }
  spx<-sp.move()
  # unlist(s)
  # t<-lapply(s,function(x){
  #   st<-x[1]
  #   nd<-x[2]
  # })  
  crit.m<-length(m)-sum(!crit)
  print(crit.m)
  
  #return(t1)
  writeLines(t2,"ezdmarkup.txt")
  return(list(vario=t1[m][crit],text=t2,eval=crit.sp))
  
}