library(R.utils)
library(fuzzyjoin)
library(stringi)
#library(Hmisc)
get.str.dist<-function(t1,sp.cast,sp.guess){
 # sp1<-c("Billing","Katrine Stockmann","dummyNO")
  sp1<-unique(unlist(strsplit(sp.cast," ")))
  sp1<-sp1[!is.na(sp1)]
  sp.guess<-sp.guess[!is.na(sp.guess)]
  x<-data.frame(id=1,cast=sp.guess)
  #y<-data.frame(id=2,cast=names(sp1))
  y<-data.frame(id=2,cast=sp1)
  x$cast<-gsub("^[ \t]{1,}[ \t.]$","",x$cast)
  get.m<-function(dist,met,mode){
    x$cast<-gsub("^[ \t]{1,}[ \t.]$","",x$cast)
    x$cast<-gsub("%cast%","",x$cast)
    y$cast<-gsub("^[ \t]{1,50}(.+)[ \t.]?$","\\1",y$cast)
    y$cast<-gsub("^[ \t]{1,}[ \t.]$","",y$cast)
    j1<-stringdist_join(x,y,max_dist = dist,by="cast",mode = mode,method = met,distance_col = "dist",ignore_case = T)
    ms<-j1$cast.x%in%j1$cast.y
    j1$dist[ms]<-j1$dist[ms]+(sd(j1$dist)*3) # score higher if
    #j1<-stringdist(sp1,sp.cast,method = "jw")
    j1
    l1<-list()
    x<-j1$cast.y[1]
    x
    ### new
    tj<-table(j1$cast.y)
    j1$fd<-NA
    for(k in names(tj)){
      r<-j1$cast.y==k
      j1$fd[r]<-j1$dist[r]/tj[k]
    }
    j1
    mj<-mean(j1$fd)
    sj<-sd(j1$fd)
    sj
    sel<-j1$fd<=mj
    print("freq based stringdist...")
    print(j1$cast.y[sel])
    
    ##############
      s2<-lapply(j1$cast.y, function(x){
      
      s4<-sum(j1$dist[j1$cast.y==x])
      l1[[x]]<-s4
      return(l1)
    })
    s2<-unlist(s2)
    print(s2)
    print(mean(s2))
    print(sd(s2))
    m1<-mean(s2)
    #s3
    #sd1<-sd(s2)
    #s3<-c((m1-sd1):(m1+sd1))
    #print(s3)
    m1<-mean(s2)
    length(s2)
    sd1<-sd(s2)
    sdc<-c((m1-sd1):(m1+sd1))
    sdc
    sdmin<-min(sdc)
    sdmax<-max(sdc)
    s4<-s2<sdmin
    s4
    #s3
    s2[s4]
    #y$cast[!y$cast%in%s2[s4]]
    
    #j3<-unique(j1$cast.y[min(s1)<=j1$dist|j1$dist>=max(s1)])
    sd(j1$dist)
    s1<-c((mean(j1$dist)-sd(j1$dist)):mean(j1$dist)+sd(j1$dist))
    s1<-c(min(s1):max(s1))
    s1
    j2<-unique(j1$cast.y[min(s1)<=j1$dist|j1$dist>=max(s1)])
    #??stringdist_join
    print(j1)
    print(y$cast[!y$cast%in%j2])
    #print(j2)
    return(list(s3=s2,sel=j1$cast.y[sel]))
  }
  s3<-get.m(1000,"lv","left")
  f.sel<-s3$sel
  s3_sf<-s3
  s3<-s3$s3*10
  m1<-mean(s3)
  length(s3)
  sd1<-sd(s3)
  sdc<-c((m1-sd1):(m1+sd1))
  sdc<-sdc[order(sdc)]
  sdc
  s3[order(s3)]
  sdmin<-min(sdc)
  sdmax<-max(sdc)
  wmin<-which.min(s3)
  wmax<-which.max(s3)
  s3[wmin]
  s3[wmax]
  ##########
  
  ##########
  ### THIS >
  m4<-s3>=m1 # greater or equal to mean string distance 
  s3[m4]
  c2<-unlist(strsplit(names(s3[m4])," "))
  c2<-gsub("[,.;:]","",c2)
  m3<-grepl("%cast",c2)
  c2<-c2[!m3]
  c2<-unique(c2)
  l1<-lapply(c2, function(x){
    l<-length(unlist(strsplit(x,"")))
    ifelse(l>1,x,NA)
  })
  l1<-l1[!is.na(l1)]
  l1<-unlist(l1)
  l1
  c2<-l1
 # m5<-t1%in%c2
  #sum(m5) # yet 243 speaker!
  #t1[m5]
  rg1<-paste0("(",paste0(c2,collapse = "|"),")")
  rg1
  m6<-grepl(rg1,t1)
  sum(m6)
  t1[m6]
  c3<-t1[m6]
  l1<-lapply(c3, function(x){
    l<-length(unlist(strsplit(x," ")))
    ifelse(l<=2,x,NA)
  })
  l1<-l1[!is.na(l1)]
  l1<-unlist(l1)
  l1
  m7<-grepl("[)(]",l1)
  l1[m7]
  m8<-grepl("\\.$",l1[m7])
  l1[m7][m8]
  l2<-l1[!m7]
  l3<-c(l2,l1[m7][m8])
  l3
  return(f.sel)
}

check_regex_dep<-function(repldf){
  sampletx<-"just a random sample text"
  tryCatch({
  for (k in 1:length(repldf$find)){
    
      sampletx<-gsub(repldf$find[k],repldf$replace[k],sampletx)
  }
      return(repldf)
    }, error = function(e) {
      message<-paste0("your replacment table contains an error at row -",k,"-")
    
     return(message)
    })
  
}
check_regex <- function(repldf) {
  sampletx <- "just a random sample text"
  for (k in seq_along(repldf$find)) {
    # Check 'find' pattern
    find_ok <- tryCatch({
      grepl(repldf$find[k], sampletx, perl = TRUE)
      TRUE
    }, error = function(e) {
      return(FALSE)
    })
    if (!find_ok) {
      return(list(success = FALSE, error = paste0("Regex error in 'find' at row ", k, ": ", repldf$find[k])))
    }
    # Check 'replace' pattern by running gsub
    replace_ok <- tryCatch({
      gsub(repldf$find[k], repldf$replace[k], sampletx, perl = TRUE)
      TRUE
    }, error = function(e) {
      return(FALSE)
    })
    if (!replace_ok) {
      return(list(success = FALSE, error = paste0("Regex error in 'replace' at row ", k, ": ", repldf$replace[k])))
    }
  }
  list(success = TRUE, result = repldf)
}
#t1<-t11
#cast<-c2
cast<-"Medvirkende:"
cast<-"ROLLELISTE"


guess_speaker<-function(t1,cast){
  # t1 is character
  ttemp<-tempfile("sp.txt")
  writeLines(t1,ttemp)
  t1<-readLines(ttemp)
 # t1<-readLines("~/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/actuel/breithaupt/breithaupt_renegat_exc.txt")
  m<-grep("^.{3,30}\\.$",t1)
  
  m2<-grepl("[)(,]",t1[m])
  m<-m[!m2]
  m3<-grep("[A-Z]{3,30}",t1)
  spc<-stri_extract_all_regex(t1,"[A-Z]{3,30}")
  spc<-unlist(spc)
  print("capital speakers...")
  print(spc)
  spc.t<-table(spc)
  mspc<-mean(spc.t)
  sdspc<-sd(spc.t)
  sdselect<-mspc-sdspc
  sp.sel<-spc.t[spc.t>sdselect]
  print("speaker table mean - sd")
  print(sp.sel)

  sp.guess<-t1[m] # NOT apply unique(): the more occurences the higher the stringdist score
  sp.guess<-c(sp.guess,sp.sel)
  sp.guess<-gsub("[@.]","",sp.guess)
  l<-length(sp.guess)
  cat("speakers guesses length:",l,"\n")
  #t4<-paste0("^(", paste0(t4$cast,collapse = "|"), ").?$")
  #t4
 # cast<-"ROLLELISTE"

  #sp.cast<-get.castlist(t1,cast)$cast
  c5<-get.castlist(t1,cast)
  c5
  c6<-gsub("[*\\)\\(]","",c5$cast)
  c7<-unique(unlist(strsplit(c6," ")))
  c7<-unique(unlist(strsplit(c7,"/")))
  c7<-gsub(",","",c7)
  c7<-gsub("%cast%","",c7)
 # c7<-c(c7,paste0(c7,":?\\.?"))
  spx<-get.str.dist(t1,c7,sp.guess)
  l3<-spx
#  l3<-get.str.dist(sp.cast)
  l3<-unique(l3)
  print(sp.cast<-c6)
 # sp.cast<-gsub("(%cast%|")
  sp.cast<-gsub(paste0("%cast%|\\.|",cast),"",sp.cast)
  sp.cast<-sp.cast[2:length(sp.cast)]
  sp.cast
  sp.guess<-sp.guess[sp.guess%in%sp.cast]
  sp.guess
  #sp.return<-paste0(sp.guess,collapse = ",")
  print(l3)
  return(l3)
  
}

extract_head_nodes <- function(html_file) {
  doc <- read_html(html_file)
  nodes <- xml_find_all(doc, "//*[self::script or self::link]")
  lapply(nodes, function(node) HTML(as.character(node)))
}

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
save_defaults<-function(rvdf){
  load("default-values.RData") #### >>> CHK if OUT!
  print(head(defaults))
  id<-rvdf[["id"]]
  defaults[id,]<-rvdf
  save(defaults,file="default-values.RData")
  print("saved")
  
}
load_defaults <- function(id=F) {
  # Replace this with your actual database query
  # Example database connection and query:
  idx<-id
  tryCatch({
    # con <- dbConnect(RSQLite::SQLite(), "your_database.db")
    # result <- dbGetQuery(con, "SELECT speaker_names FROM defaults WHERE id = 1")
    # dbDisconnect(con)
    # return(result$speaker_names[1])
    load("default-values.RData") ### >>> CHK if OUT!!!!!!
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
clean.t<-function(t,r,repldf){
  txtemp<-tempfile("txraw.txt")
  writeLines(t,txtemp)
  library(readtext)
  t2<-readtext(txtemp)$text
  # metadf<-read.table("metadf.csv",sep = "\t")
  # metadf<-fromJSON("repldf.json",flatten = T)
  # repldf<-metadf$repl
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
  ### close open <stages>
  parts <- str_match(t2,"(\\([^)]+?[^)](?=[@%$#]))")
  print(parts)
  t2<-gsub("(\\([^)]+?[^)](?=[@%$#]))","\\1)",t2,perl = T)
  t2<-gsub("\n\\)",")\n",t2)
  parts <- str_match(t2,"(\\([^)]+?[^)](?=[@%$#]))")
  print(parts)
  ###
  writeLines(t2,txtemp)
  
  t3<-readLines(txtemp)
  t3[1:150]
  t3<-gsub("%spknl%|%cast%","",t3)
  return(t3)
}
transform.ezd<-function(ezd,output_file,meta){
  #ezdtemp<-tempfile("ezd.txt")
  #writeLines(ezd,ezdtemp)
  #xmlout<-tempfile("xmlout.xml")
  #xmlout<-"r-tempxmlout.xml"
  xmlout<-output_file
  #writeLines(ezd,"ezdmarkup.txt")
  parse_drama_text(ezd,xmlout,meta)
  return(readLines(xmlout))
}
### preprocess raw text
headx.1<-"Aufzug"
headx.2<-"Auftritt"
#t1<-t3
metadf<-read.csv("lx/metadf-mlx.csv")



get.heads.2<-function(t1,h1.all,h2.all,numer.all){
m1<-grep(h1.all,t1)
m2<-grep(h2.all,t1)
m3<-grep(numer.all,t1)
t1[m3]
#numer.all
ma<-c(m1,m2)
m1x<-m3%in%ma
t1[m3][m1x]
h3<-t1[m3][m1x]
htemp<-tempfile("heads.txt")
t2<-t1
t2[m3][m1x]<-paste0("%#% ",t2[m3][m1x])
t2[m3][m1x]<-gsub(", ","\n%##% ",t2[m3][m1x]) # seperate ACT from scene declaration
writeLines(t2,htemp)
t2<-readLines(htemp)
#t2[m3][m1x]
m4<-grep("%#+%",t2)
m5<-grep("%#%",t2)
m6<-duplicated(t2[m5])
print("remove duplicated acts...")
print(m6)
t2[m5][m6]<-""
vario<-t2[m4]
vario
vario<-gsub("%","",vario)
vario
t2[m4]<-gsub("%","",t2[m4])
t2
return(list(text=t2,vario=vario))
}
get.heads.s<-function(t1,headx.1="(Akt|Act|Handlung)",headx.2="(Szene|Scene)"){
  numer<-c("(Erst|Zweyt|Zweit|Dritt|Viert|Fünfte|Fuenft|Sechs|Sieben|Acht|Neun|Zehn|Elf|Zwoelf|Zwölf|Dreizehn|Dreyzehn)")
  #  ifelse(level==1,headx<-headx.1,headx<-headx.2)
  # ifelse(level==1,ph<-"#",ph<-"##")
  numer<-metadf$cardinal
  do.caps<-function(numer){
  numer.s<-unlist(strsplit(numer,"\\|"))
  numer.s<-gsub("[)(]","",numer.s)
  numer.s<-numer.s[numer.s!=""]
  numer.s<-numer.s[!is.na(numer.s)]
  numer.c<-capitalize(numer.s)
  numer.x<-gsub("[A-ZВ-Ш]",".",numer.c)
  numer.x
  numer.dc<-decapitalize(numer.s)
  
  numer.tu<-toupper(numer.s)
  numer.tu.x<-toupper(numer.x)
  n.all<-c(numer.s,numer.c,numer.dc,numer.tu,numer.x,numer.tu.x)
  n.all<-unique(n.all)
  numer.all<-paste0("\\b",n.all,"\\b",collapse = "|")
  numer.all<-paste0(n.all,collapse = "|")
  length(numer.all)
  return(numer.all)
  }
  numer.all<-do.caps(numer)
  #h1.all<-do.caps(h1)
  #h1<-paste0("(",paste0(metadf$h1,collapse = "|"),")")
  #h2<-paste0("(",paste0(metadf$h2,collapse = "|"),")")
  h1<-metadf$h1
  h2<-metadf$h2
  h1<-c(h1,headx.1)
  h2<-c(h2,headx.2)
  h1.all<-do.caps(h1)
  h2.all<-do.caps(h2)
  h1.all
  numer<-paste0(numer,collapse = "|")
  regx.1<-paste0("^.+?",numer,".+(",headx.1,")\\.")
  regx.1<-paste0("^+?",numer,".+(",headx.1,")\\.")
# global from metadf
  # imp: ^[^a-zA-Z][ \t]{1,}?F.RSTE.+(AKT)\.?$
  regx.1<-paste0("^[ \t]{0,}?(",numer.all,").+(",h1.all,")\\.?$")
  regx.1<-paste0("^([ \t]{0,})?(",numer.all,").+?(",h1.all,")\\.?(.+)?$")
  
  print(regx.1)
  regx.2<-paste0("^.+?",numer,".+(",headx.2,")\\.")
  regx.2<-paste0("^+?",numer,".+(",headx.2,")\\.")
  regx.2<-paste0("^[ \t]{1,}?(",numer.all,").+(",h2.all,")\\.?$")
  regx.2<-paste0("^([ \t]{1,})?(",numer.all,").+?(",h2.all,")\\.?(.+)?$")
  #print(regx.2)
  #m1<-grep("1\\. AKT",t1)
  m1<-grep(regx.1,t1)
  ifelse(length(m1)==0,return(get.heads.2(t1,h1.all,h2.all,numer.all)),print("heads found with M1"))
  print("should not print after heads with M1")
  t1
  t2<-t1
 # t2[m1]<-paste0("# ",t2[m1])
  ### 15372.NOTE: the shakespeare-ingentingen contains AKT definitions at each scene, so the <div> element is always created anew. this maybe okay for the library and of editorial perspective, but for the dracor scheme its not a way since it messes up the network and speech distribution.
  
  #########################################################
  t2[m1]<-paste0("# ",t2[m1],"%hnl%") #out
#  ^([ \t]{0,})?(",numer.all,").+?(",h1.all,")\\.?(.+)?$
  # t2[m1]<-gsub(regx.1,"# \\2 \\3%hnl%\\4",t2[m1],perl = T) #in
  m2<-grep(regx.2,t1)
  #t2<-t1
#  t2[m2]<-paste0("## ",t2[m2])
  m3<-m2%in%m1
  #if (m2!=m1)
    t2[m2][!m3]<-paste0("## ",t2[m2],"%hnl%") #out
  # t2[m2]<-gsub(regx.2,"## \\2 \\3%hnl%\\4",t2[m2],perl = T) #in
  #########################################################
  vario<-c(t2[m1],t2[m2])
  h1<-t2[m1]
  h2<-t2[m2]  
  m3<-duplicated(t2[m1])
  print("duplicated act") ### > is never run
  print(m3)
  ### 15373.double act issue, no scene divisions
  #t3<-get.heads.2(t2,h1.all,h2.all,numer.all)
  #t2<-t3$text
  # t2[m1][m3]<-"" # duplicated ACT definitions are deleted
  #return(t1)
  # vario<-c(t2[m1],t2[m2])
  # h1<-t2[m1]
  # h2<-t2[m2]
    #       print(h1[1:10])
     #      print(h2[1:10])
  
           return(list(vario=vario,text=t2))
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
 #lines<-t1
# l<-47
#cast<-"Medvirkende"
# cast
 get.castlist<-function(lines,cast){
  for (l in 1:length(lines)){
    line<-lines[l]
    cast<-cast[!is.na(cast)]
    
    if(str_detect(line,paste0("^[ \t]{0,}",cast,".?$"),)){
      parts<-str_match(line,"\\^?(.*)")
      parts
      write(parts,"debug.txt",append = T)
      desc<-parts[2]
      desc
      parts<-str_match(line,"(\\^?)(.*)")
      parts
      if(parts[2]=="")
        lines[l]<-paste0("^",parts[3])
      
      r<-l:length(lines)
      print("chk $# in castlist following lines")
      m<-str_detect(lines[r],"^[$#]",) # only if h1 already applied!
      mw<-which(m)
      mw<-mw[1]
      print(mw)
      if(is.na(mw))
        break("you have to apply scene segmentation before speaker recognition...")
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
    print("getcast m")
    mw<-which(m)
    mw<-mw[1]-1
    mw<-l:r[mw]
    print(lines[mw])
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
  return(list(lines=lines,cast=lines[mw]))
 }
 #lines[1:150]
# t6<-get.castlist(text)
# t6
 #t1<-t5
 #sp<-vario
 #sp
get.speakers<-function(t1,sp,rswitch=F,copyrighted){
  regx1<-sp
  regx2<-sp
  sp01<-unlist(strsplit(sp,","))
  sp01<-sp01[!is.na(sp01)]
  sp01<-sp01[sp01!=""]
  sp01<-gsub("([).(])","\\\\\\1",sp01)
  sp01<-c(sp01,toupper(sp01))
  sp1<-paste0("(",paste0(sp01,collapse = "|"),")")
  sp2<-paste0(sp01,".")
  print(sp1)
  #regx<-paste0("^.+?",sp1,"\\.")
  if(!rswitch){
  regx1<-paste0("^",sp1,"\\.?$")
  regx2<-paste0("^(",sp1,"\\.?)( .+)?$")
  print(regx2)
  }
  m<-grep(regx1,t1)
  
  print(length(m))
  cat("\rm1",m)
#  parts<-str_match(t1[m],regx)
  #spk<-parts[1]
  #print(spk)
  t2<-t1
  m2<-grep(regx2,t1)
  cat("\rm2:",m2)
  ### 15371.critical### TO FIX !!!!########
  if(!rswitch)
    t2[m2]<-gsub(regx2,"@\\2%spknl%\\\n\\3",t2[m2]) # this wks in editor
  
  t2[m2]<-paste0("@",t2[m2],"%spknl%") # this wks in editor
  print(m2)
  print(t2[m2])
  #########################################
  t2
  sp2
  regx1<-paste0("^",sp1,"(\\.|:)?$")
  
  m<-grep(regx1,t2)
  
  crit<-t2[m]%in%sp2 # if {speaker}. appears in text
  print(crit)
  crit.sp<-t2[m][!crit]
  crit.sp
  t2[m]
  crit.sp<-crit.sp[!is.na(crit.sp)]
  #t2[m][crit]<-paste0("@",t2[m][crit],"%spknl%")
  l2<-lapply(t2[m], function(x){
    l<-length(strsplit(x," "))
    ifelse(l>1,NA,x)
  })
  l2<-l2[!is.na(l2)]
  l2<-unlist(l2)
  #critical out
  #t2[m]<-paste0("@",l2,"%spknl%")
  ###############################
  t2[m]
  #t2<-gsub("%cast%","",t2)
  print(crit.sp)
  wc<-which(!crit)
  print(wc)
  ### find speakerstages (introduction at scene head)
  #m2<-grepl(sp1,t1) #all speaker occurences in text
  regx<-paste0("^",sp1,", \\(.+\\.?\\)$")
  m2<-grep(regx,t2)
  tx<-"Orchan, (zu Therise.)"
  #sp1<-"Orchan|Zapor|Welwood"
  mreg<-paste0("(",sp1,"), (\\(.+\\.?\\))") # extract stage in speakerline
  #parts <- str_match(tx, mreg)
  
  #t2[m2]<-paste0("@",parts[2],"%spknl%")
  
  
 # m3<-grepl()
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
  m2<-t2==""
  t2<-t2[!m2]
  m2<-is.na(t2)
  t2<-t2[!m2]
  t2<-gsub("^@@","@",t2)
  t2<-gsub("^[ \t]{1,}","",t2)
 # t2<-gsub("(\\([^)]+?[^)](?=[@%$#]))","\\1)")
  #return(t1)
  print(copyrighted)
  mode(copyrighted)<-"logical"
  print(mode(copyrighted))
  print("get.speakers() finished...")
  if(!copyrighted)
    writeLines(t2,paste0(Sys.getenv("GIT_TOP"),"/test/temp/ezdmarkup.txt"))
  return(list(vario=t1[m][crit],text=t2,eval=crit.sp))
  
}
