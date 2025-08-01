#20250320(17.29)
#15126.klopstock-abel.TEI
#########################
#t1<-readLines("~/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/actuel/klopstock/klopstock_tod-abels.txt")
#t1<-readLines("~/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/actuel/goue/txt/goue_iwanette_ggl.txt")
# to fetch transcript text from transkribus API:
library(readtext)
library(httr)
#source("~/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/transkribus-api.R")
ezd_markup.ns<-paste0(Sys.getenv("GIT_TOP"),"/ETCRA5_dd23/bgltr/ocr/actuel/goue/goue_iwanette_ezd")
ezd_markup.ns_temp<-tempfile("temp_ezd")
ezd_markup.ns<-paste0(ezd_markup.ns_temp)
ezd_markup_text<-paste0(ezd_markup.ns,".txt")
ezd_markup.temp<-tempfile("ezdtemp.txt")
ezd_markup.ns
runtoezd<-function(){
#t1<-readLines("~/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/actuel/goue/goue_iwanette_apiexpo.txt")
#r<-GET("https://ids.dh-index.org/api/trans?collection=1973292&doc_id=7599198")
# r<-GET("https://ids.dh-index.org/api/trans?transcript=iwanette")
# t2<-content(r,"text")
# ttemp<-tempfile("trans.txt")
# writeLines(t2,ttemp)
# t1<-readLines(ttemp)
# t1
#t1<-tx.text # generated above
# i made few changes corrections/adaptations in the t1 static file to make this script run fluently, so modifications in the transkribus editor make no sense at the moment.
# ezd_markup.ns<-paste0("~/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/actuel/klopstock/klopstock_tod-abels_ezd")
# ezd_markup.ns<-paste0("~/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/actuel/goue/goue_iwanette_ezd")
# ezd_markup_text<-paste0(ezd_markup.ns,".txt")
# ezd_markup.temp<-tempfile("ezdtemp.txt")
#library(readtext)
# m1<-grep("¬",t1)
# m1p<-m1+1
# tm1<-paste0(t1[m1],t1[m1p])
# t2<-t1
# t2[m1]<-tm1
# m2<-1:length(t2)%in%m1p
# t2<-t2[!m2]
# t2[1]
t2<-t1
#### 15175.iwanette
# preprocess
# umlaute:
lx<-548
t2[lx]
#library(stringi)
ltint<-utf8ToInt("Gl--cks")
utf8ToInt("u")
intToUtf8(868)
utf8ToInt("zZ")
utf8ToInt("ß")
utf8ToInt("aA")
utf8ToInt("bB")
utf8ToInt("äaöoüuÄAÖOÜU")
umrepl<-c(A="Ä",O="Ö",U="Ü",a="ä",o="ö",u="ü")
umrepl
#library(purrr)
ltx<-unlist(strsplit(t2[lx]," "))
ltx
ltint<-utf8ToInt(ltx[7])
ltint
lim<-252+1
mu<-ltint>lim
mu
mup<-which(mu)-1
x<-umrepl[6]
outlist<-list()
x
umrepl
lti2<-utf8ToInt(t2[548])
lti2
k<-548
t3<-t2
k<-126
x
r<-6
r
k
mp<-4
mp<-32
for(r in 1:length(umrepl)){
  x<-umrepl[r]
  for (k in 1:length(t3)){
    ltint<-utf8ToInt(t3[k])
    mu<-ltint>lim
    mu
    mup<-which(mu)-1
    u<-intToUtf8(ltint[mup])
    u<-unlist(strsplit(u,""))
    u
am<-u==names(x)|u==x
u<-u[am]
am<-u==names(x)|u==x
if(sum(am)>0) {
  mp
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

t3[548:570]
utf8ToInt(t3[551])
#writeLines(t2,ezd_markup_text)
writeLines(t3,ezd_markup_text)
##############################
# ezd markup refine >
#####################
### front
m<-grep("@",t3)
author<-"August Siegfried von Goué"
author.p<-paste0("@author ",author)
published<-"1771"
t3[m]
m2<-grep("title|subtitle",t3[m])
front<-c(t3[m][m2],author.p)
front
m<-grepl("@",t3)
t3<-c(front,t3[!m])
############################
### personal
m<-grep("\\^",t3)
t3[m:(m+30)]
person.array<-c("Iwanette","Stormond","Golowin","Wolsey","Bender","Der Medicus","Der ältere Stormond","Der jüngere Stormond")
speaker.m<-paste0(person.array,"\\.$")
speaker.m<-paste0("^",speaker.m,collapse = "|")
speaker.m
m<-grep(speaker.m,t3)
t3[m]
#####################
### I/J replacement
# regx.I<-"J(h|w|c|s)"
# t4<-gsub(regx.I,"I\\1",t3)
# m<-grep(speaker.m,t4)
# t4[m]
##############################
### general replacement
repl.df<-data.frame(regx=NA,repl=NA)
repl.df[1,]<-c("W.l.ey","Wolsey")
repl.df[2,]<-c("J(h|w|c|s)","I\\1")
repl.df[3,]<-c("G.l.w.n","Golowin")
repl.df[4,]<-c(".wane..e","Iwanette")
repl.df[5,]<-c("#[(]#","[")
repl.df[6,]<-c("#[)]#","]")
repl.df[7,]<-c(" "," ")
repl.df[8,]<-c("  "," ")
repl.df[9,]<-c("#unclear","[unclear]")

library(jsonlite)
repl.json<-"repldf.json"
jsonpath<-paste0(Sys.getenv("GIT_TOP"),"/work/essais/dracorTEI/",repl.json)
write_json(repl.df,jsonpath)
repl.df.js<-fromJSON(jsonpath)
t4<-t3
# repl.df<-repl.df.js
for (k in 1:length(repl.df$regx)){
  t4<-gsub(repl.df$regx[k],repl.df$repl[k],t4)
}
writeLines(t4,ezd_markup_text)
### remove blancs
m<-t4==""|t4==" "|t4=="."
sum(m)
t4<-t4[!m]
m10<-grepl("#o",t4)
t4<-t4[!m10]
writeLines(t4,ezd_markup_text)
### get body start
m<-grep("^#",t4)
t4[m][1]
m2<-grep(speaker.m,t4[m[1]:length(t4)])+m[1]-1
t4[m2]
t4[m2]<-paste0("@",t4[m2],"spknl")


writeLines(t4,ezd_markup_text)
return(list(ezd=t4,repldf=repl.df,speaker=person.array))
}
##########
# run before remove linebreaks, wt fetch from api
# >>>>>>>>
ezd.p<-runtoezd()
repl.df<-ezd.p$repldf
t4<-ezd.p$ezd
##########
##############################
### remove linebreaks
mp<-grep("\\^",t4)
ms<-grep("^(#|\\$)",t4)
t4[ms]
ms<-ms[1]
personal<-t4[mp:(ms-1)]
personal
body<-t4[ms:length(t4)]
head(body)
pre1<-grep("@",t4)
prepage<-t4[1:(mp-1)]
prepage
t4[1:200]
ezdtemp<-tempfile("ezdtx.txt")
writeLines(body,ezdtemp)
ezdtx<-readtext(ezdtemp)$text
lrg<-length(repl.df$regx)
#########
### 15236.
create.meta<-function(){
meta.table<-data.frame(key=NA,id=NA,string1=NA,string2=NA,comment=NA,lfd=1:30)
mt<-meta.table
mt[1,1]<-"author"
mt[1,2]<-1
mt[1,3]<-author
mt[2,1]<-"title"
mt[3,1]<-"subtitle"
mt[4:(length(person.array)+3),1]<-"personal"
mt[4:(length(person.array)+3),2]<-1:length(person.array)
mt[4:(length(person.array)+3),3]<-person.array
mt[which(is.na(mt$key))[1],"id"]<-1
mt[which(is.na(mt$key))[1],"key"]<-"header_decl"
mt[which(is.na(mt$key))[1],"id"]<-2
mt[which(is.na(mt$key))[1],"key"]<-"header_decl"
################################################
mt[which(is.na(mt$key))[1]:(length(repl.df$regx)+which(is.na(mt$key))[1]-1),3:(length(repl.df)+2)]<-repl.df
mt[which(is.na(mt$key))[1]:(length(repl.df$regx)+which(is.na(mt$key))[1]-1),"id"]<-1
mt[which(is.na(mt$key))[1]:(length(repl.df$regx)+which(is.na(mt$key))[1]-1),"key"]<-"repl"

mt[which(is.na(mt$key))[1],"id"]<-2
mt[which(is.na(mt$key))[1],3:4]<-c("([a-zA-ZÄÜÖäüö])(¬|=|-)\n","\\1")
mt[which(is.na(mt$key))[1],"key"]<-"repl"
mt[which(is.na(mt$key))[1],"id"]<-2
mt[which(is.na(mt$key))[1],3:4]<-c("((?<!spknl)\n)(?![@#$])"," ")
mt[which(is.na(mt$key))[1],"key"]<-"repl"
mt[which(is.na(mt$key))[1],"id"]<-2
mt[which(is.na(mt$key))[1],3:4]<-c("\\) \\(","[.-.]") # double <stage> creates empty <p>
mt[which(is.na(mt$key))[1],"key"]<-"repl"
# 2nd repl
# repl.df[lrg+1,]<-c("([a-zA-ZÄÜÖäüö])(¬|=|-)\n","\\1")
# repl.df[lrg+2,]<-c("((?<!spknl)\n)(?![@#$])"," ")
# repl.df[lrg+3,]<-c("\\) \\(","[.-.]") # double <stage> creates empty <p>

# 3rd repl
mt[which(is.na(mt$key))[1],"id"]<-3
mt[which(is.na(mt$key))[1],3:4]<-c("\\.spknl","") # double <stage> creates empty <p>
mt[which(is.na(mt$key))[1],"key"]<-"repl"
#ezd_markup.temp<-gsub("\\.spknl","",ezd_markup.temp)

#
mt[which(is.na(mt$key))[1],"id"]<-4
mt[which(is.na(mt$key))[1],3:4]<-c("¬\n","&lb") # double <stage> creates empty <p>
mt[which(is.na(mt$key))[1],"key"]<-"repl"
mt[which(is.na(mt$key))[1],"id"]<-4
mt[which(is.na(mt$key))[1],3:4]<-c("¬","&lb") # double <stage> creates empty <p>
mt[which(is.na(mt$key))[1],"key"]<-"repl"
# t4<-gsub("¬\n","&lb",t4)
# t4<-gsub("¬","&lb",t4)
# writeLines(t4,ezd_markup.temp)
# repl5
mt[which(is.na(mt$key))[1],"id"]<-5
mt[which(is.na(mt$key))[1],3:4]<-c("(&lb)\n","") # double <stage> creates empty <p>
mt[which(is.na(mt$key))[1],"key"]<-"repl"
# t5<-readtext(ezd_markup.temp)$text
# t5<-gsub("(&lb)\n","",t5)
return(mt)
}
mt<-create.meta()
#mt.js<-split(mt,mt$key)
mt.js <- split(
  lapply(seq_len(nrow(mt)), function(i) {
    list(id=mt$id[i],string1 = mt$string1[i], string2 = mt$string2[i],comment=mt$comment[i],lfd=mt$lfd[i])
  }),
  mt$key
)
write_json(mt.js,jsonpath)
library(readr)
mtns<-"metadf.csv"
mtpath<-paste0(Sys.getenv("GIT_TOP"),"/ETCRA5_dd23/bgltr/ocr/actuel/goue/",mtns)

write.table(mt,file=mtpath,row.names = F,quote = F,sep = "\t")
# wks., nested json

#####################################################
repl.df[lrg+1,]<-c("([a-zA-ZÄÜÖäüö])(¬|=|-)\n","\\1")
ezdtx<-gsub(repl.df$regx[lrg+1],repl.df$repl[lrg+1],ezdtx)
repl.df[lrg+2,]<-c("((?<!spknl)\n)(?![@#$])"," ")
repl.df[lrg+3,]<-c("\\) \\(","[.-.]") # double <stage> creates empty <p>

ezdtx<-gsub(repl.df$regx[lrg+2],repl.df$repl[lrg+2],ezdtx,perl = T)
ezdtx<-gsub(repl.df$regx[lrg+3],repl.df$repl[lrg+3],ezdtx,perl = T)

#ezdtx
writeLines(ezdtx,ezd_markup_text)
#ezdtx<-gsub(repl.df$regx[lrg+2],repl.df$repl[lrg+2],ezdtx,perl = T)
#ezdtx
writeLines(ezdtx,ezdtemp)
ezd_markup.body<-readLines(ezdtemp) # read body
m<-grep("@",prepage)
prepage[m]<-paste0(prepage[m],".spknl")
writeLines(prepage,ezdtemp) # write prepages
prepage<-readtext(ezdtemp)$text # read utf prepages
prepage<-gsub(repl.df$regx[lrg+1],repl.df$repl[lrg+1],prepage)
prepage<-gsub(repl.df$regx[lrg+2],repl.df$repl[lrg+2],prepage,perl = T)
writeLines(prepage,ezdtemp)
prepage<-readLines(ezdtemp)

# ezd_markup.temp<-c(t4[1:(ms-1)],ezd_markup.temp)
ezd_markup.temp<-c(prepage,personal,ezd_markup.body)
ezd_markup.temp<-gsub("\\.spknl","",ezd_markup.temp)
#t4<-readLines(ezdtemp)
writeLines(ezd_markup.temp,ezd_markup_text)
m<-grep("@",ezd_markup.temp)
ezd_markup.temp[m]

#outlist
fun.temp1<-function(){
ltint
mout<-ltint==1000
lt2<-ltint[!mout]
intToUtf8(lt2)
m3<-grep("#f",t2)
title<-unlist(strsplit(t2[m3],"\\."))
title<-gsub("(^ )|( $)","",title)
title[1]<-"Margarete Klopstock"
title<-paste0(c("@author ","@title ","@subtitle "),title)
title
t3<-c(title[c(2,3,1)],t2[(m3+1):length(t2)])
head(t3,20)
m4<-t3==""
t3<-t3[!m4]
head(t3,20)
m6<-grep("\\^",t3)
m7<-grep("#",t3)
t3[(m6+1):(m7[1]-1)]
pers<-t3[(m6+1):(m7[1]-1)]
pers<-gsub("(\\.)|(^ )|( $)","",pers)
pers
t3[(m6+1):(m7[1]-1)]<-pers
t4<-t3
head(t4,50)
pers.m<-paste0(pers,".")
pers.m<-c(pers.m,"Beyde.")
m11<-t4%in%pers.m
t4[m11]<-paste0("@",t4[m11])
# for (n in pers){
#   gn<-paste0("(^",n,"\\.","$)")
#   m8<-grep(gn,t4)
#   t4[m8]<-paste0("@",t4[m8])
# }
head(t4,50)
m9<-grep("#",t4)
t4[m9]
t4<-gsub("¬\n","&lb",t4)
t4<-gsub("¬","&lb",t4)
writeLines(t4,ezd_markup.temp)

t5<-readtext(ezd_markup.temp)$text
t5<-gsub("(&lb)\n","",t5)
#t5<-gsub("(3p)\n"," ",t5)

writeLines(t5,ezd_markup.temp)
t4<-readLines(ezd_markup.temp)
t4<-gsub("#[(]#?","[",t4)
t4<-gsub("#[)]#","]",t4)
m10<-grep("#o",t4)
t4[m10]
m10<-grepl("#o",t4)
t4<-t4[!m10]
#t4<-gsub("^[(](.+)[)]","$\\1",t4)
char.s<-"[#@$^]"
char.gr<-paste0("^",char.s)
tbody<-(m9[1]+1):length(t4)
m12<-grep(char.gr,t4[tbody])
m12<-grepl(char.gr,t4[tbody])
t4[tbody][!m12]<-paste0(t4[tbody][!m12],"3p")
writeLines(t4,ezd_markup.temp)

t5<-readtext(ezd_markup.temp)$text
t5
t5<-gsub(paste0("(3p)\n(?!",char.s,")")," ",t5,perl = T)
t5<-gsub("(@.+\\.)\n(\\(.+\\))","\\1\\2",t5)
t5<-gsub("(3p)"," ",t5)

writeLines(t5,ezd_markup_text)
}
##########
library(reticulate)
process.ezd<-function(){
  path.local.home<-"~/Documents/GitHub/dybbuk-cor"
  system(paste0("python3 ",path.local.home,"/convert/actuel/","parser.git.py ",ezd_markup_text))
  print("finished python ezd")
} #end ezd process .txt
########################
# perform ezd processing
process.ezd()
########################
xml.finalize<-function(){
xml.ns<-paste0(ezd_markup.ns,".xml")
# read in ezd output .xml
xml.tx<-readLines(xml.ns)
m14<-grep("[0-9]{1,3}:: ",xml.tx)
#xml.tx[m14]<-gsub("([0-9]{1,3}::) ","\\1",xml.tx[m14])
m14<-grep("[0-9]{1,3}::",xml.tx)
xml.tx[m14]<-gsub("([0-9]{1,3})::",'<pb n="\\1"/>',xml.tx[m14])
m15<-grep("(\\[)",xml.tx)
xml.tx[m15]
xml.tx[m15]<-gsub("\\[","(",xml.tx[m15])

m15<-grep("(\\])",xml.tx)
xml.tx[m15]
xml.tx[m15]<-gsub("\\]",")",xml.tx[m15])
xml.tx[m15]
xmltemp<-tempfile("temp.xml")
writeLines(xml.tx,xmltemp)
xmlht<-read_xml(xmltemp)
xml_ns_strip(xmlht)
all.per<-xml_find_all(xmlht,"//persName")

xml_text(all.per)
m<-grep("jünger",xml_text(all.per))
#m2<-grep("Stormond",ezd.p$speaker)
xml_text(all.per[m])<-"Stormond"
xmlns<-xml_find_all(xmlht,"//TEI")
xml_attr(xmlns,"xmlns")<-"http://www.tei-c.org/ns/1.0"
xml.final<-paste0(Sys.getenv("GIT_TOP"),"/ETCRA5_dd23/tei/goue_iwanette.final.xml")
#writeLines(xml.tx,xml.final)
write_xml(xmlht,xmltemp)
xml.tx<-readLines(xmltemp)
xml.tx<-xml.tx[2:length(xml.tx)]
xml.tx<-gsub("^[ ].+?(<)","\\1",xml.tx)
xml.tx<-gsub('(<pb n="2"/>.+<pb n="8"/>)',"<preface>\\1</preface>",xml.tx)
xml.tx<-gsub('\\(\\.-\\.\\)',") (",xml.tx)
writeLines(xml.tx,xml.ns)
formatcom<-paste0('xmlformat ',xml.ns,' > ',xml.final)
# library(clipr)
# write_clip(formatcom)
system(formatcom)
}
xml.finalize()

