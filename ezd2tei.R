# 20250723(16.49)
# 15305.ezd2tei essai, deeps
############################
# Charger les packages nécessaires
library(xml2)
library(stringr)
library(purrr)

# Fonction d'échappement XML
escape_xml_dep <- function(text) {
  text %>% 
    str_replace_all("&", "&amp;") %>%
    str_replace_all("<", "&lt;") %>% 
    str_replace_all(">", "&gt;") %>% 
    str_replace_all("\"", "&quot;") %>% 
    str_replace_all("'", "&apos;")
}
#schema: <?xml-model href="https://dracor.org/schema.rng" type="application/xml" schematypens="http://relaxng.org/ns/structure/1.0"?>
validate_tei <- function(xmlfile, schema = "schema.rng") {
  res <- system2("jing", args = c(schema, xmlfile), stdout = TRUE, stderr = TRUE)
  list(ok = attr(res, "status") %||% 0 == 0,
       log = res)
}
# validate_tei("testheaders.xml","dracor-scheme.rng")

# Initialiser un nouveau document XML
create_tei_document <- function() {
  doc <- xml_new_document()
  # root <- xml_root(doc)
  # 
  # xml_add_sibling(root, 
  #                 "?xml-model", 
  #                 href="https://dracor.org/schema.rng", 
  #                 type="application/xml", 
  #                 schematypens="http://relaxng.org/ns/structure/1.0",
  #                 .where = "before")
  # 
  # # Ajouter xml-stylesheet avant l'élément racine  
  # xml_add_sibling(root,
  #                 "?xml-stylesheet",
  #                 type="text/css", 
  #                 href="../css/tei.css",
  #                 .where = "before")
  # NO: wrong element syntax written
  tei <- xml_add_child(doc, "TEI", 
                       `xml:id` = "insertID",
                       `xml:lang` = "ger",
                       xmlns = "http://www.tei-c.org/ns/1.0")
  
  # Ajouter l'en-tête TEI
  teiHeader <- xml_add_child(tei, "teiHeader")
  fileDesc <- xml_add_child(teiHeader, "fileDesc")
  
  # Titre et auteur
  titleStmt <- xml_add_child(fileDesc, "titleStmt")
  ### 15305.TODO > globalise!
  title<-xml_add_child(titleStmt, "title", "Der Tod Abels", type = "main")
  subtitle<-xml_add_child(titleStmt, "title", "Ein Trauerspiel", type = "sub")
  author<-xml_add_child(titleStmt, "author", "Margarete Klopstock")
  #########################################################
  # Section de publication
  publicationStmt <- xml_add_child(fileDesc, "publicationStmt")
  xml_add_child(publicationStmt, "publisher", "DraCor", `xml:id` = "dracor")
  xml_add_child(publicationStmt, "idno", "https://dracor.org", type = "URL")
  
  # Disponibilité
  availability <- xml_add_child(publicationStmt, "availability")
  licence <- xml_add_child(availability, "licence")
  xml_add_child(licence, "ab", "CC0 1.0")
  xml_add_child(licence, "ref", "Licence", 
                target = "https://creativecommons.org/publicdomain/zero/1.0/")
  
  # Source
  sourceDesc <- xml_add_child(fileDesc, "sourceDesc")
  bibl <- xml_add_child(sourceDesc, "bibl", type = "digitalSource")
  xml_add_child(bibl, "name", "ENTER SOURCE NAME HERE")
  xml_add_child(bibl, "idno", "ENTER SOURCE URL HERE", type = "URL")
  
  # Liste des personnages
  profileDesc <- xml_add_child(teiHeader, "profileDesc")
  particDesc <- xml_add_child(profileDesc, "particDesc")
  listPerson <- xml_add_child(particDesc, "listPerson")
  #person <-xml_add_child(listPerson,"person")
  # Corps du texte
  text <- xml_add_child(tei, "text")
  front <- xml_add_child(text, "front")
  castList <- xml_add_child(front, "castList")
  # xml_add_child(castList, "head", "Personen.")
  
  body <- xml_add_child(text, "body")
  
  list(
    doc = doc,
    front =front,
    title=title,
    subtitle=subtitle,
    author=author,
    body = body,
    castList = castList,
    listPerson = listPerson
  )
}

#input_file<-"klopstock_tod-abels_ezd.txt"
#input_file<-"ezdmarkup.txt"
#input_tx<-readLines("ezdmarkup.txt")

parse_drama_text <- function(input_tx, output_file) {
  # Lire le fichier d'entrée
  lines<-input_tx
  #  lines <- readLines(input_file, encoding = "UTF-8")
  lines<-lines[!is.na(lines)]
  lines<-lines[lines!=""&lines!=" "&lines!="  "]
  #lines<-gsub("\\^","!personal!",lines)
  # write("start parse---","debug.txt",append = T)
  #write(paste0(length(lines)," lines to process..."),"debug.txt",append = T)
  
  # Initialiser le document XML
  #xml_doc <- create_tei_document()
  # Initialiser le document XML
  xml_doc <- create_tei_document()
  
  # Variables d'état pour suivre la structure
  current_act <- NULL
  current_scene <- NULL
  #line<-lines[length(lines)-2]
  speaker.a<-array()
  scenes.t<-F
  # Traiter chaque ligne
  #line<-l1
  k<-39 # only for test
  line<-lines[k]
  line
  for (k in 1:length(lines)) {
    # 1. Gestion des personnages (@)
    # ?str_detect
    line.true<-""
    line<-lines[k]
    line
    write(k,"debug.txt",append = T)
    if(str_detect(line,"\\^",)){
      parts<-str_match(line,"\\^(.*)")
      write(parts,"debug.txt",append = T)
      
      desc<-parts[2]
      r<-k:length(lines)
      m<-str_detect(lines[r],"[$#@]",)
      mw<-which(m)
      mw<-mw[1]
      mw<-(k+1):r[mw-1]
      #castlist.r<-mw
      write("---catslist.r","debug.txt",append = T)
      
      write(mw,"debug.txt",append = T)
      
      castlist.t<-lines[mw]
      castlist.t
      #      castList<-xml_find_all(xml_doc$,"//castList")
      xml_add_child(xml_doc$castList, "head", desc)
      
      for(item in castlist.t){
        xml_add_child(xml_doc$castList,"castItem",item)
      }
      line.true<-"personal"
      write(castlist.t,"debug.txt",append = T)
      xml_text(xml_doc$castList)
      write_xml(xml_doc$doc,"cstest.xml")
    }
    
    if(str_detect(line,"^@title",)){
      parts<-str_match(line,"^@title (.*)")
      desc<-parts[2]
      xml_text(xml_doc$title)<-desc
      line.true<-"title"
      write(xml_text(xml_doc$title),"debug.txt",append = T)
      parts
    }
    if(str_detect(line,"^@subtitle",)){
      parts<-str_match(line,"^@subtitle (.*)")
      desc<-parts[2]
      xml_text(xml_doc$subtitle)<-desc
      line.true<-"subtitle"
      write(xml_text(xml_doc$subtitle),"debug.txt",append = T)
    }
    xml_text(xml_doc$subtitle)
    #parts
    
    if(str_detect(line,"^@author",)){
      parts<-str_match(line,"^@author (.*)")
      desc<-parts[2]
      xml_text(xml_doc$author)<-desc
      line.true<-"author"
      write(xml_text(xml_doc$author),"debug.txt",append = T)
      parts 
    }
    if(str_detect(line,"@front",)){
      parts<-str_match(line,"(@front(.+?) )(.+)")
      print(parts)
      lp<-length(parts)-1
      for(part in parts[2:lp]){
        print(part)
        line<-gsub(part,"",line)
        print(line)
      }
      line
      
      p4<-parts[4]
      n<-unlist(strsplit(parts[2],"-"))
      p1<-
        n<-n[n!=""]
      r<-k:length(lines)
      m<-str_detect(lines[r],"[\\^$#]",)
      mw<-which(m)
      lines[k:(r[mw[1]-1])]
      mw<-k:(r[mw[1]-1])
      #      mw<-(k+1):r[mw]
      lines[mw]
      front.r<-mw
      front.t<-lines[front.r]
      front.t[1]<-p4
      #      front.t<-paste0(front.t,collapse = "\n")
      #      castList<-xml_find_all(xml_doc$,"//castList")
      for(p in front.t){
        xml_add_child(xml_doc$front, "p", p)
      }
      line.true<-"front"
    }
    
    if (str_detect(line, "^@[^.]+\\.", )&!line.true%in%c("title","subtitle","author","front")) {
      parts <- str_match(line, "^@([^.]+?)\\.(.*)")
      speaker <- gsub("[@.]","",str_trim(parts[2]))
      speaker.id<-paste0(tolower(speaker))
      speaker.id<-gsub(" ","_",speaker.id)
      speaker.a<-append(speaker.a,speaker,after = k)
      speaker.a<-speaker.a[!is.na(speaker.a)]
      speaker.a<-gsub(" ","_",speaker.a)
      text <- str_trim(parts[3])
      line.true<-"speaker"
      # Traitement des numéros de page (150::)
      # text <- str_replace_all(text, "(\\d{1,4})::", "</p><pb n=\"\\1\"/><p>")
      # text <- str_replace_all(text, "(\\d{1,4})::", "<pb n=\"\\1\"/>")
      # # Traitement des didascalies inline ((texte))
      # text <- str_replace_all(text, "\\(([^)]+)\\)", "<stage>\\1</stage>")
      text<-"" # empty text array
      
      # Ajouter au XML
      if (!is.null(current_scene)) {
        sp <- xml_add_child(current_scene, "sp", who = paste0("#", speaker.id))
        xml_add_child(sp, "speaker", speaker)
        write(speaker.id,"debug.txt",append = T)
        
        p <- xml_add_child(sp, "p")
        #xml_text(p) <- text
        write("<p>","debug.txt",append = T)
        
      }
    } 
   # parts
    speaker.a
    # 2. Didascalies de bloc ($)
    if (str_detect(line, "^\\$")) {
      stage_content <- gsub("[$]","",str_trim(str_sub(line, 2)))
      if (!is.null(current_scene)) {
        xml_add_child(current_scene, "stage", stage_content)
        write("<stage>","debug.txt",append = T)
        
      }
      text<-""
      line.true<-"stage"
    }
    # 3. Actes (#)
    if (str_detect(line, "^[#]{1}[^#]")) {
      act_title <- gsub("#","",str_trim(str_sub(line, 2)))
      current_act <- xml_add_child(xml_doc$body, "div", type = "act")
      xml_add_child(current_act, "head", act_title)
      current_scene <- NULL
      ifelse(sum(str_detect(lines, "^[#]{2}"))>0,scenes.t<-T,current_scene<-current_act)
      text<-""
      line.true<-"act"
      write(act_title,"debug.txt",append = T)
      
    }
    # 4. Scènes (##)
    if (str_detect(line, "^[#]{2}")) {
      scene_title <- gsub("##","",str_trim(str_sub(line, 3)))
      if (!is.null(current_act)) {
        current_scene <- xml_add_child(current_act, "div", type = "scene")
        xml_add_child(current_scene, "head", scene_title)
        write(scene_title,"debug.txt",append = T)
        
      }
      text<-""
      line.true<-"scene"
    }
    # 5. Texte continu
    if (str_trim(line) != ""&!line.true%in%c("stage","speaker","act","scene","author","title","subtitle","personal","front")) {
      #write(processed,"debug.txt",append = T)
      
      if (!is.null(current_scene)) {
        # Appliquer les mêmes transformations que pour le texte des personnages
        processed <- line %>%
          str_replace_all("(\\d{1,4})::", "{cleftpb n=\"\\1\"{cright[\\1]{cleft/pb{cright") %>%
          str_replace_all("\\(([^)]+)\\)", "{cleftstage{cright\\1{cleft/stage{cright")
        # <pb n="11">[11]</pb>
        #        p <- xml_add_child(current_scene, "p")
       # p <- xml_add_child(sp, "p")
        write(processed,"debug.txt",append = T)
        
        xml_text(p) <- processed
        line.true<-"p"
        
      }
    }
  }
  speaker.a<-unique(speaker.a)
  speaker.a
  speaker.ids<-paste0(tolower(speaker.a))
  for(sp in 1:length(speaker.a)){
    person<-xml_add_child(xml_doc$listPerson,"person",sex="TODO",`xml:id`=speaker.ids[sp])
    xml_add_child(person,"persName",speaker.a[sp])
  }
  write(speaker.a,"debug.txt",append = T)
  
 # xml_text(xml_doc$body)
  xmltemp<-tempfile("temp.xml")
  # Écrire le fichier XML de sortie
  write_xml(xml_doc$doc, xmltemp)
  # xmltx<-readLines(output_file)
  # xmltx<-gsub("\\{cleft","<",xmltx)
  # xmltx<-gsub("\\{cright",">",xmltx)
  # writeLines(xmltx,xmltemp)
  # 
  library(XML)
  
  doc  <- xmlParse(xmltemp, useInternalNodes = TRUE, encoding = "UTF-8")
  root <- xmlRoot(doc)
  
  # add PIs before the root element (order matters: stylesheet first, then model)
  addSibling(root, newXMLPINode("xml-stylesheet", 'type="text/css" href="tei.css"'), after = FALSE)
  addSibling(root, newXMLPINode("xml-model",
                                'href="https://dracor.org/schema.rng" type="application/xml" schematypens="http://relaxng.org/ns/structure/1.0"'),
             after = FALSE)
  
  saveXML(doc, file = xmltemp, encoding = "UTF-8")
  xmltx<-readLines(xmltemp)
  xmltx<-gsub("\\{cleft","<",xmltx)
  xmltx<-gsub("\\{cright",">",xmltx)
  writeLines(xmltx,output_file)
  
 # validate_tei(output_file,"dracor-scheme.rng")
  

}
#output_file<-"testheaders.xml" # in server.R
#input_tx<-readLines("ezdmarkup.txt")
# Exemple d'utilisation
#parse_drama_text(input_tx, output_file)
#parse_drama_text("klopstock_tod-abels_ezd.txt", "toldetest.xml")

