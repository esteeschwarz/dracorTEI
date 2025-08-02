# 15317.info
essai to transform raw transkribus transkript source (via API) from transkribus collection to complete dracor TEI.   
test app [here](https://ada-sub.dh-index.org/shiny/work/essais/dracorTEI/).

## notes
the markup and conversion wks so far, still... / theres minimal markup in the transcript necessary defining the ^castlist and @front texts. maybe this should be done also via input fields to keep the transcript clean from markup or via transkribus meta tags per markup (dunno if meta tags are available in transkribus app...)

## workflow
to test, open server.R in R and run app. theres only one play (iwanette) in the database for test purpose, the defaults can be loaded with ID=3. 

## TODO 
- still the castlist at the beginning of the play is an issue and has to be marked up before applying the general replacements which remove linebreaks then.
- in the moment and for own purpose the text source is fetched directly from transkribus, so still to integrate is an option to read raw transcript from file.
- option to save all play meta (author, title, subtitle), in the moment only speaker names and heading level 1/2 declaration scheme is saved.
- refine xml for html escapes