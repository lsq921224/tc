library(hash)
library(RMySQL)
#for (i in dbListConnections(MySQL())) db.close(i)
mydb = dbConnect(RMySQL::MySQL(), user='test', password='', dbname='configuration', host='172.26.128.56')
res <- dbSendQuery(mydb, "SELECT siteID, siteName FROM sites where active = 1")
site_id_names <- dbFetch(res, n = -1)
site_ids <- c(site_id_names[1]$siteID)
site_names <- c(site_id_names[2]$siteName)
h <- hash(site_names, site_ids)
dbDisconnect(mydb)

html_list <- function(vars, id) {
  hl <- paste0("<ul id=\'",id,"\' class='stab'>")
  for(i in vars) hl <- paste0(hl, "<li class='ui-state-default stab'><span class='label'>",i,"</span></li>")
  paste0(hl, "</ul>")
}

returnOrder <- function(inputId, vars) {
  tagList(
    singleton(tags$head(tags$script(src = 'js/sort.js'))),
    singleton(tags$head(tags$link(rel = 'stylesheet', type = 'text/css', href = 'sort.css'))),
    HTML(html_list(vars, inputId)),
    tags$script(paste0("$(function() {$( '#",inputId,"' ).sortable({placeholder: 'ui-state-highlight'}); $( '#",inputId,"' ).disableSelection(); });"))
  )
}
