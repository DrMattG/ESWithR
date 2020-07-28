# Visualisation of relationship between R packages that are used in Meta-analysis
library(here)
library(cranlogs)
library(RCurl)
library(XML)
library(stringr)
library(miniCRAN)
library(igraph)
library(visNetwork)
# To get a list of packages that are used in Meta-analysis we can have a look at the 
#CRAN task view for MetaAnalysis

url<-"https://CRAN.R-project.org/view=MetaAnalysis" # location of CRAN task view

# Read in the names of the packages
html <- paste(readLines(url), collapse="\n")
matched <- str_match_all(html, "<a href=\"(.*?)\"")
matched2<-unlist(matched)
tst<-sub(".*/packages/","",matched2)
packages.list<-gsub("^(.*?)/.*", "\\1", tst)
packages.list<-packages.list[2:562]

head(packages.list)

# build a dependency graph
tags <- c(packages.list)
dg <- makeDepGraph(tags, enhances = TRUE)

#split in to depends and imports

# Depends indicates dependency on a particular packages loaded (with library()) whenever focal package is loaded.
gDepends <- subgraph.edges(graph=dg, eids=which(E(dg)$type=="Depends"), delete.vertices = TRUE)

# Imports indicates packages that are needed by the focal package (this is more likely now - does not load the whole package just the used functions)
gImports <- subgraph.edges(graph=dg, eids=which(E(dg)$type=="Imports"), delete.vertices = TRUE)

#Depends visNet
vn1 <- toVisNetworkData(gDepends)
vn1$nodes$title<-vn1$nodes$label

# Nodes are sized by degree (the number of links to other packages)
degree_value1 <- degree(gDepends, mode = "all")
vn1$nodes$value <- degree_value1[match(vn1$nodes$id, names(degree_value1))]
visNetwork(nodes = vn1$nodes, edges = vn1$edges,main="Depends",height = "500px", width = "100%")%>%
  visOptions(highlightNearest = TRUE)%>%
  visNodes(color="blue")%>%
  visSave(file =paste0(here(),"/Plots/Depends.html"), selfcontained = T)


#Imports visNet
vn2 <- toVisNetworkData(gImports)
vn2$nodes$title<-vn2$nodes$label
# Nodes are sized by degree (the number of links to other packages)
degree_value2 <- degree(gImports, mode = "all")
vn2$nodes$value <- degree_value2[match(vn2$nodes$id, names(degree_value2))]
visNetwork(nodes = vn2$nodes, edges = vn2$edges,main="Imports",height = "500px", width = "100%")%>%
  visOptions(highlightNearest = TRUE)%>%
  visNodes(color="red")%>%
  visSave(file =paste0(here(),"/Plots/Imports.html"), selfcontained = T)






