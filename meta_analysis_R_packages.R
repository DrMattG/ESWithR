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
sort(degree(dg))
Isolated = which(degree(dg)<10)
dg2 = delete.vertices(dg, Isolated)
V(dg2)
vn <- toVisNetworkData(dg2)
vn$nodes$title<-vn$nodes$label

# Nodes are sized by degree (the number of links to other packages)
degree_value <- degree(dg2, mode = "all")
vn$nodes$value <- degree_value[match(vn$nodes$id, names(degree_value))]
visNetwork(nodes = vn$nodes, edges = vn$edges,main="Both",height = "500px", width = "100%")%>%
  visOptions(highlightNearest = TRUE)%>%
  visNodes(color="blue")%>%
  visSave(file =paste0(here(),"/Plots/both.html"), selfcontained = T)



#split in to depends and imports

# Depends indicates dependency on a particular packages loaded (with library()) whenever focal package is loaded.
gDepends <- subgraph.edges(graph=dg, eids=which(E(dg)$type=="Depends"), delete.vertices = TRUE)

# Imports indicates packages that are needed by the focal package (this is more likely now - does not load the whole package just the used functions)
gImports <- subgraph.edges(graph=dg, eids=which(E(dg)$type=="Imports"), delete.vertices = TRUE)

sort(degree(gImports))

V(gImports)$name

plot(gImports,vertex.label = ifelse(degree(gImports) > 25, V(gImports)$name, NA), vertex.size=4, edge.arrow.size=.2 )

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
visNetwork(nodes = vn2$nodes, edges = vn2$edges,main="Meta-analysis packages",height = "500px", width = "100%")%>%
  visOptions(highlightNearest = TRUE)%>%
  visNodes(color="red")%>%
  visSave(file =paste0(here(),"/Plots/Imports.html"), selfcontained = T)

degree_value2[order(degree_value2)]

##### Remove nodes degree <5

Isolated = which(degree(gImports)<5)
G2 = delete.vertices(gImports, Isolated)
LO2 = layout_with_gem(G2)
par(bg="gray40")
plot(G2, layout=LO2, vertex.label=NA, edge.arrow.size=0.2, vertex.size=degree(G2), vertex.color="red", edge.color="#C0C0C0")

vn2 <- toVisNetworkData(G2)
vn2$nodes$title<-vn2$nodes$label
# Nodes are sized by degree (the number of links to other packages)
degree_value2 <- degree(G2, mode = "all")
vn2$nodes$value <- degree_value2[match(vn2$nodes$id, names(degree_value2))]
visNetwork(nodes = vn2$nodes, edges = vn2$edges,main="Meta-analysis packages",height = "500px", width = "100%")%>%
  visOptions(highlightNearest = TRUE)%>%
  visNodes(color="red")%>%
  visSave(file =paste0(here(),"/Plots/Imports.html"), selfcontained = T)


# ##### Remove nodes degree <8
# 
# Isolated = which(degree(gImports)<8)
# G2 = delete.vertices(gImports, Isolated)
# LO2 = layout_with_fr(G2)
# plot(G2, layout=LO2)
# 
# vn2 <- toVisNetworkData(G2)
# vn2$nodes$title<-vn2$nodes$label
# # Nodes are sized by degree (the number of links to other packages)
# degree_value2 <- degree(G2, mode = "all")
# vn2$nodes$value <- degree_value2[match(vn2$nodes$id, names(degree_value2))]
# visNetwork(nodes = vn2$nodes, edges = vn2$edges,main="Meta-analysis packages",height = "500px", width = "100%")%>%
#   visOptions(highlightNearest = TRUE)%>%
#   visNodes(color="red")%>%
#   visSave(file =paste0(here(),"/Plots/Imports.html"), selfcontained = T)
# 
# 
