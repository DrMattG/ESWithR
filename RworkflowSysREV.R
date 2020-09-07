rm(list=ls())#clear the workspace
#change the library path
.libPaths("E:/R folder/R_lib")
library(cranlogs)
library(RCurl)
library(XML)
library(stringr)
library(miniCRAN)
library(igraph)
#CRAN task view for MetaAnalysis
url<-"https://CRAN.R-project.org/view=MetaAnalysis"
html <- paste(readLines(url), collapse="\n")
matched <- str_match_all(html, "<a href=\"(.*?)\"")
matched2<-unlist(matched)
tst<-sub(".*/packages/","",matched2)
packages.list<-gsub("^(.*?)/.*", "\\1", tst)
packages.list<-packages.list[2:562]


tags <- c(packages.list)
pkgDep(tags)
dg <- makeDepGraph(tags, enhances = TRUE)
set.seed(1)
plot(dg, legendPosition = c(-1, 1), vertex.size = 20)
#??makeDepGraph
tagList<-pkgDep(tags)
head(tagList)
tagList
V(dg)
E(dg)
degree(dg)
g=delete.vertices(dg,which(degree(dg)<5))
E(g)$weight <- 1

#lost attribute type here but this shows the full complexity can
#use a reduced version of the original graph
G2 <- graph.adjacency(get.adjacency(g),weighted=TRUE)
E(G2)$weight
G2<-delete.edges(G2,which(E(G2)$weight<2))
plot(G2,edge.arrow.size=.0001,layout =layout.fruchterman.reingold, vertex.size=15, vertex.label=NA)
#####
degree(g)
(closeness(g)*100000)
betweenness(g)
degree(g,mode = "in")

#g1<-delete.vertices(g,which(betweenness(g)<100))
g1<-delete.vertices(g,which(degree(g, mode="in")<5))
E(g1)
V(g1)
E(g1)$color <- as.factor(E(g1)$type)
g2<-simplify(g1)
E(g2)$color <- as.factor(E(g2)$type)
V(g2)
E(g1)$type
par(bg="gray40")

plot(g1,edge.arrow.size=.0001,layout =layout_nicely, vertex.size=15,vertex.label = ifelse(betweenness(g1) > 15, V(g1)$name , NA), vertex.color="red", vertex.label.color="white", vertex.label.cex=1.4, margin=-0.3)
plot(g2,edge.arrow.size=.0001,layout =layout.fruchterman.reingold, vertex.size=8, vertex.label=NA)

#####
g3<-delete.vertices(g1,which(E(g1)$type=="Suggested"))
plot(g3,edge.arrow.size=.0001,layout =layout_nicely, vertex.size=15,vertex.label = ifelse(betweenness(g3) > 15, V(g3)$name , NA), vertex.color="red", vertex.label.color="white", vertex.label.cex=1.4, margin=-0.3)
plot(g3,edge.arrow.size=.0001,layout =layout_nicely, vertex.size=15,vertex.label = , vertex.color="red", vertex.label.color="white", vertex.label.cex=1.4, margin=-0.3)


V(g3)$onlist<-V(g3)$name%in%packages.list
plot(g3,edge.arrow.size=.0001,layout =layout_nicely, vertex.size=15,vertex.label = ifelse(V(g3)$onlist==TRUE, V(g3)$name , NA), vertex.color="red", vertex.label.color="white", vertex.label.cex=0.8, margin=-0.3)
#need to show size by downloads
downlds<-cran_downloads(packages = packages.list)
mdown<-cran_downloads(packages=c("aggregation","altmeta","bamdit","bayesmeta","bmeta","bspmma","CAMAN","CIAAWconsensus","clubSandwich","compute.es","ConfoundedMeta","CopulaREMADA","CPBayes","CRTSize","diagmeta","dosresmeta","EasyStrata","ecoreg","effsize","epiR","esc","etma","exactmeta","extfunnel","forestmodel","forestplot","gap","gemtc","getmstatistic","gmeta","hetmeta","ipdmeta","joineRmeta","joint.Cox","MAc","MAd","mada","MAVIS","MendelianRandomization","meta","meta4diag","MetaAnalyser","MetABEL","metaBMA","metacart","metacor","metafor","metaforest","metafuse","metagear","metagen","MetaIntegrator","metaLik","metaMA","metamisc","metansue","metap","MetaPath","MetaPCA","metaplotr","metaplus","MetaQC","metaRNASeq","metaSEM","metasens","MetaSKAT","MetaSubtract","metatest","Metatron","metavcov","metaviz","mmeta","MultiMeta","mvmeta","mvtmeta","netmeta","nmaINLA","nmathresh","pcnetmeta","pimeta","psychmeta","psychometric","PubBias","RandMeta","ratesci","RBesT","RcmdrPlugin.EZR","RcmdrPlugin.RMTCJags","revtools","rma.exact","rmeta","robumeta","SAMURAI","SCMA","selectMeta","seqMeta","surrosurv","TFisher","weightr","xmeta"), from = "2000-01-01", to = "2017-12-30")
sum(mdown$count)
mdown$count



#######################################################################
#install.packages("visNetwork")
library(visNetwork)
packages<-read.csv("https://raw.githubusercontent.com/mjwestgate/starchart/master/inst/extdata/packages.csv")
tags <- packages$package_name
str(tags)
tags<-as.character(tags)
pkgDep(tags)
dg <- makeDepGraph(tags, enhances = TRUE)
set.seed(1)
plot(dg, legendPosition = c(-1, 1), vertex.size = 20)
V(dg)
E(dg)$type
gDepends <- subgraph.edges(graph=dg, eids=which(E(dg)$type=="Depends"), delete.vertices = TRUE)
gImports <- subgraph.edges(graph=dg, eids=which(E(dg)$type=="Imports"), delete.vertices = TRUE)
par(bg="gray40")
plot(gDepends,edge.arrow.size=.0001,layout =layout_nicely, vertex.size=10,vertex.label = ifelse(betweenness(gDepends) > 3, V(gDepends)$name , NA), vertex.color="red", vertex.label.color="white", vertex.label.cex=1.4, margin=-0.3)
plot(gImports,edge.arrow.size=.0001,layout =layout_nicely, vertex.size=10,vertex.label = ifelse(betweenness(gImports) >150, V(gImports)$name , NA), vertex.color="red", vertex.label.color="white", vertex.label.cex=1.4, margin=-0.3)
#####
#Depends
vn <- toVisNetworkData(gDepends)
vn$nodes$title<-vn$nodes$label
degree_value <- degree(gDepends, mode = "all")
vn$nodes$value <- degree_value[match(vn$nodes$id, names(degree_value))]
visNetwork(nodes = vn$nodes, edges = vn$edges,main="Depends",height = "500px", width = "100%")%>%
  visOptions(highlightNearest = TRUE)%>%
  visSave(file ="D:/R folder/REvS/Depends.html", selfcontained = T)
#Imports
#too large
degree(gImports)
gImports<-delete.vertices(gImports, degree(gImports)<3)
vn2 <- toVisNetworkData(gImports)
vn2$nodes$title<-vn2$nodes$label
degree_value2 <- degree(gImports, mode = "all")
vn2$nodes$value <- degree_value[match(vn2$nodes$id, names(degree_value2))]
visNetwork(nodes = vn2$nodes, edges = vn2$edges,main="Imports",height = "500px", width = "100%")%>%
  visOptions(highlightNearest = TRUE)%>%
  visNodes(color="red")%>%
  visSave(file ="D:/R folder/REvS/Imports.html", selfcontained = T)
