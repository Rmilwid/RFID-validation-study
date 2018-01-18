#this code uses the data collected from the tags.
#The code is divided into four sections: Participants (horses and people) and places, horses and people, horses, and people.
#Each section includes:heatmaps, mean node levels stats, network level stats, plots, node level stats and degree distributions

rm(list = ls(all = TRUE))

setwd('C:/Users/A/Documents/PhD/thesis/code/Results/Weybread Hill Farm_nov15_2016')
library(igraph)
library(jpeg)
library(xlsx)
library(png)
library(gplots)
library(stringr)

###########################################################################
#section 1
#user inputs

#input the filepath and file name (.pdf) to save the plots 
file1<- "C:/Users/a/Documents/PhD/thesis/thesis doc/Thesis articles/pilot study/methods/figures/RES_" #plots by days per farm
#input the file path where the edge and vertice lists are stored
inputv<-list.files("C:/Users/A/Documents/PhD/thesis/code/Results/Weybread Hill Farm_nov15_2016/", pattern = ".*vertices.csv") 
inpute<-list.files("C:/Users/A/Documents/PhD/thesis/code/Results/Weybread Hill Farm_nov15_2016/", pattern = "day.*.csv")


#use these for wing function where the two numbers in the coordinate represent the min and max value of the wing
#use these for wing function. xi represent the x-coordinate boundaries for the wings on the x-axis (see map of barn ) and yi represents th y-coordinate boundaries for the wings on the y-axi 
#i.e. wing1 will be bounded by (x1,y1)
#####using wings
x1<-list(0,8)
x2<-list(8,16)
x3<-list(16,20)

y1<-list(0,22)
y2<-list(0,22)
y3<-list(0,22)
x_coords<-rbind(x1,x2,x3)#insert all the x-coordinates listed above into this vector
y_coords<-rbind(y1,y2,y3)#insert all the y-coordinates listed above into this vector
wings<-list("winga","wingb","wingc")#these names should be in accordance with the facility map.
wingsb<-list("winga","wingb","wingc")#these names should be in accordance with the facility map.

locl<-list("pasture","horse")
coll<-list("green","sienna4")



shapel<-list("square","circle")
pasturel<-list("^1$","^2$","^3$","^4$","^stall rest$","^person$","^vet$","^farrier$")
pasturecol<-list("red","blue","purple","turquoise1","lightseagreen","gray47","green","mediumorchid1")
###################################################################
#section 2-functions

#function to assign weight to the edges (based on number of interactions between indivdiual A and B)
#this function takes pre-existing weights into consideration
edge.count2 <- function(h){ 
  D <- data.frame(get.edgelist(h))  # convert to data frame 
  ones <- rep(1, nrow(D))   # a column of 1s 
  result <- aggregate(ones*E(h)$weight, by = as.list(D), FUN = sum) 
  names(result) <- c("from", "to", "weight") 
  result 
}
#set node color in the plots based on the type of node. the loc, and col inputs should 
#be lists of locations (or participant type) and their respecitve colours(loc=barn etc. color= red. etc farm=farm1.1.g etc)
col.func<-function(loc,col,farm){ 
  V(farm)$color=V(farm)$type
  for (i in (1:length(loc))){
    V(farm)$color=gsub(loc[i],col[i],V(farm)$color)
  }
  return(V(farm)$color)
}

#use this function to colour horses with respect to their pasture number. i.e. all horses from pasture 1 will be coloured blue
pasture.col.func<-function(pasture,col,farm){
  V(farm)$color=V(farm)$pasture
  
  for (i in (1:length(pasture))){
    V(farm)$color=gsub(pasture[i],col[i],V(farm)$color)
    
  }
  return(V(farm)$color)
}

#set node shape in the plots based on the type of node. the loc, and shp inputs should 
#be lists of locations (or participant type) and their respecitve shapes (loc=barn etc. shape= square etc farm=farm1.1.g etc)
shape.func<-function(loc,shp,farm){ 
  V(farm)$shape=V(farm)$type
  for (i in (1:length(loc))){
    V(farm)$shape=gsub(loc[i],shp[i],V(farm)$shape)
  }
  return(V(farm)$shape)
}
#this function changes the ID name to the type of participant
name.func<-function(loc,farm){ #set node name (loc=barn etc.  farm=farm1.2.g etc)
  V(farm)$name=V(farm)$type
  for (i in (1:length(loc))){
    V(farm)$name=gsub(loc[i],loc[i],V(farm)$name)
  }
  return(V(farm)$name)
}

#this function seperates horses by wing. It does this by naming horses in the same wing, by their wing name as specified in the 
#"wings" vector above.The inputs include : 1) two matrices of vectors: The x vector should have n rows of 2 vecors where x11<=x<x12. 
#(the same goes for the y vector),2)farm=data frame(called farmx in this code, i.e. farm3), 3) wing=list of wings, 4) pattern="horse"

wing.func<-function(xcoords,ycoords,farm,wing,pattern){   
  V(farm)$name=V(farm)$type 
  
  for (j in (1:vcount(farm))){
    if (V(farm)$name[j]=="horse"){     
      for (i in (1:nrow(xcoords))){
        if ((xcoords[i]<= as.numeric(V(farm)$xcoord[j]) && as.numeric(V(farm)$xcoord[j])<xcoords[i,2]) && (ycoords[i]<= as.numeric(V(farm)$ycoord[j]) && as.numeric(V(farm)$ycoord[j])< ycoords[i,2])){
          
          
          V(farm)$name[j]= gsub(pattern,wing[i],V(farm)$name[j])
          
        }
      }
    }
    
  }
  return(V(farm)$name)
}
#use this function to remove all contacts with the static tag. The "type" for the static tag should be "static" in teh vertice list
static_tag_remover.func<-function(dataframe){ 
  for (x in (1:NROW(dataframe))){  
    if(str_detect(dataframe[x,1],"static")|| str_detect(dataframe[x,2],"static")){ 
      #dataframe[x,1]<-0    
      #dataframe[x,2]<-0
      dataframe[x,3]<-0
      dataframe[x,4]<-0
      dataframe[x,7]<-0
    }
  }
  return(dataframe)}
#this function returns the normalized node strengths (node strength/ largest node strength)
strength_norm.func=function(farm){
  V(farm)$strength<-strength(farm,mode="all", loops=F)
  V(farm)$strengthnorm<-V(farm)$strength*(1/max(V(farm)$strength))
  return(V(farm)$strengthnorm)
}

delete1.func<-function(farm){
  for (i in (1:vcount(farm))){
    
    if (V(farm)$type[i] %in% c("vet","farrier","person")){
      V(farm)$name[i]=paste(V(farm)$type[i],V(farm)$name[i])
    }}
  
  farm2<-get.data.frame(farm)
  
  
  return(farm2)
}
#use this function to remove all contacts with the static tag. The "type" for the static tag should be "static" in teh vertice list
human_remover.func<-function(dataframe){ 
  for (x in (1:NROW(dataframe))){  
    if(str_detect(dataframe[x,1],"static")|| str_detect(dataframe[x,2],"static")){ 
      #dataframe[x,1]<-0    
      #dataframe[x,2]<-0
      dataframe[x,3]<-0
      dataframe[x,4]<-0
      dataframe[x,7]<-0
    }
    if("TRUE" %in% c(str_detect(dataframe[x,1],c("person","vet","farrier"))) && "TRUE" %in% c(str_detect(dataframe[x,2],c("person","vet","farrier")))){
      #dataframe[x,1]<-0
      # dataframe[x,2]<-0
      dataframe[x,3]<-0
      dataframe[x,4]<-0
      dataframe[x,5]<-0
      dataframe[x,6]<-0
      dataframe[x,7]<-0
    }
    
  }
  return(dataframe)}


###################################################################
#section 3
#import list of vertex and edge files

verticeL<-read.csv(inputv,header=T)
vlist<-verticeL
vlist<-vlist[order(vlist$pasture),]
h2<-1
p2<-1
v2<-1
f2<-1
vlist2<-vlist
for (i in (1:nrow(vlist2))){
  if (vlist2[i,2]=="horse"){
    vlist2[i,1]<-paste(vlist2[i,2],h2)
    h2=h2+1
  }
  if (vlist2[i,2]=="person"){
    vlist2[i,1]<-paste(vlist2[i,2],p2)
    p2=p2+1
  }
  if (vlist2[i,2]=="vet"){
    vlist2[i,1]<-paste(vlist2[i,2],v2)
    v2=v2+1
  }
  if (vlist2[i,2]=="farrier"){
    vlist2[i,1]<-paste(vlist2[i,2],f2)
    f2=f2+1
  }
}

elist<-list()
metaL<-list()
#read in vertice and edge files and store in vlist and elist
for (i in (1:length(inpute))){  
  edgeL<-read.csv(inpute[i],header=T)
  elist<-c(elist,list(edgeL))
  farmM<-graph.data.frame(edgeL,directed=F, vertices=verticeL) #this is used to plot the location of the nodes
  
  metaL<- c(metaL,list(data.frame(list("name"=c(V(farmM)$name), #this is used to plot the location of the nodes
                                       "type"=c(V(farmM)$type),
                                       "lon" = c(V(farmM)$xcoord), 
                                       "lat" = c(V(farmM)$ycoord))))) 
}
#function to count the number of differnt types. This should only be used for the matrix2.func for the participants and places section.
#the output of this function will be the "num_types" entry in the matrix2.func function
#here v_list is the vertice list (verticeL)
#exceptions are the categories you don't want to inlcude. By default, it is set to NULL.
num_type.func<-function(v_list,exceptions=NULL){
  
  num=length(unique(v_list$type))-length(exceptions)
  return(num)
}

#this function orders the heatmap vertices based on the verticeL list. 
#input includes the categories you wish to include
vertices<-c()
axes.func<-function(inclusion_cat=lapply(num_type.func(vlist),toString)){
  for (i in (1:nrow(vlist))){
    if (vlist[i,2] %in% c(inclusion_cat)){
      
      vertices<-c(vertices,vlist[i,1])
      
    }
    
  }  
  return(vertices)
}
vertices2<-c()
axes.func2<-function(inclusion_cat=lapply(num_type.func(vlist2),toString)){
  for (i in (1:nrow(vlist2))){
    if (vlist2[i,2] %in% c(inclusion_cat)){
      
      vertices2<-c(vertices2,vlist2[i,1])
      
    }
    
  }  
  return(vertices2)
}
#function to count the number of differnt types. This should only be used for the matrix2.func for the participants and places section.
#the output of this function will be the "num_types" entry in the matrix2.func function
#here v_list is the vertice list (verticeL)
#exceptions are the categories you don't want to inlcude. By default, it is set to NULL.
num_type.func<-function(v_list,exceptions=NULL){
  
  num=(unique(v_list$type))
  return(num)
}


C2<-list()
C2b<-list()
# filedest1<-paste(filenotes,"horses_cluster_notes.txt")
# # Start writing to an output file
# sink(filedest1)
# print("This doc contains info on the numbers and members of the horse clusters on each day.")
# sink()
vlistnew<-vlist
for (i in (1:nrow(vlistnew))){
  if (vlistnew[i,2] == "static"){
    vlistnew[i,1]=paste(vlistnew[i,2],vlistnew[i,1])
  }
}
for (k in (1:length(elist))){
  farm2.1<-graph.data.frame(as.data.frame(elist[k]),directed=F, vertices=as.data.frame(vlist))
  
  E(farm2.1)$weight<-E(farm2.1)$Weight
  farm2.1 <- remove.edge.attribute(farm2.1,"Weight")
  for (i in (1:vcount(farm2.1))){
    if (V(farm2.1)$type[i] == "static"){
      V(farm2.1)$name[i]=paste(V(farm2.1)$type[i],V(farm2.1)$name[i])
    }
  }
  
  farm2.1.df<-get.data.frame(farm2.1)
  f.el2<-static_tag_remover.func(farm2.1.df)
  
  f.df2<-graph.data.frame((f.el2),directed = F,vertices=vlistnew) #convert edgelist into a data frame
  V(f.df2)$type=as.character(as.data.frame(vlist)$type[match(V(f.df2)$name,as.data.frame(vlist)$from)])
  
  f.df2<-delete.edges(f.df2,E(f.df2)[E(f.df2)$weight==0])
  
  farm2.1.g<-graph.data.frame(get.data.frame(f.df2),directed = F,vertices=vlist) #turn f.df in to graph object matrix
  
  #farm2.1.g<-graph.data.frame(as.data.frame(xtabs(weight ~ from+to, data=(get.data.frame(f.df2)))),directed = F,vertices=vlist) #turn f.df in to graph object matrix
  #names(edge_attr(farm2.1.g))<-"weight"
  farm2.1.g<-delete.edges(farm2.1.g,E(farm2.1.g)[E(farm2.1.g)$weight==0])
  
  E(farm2.1.g)$weight<-E(farm2.1.g)$weight#*(1/(60*60)) #convert edge weights to min
  
  farm2c<-data.frame(get.edgelist(farm2.1.g),directed=F,weight=E(farm2.1.g)$weight) #this will be used for the layout specific plots
  
  #######set node colour
  V(farm2.1.g)$pasture=as.character(as.data.frame(vlist)$pasture[match(V(farm2.1.g)$name,as.data.frame(vlist)$from)])
  # V(farm2.1.g)$color=pasture.col.func(pasturel,pasturecol,farm2.1.g)
  
  #V(farm2.1.g)$type=as.character(as.data.frame(vlist)$type[match(V(farm2.1.g)$name,as.data.frame(vlist)$from)])
  # V(farm2.1.g)$color=col.func(locl,coll,farm2.1.g)
  ######### set node shape
  V(farm2.1.g)$type=as.character(as.data.frame(vlist)$type[match(V(farm2.1.g)$name,as.data.frame(vlist)$from)])
  V(farm2.1.g)$shape=shape.func(locl,shapel,farm2.1.g)
  
  V(farm2.1.g)$xcoord=as.character(as.data.frame(vlist)$xcoord[match(V(farm2.1.g)$name,as.data.frame(vlist)$from)])
  V(farm2.1.g)$ycoord=as.character(as.data.frame(vlist)$ycoord[match(V(farm2.1.g)$name,as.data.frame(vlist)$from)])
  
  
  ##############
  #seperate horses by wings in barn
  farm2wing<-farm2.1.g
  
  V(farm2wing)$name<-wing.func(x_coords,y_coords,farm2wing,wings,"horse")
  
  ##################
  #plots:
  #layout specific
  #if (0<length(which(V(farm2.1.g)$type=="horse"))){  #only carry out the loop if there's at least one horse
  
  smeta2<-data.frame(subset(as.data.frame(metaL[k]),as.data.frame(metaL[k])$type %in% c("horse","pasture"))) #subset of vertices and locations with horse only
  
  plotfarm2 <- graph.data.frame(farm2c, directed = FALSE, vertices = as.data.frame(metaL[k]))
  plotfarm2s<- induced.subgraph((plotfarm2), which(V(plotfarm2)$type %in% c("horse","pasture")))
  
  V(plotfarm2s)$pasture=as.character(as.data.frame(vlist)$pasture[match(V(plotfarm2s)$name,as.data.frame(vlist)$from)])
  V(plotfarm2s)$shape=shape.func(locl,shapel,plotfarm2s)
  V(plotfarm2s)$color="gray"
  # V(plotfarm2s)$color=shape.func(locl,coll,plotfarm2s)
  
  V(plotfarm2s)$degree<-degree(plotfarm2s, mode="all",loops=F,normalized = F)
  V(plotfarm2s)$strength<-strength(plotfarm2s, mode="all",loops=F)
  
  nodesize1<-V(plotfarm2s)$strength
  
  lo2 <- layout.norm(as.matrix(smeta2[,3:4]))
  fpl2<-paste(file1,"_horses_layout_day")
  fl2<-paste(fpl2,k)
  flf2<-paste(fl2,".tiff")
  
 tiff(flf2,height=12,width=17, units='cm', compression="lzw",res=300)  
  # png(file.path(flf2)) #layout specific plot
  
  plot.igraph(plotfarm2s,
              layout = lo2, 
              rescale = FALSE, 
              # main=paste("Day", k, sep = " "),
              # vertex.frame.color=NA, 
              edge.width=log(E(plotfarm2s)$weight)*.45,
              edge.curved=.25,
              # vertex.size=log(nodesize1+1),
              vertex.size=15,
              
              vertex.label=NA,
              #edge.label=E(plotfarm2s)$weight,
              vertex.label.cex=.75,
              edge.color="black")
  
  dev.off()

}


########################################################
#plot human contact networks                           #
#                                                      #
########################################################
graphics.off()

################################################################
print("end of script")