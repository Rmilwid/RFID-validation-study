#this code uses the data collected from the tags.
#The code is divided into four sections: Participants (horses and people) and places, horses and people, horses, and people.
#Each section includes:heatmaps, mean node levels stats, network level stats, plots, node level stats and degree distributions

rm(list = ls(all = TRUE))

setwd('C:/Users/A/Documents/PhD/thesis/code/Results/')
library(igraph)
library(jpeg)
library(xlsx)
library(png)
library(gplots)
library(stringr)
library(gtools)

###########################################################################
#section 1
#user inputs

#heatmap
file3<-"C:/Users/a/Documents/PhD/thesis/thesis doc/Thesis articles/pilot study/methods/figures/res_" #heatmap(by day)
#input the file path where the edge and vertice lists are stored
inputv<-list.files("C:/Users/A/Documents/PhD/thesis/code/Results//", pattern = ".*vertices.csv") 
inpute<-list.files("C:/Users/A/Documents/PhD/thesis/code/Results//", pattern = "day.*.csv")


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

locl<-list("sandpit","arena","farrier","barn","vet","wash stall","horse","grooming stall","pasture","isolation stall", "person", "tack room","feed room","other","static")


coll<-list("turquoise1","mediumorchid1","orange","red","lightblue","yellow","brown","blue","green","deeppink","darkgreen","black","purple","gray48","white")
shapel<-list("square","square","circle","square","circle","square","circle","square","square","square","circle","square","square","square","square")
pasturel<-list("^top left$","^top right$","^Mare's$","^Pond$","^Max$","^pasture 1$","^stall rest$","^8$","^9$","^10$","^11$","^12$","^13$","^person$","^vet$","^farrier$","^person$")
pasturecol<-list("turquoise1","mediumorchid1","orange","red","lightseagreen","gray47","green","blue","green4","deeppink","darkgreen","purple","mediumorchid4","violetred4","darkblue","darkred","black")
###################################################################
#section 2-functions

#function to assign weight to the edges (based on number of interactions between indivdiual A and B)
#this function takes pre-existing weights into consideration
edge.count2 <- function(h){
  h1<-get.edgelist(h)
   for (l in (1:nrow(h1))){
    if (as.numeric(h1[l,1])>as.numeric(h1[l,2])){
      a<-h1[l,1]
      h1[l,1]<-h1[l,2]
      h1[l,2]<-a
    }
  }
  D <- data.frame(h1)  # convert to data frame 

  ones <- rep(1, nrow(D))   # a column of 1s 
  result <- aggregate(ones*E(h)$weight, by = as.list(D), FUN = sum) 
  names(result) <- c("from", "to", "weight") 
  result 

}


edge.count <- function(h){
  h1<-get.edgelist(h)

  D <- data.frame(h1)  # convert to data frame 
  
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

##########################################################
#plots and tables for participants and places            #
#                                                        #
#                                                        #
##########################################################

C<-list()
vlistnew1<-vlist
for (i in (1:nrow(vlistnew1))){
  if (vlistnew1[i,2] %in% c("person","vet","farrier")){
    vlistnew1[i,1]=paste(vlistnew1[i,2],vlistnew1[i,1])
  }
}
for (j in (1:length(elist))) {
  
  #farm1.1.g is used to create the network plots and gets stats
  farm1.1<-graph.data.frame(as.data.frame(elist[j]),directed=F, vertices=as.data.frame(vlist))
  E(farm1.1)$weight<-E(farm1.1)$Weight
  farm1.1 <- remove.edge.attribute(farm1.1,"Weight")
  for (i in (1:vcount(farm1.1))){
    if (V(farm1.1)$type[i] == "static"){
      V(farm1.1)$name[i]=paste(V(farm1.1)$type[i],V(farm1.1)$name[i])
    }
  }
  
  farm1.1.df<-get.data.frame(farm1.1)
  f.el<-static_tag_remover.func(farm1.1.df)
  f.df<-graph.data.frame((f.el),directed=F) #convert edgelist into a data frame
  f.df<-delete.vertices(f.df,V(f.df)[str_detect(V(f.df)$name,"static")])
  
  f.df<-delete1.func(farm1.1)
  f.el<-human_remover.func(f.df)
  f.df<-graph.data.frame((f.el),directed = F) #convert edgelist into a data frame
  
  f.df<-delete.edges(f.df,E(f.df)[E(f.df)$weight==0])
  
  farm1.1.g<-graph.data.frame(get.data.frame(f.df),directed = F,vertices=vlistnew1) #turn f.df in to graph object matrix
  
  #farm1.1.g<-graph.data.frame(as.data.frame(xtabs(weight ~ from+to, data=(get.data.frame(f.df)))),directed=F) #turn f.df in to graph object matrix
  #names(edge_attr(farm1.1.g))<-"weight"
  farm1.1.g<-delete.edges(farm1.1.g,E(farm1.1.g)[E(farm1.1.g)$weight==0])
  
  for (i in (1:vcount(farm1.1.g))){
    
    V(farm1.1.g)$name[i]=gsub(" ","",gsub("vet","",V(farm1.1.g)$name[i]))
    V(farm1.1.g)$name[i]=gsub(" ","",gsub("person","",V(farm1.1.g)$name[i]))
    V(farm1.1.g)$name[i]=gsub(" ","",gsub("farrier","",V(farm1.1.g)$name[i]))
    
  }
  V(farm1.1.g)$xcoord=as.character(as.data.frame(vlist)$xcoord[match(V(farm1.1.g)$name,as.data.frame(vlist)$from)])
  V(farm1.1.g)$ycoord=as.character(as.data.frame(vlist)$ycoord[match(V(farm1.1.g)$name,as.data.frame(vlist)$from)])
  
  ############
  #set node colour 
  V(farm1.1.g)$type=as.character(as.data.frame(vlist)$type[match(V(farm1.1.g)$name,as.data.frame(vlist)$from)])
  V(farm1.1.g)$color=col.func(locl,coll,farm1.1.g)
  #############
  #set node shape
  # V(farm1.1.g)$type=as.character(as.data.frame(vlist)$type[match(V(farm1.1.g)$name,as.data.frame(vlist)$from)])
  # V(farm1.1.g)$shape=shape.func(locl,shapel,farm1.1.g)
  E(farm1.1.g)$weight<-E(farm1.1.g)$weight*(1/(60*60))#convert edge weights to min
  farm1.2.g<-farm1.1.g #this will be used in the heatmap

 
  ###############
  #manipulate edgelist


  farm1.2e<-edge.count2(farm1.2.g)#apply aggregate edges based on weights
  farm1.3.g<-graph.data.frame(farm1.2e,directed=F) #turn edgelist into graph object
  #############
  #heatmap stuff
  #create substitute logger ids as id types
  V(farm1.3.g)$type2=as.character(as.data.frame(vlist)$type2[match(V(farm1.3.g)$name,as.data.frame(vlist)$from)])
  V(farm1.3.g)$name=V(farm1.3.g)$type2
  
  h1<-cbind(get.edgelist(farm1.3.g),E(farm1.3.g)$weight)
  for (l in (1:nrow(h1))){
    if (as.numeric(h1[l,1])>as.numeric(h1[l,2])){
      a<-h1[l,1]
      h1[l,1]<-h1[l,2]
      h1[l,2]<-a
   
    }
  }
  farm1.4.g<-graph.data.frame(h1,directed=F)
  E(farm1.4.g)$weight<-E(farm1.3.g)$weight
  farm1.4e<-edge.count2(farm1.4.g)#apply aggregate edges based on weights
  farm1.4.g<-graph.data.frame(farm1.4e,directed=F)
  
  V(farm1.4.g)$type=as.character(as.data.frame(vlist)$type[match(V(farm1.4.g)$name,as.data.frame(vlist)$type2)])
  V(farm1.4.g)$name<-V(farm1.4.g)$type
   
  farm1.2.am<-get.adjacency(farm1.4.g, sparse=FALSE,attr="weight") #obtain weighted adjacency matrix
  ###############
  
  #plot and save heatmaps by day
  heat1<-paste(file3,"_participants_places_day") #create file name
  heat1.1<-paste(heat1,j,sep="") #create file name
  heat1.2<-paste(heat1.1,".tiff") #create file name
  breaks1=seq(0,40,length.out=100)   # enable color transition at specified limits
  gradient11=colorpanel(sum(breaks1[-1]<=15),"white","gray15")
  gradient21=colorpanel(sum(breaks1[-1]>15),"gray15","black")
  # gradient11=colorpanel(sum(breaks1[-1]<=10),"white","yellow")
  # gradient21=colorpanel(sum(breaks1[-1]>10),"yellow","red")
  
  # verticeorder1<-axes.func()
  # vopart<-as.numeric(match(verticeorder1, colnames(farm1.2.am)))
  # 
  colours=c(gradient11,gradient21) 
  
  
  # png(file.path(heat1.2)) #save heatmap into pdf
  tiff(heat1.2,height=17,width=17, units='cm',compression='lzw', res=300)
  par(oma=c(1,1,1,1))
  
  heatmap.2(farm1.2.am,
            ##cellnote = farm1.2.am,  # same data set for cell labels
            add.expr = abline(h = c(0.5,10.5),v= c(0.5,10.5), lwd = 3),
            
            # main = paste("Day",j), # heat map title
            notecol="black",      # change font color of cell labels to black
            density.info="none",  # turns off density plot inside color legend
            trace="none",         # turns off trace lines inside the heat map
            margins =c(12,12),     # widens margins around plot
            col=colours,     
            breaks=breaks1,    # enable color transition at specified limits
            dendrogram="none",     # don't include a dendrogram
            cexCol=1.5,
            cexRow=1.5,
            Colv=FALSE,
            Rowv=F,
            revC=F)
  # labRow=V(farm1.3.g)$name,
  # labCol=V(farm1.3.g)$name)
  dev.off()
  #############
}
  
