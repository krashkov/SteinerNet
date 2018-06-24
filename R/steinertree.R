####################################################################################################################
# 21 Nov 2017
#
#  update version 1.4 to work with igraph package
#
#This approximation heuristic is inspired from one steiner heuristic but developed according to steiner trees on graph network 
#
# it countinues redusing in periods until it finds a fix point.it is a fix point becasue the size of steiner tree is a fix positive number,and it is a reductive
# algorithm that tends toward the optimal solution
#
# in the arguments it accespt to check vertices if they have labels,and asks for number of checks in every reduction phase,and also aks if to print infomation of
# algorithm while processing
# 
###################################################################################################################
appr_steiner = function(runtime=5, labelcheck=TRUE, coloring= TRUE,  ter_list=NULL, glist)
{
	printworkflow=FALSE
	color=c()	
	graph=glist[[1]]
	 if (!is.null(graph)){
		if (!is.connected(graph) ){ print("Error : the graph is disconnected Steiner tree does not exist.") }
		g= graph
		g=as.undirected(g)
	 }
	 #-----------------------------label checking in the begining and end to make sure the graph is correctly labeled and if not label them in here
	 if(!is.null(ter_list)) 
	  {
		 igraph::V(g)$color="yellow"
		 V(g)[ter_list]$color="red"
	  }
	#-----------------------------label checking in the begining and end to make sure the graph is correctly labeled and if not label them in here
	if(labelcheck){
	  labels=c(igraph::V(g)$label) #list of vertices
	  names= c(igraph::V(g)$name)
	  
	  if(is.null(labels) && is.null(names))   {    #if graph has not labels make labels for it
	    r0 =1:(length(V(g)))
	    igraph::V(g)$label=sapply (r0 ,function(r0) toString(r0) )
	    labels=  igraph::V(g)$label
	  }
	  
	  if(is.null(labels) && !is.null(names)){
	    igraph::V(g)$label = igraph::V(g)$name
	    labels=names
	  }
	  
	  if(!is.null(labels) && is.null(names)){
	    igraph::V(g)$name =  igraph::V(g)$label
	    names=labels
	  }
	  
	  len=length(labels)
	  r0 =1:(len)
	  t =sapply (r0 ,function(r0) toString(labels[r0]) )
	  temp= igraph::V(g)$name  #subgraph function was crashing if labels were alphabetic ,so here we keep a copy of them and work with the index of vertices instead
	  igraph::V(g)$label=r0
	  igraph::V(g)$name=r0
	}
	#--------------------------------
 	 terminals = V(g)[color=="red"]
	 set=c()
	 paths=lapply(terminals,function(x) get.all.shortest.paths(g,x,terminals )$res)
	 r=1:length(paths)
	 t1=lapply(r,function(r) length(paths))
	 distances= lapply(r,function(r) lapply(1:t1[[r]],function(x,y) length(paths[[y]][[x]]),y=r ))
	 neighbour_distance= max(unlist(distances))
	 #paths= unique (unlist(E(minimum.spanning.tree(subgraph(g,(unique(unlist((paths)))))))))
	 paths= unique(unlist(paths))
	 set=V(g)[paths]
	 size=length(E(minimum.spanning.tree(induced_subgraph(g,union(terminals,set)))))
	 #a=Sys.time()
	 #b=Sys.time()
	 j=0
	 sizerec=size+1
	 while(size < sizerec)
	 {
	  j=j+1
	  i=1
	  if(printworkflow){
	  	cat("try number ",j," size is now ",size ,"oldsize=",sizerec ,".\n",sep=" ")
	  }
	  sizerec = size
		 while(i<runtime)
		 {
			seed=sample(unlist(neighborhood(g, neighbour_distance, nodes=terminals, mode="all")),1, replace = TRUE, prob = NULL)
			paths2= get.all.shortest.paths(g,seed,terminals )
			paths2 = paths2$res
			seedpaths=unique(unlist(paths2))
			set2=union(set,V(g)[seedpaths])
			size2=length(E(minimum.spanning.tree(induced_subgraph(g,union(terminals,set2)))))
			if(size2<size){
				if(printworkflow){
					cat("found addive",seed ,"\n",sep=" ")
				}
				size<-size2		
				set <-set2	
			}
			b=Sys.time()

			seed=sample(set,1, replace = TRUE, prob = NULL)
			set2=V(g)[setdiff(set,seed)]
			size2=length(E(minimum.spanning.tree(induced_subgraph(g,union(terminals,set2)))))
			if(size2<size && is.connected(minimum.spanning.tree(induced_subgraph(g,union(terminals,set2))))){
				size<-size2		
				set <-set2
				if(printworkflow){
					cat("found reductive",seed ,"\n",sep=" ")
					cat("try number ",j," size is now ",size ,".\n",sep=" ")
				}	
				
			}
			#b=Sys.time()
			i=i+1
		 }
	 }
	 steinert=(minimum.spanning.tree(induced_subgraph(g,union(terminals,set))))
	 #here we delete nonterminal vertices that has degree of 1
	 	 a=V(steinert)$color
		 b=igraph::degree(steinert, v=V(steinert), mode = c("all")) 
		 a1=match(a,"yellow")
		 b1=match(b,"1")
		 opt= sapply(1:length(a1),function(r) (a1[r]*b1[r] ) )
		 new_g <- delete.vertices(steinert,grep(1,opt)-1)
		 steinert= new_g
	 #one of R problems is that if you sent a graph as output of a function it would miss
	 #meta infomration about labels so I put it inside a list and return the list instead
	 if(labelcheck){
		igraph::V(g)$label=temp
		igraph::V(g)$name= temp
		labellist=c()
		r0 =1:(length(V(steinert)))
		labellist =sapply (r0 ,function(r0) temp[ as.integer(V(steinert)$label[r0])] )
		V(steinert)$label=labellist
		V(steinert)$name=labellist
	 }
	 glst=c()
	 if(coloring)
	 {
		glst[[length(glst)+1]] <- g
	 }
	 glst[[length(glst)+1]] <- steinert
	 return(glst)
}


#one more resource for 1- stiener tree :users.ece.gatech.edu/limsk/book/slides/ppt/1-steiner-routing.pptusers.ece.gatech.edu/limsk/book/slides/ppt/1-steiner-routing.ppt
#
#	according to the paper "The 1-steiner tree problem ,George Georgakopoulos, Christos H Papadimitriou" it is only for case the steiner tree has 
# only one node more than the mst.
#
# in
# http://www.sciencedirect.com/science/article/pii/0196677487900320
# also in
#https://docs.google.com/viewer?a=v&q=cache:rj49c9_w3hcJ:www.ics.uci.edu/~eppstein/pubs/AroBerEpp-DCG-94.pdf+&hl=en&pid=bl&srcid=ADGEESiRMqijggjM8OD9zJjMPyJ8p68h9Bh0Prsoavp-57DV3y7gxx6ZzV1uGQkNXk7xBu62W9m-IInwmk0oJjgXQYrbuVZOId6UyNEegvUFEtI4g7GVD8rcfKnmeVMQtt2igUWeGhJg&sig=AHIEtbTPKqcvrkr7VMJAdxGV_MQdrctyxA
#
# 	1-steiner problem is for case that the steiner tree has one more node than mst so is not interesting for us,instead I do something else:
# 	I propose a gradient descent algorithm,at first we start with all nodes and start reducing while we have aedicing(like hill climing)
#	in one-steiner they are testing adding one edge to the mst,but for the later problem we have to reduce from the set of edges.and keep on for
# 	a specified time and take care of the condition that it still remaind connected.
#	or we can repeat 1-steinr tree until we have achived a fix point (instead of a time)
#
#	there is a problem in 1-steiner algorithm above(said by mattias haubtman),and that is it assuming that 
#	the minimum spanning three algorithm returns the the nodes between terminals ,(is connected),but if it was this way 
#	the mst would return the steiner 	tree ,not mst
#	the function induced_subgraph is only adding the edges that are between the corrent number of vertices.
#
#	so algorithm  above can be used to enhance the result of other steiner tree algorithm,if instead of the terminals we put the result
#	steiner tree  but cannot be run alone
#
#	and the algorithm below can be infered from that(insteat of terminals we add all shortest pathes)and then reduce it instead of increasing it 
#
#	second allgorithm by me :Simulated annealing (inspired from the reductive algorithm of Mattias haubtman,his algorithm 
#	is wrong becasue if we had mst of terminals then ther mst was a steiner tree)
#######################################################################################################################

############################################################################################################################################################
#																			
#	This is a code version of the steiner tree with shortest path heuristic ,that is starting from one 
#	tarminal node and grows a subgraph		
#	In the "Steiner Tree Problem "book it is entitled Shortest with Original Path Heuristic,
#       
#       reference:
#       1.Section 4.1.3 of the book "The steiner tree Problem", Petter,L,Hammer
#	2."An approximate solution for the Steiner problem in graphs" ,  H Takahashi, A Matsuyama 
# 
#	in a manner it is different,with the proposed algorithm in the paper that in the paper all shortest path to a specefic 
#	terminal is considered to make the tree and we consider the distance to the last terminal added to the subreee
#	worst case error ratio : 2n-2		
#	run time:O(n(e+v(log v ))
#
#	Sample run:
#	stlst = steinertree1(g)
#	tkplot(stlst[[1]])
# 	tkplot(stlst[[2]])
#
#	The color of terminal nodes inside the graph are red 
#
#	It returns a list of 2 graph objects that consists of 2 graphs,the first is the entered graph but with 
#	colored nodes that stick the tree out on the 
#	graph(good for observing the tree insdie the graph and comparison),and the second graph object 
#	is the result steiner tree an independent graph object
#
#
#	If coloring is True it returns the original graph after coloring it with terminal and steiner nodes,but if 
#	coloring is Flase it returns just a new tree that is the steiner tree
#
############################################################################################################################################################

#with coloring=1 it reutuns also the original graph with colored stiner tree nodes
#g is a graph
steinertree1 <- function(labelcheck = TRUE, coloring = TRUE, ter_list = NULL, glist)
{
   name <- NULL
 	 color=c()	
	 graph= glist[[1]]
	 if (!is.null(graph))
	 {	
		if (!is.connected(graph) ){ print("Error : the graph is disconnected Steiner tree does not exist.") } 
		g= graph
		g=as.undirected(g)
	 }
	 if(!is.null(ter_list)) 
	  {
		 igraph::V(g)$color="yellow"
		 V(g)[ter_list]$color="red"
	  }
 #-----------------------------label checking in the begining and end to make sure the graph is correctly labeled and if not label them in here
	 if(labelcheck){
		labels=c(igraph::V(g)$label) #list of vertices
		names= c(igraph::V(g)$name)

		if(is.null(labels) && is.null(names))   {    #if graph has not labels make labels for it
		  	r0 =1:(length(V(g)))
		 	 igraph::V(g)$label=sapply (r0 ,function(r0) toString(r0) )
		 	 labels=  igraph::V(g)$label
		}

		if(is.null(labels) && !is.null(names)){
		  igraph::V(g)$label = igraph::V(g)$name
			labels=names
		}

		if(!is.null(labels) && is.null(names)){
		  igraph::V(g)$name =  igraph::V(g)$label
			names = labels
		}

		len = length(labels)
		r0 =1:(len)
		t = sapply (r0 ,function(r0) toString(labels[r0]) )
		temp = igraph::V(g)$name  #subgraph function was crashing if labels were alphabetic ,so here we keep a copy of them and work with the index of vertices instead
		igraph::V(g)$label=r0
		igraph::V(g)$name=r0
	 }
 #--------------------------------
 	 terminals = V(g)[color=="red"]
 
	 #now make a streiner tree from these chosen 
	 prob=sample(1:length(terminals),1,replace=FALSE)
 	 subtree=terminals[[prob]]
 	 edges=c()
 	 nsubtree= setdiff(terminals,subtree)
 	 queue=c()
	 #while not all terminals are not added to subtree
	 #optimize here
	 while( !all(is.element(terminals,intersect(subtree,terminals)) ) )
	 {

		 #find nearest from those terminals not in subgraph
		 #this is only to find a paths from one x to other nodes
		 #paths=get.all.shortest.paths(g,x,nsubtree ))
	   #paths = paths$res
		 #t=(sapply(paths,length))
		 #that makes a problem,the correct version of this part is in below: 

		 #paths=lapply(subtree,function(x) get.all.shortest.paths(g,x,nsubtree ))
	   #paths = paths$res
		 #this is not wrong,it is steiner tree with original heuristic :
		 paths=get.all.shortest.paths(g, subtree[length(subtree)],nsubtree )
		 paths = paths$res
		 if (length(paths) == 0 ){ print("Error : the graph is disconnected Steiner tree does not exist.") }
		 t=sapply(paths,length)
		 t2=which(t==min(t))
		 #add it to subtree
		 edges = union(edges ,paths[t2[1]] )    
		 t=length(unlist(paths[t2[1]]))
		 t3=unlist(paths[t2[1]])[t][[1]]
		 #cat("adding new path to steiner tree: ",t3,"\n")
		 subtree=union(subtree,	igraph::V(g)[name==t3])

		 #						print(subtree)
		 nsubtree= setdiff(nsubtree, igraph::V(g)[name==t3])
	 }
	 #for sake of clearness above subtree only includes terminals so:
	 subtree=union(subtree,V(g)[unique(unlist(edges)) ] )
	 steinert= minimum.spanning.tree(induced_subgraph(g,subtree))
	 #here we delete nonterminal vertices that has degree of 1
	 a=V(steinert)$color 	 
	 b=igraph::degree(steinert, v=V(steinert), mode = c("all")) #this is the way to call a function that is masked with another library(RBGL masks degree from igraph) 
	 a1=match(a,"yellow")
	 b1=match(b,"1")
	 opt= sapply(1:length(a1),function(r) (a1[r]*b1[r] ) )
	 new_g <- delete.vertices(steinert,grep(1,opt)-1)
	 steinert= new_g
	 #-----------------
	 if(coloring)
	 {
		 #this line on real test code should not be included or changed because the graph labels also will not be like here and should be corrected like the pattern in steiner 6  (also the graph will not be needed to treturned and the steiner is enough)
		 igraph::V(g)$color="yellow"
		 igraph::V(g)$color[subtree] ="green"
		 igraph::V(g)$color[terminals]="red"
	 }
	 #---------------------
	 #-----------------
	 #to recover the real label:
	 if(labelcheck){
		igraph::V(g)$label=temp
		igraph::V(g)$name= temp
		labellist=c()
		r0 =1:(length(V(steinert)))
		labellist =sapply (r0 ,function(r0) temp[ as.integer(V(steinert)$label[r0])] )
		V(steinert)$label=labellist
		V(steinert)$name=labellist
	 }

	 glst=c()    
	 #one of R problems is that if you sent a graph as output of a function it would miss
	 #meta infomration about labels so I put it inside a list and return the list instead
	 if(coloring)
	 {
		glst[[length(glst)+1]] <- g
	 }
	 glst[[length(glst)+1]] <- steinert
	 #-------------------
	 #it returnes input graph with colored vertices that stick out the steiner nodes inside the original tree
	 #and the second object inside the retuning list is the produced steiner tree in as a graph.
	 return (glst)
}


###################################################################################################################################
#	This is a code version of the steiner tree with shortest path heuristic that is starting from a terminal ,adding terminals 
#	with shortest distance from multiple #  "nodes" inside the subgraph and grows subgraph
#
#	Steiner tree heuristic with shortest path number 2
#	make a streiner tree from these chosen 
#	in this algorithm it is important from what terminal do we start
#      
#	Worst case run time O(nv~2) 	
#	error ratio 2- 2/n
#
#	Sample run:
#	stlst2 = steinertree2(g)
#	tkplot(stlst[[1]])
#	tkplot(stlst[[2]])
#
#
#	It returnes input graph with colored vertices that stick out the steiner nodes inside the original tree
#	and the second object inside the retuning list is the produced steiner tree in as a graph.
#	
#	If coloring is True it returns the original graph after coloring it with terminal and steiner nodes,but if 
#	coloring is Flase it returns just a new tree that is the steiner tree
#
########################################################################################

#g is a graph,just for the test we used common memory for g so to reduce runing time,it must be added again
steinertree2 <- function(labelcheck=TRUE , coloring=FALSE, ter_list= NULL, glist)
{
	 color=c()	
 	 graph= glist[[1]]
	 if (!is.null(graph)){
		if (!is.connected(graph) ){ print("Error : the graph is disconnected Steiner tree does not exist.") }
		g= graph
		g=as.undirected(g)
	  }
	 if(!is.null(ter_list)) 
	  {
		 igraph::V(g)$color="yellow"
		 V(g)[ter_list]$color="red"
	  }
	 #--------------
	 #subgraph function was crashing if labels were alphabetic ,so here we keep a copy of them 
	 #and work with the numerical index value of vertices instead
 	 #-----------------------------label checking in the begining and end to make sure the graph is correctly labeled and if not label them in here
 	 if(labelcheck){
 	   labels=c(igraph::V(g)$label) #list of vertices
 	   names= c(igraph::V(g)$name)
 	   
 	   if(is.null(labels) && is.null(names))   {    #if graph has not labels make labels for it
 	     r0 =1:(length(V(g)))
 	     igraph::V(g)$label=sapply (r0 ,function(r0) toString(r0) )
 	     labels=  igraph::V(g)$label
 	   }
 	   
 	   if(is.null(labels) && !is.null(names)){
 	     igraph::V(g)$label = igraph::V(g)$name
 	     labels=names
 	   }
 	   
 	   if(!is.null(labels) && is.null(names)){
 	     igraph::V(g)$name =  igraph::V(g)$label
 	     names=labels
 	   }
 	   
 	   len=length(labels)
 	   r0 =1:(len)
 	   t =sapply (r0 ,function(r0) toString(labels[r0]) )
 	   temp= igraph::V(g)$name  #subgraph function was crashing if labels were alphabetic ,so here we keep a copy of them and work with the index of vertices instead
 	   igraph::V(g)$label=r0
 	   igraph::V(g)$name=r0
 	 }
 	 #--------------------------------
	 terminals = V(g)[color=="red"]
	 prob=sample(1:length(terminals),1,replace=FALSE)
 	 subtree=terminals[[prob]]
	 edges=c()
	 nsubtree= setdiff(terminals,subtree)
	 #while not all terminals are not added to subtree
	 #optimze here
	 while( !all(is.element(terminals,intersect(subtree,terminals)) ) )
	  {
		  #find nearest from those terminals not in subgraph
		  paths=lapply(subtree,function(x) get.all.shortest.paths(g,x,nsubtree ))
		  #here find the "minimum" shortest path
		  r=1:length(paths)
		  #t is list of number all pathes from all nodes in subgraph
  		t=(sapply(r,function(r) sapply(paths[[r]]$res,length) ) )
		  #caution: length in list returns the lengh of the first cat but in array it returns number  of all enteries,
		  #so I use length for paths,but dim for t
		  #t2 list of is minimums of all pathes from any node in subgraph to all terminal outside of the subgraph (each node inside 
		  #has a minimum path in his list)
		  if(class(t)=="list" || class(t)== "integer"){
		 	r=1:length(t)
			t2=sapply(r,function(r) min(t[[r]]))
		  }
		  if(class(t)=="matrix") {
			r=1:dim(t)[2]
			#minimum of each column
			t2=sapply(r,function(r) min(t[,r]))
		 }

		 t3=which(t2==min(t2))
		 #which column(node) has the minimum of minimums
		 #t3 is index of minimums in the minimum paths list that is from all nodes in subgraph to all terminals outside of the subgraph
		 if(length(paths)>1)
		 {
		 	if(class(t)=="list" || class(t)== "integer" ){
		    		t4=which(t[[t3[1]]]==min(t[[t3[1]]]))   #to find all steiner tree should put variable instead of 1
		 	}
		 	if(class(t)=="matrix") {
		    		#use: t[t4,t3[1]] and paths$res[[t3[1]]][t4] instead
		    		t4=which((t[,t3[1]])==min(t[,t3[1]] ))
		 	}
	
		 	edges= union(edges ,paths[[t3[1]]][t4][1]$res )  #to find all steiner tree should put variable instead of 
								     #both 1s to get all the variants of the tree
		 	found=unlist(paths[[t3[1]]][t4][1]$res)  #to find all steiner tree should put variable instead of both 1s
		 }else{ #in case  of the first terminal paths have length 1 and we have one dimension less for paths
		 	edges= union(edges ,paths[[1]][t3][1]$res )
			found=unlist(paths[[1]][t3][1]$res)
		 }

		 #cat("smalest shortest path found:",found,"\n")
		 subtree=union(subtree,V(g)[unique(found)])
		 #cat("subtree until now:",subtree,"\n")
		 nsubtree= setdiff(nsubtree,V(g)[unique(found)])
	 }

	 steinert= minimum.spanning.tree(induced_subgraph(g,subtree))
 	 #here we delete nonterminal vertices that has degree of 1
 	 a=V(steinert)$color
	 b=igraph::degree(steinert, v=V(steinert), mode = c("all")) 
	 a1=match(a,"yellow")
	 b1=match(b,"1")
	 opt= sapply(1:length(a1),function(r) (a1[r]*b1[r] ) )
	 new_g <- delete.vertices(steinert,grep(1,opt)-1)
	 steinert= new_g
	#-----------------
	 if(coloring)
	 {
		igraph::V(g)$color="yellow"
		V(g)[subtree]$color="green"
		V(g)[terminals]$color="red"
	 }
	 #---------------------
	 #-----------------
	 #to recover the real label:
	 if(labelcheck){
		igraph::V(g)$label=temp
		igraph::V(g)$name= temp
		labellist=c()
		r0 =1:(length(V(steinert)))
		labellist =sapply (r0 ,function(r0) temp[ as.integer(V(steinert)$label[r0])] )
		V(steinert)$label=labellist
		V(steinert)$name=labellist
	 }
	 #-----------------
	 glst=c()    
	 #one of R problems is that if you sent a graph as output of a function it would miss
	 #meta infomration about labels so I put it inside a list and return the list instead
	 if(coloring)
	 {
		glst[[length(glst)+1]] <- g
	 }
	 glst[[length(glst)+1]] <- steinert
	 #-------------------
	 #it returnes input graph with colored vertices that stick out the steiner nodes inside the original tree
	 #and the second object inside the retuning list is the produced steiner tree in as a graph.
	 return (glst)
 }

##############################################################################################################################################################
#
#	This is a code version of the steiner tree with shortest path heuristic that is starting from multiple "tarminals" and grows terminal 
#	subgraphs separately and join the nearest ones until one of subgraph has all teminals.
#
#	Steiner tree heuristic with shortest path number 3
#	In the "Steiner Tree Problem "book it is entitled Kruskal-Based Heuristic (4.1.4)
#
#	Step 1:Begin with a forest Tkbh consisting of all isolated terminals
#	Step 2:If Tkbh is connected,then Stop.
#	Step 3:Find 2 trees in Tkbh that have the least distance.add the shortest path between these 2 trees.
#
#	Sample run
#	stlst3 = steinertree3(g)
#	tkplot(stlst[[1]])
#	tkplot(stlst[[2]])
#
#
#	The color of terminal nodes inside the graph are red 
#
#	It returns a list of 2 graph objects that consists of 2 graphs,the first is 
#	the entered graph but with colored nodes that stick the tree out 
#	on the graph(good for observing the tree insdie the graph and comparison),
#	and the second graph object is the result steiner tree an independent graph object.
#
#	If coloring is True it returns the original graph after coloring it with terminal and steiner nodes,but if 
#	coloring is Flase it returns just a new tree that is the steiner tree
#
################################################################################################################################################################

#g is a graph,just for the test we used common memory for g so to reduce runing time,it must be added again
steinertree3 <- function(labelcheck=TRUE,coloring=TRUE,ter_list=NULL,glist)
{
 	 color=subtreenum=c()	
	 graph=glist[[1]]
	 if (!is.null(graph)){
		if (!is.connected(graph) ){ print("Error : the graph is disconnected Steiner tree does not exist.") }
		g= graph
		g=as.undirected(g)
		}
	 makesubtrees = function(x)
		{
			if (!is.na(any(match(t3,x)))) {
				return( union(subtrees[[x]],found[[grep(1,match(t3,x))]][[1]])    )
			}
			else{
				return  (subtrees[[x]])	
			}
		}
	 x=c()
	# makensubtrees = function(x)
	#	{
	#		if (!is.na(any(match(t3,x)))) {
	#			return( setdiff(nsubtrees[[x]],found[[grep(1,match(t3,x))]][[1]])    )
	#		}
	#		else{
	#			return (nsubtrees[[x]])	
	#		}
	#	} 
	#####################################
          if(!is.null(ter_list)) 
	  {
		igraph::V(g)$color="yellow"
		V(g)[ter_list]$color="red"
	  }

	#subgraph function was crashing if labels were alphabetic ,so here we keep a copy of them and work with the numerical index value of vertices instead
	#-----------------------------label checking in the begining and end to make sure the graph is correctly labeled and if not label them in here
	 #-----------------------------label checking in the begining and end to make sure the graph is correctly labeled and if not label them in here
	 if(labelcheck){
	   labels=c(igraph::V(g)$label) #list of vertices
	   names= c(igraph::V(g)$name)
	   
	   if(is.null(labels) && is.null(names))   {    #if graph has not labels make labels for it
	     r0 =1:(length(V(g)))
	     igraph::V(g)$label=sapply (r0 ,function(r0) toString(r0) )
	     labels=  igraph::V(g)$label
	   }
	   
	   if(is.null(labels) && !is.null(names)){
	     igraph::V(g)$label = igraph::V(g)$name
	     labels=names
	   }
	   
	   if(!is.null(labels) && is.null(names)){
	     igraph::V(g)$name =  igraph::V(g)$label
	     names=labels
	   }
	   
	   len=length(labels)
	   r0 =1:(len)
	   t =sapply (r0 ,function(r0) toString(labels[r0]) )
	   temp= igraph::V(g)$name  #subgraph function was crashing if labels were alphabetic ,so here we keep a copy of them and work with the index of vertices instead
	   igraph::V(g)$label=r0
	   igraph::V(g)$name=r0
	 }
	 #--------------------------------
 	 terminals = V(g)[color=="red"]
	 #make a streiner tree from these chosen 
	 r=1:length(terminals)
	 subtrees = lapply(r,function(r) terminals[[r]])
	 terminals = subtrees
	 nsubtrees= lapply(r,function(r) setdiff(terminals,subtrees[r]))

	 #optimized  here : check if we have only one subtree instead of this big while condition here
	 #while not all terminals are not added to subtree
	 #while( !any( sapply(1:length(terminals) ,function(x) all( is.element(terminals,intersect(subtrees[[x]],unlist(terminals))))) )    )#while any of the subrees has not all of terminals	 
	 while(length(subtrees) >1)
	 {
		 #find nearest from those terminals not in subgraph
		 r=1:length(subtrees)
		 paths=lapply(r,function(r) lapply(subtrees[[r]],  function(x,y) get.all.shortest.paths(g,x,y )$res,y=nsubtrees[[r]] ))
		 #here find the "minimum" shortest path
		 r=1:length(paths)
		 #t is list of number all pathes from all nodes in subgraph
		 t=(sapply(r,function(r) sapply(paths[[r]][[1]],length) ) )

		 #caution: length in list returns the length of the first cat but in array it returns number  of all enteries,so I use length for paths,but dim for t
		 #t2 list of is minimums of all pathes from any node in subgraph to all terminal outside of the subgraph (each node inside has a minimum path in his list)
		 if(class(t)=="list" || class(t)=="integer"){
		 	r=1:length(t)
		 	t2=sapply(r,function(x) min(t[[x]]))
		 }
		 if(class(t)=="matrix") {
		 	r=1:dim(t)[2]
		 	#minimum of each column
		  	t2=sapply(r,function(r) min(t[,r]))
		 }

		 t3=which(t2==min(t2))
		 t3len=1:length(t3)
		 #which column(node) has the minimum of minimums
		 #t3 is index of subtrees that there exist a terminal that has the minimum distance
		 #t4 is the index of min distanc paths inside each subtree
		 if(length(paths)>1)
		 {
		 	if(class(t)=="list" || class(t)=="integer" ){
		 	   	t4= lapply(t3len,function(x) which(t[[t3[x]]]==min(t[[t3[x]]])) )  #to find all steiner tree should put variable instead of 1
		 	}
		 	if(class(t)=="matrix") {
		 	   	#use: t[t4,t3[1]] and paths[[t3[1]]][t4] instead
		 	   	t4=lapply(t3len,function(x) which((t[,t3[x]])==min(t[,t3[x]] )) )
		 	}

		 	#edges= union(edges ,paths[[t3[1]]][[t4]][1] )  #to find all steiner tree should put variable instead of both 1s to get all the variants of the tree
		 	#found=unlist(paths[[t3[1]]][[t4[[1]]]])  #to find all steiner tree should put variable instead of both 1s
			found= lapply(t3len,function(x) paths[t3[x]][[1]][[1]][t4[[x]][1]] )
		 }else{ #in case  of the first terminal paths have length 1 and we have one dimension less for paths
			#edges= union(edges ,paths[[1]][t3][1] )
			intersect(subtrees[[x]],V(g)[unlist(terminals)])#should correct it with proper samples
			cat("error /n")
			#found=unlist(paths[[1]][t3][1])
		 }

		#here those terminal with same minimum distance are added to subtrees ,but
		# in every subtree again they may exist several terminals with same minimum distance to that particular subtree,in this case we only add one of them
		#so i ignored this 2 lines from "found" calculation:
			#t4len= 1:length(t4)
			#t4len2=unlist( lapply(1:length(t4) ,function(x) length(t4[[x]]))  )
			 
		 #cat("smalest shortest path found:",found,"\n")
		 #we merge the terminals' subgraphs and their paths here
		subtrees= lapply(1:length(subtrees),function(x) makesubtrees(x) )
		#we delete  repeated subtrees here 	
		#we presume here  length(subtrees) is more than 1
        i=1
     		j=2
     		while (i  <= ((length(subtrees)-1) ))
     		{
     			j=i+1
	     			while(j  <= (length(subtrees) ) )
     					{
     					#cat(i,j,"inner loop", "\n")
     					if(length(intersect( subtrees[[i]] , subtrees[[j]] )) >0)
     						{
		     				#cat("union place found:",subtrees[[i]] ," ",  subtrees[[j]],"\n")
		     				subtrees[[i]] = union (subtrees[[i]] ,subtrees[[j]] )			     					
		     				subtrees = subtrees[-j] 
		     				j=j-1	
     						}		
	     		 		j=j+1
	     		 		}
			i=i+1	     	
	 	    }			 
		#cat("subtrees until now:",length(subtrees),"\n")
		#nsubtrees =lapply(1:length(terminals),function(x) makensubtrees(x) )
  		nsubtrees= lapply(1:length(subtrees),function(x) setdiff(terminals,subtrees[[x]]))
	 }
	 
	# subtreegroup = sapply(1:length(terminals) ,function(x) all( is.element(terminals,intersect(subtrees[[x]],unlist(terminals)))))
	 #subtreenum= grep(TRUE,subtreegroup)[1]
	 #steinert= minimum.spanning.tree(subgraph(g,subtrees[[subtreenum]]))
	 steinert= minimum.spanning.tree(induced_subgraph(g,subtrees[[1]]))
	 #here we delete nonterminal vertices that has degree of 1
	 a=V(steinert)$color
	b=igraph::degree(steinert, v=V(steinert), mode = c("all")) 
	 a1=match(a,"yellow")
	 b1=match(b,"1")
	 opt= sapply(1:length(a1),function(r) (a1[r]*b1[r] ) )
	 new_g <- delete.vertices(steinert,grep(1,opt)-1)
	 steinert= new_g	 
	#----------------coloring
	 if(coloring)
	 {
		igraph::V(g)$color="yellow"
		#V(g)[subtrees[[subtreenum]]]$color="green"
		V(g)[subtrees[[1]]]$color="green"
		V(g)[unlist(terminals)]$color="red"
	 }
	#-----------------to recover the real label:
	 if(labelcheck){
		igraph::V(g)$label=temp
		igraph::V(g)$name= temp
		labellist=c()
		r0 =1:(length(V(steinert)))
		labellist =sapply (r0 ,function(r0) temp[ as.integer(V(steinert)$label[r0])] )
		V(steinert)$label=labellist
		V(steinert)$name=labellist
	 }
	 #-----------------

	 glst=c()    
	 #one of R problems is that if you sent a graph as output of a function it would miss
	 #meta infomration about labels so I put it inside a list and return the list instead
	 #-----------------
	 if(coloring)
	 {
	 	glst[[length(glst)+1]] <- g
	 }	
	 	glst[[length(glst)+1]] <- steinert
	 #-------------------
	 #it returnes input graph with colored vertices that stick out the steiner nodes inside the original tree
	 #and the second object inside the retuning list is the produced steiner tree in as a graph.
	 return (glst)
}

#multipath path heuristic algorithm SPM
steinertree8 <- function(labelcheck= TRUE,coloring= TRUE,ter_list=NULL,ReturnAll = FALSE , glist)
{
 color=c()
 graph=glist[[1]]
 if (!is.null(graph))
 {	
	if (!is.connected(graph) ){ print("Error : the graph is disconnected Steiner tree does not exist.") } 
	g= graph
	g=as.undirected(g)
 }
 if(!is.null(ter_list)) 
  {
	igraph::V(g)$color="yellow"
	V(g)[ter_list]$color="red"
  }
 #-----------------------------label checking in the begining and end to make sure the graph is correctly labeled and if not label them in here
 if(labelcheck){
   labels=c(igraph::V(g)$label) #list of vertices
   names= c(igraph::V(g)$name)
   
   if(is.null(labels) && is.null(names))   {    #if graph has not labels make labels for it
     r0 =1:(length(V(g)))
     igraph::V(g)$label=sapply (r0 ,function(r0) toString(r0) )
     labels=  igraph::V(g)$label
   }
   
   if(is.null(labels) && !is.null(names)){
     igraph::V(g)$label = igraph::V(g)$name
     labels=names
   }
   
   if(!is.null(labels) && is.null(names)){
     igraph::V(g)$name =  igraph::V(g)$label
     names=labels
   }
   
   len=length(labels)
   r0 =1:(len)
   t =sapply (r0 ,function(r0) toString(labels[r0]) )
   temp= igraph::V(g)$name  #subgraph function was crashing if labels were alphabetic ,so here we keep a copy of them and work with the index of vertices instead
   igraph::V(g)$label=r0
   igraph::V(g)$name=r0
 }
 #--------------------------------
 terminals = V(g)[color=="red"]
 if (!ReturnAll ){
	 #now make a streiner tree from these chosen 
	terminal = c()
 	edges=c()
 	queue=c()
	#t_queue=c()
	results_queue=c()	
	edgeslist= c()
	prob=sample(1:length(terminals),1,replace=FALSE)
 	subtree=terminals[[prob]]
 	nsubtree= setdiff(terminals,subtree)
	startpoint= subtree
	paths=get.all.shortest.paths(g, subtree[length(subtree)],nsubtree )	
	paths = paths$res
	if (length(paths) == 0 ){ print("Error : the graph is disconnected Steiner tree does not exist.") }
	t=sapply(paths,length)
	t2=which(t ==min(t))
	for(i in 1:length(t2)){ queue[length(queue)+1]= paths[t2[i]]   }#push
	#for(i in 1:length(t2)){ t_queue
	index=length(t2)
	while(index>0 ){    #while queue is not empty
		edgeslist = queue[1]#pop
		for(i in 1:index){ queue[i]=  queue[i+1]  }#pop  use index,since the end of queue is not recognazable in R
		index = index -1#pop

		if(length( intersect(unlist(terminals),unlist(edgeslist))) ==  length(terminals) ){ #if passed all termials put in the result list else do the rest
			graph_is_new= TRUE
			if (length(results_queue) == 0){	 results_queue[length(results_queue)+1]= edgeslist    }	
			if (length(results_queue) > 0)
			{	
				for(count_path in 1:length(results_queue))
				{
				t1= unlist(edgeslist[[1]])
				t2= unlist(results_queue[[count_path]])
					  #all(results_queue[[count_path]] == edgeslist[[1]])   this gives different paths
					if (     length(union(t1,t2))  == length(t1)  )
 					          { 
 					         		 if (  all(union(t2,t1)== t2) ) { graph_is_new= FALSE  }
						#cat ("graph is not new", "\n")
						}
				}	
				if (graph_is_new == TRUE ) {
					 results_queue[length(results_queue)+1]= edgeslist
					 #cat ("graph is  new","\n")
					 }	
			}		 
		}else{
			subtree= intersect(unlist(terminals),unlist(edgeslist))
			nsubtree= setdiff(terminals,subtree)
			paths=get.all.shortest.paths(g, subtree[length(subtree)],nsubtree ) #here the second algorithm must look at all the terminals distances	
			paths = paths$res
			t=sapply(paths,length)
			t2=which(t ==min(t))
			for(i in 1:length(t2)){ queue[[index+i]]= union(unlist(edgeslist),unlist(paths[t2[i]]))  }#push
			#for(i in 1:length(t2)){ t_queue
			index=index + length(t2)
		}
	}
	#an optimasation here: again find the minimum through all variations of pathes that include the terminals:
	paths = results_queue
	t=sapply(paths,length)
	t2=which(t ==min(t))
	queue = paths[t2]
	 steinert= minimum.spanning.tree(induced_subgraph(g,queue[[1]]))
	 #here we delete nonterminal vertices that has degree of 1
	 a=V(steinert)$color
	 b=igraph::degree(steinert, v=V(steinert), mode = c("all")) 
	 a1=match(a,"yellow")
	 b1=match(b,"1")
	 opt= sapply(1:length(a1),function(r) (a1[r]*b1[r] ) )
	 new_g <- delete.vertices(steinert,grep(1,opt)-1)
	 steinert= new_g
	 #-----------------
	 if(coloring)
	 {
		 #this line on real test code should not be included or changed because the graph labels also will not be like here and should be corrected like the pattern in steiner 6  (also the graph will not be needed to treturned and the steiner is enough)
		 igraph::V(g)$color="yellow"
		 igraph::V(g)$color[subtree] ="green"
		 igraph::V(g)$color[terminals]="red"
	 }
	 #---------------------
	 #-----------------
	 #to recover the real label:
	 if(labelcheck){
		igraph::V(g)$label=temp
		igraph::V(g)$name= temp
		labellist=c()
		r0 =1:(length(V(steinert)))
		labellist =sapply (r0 ,function(r0) temp[ as.integer(V(steinert)$label[r0])] )
		V(steinert)$label=labellist
		V(steinert)$name=labellist
	 }

	 glst=c()    
	 #one of R problems is that if you sent a graph as output of a function it would miss
	 #meta infomration about labels so I put it inside a list and return the list instead
	 if(coloring)
	 {
		glst[[length(glst)+1]] <- g
	 }
	 glst[[length(glst)+1]] <- steinert
	 #-------------------
	 #it returnes input graph with colored vertices that stick out the steiner nodes inside the original tree
	 #and the second object inside the retuning list is the produced steiner tree in as a graph.
	 return (glst)
	}

 if (ReturnAll ){
 	#now make a streiner tree from these chosen 
 	terminal = c()
 	edges=c()
 	queue=c()
	#t_queue=c()
	results_queue=c()	
	edgeslist= c()
	prob=sample(1:length(terminals),1,replace=FALSE)
 	subtree=terminals[[prob]]
 	nsubtree= setdiff(terminals,subtree)
	startpoint= subtree
	paths=get.all.shortest.paths(g, subtree[length(subtree)],nsubtree )	
	paths = paths$res
	if (length(paths) == 0 ){ print("Error : the graph is disconnected Steiner tree does not exist.") }
	t=sapply(paths,length)
	t2=which(t ==min(t))
	for(i in 1:length(t2)){ queue[length(queue)+1]= paths[t2[i]]   }#push
	#for(i in 1:length(t2)){ t_queue
	index=length(t2)
	while(index>0 ){    #while queue is not empty
		edgeslist = queue[1]#pop
		for(i in 1:index){ queue[i]=  queue[i+1]  }#pop  use index,since the end of queue is not recognazable in R
		index = index -1#pop

		if(length( intersect(unlist(terminals),unlist(edgeslist))) ==  length(terminals) ){ #if passed all termials put in the result list else do the rest
			graph_is_new= TRUE
			if (length(results_queue) == 0){	 results_queue[length(results_queue)+1]= edgeslist    }	
			if (length(results_queue) > 0)
			{	
				for(count_path in 1:length(results_queue))
				{
				t1= unlist(edgeslist[[1]])
				t2= unlist(results_queue[[count_path]])
					if (  length(union(t2,t1)) == length(t1)  )  #all(results_queue[[count_path]] == edgeslist[[1]])   this gives different paths
 					        { 
							assign("last.warning", NULL, envir = baseenv())
						        if (  all(union(t2,t1)== t2) ) { graph_is_new= FALSE  }
							assign("last.warning", NULL, envir = baseenv()) 
							if (exists("last.warning", envir = baseenv())) 
						        
							last.warning <- get("last.warning", envir = baseenv())
							#print(last.warning)
							#cat ("graph is not new", "\n")
						}
				}	
				if (graph_is_new == TRUE ) {
					 results_queue[length(results_queue)+1]= edgeslist
					 #cat ("graph is  new","\n")
					 }	
			}		 
		}else{

			subtree= intersect(unlist(terminals),unlist(edgeslist))
			nsubtree= setdiff(terminals,subtree)
			paths=get.all.shortest.paths(g, subtree[length(subtree)],nsubtree ) #here the second algorithm must look at all the terminals distances	
			paths = paths$res
			t=sapply(paths,length)
			t2=which(t ==min(t))
			for(i in 1:length(t2)){ queue[[index+i]]= union(unlist(edgeslist),unlist(paths[t2[i]]))  }#push
			#for(i in 1:length(t2)){ t_queue
			index=index + length(t2)
		}
	}
	#an optimasation here: again find the minimum through all variations of pathes that include the terminals:
	paths = results_queue
	t=sapply(paths,length)
	t2=which(t ==min(t))
	queue = paths[t2]


	 #for sake of clearness above subtree only includes terminals so:
	 steinert_list = c()
	 for(i in 1:length(t2)){
	 steinert= minimum.spanning.tree(induced_subgraph(g,queue[[i]]))
	 
	 a=V(steinert)$color
	 b=igraph::degree(steinert, v=V(steinert), mode = c("all")) 
	 a1=match(a,"yellow")
	 b1=match(b,"1")
	 opt= sapply(1:length(a1),function(r) (a1[r]*b1[r] ) )
	 new_g <- delete.vertices(steinert,grep(1,opt)-1)
	 steinert= new_g
	 steinert_list[[length(steinert_list)+1]]=  steinert
	 }

	 #-----------------
	 if(coloring)
	 {
		 #this line on real test code should not be included or changed because the graph labels also will not be like here and should be corrected like the pattern in steiner 6  (also the graph will not be needed to treturned and the steiner is enough)
		 igraph::V(g)$color="yellow"
		 igraph::V(g)$color[subtree] ="green"
		 igraph::V(g)$color[terminals]="red"
	 }
	 #---------------------
	 #-----------------
	 #to recover the real label:
	 if(labelcheck){
		igraph::V(g)$label=temp
		igraph::V(g)$name= temp
		labellist=c()
		for(i in 1:length(t2)){
	 		steinert= steinert_list[[i]]
			r0 =1:(length(V(steinert)))
			labellist =sapply (r0 ,function(r0) temp[ as.integer(V(steinert)$label[r0])] )
			V(steinert)$label=labellist
			V(steinert)$name=labellist
			steinert_list[[i]]=  steinert
		}
	 }

	 glst=c()    
	 #one of R problems is that if you sent a graph as output of a function it would miss
	 #meta infomration about labels so I put it inside a list and return the list instead
	 if(coloring)
	 {
		glst[[length(glst)+1]] <- g
	 }
	 glst[[length(glst)+1]] <- steinert_list
	 #-------------------
	 #it returnes input graph with colored vertices that stick out the steiner nodes inside the original tree
	 #and the second object inside the retuning list is the produced steiner tree in as a graph.
	 return (glst)
	}
}


#######################################################################################################
#
#	This function takes a list of varient topologies of steiner trees using same terminal set on
#	a graph and returns the multiple steiner tree over them
#	Bonn Achen international center Bonn Germany
#	Afshin Sadeghi 
#	31 jan 2012 contact: sadeghi.afshin@gmail.com
#
######################################################################################################
Merge_Steiner = function(glist)
{
	g = glist[[1]]
	MStreeList = glist[[2]]

	merged= igraph.to.graphNEL(MStreeList[[1]]) 
	
	
	if(length(MStreeList) > 1){ 
	 	for (i  in 2:length(MStreeList) ){
		 merged= graph::join(merged,igraph.to.graphNEL(MStreeList[[i]]))	
	 	}		
	}
	return(igraph.from.graphNEL(merged))
}

###########################################################################################################
#	This method is my first exact method that i made and mailed the sudo to holger ,but first time it is mentiond by yelvin[36] 1971 and later enhaced by Lawer[34] 1976
#
#	This function returns all of the exact solutions if ReturnAll is 1
#
#	update on introducing an upper limit for number of vertices to include ,6,oct 2011 .Afshin
#	removing with the error of lables,the graphs that had already label,change the terminal from id list to label ,30,jan 2012 .Afshin
#	exact solution is updated with a new lower limit,it would start with number of terminals,and grow the number tested vertices ,and stops growing if it finds a minimum steiner tree
#
#	sample run
#	g <- graph.ring(10)
#	ter_list= c(1,2)
#	stexlst = steinerexact(TRUE,FALSE,ter_list,FALSE,g)
#	tkplot(stexlst[[1]])
#	
#	all parameter says to return ReturnAll the solutions,if 1 returns all exact solutions,else returns only the first solution,
#	last updated in 19 oct 2011 ,defining the lower bound for the exact solution.
#	info is to tell to print the size of result steiner tree,the list of result trees etc 
#
###########################################################################################################

steinerexact <- function(labelcheck = FALSE , coloring=FALSE, ter_list= NULL, ReturnAll = FALSE, glist)
{
	graph=glist[[1]]
	printinfo= FALSE
	if (coloring){printinfo=TRUE}
	if (!is.connected(graph) ){ print("Error : the graph is disconnected Steiner tree does not exist.") }
	g=graph
	if (!is.null(igraph::V(g)$name) ) { t=igraph::V(g)$name}

	if (class(ter_list)== "numeric" ){ter_list= as.character(ter_list)}
	
	if(!is.null(ter_list)) 
	{
	  igraph::V(g)$color="yellow"
	  V(g)[ter_list]$color="red"
	}
	#-----------------------------label checking in the begining and end to make sure the graph is correctly labeled and if not label them in here
	if(labelcheck){
	  labels=c(igraph::V(g)$label) #list of vertices
	  names= c(igraph::V(g)$name)
	  
	  if(is.null(labels) && is.null(names))   {    #if graph has not labels make labels for it
	    r0 =1:(length(igraph::V(g)))
	    igraph::V(g)$label=sapply (r0 ,function(r0) toString(r0) )
	    labels=  igraph::V(g)$label
	  }
	  
	  if(is.null(labels) && !is.null(names)){
	    igraph::V(g)$label = igraph::V(g)$name
	    labels=names
	  }
	  
	  if(!is.null(labels) && is.null(names)){
	    igraph::V(g)$name =  igraph::V(g)$label
	    names=labels
	  }
	  temp= igraph::V(g)$name  #subgraph function was crashing if labels were alphabetic ,so here we keep a copy of them and work with the index of vertices instead
	  r0 =1:(length(igraph::V(g)))
	  igraph::V(g)$label=r0
	  igraph::V(g)$name=r0
	  len=length(labels)
	  r0 =1:(len)
	  t =sapply (r0 ,function(r0) toString(r0) )
	}
	#--------------------------------
	terminals= igraph::V(g)$name[igraph::V(g)$color=="red"]
	len=length(V(g))
	#introducing an lower limit and higher limit of the exact solution
	lim=length(V(g))-length(terminals)
	#higher limit = number of all vertices 
	#lower limit = number of terminals
	# if we know all nodes are not terminals lim is 1(according the paper).but we can compare numbers of them.
	#update.run the code with smaller set of steiner nodes,if possible,if now grow until enough steiner nodes are in hand
	#---------------------------------------------------------------------------------------------------------------------
	#while(!is.element(TRUE,unlist(smst)) && lim>0){
	#use sapply instead of while:
	en=new.env(hash = TRUE, parent = parent.frame(), size = NA)
	assign("runloop",TRUE, envir = en)
	#runloop <<- TRUE
	assign("sol_place",0, envir = en)
	#sol_place =0
	smst=c()	
	rwhile =function(lim,len)
	{
	#lim=lim-1 
		if(get("runloop",envir=en))
		{
			r =(len- lim) 
			allcom = combn(t[1:len],r)
			allmst= lapply(1:dim(allcom)[2], function(x)  minimum.spanning.tree(induced_subgraph(g,allcom[,x] ) ))
			assign("allmst",allmst, envir = en)
			#subgraph return answeres with changed ides, but keeps the labels
			edgmst= lapply (1:dim(allcom)[2], function(x)  get.edgelist(allmst[[x]], names=TRUE)) 
			assign("edgmst",edgmst, envir = en)
			#can put this 3 lines below in one line
			#list of connected trees
			connectedlist = lapply(1:dim(allcom)[2], function(x) is.connected(allmst[[x]])) 
			#list of trees that have terminals inside
			withterminals = lapply(1:dim(allcom)[2], function(x)  all(is.element(terminals,V(allmst[[x]])$label))) 
			#trees having both condistions above
			smst= lapply(1:dim(allcom)[2], function(x)  connectedlist[[x]] && withterminals[[x]])  
			#cat(r,lim,len,length(smst),length(allmst),"\n",sep=" ")
			assign("runloop",!is.element(TRUE,unlist(smst)), envir = en)
			assign("sol_place",get("sol_place",envir=en)+1, envir = en)
			#sol_place  <<- sol_place  +1 	
			#runloop<<- !is.element(TRUE,unlist(smst))
		}
	 #cat("runloop",runloop,"\n")
	 return (smst)
	}
	res=lim:1
	sol<-sapply(res, function(x) rwhile(x,len) )
 	sol_place=get("sol_place",envir=en)
	allmst =get("allmst",envir=en)
	edgmst =get("edgmst",envir=en)
	#--------------------------------------------------------------------------------------------------
	#size of trees
	iter=length(sol[[sol_place]])
	size= lapply (1: iter, function(x)  length(edgmst[[x]]) /2)
	#insol=length(sol)
	midresult= lapply(1:length(sol[[sol_place]]), function(x)  {size[[x]] * as.integer(sol[[sol_place ]][[x]]) })  
         # midresult[midresult>0]
         #is.element(TRUE,unlist(sol[[sol_place ]]))

	mmin=length(t)
	#find the minimum spanning tree
	position.a=0
	mymin = function(a)
	{
	if(mmin >= midresult[[a]] &&  midresult[[a]]!=0 ){
	#print("found")	
	 mmin <<- midresult[[a]]
	 position.a <<- a
	 }
	return(mmin)
	}

	result= lapply(1:length(sol[[sol_place]]), function(x) mymin(x))
	stgraphlist=c()

	if(position.a !=0)
	 {
			steinert=allmst[[position.a]]
			if(printinfo)
			{
			cat("min spanning tree size is: ", mmin ,"\n")
			# show the first result:
			#cat("min spanning tree is: \n")
			print(steinert)
			}
	 			#-----------------
			 if(coloring)
			 {
				 #this line on real test code should not be included or changed because the graph labels also will not be like here and should be corrected like the pattern in steiner 6  (also the graph will not be needed to treturned and the steiner is enough)
				 igraph::V(g)$color="yellow"
				 V(g)[V(steinert)$label]$color ="green"
				 V(g)[terminals]$color="red"
			 }
			 #----------------------label check
			 if(labelcheck){
					 #to recover the real label:
					 igraph::V(g)$label=temp
					 igraph::V(g)$name =temp
					 labellist=c()
					 for (num in 1:length(V(steinert)$label))
					 {
					   labellist[num]=temp[ as.integer(V(steinert)$label[num])]
					 }
					 V(steinert)$label=labellist
					 V(steinert)$name=labellist
					 }
					if(coloring && !ReturnAll)
					 {
						stgraphlist[[length(stgraphlist)+1]] <-g

					 }
				stgraphlist[[length(stgraphlist)+1]] <-steinert

	 }

	if(ReturnAll )
	{
	#to find all minimum steiner trees
	 poslist.a=c()
	 
	 allmin = function(a)
	 {
	 if(mmin == midresult[[a]]){
	  poslist.a[length(poslist.a)+1] <<- a
	  }
	 return(mmin)
	 }

	 results= lapply(1:length(sol[[sol_place]]), function(x) allmin(x))  

	 if (!is.null(poslist.a ))
	 {

		r2=1:length(poslist.a)
		stgraphlist= lapply(r2, function(r2) allmst[[poslist.a[r2]]] )
	        #------------------------------doing label check		
		if(labelcheck ){
			#to recover the real labels for the graphs in the list:
			for(numst in 1:length(stgraphlist) )
			{
				labellist=c()
				for (num in 1:length(V(stgraphlist[[numst]])$label))
				{
					labellist[num]=temp[ as.integer(V(stgraphlist[[numst]])$label[num])]
				
				}
				V(stgraphlist[[numst]])$label=labellist
				V(stgraphlist[[numst]])$name=labellist
			}
	    }  }
	        #---------------------------doing label check
		#show the result in:
	 stgraphlist2 = c()
	 if(coloring && ReturnAll)
	 {
	   stgraphlist2[[length(stgraphlist2)+1]] <-g
	   stgraphlist2[[length(stgraphlist)+1]] <-stgraphlist
	   stgraphlist = stgraphlist2
	 }
		if(printinfo){	
		cat("list of all min spanning trees is: \n")
		print( stgraphlist )
		}
	}
	return (stgraphlist)
}

####################################################################################
#   A set of 6 steiner tree algorithms 
#
# 	"graph" is the input graph
# 	
#   "Type" defines the steiner algorithm type to use:
#	        SP is the Shortest Path heuristic.
#	        KRU is Kruskal-Based Heuristic  algorithm
#	        RSP is a Random Approximation algorithm
#	        EXA uses the exact algorithm 
#	        SPM returns and term from a component set of enumerated steiner trees for the graph using the heuristic algorithm
#
#	  Label check checks if lables exist on the graph. The function will not work if the graph is not labled,
#   so if it is not labeled by making labelcheck= TRUE it will be able to work with them too
#
# 	If "coloring" be true, it a returns a list. The fist second item is a resulted steiner tree, and the first
#       item in the list is a copy of the input graph 
#       that shows the stiener tree on that by coloring. 
#       Yellow nodes designate the steiner nodes and red nodes show the terminals.
#
# 	If "enumerate"  is selected to be TRUE it will force the algorithm to return multiple steiner tree algorithm solutions.It is available for Ex and SPM
#
#	  "ter_list" is the list of terminals for the steiner tree
#
#  Example:
# library(igraph)
# library(tcltk2)

# el <- matrix( c("a", "b", "a", "c", "b", "d","d","e", "c", "b" ), nc = 2, byrow = TRUE)
# g1 =graph_from_edgelist(el)
# ter_list= c("a","b","e")

# SP=steinertree("SP", ter_list, g1, TRUE, FALSE)
# tkplot(SP[[1]])

# SPM=steinertree("SPM", ter_list, g1 ,TRUE, TRUE)
# tkplot(SPM[[2]])

# SPM2 = steinertree("SPM", ter_list, g1 , TRUE, FALSE)
# tkplot(SPM2[[1]])


# RSP = steinertree("RSP", ter_list, g1 , FALSE, FALSE)
# tkplot(RSP[[1]])

# RSP2 = steinertree("RSP", ter_list, g1 , FALSE, TRUE)
# tkplot(RSP2[[2]])

# KB = steinertree("KB", ter_list, g1 , TRUE)
# tkplot(KB[[1]])
# tkplot(KB[[2]])

# EXA = steinertree("EXA", ter_list, g1 , TRUE , FALSE)
# tkplot(EXA[[1]])

# EXA2 = steinertree("EXA", ter_list, g1 , TRUE , TRUE)
# tkplot(EXA2[[2]])

# EXA3 = steinertree("EXA", ter_list, g1 , FALSE , FALSE)
# tkplot(EXA3[[1]])

steinertree <- function( type, ter_list = NULL, graph, enumerate= FALSE  ,coloring= TRUE)
{	
	color=c()
	if(is.null(graph)){
		print("Error, the input graph object is Null.")
		return()		
	}
	if(!is.connected(graph)){
		print("Error, the input graph is not connected.")
		return()		
	}

	if( length(V(graph))== 0 ){
		print("Error, the input graph has no vertices.")
		return()		
	}

	if( is.null(V(graph)) ){
		print("Error, the input graph vertices are NULL.")
		return()		
	}

	if( length(V(graph)$name)==0 ){
		print("Warning, the input graph vertices have no name and no label.")
	}

	if (is.null(ter_list) || is.na(ter_list) || ter_list == FALSE || length(ter_list)==0){
			ter_list = V(graph)[color=="red"]
		}
	if  (is.null(ter_list) || is.na(ter_list) || length(ter_list)==0){
		print("Error,no terminal list is defined.")
		return()
	}

	if  (class(ter_list) == "numeric" || class(ter_list) == "integer" ){
		print("Error, terminal list is must be a character type list.")
		return()
	}

	V(graph)$color="yellow"
	V(graph)[ter_list]$color="red"
	labelcheck= FALSE
	if (is.null(c(V(graph)$name)) || is.null(c( V(graph)$label))){  
		 ter_list = as.character(ter_list)
		 labelcheck= TRUE
	}

	correct_type = FALSE	
	if( type == "SPM" || type == "EXA" || type == "SP" || type == "RSP" || type == "KB" ){
	correct_type= TRUE
	}
	if(!correct_type)
	{
		print("Error, the input type is not correct; Choose one from SPM,EXA,SP,RSP,KB.")
		return()	
	}
	glist=c()
	glist[[1]]=graph

	if (type == "SP"){
	 	result= steinertree2(labelcheck, coloring, ter_list, glist)
	}

	if (type == "KB"){
		result=steinertree3(labelcheck, coloring, ter_list, glist)
	}

	if (type == "RSP"){
		result = appr_steiner(70, labelcheck, coloring, ter_list, glist) 
	}

	if (type == "EXA"){
	    labelcheck= TRUE
			result=steinerexact(labelcheck, coloring, ter_list, enumerate, glist)
		 	if (enumerate){
		 	    if(coloring){
		 	    print(length(result))
		 	    glist[[1]]=result[[1]]
		 	    glist[[2]]=result[[2]]
		 	    result[[2]] = Merge_Steiner(glist) 
		 	    }else{
			   	  glist[[2]]=result
		 		    result[[1]] = Merge_Steiner(glist)
		 	    }	
		 	}
	}
	if (type == "SPM"){
			result= steinertree8(labelcheck, coloring, ter_list, enumerate, glist)
			 	if (enumerate){
			 	    if(coloring){
			 	      glist[[1]]=result[[1]]
			 	      glist[[2]]=result[[2]]
			 	      result[[2]] = Merge_Steiner(glist) 
			 	    }else{
			 	      glist[[2]]=result[[1]]
			 	      result[[1]] = Merge_Steiner(glist)
		 	    }
	 	 } 	 
	}
assign("last.warning", NULL, envir = baseenv())
return(result)
}
