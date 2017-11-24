 #-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  This is the main module that calls random walk and generates sample,runs different steiner tree algorithms on the simulated data,
#  and in the end the steinertttestplot should be called to plot the results.
#
# 		3 functions : gensample() generates sample data ,
#          	 review() shows a review of generated data ,
#		runtest() run steiner tree tests on samples,
# 		the exact solusion is not recommanded to test when the number of vertices are more than 23(takes about 50 gigabyes of memory) 
#		The number of experiments in form of  probability of choosing nodes in random walk and number of terminals, as in our experiments:
#  		 listofterminaltest=c(5,8,15,50,70) 
#		 repetition=c(0.5,0.5,0.5,0.5,0.5  ,0.5,0.5,0.5,0.5,0.5 ,0.5,0.5,0.5,0.5,0.5 ,0.5,0.5,0.5,0.5,0.5 ,0.5,0.5,0.5,0.5,0.5 ,0.5,0.5,0.5,0.5,0.5 ,0.5,0.5,0.5,0.5,0.5 ,0.5,0.5,0.5,0.5,0.5 ,0.5,0.5,0.5,0.5,0.5 ,0.5,0.5,0.5,0.5,0.5) # i with 10*5=50 elements
##--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#This function generates a random graph using a base graph and random walk algorithm
gRandWalk <- function(graph,numofterminals,terminalpr,makenewgraph=FALSE)
{
	 terlist=c()
	 nodlist=c()
	 g=as.undirected(graph)
	 g2=g
	 seed=sample(1:length(V(g))-1,1)	

	 terminalpr = terminalpr * 10
	 i=1
	 lastseed = -1
	 while(i < numofterminals)
	 {
		#this part is for when the graph is directed	
		if ( length(neighbors(g,seed))==0 ) #&& unlist(neighborhood(g, 1, nodes=seed, mode="in"))==lastseed  )
		{
		 r=1: length(nodlist)
		 allneib =unique(unlist(lapply (r ,function(r) neighborhood(g, 1, nodes=seed, mode="all")  )))	
		 seed=allneib[sample(1:length(allneib),1)]
		 cat("in deadend, backtracking,seed:",seed,"\n") 	
		}
	 	lastseed=seed
		#here choose one of neghbors with a uniform probability 
		seed=sample(unlist(neighborhood(g, 1, nodes=seed, mode="all")),1)
		
		cat(i ,"th node has label ",seed ,".","\r" ,sep="")
		#append the object to list of desired vertices
	
		nodlist[[length(nodlist)+1]] <- seed
		nodlist = unique((unlist(nodlist)))

		prob=sample(1:10,1,replace=FALSE)
		if (prob < terminalpr ) {
		terlist[[length(terlist)+1]] <- V(g)[seed]$name
		terlist = unique((unlist(terlist)))
		cat("terminallist",terlist," node number :",i ,"seed:",seed ,", and selected to be terminal. \n" )
		cat ("terlist,",terlist ,"\n")
		i = length(terlist)
		}
	 }
	g=g2
	terminals=terlist
	V(g)$color="yellow"
	V(g)[terminals]$color="red"


	 if(makenewgraph){

		rangr=subgraph(g2,nodlist)
		g=rangr
		terminals=terlist
		V(g)$color="yellow"
		V(g)[terminals]$color="red"

		labels=c(V(g)$label) #list of vertices
		names= c(V(g)$name)

		if(is.null(labels) && !is.null(names)){
			g[[9]][[3]]$label = g[[9]][[3]]$name
			labels=names
		}

		if(!is.null(labels) && is.null(names)){
			g[[9]][[3]]$name = g[[9]][[3]]$label
			names=labels
		}

		if(is.null(labels) && is.null(names))   {    #if graph has not labels make labels for it
			  r0 =0:(length(V(g))-1)
			  g[[9]][[3]]$label==sapply (r0 ,function(r0) toString(r0) )
			  labels= g[[9]][[3]]$label
		}

	 }

	 glst=c()    
	 glst[[length(glst)+1]] <- g
	 glst[[length(glst)+1]] <- terminals
	 return (glst)
}

#this function gerenerates the random graph as much as needed
# repetition is list of probabilites to choose a node to be in the result in each step of random walk,the length of this list 
#listofteminaltest is  a list of number of terminal to use in each set of experiments, each set of experiment is repeated by number of probabilites,therefore number of all the experiments is number of probabilites multiplied by number of listofterminaltest
gensample= function(destination, makesubgraphs = FALSE, graph, listofterminaltest, repetition)
{
  color= c()
  grw =  c()
  g=as.undirected(graph)
  g[[9]][[3]]$label = g[[9]][[3]]$name
  k =length(listofterminaltest)
  
      for(j in 1:k)
  	{
	 	for(i in 1:length(repetition) ){
	   		grw= gRandWalk(g,listofterminaltest[j], repetition[i],makesubgraphs)
	   		g2=grw[[1]]
			terlist =grw[[2]]
		   	while(makesubgraphs && length(V(g2))> 24 && j < 4){   #to make sure graphs in exact algorithm test are small enough
			grw= gRandWalk(g,listofterminaltest[j], repetition[i],makesubgraphs)
	   		g2=grw[[1]]
			terlist =grw[[2]]
			}	
			terminals=  V(g2)[color=="red"]
	   		save(grw, file = paste(destination,"/g",j,"of",i,".RData",sep=""))
	 	}  
 	 }

}
#-----------------------------------------------------------------------------------------------------------
#	Generates and stores test result of steiner tree algorithms on the random walk sample data
#
#       takes the address of saved graphs folder to run the experiment on them
#-----------------------------------------------------------------------------------------------------------
# Enumeration STEINER TREE  COMPARISONS WITH THE ALL THE SHORTEST PATHS
mulSteiner = function(folder,experiments = NULL,listofterminaltest,repetition)
{
 counter = experiments
 color=c()
 grw=c()
 SPM=Ex=t2=c()
 if (is.null(experiments) ){ counter = 1:length(listofterminaltest) }
  for(j in counter )
  {
 	for(i in 1:length(repetition) )
 	{		 
		 cat(i,j,"\n")
		 load(paste(folder,"/g",j,"of",i,".RData",sep="")) 
		 #for the case of speed g is not sent to the functions,but terminals are sent
		 g<-grw[[1]]
		 #the exact solution takes so much time for nodes bigger than 15
 		 cat("runing of the exact for graph with |v|=",length(V(g))," \n")
		 a=Sys.time()
		 	 
		 EXA = steinertree("EXA",NULL,g,TRUE, FALSE)
		 b=Sys.time()
		 t1=b-a
 		 cat("runtime of multiple EXA unification" ,i,j,"is", t1, units(t1),"\n")
		 
		 cat("runing of test SPM (multi steiner tree) \n")
		 a= Sys.time()
		 SPM = steinertree("SPM",NULL,g,TRUE, FALSE)
		 b= Sys.time()
		 t2 =b-a
 		 cat("runtime of multiple SPM unification" ,i,j,"is", t2,units(t2),"\n")
		#creat a subgraph ofall the shortest paths to compare with with size of it
		g1 <- graph.empty()
		subg=g
		subg[[3]]=g1[[3]]
		subg[[4]]=g1[[4]]
		cat("runing of all paths soluton \n")
		a=Sys.time()
		terminals = V(subg)[color=="red"]
		paths=lapply(terminals,function(x) (get.all.shortest.paths(g,x,terminals )) )
		m=0
		nodes=c()	 
		for (ii in 1:length(paths))
				 {
					 for (jj in 1:length(paths[[ii]]))
						{
						nodes=union(nodes,( unlist(paths[[ii]][[jj]])) )
						len=length(as.character(unlist( paste( unlist(paths[[ii]][[jj]])) )  ,sep="") )
						if (len > 1)	
						   	 {
						   	 	 count=len-1
						    		for(kkk in 1:count) {subg=add.edges(subg,c(paths[[ii]][[jj]][kkk], paths[[ii]][[jj]][kkk+1]))}#constructing the subgraph by adding edges found in the shotest path to a graph that has the nodes
							}	
						}
				 }
		 subg=simplify(subg, remove.multiple = TRUE, remove.loops = FALSE)
		 nodes=unique(nodes)	 
		 ver=nodes
		  m=length(E(subg))	 	 
 	         #m is the number of uniq edges all shortest paths
		 b=Sys.time()
		 t3 =b-a
 		 cat("runtime of all paths calcualtions" ,i,j,"is", t3,units(t3),"\n")
		 runtimes=c(t1,units(t1),t2,units(t2),t3,units(t3))
		 cat("saving result data into time and multi steinter tree files... \n")		
		 try({save(EXA, file = paste(folder,"/MEXA",j,"of",i,".RData",sep=""))}, silent=FALSE)
 		 try({save(SPM, file = paste(folder,"/MSPM",j,"of",i,".RData",sep=""))}, silent=FALSE)
		 try({save(subg,m,ver, file = paste(folder,"/ALPH",j,"of",i,".RData",sep=""))}, silent=FALSE)
		 save(runtimes, file = paste(folder,"/runtimesM",j,"of",i,".RData",sep=""))
	}
  }	
#end of run test for multi steiner tree algorithm
}



#COMPARISON OF  APPROXIMATE METHODS WITH EACH OTHER
appr_tests= function(folder , experiments = NULL, listofterminaltest, repetition)
{ 
 color=c()
 grw = c()
 ST1=ST2=ST3=ST4=c()
 counter = experiments
 if (is.null(experiments) ){ counter = 1:length(listofterminaltest) }
 for(j in counter)
  {
 	for(i in 1:length(repetition) )
 	{
 		 load(paste(folder,"/g",j,"of",i,".RData",sep="")) 
		 #for the case of speed g is not sent to the functions,but terminals are sent
		 g<-grw[[1]]
		 ter_list = V(g)[color=="red"]
		 #the exact solution takes so much time for nodes bigger than 15
		 t1=0
		 cat("runing of test SP.. \n") 
		 a=Sys.time()
		 ST2=steinertree("SP",ter_list,g,FALSE,FALSE)
		 b=Sys.time()
		 t3=b-a
		 cat("runtime of test SP" ,i,j,"is", t3,units(t3),"\n")
		 #cat("runing of test ST3 \n")	 
		 #a=Sys.time()
		 #ST3=steinertree("KRU",ter_list,g,FALSE,FALSE)
		 #b=Sys.time()
		 #t4=b-a
		 #cat("runtime of test ST3" ,i,j,"is", t4,units(t4),"\n") 

		 cat("runing of test ST4 appr steiner \n")
		 a=Sys.time()
		 ST4=steinertree("RSP",ter_list,g,FALSE,FALSE) 
		 b=Sys.time()
		 t5=b-a
		 cat("runtime of test ST4" ,i,j,"is", t5,units(t5),"\n") 
		
		 #runtimes=c(t3,t4,t5,units(t3),units(t4),units(t5))
		 runtimes=c(t3,t5,units(t3),units(t5))
		 cat("saving result data into time and steinter tree files... \n")
	
		 try({save(ST2, file = paste(folder,"/ST2",j,"of",i,".RData",sep=""))}, silent=FALSE)
		# try({save(ST3, file = paste(folder,"/ST3",j,"of",i,".RData",sep=""))}, silent=FALSE)
		 try({save(ST4, file = paste(folder,"/ST4",j,"of",i,".RData",sep=""))}, silent=FALSE)
		 save(runtimes, file = paste(folder,"/runtimes",j,"of",i,".RData",sep=""))
	}
  }	
#end of run test
}

#KB in COMPARISON OF  APPROXIMATE METHODS WITH EACH OTHER
appr_KB_tests= function(folder , experiments = NULL, listofterminaltest, repetition)
{ 
 color=c()
 grw = c()
 ST1=ST2=ST3=ST4=c()
 counter = experiments
 if (is.null(experiments) ){ counter = 1:length(listofterminaltest) }
 for(j in counter)
  {
 	for(i in 1:length(repetition) )
 	{
 		 load(paste(folder,"/g",j,"of",i,".RData",sep="")) 
		 #for the case of speed g is not sent to the functions,but terminals are sent
		 g<-grw[[1]]
		 ter_list = V(g)[color=="red"]
		 cat("runing of test KB  j,i :",j,i," \n")	 
		 a=Sys.time()
		 ST3=steinertree("KB",ter_list,g,FALSE,FALSE)
		 b=Sys.time()
		 t4=b-a
		 cat("runtime of test KB" ,j,i,"is", t4,units(t4),"\n") 
 		 load(paste(folder,"/g",j,"of",i,".RData",sep="")) 
		 runtimes=c(t4,units(t4))
		 cat("saving result data into time and steinter tree files... \n")
		 try({save(ST3, file = paste(folder,"/STKB",j,"of",i,".RData",sep=""))}, silent=FALSE)
		 save(runtimes, file = paste(folder,"/runtimesKB",j,"of",i,".RData",sep=""))
	}
  }	
#end of run test
}

asp_in_appr_tests= function(folder , experiments = NULL, listofterminaltest, repetition)
{ 

 color=c()
 grw = c()
 ST1=ST2=ST3=ST4=c()
 counter = experiments
 if (is.null(experiments) ){ counter = 1:length(listofterminaltest) }
 for(j in counter)#
  { # j=5
 	for(i in 1:length(repetition) ) #in 43...
 	{
 		 load(paste(folder,"/g",j,"of",i,".RData",sep="")) 
		 #for the case of speed g is not sent to the functions,but terminals are sent
		 g<-grw[[1]]
		 ter_list = V(g)[color=="red"]
		 t3=0
		g1 <- graph.empty()
		subg=g
		subg[[3]]=g1[[3]]
		subg[[4]]=g1[[4]]
		cat("runing of all paths soluton",i,j," \n")
		a=Sys.time()
		terminals = V(subg)[color=="red"]
		paths=lapply(terminals,function(x) (get.all.shortest.paths(g,x,terminals )) )
		m=0
		nodes=c()
		nodes=unique(unlist(paths))
		paths=unlist(paths, recursive=FALSE)					
		for (ii in 1:length(paths))
				 {
					# for (jj in 1:length(paths[[ii]]))
						#{
				#improvement:instead of adding edge by edge now we add path by path
				subg=add.edges(subg,as.vector(aperm(get.edges(g,E(g, path= paths[[ii]])))))						
						#nodes=union(nodes,( unlist(paths[[ii]][[jj]])) )
						#len=length(as.character(unlist( paste( unlist(paths[[ii]][[jj]])) )  ,sep="") )
						#if (len > 1)	
						#   	 {
						#   	 	 count=len-1
						#    		for(kkk in 1:count) {subg=add.edges(subg,c(paths[[ii]][[jj]][kkk], paths[[ii]][[jj]][kkk+1]))}#constructing the subgraph by adding edges found in the shotest path to a graph that has the nodes
						#	}	
					#	}
				 }
		 subg=subgraph(subg,nodes)
		 subg=simplify(subg, remove.multiple = TRUE, remove.loops = FALSE)
		 #nodes=unique(nodes)	 
		 #ver=nodes
		 ver=V(subg)
		 m=length(E(subg))	 	 
 	         #m is the number of uniq edges all shortest paths
		 b=Sys.time()
		 t3 =b-a
		 runtimes=c(t3,units(t3))
		 try({save(subg,m,ver, file = paste(folder,"/ALPH",j,"of",i,".RData",sep=""))}, silent=FALSE)
		 save(runtimes, file = paste(folder,"/runtimesAL",j,"of",i,".RData",sep=""))
	}
  }  #	
#end of run test

}

#comparison steiner tree methods with the optimum algorithm
exact_tests= function(folder, experiments= NULL, listofterminaltest, repetition )
{
  color=c()
  grw=c()
  ST1=ST2=ST3=ST4=SPM=EXA=c()
  t1=t2=t3=t4=t5=t6=0
  counter = experiments
  if (is.null(experiments) ){ counter = 1:length(listofterminaltest) }
  for(j in counter)
  {
 	for(i in 1:length(repetition) )
 	{
 		load(paste(folder,"/g",j,"of",i,".RData",sep="")) 
		#for the case of speed g is not sent to the functions,but terminals are sent
		g<-grw[[1]]
		ter_list = V(g)[color=="red"]
		#cat(length(V(g))," \n")
		#the exact solution takes so much time for nodes bigger than 15
		t1=0
		cat("runing of test Seiner Tree Exact solution.. \n")
		a=Sys.time()
		try({EXA=steinertree("EXA",NULL,g ,FALSE,FALSE)}, silent=FALSE)
		b=Sys.time()   
		t1=b-a    
		cat("runtime of test EXA exact solution" ,i,j,"is", t1,units(t1)," and the steiner tree size is ",length(E(EXA[[1]])),"\n")
		if (length(EXA)> 1){cat( "solution is a list of  " ,length(EXA) , " ST trees",  "\n")	}	    
		cat("Executing SP test: \n") 
		a=Sys.time()
		ST2=steinertree("SP", NULL, g, FALSE, FALSE)
		b=Sys.time()
		t2=b-a
		cat("runtime of test ST2" ,i,j,"is", t2,units(t2)," and the steiner tree size is ",length(E(ST2[[1]])),"\n")
		#cat("Executing  ST3 test \n")	 
		#a=Sys.time()
		#ST3=steinertree("KRU", NULL, g, FALSE, FALSE)
		#b=Sys.time()
		#t4=b-a
		#cat("runtime of test ST3" ,i,j,"is", t4,units(t4)," and the steiner tree size is ",length(E(ST3[[1]])),"\n")  
		#cat("Executing ST4 appr steiner test \n")
		#a=Sys.time()
		#ST4=steinertree("RSP", NULL, g, FALSE, FALSE) #269 biggest num of V when 100 terminals and 13500 was the size
		#b=Sys.time()
		#t5=b-a
		#cat("runtime of test ST4 approximation alg" ,i,j,"is", t5,units(t5)," and the steiner tree size is ",length(E(ST4[[1]])),"\n") 
		#cat("Executing  SPM appr steiner test \n")
		#a=Sys.time()
		#SPM=steinertree("SPM", NULL, g, FALSE, FALSE)
		#b=Sys.time()
		#t6=b-a
		#if (class(SPM[[1]])== "igraph" ){cat("runtime of test SPM" ,i,j,"is", t1,units(t1)," and the steiner tree size is ",length(E(SPM[[1]])),"\n")	}	    
		#if (class(SPM[[1]])== "list" ){cat("runtime of test SPM" ,i,j,"is", t1,units(t1)," and the steiner tree size is ",length(E(SPM[[1]][[1]])) , "a list of  " ,length(SPM[[1]]) , " ST trees",  "\n")	}	    
	
 	       #--------------------------------------------------------------	
		runtimes=c(t1,t2,units(t1),units(t2))
		#runtimes=c(t1,t2,t3,t4,t5,t6,units(t1),units(t2),units(t3),units(t4),units(t5),units(t6))
		 cat("saving result data into time and steinter tree files... \n")
 	   	 try({save(EXA, file = paste(folder,"/EXA",j,"of",i,".RData",sep=""))}, silent=FALSE)
		 try({save(ST2, file = paste(folder,"/ST2",j,"of",i,".RData",sep=""))}, silent=FALSE)
		 #try({save(ST3, file = paste(folder,"/ST3",j,"of",i,".RData",sep=""))}, silent=FALSE)
		 #try({save(ST4, file = paste(folder,"/ST4",j,"of",i,".RData",sep=""))}, silent=FALSE)
		 #try({save(SPM, file = paste(folder,"/SPM",j,"of",i,".RData",sep=""))}, silent=FALSE)
		 save(runtimes, file = paste(folder,"/runtimes",j,"of",i,".RData",sep=""))
	}
  }	
#end of run test
}


#------------------------------------------------
# prints review of generated graphs by randomwalk
#
# can take a folder address to look inside it 
#------------------------------------------------

review_st_samples = function(folder= NULL,listofterminaltest,repetition)
{
	  grw=c()
	 if (is.null(folder))folder="steinerdata"
	 for(j in 1:length(listofterminaltest))
	 {
	 	for(i in 1:length(repetition) )
	 	 {
			cat("j,i:",j,"of",i,"\t")
			load(paste(folder,"/g",j,"of",i,".RData",sep="")) 
			cat("Ve num :",length(V(grw[[1]])) ,"\t")
			cat("Edge num :",length(E(grw[[1]])) ,"\t")
			cat("Terminals:",listofterminaltest[j],"\t")
			cat("Connected graph:",is.connected(grw[[1]]),"\n")

			if( i %% 25 == 0   )
			{
				cat("press Enter to continue...","\n")
				temp=scan()
		 	}

	 	 }
	 }
}


generate_st_samples= function(test, graph, folder= NULL,listofterminaltest,repetition)
{

	# load("mergedpathway.RData")
	# t=cbind(result[,1],result[,3],result[,2])
	# g=matrixtograph(t)
	# or instead use  g= read.graph("mergedpathway.gml", format="gml") to have the bigger graph

	#g= read.graph("pathway.gml", format="gml")
	#G=igraph.to.graphNEL(g)

	#for our experiment:
	#  load(file="/home/abidata/datasets/ProteinInteractions/PathwayCommons_graph.rda")

	# local access: setwd("/home/bit/sadeghi/Datasets")
	#load(file="PathwayCommons_graph.rda")

	#This graph forest has 418 components ,the biggest graph in the forest has 13340 nodes.
	#add="/home/abidata/datasets/ProteinInteractions/PathwayCommons_graph.rda"
	
	if(test=="exact"){
		#for exact solution comparison test
		if(is.null(folder)) folder= "steinerdata2"
		gensample(folder,TRUE,graph,listofterminaltest,repetition) #we make smaller graphs for exact experiments
		}
	if(test=="appr"){
		#for comparison of approximation algorithms
		if(is.null(folder)) folder= "steinerdata"
		gensample(folder,FALSE,graph,listofterminaltest,repetition) 
	}

	if(test=="enum"){
		#for comparison of enumerative akgorithms
		if(is.null(folder)) folder= "steinerdataEnum"
		gensample(folder,TRUE,graph,listofterminaltest,repetition) #we make smaller graphs for exact experiments

	}


}

steiner_simulation=function(test,listofterminaltest,repetition,testfolder = NULL)
{

	if(test== "exact"){
		if(is.null(listofterminaltest))counter = 1:3
		if(!is.null(listofterminaltest))counter = 1:length(listofterminaltest)
		#with exact solution  #we use same data sets now for both experiments to see the exact solution works with big numbers od veritces
		if(is.null(testfolder)) testfolder= "steinerdata2"
		exact_tests(testfolder, counter,listofterminaltest,repetition)
	}

	if(test=="appr"){
		#just approximations comparison
		if(is.null(listofterminaltest))counter = 1:5
		if(!is.null(listofterminaltest))counter = 1:length(listofterminaltest)
		if(is.null(testfolder)) testfolder= "steinerdata"
		appr_tests(testfolder , counter,listofterminaltest,repetition)
		asp_in_appr_tests(testfolder, counter, listofterminaltest, repetition)
	       appr_KB_tests(testfolder, counter, listofterminaltest, repetition)

	}
	if(test== "enum"){
		#adding the multi steiner algorithm results on simulation data for sake of comarison with other approximation algorithms
		if(is.null(listofterminaltest))counter = 1:2
		if(!is.null(listofterminaltest))counter = 1:length(listofterminaltest)
		if(is.null(testfolder)) testfolder= "steinerdataenum"
	        mulSteiner(testfolder,counter,listofterminaltest,repetition)
	}

	if(test== "asp_app"){
		#adding the multi steiner algorithm results on simulation data for sake of comarison with other approximation algorithms
		if(is.null(listofterminaltest))counter = 1:5
		if(!is.null(listofterminaltest))counter = 1:length(listofterminaltest)
		if(is.null(testfolder)) testfolder= "steinerdata"
	        asp_in_appr_tests(testfolder,counter,listofterminaltest,repetition)
	}
	if(test== "KB_app"){
		#adding the multi steiner algorithm results on simulation data for sake of comarison with other approximation algorithms
		if(is.null(listofterminaltest))counter = 1:5
		if(!is.null(listofterminaltest))counter = 1:length(listofterminaltest)
		if(is.null(testfolder)) testfolder= "steinerdata"
	        appr_KB_tests(testfolder,counter,listofterminaltest,repetition)
	}
}
# sample :  steiner_simulation("asp_app", c(5,8,15,50,70) ,repetition, "steinerdata")



