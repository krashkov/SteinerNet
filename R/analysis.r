steinertestplot_appr_dens_e  = function(testfolder , outputname , incldue_exact_sol, experiments= NULL, listofterminaltest = NULL ,repetition= NULL)
{
  incldue_exact_sol = FALSE
 if(is.null(repetition)){
  listofterminaltest=c(5,8,20,50,70)#for exact solution c(5,8,15) ,for appr :  c(5,8,20,50,70)
  repetition=c(0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5    ,0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5,   0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5 ,   0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5,   0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5 ) # i with 50 elements
  }
  #----------------------------------- 
  # reading the data into proper time frame for box plot
  #
  #------------------------------------
  time_list_1=c()
  size_list_0=c()
  alg_list_0=c()
  alg_list_1=c()
  timeplot=c()
  sizeplot=c()
  tlist=c()
  slist=c()
  ST1=ST2=ST3=ST4=c()
  counter = experiments
  if (is.null(experiments) ){ counter = 1:length(listofterminaltest) }
	
  pdf(outputname, width=10, height =10, paper="a4r")
  par(mfrow=c(2,1))
  for(j in counter)
  {
	for(i in 1:length(repetition) )
	{
		 cat("heuristic approach comparison test number i,j",i,j,"\n")
		 load(paste(testfolder,"/runtimes",j,"of",i,".RData",sep=""))
			 
		 load(paste(testfolder,"/ST2",j,"of",i,".RData",sep=""))
		 v2= length(V(ST2[[1]]))
		 e2= length(E(ST2[[1]]))
		 size_list_0[length( size_list_0)+1]=(2*e2)/(v2*(v2 - 1))

		 alg_list_0 [length(alg_list_0)+1] = "SP"
		 alg_list_1 [length(alg_list_1)+1] = "SP"

		 load(paste(testfolder,"/ST3",j,"of",i,".RData",sep=""))
		 v3= length(V(ST3[[1]]))
		 e3= length(E(ST3[[1]]))
		 size_list_0[length( size_list_0)+1]= (2*e3)/(v3*(v3-1))

		 alg_list_0 [length(alg_list_0)+1] = "KBH"
		 alg_list_1 [length(alg_list_1)+1] = "KBH"

		 load(paste(testfolder,"/ST4",j,"of",i,".RData",sep=""))
		 v4= length(V(ST4[[1]]))
		 e4= length(E(ST4[[1]]))
		 size_list_0[length( size_list_0)+1]= (2*e4)/(v4*(v4-1))

		 alg_list_0 [length(alg_list_0)+1] = "RSP"
		 alg_list_1 [length(alg_list_1)+1] = "RSP"
	}

	sizeplot = data.frame(sizes =size_list_0 ,algorithm = alg_list_0 )
	size_list_0=c()
	alg=c()
	boxplot(sizes ~ algorithm, data = sizeplot , col = "lightgray" ,xlab="Algorithm type", ylab="Density ")
	title("Steiner Tree Density Comparison", paste("Number of terminals",listofterminaltest[j]))
    }

	mtext("PS: Shortest Path Heuristic " ,4 ,line=0)
	mtext("KBH: Kruskal-Based Heuristic        RSP: Random Approximation New" ,4 ,line=1)
#mtext("Graph with 418 components ,Biggest graph 13340 nodes.",2 ,line=6)

	dev.off()
}




steinertestplot_exact_dens_e  = function(testfolder , outputname , incldue_exact_sol,experiments= NULL,  listofterminaltest = NULL ,repetition= NULL)
{
  incldue_exact_sol = TRUE
  if(is.null(repetition)){
  listofterminaltest=c(5,8,15)#for exact solution c(5,8,15) ,for appr :  c(5,8,20,50,100)
  repetition=c(0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5,    0.5,0.5,0.5,0.5,0.5,   0.5,0.5,0.5,0.5,0.5 ,   0.5,0.5,0.5,0.5,0.5 ,   0.5,0.5,0.5,0.5,0.5 ,   0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5 , 0.5,0.5,0.5,0.5,0.5 , 0.5,0.5,0.5,0.5,0.5 ) # i with 50 elements
  }
  #----------------------------------- 
  # reading the data into proper time frame for box plot
  #
  #------------------------------------
  EXA=SPM=ST1=ST2=ST3=ST4=grw=c()
  time_list_1=c()
  size_list_0=c()
  alg_list_0=c()
  alg_list_1=c()
  timeplot=c()
  sizeplot=c()
  tlist=c()
  slist=c()
  counter = experiments
  if (is.null(experiments) ){ counter = 1:length(listofterminaltest) }
	
  pdf(outputname, width=10, height =10, paper="a4r")
  par(mfrow=c(2,1))
  for(j in counter)
  {
	for(i in 1:length(repetition) )
	{
		#load(paste(testfolder,"/g",j,"of",i,".RData",sep="")) 
		#v0= length(V(grw[[1]]))
		#e0= length(E(grw[[1]])) 
		#size_list_0[length( size_list_0)+1]=e0 
		#time_list_1[length( time_list_1)+1]= "NA" 
		#alg [length(alg)+1] = "NA"
		cat("exact approach comparison, test number i,j",i,j,"\n")
		ve= NA
		ee= NA		
		load(paste(testfolder,"/runtimes",j,"of",i,".RData",sep=""))
		#the exact solution takes so much time for nodes bigger than 23
		#if(length(V(grw[[1]])) < 23 )  # we will not include it inside when j>2 becasue sometimes the nodes are more and some times less than 23 when j=3 
		load(paste(testfolder,"/g",j,"of",i,".RData",sep=""))
		cat("num ",length(V(grw[[1]]))," of vertices of graph having exact steiner tree in",j,"of",i ,"SFX file \n")
		load(paste(testfolder,"/EXA",j,"of",i,".RData",sep=""))
		ve <- length(V(EXA[[1]]))
		ee <- length(E(EXA[[1]]))
		size_list_0[length( size_list_0)+1]=(2*ee)/(ve*(ve-1))
		alg_list_0 [length(alg_list_0)+1] = "Ext"
			
			 
		 load(paste(testfolder,"/ST2",j,"of",i,".RData",sep=""))
		 v2= length(V(ST2[[1]]))
		 e2= length(E(ST2[[1]]))
		 size_list_0[length( size_list_0)+1]= (2*e2)/(v2*(v2-1))
		 alg_list_0 [length(alg_list_0)+1] = "PS"

		 load(paste(testfolder,"/ST3",j,"of",i,".RData",sep=""))
		 v3= length(V(ST3[[1]]))
		 e3= length(E(ST3[[1]]))
		 size_list_0[length( size_list_0)+1]= (2*e3)/(v3*(v3-1))
		 alg_list_0 [length(alg_list_0)+1] = "KBH"

		 #ST4 RSP
		 load(paste(testfolder,"/ST4",j,"of",i,".RData",sep=""))
		 load(paste(testfolder,"/runtimesST4",j,"of",i,".RData",sep=""))
		 v4= length(V(ST4[[1]]))
		 e4= length(E(ST4[[1]]))
		 size_list_0[length( size_list_0)+1]= (2*e4)/(v4*(v4-1))
		 alg_list_0 [length(alg_list_0)+1] = "RSP"

		 #SPM
		 load(paste(testfolder,"/SPM",j,"of",i,".RData",sep=""))
		 load(paste(testfolder,"/runtimesSPM",j,"of",i,".RData",sep=""))	
		 v5= length(V(SPM[[1]]))
		 e5= length(E(SPM[[1]]))
		 size_list_0[length( size_list_0)+1]= (2*e5)/(v5*(v5-1))
		 alg_list_0 [length(alg_list_0)+1] = "SPM"
	}

	sizeplot = data.frame(sizes =size_list_0 ,algorithm = alg_list_0 )
	size_list_0=c()
	alg=c()
	boxplot(sizes ~ algorithm, data = sizeplot , col = "lightgray" ,xlab="Algorithm type", ylab="Density ")
	title("Steiner Tree Density Comparison", paste("Number of terminals",listofterminaltest[j]))
    }

	mtext("PS: Shortest Path Heuristic "        ,4 ,line=1)
	mtext("KBH: Kruskal-Based Heuristic        RSP: Random Approximation New" ,4 ,line=2)
	mtext("SPM: Enumerate Approximation PH      ASP: All the Shortest Paths" ,4 ,line=3)
#mtext("Graph with 418 components ,Biggest graph 13340 nodes.",2 ,line=6)

	dev.off()
}


steinertestplot_mul_dens_e  = function(testfolder , outputname , incldue_exact_sol,experiments= NULL , listofterminaltest = NULL ,repetition= NULL)
{
  incldue_exact_sol = TRUE
  if(is.null(repetition)){
  listofterminaltest=c(5,8,15)#for exact solution c(5,8,15) ,for appr :  c(5,8,20,50,100)
  repetition=c(0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5    ,0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5 ,   0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5 ,   0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5 , 0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5  ) # i with 50 elements
  }
  #----------------------------------- 
  # reading the data into proper time frame for box plot
  #
  #------------------------------------
  EXA=subg=SPM=grw=ver=m=c()
  time_list_1=c()
  size_list_0=c()
  alg_list_0=c()
  alg_list_1=c()
  timeplot=c()
  sizeplot=c()
  tlist=c()
  slist=c()
  counter = experiments
  if (is.null(experiments) ){ counter = 1:length(listofterminaltest) }
	
  pdf(outputname, width=10, height =10, paper="a4r")
  par(mfrow=c(2,1))
  for(j in counter)
  {
	for(i in 1:length(repetition) )
	{
		 cat("steiner tree enumeration comparison test number i,j",i,j,"\n")
		 ve= NA
		 ee= NA
		load(paste(testfolder,"/g",j,"of",i,".RData",sep=""))
		cat("num ",length(V(grw[[1]]))," of vertices of graph having exact steiner tree in",j,"of",i ,"SFX file \n")
		load(paste(testfolder,"/MEXA",j,"of",i,".RData",sep=""))
		ve=length(V(EXA))
		ee=length(E(EXA))
		size_list_0[length( size_list_0)+1]= (2*ee)/(ve*(ve-1))
		load(paste(testfolder,"/runtimesM",j,"of",i,".RData",sep=""))
		alg_list_0 [length(alg_list_0)+1] = "Ext"
			
		 load(paste(testfolder,"/MSPM",j,"of",i,".RData",sep=""))
		 #second object in ST1 sould be taken.the first result is the original graph with colored nodes.
		 v2= length(V(SPM))
		 e2= length(E(SPM))
		 size_list_0[length( size_list_0)+1]= (2*e2)/(v2*(v2-1))
		alg_list_0 [length(alg_list_0)+1] = "SPM"

		 load(paste(testfolder,"/ALPH",j,"of",i,".RData",sep=""))
		 print (length(ver))
		 v3= length(ver)
		 e3= m
		 size_list_0[length( size_list_0)+1]= (2*e3)/(v3*(v3-1))
		 alg_list_0 [length(alg_list_0)+1] = "ASP"
	}

	sizeplot = data.frame(sizes =size_list_0 ,algorithm = alg_list_0 )
	size_list_0=c()
	alg=c()
	boxplot(sizes ~ algorithm, data = sizeplot , col = "lightgray" ,xlab="Algorithm type", ylab=" Density")
	title("Multi Steiner Tree Density Comparison", paste("Number of terminals",listofterminaltest[j]))
    }

	mtext("EXA: EnumerateExact,		SPM: Enumerate Approximation PH,		ASP: All the Shortest Paths" ,4 ,line=1)
	dev.off()
}

steinertestplot_org_dens_e = function(testfolder , outputname , incldue_exact_sol,experiments= NULL , listofterminaltest = NULL ,repetition= NULL)
{
  incldue_exact_sol = TRUE
  if(is.null(repetition)){
  listofterminaltest=c(5,8,15)#for exact solution c(5,8,15) ,for appr :  c(5,8,20,50,100)
  repetition=c(0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5    ,0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5 ,   0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5 ,   0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5 , 0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5 ) # i with 50 elements
  }
  #----------------------------------- 
  # reading the data into proper time frame for box plot
  #
  #------------------------------------
  time_list_1=c()
  size_list_0=c()
  alg_list_0=c()
  alg_list_1=c()
  timeplot=c()
  sizeplot=c()
  tlist=c()
  slist=c()
  g=grw=c()
  counter = experiments
  if (is.null(experiments) ){ counter = 1:length(listofterminaltest) }
	
  pdf(outputname, width=10, height =10, paper="a4r")
  par(mfrow=c(2,1))
  j=1
  #for(j in counter)
  #{
	for(i in 1:length(repetition) )
	{
	
		cat("original graph i,j",i,j,"\n")
		load(paste(testfolder,"/g",j,"of",i,".RData",sep=""))
		ve=length(V(grw[[1]]))
		ee=length(E(grw[[1]]))		
		size_list_0[length( size_list_0)+1]= (2*ee)/(ve*(ve-1))
		alg_list_0 [length(alg_list_0)+1] = "|S|=5"

		load(paste(testfolder,"/g",j+1,"of",i,".RData",sep=""))
		ve=length(V(grw[[1]]))
		ee=length(E(grw[[1]]))
		size_list_0[length( size_list_0)+1]= (2*ee)/(ve*(ve-1))
		alg_list_0 [length(alg_list_0)+1] = "|S|=8"


	}

	sizeplot = data.frame(sizes =size_list_0 ,algorithm = alg_list_0 )
	size_list_0=c()
	alg=c()
	boxplot(sizes ~ algorithm, data = sizeplot , col = "lightgray" ,xlab=" Number of terminals", ylab="Density")
	title("Original Random Subgraph Density", paste("Number of terminals",listofterminaltest[j] ,", ",listofterminaltest[j]))
	dev.off()
}



#-----------------------------------------------------------------------------------------------------------------------------------------
steinertest_median_node_edge_mul_venn  = function(testfolder , outputname , incldue_exact_sol,experiments= NULL, listofterminaltest = NULL ,repetition= NULL)
{
 incldue_exact_sol = TRUE
 if(is.null(repetition)){
 	listofterminaltest=c(5,8)#for exact solution c(5,8,15) ,for appr :  c(5,8,20,50,100)
 	repetition=c(0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5    ,0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5 ,   0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5 ,   0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5 , 0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5  ) # i with 50 elements
  }
  grw=c()
  #----------------------------------- 
  # reading the data into proper time frame for box plot
  #
  #------------------------------------
  	   v_list_1=c()
 	   v_list_2=c()
 	   v_list_3=c()
 	   v_list_4=c()
 	   v_list_5=c()
 	   v_list_6=c()
 	   v_list_7=c()
 	   v_list_8=c()
 	   	   
 	   e_list_1=c()
 	   e_list_2=c()
 	   e_list_3=c()
 	   e_list_4=c()
 	   e_list_5=c()
 	   e_list_6=c()
 	   e_list_7=c()
 	   e_list_8=c()
  EXA=subg=SPM=c()
  counter = experiments
  if (is.null(experiments) ){ counter = 1:length(listofterminaltest) }
	
  pdf(outputname, width=10, height =10, paper="a4r")
  par(mfrow=c(2,1))
  for(j in counter)
  {
	for(i in 1:length(repetition) )
	{
	 	cat("Enumeration  steiner tree comparison test number i,j",i,j,"\n")
		
 		load(paste(testfolder,"/g",j,"of",i,".RData",sep=""))
		cat("num ",length(V(grw[[1]]))," of vertices of graph having steiner tree in",j,"of",i ,"SPM,EXA,ASP, file \n")
		load(paste(testfolder,"/MEXA",j,"of",i,".RData",sep=""))
		load(paste(testfolder,"/MSPM",j,"of",i,".RData",sep=""))
    		load(paste(testfolder,"/ALPH",j,"of",i,".RData",sep=""))
		#for testing : load(paste("steinerdatamulti" ,"/ALPH",1,"of",2,".RData",sep=""))
		vx =EXA[[9]][[3]]$name    
		vm= SPM[[9]][[3]]$name
		vs=subg[[9]][[3]]$name

		ex =E(EXA)
		em= E(SPM)
		es=E(subg)	
	
		uni=union(vx,vm)
		uni=union(uni,vs)	
			
		euni=union(ex,em)
		euni=union(euni,es)	
			
		VX=is.element(uni,vx)
		VM=is.element(uni,vm)
		VS=is.element(uni,vs)
		c3=cbind(VX,VM,VS)
		ve <- vennCounts(c3)
		sump=0
		for (i in 1: 8 )sump= sump+ ve[[i,4]]
 		m=as.vector(1:8)
		for (i in 1: 8 ) m[i]=  ve[[i,4]] *100/sump	
		
		EX=is.element(euni,ex)
		EM=is.element(euni,em)
		ES=is.element(euni,es)
		ec3=cbind(EX,EM,ES)
		ee <- vennCounts(ec3)
		sump=0
		for (i in 1: 8 )sump= sump+ ee[[i,4]]
 		em=as.vector(1:8)
		for (i in 1: 8 )em[i]=  ee[[i,4]] *100/sump	
				
		v_list_1[length( v_list_1)+1]=  m[1]
		v_list_2[length( v_list_2)+1]=  m[2]			
		v_list_3[length( v_list_3)+1]=  m[3]				
		v_list_4[length(v_list_4 )+1]=  m[4]			
		v_list_5[length( v_list_5)+1]=  m[5]
		v_list_6[length( v_list_6)+1]=  m[6]			
		v_list_7[length( v_list_7)+1]=  m[7]			
		v_list_8[length( v_list_8)+1]=  m[8]
			
		e_list_1[length( e_list_1)+1]=  em[1]
		e_list_2[length( e_list_2)+1]=  em[2]			
		e_list_3[length( e_list_3)+1]=  em[3]				
		e_list_4[length( e_list_4)+1]=  em[4]			
		e_list_5[length( e_list_5)+1]=  em[5]
		e_list_6[length( e_list_6)+1]=  em[6]			
		e_list_7[length( e_list_7)+1]=  em[7]			
		e_list_8[length( e_list_8)+1]=  em[8]	
	}

			cat (paste("Node coverage percent comparison ", "Number of terminals: ",listofterminaltest[j], " Non ",median(v_list_1)))
			cat (paste("Node coverage percent comparison ", "Number of terminals: ",listofterminaltest[j], " ASP only ",median(v_list_2)))
			cat (paste("Node coverage percent comparison ", "Number of terminals: ",listofterminaltest[j], " SPM only ",median(v_list_3)))
			cat (paste("Node coverage percent comparison ", "Number of terminals: ",listofterminaltest[j], " SPM and ASP ",median(v_list_4)))
			cat (paste("Node coverage percent comparison ", "Number of terminals: ",listofterminaltest[j], " Eex only ",median(v_list_5)))
			cat (paste("Node coverage percent comparison ", "Number of terminals: ",listofterminaltest[j], " Eex and ASP ",median(v_list_6)))
			cat (paste("Node coverage percent comparison ", "Number of terminals: ",listofterminaltest[j], " Eex and SPM ",median(v_list_7)))
			cat (paste("Node coverage percent comparison ", "Number of terminals: ",listofterminaltest[j], " Eex and SPM and ASP  ",median(v_list_8)))
  
			cat (paste("Edge coverage percent comparison ", "Number of terminals: ",listofterminaltest[j], " Non ",median(e_list_1)))
			cat (paste("Edge coverage percent comparison ", "Number of terminals: ",listofterminaltest[j], " ASP only ",median(e_list_2)))
			cat (paste("Edge coverage percent comparison ", "Number of terminals: ",listofterminaltest[j], " SPM only ",median(e_list_3)))
			cat (paste("Edge coverage percent comparison ", "Number of terminals: ",listofterminaltest[j], " SPM and ASP ",median(e_list_4)))
			cat (paste("Edge coverage percent comparison ", "Number of terminals: ",listofterminaltest[j], " Eex only ",median(e_list_5)))
			cat (paste("Edge coverage percent comparison ", "Number of terminals: ",listofterminaltest[j], " Eex and ASP ",median(e_list_6)))
			cat (paste("Edge coverage percent comparison ", "Number of terminals: ",listofterminaltest[j], " Eex and SPM ",median(e_list_7)))
			cat (paste("Edge coverage percent comparison ", "Number of terminals: ",listofterminaltest[j], " Eex and SPM and ASP  ",median(e_list_8)))
  }
	dev.off()
}


#---------------------------------------------------------------------------------------------------------------------------------------
steinertestplot_exact  = function(test_folder , outputname , incldue_exact_sol,experiments= NULL, listofterminaltest = NULL ,repetition= NULL)
{
	  incldue_exact_sol = TRUE
	  if(is.null(repetition)){
	  	listofterminaltest=c(5,8,15)#for exact solution c(5,8,15) ,for appr :  c(5,8,20,50,100)
	  	repetition=c(0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5,    0.5,0.5,0.5,0.5,0.5,   0.5,0.5,0.5,0.5,0.5 ,   0.5,0.5,0.5,0.5,0.5 ,   0.5,0.5,0.5,0.5,0.5 ,   0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5 , 0.5,0.5,0.5,0.5,0.5 , 0.5,0.5,0.5,0.5,0.5 ) # i with 50 elements
	  }
	  ST1=ST2=ST3=ST4=SPM=EXA=runtimes=runtimes4=runtimesM=grw=c()
	  #----------------------------------- 
	  # reading the data into proper time frame for box plot
	  #
	  #------------------------------------
	  time_list_1=c()
	  size_list_0=c()
	  alg_list_0=c()
	  alg_list_1=c()
	  timeplot=c()
	  sizeplot=c()
	  tlist=c()
	  slist=c()
	  counter = experiments
	  if (is.null(experiments) ){ counter = 1:length(listofterminaltest) }
		
	  pdf(outputname, width=10, height =10, paper="a4r")
	  par(mfrow=c(2,1))
	  for(j in counter)
	  {
		for(i in 1:length(repetition) )
		{
			 #load(paste(test_folder,"/g",j,"of",i,".RData",sep="")) 
			 #v0= length(V(grw[[1]]))
			 #e0= length(E(grw[[1]])) 
			 #size_list_0[length( size_list_0)+1]=e0 
			 #time_list_1[length( time_list_1)+1]= "NA" 
			 #alg [length(alg)+1] = "NA"
			 cat("exact approach comparison, test number i,j",i,j,"\n")
			 ve= NA
			 ee= NA		
			 load(paste(test_folder,"/runtimes",j,"of",i,".RData",sep=""))
			 #the exact solution takes so much time for nodes bigger than 23
			 #if(length(V(grw[[1]])) < 23 )  # we will not include it inside when j>2 becasue sometimes the nodes are more and some times less than 23 when j=3 
			load(paste(test_folder,"/g",j,"of",i,".RData",sep=""))
			cat("num ",length(V(grw[[1]]))," of vertices of graph having exact steiner tree in",j,"of",i ,"SFX file \n")
			load(paste(test_folder,"/EXA",j,"of",i,".RData",sep=""))
			ve <- length(V(EXA[[1]]))
			ee <- length(E(EXA[[1]]))
			size_list_0[length( size_list_0)+1]=ee
			
			time_list_1[length( time_list_1)+1]=time_norm(runtimes[1],runtimes[7] ) 
						  
			alg_list_1 [length(alg_list_1)+1] = "Ext"
			alg_list_0 [length(alg_list_0)+1] = "Ext"
				
				 
			 load(paste(test_folder,"/ST2",j,"of",i,".RData",sep=""))
			 v2= length(V(ST2[[1]]))
			 e2= length(E(ST2[[1]]))
			 size_list_0[length( size_list_0)+1]=e2 
			 time_list_1[length( time_list_1)+1]=time_norm(runtimes[3],runtimes[9])
			 alg_list_0 [length(alg_list_0)+1] = "SP"
			 alg_list_1 [length(alg_list_1)+1] = "SP"

			 load(paste(test_folder,"/ST3",j,"of",i,".RData",sep=""))
			 v3= length(V(ST3[[1]]))
			 e3= length(E(ST3[[1]]))
			 size_list_0[length( size_list_0)+1]=e3
			 time_list_1[length( time_list_1)+1]=time_norm(runtimes[4], runtimes[10])
			 alg_list_0 [length(alg_list_0)+1] = "KBH"
			 alg_list_1 [length(alg_list_1)+1] = "KBH"

			 #ST4 RSP
			 load(paste(test_folder,"/STA4",j,"of",i,".RData",sep=""))
			 load(paste(test_folder,"/runtimesST4",j,"of",i,".RData",sep=""))
			 v4= length(V(ST4[[1]]))
			 e4= length(E(ST4[[1]]))
			 size_list_0[length( size_list_0)+1]=e4
			 time_list_1[length( time_list_1)+1]=time_norm(runtimes4[1] ,runtimes4[1])
			 alg_list_0 [length(alg_list_0)+1] = "RSP"
			 alg_list_1 [length(alg_list_1)+1] = "RSP"

			 #SPM
			 load(paste(test_folder,"/SPM2",j,"of",i,".RData",sep=""))
			 load(paste(test_folder,"/runtimesSPM",j,"of",i,".RData",sep=""))	
			 v5= length(V(SPM[[1]]))
			 e5= length(E(SPM[[1]]))
			 size_list_0[length( size_list_0)+1]=e5
			 time_list_1[length( time_list_1)+1]=time_norm(runtimesM[1] ,runtimesM[1])
			 alg_list_0 [length(alg_list_0)+1] = "SPM"
			 alg_list_1 [length(alg_list_1)+1] = "SPM"	
		}

		timeplot = data.frame(times =time_list_1 ,algorithm = alg_list_1 )
		sizeplot = data.frame(sizes =size_list_0 ,algorithm = alg_list_0 )
		time_list_1=c()
		size_list_0=c()
		alg=c()
		boxplot(times ~ algorithm, data = timeplot , col = "lightgray" ,xlab="Algorithm type", ylab="Time log10(sec) ")
		title("Time comparison", paste("Number of terminals:",listofterminaltest[j]))
		boxplot(sizes ~ algorithm, data = sizeplot , col = "lightgray" ,xlab="Algorithm type", ylab="Steiner graph Edges ")
		title("Steiner Tree size comparison", paste("Number of terminals",listofterminaltest[j]))
	    }

		mtext("SP: Shortest Path Heuristic"    ,3 ,line=0)
		mtext("KBH: Kruskal-Based Heuristic        RSP: Random Approximation New" ,3 ,line=1)
		mtext("SPM: Enumerated Approximation PH      ASP: All the Shortest Paths" ,3 ,line=2)
	#mtext("Graph with 418 components ,Biggest graph 13340 nodes.",2 ,line=6)

		dev.off()
}

#show the range of v and size of each experiment
#show approximation ratio in each experiment


steinertestplot_mul  = function(test_folder , outputname , incldue_exact_sol,experiments= NULL, listofterminaltest = NULL ,repetition= NULL)
{
	  incldue_exact_sol = TRUE
	  if(is.null(repetition)){
	  listofterminaltest=c(5,8,15)#for exact solution c(5,8,15) ,for appr :  c(5,8,20,50,100)
	  repetition=c(0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5    ,0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5 ,   0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5 ,   0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5 , 0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5  ) # i with 50 elements
	  }
	  #----------------------------------- 
	  # reading the data into proper time frame for box plot
	  #
	  #------------------------------------
	  time_list_1=c()
	  size_list_0=c()
	  alg_list_0=c()
	  alg_list_1=c()
	  timeplot=c()
	  sizeplot=c()
	  tlist=c()
	  slist=c()
	  EXA=subg=SPM=m=grw=runtimes=c()
	  counter = experiments
	  if (is.null(experiments) ){ counter = 1:length(listofterminaltest) }
		
	  pdf(outputname, width=10, height =10, paper="a4r")
	  par(mfrow=c(2,1))
	  for(j in counter)
	  {
		for(i in 1:length(repetition) )
		{
			 cat("Enum steiner tree comparison test number i,j",i,j,"\n")
			 ve= NA
			 ee= NA
			load(paste(test_folder,"/g",j,"of",i,".RData",sep=""))
			cat("num ",length(V(grw[[1]]))," of vertices of graph having exact steiner tree in",j,"of",i ,"SFX file \n")
			load(paste(test_folder,"/MEXA",j,"of",i,".RData",sep=""))
			ee <- length(E(EXA))
			size_list_0[length( size_list_0)+1]=ee
			load(paste(test_folder,"/runtimesM",j,"of",i,".RData",sep=""))
			time_list_1[length( time_list_1)+1]=time_norm(runtimes[1] ,runtimes[2])	  
			alg_list_1 [length(alg_list_1)+1] = "Ext"
			alg_list_0 [length(alg_list_0)+1] = "Ext"
				
			 load(paste(test_folder,"/MSPM",j,"of",i,".RData",sep=""))
			 #second object in ST1 sould be taken.the first result is the original graph with colored nodes.
			 e1= length(E(SPM))
			 size_list_0[length( size_list_0)+1]=e1
			time_list_1[length( time_list_1)+1]=time_norm(runtimes[3] ,runtimes[4])	  
			alg_list_0 [length(alg_list_0)+1] = "SPM"
	 		alg_list_1 [length(alg_list_1)+1] = "SPM"

			 load(paste(test_folder,"/ALPH",j,"of",i,".RData",sep=""))
			 e3= m
			 size_list_0[length( size_list_0)+1]=e3
			 time_list_1[length( time_list_1)+1]=time_norm(runtimes[5] ,runtimes[6])
			 alg_list_0 [length(alg_list_0)+1] = "ASP"
			 alg_list_1 [length(alg_list_1)+1] = "ASP"	
		}

		timeplot = data.frame(times =time_list_1 ,algorithm = alg_list_1 )
		sizeplot = data.frame(sizes =size_list_0 ,algorithm = alg_list_0 )
		time_list_1=c()
		size_list_0=c()
		alg=c()
		boxplot(times ~ algorithm, data = timeplot , col = "lightgray" ,xlab="Algorithm type", ylab="Time log10(sec)")
		title("Time comparison", paste("Number of terminals:",listofterminaltest[j]))
		boxplot(sizes ~ algorithm, data = sizeplot , col = "lightgray" ,xlab="Algorithm type", ylab="Steiner graph Edges ")
		title("Steiner Tree size comparison", paste("Number of terminals",listofterminaltest[j]))
	    }

		mtext("EXA: Enumerated Exact,		SPM: Enumerated Approximation PH,		ASP: All the Shortest Paths" ,4 ,line=1)
		dev.off()
}

#show the range of v and size of each experiment
#show approximation ratio in each experiment


#this is the plot for the original graph
steinertestplot_org = function(test_folder , outputname , incldue_exact_sol,experiments= NULL, listofterminaltest = NULL ,repetition= NULL)
{
	  incldue_exact_sol = TRUE
	  if(is.null(repetition)){
	  	listofterminaltest=c(5,8,15)#for exact solution c(5,8,15) ,for appr :  c(5,8,20,50,100)
	  	repetition=c(0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5    ,0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5 ,   0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5 ,   0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5 , 0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5 ) # i with 50 elements
	  }
	  #----------------------------------- 
	  # reading the data into proper time frame for box plot
	  #
	  #------------------------------------
	  time_list_1=c()
	  size_list_0=c()
	  alg_list_0=c()
	  alg_list_1=c()
	  timeplot=c()
	  sizeplot=c()
	  tlist=c()
	  slist=c()
	  g=grw=c()
	  counter = experiments
	  if (is.null(experiments) ){ counter = 1:length(listofterminaltest) }
		
	  pdf(outputname, width=10, height =10, paper="a4r")
	  par(mfrow=c(2,1))
	  j=1
	  #for(j in counter)
	  #{
		for(i in 1:length(repetition) )
		{
		
			cat("original graph i,j",i,j,"and",i,j+1,"\n")
			load(paste(test_folder,"/g",j,"of",i,".RData",sep=""))
			size_list_0[length( size_list_0)+1]=length(E(grw[[1]]))
			alg_list_0 [length(alg_list_0)+1] = "|S|=5"

			load(paste(test_folder,"/g",j+1,"of",i,".RData",sep=""))
			size_list_0[length( size_list_0)+1]=length(E(grw[[1]]))
			alg_list_0 [length(alg_list_0)+1] = "|S|=8"


		}

		sizeplot = data.frame(sizes =size_list_0 ,algorithm = alg_list_0 )
		size_list_0=c()
		alg=c()
		boxplot(sizes ~ algorithm, data = sizeplot , col = "lightgray" ,xlab="Algorithm type", ylab="Steiner graph Edges ")
		title("Original Random Subgraph Size", paste("Number of terminals",listofterminaltest[j] ,", ",listofterminaltest[j+1]))
	    #}


	#mtext("Graph with 418 components ,Biggest graph 13340 nodes.",2 ,line=6)

		dev.off()
}




steinertestplot_appr  = function(test_folder , outputname , incldue_exact_sol,experiments= NULL, listofterminaltest = NULL ,repetition= NULL)
{
	  incldue_exact_sol = FALSE
	  if(is.null(repetition)){
	  	listofterminaltest=c(5,8,20,50,70)#for exact solution c(5,8,15) ,for appr :  c(5,8,20,50,70)
	  	repetition=c(0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5    ,0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5,   0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5 ,   0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5,   0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5 ) # i with 50 elements
	  }
	  #----------------------------------- 
	  # reading the data into proper time frame for box plot
	  #
	  #------------------------------------
	  ST1=ST2=ST3=ST4=runtimes=c()
	  time_list_1=c()
	  size_list_0=c()
	  alg_list_0=c()
	  alg_list_1=c()
	  timeplot=c()
	  sizeplot=c()
	  tlist=c()
	  slist=c()
	  counter = experiments
	  if (is.null(experiments) ){ counter = 1:length(listofterminaltest) }
		
	  pdf(outputname, width=10, height =10, paper="a4r")
	  par(mfrow=c(2,1))
	  for(j in counter)
	  {
		for(i in 1:length(repetition) )
		{
			 cat("heuristic approach comparison test number i,j",i,j,"\n")
			 load(paste(test_folder,"/runtimes",j,"of",i,".RData",sep=""))

					 
			 load(paste(test_folder,"/ST2",j,"of",i,".RData",sep=""))
			 v2= length(V(ST2[[1]]))
			 e2= length(E(ST2[[1]]))
			 size_list_0[length( size_list_0)+1]=e2 
			 time_list_1[length( time_list_1)+1]=time_norm(runtimes[2],runtimes[6])
			 alg_list_0 [length(alg_list_0)+1] = "SP"
			 alg_list_1 [length(alg_list_1)+1] = "SP"

			 load(paste(test_folder,"/ST3",j,"of",i,".RData",sep=""))
			 v3= length(V(ST3[[1]]))
			 e3= length(E(ST3[[1]]))
			 size_list_0[length( size_list_0)+1]=e3
			 time_list_1[length( time_list_1)+1]=time_norm(runtimes[3], runtimes[7])
			 alg_list_0 [length(alg_list_0)+1] = "KBH"
			 alg_list_1 [length(alg_list_1)+1] = "KBH"

			 load(paste(test_folder,"/ST4",j,"of",i,".RData",sep=""))
			 v3= length(V(ST4[[1]]))
			 e3= length(E(ST4[[1]]))
			 size_list_0[length( size_list_0)+1]=e3
			 time_list_1[length( time_list_1)+1]=time_norm(runtimes[4] ,runtimes[8])
			 alg_list_0 [length(alg_list_0)+1] = "RSP"
			 alg_list_1 [length(alg_list_1)+1] = "RSP"
		}


		timeplot = data.frame(times =time_list_1 ,algorithm = alg_list_1 )
		sizeplot = data.frame(sizes =size_list_0 ,algorithm = alg_list_0 )
		time_list_1=c()
		size_list_0=c()
		alg=c()
		boxplot(times ~ algorithm, data = timeplot , col = "lightgray" ,xlab="Algorithm type", ylab="Time log(sec)")
		title("Time comparison", paste("Number of terminals:",listofterminaltest[j]))
		boxplot(sizes ~ algorithm, data = sizeplot , col = "lightgray" ,xlab="Algorithm type", ylab="Steiner graph Edges ")
		title("Steiner Tree size comparison", paste("Number of terminals",listofterminaltest[j]))
    }

	mtext("SP: Shortest Path Heuristic " ,4 ,line=0)
	mtext("KBH: Kruskal-Based Heuristic        RSP: Random Approximation New" ,4 ,line=1)
#mtext("Graph with 418 components ,Biggest graph 13340 nodes.",2 ,line=6)

	dev.off()
}


time_norm = function(value,type)
{
	num= as.numeric(value)
	if (type== "mins" ){num=num*60}
	if (type== "hours" ){num=num*3600}
	if (type== "days" ){num=num*86400}
	ret=log10(num)
	return (ret)
}

time_norm_add = function(value,type,value2,type2)
{
	num= as.numeric(value)
	if (type== "mins" ){num=num*60}
	if (type== "hours" ){num=num*3600}
	if (type== "days" ){num=num*86400}

	num2= as.numeric(value2)
	if (type2== "mins" ){num=num*60}
	if (type2== "hours" ){num=num*3600}
	if (type2== "days" ){num=num*86400}

	ret=log10(num+num2)
	return (ret)
}


steinertestplot_mul_dens  = function(test_folder , outputname , incldue_exact_sol,experiments= NULL, listofterminaltest = NULL ,repetition= NULL)
{
	  incldue_exact_sol = TRUE
	  if(is.null(repetition)){
	  	listofterminaltest=c(5,8,15)#for exact solution c(5,8,15) ,for appr :  c(5,8,20,50,100)
	  	repetition=c(0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5    ,0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5 ,   0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5 ,   0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5 , 0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5  ) # i with 50 elements
	  }
	  SPM=grw=EXA=ver=m=c()
	  #----------------------------------- 
	  # reading the data into proper time frame for box plot
	  #
	  #------------------------------------
	  time_list_1=c()
	  size_list_0=c()
	  alg_list_0=c()
	  alg_list_1=c()
	  timeplot=c()
	  sizeplot=c()
	  tlist=c()
	  slist=c()
	  counter = experiments
	  if (is.null(experiments) ){ counter = 1:length(listofterminaltest) }
		
	  pdf(outputname, width=10, height =10, paper="a4r")
	  par(mfrow=c(2,1))
	  for(j in counter)
	  {
		for(i in 1:length(repetition) )
		{
			 cat("Enum steiner tree comparison test number i,j",i,j,"\n")
			 ve= NA
			 ee= NA
			load(paste(test_folder,"/g",j,"of",i,".RData",sep=""))
			cat("num ",length(V(grw[[1]]))," of vertices of graph having exact steiner tree in",j,"of",i ,"SFX file \n")
			load(paste(test_folder,"/MEXA",j,"of",i,".RData",sep=""))
			a=listofterminaltest[j]/length(V(EXA))
			size_list_0[length( size_list_0)+1]= a
			load(paste(test_folder,"/runtimesM",j,"of",i,".RData",sep=""))
			alg_list_0 [length(alg_list_0)+1] = "Ext"
				
			 load(paste(test_folder,"/MSPM",j,"of",i,".RData",sep=""))
			 #second object in ST1 sould be taken.the first result is the original graph with colored nodes.
			 v2= length(V(SPM))
			 a2= listofterminaltest[j]/v2
			 size_list_0[length( size_list_0)+1]= a2
			alg_list_0 [length(alg_list_0)+1] = "SPM"

			 load(paste(test_folder,"/ALPH",j,"of",i,".RData",sep=""))
			 print (length(ver))
			 a3= listofterminaltest[j]/length(ver)		
			 size_list_0[length( size_list_0)+1]= a3
			 alg_list_0 [length(alg_list_0)+1] = "ASP"
		}

		sizeplot = data.frame(sizes =size_list_0 ,algorithm = alg_list_0 )
		size_list_0=c()
		alg=c()
		boxplot(sizes ~ algorithm, data = sizeplot , col = "lightgray" ,xlab="Algorithm type", ylab=" |S| / |V|")
		title("Enum Steiner Tree Density Comparison", paste("Number of terminals",listofterminaltest[j]))
	    }

		mtext("EXA: Enumerated Exact,		SPM: Enumerated Approximation PH,		ASP: All the Shortest Paths" ,4 ,line=1)
		dev.off()
}

#------------------------dense plots
#This function creates the dens plots 
steinertestplot_appr_dens  = function(test_folder , outputname , incldue_exact_sol,experiments= NULL, listofterminaltest = NULL ,repetition= NULL)
{
	  incldue_exact_sol = FALSE
	  if(is.null(repetition)){
	  	listofterminaltest=c(5,8,20,50,70)#for exact solution c(5,8,15) ,for appr :  c(5,8,20,50,70)
	  	repetition=c(0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5    ,0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5,   0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5 ,   0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5,   0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5 ) # i with 50 elements
	  }
	  #----------------------------------- 
	  # reading the data into proper time frame for box plot
	  #
	  #------------------------------------
	  ST1=ST2=ST3=ST4=c()
	  time_list_1=c()
	  size_list_0=c()
	  alg_list_0=c()
	  alg_list_1=c()
	  timeplot=c()
	  sizeplot=c()
	  tlist=c()
	  slist=c()
	  counter = experiments
	  if (is.null(experiments) ){ counter = 1:length(listofterminaltest) }
	  pdf(outputname, width=10, height =10, paper="a4r")
	  par(mfrow=c(2,1))
	  for(j in counter)
	  {
		for(i in 1:length(repetition) )
		{
			 cat("heuristic approach comparison test number i,j",i,j,"\n")
			 load(paste(test_folder,"/runtimes",j,"of",i,".RData",sep=""))

			 
			 load(paste(test_folder,"/ST2",j,"of",i,".RData",sep=""))
			 v2= length(V(ST2[[1]]))
			 e2= length(E(ST2[[1]]))
			 size_list_0[length( size_list_0)+1]=listofterminaltest[j]/v2

			 alg_list_0 [length(alg_list_0)+1] = "SP"
			 alg_list_1 [length(alg_list_1)+1] = "SP"

			 load(paste(test_folder,"/ST3",j,"of",i,".RData",sep=""))
			 v3= length(V(ST3[[1]]))
			 e3= length(E(ST3[[1]]))
			 size_list_0[length( size_list_0)+1]=listofterminaltest[j]/v3

			 alg_list_0 [length(alg_list_0)+1] = "KBH"
			 alg_list_1 [length(alg_list_1)+1] = "KBH"

			 load(paste(test_folder,"/ST4",j,"of",i,".RData",sep=""))
			 v4= length(V(ST4[[1]]))
			 e4= length(E(ST4[[1]]))
			 size_list_0[length( size_list_0)+1]=listofterminaltest[j]/v4

			 alg_list_0 [length(alg_list_0)+1] = "RSP"
			 alg_list_1 [length(alg_list_1)+1] = "RSP"
		}

		sizeplot = data.frame(sizes =size_list_0 ,algorithm = alg_list_0 )
		size_list_0=c()
		alg=c()
		boxplot(sizes ~ algorithm, data = sizeplot , col = "lightgray" ,xlab="Algorithm type", ylab="|S| / |V| ")
		title("Steiner Tree  Terminal frequency comparison", paste("Number of terminals",listofterminaltest[j]))
	    }

		mtext("PH: Shortest Path Heuristic "  ,4 ,line=0)
		mtext("KBH: Kruskal-Based Heuristic        RSP: Random Approximation New" ,4 ,line=1)
	#mtext("Graph with 418 components ,Biggest graph 13340 nodes.",2 ,line=6)

		dev.off()
}




steinertestplot_exact_dens  = function(test_folder , outputname , incldue_exact_sol,experiments= NULL, listofterminaltest = NULL ,repetition= NULL)
{
	  incldue_exact_sol = TRUE
	  if(is.null(repetition)){
	  	listofterminaltest=c(5,8,15)#for exact solution c(5,8,15) ,for appr :  c(5,8,20,50,100)
	  	repetition=c(0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5,    0.5,0.5,0.5,0.5,0.5,   0.5,0.5,0.5,0.5,0.5 ,   0.5,0.5,0.5,0.5,0.5 ,   0.5,0.5,0.5,0.5,0.5 ,   0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5 , 0.5,0.5,0.5,0.5,0.5 , 0.5,0.5,0.5,0.5,0.5 ) # i with 50 elements
	  }
	  ST1=ST2=ST3=ST4=SPM=EXA=grw=c()
	  #----------------------------------- 
	  # reading the data into proper time frame for box plot
	  #
	  #------------------------------------
	  time_list_1=c()
	  size_list_0=c()
	  alg_list_0=c()
	  alg_list_1=c()
	  timeplot=c()
	  sizeplot=c()
	  tlist=c()
	  slist=c()
	  counter = experiments
	  if (is.null(experiments) ){ counter = 1:length(listofterminaltest) }
		
	  pdf(outputname, width=10, height =10, paper="a4r")
	  par(mfrow=c(2,1))
	  for(j in counter)
	  {
		for(i in 1:length(repetition) )
		{
			 #load(paste(test_folder,"/g",j,"of",i,".RData",sep="")) 
			 #v0= length(V(grw[[1]]))
			 #e0= length(E(grw[[1]])) 
			 #size_list_0[length( size_list_0)+1]=e0 
			 #time_list_1[length( time_list_1)+1]= "NA" 
			 #alg [length(alg)+1] = "NA"
			 cat("exact approach comparison, test number i,j",i,j,"\n")
			 ve= NA
			 ee= NA		
			 load(paste(test_folder,"/runtimes",j,"of",i,".RData",sep=""))
			 #the exact solution takes so much time for nodes bigger than 23
			 #if(length(V(grw[[1]])) < 23 )  # we will not include it inside when j>2 becasue sometimes the nodes are more and some times less than 23 when j=3 
			load(paste(test_folder,"/g",j,"of",i,".RData",sep=""))
			cat("num ",length(V(grw[[1]]))," of vertices of graph having exact steiner tree in",j,"of",i ,"SFX file \n")
			load(paste(test_folder,"/EXA",j,"of",i,".RData",sep=""))
			ve <- length(V(EXA[[1]]))
			ee <- length(E(EXA[[1]]))
			size_list_0[length( size_list_0)+1]=listofterminaltest[j]/ve
			alg_list_0 [length(alg_list_0)+1] = "Ext"
				
					 
			 load(paste(test_folder,"/ST2",j,"of",i,".RData",sep=""))
			 v2= length(V(ST2[[1]]))
			 e2= length(E(ST2[[1]]))
			 size_list_0[length( size_list_0)+1]=listofterminaltest[j]/v2
			 alg_list_0 [length(alg_list_0)+1] = "SP"

			 load(paste(test_folder,"/ST3",j,"of",i,".RData",sep=""))
			 v3= length(V(ST3[[1]]))
			 e3= length(E(ST3[[1]]))
			 size_list_0[length( size_list_0)+1]=listofterminaltest[j]/v3
			 alg_list_0 [length(alg_list_0)+1] = "KBH"

			 #ST4 RSP
			 load(paste(test_folder,"/STA4",j,"of",i,".RData",sep=""))
			 load(paste(test_folder,"/runtimesST4",j,"of",i,".RData",sep=""))
			 v4= length(V(ST4[[1]]))
			 e4= length(E(ST4[[1]]))
			 size_list_0[length( size_list_0)+1]=listofterminaltest[j]/v4
			 alg_list_0 [length(alg_list_0)+1] = "RSP"

			 #SPM
			 load(paste(test_folder,"/SPM2",j,"of",i,".RData",sep=""))
			 load(paste(test_folder,"/runtimesSPM",j,"of",i,".RData",sep=""))	
			 v5= length(V(SPM[[1]]))
			 e5= length(E(SPM[[1]]))
			 size_list_0[length( size_list_0)+1]=listofterminaltest[j]/v5
			 alg_list_0 [length(alg_list_0)+1] = "SPM"
		}

		sizeplot = data.frame(sizes =size_list_0 ,algorithm = alg_list_0 )
		size_list_0=c()
		alg=c()
		boxplot(sizes ~ algorithm, data = sizeplot , col = "lightgray" ,xlab="Algorithm type", ylab="|S| / |V| ")
		title("Steiner Tree Terminal Density Comparison", paste("Number of terminals",listofterminaltest[j]))
	    }

		mtext("SP: Shortest Path Heuristic  " ,2 ,line=1)
		mtext("KBH: Kruskal-Based Heuristic        RSP: Random Approximation New" ,2 ,line=2)
		mtext("SPM: Enumeration Approximation PH      ASP: All the Shortest Paths" ,2 ,line=3)
	#mtext("Graph with 418 components ,Biggest graph 13340 nodes.",2 ,line=6)

		dev.off()
}


#---------------------------------------------------------------------------

steiner_comparison_plots = function (test_name, test_folder= NULL, outputname = NULL, listofterminaltest = NULL ,repetition= NULL)
{
#The functions in this file create the various plots used in this study

		  #first experiment comparison of heuristics with exact solution:
		  if(test_name=="exact"){
			if (is.null(test_folder)) test_folder = "steinerdata2"
			if (is.null(outputname)) outputname = "timesizeplot-Exact.pdf"
			incldue_exact_sol = TRUE
			experiments= 1:2
			steinertestplot_exact (test_folder , outputname , incldue_exact_sol, experiments, listofterminaltest ,repetition)
			}
		  if(test_name=="appr"){
			#second experimant comparison of approximation algorithms:
			if (is.null(test_folder)) test_folder = "steinerdata"
			if (is.null(outputname)) outputname = "timesizeplot-app.pdf"
			incldue_exact_sol = FALSE
			experiments= 1:5
			steinertestplot_appr (test_folder , outputname , incldue_exact_sol,experiments, listofterminaltest ,repetition)
			}
		 if(test_name=="Enum"){
			#comparison of Steiner tree enumeration  algorithms
			if (is.null(test_folder)) test_folder = "steinerdataEnum"
			if (is.null(outputname)) outputname = "timesizeplot-mul.pdf"
			incldue_exact_sol = TRUE
			experiments= 1:2
			steinertestplot_mul (test_folder , outputname , incldue_exact_sol,experiments, listofterminaltest ,repetition)
			}

				
		if(test_name=="Enum-median-venn-node-edge"){
			#comparison of Steiner tree enumeration  algorithms
			#test_folder = "steinerdataEnum"
			if (is.null(outputname)) outputname = "venn-median-mul.pdf"
			incldue_exact_sol = TRUE
			experiments= 1:2
		          steinertest_median_node_edge_mul_venn (test_folder , outputname , incldue_exact_sol,experiments, listofterminaltest ,repetition)
			}
			
		 if(test_name=="org"){
			# comparison of random trees for the exact test
			if (is.null(test_folder)) test_folder = "steinerdata2"
			if (is.null(outputname)) outputname = "sizeplot-org.pdf"
			incldue_exact_sol = TRUE
			experiments= 1:2
			steinertestplot_org (test_folder , outputname , incldue_exact_sol,experiments, listofterminaltest ,repetition)
			}

		 if(test_name=="org-dens-e"){
			#comparison of random trees for the exact test,comparing the densites
			if (is.null(test_folder)) test_folder = "steinerdata2"
			if (is.null(outputname)) outputname = "dens-org-e.pdf"
			incldue_exact_sol = TRUE
			experiments= 1:2
			steinertestplot_org_dens_e (test_folder , outputname , incldue_exact_sol,experiments, listofterminaltest ,repetition)
			}

		 if(test_name=="appr-vfreq"){
			#second experimant comparison of approximation algorithms:
			if (is.null(test_folder)) test_folder = "steinerdata"
			if (is.null(outputname)) outputname = "vfreqplot-app.pdf"
			incldue_exact_sol = FALSE
			experiments= 1:5
			steinertestplot_appr_dens (test_folder , outputname , incldue_exact_sol,experiments, listofterminaltest ,repetition)
			}

		  if(test_name=="exact-vfreq"){
			#second experimant comparison of approximation algorithms:
			if (is.null(test_folder)) test_folder = "steinerdata2"
			if (is.null(outputname)) outputname = "vfreqplot-exact.pdf"
			incldue_exact_sol = TRUE
			experiments= 1:2
			steinertestplot_exact_dens (test_folder , outputname , incldue_exact_sol,experiments, listofterminaltest ,repetition)
			}

		if(test_name=="Enum-vfreq"){
			#second experimant comparison of approximation algorithms:
			if (is.null(test_folder)) test_folder = "steinerdataEnum"
			if (is.null(outputname)) outputname = "vfreqplot-Enum.pdf"
			incldue_exact_sol = TRUE
			experiments= 1:2
			steinertestplot_mul_dens(test_folder , outputname , incldue_exact_sol,experiments, listofterminaltest ,repetition)
			}

		 if(test_name=="appr-density-e"){
			#second experimant comparison of approximation algorithms:
			if (is.null(test_folder)) test_folder = "steinerdata"
			if (is.null(outputname)) outputname = "densityplot-app-e.pdf"
			incldue_exact_sol = FALSE
			experiments= 1:5
			steinertestplot_appr_dens_e (test_folder , outputname , incldue_exact_sol,experiments, listofterminaltest ,repetition)
			}

		  if(test_name=="exact-density-e"){
			#second experimant comparison of approximation algorithms:
			if (is.null(test_folder)) test_folder = "steinerdata2"
			if (is.null(outputname)) outputname = "densityplot-exact-e.pdf"
			incldue_exact_sol = TRUE
			experiments= 1:2
			steinertestplot_exact_dens_e (test_folder , outputname , incldue_exact_sol,experiments, listofterminaltest ,repetition)
			}

		if(test_name=="Enum-density-e"){
			#second experimant comparison of  edge density in enumeration  algorithms:
			if (is.null(test_folder)) test_folder = "steinerdataEnum"
			if (is.null(outputname)) outputname = "densityplot-Enum-e.pdf"
			incldue_exact_sol = TRUE
			experiments= 1:2
			steinertestplot_mul_dens_e(test_folder , outputname , incldue_exact_sol,experiments, listofterminaltest ,repetition)
			}

}
