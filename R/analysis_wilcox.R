steinertestplot_appr_dens_e_wilcox  = function(testfolder , outputname , incldue_exact_sol, experiments= NULL, listofterminaltest = NULL ,repetition= NULL)
{


  incldue_exact_sol = FALSE
 if(is.null(repetition)){
  listofterminaltest=c(5,8,15,50,70)#for exact solution c(5,8,15) ,for appr :  c(5,8,15,50,70)
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
  edge_density=c()
  tlist=c()
  slist=c()
  ST1=ST2=ST3=ST4=ver=subg=m=c()
  counter = experiments
  if (is.null(experiments) ){ counter = 1:length(listofterminaltest) }
	
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


		 load(paste(testfolder,"/STKB",j,"of",i,".RData",sep=""))
		 v3= length(V(ST3[[1]]))
		 e3= length(E(ST3[[1]]))
		 size_list_0[length( size_list_0)+1]= (2*e3)/(v3*(v3-1))
		 alg_list_0 [length(alg_list_0)+1] = "KB"


		 load(paste(testfolder,"/ST4",j,"of",i,".RData",sep=""))
		 v4= length(V(ST4[[1]]))
		 e4= length(E(ST4[[1]]))
		 size_list_0[length( size_list_0)+1]= (2*e4)/(v4*(v4-1))
		 alg_list_0 [length(alg_list_0)+1] = "RSP"


		 load(paste(testfolder,"/ALPH",j,"of",i,".RData",sep=""))
		 v5= length(ver)
		 e5= m
		 size_list_0[length( size_list_0)+1]= (2*e5)/(v5*(v5-1))
		 alg_list_0 [length(alg_list_0)+1] = "ASP"

	}

	edge_density = pairwise.wilcox.test(size_list_0, alg_list_0, p.adjust.method="holm", paired=T)
	save(  edge_density , file=paste("wilcox_app_edge_density_",listofterminaltest[j],"_terminals_.RData"))
		edge_density=c()
		size_list_0=c()
		alg_list_0=c()
		alg_list_1=c()  	
		alg=c()
	}

}



steinertestplot_exact_sp_wilcox  = function(test_folder , outputname , incldue_exact_sol,experiments= NULL, listofterminaltest = NULL ,repetition= NULL)
{
	  incldue_exact_sol = TRUE

	  if(is.null(listofterminaltest)){
		listofterminaltest=c(5,8,15)
	  }	

	  if(is.null(repetition)){
	  	repetition=c(0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5,    0.5,0.5,0.5,0.5,0.5,   0.5,0.5,0.5,0.5,0.5 ,   0.5,0.5,0.5,0.5,0.5 ,   0.5,0.5,0.5,0.5,0.5 ,   0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5 , 0.5,0.5,0.5,0.5,0.5 , 0.5,0.5,0.5,0.5,0.5 ) # i with 50 elements
	  }
	  ST1=ST2=ST3=ST4=SPM=EXA=STX=runtimes=runtimes4=runtimesM=grw=c()
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
	  wilcox_time =c()
	  wilcox_edge=c()
	  counter = experiments
	  if (is.null(experiments) ){ counter = 1:length(listofterminaltest) }

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
			
			time_list_1[length( time_list_1)+1]=time_norm(runtimes[1],runtimes[3] ) 
						  
			alg_list_1 [length(alg_list_1)+1] = "Exact"
			alg_list_0 [length(alg_list_0)+1] = "Exact"
				
				 
			 load(paste(test_folder,"/ST2",j,"of",i,".RData",sep=""))
			 v2= length(V(ST2[[1]]))
			 e2= length(E(ST2[[1]]))
			 size_list_0[length( size_list_0)+1]=e2 
			 time_list_1[length( time_list_1)+1]=time_norm(runtimes[2],runtimes[4])
			 alg_list_0 [length(alg_list_0)+1] = "SP"
			 alg_list_1 [length(alg_list_1)+1] = "SP"

		}
		
		wilcox_edge = pairwise.wilcox.test(size_list_0, alg_list_0, p.adjust.method="holm", paired=T)
		wilcox_time = pairwise.wilcox.test(time_list_1, alg_list_0, p.adjust.method="holm", paired=T)
	
		save(  wilcox_edge , file=paste("wilcox_exact_edge_",listofterminaltest[j],"_terminals_.RData"))
		save(  wilcox_time, file=paste("wilcox_exact_time_",listofterminaltest[j],"_terminals_.RData") )

		wilcox_time =c()
		wilcox_edge=c()
		
		time_list_1=c()
		size_list_0=c()
		alg_list_0=c()
		alg_list_1=c()  	
		alg=c()

	    }
}




steinertestplot_appr_wilcox  = function(test_folder , outputname , incldue_exact_sol,experiments= NULL, listofterminaltest = NULL ,repetition= NULL)
{
	  incldue_exact_sol = FALSE
	  if(is.null(repetition)){
	  	listofterminaltest=c(5,8,15,50,70)#for exact solution c(5,8,15) ,for appr :  c(5,8,20,50,70)
	  	repetition=c(0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5    ,0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5,   0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5 ,   0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5,   0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5 ) # i with 50 elements
	  }
	  #----------------------------------- 
	  # reading the data into proper time frame for box plot
	  #
	  #------------------------------------
	  ST1=ST2=ST3=ST4=runtimes=subg=m=c()
	  time_list_1=c()
	  size_list_0=c()
	  alg_list_0=c()
	  alg_list_1=c()
	  timeplot=c()
	  sizeplot=c()
	  wilcox_time =c()
	 wilcox_edge=c()
	 terminals=c()
	 #wi=c()
	
	  tlist=c()
	  slist=c()
	  counter = experiments
	  if (is.null(experiments) ){ counter = 1:length(listofterminaltest) }
		
	  for(j in counter)
	  {
		for(i in 1:length(repetition) )
		{
			 cat("heuristic approach comparison test number i,j",i,j,"\n")
			 load(paste(test_folder,"/runtimes",j,"of",i,".RData",sep=""))

		
			 load(paste(test_folder,"/ST2",j,"of",i,".RData",sep=""))
			 v2= length(V(ST2[[1]]))
			 e2= length(E(ST2[[1]]))
			 size_list_0[length( size_list_0)+1]=log10(e2)
			 time_list_1[length( time_list_1)+1]=time_norm(runtimes[1],runtimes[3])
			 alg_list_0 [length(alg_list_0)+1] = "SP"
			 alg_list_1 [length(alg_list_1)+1] = "SP"

			 load(paste(test_folder,"/STKB",j,"of",i,".RData",sep=""))
 			 load(paste(test_folder,"/runtimesKB",j,"of",i,".RData",sep=""))
			 v3= length(V(ST3[[1]]))
			 e3= length(E(ST3[[1]]))
			 size_list_0[length( size_list_0)+1]=log10(e3)
			 time_list_1[length( time_list_1)+1]=time_norm(runtimes[1], runtimes[2])
			 alg_list_0 [length(alg_list_0)+1] = "KB"
			 alg_list_1 [length(alg_list_1)+1] = "KB"

			 load(paste(test_folder,"/ST4",j,"of",i,".RData",sep=""))
			 load(paste(test_folder,"/runtimes",j,"of",i,".RData",sep=""))	 
			 v3= length(V(ST4[[1]]))
			 e3= length(E(ST4[[1]]))
			 size_list_0[length( size_list_0)+1]=log10(e3)
			 time_list_1[length( time_list_1)+1]=time_norm(runtimes[2] ,runtimes[4])
			 alg_list_0 [length(alg_list_0)+1] = "RSP"
			 alg_list_1 [length(alg_list_1)+1] = "RSP"

	 		 load(paste(test_folder,"/ALPH",j,"of",i,".RData",sep=""))
			 load(paste(test_folder,"/runtimesAL",j,"of",i,".RData",sep=""))
			 size_list_0[length( size_list_0)+1]= log10(length(E(subg)))
			 time_list_1[length( time_list_1)+1]=time_norm(runtimes[1] ,runtimes[2])
			 alg_list_0 [length(alg_list_0)+1] = "ASP"
			 alg_list_1 [length(alg_list_1)+1] = "ASP"

		}
	wilcox_edge = pairwise.wilcox.test(size_list_0, alg_list_0, p.adjust.method="holm", paired=T)
	wilcox_time = pairwise.wilcox.test(time_list_1, alg_list_0, p.adjust.method="holm", paired=T)
	
	save(  wilcox_edge , file=paste("wilcox_app_edge_",listofterminaltest[j],"_terminals_.RData"))
	save(  wilcox_time, file=paste("wilcox_app_time_",listofterminaltest[j],"_terminals_.RData") )
	#wi$wilcox_edge [length(wi$wilcox_edge)+1]=wilcox_edge
	#wi$wilcox_time [length(wi$wilcox_time)+1]=wilcox_time
	#wi$terminals[length(wi$terminals)+1]=j

		wilcox_time =c()
		wilcox_edge=c()
		terminals=c()
		
		time_list_1=c()
		size_list_0=c()
		alg_list_0=c()
		alg_list_1=c()  	
		alg=c()
    }
}





steinertestplot_appr_dens_wilcox  = function(test_folder , outputname , incldue_exact_sol,experiments= NULL, listofterminaltest = NULL ,repetition= NULL)
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
	  ST1=ST2=ST3=ST4=ver=subg=c()
	  time_list_1=c()
	  size_list_0=c()
	  alg_list_0=c()
	  alg_list_1=c()
	  timeplot=c()
	  sizeplot=c()
	  frequency =c()	  
	  tlist=c()
	  slist=c()
	  counter = experiments
	  if (is.null(experiments) ){ counter = 1:length(listofterminaltest) }
	  for(j in counter)
	  {
		for(i in 1:length(repetition) )
		{
			 cat("heuristic approach comparison test number i,j",i,j,"\n")
			 load(paste(test_folder,"/runtimes",j,"of",i,".RData",sep=""))

			 load(paste(test_folder,"/ST2",j,"of",i,".RData",sep=""))
			 v2= length(V(ST2[[1]]))
			 e2= length(E(ST2[[1]]))
			 size_list_0[length( size_list_0)+1]=length(V(ST2[[1]])[V(ST2[[1]])$color=="red" ])/v2
			 alg_list_0 [length(alg_list_0)+1] = "SP"

			 load(paste(test_folder,"/STKB",j,"of",i,".RData",sep=""))
			 v3= length(V(ST3[[1]]))
			 e3= length(E(ST3[[1]]))
			 size_list_0[length( size_list_0)+1]=length(V(ST3[[1]])[V(ST3[[1]])$color=="red" ])/v3
			 alg_list_0 [length(alg_list_0)+1] = "KB"

			 load(paste(test_folder,"/ST4",j,"of",i,".RData",sep=""))
			 v4= length(V(ST4[[1]]))
			 e4= length(E(ST4[[1]]))
			 size_list_0[length( size_list_0)+1]=length(V(ST4[[1]])[V(ST4[[1]])$color=="red" ])/v4

			 alg_list_0 [length(alg_list_0)+1] = "RSP"

			 load(paste(test_folder,"/ALPH",j,"of",i,".RData",sep=""))
			 v5= length(unique(unlist(V(subg))))
			 size_list_0[length( size_list_0)+1]= length(V(subg)[V(subg)$color=="red" ])/v5
			 alg_list_0 [length(alg_list_0)+1] = "ASP"

		}
		
	frequency = pairwise.wilcox.test(size_list_0, alg_list_0, p.adjust.method="holm", paired=T)
	save(  frequency , file=paste("wilcox_app_frequency_",listofterminaltest[j],"_terminals_.RData"))
		frequency=c()		
		size_list_0=c()
		alg=c()

	    }

}



steinertestplot_exact_sp_e_dens_wilcox  = function(test_folder , outputname , incldue_exact_sol,experiments= NULL, listofterminaltest = NULL ,repetition= NULL)
{
	  incldue_exact_sol = TRUE
	  if(is.null(repetition)){
	  	listofterminaltest=c(5,8,15)#for exact solution c(5,8,15) ,for appr :  c(5,8,20,50,100)
	  	repetition=c(0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5,    0.5,0.5,0.5,0.5,0.5,   0.5,0.5,0.5,0.5,0.5 ,   0.5,0.5,0.5,0.5,0.5 ,   0.5,0.5,0.5,0.5,0.5 ,   0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5 , 0.5,0.5,0.5,0.5,0.5 , 0.5,0.5,0.5,0.5,0.5 ) # i with 50 elements
	  }
	  ST1=ST2=ST3=ST4=SPM=EXA=STX=runtimes=runtimes4=runtimesM=grw=c()
	  #----------------------------------- 
	  # reading the data into proper time frame for box plot
	  #
	  #------------------------------------
	  time_list_1=c()
	  size_list_0=c()
	  alg_list_0=c()
	  alg_list_1=c()
	  tlist=c()
	  slist=c()
	  edge_density=c()
	  counter = experiments
	  if (is.null(experiments) ){ counter = 1:length(listofterminaltest) }

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
			size_list_0[length( size_list_0)+1]= (2*ee)/(ve*(ve-1))
			alg_list_0 [length(alg_list_0)+1] = "Exact"
				 
			 load(paste(test_folder,"/ST2",j,"of",i,".RData",sep=""))
			 v2= length(V(ST2[[1]]))
			 e2= length(E(ST2[[1]]))
			 size_list_0[length( size_list_0)+1]=(2*e2)/(v2*(v2-1))
			 alg_list_0 [length(alg_list_0)+1] = "SP"
		
		}

	edge_density = pairwise.wilcox.test(size_list_0, alg_list_0, p.adjust.method="holm", paired=T)
	save(  edge_density , file=paste("wilcox_exact_sp_e_density",listofterminaltest[j],"_terminals_.RData"))
	
		edge_density=c()
		size_list_0=c()
		alg_list_0=c()
		alg=c()

	    }

}

steinertestplot_mul_wilcox  = function(test_folder , outputname , incldue_exact_sol,experiments= NULL, listofterminaltest = NULL ,repetition= NULL)
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
	  wilcox_edge=c()
	  wilcox_time=c()
	  tlist=c()
	  slist=c()
	  EXA=subg=SPM=m=grw=runtimes=c()
	  counter = experiments
	  if (is.null(experiments) ){ counter = 1:length(listofterminaltest) }

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
#EXA=STX
			ee <- length(E(EXA))
			size_list_0[length( size_list_0)+1]=ee
			load(paste(test_folder,"/runtimesM",j,"of",i,".RData",sep=""))
			time_list_1[length( time_list_1)+1]=time_norm(runtimes[1] ,runtimes[2])	  
			alg_list_1 [length(alg_list_1)+1] = "Ext"
			alg_list_0 [length(alg_list_0)+1] = "Ext"
				
			 load(paste(test_folder,"/MSPM",j,"of",i,".RData",sep=""))
			 #second object in ST1 sould be taken.the first result is the original graph with colored nodes.
#SPM=STM
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

		wilcox_edge = pairwise.wilcox.test(size_list_0, alg_list_0, p.adjust.method="holm", paired=T)
		wilcox_time = pairwise.wilcox.test(time_list_1, alg_list_0, p.adjust.method="holm", paired=T)
	
		save(  wilcox_edge , file=paste("wilcox_enum_edge_",listofterminaltest[j],"_terminals_.RData"))
		save(  wilcox_time, file=paste("wilcox_enum_time_",listofterminaltest[j],"_terminals_.RData") )
	
		wilcox_edge=c()
		wilcox_time=c()
		size_list_0=c()
		alg_list_0=c()
		alg=c()
	    }

}


steinertestplot_exact_freq_sp_wilcox  = function(test_folder , outputname , incldue_exact_sol,experiments= NULL, listofterminaltest = NULL ,repetition= NULL)
{
	  incldue_exact_sol = TRUE
	  if(is.null(repetition)){
	  	listofterminaltest=c(5,8,15)#for exact solution c(5,8,15) ,for appr :  c(5,8,20,50,100)
	  	repetition=c(0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5,    0.5,0.5,0.5,0.5,0.5,   0.5,0.5,0.5,0.5,0.5 ,   0.5,0.5,0.5,0.5,0.5 ,   0.5,0.5,0.5,0.5,0.5 ,   0.5,0.5,0.5,0.5,0.5   ,0.5,0.5,0.5,0.5,0.5 , 0.5,0.5,0.5,0.5,0.5 , 0.5,0.5,0.5,0.5,0.5 ) # i with 50 elements
	  }
	  ST1=ST2=ST3=ST4=SPM=EXA=STX=grw=c()
	  #----------------------------------- 
	  # reading the data into proper time frame for box plot
	  #
	  #------------------------------------
	  size_list_0=c()
	  alg_list_0=c()
	  alg_list_1=c()
	  frequency=c()
	  sizeplot=c()
	  tlist=c()
	  slist=c()
	  counter = experiments
	  if (is.null(experiments) ){ counter = 1:length(listofterminaltest) }

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
			 #the exact solution takes so much time for nodes bigger than 23
			 #if(length(V(grw[[1]])) < 23 )  # we will not include it inside when j>2 becasue sometimes the nodes are more and some times less than 23 when j=3 
			load(paste(test_folder,"/g",j,"of",i,".RData",sep=""))
			cat("num ",length(V(grw[[1]]))," of vertices of graph having exact steiner tree in",j,"of",i ,"SFX file \n")
			load(paste(test_folder,"/EXA",j,"of",i,".RData",sep=""))
			ve <- length(V(EXA[[1]]))
			ee <- length(E(EXA[[1]]))
			size_list_0[length( size_list_0)+1]=listofterminaltest[j]/ve
			alg_list_0 [length(alg_list_0)+1] = "Exact"		
					 
			 load(paste(test_folder,"/ST2",j,"of",i,".RData",sep=""))
			 v2= length(V(ST2[[1]]))
			 e2= length(E(ST2[[1]]))
			 size_list_0[length( size_list_0)+1]=listofterminaltest[j]/v2
			 alg_list_0 [length(alg_list_0)+1] = "SP"
		}

	        frequency = pairwise.wilcox.test(size_list_0, alg_list_0, p.adjust.method="holm", paired=T)
		save(  frequency , file=paste("wilcox_exact_sp_freq",listofterminaltest[j],"_terminals_.RData"))
		frequency=c()
		size_list_0=c()
		alg_list_0=c()
		alg=c()
	
	    }

}

steinertestplot_mul_freq_wilcox  = function(test_folder , outputname , incldue_exact_sol,experiments= NULL, listofterminaltest = NULL ,repetition= NULL)
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
	  frequency=c()
	  tlist=c()
	  slist=c()
	  counter = experiments
	  if (is.null(experiments) ){ counter = 1:length(listofterminaltest) }
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
#EXA=STX
			a=listofterminaltest[j]/length(V(EXA))
			size_list_0[length( size_list_0)+1]= a
			load(paste(test_folder,"/runtimesM",j,"of",i,".RData",sep=""))
			alg_list_0 [length(alg_list_0)+1] = "Ext"
				
			 load(paste(test_folder,"/MSPM",j,"of",i,".RData",sep=""))
#SPM=STM

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

		frequency = pairwise.wilcox.test(size_list_0, alg_list_0, p.adjust.method="holm", paired=T)
		save(  frequency , file=paste("wilcox_enum_frequency_",listofterminaltest[j],"_terminals_.RData"))
		frequency=c()
		size_list_0=c()
		alg_list_0=c()
		alg=c()
	    }

}



steinertestplot_mul_dens_e_wilcox  = function(testfolder , outputname , incldue_exact_sol,experiments= NULL , listofterminaltest = NULL ,repetition= NULL)
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
  edge_density=c()
  counter = experiments
  if (is.null(experiments) ){ counter = 1:length(listofterminaltest) }
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
#EXA=STX
		ve=length(V(EXA))
		ee=length(E(EXA))
		size_list_0[length( size_list_0)+1]= (2*ee)/(ve*(ve-1))
		load(paste(testfolder,"/runtimesM",j,"of",i,".RData",sep=""))
		alg_list_0 [length(alg_list_0)+1] = "Ext"
			
		 load(paste(testfolder,"/MSPM",j,"of",i,".RData",sep=""))
		 #second object in ST1 sould be taken.the first result is the original graph with colored nodes.
#SPM=STM
		 v2= length(V(SPM))
		 e2= length(E(SPM))
		 size_list_0[length( size_list_0)+1]= (2*e2)/(v2*(v2-1))
		alg_list_0 [length(alg_list_0)+1] = "SPM"

		 load(paste(testfolder,"/ALPH",j,"of",i,".RData",sep=""))
		 v3= length(ver)
		 e3= m
		 size_list_0[length( size_list_0)+1]= (2*e3)/(v3*(v3-1))
		 alg_list_0 [length(alg_list_0)+1] = "ASP"
	}

	        edge_density = pairwise.wilcox.test(size_list_0, alg_list_0, p.adjust.method="holm", paired=T)
		save(  edge_density , file=paste("wilcox_enum_edge_density",listofterminaltest[j],"_terminals_.RData"))
		edge_density=c()
		size_list_0=c()
		alg_list_0=c()
		alg=c()
    }

}



steiner_comparison_wilcox = function (test_name, test_folder= NULL, outputname = NULL, listofterminaltest = NULL ,repetition= NULL)
{
#The functions in this file create the various plots used in this study

			
		if(test_name=="exact-sp"){
			if (is.null(test_folder)) test_folder = "steinerdata2"
			if (is.null(outputname)) outputname = "timesizeplot-Exact-sp.pdf"
			incldue_exact_sol = TRUE
			experiments= 1:2
			steinertestplot_exact_sp_wilcox(test_folder , outputname , incldue_exact_sol, experiments, listofterminaltest , repetition)
			}
			
		 if(test_name=="exact-sp-edge-dens"){
			if (is.null(test_folder)) test_folder = "steinerdata2"
			if (is.null(outputname)) outputname = "timesizeplot-Exact-sp-edge-dens.pdf"
			incldue_exact_sol = TRUE
			experiments= 1:2
			steinertestplot_exact_sp_e_dens_wilcox(test_folder , outputname , incldue_exact_sol, experiments, listofterminaltest , repetition)
			}	
		
		 if(test_name=="appr"){
			#second experimant comparison of approximation algorithms:
			if (is.null(test_folder)) test_folder = "steinerdata"
			if (is.null(outputname)) outputname = "wilcoxapp_edges.Rdata" 
			incldue_exact_sol = FALSE
			experiments= 1:5
			steinertestplot_appr_wilcox (test_folder , outputname , incldue_exact_sol,experiments, listofterminaltest , repetition)
			}					 
			
		 if(test_name=="Enum"){
			#comparison of Steiner tree enumeration  algorithms
			if (is.null(test_folder)) test_folder = "steinerdataEnum"
			if (is.null(outputname)) outputname = "timesizeplot-mul.pdf"
			incldue_exact_sol = TRUE
			experiments= 1:2
			steinertestplot_mul_wilcox (test_folder , outputname , incldue_exact_sol,experiments, listofterminaltest , repetition)
			}

		if(test_name=="appr-vfreq"){
			#second experimant comparison of approximation algorithms:
			if (is.null(test_folder)) test_folder = "steinerdata"
			if (is.null(outputname)) outputname = "vfreqplot-app.pdf"
			incldue_exact_sol = FALSE
			experiments= 1:5
			steinertestplot_appr_dens_wilcox (test_folder , outputname , incldue_exact_sol,experiments, listofterminaltest , repetition)
			}

		  if(test_name=="exact-vfreq-sp"){
			#second experimant comparison of approximation algorithms:
			if (is.null(test_folder)) test_folder = "steinerdata2"
			if (is.null(outputname)) outputname = "vfreqplot-exact-sp.pdf"
			incldue_exact_sol = TRUE
			experiments= 1:2

			steinertestplot_exact_freq_sp_wilcox (test_folder , outputname , incldue_exact_sol,experiments, listofterminaltest , repetition)
			}

		if(test_name=="Enum-vfreq"){
			#second experimant comparison of approximation algorithms:
			if (is.null(test_folder)) test_folder = "steinerdataEnum"
			if (is.null(outputname)) outputname = "vfreqplot-Enum.pdf"
			incldue_exact_sol = TRUE
			experiments= 1:2
			steinertestplot_mul_freq_wilcox(test_folder , outputname , incldue_exact_sol,experiments, listofterminaltest , repetition)
			}

		 if(test_name=="appr-density-e"){
			#second experimant comparison of approximation algorithms:
			if (is.null(test_folder)) test_folder = "steinerdata"
			if (is.null(outputname)) outputname = "densityplot-app-e.pdf"
			incldue_exact_sol = FALSE
			experiments= 1:5
			steinertestplot_appr_dens_e_wilcox(test_folder , outputname , incldue_exact_sol,experiments, listofterminaltest , repetition)
			}


		if(test_name=="Enum-density-e"){
			#second experimant comparison of  edge density in enumeration  algorithms:
			if (is.null(test_folder)) test_folder = "steinerdataEnum"
			if (is.null(outputname)) outputname = "densityplot-Enum-e.pdf"
			incldue_exact_sol = TRUE
			experiments= 1:2
			steinertestplot_mul_dens_e_wilcox(test_folder , outputname , incldue_exact_sol,experiments, listofterminaltest , repetition)
			}

}
