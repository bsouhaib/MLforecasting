
# This main file is for running multiple strategies and models

knn.rec.matrix <- knn.dir.matrix <- NULL
mlp.rec.matrix <- mlp.dir.matrix <- NULL 
lin.rec.matrix <- lin.dir.matrix <- NULL 
bst1.rec.matrix <- bst1.dir.matrix <- NULL 
bst2.rec.matrix <- bst2.dir.matrix <- NULL 


rec.matrix <- dir.matrix <- NULL
base.results <- NULL

init.set.embedding   <- set.embedding
init.embeddings.base <- embeddings.base
init.embeddings.rect <- embeddings.rect

for(i in seq_along(strategies)){
	
	istrategy <- strategies[i]
print(istrategy)
	
	if(istrategy!="MEAN" && istrategy!="NAIVE"){
		

###################
		# REC-LIN will have the same forecasts as the base forecasts of all RFY
		details <- GetDetails(ifelse(istrategy=="REC-LIN", "RFY-NONE", istrategy), H = H)
		

		learner <- switch(details$learner,
						  KNN  = knn,
						  MLP  = mlp,
						  MLPmo  = mlp,
						  BST1 = bst1,
						  BST2 = bst2,
						  LIN  = lin,
						  LINMIS = lin,
						  NONE = NULL)
		
		learner.bmodel <- lin	
		
		set.embedding <- init.set.embedding
		embeddings.base <- init.embeddings.base
		embeddings.rect <- init.embeddings.rect
	
		if(exists("DGP") && DGP == "SUNSPOT"){
	
			if(details$learner == "LINMIS"){
				set.embedding <- embeddings.base <- embeddings.rect <- c(1, 2, 3)
			}
			if(details$procedure == "RFYMIS"){
				embeddings.base <- c(1, 2, 3)
			}

		}		
#print(set.embedding)
#print(embeddings.base)
#print(embeddings.rect)
		
###################
				
		
		if(details$learner == "LIN")
		{
			rec.matrix <- lin.rec.matrix
			dir.matrix <- lin.dir.matrix
			
		}else if(details$learner == "MLP")
		{
			rec.matrix <- mlp.rec.matrix
			dir.matrix <- mlp.dir.matrix
			
		}else if(details$learner == "KNN"){
			
			rec.matrix <- knn.rec.matrix
			dir.matrix <- knn.dir.matrix
			
		}else if(details$learner == "BST1"){
			
			rec.matrix <- bst1.rec.matrix
			dir.matrix <- bst1.dir.matrix
			
		}else if(details$learner == "BST2"){
			
			rec.matrix <- bst2.rec.matrix
			dir.matrix <- bst2.dir.matrix
			
		}else{
			rec.matrix <- dir.matrix <- NULL 
		}
		
		# To avoid problems with RFYJT 
		if(grepl("RFY",details$procedure)){
			rec.matrix <- dir.matrix <- NULL
		}
		
		if(details$proc.type == "recursive"){
			
			results <- recursive(time.series = trainset, H=H, set.embedding = set.embedding, objectives = details$objectives, Xtest = Xtest, error.matrix = rec.matrix,
								 learner = learner, control = control)
			forecasts <- results$forecasts
			
			if(details$learner == "LIN" && is.null(lin.rec.matrix) ){
				lin.rec.matrix <- results$error.matrix
			}else if(details$learner == "MLP" && is.null(mlp.rec.matrix) ){
				mlp.rec.matrix <- results$error.matrix
			}else if(details$learner == "KNN" && is.null(knn.rec.matrix)){
				knn.rec.matrix <- results$error.matrix
			}else if(details$learner == "BST1" && is.null(bst1.rec.matrix)){
				bst1.rec.matrix <- results$error.matrix
			}else if(details$learner == "BST2" && is.null(bst2.rec.matrix)){
				bst2.rec.matrix <- results$error.matrix
			}
			
		}else if(details$proc.type == "direct"){
						
			results <- direct(time.series = trainset, H=H, set.embedding = set.embedding, objectives = details$objectives, Xtest = Xtest, error.matrix = dir.matrix,
							  learner = learner, control = control)
			forecasts <- results$forecasts
			
			if(details$learner == "LIN" && is.null(lin.dir.matrix) ){
				lin.dir.matrix <- results$error.matrix
			}else if(details$learner == "MLP" && is.null(mlp.dir.matrix) ){
				mlp.dir.matrix <- results$error.matrix
			}else if(details$learner == "KNN" && is.null(knn.dir.matrix)){
				knn.dir.matrix <- results$error.matrix
			}else if(details$learner == "BST1" && is.null(bst1.dir.matrix)){
				bst1.dir.matrix <- results$error.matrix
			}else if(details$learner == "BST2" && is.null(bst2.dir.matrix)){
				bst2.dir.matrix <- results$error.matrix
			}
			
		}else if(details$proc.type == "rectify"){
						
			
			results <- rectify(time.series = trainset, H=H, embeddings.base = embeddings.base, embeddings.rect = embeddings.rect, objectives = details$objectives, Xtest = Xtest, base.results = base.results, learner.bmodel = learner.bmodel,
							   learner = learner, control = control)
			
#if(is.null(base.results)){
#	base.results <- results$base.results
#}
			forecasts <- results$forecasts
		}
		
	}else if(istrategy == "MEAN"){
		
		mforecasts<-rep(mean(trainset),H)
		forecasts <- matrix(rep(mforecasts,Nbtest),nrow=Nbtest,ncol=H,byrow=T)
	}else if(istrategy == "NAIVE"){
		naiveforecasts<-rep(tail(trainset, 1),H)
                forecasts <- matrix(rep(naiveforecasts,Nbtest),nrow=Nbtest,ncol=H,byrow=T)
	}
	
	all.forecasts[[i]]$forecasts[, seq(H), id.run] <- as.matrix(forecasts)
	
	if(grepl("RFY",istrategy)){
		all.forecasts[[i]]$comp1[, seq(H), id.run] <- as.matrix(results$base.forecasts)
		all.forecasts[[i]]$comp2[, seq(H), id.run] <- as.matrix(results$rectifications)
	}
	
}
