ErrorMatrix <- function(obj, horizon){

	
	n.fold <- obj$control$validation$n.fold
	
	if(obj$learner$name == "KNN"){
		error.matrix <- array(NA, c(obj$n.init.obs, n.fold, obj$H, obj$n.init.obs, obj$n.embedding))
	}else if(obj$learner$name == "MLP"){
		error.matrix <- array(NA, c(obj$n.init.obs, n.fold, obj$H, length(obj$learner$set.hidden), length(obj$learner$set.decay), obj$n.embedding))
	}else if(obj$learner$name == "BST"){
		error.matrix <- array(NA, c(obj$n.init.obs, n.fold, obj$H, obj$learner$max.mstop+2))
	}
	
	for(h in seq(horizon)){
		
		y.variable <- paste("Y", h, sep="")
		
		id.complete <- which(complete.cases(obj$data.set[ , c(y.variable,obj$all.x.variables), drop=F]))
		complete.data <- obj$data.set[id.complete, c(y.variable,obj$all.x.variables), drop=F]
		n.obs <- nrow(complete.data)
		
		validation.matrix <- ValidationMatrix(obj$control$validation, n.obs)
		
		all.embeddings <- obj$set.embedding
		if(obj$learner$name == "BST"){
			
			all.embeddings <- max(obj$set.embedding)

		}
		
		for(embedding in all.embeddings){
			
			x.variables <- paste("X" , seq(embedding), sep="")
			
			if(obj$procedure$type=="recursive"){
				
				if(obj$learner$name=="KNN"){
					formula.knn <- paste("Y1", paste(x.variables,collapse="+"), sep="~")
				}else if(obj$learner$name=="BST"){
					formula.bst <- MakeFormula(y.variable = "Y1", obj$all.x.variables, interactions = obj$learner$interactions)
				}
			}else{
				
				if(obj$learner$name=="KNN"){
					formula.knn <- paste(y.variable, paste(x.variables,collapse="+"), sep="~")
				}else if(obj$learner$name=="BST"){
					formula.bst <- MakeFormula(y.variable = y.variable, obj$all.x.variables, interactions = obj$learner$interactions)
				}
			}


			
			for(i.fold in seq(ncol(validation.matrix))){
				
				validation.vector <- validation.matrix[,i.fold]
				
				index.train <- which(validation.vector==1) 
				index.valid <- which(validation.vector==0) 
				
				validation.y <- obj$data.set[id.complete[index.valid], y.variable]

			
				if(obj$procedure$type=="recursive"){
					
					######### CODE RECURSIVE ##################
					training.data <- obj$data.set[id.complete[index.train], c("Y1", x.variables), drop=F]
					validation.points <- obj$data.set[id.complete[index.valid], x.variables, drop=F]
					
					
					if(obj$learner$name=="KNN"){

						max.k <- nrow(training.data) -1
						set.k <- seq(1,max.k,by=obj$learner$inc)
										
						for(current.k in set.k){
							
							validation.inputs <- validation.points
								
							for(iter in seq(h)){
														
								predictions <- mykknn(as.formula(formula.knn), training.data, validation.inputs,k=current.k)[current.k,]
								stopifnot(all(!is.na(predictions)))
								
								validation.inputs <- data.frame(predictions,validation.inputs)
								validation.inputs <- validation.inputs[,-ncol(validation.inputs),drop=F]
								colnames(validation.inputs) <- x.variables
							}
							
							# errors is a numeric vector
							errors <- (predictions - validation.y)^2
							error.matrix[id.complete[index.valid] , i.fold, h, current.k, which(embedding==obj$set.embedding)] <- errors
							
						}# END FOR 
					
					}else if(obj$learner$name=="MLP"){
					
						for(hidden in obj$learner$set.hidden){
							for(decay in obj$learner$set.decay){
																
								all.models<-lapply(seq(obj$learner$n.runs),function(i){
													#skip <- ifelse(hidden==0,TRUE,FALSE)
													#model <- nnet(y = training.data["Y1"],x = training.data[x.variables],size = hidden,decay = decay,maxit=obj$learner$maxiter,linout=T,trace=F,skip=skip)
												   model <- mynnet(y = training.data["Y1"],x = training.data[x.variables],size = hidden,decay = decay, config = obj$learner)
												   model})
								
								validation.inputs <- validation.points
								
								for(iter in seq(h)){
																		
									all.runs <- lapply(all.models,predict, validation.inputs)
									predictions <- Reduce("+",all.runs)/obj$learner$n.runs
									colnames(predictions) <- NULL
									
									
									stopifnot(all(!is.na(predictions)))
									
									validation.inputs <- data.frame(predictions,validation.inputs)
									validation.inputs <- validation.inputs[,-ncol(validation.inputs),drop=F]
									colnames(validation.inputs) <- x.variables
								}
								
								# errors is a numeric vector
								errors <- (predictions - validation.y)^2
								error.matrix[id.complete[index.valid], i.fold, h, which(hidden==obj$learner$set.hidden),which(decay==obj$learner$set.decay),which(embedding==obj$set.embedding)] <- errors

								
							}
						}
								
					}else if(obj$learner$name=="BST"){
					
						model.bst <- gamboost(as.formula(formula.bst), data = training.data, control = boost_control(mstop = obj$learner$max.mstop,nu = obj$learner$nu))
						
						for(istop in seq(obj$learner$max.mstop)){
							
							model.bst[istop]
							
							validation.inputs <- validation.points
							
							for(iter in seq(h)){
															
								# All iterations > 0
								predictions <- predict(model.bst, validation.inputs)
								stopifnot(all(!is.na(predictions)))

								
								validation.inputs <- data.frame(predictions,validation.inputs)
								validation.inputs <- validation.inputs[,-ncol(validation.inputs),drop=F]
								colnames(validation.inputs) <- x.variables
								
							}
							error.matrix[id.complete[index.valid], i.fold, h, 2 + istop] <- (predictions - validation.y)^2 

						}
						
						# Real zero iterations 
						error.matrix[id.complete[index.valid],i.fold, h, 1] <- (validation.y)^2
						
						# Zero iterations (mean forecasts)
						error.matrix[id.complete[index.valid],i.fold, h, 2] <- (model.bst$offset - validation.y)^2
					}

					
					##########################################
					
				}else if(obj$procedure$type=="direct"){
					
					######### CODE DIRECT ##################
					training.data	<- complete.data[index.train,,drop=F]
					validation.data <- complete.data[index.valid,,drop=F]
					
					
					##### KNN model 
					if(obj$learner$name=="KNN"){

						max.k <- nrow(training.data) -1
						
						predictions <- mykknn(as.formula(formula.knn), training.data, validation.data, inc = obj$learner$inc, k = max.k, allk = TRUE)
						
						# We can have some NA if obj$learner$inc!=1
						#stopifnot(all(!is.na(predictions)))
											
						# predictions : [kmax x N ](not as in recursive.matrix)
						# I have transposed predictions to compute errors correctly
						# I did not transpose again because error.matrix is [N x kmax]
						errors <- ( t(predictions)-validation.data[ , y.variable] )^2
						error.matrix[id.complete[index.valid], i.fold, h, seq(max.k), which(embedding==obj$set.embedding)] <- errors	
					
					}else if(obj$learner$name=="MLP"){
					
						for(hidden in obj$learner$set.hidden){
							for(decay in obj$learner$set.decay){
								
								
								all.runs<-lapply(seq(obj$learner$n.runs),function(i){
#												 skip <- ifelse(hidden==0,TRUE,FALSE)
#												 model <- nnet(y=training.data[y.variable], x=training.data[x.variables],size=hidden,decay=decay,maxit=obj$learner$maxiter,linout=T,trace=F,skip=skip)
											   model <- mynnet(y=training.data[y.variable], x=training.data[x.variables],size=hidden,decay=decay, config = obj$learner)
												 predictions <- predict(model,validation.data[x.variables])
												 predictions})							
								
								predictions <- Reduce("+",all.runs)/obj$learner$n.runs
								
								errors <- (predictions - validation.data[y.variable])^2
								errors <- apply(errors,1,mean)
								stopifnot(all(!is.na(errors)))
								
								error.matrix[id.complete[index.valid], i.fold, h, which(hidden==obj$learner$set.hidden),which(decay==obj$learner$set.decay),which(embedding==obj$set.embedding)] <- errors
								
#browser()
							}
						}
						
					}else if(obj$learner$name=="BST"){
						
						model.bst <- gamboost(as.formula(formula.bst), data = training.data, control = boost_control(mstop = obj$learner$max.mstop,nu = obj$learner$nu))
						
						# All iterations > 0
						predictions <- predict(model.bst, validation.data, aggregate="cumsum")
						stopifnot(all(!is.na(predictions)))
						error.matrix[id.complete[index.valid], i.fold, h, seq(3,2+obj$learner$max.mstop)] <- (predictions - validation.y)^2 

						
						# Real zero iterations 
						error.matrix[id.complete[index.valid],i.fold, h, 1] <- (validation.y)^2
						
						# Zero iterations (mean forecasts)
						error.matrix[id.complete[index.valid],i.fold, h, 2] <- (model.bst$offset - validation.y)^2
					
					}
					
					##########################################
				

				}else{stop("ERROR IN error.matrix !");}

			}
		}
	}
	error.matrix
}

autoregression <- function(obj, horizons, error.matrix)
{	
	n.fold <- obj$control$validation$n.fold
	y.variables <- paste("Y",horizons,sep="")
	all.variables <- c(y.variables, obj$all.x.variables)

 	
	learner <- obj$learner
	
	if(learner$name=="MLPmo"){
		error.matrix <- array(NA, c(obj$n.init.obs, n.fold, length(learner$set.hidden), length(learner$set.decay), obj$n.embedding))
	}
	
	
	id.complete <- which(complete.cases(obj$data.set[all.variables]))
	complete.data <- obj$data.set[id.complete, all.variables, drop=F]
	n.obs <- nrow(complete.data)
	
	validation.matrix <- ValidationMatrix(obj$control$validation, n.obs)

	
	if(learner$name=="MLPmo" || learner$name=="SVM"){
		
		for(embedding in obj$set.embedding){
			
			x.variables <- paste("X" , seq(embedding), sep="")
			
			for(i.fold in seq(ncol(validation.matrix))){	
				
				validation.vector <- validation.matrix[,i.fold]
				
				index.train <- which(validation.vector==1) 
				index.valid <- which(validation.vector==0) 
				
				training.data	<- complete.data[index.train,,drop=F]
				validation.data <- complete.data[index.valid,,drop=F]
				
				if(learner$name=="MLPmo"){
					
					for(hidden in learner$set.hidden){
						for(decay in learner$set.decay){
							
														
							all.runs<-lapply(seq(learner$n.runs),function(i){
											 skip <- ifelse(hidden==0,TRUE,FALSE)
											 model <- nnet(y=training.data[y.variables],x=training.data[x.variables],size=hidden,decay=decay,maxit=learner$maxiter,linout=T,trace=F,skip=skip)
											 predictions <- predict(model,validation.data[x.variables])
											 predictions})							

							predictions <- Reduce("+",all.runs)/learner$n.runs
														
							errors <- (predictions - validation.data[y.variables])^2
							errors <- apply(errors,1,mean)
							stopifnot(all(!is.na(errors)))

							error.matrix[id.complete[index.valid], i.fold, which(hidden==learner$set.hidden),which(decay==learner$set.decay),which(embedding==obj$set.embedding)] <- errors
						}
					}
				}else if(learner$name=="SVM"){
					
					############## Coming Soon ;-) ###############
				
				}
			}
		} # end embedding
		
	}
		
############ Selecting best models ##################
	best.model <- list()
	

	
	if(learner$name=="KNN"){
				
# error.matrix is  array(NA, c(obj$n.init.obs, n.fold, obj$H, obj$n.init.obs, obj$n.embedding))
		
		err.matrix <- error.matrix[,,horizons,,,drop=F]
		
		# Average over points and folds 
		avg1.error <- apply(err.matrix,c(3,4,5),mean,na.rm=T)

		# Average over the horizon WITHOUT removing NA
		# This means that if there is one NA in one of the output, the whole point is removed
		# This is in contrast with avg.error <- apply(err.matrix, c(4,5), mean, na.rm=T)
		# We have to do that otherwise errors for different points will not be comparable 
		avg.error <- apply(avg1.error,c(2,3),mean)
				
		
		avg.error[which(is.nan(avg.error))] <- Inf
		stopifnot(any(avg.error != Inf))
		best.index <- arrayInd(which.min(avg.error),dim(avg.error))
				
		best.model$k <- best.index[1]
		best.model$embedding <- obj$set.embedding[best.index[2]]
		
		
	}else if(learner$name=="MLP"){
		

# error.matrix is array(NA, c(obj$n.init.obs, n.fold, obj$H, length(learner$set.hidden), length(learner$set.decay), obj$n.embedding))
		
		err.matrix <- error.matrix[,,horizons,,,,drop=F]
		
# Average over points and folds 
		avg1.error <- apply(err.matrix,c(3,4,5,6),mean,na.rm=T)
		
		
		avg.error <- apply(avg1.error,c(2,3,4),mean)
				
		stopifnot(any(avg.error != Inf))
		best.index <- arrayInd(which.min(avg.error),dim(avg.error))
		
		best.model$hidden <- learner$set.hidden[best.index[1]]
		best.model$decay <- learner$set.decay[best.index[2]]
		best.model$embedding <- obj$set.embedding[best.index[3]]
			
		
	}else if(learner$name=="MLPmo"){

# error.matrix is array(NA, c(obj$n.init.obs, n.fold, length(learner$set.hidden), length(learner$set.decay), obj$n.embedding))
		
		# Note that the average over the horizon has already been performed above
		
		# OLD CODE
		# avg.error <- apply(error.matrix,c(2,3,4),mean,na.rm=T)
		
		avg.error <- apply(error.matrix,c(3,4,5),mean,na.rm=T)
		
		stopifnot(all(!is.na(avg.error)))
		best.index<-arrayInd(which.min(avg.error),dim(avg.error))		
		
		best.model$hidden <- learner$set.hidden[best.index[1]]
		best.model$decay <- learner$set.decay[best.index[2]]
		best.model$embedding <- obj$set.embedding[best.index[3]]
		
	}else if(learner$name=="SVM"){
# Coming Soon	
	}else if(learner$name=="BST"){
		
#		error.matrix[id.complete[index.valid], i.fold, h, seq(3,2+obj$learner$max.mstop)]
		
		err.matrix <- error.matrix[,,horizons,,drop=F]
		
# Average over points and folds 
		avg1.error <- apply(err.matrix,c(3,4),mean,na.rm=T)
		
		avg.error <- apply(avg1.error,c(2),mean)
		stopifnot(any(avg.error != Inf))
		best.index <- which.min(avg.error)
		
		if(best.index == 1){
			best.mstop <- -1
		}else if(best.index == 2){
			best.mstop <- 0
		}else{
			best.mstop <- best.index - 2
		}

		best.model$mstop <- best.mstop

	}
	
	
	list(best.model = best.model)

}



learn <- function(object, ...)
UseMethod("learn")



learn.strategy <- function(obj, error.matrix=NULL){
	
	
	if(obj$procedure$type=="recursive" && is.null(obj$procedure$objectives)){
	   		
		rec.matrix <- NULL 
		if(obj$learner$name=="KNN" || obj$learner$name=="MLP" || obj$learner$name=="BST"){

			# REC-MODEL is DIR-MODEL with h=1 (to avoid using the recursive procedure which is slower)
			obj.copy <- obj
			obj.copy$procedure$type <- "direct"
			rec.matrix <- ErrorMatrix(obj.copy,1)
			#rec.matrix <- ErrorMatrix(obj,1)

		}

		results <- autoregression(obj,1, rec.matrix)
		# IMPORTANT : Others strategies cannot use this rec.matrix
		# So error.matrix from ErrorMatrix should not be available after this line.
			
	}else{
				
		no.matrix <- is.null(error.matrix)

		if( (obj$learner$name=="KNN" || obj$learner$name=="MLP" || obj$learner$name=="BST") && no.matrix){
			error.matrix <- ErrorMatrix(obj,obj$H)
		}
	   
		
		n.objectives <- length(obj$procedure$unique.objectives)
		results <- vector("list",n.objectives)
			   
		for(id.objectif in seq_along(obj$procedure$unique.objectives)){
	   
			horizons <- obj$procedure$unique.objectives[[id.objectif]]
			results[[id.objectif]] <- autoregression(obj,horizons,error.matrix)
			
		}

						
	}
	list(results=results, error.matrix = error.matrix)
	
}


predict.strategy <- function(obj,learning.results,Xtest=NULL)
{
	if(is.null(Xtest)){
	# Forecasting the end of the time series
					   
		x.var <- head(obj$all.x.variables,-1)
		Xtest <- tail(obj$data.set[c("Y1",x.var)],1)
		colnames(Xtest) <- obj$all.x.variables
	}
	
	Xtest <-  as.data.frame(Xtest)
	n.test <- nrow(Xtest)
	forecasts <- data.frame(matrix(NA, nrow = n.test, ncol = obj$H))
	colnames(forecasts) <- paste("Y", seq(obj$H), sep = "")
	
	details <- NULL 
	learner <- obj$learner
	
	if(obj$procedure$type=="recursive" && is.null(obj$procedure$objectives)){
						
		best.model <- learning.results$best.model
				
		if(learner$name=="KNN" || learner$name=="MLP"){
			embedding <- best.model$embedding
			x.variables <- paste("X",seq(embedding),sep="")
		
		}else if(learner$name == "BST"){
			x.variables <- obj$all.x.variables
		}
		
		y.variable <- "Y1"
		all.variables <- c(y.variable, x.variables)
		id.complete <- which(complete.cases(obj$data.set[all.variables]))
		complete.data <- obj$data.set[id.complete, all.variables, drop=F]
		queries <- Xtest[,x.variables,drop=F]
				
		if(learner$name=="MLP"){
			
			all.models<-lapply(seq(learner$n.runs),function(i){
#							 skip <- ifelse(best.model$hidden==0,TRUE,FALSE)
#							 model <- nnet(y=complete.data[y.variable],x=complete.data[x.variables],size=best.model$hidden,decay=best.model$decay,maxit=learner$maxiter,linout=T,trace=F,skip=skip)
						   model <- mynnet(y=complete.data[y.variable],x=complete.data[x.variables],size=best.model$hidden,decay=best.model$decay, config = obj$learner)  
							 model})	
				

		}else if(learner$name=="BST"){
		
			myformula <- MakeFormula(y.variable = y.variable, obj$all.x.variables, interactions = obj$learner$interactions)
			best.model$model <- gamboost(as.formula(myformula), data = complete.data, control = boost_control(mstop = obj$learner$max.mstop,nu = obj$learner$nu))
			if(best.model$mstop > 0){
				best.model$model[best.model$mstop]
			}
			
		}

		for(h in seq(obj$H)){
		
			if(learner$name =="KNN"){
				
				predictions <- mykknn(Y1~., complete.data, queries, k=best.model$k)[best.model$k, ]
				
			}else if(learner$name == "MLP"){
				all.runs <- lapply(all.models,predict,queries)
				predictions <- Reduce("+",all.runs)/learner$n.runs
				colnames(predictions) <- NULL
				

								
			}else if(learner$name == "BST"){
					
				predictions <- predictions.bst(best.model$mstop, best.model$model, queries)
			}

			forecasts[,h] <- predictions
			
			queries <- cbind(predictions,queries[,-ncol(queries),drop=F])
			colnames(queries) <- x.variables
		}
	}else if(obj$procedure$type=="direct" || !is.null(obj$procedure$objectives)){
		
		
		best.models <- learning.results
		unique.objectives <- obj$procedure$unique.objectives
		
		if(learner$name=="BST" && learner$save.all){
			details <- list()
			details$all.iterations <- array(NA,c(n.test,learner$max.mstop,obj$H))
			details$best.mstop <- numeric(obj$H)
		}
		
		for(id.objectif in seq_along(unique.objectives)){
			
			objectif <- unique.objectives[[id.objectif]]
			
			if(obj$procedure$type=="recursive"){
				y.variables <- "Y1"
			}else if(obj$procedure$type=="direct"){
				y.variables <- paste("Y",objectif,sep="")
			}else{stop("ERROR in predict !");}
			
			
			# set.horizons = horizons for which the same objectif is used
			set.horizons <- which(unlist(lapply(obj$procedure$objectives,identical,objectif)))
			horizon.variables <- paste("Y",set.horizons,sep="")

			best.model <- best.models[[id.objectif]]$best.model 
			
			if(learner$name=="KNN" || learner$name=="MLP" || learner$name=="MLPmo"){
				embedding <- best.model$embedding
				x.variables <- paste("X",seq(embedding),sep="")
				
			}else if(learner$name == "BST"){
				x.variables <- obj$all.x.variables
			}

			all.variables <- c(y.variables, x.variables)
			id.complete <- which(complete.cases(obj$data.set[all.variables]))
			complete.data <- obj$data.set[id.complete, all.variables, drop=F]
			queries <- Xtest[x.variables]
			
			
			
			if(learner$name=="KNN"){
	
			   if(obj$procedure$type=="recursive"){
						   
					formula.knn <- as.formula(paste(y.variables,"~",paste(x.variables,collapse="+"),sep=""))
						   
				   for(h in seq(max(set.horizons))){
						   					   
						   predictions <- mykknn(formula.knn, complete.data, queries, k=best.model$k)[best.model$k, ]
						   
							if(h %in% set.horizons){
								forecasts[,h] <- predictions
							}
					   
						   queries <- cbind(predictions,queries[,-ncol(queries),drop=F])
						   colnames(queries) <- x.variables
				   }
			   
			   }else if(obj$procedure$type=="direct"){
			   
				   for(h in set.horizons){
				   					   
					   formula.knn <- as.formula(paste(paste("Y",h,sep=""),"~",paste(x.variables,collapse="+"),sep=""))
					   predictions <- mykknn(formula.knn, complete.data, queries, k=best.model$k)[best.model$k, ]
					   forecasts[,h] <- predictions
				   }
			   }
						   				
			}else if(learner$name=="MLP"){
				
				
				if(obj$procedure$type=="recursive"){
										
					all.models<-lapply(seq(learner$n.runs),function(i){
#									   skip <- ifelse(best.model$hidden==0,TRUE,FALSE)
#									   model <- nnet(y=complete.data[y.variables],x=complete.data[x.variables],size=best.model$hidden,decay=best.model$decay,maxit=learner$maxiter,linout=T,trace=F,skip=skip)
									   model <- mynnet(y=complete.data[y.variables],x=complete.data[x.variables],size=best.model$hidden,decay=best.model$decay, config = obj$learner)
									   model})	
					
					for(h in seq(max(set.horizons))){
						
						all.runs <- lapply(all.models,predict,queries)
						predictions <- Reduce("+",all.runs)/learner$n.runs
						
						if(h %in% set.horizons){
							forecasts[,h] <- predictions
						}
						
						queries <- cbind(predictions,queries[,-ncol(queries),drop=F])
						colnames(queries) <- x.variables
					}
					
				}else if(obj$procedure$type=="direct"){
					
					for(h in set.horizons){
						
						all.models<-lapply(seq(learner$n.runs),function(i){
#										   skip <- ifelse(best.model$hidden==0,TRUE,FALSE)
#										   model <- nnet(y=complete.data[paste("Y",h,sep="")],x=complete.data[x.variables],size=best.model$hidden,decay=best.model$decay,maxit=learner$maxiter,linout=T,trace=F,skip=skip)
										 model <- mynnet(y=complete.data[paste("Y",h,sep="")],x=complete.data[x.variables],size=best.model$hidden,decay=best.model$decay, config = obj$learner)
										   model})	
						
						all.runs <- lapply(all.models,predict,queries)
						predictions <- Reduce("+",all.runs)/learner$n.runs
						

						
						forecasts[,h] <- predictions
					}
				}
				
			
				
			}else if(learner$name=="MLPmo"){
				
				all.models<-lapply(seq(learner$n.runs),function(i){
								   skip <- ifelse(best.model$hidden==0,TRUE,FALSE)
								   model <- nnet(y=complete.data[y.variables],x=complete.data[x.variables],size=best.model$hidden,decay=best.model$decay,maxit=learner$maxiter,linout=T,trace=F,skip=skip)
								   model})		
				
				all.runs <- lapply(all.models,predict,queries)
				predictions <- Reduce("+",all.runs)/learner$n.runs
				
				
				forecasts[,horizon.variables] <- predictions[,horizon.variables,drop=F]
				
			}else if(learner$name=="BST"){
								
				
				if(obj$procedure$type=="recursive"){
					
					myformula <- MakeFormula(y.variable = y.variables, obj$all.x.variables, interactions = obj$learner$interactions)
					best.model$model <- gamboost(as.formula(myformula), data = complete.data, control = boost_control(mstop = obj$learner$max.mstop,nu = obj$learner$nu))
					if(best.model$mstop > 0){
						best.model$model[best.model$mstop]
					}
					
					for(h in seq(max(set.horizons))){
						
						predictions <- predictions.bst(best.model$mstop, best.model$model, queries)
						
						if(h %in% set.horizons){
							forecasts[,h] <- predictions
						}
						
						queries <- cbind(predictions,queries[,-ncol(queries),drop=F])
						colnames(queries) <- x.variables
					}
					
				}else if(obj$procedure$type=="direct"){
					
					for(h in set.horizons){
						
						myformula <- MakeFormula(y.variable = paste("Y",h,sep=""), obj$all.x.variables, interactions = obj$learner$interactions)
						best.model$model <- gamboost(as.formula(myformula), data = complete.data, control = boost_control(mstop = obj$learner$max.mstop,nu = obj$learner$nu))
						if(best.model$mstop > 0){
							best.model$model[best.model$mstop]
						}
						
						predictions <- predictions.bst(best.model$mstop, best.model$model, queries)
						
						forecasts[,h] <- predictions
					}
				}
				
			}# END BST
		}
	}
	list(forecasts=forecasts, details=details)
}


