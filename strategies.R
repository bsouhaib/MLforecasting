
recursive <- function(time.series, H, set.embedding, objectives = NULL, Xtest = NULL, error.matrix = NULL, ...){
	
	obj <- strategy(time.series, H, set.embedding = set.embedding, procedure=strategy_procedure(type="recursive", objectives = objectives), ...)
	learning <- learn(obj, error.matrix)
	
	forecasts <- predict(obj, learning$results, Xtest = Xtest)$forecasts
	list(forecasts=forecasts, error.matrix = learning$error.matrix)
}

direct <- function(time.series, H, set.embedding, data.set = NULL , objectives = NULL, Xtest = NULL, error.matrix = NULL, ...){
		
	obj <- strategy(time.series, H, set.embedding = set.embedding, data.set, procedure=strategy_procedure(type="direct", objectives = objectives), ...)
	learning <- learn(obj, error.matrix)
	
	forecasts <- predict(obj, learning$results, Xtest = Xtest)$forecasts
	list(forecasts=forecasts, error.matrix = learning$error.matrix)
}

rectify <-function(time.series, H, embeddings.base, embeddings.rect, objectives = NULL, Xtest = NULL, base.results = NULL, learner.bmodel,  ...){
	
	args <- list(...)
	
	if(is.null(base.results)){
	############## Base model ##############
		obj <- strategy(time.series, H, set.embedding = embeddings.base,
						learner = learner.bmodel, 
						procedure = strategy_procedure(type = "recursive"), 
						control = args$control)
		learning <- learn(obj)
		base.forecasts <- predict(obj, learning$results, Xtest)$forecasts
				
		
		id.complete <- which(complete.cases(obj$data.set[obj$all.x.variables]))
		queries <- obj$data.set[id.complete, obj$all.x.variables, drop=F]
		predictions <- predict(obj, learning$results, Xtest = queries)$forecasts
		
# This code require the same embedding for the base model and the rectification models		
# pseudo.data <- obj$data.set
# pseudo.data[obj$all.y.variables] <- NA
# pseudo.data[id.complete, obj$all.y.variables] <- obj$data.set[id.complete, obj$all.y.variables, drop=F] - rev(predictions)
				
#### This code allow a different embedding for rectifications
		max.embedding.rect <- max(embeddings.rect)
		x.label <- "X"; y.label <- "Y";
		data.set <- EmbedTimeSeries(time.series, c(seq(H-1, 0), -seq(max.embedding.rect)), y.label, x.label)
		all.x.variables <- paste(x.label,seq(max.embedding.rect),sep="")
		all.y.variables <- paste(y.label,seq(H,1),sep="")
		pseudo.data <- data.set
		pseudo.data[all.y.variables] <- NA
		pseudo.data[id.complete, all.y.variables] <- data.set[id.complete, all.y.variables, drop=F] - rev(predictions)

				
		base.results <- list(base.forecasts = base.forecasts, pseudo.data = pseudo.data)
	}
	
	base.forecasts <- base.results$base.forecasts
	pseudo.data <- base.results$pseudo.data
	
	if(is.null(Xtest)){
		stopifnot(all(dim(base.forecasts)==c(1,H)))
	}else{
		stopifnot(all(dim(base.forecasts)==c(nrow(Xtest),H)))
	}
		   
	############## Rectification models ##############
	rectifications <- 0

	if(!is.null(learner)){
		rectifications <- direct(time.series, H, set.embedding = embeddings.rect, data.set=pseudo.data, objectives = objectives, Xtest = Xtest, ...)$forecasts
	}
	
	forecasts <- base.forecasts + rectifications

	
	list(forecasts = forecasts, base.forecasts = base.forecasts, rectifications = rectifications, base.results = base.results)
}


rec <- function(time.series, H, ...){
	
	recursive(time.series, H, ...)$forecasts
}

dir <- function(time.series, H, ...){
	
	direct(time.series, H, ...)$forecasts
}

mystrategy <- function(time.series, H, ...){
	obj <- strategy(time.series, H, ...)
	results <- learn(obj)
	forecasts <- predict(obj,results)$forecasts
	forecasts
}


