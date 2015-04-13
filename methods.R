
strategy_learner <- function(name=c("KNN","MLP", "MLP", "BST"),...) {
	
	name <- match.arg(name)
	
	parameters <- list(...)
	par.names <- names(parameters)
	
	if(name=="KNN"){
		
		if(!is.element("inc",par.names)){
			
			parameters$inc <- 2
		}
		
	}else if(name=="MLP"){
		
		if(!is.element("set.hidden",par.names)){
			
			parameters$set.hidden <- c(0, 1, 2, 3, 4)
		}
		
		if(!is.element("set.decay",par.names)){
			
			parameters$set.decay <- c(0, 0.001, 0.1, 0.5)
		}
		
		if(!is.element("n.runs",par.names)){
			
			parameters$n.runs <- 1
		}
		
		if(!is.element("maxiter",par.names)){
			parameters$maxiter <- 100
		}
		
		##### Checking #####
		
		if(parameters$n.runs<1)
		stop("n.runs must be > 0 !")
		
		if( any(parameters$set.decay<0) || any(parameters$set.hidden<0) )
		stop(" All elements of set.decay and set.hidden must be >= 0 ")
		
	}else if (name=="BST"){
		
		if(!is.element("interactions",par.names)){
			
			parameters$interactions <- 1
		}
		
		if(!is.element("nu",par.names)){
			
			parameters$nu <- 0.2
		}
		
		if(!is.element("max.mstop",par.names)){
			
			parameters$max.mstop <- 100
		}
		
		if(!is.element("save.all",par.names)){
			
			parameters$save.all <- FALSE
		}
		
		##### Checking #####
		if(!(parameters$nu>=0 && parameters$nu<=1))
		stop("nu must be in [0,1] !")
		
		
	}
	
	RET <- c(list(name = name),parameters)
	class(RET) <- c("learner")
	RET
}


strategy_procedure <- function(type=c("recursive","direct"), objectives = NULL) {
	
	type <- match.arg(type)
	
	RET <- list(type = type, objectives = objectives)
	class(RET) <- c("procedure")
	RET
}


strategy_control <- function(method = c("ts-cv","cv"),...) {
	
	method <- match.arg(method)
	parameters <- list(...)
	par.names <- names(parameters)
	
	if(method == "ts-cv"){
		
		if(!is.element("train.percentage",par.names)){
			
			parameters$train.percentage <- 0.7
		}		
	}
	
	if(!is.element("n.fold",par.names)){
		
		parameters$n.fold <- 5
	}
	
	validation <- c(list(method = method), parameters)
	
	RET <- list(validation = validation)
	class(RET) <- c("control")
	RET
}


strategy <- function(time.series , H, set.embedding = seq(1,5), data.set=NULL, procedure=strategy_procedure(), learner = strategy_learner(), control=strategy_control()){
	
	####### TESTING INTEGRITY OF STRATEGY ########
	stopifnot(length(time.series) > 0)
	stopifnot(H>0)
	stopifnot(max(set.embedding) < length(time.series))

	if(any(set.embedding<1)){
		stop(" All elements of set.embedding must be > 0 ")
	}
		 
	if( procedure$type=="recursive" && learner$name!="KNN" && learner$name!="MLP" && learner$name!="BST" && !is.null(procedure$objectives) ){
		stop(" Only KNN can be used with a recursive procedure and objectives != NULL ")
	}
	
	if( learner$name=="BST" && learner$interactions==2 && max(set.embedding) == 1 ){
		stop(" To allow two interactions (interactions=2), all elements of set.embedding must be > 1 ! ")
	}
	
	
	#if(learner$name=="BST" && !is.null(procedure$objectives)){
#				
#		if( !identical( as.list(procedure$objectives), lapply(seq(H),identity)) ){
#			stop(" With BST, objectives must be seq(H) or be equal to NULL ! ")
#		}
#		
#	}
	############################################
	
	
	max.embedding <- max(set.embedding)
	y.label <- "Y"
	x.label <- "X"
	
	all.x.variables <- paste(x.label,seq(max.embedding),sep="")
	all.y.variables <- paste(y.label,seq(H,1),sep="")
	
	if(is.null(data.set)){
		data.set <- EmbedTimeSeries(time.series, c(seq(H-1, 0), -seq(max.embedding)), y.label, x.label)
	}
	
# CHECK HERE THAT h is in the objective for direct and ALL the objectives are all in seq(H)

	
	if( procedure$type=="direct" && is.null(procedure$objectives) ){
		
		procedure$objectives <- lapply(seq(H),identity)
	}
	procedure$unique.objectives <- unique(procedure$objectives)

	
	
	
	RET <- list(time.series=time.series, 
				H=H, 
				data.set=data.set, 
				n.init.obs=nrow(data.set), 
				set.embedding=set.embedding, 
				n.embedding=length(set.embedding), 
				max.embedding=max(set.embedding), 
				all.x.variables=all.x.variables,
				all.y.variables=all.y.variables,
				procedure=procedure, 
				learner=learner, 
				control=control)
	
	class(RET) <- c("strategy")
	RET
}



type.strategy <- function(x){
	x$procedure$type
}

multistep.matrix.strategy <- function(x){

	if(x$learner$name!="KNN")
	stop(" This function is only available for KNN !")
	
	error.matrix <- ifelse(type(x)=="recursive",
		   recursive.matrix(x),
		   direct.matrix(x))
	
	error.matrix
}


print.learner <- function(x){
	cat("Learner : ",x$name,"\n")
	invisible(x)
}

print.procedure <- function(x){
	cat("Type : ",x$type,"\n")
	cat("Criterion : ",x$criterion,"\n")
	invisible(x)
}


print.control <- function(x){
  cat("Validation : ",x$validation,"\n")
}


print.strategy <- function(x){
	
	cat("Length : ",length(x$time.series),"\n")
	cat("H : ",x$H,"\n")
	cat("Set.embedding : ",x$set.embedding,"\n")
	
	print(x$procedure)
	cat("--- \n")
	print(x$learner)
	cat("--- \n")
	print(x$control)
	
	invisible(x)
}
