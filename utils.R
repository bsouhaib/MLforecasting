mynnet <- function(config, ...)
{
# config contient obj$learner
	
	all.param <- list(...)
	drop.linbias <- config$drop.linbias
	is.linear <- (all.param$size == 0)
	skip <- ifelse(is.linear,TRUE,FALSE)
	maxit <- config$maxiter
		
	if(is.linear && drop.linbias)
	{
		n.inputs <- ncol(all.param$x)
		rang <- 0.7
		mask <- c(0, rep(1,n.inputs))
		nwts <- n.inputs + 1
		Wts <- runif(nwts, -rang, rang)
		Wts[1] <- 0
		
		model <- nnet(..., mask = mask, Wts = Wts, 
					  linout = T,trace = F, maxit = maxit, skip = skip)
		
		
	}else{
		model <- nnet(...,
					  linout = T,trace = F, maxit = maxit, skip = skip)
	}
	model
}



predictions.bst <- function(mstop, model, X){
	
	if(mstop > 0){
		predictions <- predict(model, X)
	}else if(mstop == 0){
		predictions <- rep(model$offset, nrow(X))
	}else if(mstop == -1){
		predictions <- rep(0, nrow(X))
	}
	predictions
}


#if(best.model$mstop > 0){
#					forecasts[,horizon.variables] <- predict(best.model$model, Xtest) 
#					
#					if(learner$save.all){
#						details$best.mstop[id.objectif] <- best.model$mstop 
#						details$all.iterations[,seq(best.model$mstop),id.objectif] <- predict(best.model$model,Xtest,aggregate="none")
#					}
#					
#				}else if(best.model$mstop == 0){
#					forecasts[,horizon.variables] <- rep(best.model$model$offset, nrow(Xtest))
#					
#					if(learner$save.all){
#						details$best.mstop[id.objectif] <- 0
#					}
#				}else if(best.model$mstop == -1){
#					forecasts[,horizon.variables] <- rep(0, nrow(Xtest))
#					if(learner$save.all){
#						details$best.mstop[id.objectif] <- -1
#					}
#				}


EmbedTimeSeries<-function(x,lags,y.label,x.label){
	
	n.obs<-length(x)
	
# Remove duplicate lags
	lags<-union(lags, lags)
	
	lags<-sort(lags, decreasing=T)
	index<-rep(seq(n.obs)) + rep(lags, each=n.obs)
	id.out<-which(index <= 0 | index > n.obs)
	
	if(length(id.out) > 0){
		index[id.out] <- 1
	}
	
	dataset<-x[index]
	
	if(length(id.out) > 0){
		dataset[id.out] <- NA
	}
	
	dim(dataset)<-c(n.obs, length(lags))
	dataset <- data.frame(dataset)
	colnames(dataset)<-ifelse(lags >= 0, paste(y.label, lags+1 ,sep=""), paste(x.label, abs(lags), sep=""))
	dataset
}

MakeFormula <-function(y.variable, x.variables, interactions=1,nbKnotsUni=20,nbKnotsBiv=5)
{
	stopifnot(interactions%in%c(1,2,12))
	
	n.variables<-length(x.variables)
	
	
	if(interactions==1 || interactions==12)
	{
		myformula<-uniformula<- paste("bbs(",x.variables,",df=4,center=TRUE,knots=",nbKnotsUni,")",sep="",collapse="+")
	}
	
	
	if(interactions==2 || interactions==12)
	{
		
		bivformula<-""
		allcomb <- combinations(n.variables,2,v=x.variables)
		for(icomb in seq(nrow(allcomb)))
		{
			term<-paste("bbs(",paste(allcomb[icomb,],collapse=",",sep=""),",df=4,center=TRUE,",
						paste("knots=list(",paste(allcomb[icomb,],"=",nbKnotsBiv,collapse=",",sep=""),")",sep=""),")",sep="")
			
			bivformula <- ifelse(icomb==1, term, paste(bivformula,term,sep="+"))
		}
		myformula <- bivformula
		
		if(interactions==12)
		{
			myformula<-paste(uniformula,bivformula,sep="+")
		}
	}	
	finalformula<-paste(y.variable,"~",myformula,sep="")
	finalformula
}


ValidationMatrix <- function(validation, n.obs)
{
	
	if(validation$method == "ts-cv"){
		
		n.train <- floor(n.obs * validation$train.percentage)
		n.valid <- n.obs - n.train
		
		if(n.valid < validation$n.fold){
			stop(" The validation set is too short to have this number of folds ")
		}
		size.block <- floor(n.valid/ validation$n.fold)
		
		folds <- unlist(lapply( seq(validation$n.fold), function(i.fold) {c( rep(1, n.train) ,rep( rep(1, size.block), i.fold -1 ) , rep(0, n.valid-(i.fold-1)*size.block  ) )} ))
		validation.matrix <- matrix(folds, nrow = n.obs)
		stopifnot( ncol(validation.matrix) == validation$n.fold )
	} else if(validation$method=="cv"){
		
		cvkfold <- function(n, k) {
			if (k > n / 2) stop("k > n/2")
			fl <- floor(n/k)
			folds <- c(rep(c(rep(0, fl), rep(1, n)), k - 1),
					   rep(0, n * k - (k - 1) * (fl + n)))
			matrix(folds, nrow = n)[1:n,, drop = FALSE]
		}
		
		validation.matrix <- cvkfold(n.obs, validation$n.fold)
		
	}else{
		stop("error in validation$method !")
	}
	validation.matrix
}


getObjectifJTs <- function(s1, s2, H)
{
	RET <- removeOut( lapply(seq(H),"+",seq(-s1,s2)) , H)
	RET
}

getS <- function(procedure){
	n <- nchar(procedure)
	s1 <- as.numeric(substring(procedure, n-1, n-1))
	s2 <- as.numeric(substring(procedure, n, n)) 
	list(s1 = s1, s2 = s2)
}

removeOut <- function(mylist,H){
	
	for( i in seq(length(mylist)) ){
		vec <- mylist[[i]]
		id <- which( !(vec %in% seq(H)))
		if(length(id)>0){
			RET <- vec[-id]
		}else{
			RET <- vec
		}
		mylist[[i]] <- RET
	}
	mylist
}

GetDetails <- function(strategy,H)
{
	res <- unlist(strsplit(strategy,"-"))
	procedure <- res[1]
	learner <- res[2]
	
	if(procedure == "REC"){
		
		objectives <- NULL
		
	}else if(procedure %in% c("RTI", "DIR", "RFY", "RFYMIS")){
		
		objectives <- seq(H)
	
	}else if( grepl("JTL",procedure) ){
# RJTLab DJTLab
	
		res <- getS(procedure)
		objectives <- getObjectifJTs(res$s1, res$s2, H)
		
	}else if( procedure %in% c("RJT", "DJT")){
	
		objectives <- lapply(lapply(rep(H,H),identity),seq)
		
	}else if( grepl("RJT",procedure) )
	{
# RJTa
		n <- nchar(procedure)
		
		mychar <- substring(procedure, 4, n)
		s <- as.numeric(mychar)
		objectives <- lapply(rep(s,H),seq)
	}
	
		
# TO DO : Check if the procedure belongs to the set of possible procedures 
		
	if(procedure == "RFY" || procedure == "RFYMIS" || procedure == "RFYJT"){
		proc.type <- "rectify"
	}else if( grepl("R",procedure) && procedure != "DIR"){
		proc.type <- "recursive"
	}else if( grepl("D",procedure) ){
		proc.type <- "direct"
	}else{
		stop("procedure not found ! ")
	}


	details <- list(proc.type = proc.type, procedure = procedure, learner = learner, objectives = objectives)
	details
}


################################
# improved list of objects
# from http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session
.ls.objects <- function (pos = 1, pattern, order.by,
decreasing=FALSE, head=FALSE, n=5) {
    napply <- function(names, fn) sapply(names, function(x)
                                         fn(get(x, pos = pos)))
    names <- ls(pos = pos, pattern = pattern)
    obj.class <- napply(names, function(x) as.character(class(x))[1])
    obj.mode <- napply(names, mode)
    obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
    obj.prettysize <- napply(names, function(x) {
							 capture.output(print(object.size(x), units = "auto")) })
    obj.size <- napply(names, object.size)
    obj.dim <- t(napply(names, function(x)
                        as.numeric(dim(x))[1:2]))
    vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
    obj.dim[vec, 1] <- napply(names, length)[vec]
    out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
    names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
    if (!missing(order.by))
	out <- out[order(out[[order.by]], decreasing=decreasing), ]
    if (head)
	out <- head(out, n)
    out
}

# shorthand
lsos <- function(..., n=10) {
    .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}
