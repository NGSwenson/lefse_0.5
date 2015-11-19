rank.abund.fric.null = function(my.sample,my.traits){

rownames(my.traits) = sample(rownames(my.traits), length(rownames(my.traits)), replace=F)

rank.com.fun = function(my.sub.sample){
my.species = sort(my.sub.sample[my.sub.sample > 0],decreasing=T)
pd.output = matrix(NA, nrow=1, ncol=length(my.species)-2)
	if(length(my.species)>=3){
	
		for(i in 3:length(my.species)){
	
			pd.output[,i-2] = 	dbFD(my.traits[names(my.species[1:i]),],t(as.matrix(my.species[1:i])),messages=F)$FRic

		}

	}

	else{
		pd.output = c(NA)
	}
pd.output
}

outt= apply(my.sample, 1, rank.com.fun)
outt
}