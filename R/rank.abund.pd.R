rank.abund.pd = function(my.sample,my.phylo){

rank.com.fun = function(my.sub.sample){
my.species = sort(my.sub.sample[my.sub.sample > 0],decreasing=T)
pd.output = matrix(NA, nrow=1, ncol=length(my.species)-1)
	if(length(my.species)>=2){
	
		for(i in 2:length(my.species)){
	
			pd.output[,i-1] = pd(t(as.matrix(my.species[1:i])),my.phylo)$PD

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