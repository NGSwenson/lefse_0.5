new.comdist = function(my.sample, my.dist.mat){

	get.presents = function(x){
		names(x[x>0])
		}

	list.of.names = apply(my.sample, 1, get.presents)
	
	Dpw.apply.function = function(x){
		tmp.function = function(z){
			mean(my.dist.mat[x,z])
			}
		
		lapply(list.of.names,FUN=tmp.function)
		
		}

dpw.output = lapply(list.of.names,Dpw.apply.function)

outt = do.call(rbind,lapply(dpw.output,unlist))


outt

}