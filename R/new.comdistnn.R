

new.comdistnn = function(my.sample, my.dist.mat){

	get.presents = function(x){
		names(x[x>0])
		}

	list.of.names = apply(my.sample, 1, get.presents)
	
	Dnn.apply.function = function(x){
		tmp.function = function(z){
			
			mean(c(apply(my.dist.mat[x,z], MARGIN=1,min, na.rm=T), apply(my.dist.mat[x,z], MARGIN=2, min, na.rm=T)))
			
			}
		
		lapply(list.of.names, FUN=tmp.function)
		
		}

dnn.output = lapply(list.of.names,Dnn.apply.function)


outt = do.call(rbind,lapply(dnn.output,unlist))

outt

}