new.comdist.prime = function(my.sample, my.dist.mat){

my.ra.sample = my.sample/rowSums(my.sample)

	get.presents = function(x){
		names(x[x>0])
		}

	get.weights = function(x){
		x[x>0]
		}

	list.of.weights = apply(my.ra.sample, 1, get.weights)

	
	Dpw.apply.function = function(x){
		tmp.function = function(z){
			dpw.output = sum(my.dist.mat[names(x),names(z)] * outer(x, z))

			
			}
		
		lapply(list.of.weights, FUN=tmp.function)
		
		}

dpw.output = lapply(list.of.weights, Dpw.apply.function)

outt = do.call(rbind,lapply(dpw.output,unlist))


outt
}

