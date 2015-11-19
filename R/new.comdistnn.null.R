new.comdistnn.null = function(my.sample, my.dist.mat){

row.names(my.dist.mat) = sample(row.names(my.dist.mat), length(row.names(my.dist.mat)), replace=F)

my.ra.sample = my.sample/rowSums(my.sample)

	get.presents = function(x){
		names(x[x>0])
		}

	get.weights = function(x){
		x[x>0]
		}


	list.of.weights = apply(my.ra.sample, 1, get.weights)

	Dnn.apply.function = function(x){
		tmp.function = function(z){

			weighted.mean(c(apply(as.matrix(my.dist.mat[names(x),names(z)]), MARGIN=1,min, na.rm=T), apply(as.matrix(my.dist.mat[names(x),names(z)]), MARGIN=2, min, na.rm=T)),
			c(x,z)
			)		
			
			}
		
		lapply(list.of.weights, FUN=tmp.function)
		
		}

dnn.output = lapply(list.of.weights, Dnn.apply.function)


dnn.output =do.call(rbind,lapply(dnn.output,unlist))

}

