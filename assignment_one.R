#import madelon file
madelon<-read.arff("/home/unidentified/Data_mining/assignment01/Assignment_01/scripts/madelon.arff")
#rank features by their information gain
A<-InfoGainAttributeEval(class~.,data=madelon,control=Weka_control(T=-1.7))
#sort in ascending order
ranked_list<-A[order(A)]

##method to select which classifier to used
select_classifier<-function(sclass,classname){
	
	classi_path<-""
	if(sclass == "J48"){
		classi_path<-"weka/classifiers/trees/J48"
		}
	else if (sclass == "NaiveBayes"){
		classi_path<-"weka/classifiers/bayes/NaiveBayes"
		}
	else if (sclass == "kNN"){
		classi_path<-"weka/classifiers/lazy/IBk"
		}
	else if (sclass == "RandomForest"){
		classi_path<-"weka/classifiers/trees/RandomForest"
		}
	else{
		print("Invalid input")
		}

	sclass<-make_Weka_classifier(classi_path)		
	to_value<-0
	iteration<-0
	
	for(g in 1:2){
	
	accuracy_vector<-numeric()
	#hold all the total times in an array
	time_vector<-numeric()
	#normalise ttotal
	normalised_times<-numeric()
	#P measure of time+accuracy
	p_value<-numeric()
	#combine all results to be exported into a csv
	printable_vector<-numeric()
	normalised_times<-numeric()
	#define the file name based on the iteration value
	file_name<-paste(classname,".csv")
	trimmed_name<-gsub(" ","",file_name)
	
	if(g == 1){
		to_value<-500
		iteration<-100
		features<-0
	}
	else if(g == 2){
		to_value<-100
		iteration<-10
		#this number was constant throughout all tests prior
		features<-400
		file_name<-paste(classname,"10.csv")
		trimmed_name<-gsub(" ","",file_name)
	}
	else{
		print("Something probably went wrong")
	}
	
	for (k in seq(1,to_value,iteration)){
		s<-ranked_list[1:(features+k)]
		#identify names of low rank features
		cols.dont.want<-c(names(s))
		#drop lowest ranked features and put into a new filtered madelon set
		madelon_filtered<-madelon[,!names(madelon) %in% cols.dont.want, drop=T]
		#create new model which uses the madelon_filtered list and apply classifier; J48, Naive Bayes, kNN, Random Forest
		model<-sclass(class~.,data=madelon_filtered)
		#evaluate the model, set the num of folds and seed.
		e<-evaluate_Weka_classifier(model, numFolds=2, seed=1)
		accuracy<-e$details[[1]]
		iteration_number<-paste("Iteration level: ", k)
		print(iteration_number)
		prnt_acc<-paste("Accuracy: ", accuracy)
		print(prnt_acc)
		tmodel<-system.time(sclass(class~.,data=madelon_filtered))
		tdeploy<-system.time(evaluate_Weka_classifier(model, numFolds=2))
		ttotal<-tmodel+tdeploy
		total_time<-paste("Total time: ",format(round(ttotal[[1]],2), nsmall=2))
		print(total_time)
		accuracy_vector<-c(accuracy_vector, accuracy)
		time_vector<-c(time_vector, ttotal[[1]])
		printable_vector<-c(printable_vector,iteration_number,prnt_acc,total_time,"\n")				
	}
	
	## see if we can identify optimal features out of the entire list
	
	
	#normalise the time ttotal values
	for (i in time_vector){
		normalised<-(i-min(time_vector))/(max(time_vector-min(time_vector)))
		normalised_times<-c(normalised_times, normalised)
	}
	print(normalised_times)
	counterone<-1
	for(j in accuracy_vector){
		p<-(0.7*(j/100)+0.3*(1-normalised_times[counterone]))*100
		p_formatted<-format(round(p,2), nsmall=2)
		p_value<-c(p_value,p_formatted)
		counterone<-counterone+1
		}
	
	counter<-1
	for (d in p_value){
		
		p_runs<-paste("On run ",counter," the combined time and accuracy value is: ",d,"%")
		print(p_runs)
		printable_vector<-c(printable_vector, p_runs)
		counter<-counter+1
		}
	#print results to a csv
	#write.table(printable_vector, file=trimmed_name, row.names=FALSE,col.names="Results")
	}
		
	

}

