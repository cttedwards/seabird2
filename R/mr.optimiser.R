#' mr.optimiser
#'
#' @export
#'
mr.optimiser <- function(object,labels){
    ans <- object
    if(missing(labels)){
        labels <- c()
        for(label in names(object)){
            if(SeaBird.regexpr("mark_recapture",label)>0)
               labels <- c(labels,substr(label,SeaBird.regexpr("\\[",label)+1,SeaBird.regexpr("\\]",label)-1))
        }
    } else {
        for (label in labels) {
            if( !paste("mark_recapture[",label,"]",sep="") %in% names(object))
            stop(paste(label," not found in the list"))
        }
    }
    for(label in labels){
        MR <- object[[paste("mark_recapture[",label,"]",sep="")]]
        MR_optimiser <- list(command="mark_recapture_optimiser",value=label);
        banded_no <- MR[["banded_no"]]
        banded_labels <- paste("banded",banded_no,sep="_")
        MR_optimiser <- c( MR_optimiser,rep("",length(banded_labels)))
        names( MR_optimiser) <- c("command","value",banded_labels)
        
        for(i in 1:length(banded_no)){
            this_binded_no <- banded_no[i]
            this_data<- as.numeric(MR[[banded_labels[i]]])            
            this_first_observed_year <- this_data[1]
            this_last_observed_year <- this_data[2]
            most_similar_year <- this_first_observed_year
            if(i>1){           
                for(j in (i-1):1){
                    that_data <- as.numeric(MR[[banded_labels[j]]])
                    that_binded_no <- banded_no[j]
                    that_first_observed_year <- that_data[1]
                    that_last_observed_year <-  that_data[2]
                    if( this_first_observed_year== that_first_observed_year){
                        similar_year <- this_first_observed_year
                        for(k in 3:((min(this_last_observed_year,that_last_observed_year)- this_first_observed_year+3))){
                            if(this_data[k]==that_data[k]){
                                similar_year=that_first_observed_year+k-3;
                            } else {
                                break;
                            }
                        }
                        if(similar_year > most_similar_year){
                            most_similar_year <- similar_year
                            similar_binded_no <- that_binded_no
                        }
                        if(most_similar_year== this_last_observed_year) break
                    }
                }
            }
            if(most_similar_year > this_first_observed_year){
                MR_optimiser[[banded_labels[i]]] <- c(similar_binded_no, most_similar_year)
            } else {
                MR_optimiser[[banded_labels[i]]] <- c(this_binded_no, this_last_observed_year )
            }
            
        }
                              
        ans <- c(ans,list(MR_optimiser))
        names(ans)[length(names(ans))] <- paste("mark_recapture_optimiser[",label,"]",sep="")
    }
    ans
}

