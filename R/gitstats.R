#' Plot github stats
#' 
#' This function plots the total number of forks and followers for all repositories of a certain user or organization.
#' 
#' @param id name of the github user or organization
#' @param type either "users" or "orgs"
#' @param max maximum number of repositories to plot
#' @author Scott Chamberlain, Jeroen Ooms
#' @import ggplot2 httr RJSONIO reshape2
#' @export
gitstats <- function (id = "hadley", type = c("users", "orgs"), max=20) {
	type <- match.arg(type, choices=c('users','orgs'))
	
	#call github API using httr
	url2 <- paste("https://api.github.com", type, id, "repos?per_page=200", sep="/")
	xx <- GET(url2)
	if(xx$status != 200){
		stop("Github returned an error: ", xx$status)
	}
	tt <- fromJSON(rawToChar(xx$content))
	
	#convert to data frame
	out <- do.call("rbind", lapply(tt, "[", c("name", "forks", "watchers")))
	out <- as.data.frame(lapply(as.data.frame(out), unlist))
	
	#resort factor)
	out <- out[head(order(out$watchers, decreasing=TRUE), max),];
	out$name <- factor(out$name, levels=rev(out$name));
	
	#reshape to "long" dataframe"
	names(out) <- c("Repo", "Forks", "Stars")
	out2 <- reshape2::melt(out, id = 1)
	
	#create ggplot object
	myplot <- ggplot(out2, aes(Repo, value)) + 
    geom_bar(stat="identity") + coord_flip() + 
		facet_wrap(~variable) + 
    theme_bw(base_size = 18) + 
    xlab("") + ylab("")
	
	#don't return anything
	print(myplot)  
	invisible();
}