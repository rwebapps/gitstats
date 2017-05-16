#' Plot github stats
#' 
#' Plots the total number of forks and stars for all repositories of 
#' a user or organization.
#' 
#' @param id name of the github user or organization. Default: "Hadley"
#' @param type either "users" (default) or "orgs"
#' @param max maximum number of repositories to plot. Default: 100. Max: 100
#' @param ... curl options passed on to \code{\link[httr]{GET}}
#' @author Scott Chamberlain, Jeroen Ooms
#' @import ggplot2 httr jsonlite reshape2
#' @export
#' @examples \dontrun{
#' gitstats(max = 10)
#' gitstats(max = 40)
#' gitstats(id = "jeroen", max = 30)
#' gitstats(id = "ropensci", type = "orgs", max = 70)
#' }
gitstats <- function (id = "hadley", type = c("users", "orgs"), max = 100, 
                      ...) {
  
	type <- match.arg(type, choices=c('users','orgs'))
	
	#call github API using httr
	url <- file.path("https://api.github.com", type, id, "repos")
	xx <- GET(url, query = list(per_page = max), ...)
	if (xx$status != 200) {
		stop("Github returned an error: ", xx$status, "\n\n", rawToChar(xx$content))
	}
	out <- fromJSON(rawToChar(xx$content))
	
	#resort factor)
	out <- out[order(out$watchers, decreasing = TRUE), 
	           c("name", "forks", "watchers")]
	out$name <- factor(out$name, levels = rev(out$name))
	
	#reshape to "long" dataframe"
	names(out) <- c("Repo", "Forks", "Stars")
	out2 <- reshape2::melt(out, id = 1)
	
	#create ggplot object
	Repo <- value <- NULL
	myplot <- ggplot(out2, aes(Repo, value)) + 
    geom_bar(stat="identity") + coord_flip() + 
		facet_wrap(~variable) + 
    xlab("") + ylab("")
	
	#don't return anything
	print(myplot)  
	invisible();
}