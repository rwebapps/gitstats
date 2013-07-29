\name{gitstats}
\alias{gitstats}
\title{Plot github stats}
\usage{
  gitstats(id = "hadley", type = c("users", "orgs"),
    max = 20)
}
\arguments{
  \item{id}{name of the github user or organization}

  \item{type}{either "users" or "orgs"}

  \item{max}{maximum number of repositories to plot}
}
\description{
  This function plots the total number of forks and
  followers for all repositories of a certain user or
  organization.
}
\author{
  Scott Chamberlain, Jeroen Ooms
}

