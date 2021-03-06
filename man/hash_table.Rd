% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hash_elements.r, R/hash_table.r, R/hash_value.r
\name{hash_table}
\alias{hash_table}
\alias{hash_elements}
\alias{hash_table}
\alias{hash_table}
\alias{hash_value}
\title{Hash Elements}
\usage{
hash_elements(ht)

hash_table(env)

hash_value(str)
}
\arguments{
\item{ht}{'hash_table' object.}

\item{env}{An environment.}

\item{str}{A character vector}
}
\value{
character vector containing all strings in 'ht'.

object of class 'hash_table', a list of character vectors

numeric vector
}
\description{
The elements of the 'hash_table'

Returns the hash table in a list of character vectors from the provided hashed environment.

Returns the computed hash value for the given character vector.
}
