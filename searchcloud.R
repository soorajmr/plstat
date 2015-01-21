#
# Get some crude measure of programming language popularity based on google search hits
# The results are plotted as a tag cloud of language names

library(XML)
library(RCurl)
library(wordcloud)

# Perform google search and return the hit count
get_search_hit_count <- function(search_string) {
  # enclose in quotes
  search_string <- paste0("%22", search_string, "%22")
  url <- paste0("http://www.google.co.in/search?q=", search_string)
  hdoc <- htmlParse(getURL(url, .opts=curlOptions(followlocation=TRUE)))
  count_string <- xpathSApply(hdoc, "//*/div[@id='resultStats']", xmlValue)
  
  # extract the number
  count <- unlist(regmatches(count_string, gregexpr("[0-9]+(,[0-9]+)*", count_string)))
  # remove commas
  count <- as.numeric(gsub(",", "", count))
  return(count)
}

# List of languages we are interested in
lang_names <- c("Java", "C++", "C", "C#", "Objective C", "Perl", "PHP", "Ruby", 
                "Fortran", "JavaScript", "Visual Basic", "Smalltalk", "Go", "R", 
                "Python", "Swift", "Scala", "Lisp", "Haskell", "SML", "Ocaml", 
                "F#", "Clojure", "Erlang")

# Construct more restrictive query strings to weed out false positives
query_strings <- unlist(lapply(lang_names, function(str) paste0(URLencode(str, reserved = TRUE), "+programming+language")))

# Now do the search for all strings!
lang_counts <- unlist(lapply(query_strings, get_search_hit_count))

# plot the tag cloud
wordcloud(lang_names, lang_counts,
          scale = c(9, 0.2), random.order = F, random.color = F, use.r.layout=T, rot.per=0.35,
          colors = rev(brewer.pal(8, "Dark2")))

