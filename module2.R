fasterSymbolArray <- function(text, pattern) {
  coords = list()
  n = nchar(text)
  extended_genome = paste0(text, substring(text, 1, floor(n/2))) 
  
  # look at the first half of Genome to compute first array value
  coords[1] = patternCount(substring(text, 1, floor(n/2)), pattern)
  for(i in 2:n) {
    # the current array value can differ from the previous array value by at most 1
    if(substring(extended_genome, i-1, i-1) == pattern) {
      coords[i] = coords[[i-1]] - 1 }
    if(substring(extended_genome, i + floor(n/2) - 1, i + floor(n/2) - 1) == pattern) {
      coords[i] = coords[[i-1]] + 1
    }
  }
  
  coords
  
}

pattern = "A"
text = "AAAAGGGG"
fasterSymbolArray(text, pattern)


skew_diagram <- function(text, pattern) {
  
  plot(unlist(fasterSymbolArray(text, pattern)), 
       ylab = paste("count of", pattern, "in half-genome starting at given position"), 
       xlab = "genome position")
}

skew_diagram(text, pattern)


#url <- "https://bioinformaticsalgorithms.com/data/realdatasets/Replication/E_coli.txt"
#ecoli <- readLines(url)
#head(ecoli)  # View first few lines

#skew_diagram(ecoli, "C")





skew_array <- function(genome) {
  base = strsplit(genome, "")[[1]]
  
  count = vector(mode = "numeric", length = nchar(genome))
  
  
  for (b in seq_along(base)) {
    if (base[b] == "C") {
      count[b] = -1
    }
    if (base[b] == "G") {
      count[b] = 1
    }
    if (base[b] %in% c("A", "T")) {
      count[b] = 0
    }
  }
genome = "CATGGGCATCGGCCATACGCC"
skew_array(genome)  


