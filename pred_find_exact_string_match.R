mylist = list("rm", "ff", "ffu", "fkw")
mylist[1]

# Find the position of an exact match

# Use word boundaries
grep("\\bff\\b", mylist)
mylist[grep("\\bff\\b", mylist)+1]

# Or which
which(mylist == "ff")
