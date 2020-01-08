# Read all files in folder
mydocpath = "C:/Users/.../data"
files <- list.files(path=sprintf("%s",mydocpath), pattern="*.csv", full.names=TRUE, recursive=FALSE)
print(files)

# Read in loop and post in "list of DFs"
myfiles = lapply(files, function(x) unique(as.data.frame(read.csv(x, sep=";", header=F, na = "na", skip=1))))
# Access single DF
# myfiles[[1]]

# Now "rbind" the dfs to one large df
de <- Reduce(rbind, myfiles)
