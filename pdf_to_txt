# Read PDFs and convert to TXT

library(pdftools)

# Path to PDF files
mypath  = "C:\\Users\\user\\Documents\\" 

# Get list of PDF files
files <- list.files(path=sprintf("%s",mypath), pattern="*.pdf", full.names=TRUE, recursive=FALSE)

# COnvert to TXT
lapply(files, function(x) {
    # Get name of file
    myfilename = substr(x, nchar(mypath)+1, nchar(x)-4)
    # PDF to text
    out <- NULL
    try(out <- pdf_text(x), TRUE)
    # write to file (and check if text was found)
    if (is.null(out)==FALSE) {
      write.table(out, sprintf("C:\\Users\\PHE\\Documents\\R\\%s.txt",myfilename), sep="\t", quote=FALSE, row.names=FALSE, col.names=TRUE)
    }
})
