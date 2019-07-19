data(mtcars)
reg <- lm(mpg~.,data=mtcars)

library(huxtable) # for regression tables
# Examples:
# https://cran.r-project.org/web/packages/huxtable/vignettes/huxreg.html

# Print regression table
huxreg(reg)

# Store regression table
hr = huxreg(reg)
quick_docx(hr, file = "huxtable-output.docx")

# Examples:
# https://www.rdocumentation.org/packages/huxtable/versions/4.1.0/topics/quick-output
