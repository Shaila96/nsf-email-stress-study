# devtools::install_github("statsmaths/coreNLP")
# coreNLP::downloadCoreNLP()

library(coreNLP)

# initCoreNLP()


catInHat = c("the sun did not shine.", "it was too wet to play.",
             "so we sat in the house all that cold, cold, wet day.")
output = annotateString(catInHat)


getToken(output)[,c(1:3,6:7)]
getDependency(output)
getSentiment(output)

