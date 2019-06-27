# df <- data.frame(dose=c("D0.5", "D1", "D2", "D0.5", "D1", "D2"),
#                  len=c(4.2, 10, 29.5, 20, 39.5, 8))
# head(df)
# 
# 
# 
# library(ggplot2)
# # Basic line plot with points
# ggplot(data=df, aes(x=dose, y=len, group=1)) +
#   geom_line()+
#   geom_point()
# # Change the line type
# ggplot(data=df, aes(x=dose, y=len, group=1)) +
#   geom_line(linetype = "dashed")+
#   geom_point()
# # Change the color
# ggplot(data=df, aes(x=dose, y=len, group=1)) +
#   geom_line(color="red")+
#   geom_point()






# library(ggplot2)
# 
# p <- ggplot(mtcars, aes(cyl, mpg)) +
#   geom_line()
# 
# # # Create a simple secondary axis
# # p + scale_y_continuous(sec.axis = sec_axis(~.+10))
# # 
# # 
# # # Inherit the name from the primary axis
# # p + scale_y_continuous("Miles/gallon", sec.axis = sec_axis(~.+10, name = derive()))
# 
# # Duplicate the primary axis
# p + scale_y_continuous(sec.axis=dup_axis())






# library(ggplot2)
# library(gridExtra)
# library(grid)
# 
# p1 <- qplot(mpg, wt, data = mtcars, colour = cyl)
# p2 <- qplot(mpg, data = mtcars) + ggtitle("title")
# p3 <- qplot(mpg, data = mtcars, geom = "dotplot")
# p4 <-
#   p1 + facet_wrap( ~ carb, nrow = 1) + theme(legend.position = "none") +
#   ggtitle("facetted plot")
# 
# p1
# p2
# p3
# p4
# 
# grid.arrange(p1, p2, nrow = 1)
# grid.arrange(p1, p2)
# 
# 
# grid.arrange(
#   p3,
#   p3,
#   p3,
#   p3,
#   ncol = 2,
#   top = "Title of the page",
#   bottom = textGrob(
#     "this footnote is right-justified",
#     gp = gpar(fontface = 3, fontsize = 9),
#     hjust = 1,
#     x = 1
#   )
# )





# df = data.frame(animal=c('goat','horse-d','horse','two', 'five'), level=c('five','one','three',30,'abc-horse'), length=c(10, 20, 30, 'horse', 'eight'))
# df_index <- which(df == "horse", arr.ind=T)
# df[df_index] 







# library(ggplot2)
# 
# # create a dataset
# specie=c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
# condition=rep(c("normal" , "stress" , "Nitrogen") , 4)
# value=abs(rnorm(12 , 0 , 15))
# data=data.frame(specie,condition,value)
# 
# str(data)
# 
# # Grouped
# ggplot(data, aes(fill=condition, y=value, x=specie)) +
#   geom_bar(position="dodge", stat="identity")

# # Stacked
# ggplot(data, aes(fill=condition, y=value, x=specie)) +
#   geom_bar( stat="identity")
# 
# 
# # Stacked Percent
# ggplot(data, aes(fill=condition, y=value, x=specie)) +
#   geom_bar( stat="identity", position="fill")



# prop.test(c(14, 46), c(60, 60), p = NULL, alternative = "two.sided", correct = TRUE)


# library(mvtnorm)
# dmvnorm(x=c(0,0))
# dmvnorm(x=c(0,0), mean=c(1,1))
# x <- rmvnorm(n=100, mean=c(1,1))
# plot(x)





library(ggiraph)
library(ggplot2)

set.seed(1)
d <- seq(Sys.Date(), (Sys.Date() + 20), by = "day")
n <- length(d)
x <- rnorm(n)
x[floor(n/2)] <- NA
df <- data.frame(d = d, x = x)
str(df)

ggplot(df, aes(d, x)) +
  geom_line(na.rm = F) +
  labs(title = "ggplot2::geom_line")

ggplot(df, aes(d, x)) +
  geom_line(na.rm = T) +
  labs(title = "ggplot2::geom_line")

ggplot(df, aes(d, x)) +
  geom_line_interactive()  +
  labs(title = "ggiraph::geom_interactive_line")
