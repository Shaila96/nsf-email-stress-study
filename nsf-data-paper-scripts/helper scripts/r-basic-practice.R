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