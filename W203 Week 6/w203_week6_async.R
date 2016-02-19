z <- c("red", "blue", "NA")
class(z)
fz <- factor(z)
class(fz)
fz
fz <- factor(z, levels = c('green', 'red', 'yellow', 'black', 'blue'))
fz
class(fz)
print(fz)
levels(fz)

fz+1

unclass(fz)
fz=='red'
c(fz, 'red')
temp=c(as.character(fz), 'red')
temp
factor(temp, c('green', 'red', 'yellow', 'black', 'blue'))
