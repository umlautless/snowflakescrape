# snowflakescrape
Scraping snowflakes with wayback

install.packages("Rcpp")
devtools::install_github("hrbrmstr/wayback")

library(wayback)
library(tidyverse)

x = readLines('http://www.snowcatcherphotos.com/blahg/patterns/SnowcatcherSnowflakeDirectory.html')

df=data.frame(x)
print(x)

df2<-df %>%
  slice(41:2266) %>%
  mutate (x = as.character(x))

df2<-df2 %>% filter(x != "")
df2<-df2 %>% filter(x != "<tr>")
df2<-df2 %>% filter(x != "</tr>")

df2 <- df2%>% slice(1:1214)

dplus <- df2%>%slice(230:1213)

badrows <- c(97, 382, 369, 404, 411, 416, 433, 438, 461, 478, 581, 707, 716, 724, 727, 817)


newdata <- dplus[-badrows,]
#newdata = data.frame(newdata)

#list.df2 <- list(df2)

#matrix.df2 <- matrix(unlist(list.df2), ncol = 2, byrow = T)

list.dplus <- list(newdata)
matrix.dplus <- matrix(unlist(list.dplus), ncol=2, byrow=T)

cleanlist <- data.frame(matrix.dplus)

cleanlist <- cleanlist %>%
  mutate(X2 = substr(X2, 1, regexpr('<', X2)-1))

#TDs
html_td = '<td><center><font face=\"comic sans ms\">'

quotemark = '\"'


# get first matched group
cleanlist <- cleanlist %>%
  mutate(X1 = gsub(html_td, "", X1)) %>%
  mutate(X1 = substr(X1, 10, length(X1))) %>%
  mutate(location = regexpr(quotemark, X1)) %>%
  mutate(X1 = substr(X1, 1, location-1)) %>%
  select(-location)
  


 
head(cleanlist)
output.dir <- "C:/Users/anoel1/Desktop/snowtext"

#Once to make sure it works right with hard code
mem <- read_memento("https://www.snowcatcher.net/2014/03/snowflake-monday_31.html")
res <- stringi::stri_split_lines(mem)[[1]]
out1 <- data.frame(res)

startline <- '<div class=\"post-body entry-content\">'
endline <- '<span class=\"post-comment-link\">'

line1.val <- which(out1$res == startline)
line2.val <- which(out1$res == endline)

out2 <- out1 %>% slice(line1.val:line2.val)
write.csv(out2, file=file.path(output.dir, "D-stitch snowflake.txt"), row.names = FALSE)


rm(mem, res, line1.val, line2.val, out2)



X1.list <- as.list(cleanlist$X1)
X2.list <- as.list(cleanlist$X2)


wb.text <- function(i) {

  sURL<- sprintf("%s", X1.list[i])
  fileout <-sprintf("%s.txt", X2.list[i])
  
mem <- read_memento(sURL)
res <- stringi::stri_split_lines(mem)[[1]]
out1 <- data.frame(res)


line1.val <- as.numeric(which(out1$res == startline))
line2.val <- as.numeric(which(out1$res == endline))

out2 <- out1 %>% slice(line1.val:line2.val)


write.csv(out2, file=file.path(output.dir, fileout), row.names = FALSE)
}

LL<- 484

for (i in 477:484){   
  wb.text(i)   
  print(i) 
}
