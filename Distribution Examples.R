x <- data.frame(k = runif(1000, 0.84834637 - (40*1.744392e-02), 0.84834637 + (40*1.744392e-02)),
           rate = runif(1000, 0.00311007 - (40*7.517423e-05), 0.00311007 + (40*7.517423e-05)))
y <- as.data.frame(matrix(nrow = 1000, ncol = 1000))
for(i in 1:nrow(y)){
  y[i,1:1000] <- rgamma(1000, x$k[i], x$rate[i])
}

y1 <- y %>% 
  rowwise() %>%
  mutate(Max = max(c_across(V1:V1000)),
         Mean = mean(c_across(V1:V1000)))
x$Max <- y1$Max
x$Mean <- y1$Mean

require(ggplot2)

ggplot(data = y1) +
  geom_density(aes(x = Max))

ggplot(data = y1) +
  geom_density(aes(x = Mean))

ggplot(data = x, aes(x = k, y = Max)) +
  geom_point()+
  scale_y_continuous(trans='log2')

ggplot(data = x, aes(x = k, y = Mean)) +
  geom_point()+
  scale_y_continuous(trans='log2')

ggplot(data = x, aes(x = rate, y = Max)) +
  geom_point()+
  scale_y_continuous(trans='log2')

ggplot(data = x, aes(x = rate, y = Mean)) +
  geom_point() +
  scale_y_continuous(trans='log2')
