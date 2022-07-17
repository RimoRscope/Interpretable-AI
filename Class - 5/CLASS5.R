library(partykit)

data("WeatherPlay")

WeatherPlay

lh1 <- partysplit(1L, index = 1:3)
lh3 <- partysplit(3L, breaks = 60)
lh4 <- partysplit(4L, index = 1:2)


pn <- partynode(1L, split = lh1, kids = list(
  partynode(2L, split = lh3, kids = list(
    partynode(3L, info = "yes"),
    partynode(4L, info = "no"))),
  partynode(5L, info = "yes"),
  partynode(6L, split = lh4, kids = list(
    partynode(7L, info = "yes"),
    partynode(8L, info = "no")))))
))

pastree <- party(pn, WeatherPlay)

plot(pastree)

newn1 <- partynode(id = 1L, split = lh1, kids = lapply(2L:4L, partynode))

tree_new <- party(newn1,
                  data = WeatherPlay,
                  fitted = data.frame(
                    "(fitted)" = fitted_node(newn1, data = WeatherPlay),
                    "(response)" = WeatherPlay$play,
                    check.names = FALSE),
                    terms = terms(play ~ ., data = WeatherPlay)
                  )
)

tree_new <- as.constparty(tree_new)

print(tree_new)
plot(tree_new)



