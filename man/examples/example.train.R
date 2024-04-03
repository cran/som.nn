## get example data and add class labels:
data(iris)
species <- iris$Species

## train with default radius = diagonal / 2:
rlen <- 500
som <- som.nn.train(iris, class.col = "Species", kernel = "internal",
                    xdim = 15, ydim = 9, alpha = 0.2, len = rlen, 
                    norm = TRUE, toroidal = FALSE)


## continue training with different alpha and radius;
som <- som.nn.continue(som, iris, alpha = 0.02, len=500, radius = 5)
som <- som.nn.continue(som, iris, alpha = 0.02, len=500, radius = 2)

## predict some samples:
unk <- iris[,!(names(iris) %in% "Species")]

setosa <- unk[species=="setosa",]
setosa <- setosa[sample(nrow(setosa), 20),]

versicolor <- unk[species=="versicolor",]
versicolor <- versicolor[sample(nrow(versicolor), 20),]

virginica <- unk[species=="virginica",]
virginica <- virginica[sample(nrow(virginica), 20),]

p <- predict(som, unk)
head(p)

## plot:
plot(som)
dev.off()
plot(som, predict = predict(som, setosa))
plot(som, predict = predict(som, versicolor), add = TRUE, pch.col = "magenta", pch = 17)
plot(som, predict = predict(som, virginica), add = TRUE, pch.col = "white", pch = 8)

