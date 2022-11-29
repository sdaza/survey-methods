population = rnorm(10000)
mean(population)

unbiased = NULL

for (i in 1:1000) {
    unbiased = c(unbiased, mean(sample(population, 100)))
}

sd(unbiased)
summary(unbiased)

hist(unbiased)

biased = NULL
for (i in 1:1000) {
    biased = c(
        biased, 
        mean(
            c(sample(population[population>0.3], 60), sample(population[population<=0.3], 40))
        )
        )
}

hist(biased)
summary(biased)
sd(biased)
