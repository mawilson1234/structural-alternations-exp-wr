source('Bayesian scripts/sim_functions.r')

name <- 'crossed_model_accuracy'

results <- read.csv('accuracy-data.csv') |>
	mutate(
		subject = as.factor(subject),
		item 	= as.factor(item)
	)

# constants for the number of simulations and groups
# are defined in sim_functions.r
cis <- run.simulations(
	data = results, 
	name = name,
	groups = c('voice.n', 'data_source.n', 'target_response.n'),
	formula = correct ~ voice.n * data_source.n * target_response.n +
		(1 + voice.n * target_response.n | subject:data_source.n) +
		(1 + data_source.n | item:voice.n:target_response.n),
	family = bernoulli()
)

save.ci.plots(cis, name=name)
save.ci.summary(cis, name=name)