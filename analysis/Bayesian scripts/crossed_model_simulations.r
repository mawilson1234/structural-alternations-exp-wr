source('Bayesian scripts/sim_functions.r')

name <- 'crossed_model_accuracy'

results <- read.csv('accuracy-data.csv') |>
	mutate(
		subject = as.factor(subject),
		item 	= as.factor(item)
	)

priors_crossed <- c(
	set_prior('normal(0, 10)', class='Intercept'),
	set_prior('lkj(2)', class='cor'),
	set_prior('normal(0, 1)', class = 'b', coef=unlist(
		sapply(
			c(1,2,3,4),
			\(i) combn(
				c(
					'voice.n', 
					'data_source.n', 
					'target_response.n', 
					'seen_in_training.n'
				),
				m=i,
				FUN=\(x) paste(x, collapse=':')
			)
		)
	))
)

# constants for the number of simulations and groups
# are defined in sim_functions.r
cis <- run.simulations(
	data = results, 
	name = name,
	formula = correct ~ voice.n * data_source.n * target_response.n * seen_in_training.n +
		(1 + voice.n * target_response.n * seen_in_training.n | data_source.n:subject) +
		(1 + data_source.n | voice.n:target_response.n:seen_in_training.n:item),
	family = bernoulli(),
	prior = priors_crossed
)

save.ci.plots(cis, name=name)
save.ci.summary(cis, name=name)