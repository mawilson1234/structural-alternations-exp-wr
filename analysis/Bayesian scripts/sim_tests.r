library(plyr)
source('Bayesian scripts/sim_functions.r')

results <- read.csv('accuracy-data.csv') |>
	mutate(
		subject = as.factor(subject),
		item 	= as.factor(item)
	)

priors_list = list(
	crossed_model_accuracy_flat_priors = c(
			set_prior('', class='Intercept'),
			set_prior('lkj(2)', class='cor'),
			set_prior('', class = 'b', coef=unlist(
				sapply(
					c(1,2,3),
					\(i) combn(
						c(
							'voice.n', 
							'data_source.n', 
							'target_response.n'
						),
						m=i,
						FUN=\(x) paste(x, collapse=':')
					)
				)
			)),
			set_prior('', class = 'sd')
		),
	crossed_model_accuracy_wide_priors_sd_01 = c(
			set_prior('normal(0, 10)', class='Intercept'),
			set_prior('lkj(2)', class='cor'),
			set_prior('normal(0, 10)', class = 'b', coef=unlist(
				sapply(
					c(1,2,3),
					\(i) combn(
						c(
							'voice.n', 
							'data_source.n', 
							'target_response.n'
						),
						m=i,
						FUN=\(x) paste(x, collapse=':')
					)
				)
			)),
			set_prior('normal(0, 1)', class = 'sd')
		)
)

# constants for the number of simulations and groups
# are defined in sim_functions.r
l_ply(
	names(priors_list),
	\(name) {
		cis <- run.simulations(
			data = results, 
			name = name,
			groups = c('voice.n', 'data_source.n', 'target_response.n'),
			priors = priors_list[[name]],
			formula = correct ~ voice.n * data_source.n * target_response.n +
				(1 + voice.n * target_response.n | subject:data_source.n) +
				(1 + data_source.n | item:voice.n:target_response.n),
			family = bernoulli()
		)
		
		save.ci.plots(cis, name=name)
		save.ci.summary(cis, name=name)
	}
)