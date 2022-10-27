source('Bayesian scripts/summary_functions.r')

models.dir <- 'Models/Bayesian'
dir.create(models.dir, showWarnings=FALSE, recursive=TRUE)

MAX_RT_IN_SECONDS <- 10
OUTLIER_RT_SDS <- 2

results <- read.csv('accuracy-data.csv') |>
	mutate(
		subject = as.factor(subject),
		item 	= as.factor(item)
	) |>
	filter(
		data_source == 'human',
		log.RT < log(MAX_RT_IN_SECONDS*1000)
	) |>
	group_by(subject) |>
	mutate(sd.log.RT = sd(log.RT)) |>
	filter(
		log.RT < mean(log.RT) + (OUTLIER_RT_SDS * sd.log.RT), 
		log.RT > mean(log.RT) - (OUTLIER_RT_SDS * sd.log.RT)
	) |>
	ungroup() |>
	select(-data_source, -data_source.n) |>
	droplevels()

priors_crossed_RT <- c(
	set_prior('normal(0, 10)', class='Intercept'),
	set_prior('lkj(2)', class='cor'),
	set_prior('normal(0, 1)', class = 'b', coef=unlist(
		sapply(
			c(1,2,3),
			\(i) combn(
				c(
					'voice.n', 
					'target_response.n', 
					'seen_in_training.n',
					'linear.n'
				),
				m=i,
				FUN=\(x) paste(x, collapse=':')
			)
		)
	))
)

brm.args <- list(
	iter=6500, chains=4, cores=4,
	backend='cmdstanr', threads=threading(4),
	control=list(adapt_delta=0.99),
	seed=425, refresh=1
)

models <- list()

models['Crossed model (RTs)'] <- do.call(brm, append(brm.args, list(
	formula = RT ~ voice.n * target_response.n * seen_in_training.n * linear.n +
		(1 + voice.n * target_response.n * seen_in_training.n | linear.n:subject) +
		(1 + linear.n | voice.n:target_response.n:seen_in_training.n:item),
	data = results,
	family = lognormal(),
	prior = priors_crossed_RT,
	file = file.path(models.dir, 'crossed_model_RTs.rds')
))) |> list()

save_model_summaries(
	models,
	filename='crossed_model_RTs_summary.txt', 
	overwrite=TRUE
)

save_pmcmc(
	models,
	filename='crossed_model_RTs_pmcmcs.txt'
)

save_model_plots(models)