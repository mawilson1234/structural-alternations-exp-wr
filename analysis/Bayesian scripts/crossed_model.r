source('Bayesian scripts/summary_functions.r')

models.dir <- 'Models/Bayesian'
dir.create(models.dir, showWarnings=FALSE, recursive=TRUE)

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

brm.args <- list(
	iter=6500, chains=4, cores=4,
	backend='cmdstanr', threads=threading(4),
	control=list(adapt_delta=0.99),
	seed=425, refresh=1
)

# generate a list of n.human.subjects integers
# within the range of the model.ids to
# fit the model using the same number of
# human and computer participants
n.human.subjects <- results |> 
	filter(data_source == 'human') |>
	pull(subject) |>
	unique() |> 
	length()

model.ids <- results |>
	filter(data_source == 'BERT') |>
	pull(subject) |>
	unique()

N_RUNS = 10

set.seed(425)
model.lists <- list()
while (!(length(unique(model.lists)) == N_RUNS)) {
	model.lists <- replicate(
		N_RUNS, 
		sort(
			sample(
				model.ids, 
				size=n.human.subjects
			)
		), 
		simplify=FALSE
	)
}


models <- list()
for (i in seq_along(model.lists)) {
	models[sprintf('Crossed model %02d', i)] <- do.call(brm, append(brm.args, list(
		formula = correct ~ voice.n * data_source.n * target_response.n * seen_in_training.n +
			(1 + voice.n * target_response.n * seen_in_training.n | data_source.n:subject) +
			(1 + data_source.n | voice.n:target_response.n:seen_in_training.n:item),
		data = results |> filter(data_source == 'human' | subject %in% model.lists[[i]]),
		family = bernoulli(),
		prior = priors_crossed,
		file = file.path(models.dir, sprintf('crossed_model_accuracy_%02d.rds', i))
	))) |> list()
}

save_model_summaries(
	models,
	filename='crossed_model_accuracy_summaries.txt', 
	overwrite=TRUE
)

save_pmcmc(
	models,
	filename='crossed_model_accuracy_pmcmcs.txt'
)

save_model_plots(models)