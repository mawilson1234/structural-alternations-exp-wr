suppressMessages(library(plyr))
suppressMessages(library(lme4))
suppressMessages(library(brms))
suppressMessages(library(doRNG))
suppressMessages(library(foreach))
suppressMessages(library(ggplot2))
suppressMessages(library(lmerTest))
suppressMessages(library(doFuture))
suppressMessages(library(parallel))
suppressMessages(library(gridExtra))
suppressMessages(library(tidyverse))
suppressMessages(library(RhpcBLASctl))

blas_set_num_threads(1)

registerDoFuture()
registerDoRNG()
n.cores <- as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK"))
if (is.na(n.cores)) {
	n.cores <- 1
}

cat(sprintf('Using %d cores\n', n.cores))
plan(multicore, workers = n.cores)

beta_ci <- function(y, ci=0.95) {
	alpha <- sum(y) + 1
	beta <- length(y) - sum(y) + 1
	lower_bound <- (1-ci)/2
	upper_bound <- ci+lower_bound
	qs <- c(qbeta(lower_bound, alpha, beta), qbeta(upper_bound, alpha, beta))
	df <- data.frame(qs) |> t()
	colnames(df) <- c('ymin', 'ymax')
	rownames(df) <- NULL
	df <- cbind(df,data.frame(y=mean(y)))
	return(df)
}

results <- read.csv('accuracy-data.csv')

freq.results.dir <- file.path('Models', 'Frequentist simulations', '30 subjects')
bayes.results.dir <- file.path('Models', 'Bayesian simulations', '30 subjects')
dir.create(freq.results.dir, showWarnings = FALSE, recursive = TRUE)
dir.create(bayes.results.dir, showWarnings = FALSE, recursive = TRUE)

cat('Getting original frequentist model\n')
if (file.exists(file.path(freq.results.dir, 'original_model.rds'))) {
	model.freq <- readRDS(file.path(freq.results.dir, 'original_model.rds'))
} else {
	model.freq <- glmer(
		data = results,
		formula = correct ~ voice.n * data_source.n * target_response.n +
			(1 + voice.n * target_response.n || subject:data_source.n) +
			(1 + data_source.n || item:voice.n:target_response.n),
		family = binomial(link = 'logit'),
		control = glmerControl(
				optimizer = 'bobyqa',
				optCtrl = list(maxfun = 1e6)
			),
		verbose = 2
	)
	
	saveRDS(model.freq, file.path(freq.results.dir, 'original_model.rds'))
}

priors <- c(
			set_prior('normal(0, 10)', class='Intercept'),
			set_prior('lkj(2)', class='cor'),
			set_prior('normal(0, 10)', class = 'b', coef=unlist(
				sapply(
					c(1,2,3),
					\(i) combn(
						c('voice.n', 'data_source.n', 'target_response.n'),
						m=i,
						FUN=\(x) paste(x, collapse=':')
					)
				)
			)),
			set_prior('normal(0, 10)', class = 'sd')
		)

cat('Getting original Bayesian model\n')
model.bayes <- brm(
	data = results,
	formula = correct ~ voice.n * data_source.n * target_response.n +
		(1 + voice.n * target_response.n | subject:data_source.n) +
		(1 + data_source.n | item:voice.n:target_response.n),
	family = bernoulli(),
	prior = priors,
	# 650 is the minimum amount required to get good Rhats.
	# we want more for the actual models, but for simulations
	# it's too expensive
	iter = 650, 
	chains = 4,
	cores = 4,
	refresh = 0,
	silent = 2,
	backend = 'cmdstanr',
	threads = threading(4, static = TRUE),
	control = list(adapt_delta = 0.99),
	seed = 425,
	file = file.path(bayes.results.dir, 'original_model.rds')
)

simulate.with.n.subjects <- function(
	model,
	n.subjects.per.group,
	file.name = 'model.rds',
	save.model = FALSE
) {
	if (class(model) == 'glmerMod') {
		original.data <- getData(model)
	} else if (class(model) == 'brmsfit') {
		original.data <- model$data |>
			select(!contains(':'))
	}
		
	original.data <- original.data |>
		mutate(subject = as.numeric(as.character(subject)))
	
	n.current.per.group <- original.data |>
		select(subject, data_source.n) |>
		distinct() |>
		group_by(data_source.n) |>
		summarize(n = n()) |>
		pull(n) |>
		unique()
	
	if (all(n.current.per.group == n.subjects.per.group)) {
		warning(paste0(
			'The original data has the number of participants requested per group. ',
			'Instead of using the original data, ', n.subjects.per.group,
			" participants' data will be simulated (if necessary) and randomly drawn."
		))
		
		n.current.per.group <- 0
	}
	
	sim.data <- data.frame()
	
	while (!all(n.current.per.group == n.subjects.per.group)) {
		if (class(model) == 'glmerMod') {
			predictions <- simulate(model, nsim = 1) |>
				pull(sim_1)
		} else if (class(model) == 'brmsfit') {
			predictions <- posterior_predict(model.bayes)
			row <- sample(seq_len(nrow(predictions)), 1)
			predictions <- predictions[row,]
		}
		
		new_data <- original.data |>
			mutate(
				correct = as.logical(predictions),
				subject = subject + max(original.data$subject, sim.data$subject)
			)
		
		sim.data <- rbind(sim.data, new_data)
		
		human.subjects <- sim.data |>
			filter(data_source.n == 0.5) |>
			pull(subject) |>
			unique()
		
		bert.subjects <- sim.data |>
			filter(data_source.n == -0.5) |>
			pull(subject) |>
			unique()
		
		subjects <- sort(c(
			sample(human.subjects, size=min(n.subjects.per.group, length(human.subjects))), 
			sample(bert.subjects,  size=min(n.subjects.per.group, length(bert.subjects)))
		))
		
		sim.data <- sim.data |>
			filter(subject %in% subjects)
		
		n.current.per.group <- sim.data |>
			select(subject, data_source.n) |>
			distinct() |>
			group_by(data_source.n) |>
			summarize(n = n()) |>
			pull(n) |> 
			unique()
	}
	
	if (class(model) == 'glmerMod') {
		if (file.exists(file.name)) {
			new.model <- readRDS(file.name)
		} else {
			new.model <- glmer(
				data = sim.data,
				formula = formula(model),
				family = family(model),
				control = glmerControl(
						optimizer = 'bobyqa',
						optCtrl = list(maxfun = 1e6)
					)
			)
			
			if (save.model) {
				saveRDS(new.model, file.name)
			}
		}
		
		coefs <- summary(new.model)$coefficients
		coefs.names <- rownames(coefs)
		coefs <- coefs |>
			as_tibble() |>
			mutate(param = coefs.names) |>
			select(param, everything()) |>
			mutate(sig = `Pr(>|z|)` < 0.05)	
	} else if (class(model) == 'brmsfit') {
		update.args <- list(
			object = model,
			newdata = sim.data,
			silent = 2,
			refresh = 0,
			threads = threading(1, static = TRUE)
		)
		
		if (save.model) {
			update.args <- append(update.args, list(file = file.name))
		}
		
		new.model <- do.call(update, update.args)
		
		pmcmcs <- posterior_samples(new.model) |>
			select(starts_with('b_')) |>
			pivot_longer(everything(), names_to = 'param', values_to = 'value') |>
			mutate(
				param = gsub('^b_', '', param) |>
					fct_relevel(gsub('^b_', '', unique(param)))
			) |>
			group_by(param) |>
			summarize(pmcmc = length(value[value > 0])/length(value)) |>
			mutate(
				pmcmc = case_when(pmcmc > 0.5 ~ 1 - pmcmc, TRUE ~ pmcmc),
				sig.pmcmc = pmcmc < 0.05
			) 
		
		cis <- fixef(new.model)
		cis.names <- rownames(cis)
		cis <- cis |>
			as_tibble() |>
			mutate(
				param = cis.names |>
					fct_relevel(cis.names)
			) |>
			select(param, everything()) |>
			mutate(sig.ci = (Q2.5 < 0) == (Q97.5 < 0))
		
		coefs <- left_join(pmcmcs, cis)
	}
	
	return (coefs)
}

simulate.n.times.with.group.sizes <- function(
	model, 
	n.times = 100, 
	group.sizes, 
	file.name.prefix = 'model',
	save.all = FALSE
) {
	if (class(model) == 'glmerMod') {
		results.dir <- freq.results.dir
	} else if (class(model) == 'brmsfit') {
		results.dir <- bayes.results.dir
	}
	
	dir.create(results.dir, showWarnings = FALSE, recursive = TRUE)
	
	n.digits.hp <- max(nchar(as.character(group.sizes)))
	n.digits.i <- nchar(as.character(n.times))
	format_string <- paste0('%s_%0', n.digits.hp, 'd_hp_%0', n.digits.i, 'd.rds')
	
	coefs <- foreach(x = group.sizes, .combine=rbind) %do% {
		res <- foreach(i = seq_len(n.times), .combine=rbind) %dorng% {
			df <- simulate.with.n.subjects(
					model = model, 
					n.subjects.per.group = x, 
					file.name = file.path(results.dir, sprintf(format_string, file.name.prefix, x, i)), 
					save.model = save.all
				) |>
				mutate(
					n.subjects.per.group = x,
					run.no = i
				)
			return (df)
		}
		return (res)
	}
	
	write.csv(coefs, file.path(results.dir, 'sim_coefs.csv'), row.names = FALSE)
	
	if (class(model) == 'glmerMod') {
		plots <- llply(
			unique(coefs$param),
			\(p) {
				coefs |>
					filter(param == !!p) |>
					ggplot(aes(x = n.subjects.per.group, y = as.numeric(sig))) +
					stat_summary(fun = mean, geom = 'line', linewidth = 1) +
					stat_summary(fun = mean, geom = 'point', cex = 2.5) +
					stat_summary(fun.data = beta_ci, geom = 'errorbar', width = 0.33) +
					geom_hline(yintercept = 0.8, linetype = 'dashed') +
					scale_x_continuous(
						'No. subjects per data source',
						breaks = coefs$n.subjects.per.group |> unique(),
						labels = coefs$n.subjects.per.group |> unique()
					) +
					scale_y_continuous(
						paste0('Pr. signficant (of ', max(coefs$run.no), ')'),
						breaks = seq(0, 1, 0.2),
						labels = seq(0, 1, 0.2)
					) +
					expand_limits(y = c(0, 1)) +
					ggtitle(paste0('Pr. significant estimates for ', p)) +
					theme(panel.grid.minor.x = element_blank())
			}
		)
	} else if (class(model) == 'brmsfit') {
		plots <- llply(
			unique(coefs$param),
			\(p) {
				coefs |>
					filter(param == !!p) |>
					pivot_longer(c(sig.pmcmc, sig.ci), names_to = 'Measure', values_to = 'sig') |>
					ggplot(aes(x = n.subjects.per.group, y = as.numeric(sig), color = Measure, group = Measure)) +
					geom_hline(yintercept = 0.8, linetype = 'dashed') +
					stat_summary(fun = mean, geom = 'line', linewidth = 1, position = position_dodge(width = 1)) +
					stat_summary(fun = mean, geom = 'point', cex = 2.5, position = position_dodge(width = 1)) +
					stat_summary(fun.data = beta_ci, geom = 'errorbar', width = 0.33, position = position_dodge(width = 1)) +
					scale_x_continuous(
						'No. subjects per data source',
						breaks = coefs$n.subjects.per.group |> unique(),
						labels = coefs$n.subjects.per.group |> unique()
					) +
					scale_y_continuous(
						paste0('Pr. signficant (of ', max(coefs$run.no), ')'),
						breaks = seq(0, 1, 0.2),
						labels = seq(0, 1, 0.2)
					) +
					scale_color_discrete(
						'Measure',
						breaks = c('sig.pmcmc', 'sig.ci'),
						labels = c(expression(italic('p')['MCMC']), '95% CI')
					) +
					expand_limits(y = c(0, 1)) +
					ggtitle(paste0('Pr. "significant" estimates for ', p)) +
					theme(panel.grid.minor.x = element_blank())
			}
		)
	}
	
	if (class(model) == 'glmerMod') {
		filename <- file.path(results.dir, paste0('freq_simulations_sig_plots.pdf'))
	} else if (class(model) == 'brmsfit') {
		filename <- file.path(results.dir, paste0('bayes_simulations_sig_plots.pdf'))
	}
	
	ggsave(
		plot = marrangeGrob(plots, nrow = 1, ncol = 1),
		filename = filename,
		device = 'pdf',
		width = 6,
		height = 4,
		scale = 1.1,
		units = 'in'
	)
	# marrangeGrob creates a blank file
	if (file.exists('Rplots.pdf')) {
		unlink('Rplots.pdf')
	}
	
	return (coefs)
}

n.times <- 1000
group.sizes <- c(30, 35, 40, 50)

cat('Simulating frequentist models\n')
freq.coefs <- simulate.n.times.with.group.sizes(
	model = model.freq,
	n.times = n.times,
	group.sizes = group.sizes,
	file.name.prefix = 'crossed_model_accuracy',
	save.all = TRUE
)

cat('Simulating Bayesian models\n')
bayes.coefs <- simulate.n.times.with.group.sizes(
	model = model.bayes,
	n.times = n.times,
	group.sizes = group.sizes,
	file.name.prefix = 'crossed_model_accuracy',
	save.all = TRUE
)