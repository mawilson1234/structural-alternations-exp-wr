source('Bayesian scripts/summary_functions.r')
library(ggdist)
library(stringr)
library(forcats)
library(gridExtra)

CI_RANGE <- 0.95
TARGET_CI_WIDTH <- 2

N_HUMAN_PARTICIPANTS_PER_RUN <- sort(c(35, seq(from=30, to=80, by=10)))
N_RUNS_PER_SIZE <- 10

plots.dir <- 'Plots/Bayesian simulations'
models.dir <- 'Models/Bayesian simulations'
dir.create(plots.dir, showWarnings=FALSE, recursive=TRUE)
dir.create(models.dir, showWarnings=FALSE, recursive=TRUE)

get.lists <- function(n.participants, n.runs, sample.from) {
	n.models <- length(sample.from)
	model.lists <- list()
	
	while (
		(
			!(length(unique(model.lists)) == n.runs) &
			n.participants %% n.models != 0
		) | (
			!(length(model.lists) == n.runs) & 
			n.participants %% n.models == 0
		)
	) {
		model.lists <- vector(mode='list', length=n.runs)
		# if we can evenly divide the number of participants desired
		# by the number of models, there's only one way to run the model
		if (n.participants %% n.models == 0) {
			for (i in seq_along(model.lists)) {
				while (length(model.lists[[i]]) != n.participants) {
					if (is.null(model.lists[[i]])) {
						model.lists[[i]] <- sample.from
					} else {
						model.lists[[i]] <- c(model.lists[[i]], sample.from)
					}
				}
			}
		} else if (n.participants > n.models) {
			for (i in seq_along(model.lists)) {
				while ((n.participants - length(model.lists[[i]])) >= n.models) {
					if (is.null(model.lists[[i]])) {
						model.lists[[i]] <- sample.from
					} else {
						model.lists[[i]] <- c(model.lists[[i]], sample.from)
					}
				}
			}
		}
		
		for (i in seq_along(model.lists)) {
			while (length(model.lists[[i]]) != n.participants) {
				if (is.null(model.lists[[i]])) {
					model.lists[[i]] <- sort(
						sample(
							sample.from, 
							size=n.participants
						)
					)
				} else {
					original.n <- length(model.lists[[i]])
					while (
						(model.lists[i] %in% model.lists[-i]) |
						(i == length(model.lists) & length(model.lists[[i]]) != n.participants)
					) {
						model.lists[[i]] <- model.lists[[i]][1:original.n]
						model.lists[[i]] <- c(model.lists[[i]], 
							sort(
								sample(
									sample.from, 
									size=n.participants - length(model.lists[[i]])
								)
							)
						)
					}
				}
			}
		}
	}
	
	return (model.lists)
}

brm.args <- list(
	iter=6500, chains=4, cores=4,
	backend='cmdstanr', threads=threading(4),
	control=list(adapt_delta=0.99),
	seed=425, refresh=1
)

get.duplicated.data <- function(
	data, 
	model.list, 
	human.list, 
	n.participants
) {
	n.each.model <- table(model.list)
	n.each.model <- n.each.model[n.each.model > 0]
	
	n.each.human <- table(human.list)
	n.each.human <- n.each.human[n.each.human > 0]
	
	results.with.duplicates <- data.frame(matrix(ncol=length(colnames(data)), nrow=0))
	colnames(results.with.duplicates) <- colnames(data)
	
	dupe_string <- ''
	while (length(results.with.duplicates$subject |> unique()) < (n.participants * 2)) {
		models.to.duplicate <- names(n.each.model)[n.each.model > 0]
		humans.to.duplicate <- names(n.each.human)[n.each.human > 0]
		to.duplicate <- c(humans.to.duplicate, models.to.duplicate)
		
		results.with.duplicates <- rbind(
				results.with.duplicates,
				data |> 
					filter(subject %in% to.duplicate) |>
					mutate(
						subject = case_when(
							subject %in% unique(results.with.duplicates$subject) ~ paste0(as.character(subject), dupe_string),
							TRUE ~ as.character(subject)
						)
					)
			)
		
		n.each.model <- n.each.model - 1
		n.each.model <- n.each.model[n.each.model > 0]
		
		n.each.human <- n.each.human - 1
		n.each.human <- n.each.human[n.each.human > 0]
		dupe_string <- paste0(dupe_string, '_d')
	}
	
	results.with.duplicates <- results.with.duplicates |>
		mutate(subject = as.factor(subject))
	
	return (results.with.duplicates)
}

run.simulations <- function(data, name, ...) {
	cis <- data.frame(
		model.number = integer(0),
		effect = character(0),
		n.humans = integer(0),
		ci.upper = numeric(0),
		ci.lower = numeric(0),
		median = numeric(0)
	)
	
	human.subject.ids <- data |>
		filter(data_source == 'human') |>
		droplevels() |>
		pull(subject) |> 
		unique()
	
	model.subject.ids <- data |>
		filter(data_source == 'BERT') |>
		pull(subject) |> 
		unique() |>
		droplevels()
	
	for (n.participants in N_HUMAN_PARTICIPANTS_PER_RUN) {
		model.lists <- get.lists(
			n.participants=n.participants, 
			n.runs=N_RUNS_PER_SIZE, 
			sample.from=model.subject.ids
		)
		
		human.lists <- get.lists(
			n.participants=n.participants,
			n.runs=N_RUNS_PER_SIZE,
			sample.from=human.subject.ids
		)
		
		if (length(unique(model.lists)) == 1 & length(unique(human.lists)) == 1) {
			model.lists <- unique(model.lists)
			human.lists <- unique(human.lists)
		}
		
		models <- list()
		for (i in seq_along(model.lists)) {
			cat(sprintf('Fitting %s with %02d human participants #%02d\n', gsub('_|\\.', ' ', name), n.participants, i))
			model_name <- sprintf('%s with %02d human participants #%02d', gsub('_', ' ', toTitleCase(name)), n.participants, i)
			results.with.duplicates <- get.duplicated.data(
					data=data, 
					model.list=model.lists[[i]], 
					human.list=human.lists[[i]], 
					n.participants=n.participants
				)
			
			models[model_name] <- do.call(brm, append(brm.args, list(
				...,
				data = results.with.duplicates |> filter(data_source == 'human' | subject %in% model.lists[[i]]),
				file = file.path(models.dir, sprintf(paste0(name, '_%02d_hp_%02d.rds'), n.participants, i))
			))) |> list()
			
			model.cis <- models[[model_name]] |>
				posterior_samples(pars='^b_') |>
				pivot_longer(everything()) |>
				group_by(name) |>
				median_qi(value, width=CI_RANGE) |>
				select(name:value.upper) |>
				rename(
					effect = name,
					median = value,
					ci.lower = value.lower,
					ci.upper = value.upper
				) |>
				mutate(
					model.number = i,
					n.humans = n.participants
				)
			
			cis <- rbind(cis, model.cis)
		}
		
		save_model_summaries(
			models,
			filename=sprintf(paste0(name, '_summaries_%02d_hp.txt'), n.participants), 
			overwrite=TRUE
		)
	}
	
	cis <- cis |>
		mutate(
			`Overlaps 0?` = case_when(
					((ci.upper > 0 & ci.lower > 0) | (ci.upper < 0 & ci.lower < 0)) ~ 'No overlap',
					TRUE ~ 'Overlap'
				),
			`Overlaps 0?` = fct_relevel(`Overlaps 0?`, 'Overlap', 'No overlap'),
			width = case_when(
					ci.upper - ci.lower > TARGET_CI_WIDTH ~ paste0('>', TARGET_CI_WIDTH),
					TRUE ~ paste0('<=', TARGET_CI_WIDTH)
				),
			width = fct_relevel(width, paste0('>', TARGET_CI_WIDTH), paste0('<=', TARGET_CI_WIDTH))
		)
	
	return (cis)
}

save.ci.plots <- function(cis, name) {
	cis.plots <- list()
	
	effects <- cis |>
		pull(effect) |>
		unique() |>
		sapply(\(e) ifelse(grepl('Intercept', e), '0000', paste0(sprintf('%04d', str_count(e, ':')), '_', e))) |>
		sort() |> 
		names()
	
	for (effect in effects) {
		# do a plot for each effect
		ylab <- gsub('^b\\_', '', effect) %>%
			gsub('\\.n(:|$)', '\\1', .) %>%
			gsub('(\\.|\\_)', ' ', .) %>%
			gsub(':', ' × ', .) %>%
			toTitleCase(.)
		
		cis.plots[effect] <- list(
			cis |>
				filter(effect == !!effect) |>
				ggplot(aes(
					x=as.factor(model.number), 
					ymin=ci.lower, ymax=ci.upper, 
					color=`Overlaps 0?`,
					linetype=width
				)) +
				geom_hline(yintercept=0, color='white') +
				geom_linerange(size=1) +
				geom_point(aes(x=as.factor(model.number), y=median), cex=2.5) +
				xlab('Simulation no.') +
				scale_y_continuous(paste0(CI_RANGE*100, '% CI of ', ylab)) +
				scale_linetype_discrete(paste0('Width >', TARGET_CI_WIDTH, '?')) +
				scale_color_manual(
					breaks = c('Overlap', 'No overlap'),
					values = c('#F8766D', '#00BFC4')
				) + 
				facet_grid(paste(n.humans, 'human participants') ~ .)
			)
	}
	
	ggsave(
		plot=marrangeGrob(cis.plots, nrow=1, ncol=1),
		filename=file.path(plots.dir, paste0(name, '_simulations_cis_plots.pdf')),
		device='pdf',
		width=11,
		height=10.625,
		scale=1,
		units='in'
	)
}

save.ci.summary <- function(cis, name) {
	write.csv(cis, file.path(models.dir, paste0(name, '_simulations_cis.csv')), row.names=FALSE)
	cis.summary <- cis |> 
		group_by(effect, n.humans, `Overlaps 0?`, width) |>
		summarize(pr.of.runs.in.group = n()/N_RUNS_PER_SIZE)
	
	sink(file.path(models.dir, paste0(name, '_simulations_cis_summary.txt')))
	cis.summary
	sink()
}