# Load libraries
library(brms)
library(tools)
library(tidyr)
library(dplyr)
library(ggpubr)
library(R.utils)
library(bayesplot)

# Create directories to store results
plots.dir <- 'Plots/Bayesian'
models.dir <- 'Models/Bayesian'
dir.create(plots.dir, showWarnings=FALSE, recursive=TRUE)
dir.create(models.dir, showWarnings=FALSE, recursive=TRUE)

bayesplot_theme_set(theme_default(base_family = getOption('bayesplot.base_family', 'sans')))

posteriors_plot <- function(x, pars = '', labels = '', title = '', color_scheme = ''){
	if (color_scheme == ''){
		color_scheme_set('gray')
	} else {
		color_scheme_set(color_scheme)
	}
	
	if (title == ''){
		title <- deparse(substitute(x))
		title <- gsub('\\.', ' ', title)
		title <- paste(title, 'model posteriors')
	}
	
	if (pars == ''){
		pars <- rev(colnames(x)[grepl('^b\\_(?!Intercept)', colnames(x), perl = TRUE)])
	}
	
	if (labels == '') {
		labels <- gsub('^b\\_', '', pars)
		labels <- gsub('\\.n(:|$)', '\\1', labels)
		labels <- gsub('(\\.|\\_)', ' ', labels)
		labels <- gsub(':', ' × ', labels)
		labels <- toTitleCase(labels)
	} else if (length(labels) != length(pars)) {
		cat("Warning: the number of labels doesn't match the number of parameters to plot!")
		cat('Some parameters may not be labeled, or may be labeled incorrectly.')
	}
	
	# for some reason the pars are plotted in reverse, so we need to reverse
	# the labels to make it line up (???)
	plot <- mcmc_areas(x, pars=pars, prob=0.95, prob_outer=0.99, point_est='mean') +
		expand_limits(x=0) +
		scale_x_continuous('', n.breaks=8) +
		scale_y_discrete(labels=labels) +
		ggtitle(title)
	
	x_range <- range(plot$data$x)
	
	plot <- plot +
		theme(plot.margin = unit(c(0.2, 0.5, 0.2, 0.2), 'cm'))
	
	color_scheme_set()
	
	return (plot)
}

save_model_summaries <- function(
	models = list(), 
	filename = '', 
	overwrite = FALSE
){
	topsep <- paste0(paste0(rep('#', 81), collapse = ''), '\n')
	midsep <- paste0('\n', paste0(rep('-', 81), collapse = ''), '\n')
	botsep <- paste0('\n', paste0(rep('#', 81), collapse = ''), '\n')
	
	if (filename == '') {
		filename <- file.path(models.dir, 'model_summaries.txt')
	} else {
		filename <- file.path(models.dir, filename)
	}
	
	if (file.exists(filename) & !overwrite){
		cat(paste0('File "', filename, '" already exists. Use overwrite = TRUE to overwrite.\n'))
		return ()
	}
	
	text <- ''
	
	withOptions(
		{
			for (model_name in names(models)){
				cat('Processing model "', model_name, '"...\n', sep = '')
				text <- paste0(text, topsep, model_name, midsep)
				
				output <- capture.output(print(summary(models[[model_name]])))
				for(line in output){
					if (grepl('^Formula:|\\$(.*):', line)) {
						line <- gsub('\\s+', ' ', line)
						pad <- ifelse(grepl('^Formula:', line), paste0(rep(' ', nchar('Formula: ')), collapse = ''), '\t')
						line <- gsub('(\\+ \\([01](.*?)\\|(\\|)?(.*?)\\))', paste0('\n', pad, '\\1'), line)
					}
					text <- paste0(text, line, '\n')
				}
				text <- paste0(text, botsep)
			}
			
			sink(filename)
			cat(text)
			sink()
		},
		# Increase the maximum printing range for saving results to files to work correctly
		max.print = 100000,
		width = 10000
	)
}

save_pmcmc <- function(
	models = list(),
	filename = '',
	variable = '^b\\_',
	regex = TRUE,
	max_digits = 4
) {
	topsep <- paste0(paste0(rep('#', 81), collapse = ''), '\n')
	midsep <- paste0('\n', paste0(rep('-', 81), collapse = ''))
	botsep <- paste0('\n', paste0(rep('#', 81), collapse = ''), '\n')
	
	text <- ''
	
	printformat <- paste0('%.0', max_digits, 'f')
	
	if (filename == '') {
		filename <- file.path(models.dir, 'model_pmcmcs.txt')
	} else {
		filename <- file.path(models.dir, filename)
	}
	
	withOptions(
		{
			for (model_name in names(models)){
				model <- models[[model_name]]
				
				# posteriors <- as_draws_df(model, variable=variable, regex=regex)
				posteriors <- posterior_samples(model, pars=variable, fixed=(!regex))
				
				summary <- posteriors |>
					# select(-`.chain`, -`.iteration`, -`.draw`) |>
					pivot_longer(everything()) |>
					group_by(name) |>
					summarize_all(list(sum=\(x) sum(x > 0),length=length)) |>
					mutate(p_mcmc = sum/length) |>
					select(name, p_mcmc)
				
				text <- paste0(text, topsep, model_name, ' posteriors', midsep)
				
				for (i in seq_len(nrow(summary))) {
					effect <- gsub('^b\\_', '', summary[i,'name'][[1]])
					effect <- gsub('\\.n(:|$)', '\\1', effect)
					effect <- gsub(':', ' x ', effect)
					effect <- gsub('(\\.|\\_)', ' ', effect)
					effect <- toTitleCase(effect)
					pmcmc <- summary[i, 'p_mcmc'][[1]]
					dir <- ifelse(pmcmc > 0.5, ' < 0', ' > 0')
					pmcmc <- ifelse(pmcmc > 0.5, 1 - pmcmc, pmcmc)
					text <- paste0(text, '\n', sprintf(printformat, pmcmc), ': ', effect, dir)
				}
				text <- paste0(text, botsep)
			}
			
			text <- gsub('\\n$', '', text)
			
			sink(filename)
			cat(text)
			sink()
		},
		max.print = 100000,
		width = 10000
	)
}

save_model_plots <- function(models = list()) {	
	for (model_name in names(models)) {	
		# we have to put every plot in a list or else R flattens them out
		# and they're unusable. amazing behavior. great language
		model <- models[[model_name]]
		plots <- list()
		plot_types <- list('trace plot'='trace', 'marginal posteriors'='hist')
		variables <- list('(slopes)'='^b\\_', '(standard deviations)'='^sd\\_', '(correlations)'='^cor\\_')
		for (plot_type in names(plot_types)){
			for (variable in names(variables)){
				plots <- append(
					plots,
					list(mcmc_plot(
						model, 
						type=plot_types[[plot_type]], 
						pars=variables[[variable]]
						# variable=variables[[variable]], 
						# regex=TRUE
					) + ggtitle(paste(model_name, plot_type, variable)))
				)
			}
		}

		plots <- append(plots, list(pp_check(model, ndraws=100) + ggtitle(sprintf('%s PP check', model_name))))
		plots <- append(
					plots, 
					list(
						pp_check(model, ndraws=9, type='error_binned') + 
						ggtitle(sprintf('%s binned residuals PP check', model_name))
					)
				)
		
		# posteriors <- as_draws_df(model, variable='^b', regex=TRUE)
		posteriors <- posterior_samples(model, pars='^b')
		plots <- append(list(posteriors_plot(posteriors, title=sprintf('%s posteriors', model_name))), plots)
		
		# save the plots
		ggexport(
			plotlist = plots,
			filename = file.path(plots.dir, sprintf('%s_plots.pdf', gsub(' ', '_', tolower(model_name)))),
			width = 15
,			height = 12,
			scale = 0.9
		)
	}
}
