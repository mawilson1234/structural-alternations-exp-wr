# Load libraries
suppressMessages(library(brms))
suppressMessages(library(tools))
suppressMessages(library(tidyr))
suppressMessages(library(dplyr))
suppressMessages(library(purrr))
suppressMessages(library(ggpubr))
suppressMessages(library(forcats))
suppressMessages(library(stringr))
suppressMessages(library(R.utils))
suppressMessages(library(bayesplot))

# brms will not accept essentially any
# symbol in a variable name, so we're
# stuck with this. it also won't 
# take double underscores.
NESTING_SEPARATOR <- '_i_n_'
LEVEL_SEPARATOR <- '_a_t_'

# for filtering RTs
MAX_RT_IN_SECONDS <- 10
OUTLIER_RT_SDS <- 2

# Create directories to store results
plots.dir <- 'Plots/Bayesian'
models.dir <- 'Models/Bayesian'
dir.create(plots.dir, showWarnings=FALSE, recursive=TRUE)
dir.create(models.dir, showWarnings=FALSE, recursive=TRUE)

bayesplot_theme_set(theme_default(base_family = getOption('bayesplot.base_family', 'sans')))

brm.args <- list(
	iter=6500, 
	chains=4, 
	cores=4,
	refresh=650,
	backend='cmdstanr', 
	threads=threading(4, static=TRUE),
	control=list(adapt_delta=0.99),
	seed=425
)

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
		labels <- gsub('^b_', '', pars)
		labels <- gsub('\\.n(_|:|$)', '\\1', labels)
		labels <- gsub(NESTING_SEPARATOR, ' (in) ', labels)
		labels <- gsub(LEVEL_SEPARATOR, ' @ ', labels)
		labels <- gsub('(\\.|_)', ' ', labels)
		labels <- gsub(':', ' × ', labels)
		labels <- toTitleCase(labels)
		labels <- gsub('Svo', 'SVO', labels)
		labels <- gsub('Ovs', 'OVS', labels)
		labels <- gsub(' Tr ', ' T.R. ', labels)
		labels <- gsub(' Ds ', ' D.S. ', labels)
		labels <- gsub(' Sit ', ' S.i.T. ', labels)
		labels <- gsub(' v ', ' V. ', labels)
		labels <- gsub(' l ', ' L. ', labels)
		labels <- gsub('Bert', 'BERT', labels)
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
	variable = '^b_',
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
					mutate(
						name = as.factor(name) |>
							fct_relevel(colnames(posteriors))
					) |> 
					group_by(name) |>
					summarize(
						across(
							everything(), 
							list(sum=\(x) sum(x > 0), length=length)
						)
					) |>
					mutate(p_mcmc = value_sum/value_length) |>
					select(name, p_mcmc)
				
				text <- paste0(text, topsep, model_name, ' posteriors', midsep)
				
				for (i in seq_len(nrow(summary))) {
					effect <- gsub('^b_', '', summary[i,'name'][[1]])
					effect <- gsub('\\.n(_|:|$)', '\\1', effect)
					effect <- gsub(NESTING_SEPARATOR, ' (in) ', effect)
					effect <- gsub(LEVEL_SEPARATOR, ' @ ', effect)
					effect <- gsub('(\\.|_)', ' ', effect)
					effect <- gsub(':', ' × ', effect)
					effect <- toTitleCase(effect)
					effect <- gsub('Svo', 'SVO', effect)
					effect <- gsub('Ovs', 'OVS', effect)
					effect <- gsub(' Tr ', ' Target response ', effect)
					effect <- gsub(' Ds ', ' Data source ', effect)
					effect <- gsub(' Sit ', ' Seen in Training ', effect)
					effect <- gsub(' v ', ' Voice ', effect)
					effect <- gsub(' l ', ' Linear ', effect)
					effect <- gsub('Bert', 'BERT', effect)
					pmcmc <- summary[i, 'p_mcmc'][[1]]
					dir <- ifelse(pmcmc > 0.5, ' < 0', ' > 0')
					pmcmc <- ifelse(pmcmc > 0.5, 1 - pmcmc, pmcmc)
					sig <- ifelse(pmcmc < 0.05, '*', '')
					sig <- ifelse(pmcmc < 0.001, '**', sig)
					sig <- ifelse(pmcmc < 0.0001, '***', sig)
					text <- paste0(text, '\n', sprintf(printformat, pmcmc), ': ', effect, dir, ' ', sig)
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

save_model_plots <- function(models = list(), plots.dir = '.') {	
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

		plots <- append(plots, list(pp_check(model, nsamples=100) + ggtitle(sprintf('%s PP check', model_name))))
		
		if (model$family$family == 'bernoulli') {
			type <- 'error_binned'
			title <- '%s binned residuals PP check'
		} else {
			type <- 'scatter_avg'
			title <- '%s scatterplot PP check'
		}
		
		plots <- append(
					plots, 
					list(
						pp_check(model, type=type) + 
						ggtitle(sprintf(title, model_name))
					)
				)
		
		# posteriors <- as_draws_df(model, variable='^b', regex=TRUE)
		posteriors <- posterior_samples(model, pars='^b')
		plots <- append(list(posteriors_plot(posteriors, title=sprintf('%s posteriors', model_name))), plots)
		
		# save the plots
		ggexport(
			plotlist = plots,
			filename = file.path(plots.dir, sprintf('%s_plots.pdf', gsub(' |\\.', '_', tolower(model_name)))),
			width = 15,
			height = 12,
			scale = 0.9
		)
	}
}

get_nested_data <- function(data, cols, gcols, out_of_group_value = 0) {	
	for (c in cols) {
		other_cols <- gcols[which(cols != c)]
		cat(sprintf('Getting nestings for %s within levels of %s', c, paste0(other_cols, collapse=', ')), '\n')
		
		comb <- lapply(
				seq_along(other_cols),
				\(i) combn(
					other_cols,
					m=i
				)
			)
		
		comb <- lapply(
					comb,
					\(x) {
						x <- as.matrix(x)
						lapply(
							seq_len(ncol(x)),
							\(i) x[,i]
						)
					}
				)
		
		comb <- flatten(comb)
		
		for (co in comb) {
			cat(sprintf('Working on %s', paste0(co, collapse=' X ')), '\n')
			groups <- data |>
				select_at(co) |>
				distinct()
			
			for (i in seq_len(nrow(groups))) {
				group <- groups[i,]
				# R/brms won't accept any non-word character in a variable name except for a dot or a single underscore
				group_s <- tolower(gsub('(?:(?![.])([[:punct:]]| ))+', '_', paste(paste0(gsub('(\\w)(.*?)(_|$)', '\\1', names(groups))), group, sep=LEVEL_SEPARATOR, collapse=NESTING_SEPARATOR), perl=TRUE))
				group_s <- remove.double.underscores(group_s)
				# brms won't take trailing underscores in variable names
				group_s <- gsub('_$', '', group_s)
				cat(sprintf(paste0('Working on group nesting %s', NESTING_SEPARATOR, '%s'), c, group_s), '\n')
				nested_name <- paste0(c, NESTING_SEPARATOR, group_s)
				
				data <- data |> 
					rowwise() |>
					mutate(
						`__tmp__` = tolower(gsub('(?:(?![.])([[:punct:]])| )+', '_', paste(gsub('(\\w)(.*?)(_|$)', '\\1', names(groups)), c(!!!rlang::syms(names(groups))), sep=LEVEL_SEPARATOR, collapse=NESTING_SEPARATOR), perl=TRUE)) |>
							remove.double.underscores() %>%
							gsub('_$', '', .),
						'{nested_name}' := case_when(
							`__tmp__` == group_s ~ !!!rlang::syms(c),
							TRUE ~ out_of_group_value
						)
					) |> 
					select(-`__tmp__`)
			}
		}
		cat('\n')
	}
	
	return (data)
}

remove.double.underscores <- function(s) {
	while ('__' %in% s) {
		s <- gsub('__', '_', s)
	}
	return (s)
}

re.findall <- function(regex, str) {
	# lol wut
	return (regmatches(str, gregexpr(regex, str, perl=TRUE)))
}

get.nested.model.formulae <- function(
	all.nested.effects, 
	all.effect.cols,
	depvar, 
	ranefs, 
	ranef.nestings = list()
) {
	# gets formulae for all nestings of predictors in all.nested effects
	# this may not work for other designs, but it works for ours! 
	#
	# good luck if you want to change this, string manipulation in R is not fun
	# 
	# 	params:
	#  		all.nested.effects (string vector): a vector of strings contained all of the nested effects
	#											these are the column names added by get_nested_data
	#		depvar (string)					  : the name of the dependent variable
	#		ranefs (string vector)			  : the names of the random effects groups
	# 		ranef.nestings (list[str,str])	  : a list mapping random effects to
	#											the variables within which they are nested
	#
	#   returns:
	#		list[str,formula]				  : a list mapping abbreviated group names
	#										    to the formula for that nested model
	#
	nested.model.formulae <- data.frame(effect = all.nested.effects) |>
		as_tibble() |> 
		filter(grepl(NESTING_SEPARATOR, effect, fixed=TRUE)) |>
		mutate(n.effects = str_count(effect, NESTING_SEPARATOR)) |>
		arrange(n.effects, effect) |>
		rowwise() |>
		mutate(
			nested = re.findall(paste0('^(.*?)(?=', NESTING_SEPARATOR, ')'), effect)[[1]],
			group = paste0(re.findall(paste0('(?<=', NESTING_SEPARATOR, ')(.*?)(?=', LEVEL_SEPARATOR, '|$)'), effect)[[1]], collapse=NESTING_SEPARATOR),
			added.effects = all.effect.cols[
								grepl(gsub(NESTING_SEPARATOR, '|', group), gsub('(\\w)(.*?)(_|$)', '\\1', all.effect.cols))
							] |>
							paste0(collapse=' * '),
			crossed.effects = all.effect.cols[
								!grepl(nested, all.effect.cols) & 
								!grepl(gsub(NESTING_SEPARATOR, '|', group), gsub('(\\w)(.*?)(_|$)', '\\1', all.effect.cols))
							] |>
							paste0(collapse=' * ')
		) |>
		ungroup() |> 
		group_by(nested, n.effects, group) |>
		mutate(
			fixef = paste0(
						ifelse(unique(crossed.effects) != '', '(', ''),
						paste0('`', paste0(effect, collapse='` + `'), '`'),
						ifelse(unique(added.effects) != '', paste0(' + `', gsub('( \\* | \\+ |:)', '`\\1`', unique(added.effects)), '`'), ''),
						ifelse(unique(crossed.effects) != '', paste0(')', paste0(' * `', gsub('( \\* | \\+ |:)', '`\\1`', unique(crossed.effects)), '`')), '')
					)
		)
	
	# format strings to add backticks
	nested.model.formulae <- nested.model.formulae |>
		group_by(nested, n.effects, group) |>
		mutate(nested.effects = paste0('`', paste0(unique(effect), collapse='` + `'), '`'),) |>
		ungroup() |>
		select(-effect, -n.effects) |> 
		distinct() |>
		rowwise() |>
		mutate(
			added.effects = gsub('``', '', paste0('`', gsub(' \\* ', '` * `', added.effects), '`')),
			crossed.effects = gsub('``', '', paste0('`', gsub(' \\* ', '` * `', crossed.effects), '`')),
		)
	
	get.ranef.string <- function(
		ranef.nesting, 
		added.effects, 
		crossed.effects, 
		nested.effects
	) {
		any.exist <- function(v) {
			return (length(v) > 0)
		}
		
		startsWithAny <- function(source, target) {
			# returns a logical vector indicating
			# which elements of source start with
			# any element in target
			indices <- c()
			for (i in seq_along(source)) {
				s <- source[i]
				indices[i] <- FALSE
				for (t in target) {
					if (startsWith(s, t)) {
						indices[i] <- TRUE
						break
					}
				}
			}
			
			return (indices)
		}
		
		get.lhs <- function(
			ranef.nesting, 
			added.effects, 
			crossed.effects, 
			nested.effects
		) {
			get.effects.not.in.group.nesting <- function(nesting, effects) {
				effects.unique <- strsplit(gsub('`', '', effects), ' (\\+|\\*) ')[[1]]
				effects.unique <- gsub('(.*?)\\.n.*', '\\1.n', effects.unique)
				effects.unique <- unique(effects.unique)
				
				effects.in.nesting <- effects.unique[effects.unique %in% unlist(nesting)]
				if (any.exist(effects.in.nesting)) {
					effects.in.nesting <- paste0('`', effects.in.nesting)
					effects <- strsplit(effects, ' \\* ')[[1]]
					effects <- effects[!startsWithAny(effects, effects.in.nesting)]
					effects <- paste0(effects, collapse=' * ')
				}
				
				if (!effects == '') {
					return (effects)
				} else {
					return (character(0))
				}
			}
			
			# check where any nested fixed effects are in the nesting group
			# for the random effect
			# if so, they should not be added to the lhs, but to the rhs
			nested.effects.filtered <- get.effects.not.in.group.nesting(ranef.nesting, nested.effects)
			added.effects.filtered <- get.effects.not.in.group.nesting(ranef.nesting, added.effects)
			crossed.effects.filtered <- get.effects.not.in.group.nesting(ranef.nesting, crossed.effects)
			
			lhs.string <- ''
			if (any.exist(nested.effects.filtered)) {
				lhs.string <- ' + '
				if (any.exist(crossed.effects.filtered)) {
					lhs.string <- paste0(lhs.string, '(')
				}
				lhs.string <- paste0(lhs.string, nested.effects.filtered)
			}
			
			if (any.exist(added.effects.filtered)) {
				lhs.string <- paste0(lhs.string, ' + ', added.effects.filtered)
			}
			
			if (any.exist(nested.effects.filtered) & any.exist(crossed.effects.filtered)) {
				lhs.string <- paste0(lhs.string, ')')	
			}
			
			if (any.exist(crossed.effects.filtered)) {
				if (!any.exist(added.effects.filtered) & !any.exist(nested.effects.filtered)) {
					sep <- ' + '
				} else {
					sep <- ' * '
				}
				
				lhs.string <- paste0(lhs.string, sep, crossed.effects.filtered)
			}
			
			return (lhs.string)
		}
		
		get.rhs <- function(
			ranef.nesting,
			added.effects,
			crossed.effects,
			nested.effects
		) {
			get.effects.in.group.nesting <- function(nesting, effects, nested = TRUE) {
				effects <- strsplit(gsub('`', '', effects), ' (\\+|\\*) ')[[1]]
				effects.base <- gsub('(.*?)\\.n.*', '\\1.n', effects)
				
				effects.in.nesting <- effects[effects.base %in% unlist(nesting)]
				if (any.exist(effects.in.nesting)) {
					if (nested) {
						collapse = ' + '
					} else {
						collapse = ':'
					}
					
					effects.in.nesting <- paste0(paste0('`', effects.in.nesting, '`'), collapse=collapse)
					if (!effects.in.nesting == '') {
						return (effects.in.nesting)
					} else {
						return (character(0))
					}
				} else {
					return (character(0))
				}
			}
		
			# get the effects that are in the group and put them on the rhs
			nested.effects.filtered <- get.effects.in.group.nesting(ranef.nesting, nested.effects)
			rhs.string <- ''
			if (any.exist(nested.effects.filtered)) {
				rhs.string <- '('
				rhs.string <- paste0(rhs.string, nested.effects.filtered)
				rhs.string <- paste0(rhs.string, ')')	
			}
			
			added.effects.filtered <- get.effects.in.group.nesting(ranef.nesting, added.effects, nested=FALSE)
			if (any.exist(added.effects.filtered)) {
				if (any.exist(nested.effects.filtered)) {
					rhs.string <- paste0(rhs.string, ':')
				}
				rhs.string <- paste0(rhs.string, added.effects.filtered)
			}
			
			crossed.effects.filtered <- get.effects.in.group.nesting(ranef.nesting, crossed.effects, nested=FALSE)
			if (any.exist(crossed.effects.filtered)) {
				if (any.exist(nested.effects.filtered) | any.exist(added.effects.filtered)) {
					rhs.string <- paste0(rhs.string, ':')
				}
				rhs.string <- paste0(rhs.string, crossed.effects.filtered)
			}
			
			return (rhs.string)
		}
		
		# intercept
		ranef.string <- '(1'
		ranef.string <- paste0(
			ranef.string, 
			get.lhs(ranef.nesting, added.effects, crossed.effects, nested.effects)
		)
		
		# move over to the group side
		ranef.string <- paste0(ranef.string, ' | ')
		rhs <- get.rhs(ranef.nesting, added.effects, crossed.effects, nested.effects)
		if (rhs != '') {
			rhs <- paste0('`', names(ranef.nesting), '`:', rhs)
		} else {
			rhs <- paste0('`', names(ranef.nesting), '`')
		}
		
		# add the group and close the parens
		ranef.string <- paste0(ranef.string, rhs, ')')
	}
	
	for (ranef.group in names(ranef.nestings)) {
		ranef.nesting <- ranef.nestings[ranef.group]
		nested.model.formulae <- nested.model.formulae |>
			mutate(
				'ranef_{ranef.group}' := get.ranef.string(
											ranef.nesting,
											added.effects,
											crossed.effects,
											nested.effects
										)
			)
	}
	
	nested.model.formulae.list <- nested.model.formulae |>
		unite(ranef, all_of(starts_with('ranef_')), remove=TRUE, sep=' + ') |>
		mutate(formula = paste0(depvar, ' ~ ', fixef, ' + ', ranef)) |>
		pull(formula)
	
	nested.model.formulae.list <- lapply(nested.model.formulae.list, formula)
	names(nested.model.formulae.list) <- paste0(nested.model.formulae$nested, NESTING_SEPARATOR, nested.model.formulae$group)
	
	return (nested.model.formulae.list)
}

ident <- function(df) return (df)

fit.model <- function(
	data.type,
	model.type, 
	data.file,
	data.function,
	formulae.file,
	formula.no,
	model.lists.file,
	model.no,
	family=bernoulli()
) {
	models.dir <- file.path('Models', 'Bayesian', data.type)
	plots.dir <- file.path('Plots', 'Bayesian', data.type)
	dir.create(models.dir, showWarnings=FALSE, recursive=TRUE)
	dir.create(plots.dir, showWarnings=FALSE, recursive=TRUE)
	
	results <- read.csv(data.file) |> 
		mutate(
			subject = as.factor(subject),
			item 	= as.factor(item)
		) |>
		data.function()
	
	model.formulae <- readRDS(file.path('Bayesian scripts', formulae.file))
	name <- names(model.formulae)[[formula.no]]
	formula <- model.formulae[[name]]
	effects <- attr(terms(formula), 'term.labels')
	fixef <- effects[!grepl('^1|0 + ', effects)]
	priors <- c(
		set_prior('normal(0, 10)', class='Intercept'),
		set_prior('lkj(2)', class='cor'),
		set_prior('normal(0, 10)', class = 'b', coef=fixef),
		set_prior('normal(0, 10)', class = 'sd')
	)
	
	models <- list()
	i <- model.no
	model.lists <- readRDS(file.path('Bayesian scripts', model.lists.file))
	
	if (model.type == name) {
		model.dir <- file.path(models.dir, paste0(model.type))
		plot.dir <- file.path(plots.dir, paste0(model.type))
		if (length(model.lists) == 1) {
			out.file <- sprintf('%s_model_%s', model.type, data.type)
			info.str <- sprintf('%s model (%s)', model.type, data.type)
			model.name <- sprintf('%s model (%s)', toTitleCase(model.type), data.type)
		} else {
			out.file <- sprintf('%s_model_%s_%02d', model.type, data.type, i)
			info.str <- sprintf('%s model (%s) %02d', model.type, data.type, i)
			model.name <- sprintf('%s model (%s) %02d', toTitleCase(model.type), data.type, i)
		}
	} else {
		model.dir <- file.path(models.dir, paste0(model.type, '_', name))
		plot.dir <- file.path(plots.dir, paste0(model.type, '_', name))
		if (length(model.lists) == 1) {
			out.file <- sprintf('%s_model_%s_%s', model.type, data.type, name)
			info.str <- sprintf('%s model (%s) %s', model.type, data.type, name)
			model.name <- sprintf('%s model (%s) %s', toTitleCase(model.type), data.type, name)
		} else {
			out.file <- sprintf('%s_model_%s_%s_%02d', model.type, data.type, name, i)
			info.str <- sprintf('%s model (%s) %s %02d', model.type, data.type, name, i)
			model.name <- sprintf('%s model (%s) %s %02d', toTitleCase(model.type), data.type, name, i)
		}
	}
	
	dir.create(model.dir, showWarnings=FALSE, recursive=TRUE)
	dir.create(plot.dir, showWarnings=FALSE, recursive=TRUE)
	
	if (file.exists(file.path(model.dir, paste0(out.file, '.rds')))) {
		cat('Loading ', info.str, '\n', sep='')
	} else {
		cat('Fitting ', info.str, '\n', sep='')
	}
	
	# debug
	# cat(paste0('out.file: ', out.file), '\n')
	# cat(paste0('model.dir: ', model.dir), '\n')
	# cat(paste0('plot.dir: ', plot.dir), '\n')
	# quit()
	
	models[model.name] <- do.call(brm, append(brm.args, list(
		formula = formula,
		data = results |> filter(data_source == 'human' | subject %in% model.lists[[i]]),
		family = family,
		prior = priors,
		file = file.path(model.dir, paste0(out.file, '.rds'))
	))) |> list()
	
	save_model_summaries(
		models,
		filename=file.path(model.dir, paste0(out.file, '_summary.txt')),
		overwrite=TRUE
	)

	save_pmcmc(
		models,
		filename=file.path(model.dir, paste0(out.file, '_pmcmcs.txt'))
	)

	save_model_plots(
		models,
		plots.dir=plot.dir
	)
}
