# Load libraries
library(plyr)
library(ggpubr)
library(stringr)
library(tidyverse)
library(data.table)
library(reticulate)

# Create directories to store results
current.exp <- 'wr'
plots.dir 	<- paste0('Plots/')
models.dir 	<- paste0('Models/')
dir.create(plots.dir, 	showWarnings=FALSE, recursive=TRUE)
dir.create(models.dir, 	showWarnings=FALSE, recursive=TRUE)

MOST_RECENT_SUBJECTS <- 40:45

MAX_RT_IN_SECONDS <- 10
OUTLIER_RT_SDS <- 2

# confidence interval for beta distribution
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

# convenience
s_view <- function(d, x) return (d |> filter(subject %in% x))

s_mr <- function(d) return (d |> s_view(MOST_RECENT_SUBJECTS))

# Load data
colnames <- c(
	'time_received', 'ip_md5', 'controller', 'order', 'element_no', 'condition', 
	'latin_square_group', 'element_type', 'element_name', 'parameter', 'value', 
	'event_time', 'item', 'word', 'target_response', 'args_group', 'sentence_type', 
	'sentence', 'adverb', 'seen_in_training', 'template', 'comments'
)

results <- rbind(results, read.csv(
		paste0('results-', current.exp, '.csv'), 
		comment.char='#',
		header=FALSE, 
		quote='',
		col.names=colnames, 
		fill=TRUE
	) |> 
	as_tibble() |>
	select(ip_md5, condition, order, element_name:template) |>
	mutate(data_source = 'human')
)

# Relabel subjects with smaller values
results <- results |> 
	mutate(
		subject = match(ip_md5, unique(ip_md5)),
		subject = as.factor(subject)
	) |>
	select(subject, everything()) |>
	select(-ip_md5) |>
	mutate(
		subject = as.numeric(as.character(subject)),
		subject = match(subject, unique(subject)),
		subject = as.factor(subject)
	)

# add columns so that models can be added
# results <- read.csv(paste0('results-', current.exp, '.csv')) |> 
results <- results |>
	as_tibble() |>
	mutate(
		subject = as.factor(subject),
		correct.pre.training = NA_real_,
		mask_added_tokens = "Don't mask blork",
		stop_at = 'convergence'
	)

# get break times
breaktimes <- results |>
	filter(condition == 'break') |>
	select(subject, value, event_time) |>
	group_by(subject) |>
	mutate(break_number = sort(rep(1:(n()/2),2))) |>
	spread(value, event_time) |>
	mutate(duration = End - Start) |>
	select(-Start, -End) |>
	mutate(
		minutes = duration/(60 * 1000),
		seconds = (minutes %% 1) * 60,
		milliseconds = (seconds %% 1) * 1000,
		break_time = paste0(
			str_pad(trunc(minutes),2,pad='0'), ':', 
			str_pad(trunc(seconds), 2, pad='0'), '.', 
			str_pad(round(milliseconds), 3, pad='0')
		)
	) |>
	select(subject, break_number, break_time, duration)

# training first choice accuracy by half
# for some subjects, this is inaccurate due to
# a bug in the way PCIbex records results
# with identical trial labels. the training
# repetition criteria worked correctly
training.accuracy.by.dozen <- results |>
	filter(
		condition %like% 'trial_train',
		parameter == 'Drop'
	) |> 
	group_by(subject, mask_added_tokens, stop_at) |>
	mutate(order = rleid(order)) |>
	group_by(subject, data_source, mask_added_tokens, stop_at, order) |>
	summarize(n_tries = n()) |>
	ungroup() |>
	mutate(dozen = ceiling(order/12)) |>
	group_by(subject, data_source, mask_added_tokens, stop_at, dozen) |>
	mutate(total = n()) |>
	group_by(subject, data_source, mask_added_tokens, stop_at, dozen, n_tries, total) |>
	summarize(pr_correct = n()/total) |>
	distinct() |>
	ungroup() |>
	group_by(subject, data_source, mask_added_tokens, stop_at) |>
	filter(
		# dozen == max(dozen),
		n_tries == min(n_tries)
	) |>
	select(-n_tries, -total) |>
	rename(pr_first_choice_correct = pr_correct)

less.than.75.on.training <- training.accuracy.by.dozen |>
	filter(
		dozen == max(dozen),
		pr_first_choice_correct < 0.75
	)

# Get feedback
feedback <- results |>
	filter(element_name == 'feedback', item != 'Shift', parameter == 'Final') |>
	select(subject, value) |>
	rename(feedback = value) |>
	mutate(feedback = gsub('%2C', ',', feedback))

# organize results to one row per trial
results <- results |> 
	filter(
		condition == 'trial',
		!(parameter %in% c('Click', 'Drag', 'Final')),
	) |> 
	mutate(
		parameter = case_when(
						value %in% c('Start', 'End') ~ paste0(parameter, value),
						TRUE 						 ~ parameter
					),
		element_name = case_when(
						element_name == 'dd'			~ 'response',
						startsWith(parameter, '_Trial')	~ parameter,
						TRUE 				 			~ element_name
					),
		value = case_when(
					parameter %in% c('_Trial_Start', '_Trial_End') ~ as.character(event_time),
					TRUE 										   ~ value
				)
	) |>
	select(-parameter, -event_time, -order, -template) |>
	pivot_wider(
		names_from = element_name,
		values_from = value
	)

# add log RTs
results <- results |>
	dplyr::rename(Trial_Start = '_Trial_Start', Trial_End = '_Trial_End') |>
	mutate(
		Trial_Start = as.numeric(Trial_Start),
		Trial_End = as.numeric(Trial_End),
		sentence_delay = (str_count(sentence, ' ') + 1) * 325,
		log.RT = log(Trial_End - sentence_delay - Trial_Start)
	) |> 
	select(-Trial_Start, -Trial_End, -sentence_delay)

# add experimental conditions
results <- results |>
	mutate(
		condition 	= case_when(
						grepl('_', args_group) 	~ 'experimental',
						TRUE 					~ 'filler'
					),
		correct 	= case_when(
						response == target_response ~ TRUE,
						TRUE 						~ FALSE						
					),
		voice 		= case_when(
						grepl('passive', sentence_type, fixed=TRUE) ~ 'OVS passive',
						TRUE 										~ 'SVO active'
					)
	)

# get unique item identifiers to add to the model results
item.ids <- results |>
	select(item, condition, word, args_group, sentence_type) |>
	distinct() |> 
	mutate(item = as.numeric(as.character(item))) |>
	arrange(item)

exp.item.ids <- item.ids |>
	filter(condition == 'experimental') |>
	select(-args_group)

filler.item.ids <- item.ids |>
	filter(condition == 'filler') |>
	select(-word) |>
	distinct()

# We use python to read the gzipped files since it is MUCH faster than doing it in R
py_run_string('import pandas as pd')
py_run_string('from glob import glob')
py_run_string('from tqdm import tqdm')
py_run_string(
	paste0(
		'csvs = glob("C:/Users/mawilson/OneDrive - Yale University/',
		'CLAY Lab/structural-alternations/outputs/fheO/*bert*/', current.exp, '-margs*/',
		'**/*odds_ratios.csv.gz", recursive=True)'
	)
)
py_run_string('model_results = pd.concat([pd.read_csv(f) for f in tqdm(csvs)], ignore_index=True)')

model.results <- py$model_results |> 
	as_tibble() |>
	mutate(
		data_source = gsub('(B|b)ert', 'BERT', str_to_title(model_name)),
		token = gsub('^\u0120', '', token) # formatting bug with roberta models
	)

# convert model results to format comparable with results from humans
model.results <- model.results |>
	dplyr::rename(
		word = token,
		target_response = arg_type
	) |>
	mutate(
		condition = case_when(
						eval_data == 'syn_blorked_SVO-OSV_for_human_exp' ~ 'experimental',
						TRUE ~ 'filler'
					)
	) |>
	# filter out the unwanted eval for the training tokens from the filler items
	filter(
		# keep everything from the experimental conditions
		(condition == 'experimental') | 
		# keep everything from fillers only when the token_type is "eval special"
		(condition == 'filler' & token_type == 'eval special')
	) |>
	mutate(
		stop_at = case_when(
						max_epochs == 5000 ~ 'convergence',
						TRUE ~ '260 epochs'
					),
		mask_added_tokens = case_when(
				 mask_added_tokens ~ "Mask blork",
				!mask_added_tokens ~ "Don't mask blork"
			),
		correct = odds_ratio > 0,
		correct.pre.training = (odds_ratio - odds_ratio_pre_post_difference) > 0,
		sentence = gsub('(\\D)(\\D+)', '\\U\\1\\L\\2', sentence, perl=TRUE),
		# training = case_when(
		# 				eval_data %like% 'blorked_ext' ~ 'SVO only',
		# 				eval_data %like% 'SVO-OSV' ~ 'SVO+OSV',
		# 			),
		training = 'SVO+OSV',
		seen_in_training = case_when(
			token_type == 'tuning' ~ 'True',
			token_type == 'eval added' ~ 'False',
			token_type == 'eval special' ~ NA_character_
		),
		mouse = NA_character_,
		response = case_when(
			odds_ratio > 0 ~ target_response,
			TRUE ~ gsub('.*\\/(.*)', '\\1', ratio_name)
		),
		log.RT = NA_real_,
		voice = case_when(
			grepl('passive', sentence_type, fixed=TRUE) 		  ~ 'OVS passive',
			grepl('.*\\[obj\\].*\\[subj\\].*blorked.*', sentence) ~ 'OSV active',
			TRUE 												  ~ 'SVO active'
		),
		adverb = case_when(
			sentence %like% 'always' ~ 'always',
			sentence %like% 'often' ~ 'often',
			sentence %like% 'usually' ~ 'usually',
			sentence %like% 'typically' ~ 'typically'
		),
		subject = match(random_seed, c(
			1:max(1,as.numeric(as.character(results$subject))), 
			random_seed |> unique()
		)),
		args_group = case_when(
						condition == 'experimental' ~ args_group,
						condition == 'filler' ~ gsub('syn_(.*?)_ext_for_human_exp', '\\1', eval_data)
					)
	) |>
	filter(training == 'SVO+OSV') |>
	# left_join(exp.item.ids) |>
	# left_join(
	# 	filler.item.ids, 
	# 	by=c('condition', 'args_group', 'sentence_type'), 
	# 	suffix=c('', '.y')
	# ) |>
	# mutate(item = coalesce(item, item.y)) |>
	# select(-item.y) |>
	mutate(item = NA_integer_) |>
	select(
		subject, condition, training, item, word, target_response, args_group, sentence_type, 
		sentence, adverb, seen_in_training, data_source, mouse, response, log.RT,
		correct, correct.pre.training, voice, mask_added_tokens, stop_at
	) |> 
	arrange(subject, item, word, adverb)

# merge model results with human results
results <- results |> 
	# mutate(
	# 	training = 'SVO+OSV',
	# 	subject = as.numeric(as.character(subject))
	# ) |> 
	rbind(
		model.results |>
			mutate(subject = as.numeric(as.character(subject)))
	) |>
	mutate(
		subject = as.factor(subject),
		seen_in_training = case_when(
								seen_in_training == 'True' ~ 'Seen',
								seen_in_training == 'False' ~ 'Unseen'
							),
		target_response = case_when(
							target_response == '[subj]' ~ 'Subject target',
							target_response == '[obj]'  ~ 'Object target'
						) |> 
						fct_relevel('Subject target', 'Object target'),
		args_group = paste(gsub('\\_', '+', args_group), 'args') |> 
						fct_relevel(
							'female+male args', 
							'red+yellow args', 
							'vehicles+buildings args', 
							'buildings+vehicles args',
							'vanimals+canimals args',
							'white+red args',
							'eat args',
							'regret args',
							'break args', 
							'buy args',
							'read args'
						),
		sentence_type = fct_relevel(
				sentence_type,
				'perfect transitive',
				'raising perfect transitive',
				'cleft subject perfect transitive',
				'neg perfect transitive',
				'cleft subject raising perfect transitive',
				'perfect passive',
				'raising perfect passive',
				'cleft subject perfect passive',
				'neg perfect passive',
				'cleft subject raising perfect passive',
				'cleft object perfect transitive',
				'presentational ORC perfect transitive'
			),
		voice = case_when(
					grepl('passive', sentence_type, fixed=TRUE) 		  ~ 'OVS passive',
					grepl('.*\\[obj\\].*\\[subj\\].*blorked.*', sentence) ~ 'OSV active',
					TRUE 												  ~ 'SVO active'
				),
		voice = fct_relevel(voice, 'SVO active', 'OVS passive', 'OSV active'),
		data_source = fct_relevel(data_source, 'human', 'BERT', 'DistilBERT', 'RoBERTa'),
		mask_added_tokens = fct_relevel(mask_added_tokens, "Mask blork", "Don't mask blork"),
		stop_at = fct_relevel(stop_at, '260 epochs', 'convergence')
	)

# exclude subjects less than <75% accurate on fillers (no stats)
less.than.75.on.fillers <- results |>
	filter(condition == 'filler') |>
	group_by(subject, data_source, mask_added_tokens, stop_at) |>
	summarize(mean = mean(correct)) |>
	filter(mean < 0.75)

# exclude subjects/models not >50% on training structure as determined by chisq with p > 0.2.
not.above.chance.on.training.str <- results |>
	filter(
		condition == 'experimental',
		sentence_type == 'perfect transitive', 
		seen_in_training == 'Seen'
	) |>
	group_by(subject, data_source, training, mask_added_tokens, stop_at) |>
	summarize(
		correct.total = sum(correct),
		incorrect.total = n() - correct.total
	) |>
	rowwise() |>
	mutate(p.value = chisq.test(c(correct.total,incorrect.total))$p.value) |>
	filter(p.value > 0.2 | correct.total < incorrect.total) |>
	ungroup()

# subjects/models whose performance is purely linear
# the goal is to exclude subjects whose performance on passives is not distinguishable
# from the inverse of their performance on actives as determined by chisq.test > 0.05
purely.linear.by.arguments.both <- results |>
	filter(condition == 'experimental') |>
	select(
		subject, data_source, mask_added_tokens, 
		stop_at, training, voice, target_response, correct
	) |>
	filter(voice %in% c('SVO active', 'OVS passive')) |> 
	mutate(
		correct = case_when(
					voice %like% 'active' ~ !correct,
					voice %like% 'passive' ~ correct
				),
		target_response = case_when(
							target_response %like% 'Subject' ~ 'subj',
							target_response %like% 'Object' ~ 'obj'
						)
	) |> 
	group_by(
		subject, mask_added_tokens, stop_at, 
		training, data_source, target_response
	) |>
	summarize(
		n_active = length(correct[voice %like% 'active']),
		n_passive = length(correct[voice %like% 'passive']),
		inv.correct = sum(correct[voice %like% 'active']),
		pass.correct = sum(correct[voice %like% 'passive']),
	) |>
	pivot_wider(
		names_from=target_response, 
		values_from=c(n_active, n_passive, inv.correct, pass.correct)
	) |>
	rowwise() |>
	mutate(
		p.value.subj = chisq.test(
							matrix(c(
								inv.correct_subj, 	n_active_subj  - inv.correct_subj,
								pass.correct_subj, 	n_passive_subj - pass.correct_subj
						), ncol=2))$p.value,
		# if both numbers are 0, it is indistinguishable from linear, 
		# but chisq can't tell us that (it gives NaN)
		# convert to a number (Inf) for ease of exclusion below
		p.value.subj = case_when(
							is.na(p.value.subj) ~ Inf,
							TRUE ~ p.value.subj
						),
		p.value.obj  = chisq.test(
							matrix(c(
								inv.correct_obj, 	n_active_obj  - inv.correct_obj,
								pass.correct_obj, 	n_passive_obj - pass.correct_obj
						), ncol=2))$p.value,
		p.value.obj  = case_when(
							is.na(p.value.obj) ~ Inf,
							TRUE ~ p.value.obj
						)
	) |>
	filter(
		p.value.subj > 0.05, 
		p.value.obj > 0.05
	) |> 
	# change , to | for both arguments must differ from linear for INCLUSION. currently you are EXCLUDED
	# only if BOTH are not different from linear. changing to OR means you are EXCLUDED if EITHER
	# differs from linear. we favor the weaker criterion, where EXCLUSION means BOTH are NOT different from linear
	ungroup()

# tag linear performance
results <- results |>
	mutate(
		linear = case_when(
					subject %in% purely.linear.by.arguments.both$subject ~ 'Linear',
					TRUE ~ 'Non-linear'
				)
	)

# get all excluded subjects
all.excluded.subjects <- c(
		as.numeric(as.character(less.than.75.on.training$subject)),
		as.numeric(as.character(less.than.75.on.fillers$subject)),
		as.numeric(as.character(not.above.chance.on.training.str$subject)),
		as.numeric(as.character(purely.linear.by.arguments.both$subject))
	) |> 
	sort() |>
	unique() %>%
	data.frame(subject = .) |>
	left_join(
		results |> 
			mutate(subject = as.numeric(as.character(subject))) |>
			select(subject, data_source, mask_added_tokens, stop_at) |> 
			unique()
	) |>
	mutate(
		subject = as.factor(subject),
		why = '',
		why = case_when(
				subject %in% less.than.75.on.training$subject ~ paste0(why, '<75% on training; '),
				TRUE ~ why
			),
		why = case_when(
				subject %in% less.than.75.on.fillers$subject ~ paste0(why, '<75% on fillers; '),
				TRUE ~ why
			),
		why = case_when(
				subject %in% not.above.chance.on.training.str$subject ~ paste0(why, '<50% on training structure; '),
				TRUE ~ why
			), 
		why = case_when(
				subject %in% purely.linear.by.arguments.both$subject ~ paste0(why, 'n.d. from linear'),
				TRUE ~ why
			),
		why = gsub('; $', '', why)
	) |>
	rename(why.excluded = why) |>
	as_tibble()

excluded.for.reasons.other.than.nonlinear <- all.excluded.subjects |>
	filter(why.excluded != 'n.d. from linear')

results <- results |>
	filter(!(subject %in% excluded.for.reasons.other.than.nonlinear$subject)) |>
	droplevels()

# split to separate data frames
exp <- results |>
	filter(condition == 'experimental') |>
	select(-condition)

filler <- results |>
	filter(condition == 'filler') |>
	select(-condition)

# f scores for experimental items
f.scores <- exp |>
	group_by(subject, voice) |>
	mutate(
		false.positives.subjects = case_when(
									(target_response == 'Object target' & response == '[subj]') ~ TRUE,
									TRUE ~ FALSE
								),
		false.negatives.subjects  = case_when(
									!correct & !false.positives.subjects ~ TRUE,
									TRUE ~ FALSE
								),
		false.positives.objects = case_when(
									(target_response == 'Subject target' & response == '[obj]') ~ TRUE,
									TRUE ~ FALSE
								),
		false.negatives.objects  = case_when(
									!correct & !false.positives.objects ~ TRUE,
									TRUE ~ FALSE
								)
	) |>
	summarize(
		true.positives.subjects = sum(correct[target_response == 'Subject target']),
		true.positives.objects  = sum(correct[target_response == 'Object target']),
		false.positives.subjects = sum(false.positives.subjects),
		false.negatives.subjects = sum(false.negatives.subjects),
		false.positives.objects = sum(false.positives.objects),
		false.negatives.objects = sum(false.negatives.objects),
		precision.subjects = true.positives.subjects/(true.positives.subjects+false.positives.subjects),
		precision.objects = true.positives.objects/(true.positives.objects+false.positives.objects),
		recall.subjects = true.positives.subjects/(true.positives.subjects+false.negatives.subjects),
		recall.objects = true.positives.objects/(true.positives.objects+false.negatives.objects),
		f.score.subjects = 2 * (precision.subjects*recall.subjects)/(precision.subjects+recall.subjects),
		f.score.objects = 2 * (precision.objects*recall.objects)/(precision.objects+recall.objects)
	) |>
	select(
		subject, voice, precision.subjects, precision.objects, 
		recall.subjects, recall.objects, 
		f.score.subjects, f.score.objects
	) |>
	distinct() |>
	pivot_longer(
		c(f.score.subjects,f.score.objects),
		names_to='target_response',
		values_to='f.score'
	) |>
	mutate(
		target_response = case_when(
							target_response == 'f.score.subjects' ~ 'Subject target',
							TRUE ~ 'Object target'
						),
		target_response = fct_relevel(target_response, 'Subject target', 'Object target')
	)

f.scores.pre.training <- exp |>
	group_by(subject, voice) |>
	mutate(response_pre_training = case_when(
									correct == correct.pre.training ~ response,
									response == '[subj]' ~ '[obj]',
									response == '[obj]'  ~ '[subj]'
								)
	) |>
	mutate(
		false.positives.subjects = case_when(
									(target_response == 'Object target' & response_pre_training == '[subj]') ~ TRUE,
									TRUE ~ FALSE
								),
		false.negatives.subjects  = case_when(
									!correct.pre.training & !false.positives.subjects ~ TRUE,
									TRUE ~ FALSE
								),
		false.positives.objects = case_when(
									(target_response == 'Subject target' & response_pre_training == '[obj]') ~ TRUE,
									TRUE ~ FALSE
								),
		false.negatives.objects  = case_when(
									!correct & !false.positives.objects ~ TRUE,
									TRUE ~ FALSE
								)
	) |>
	dplyr::summarize(
		true.positives.subjects = sum(correct.pre.training[target_response == 'Subject target']),
		true.positives.objects  = sum(correct.pre.training[target_response == 'Object target']),
		false.positives.subjects = sum(false.positives.subjects),
		false.negatives.subjects = sum(false.negatives.subjects),
		false.positives.objects = sum(false.positives.objects),
		false.negatives.objects = sum(false.negatives.objects),
		precision.subjects = true.positives.subjects/(true.positives.subjects+false.positives.subjects),
		precision.objects = true.positives.objects/(true.positives.objects+false.positives.objects),
		recall.subjects = true.positives.subjects/(true.positives.subjects+false.negatives.subjects),
		recall.objects = true.positives.objects/(true.positives.objects+false.negatives.objects),
		f.score.subjects = 2 * (precision.subjects*recall.subjects)/(precision.subjects+recall.subjects),
		f.score.objects = 2 * (precision.objects*recall.objects)/(precision.objects+recall.objects)
	) |>
	select(
		subject, voice, precision.subjects, precision.objects, 
		recall.subjects, recall.objects, 
		f.score.subjects, f.score.objects
	) |>
	distinct() |> 
	pivot_longer(
		c(f.score.subjects,f.score.objects),
		names_to='target_response',
		values_to='f.score'
	) |>
	mutate(
		target_response = case_when(
							target_response == 'f.score.subjects' ~ 'Subject target',
							TRUE ~ 'Object target'
						),
		target_response = fct_relevel(target_response, 'Subject target', 'Object target')
	)

exp <- exp |>
	left_join(
		f.scores |> 
			select(subject, voice, target_response, f.score)
	) |>
	left_join(
		f.scores.pre.training |>
			select(subject, voice, target_response, f.score) |>
			rename(f.score.pre.training = f.score)	
	)

# save results for accuracy analysis
accuracy.data <- exp |>
	filter(
		data_source %in% c('human', 'BERT'),
		mask_added_tokens == "Don't mask blork",
		stop_at == 'convergence',
		# filter to only the actual sentences humans saw
		paste(word, sentence) %in% (
			exp |> 
				filter(data_source == 'human') |>
				select(word, sentence) |>
				distinct() |>
				droplevels() |>
				mutate(word_sentence = paste(word, sentence)) |>
				pull(word_sentence)
		)
	) |>
	select(
		-word, -args_group, -mouse, -correct.pre.training,
		-f.score, -sentence, -training, -adverb
	) |>
	mutate(
		seen_in_training.n = case_when(
								seen_in_training == 'Seen'   ~  0.5,
								seen_in_training == 'Unseen' ~ -0.5
							),
		target_response.n = case_when(
								target_response == 'Object target'  ~  0.5,
								target_response == 'Subject target' ~ -0.5
							),
		data_source.n = case_when(
							data_source == 'human' ~  0.5,
							data_source == 'BERT'  ~ -0.5
						),
		voice.n = case_when(
					voice == 'SVO active'  ~  0.5,
					voice == 'OVS passive' ~ -0.5
				),
		linear.n = case_when(
					linear %like% '^Non-linear' ~  0.5,
					linear %like% '^Linear'     ~ -0.5
				),
		RT = exp(log.RT)
	)

write.csv(accuracy.data, 'accuracy-data.csv', row.names=FALSE)

# logistic regression including data source 
# separate regressions for each argument group
#	(model, human) * voice (active, passive) * (seen, unseen) * target_response
# 	random effects for subjects/model random seed, random effect of word (nested within (seen, unseen)), 
#   			       items
# 
# if humans succeed but models don't we expect interaction with data source (model, human)
# nested comparisons for sentence types within voice
#
# separate analyses for each argument group
# also correlation analyses including all human subjects who pass the filler exclusion criterion
# to see how well accuracy in actives predicts accuracy in passives for individual tokens
# (i.e., have they learned categories or individual tokens?)
# one where correlation of accuracy across voice (one point per subject)
# one where correlation of accuracy across voice (one point per word per subject)
# also grouped by (seen, unseen), target_response, sentence_type (paired active and passive constructions)
#
# maybe exclude nouns where subjects < 100% on training structure got it wrong?

linear_labels <- geom_text(
		data = (
			exp |> 
				select(subject, data_source, mask_added_tokens, stop_at, linear) |>
				distinct() |>
				group_by(data_source, mask_added_tokens, stop_at, linear) |> 
				summarize(count=sprintf("n=%02d", n()))
		),
		mapping = aes(x=-Inf, y=-Inf, label=count),
		hjust=-0.5,
		vjust=-1,
		inherit.aes=FALSE
	)

linear_labels_no_humans <- geom_text(
		data = (
			exp |> 
				filter(data_source != 'human') |>
				select(subject, data_source, mask_added_tokens, stop_at, linear) |>
				distinct() |>
				group_by(data_source, mask_added_tokens, stop_at, linear) |> 
				summarize(count=sprintf("n=%02d", n()))
		),
		mapping = aes(x=-Inf, y=-Inf, label=count),
		hjust=-0.5,
		vjust=-1,
		inherit.aes=FALSE
	)

linear_labels_humans <- geom_text(
		data = (
			exp |> 
				filter(data_source == 'human') |>
				select(subject, linear) |>
				distinct() |>
				group_by(linear) |> 
				summarize(count=sprintf("n=%02d", n()))
		),
		mapping = aes(x=-Inf, y=-Inf, label=count),
		hjust=-0.5,
		vjust=-1,
		inherit.aes=FALSE
	)

########################################################################
###################### EXPERIMENTAL ITEMS ##############################
########################################################################
# accuracy by voice, target_response, and data_source
exp |> 
	ggplot(aes(x=voice, y=as.numeric(correct), fill=target_response)) +
	stat_summary(fun=mean, geom='bar', position='dodge', width=0.9) +
	stat_summary(fun.data=beta_ci, geom='errorbar', width=0.33, position=position_dodge(0.9)) +
	stat_summary(
		fun.data=\(y) data.frame(y=mean(y), label=sprintf('%.2f', mean(y)), fill='white'), 
		geom='label', position=position_dodge(0.9), show.legend=FALSE
	) +
	ylim(0, 1) +
	scale_x_discrete(
		'Template',
		breaks = c('SVO active', 'OVS passive', 'OSV active'),
		labels = c('SVO', 'OVS', '(OSV)')
	) +
	ylab('Pr. Correct') +
	scale_fill_discrete('Target response') +
	ggtitle(paste0('Pr. Correct by Voice')) +
	facet_grid(data_source ~ mask_added_tokens + stop_at)
	
# accuracy by voice, target_response, and data_source (subject means)
exp |> 
	group_by(subject, data_source, mask_added_tokens, stop_at, voice, target_response) |>
	summarize(correct = mean(correct)) |>
	ggplot(aes(x=voice, y=as.numeric(correct), fill=target_response)) +
	geom_point(
		shape=21,
		cex=2,
		position=position_jitterdodge(dodge.width=0.9, jitter.width=0.45, jitter.height=0.0)
	) +
	geom_violin(alpha=0.3) +
	ylim(0, 1) +
	scale_x_discrete(
		'Template',
		breaks = c('SVO active', 'OVS passive', 'OSV active'),
		labels = c('SVO', 'OVS', '(OSV)')
	) +
	ylab('Pr. Correct') +
	scale_fill_discrete('Target response') +
	ggtitle(paste0('Pr. Correct by Voice (subject means)')) +
	facet_grid(data_source ~ mask_added_tokens + stop_at)

# accuracy by voice, target_response, data_source, and linear
exp |>
	ggplot(aes(x=voice, y=as.numeric(correct), fill=target_response)) +
	stat_summary(fun=mean, geom='bar', position='dodge', width=0.9) +
	stat_summary(fun.data=beta_ci, geom='errorbar', width=0.33, position=position_dodge(0.9)) +
	stat_summary(
		fun.data=\(y) data.frame(y=mean(y), label=sprintf('%.2f', mean(y)), fill='white'), 
		geom='label', position=position_dodge(0.9), show.legend=FALSE
	) +
	linear_labels + 
	ylim(0, 1) +
	scale_x_discrete(
		'Template',
		breaks = c('SVO active', 'OVS passive', 'OSV active'),
		labels = c('SVO', 'OVS', '(OSV)')
	) +
	ylab('Pr. Correct') +
	scale_fill_discrete(
		'Target response',
		breaks = c('Subject target', 'Object target'),
		labels = c('Subject target', 'Object target')
	) +
	ggtitle(paste0('Pr. Correct by Voice')) +
	facet_grid(data_source ~ mask_added_tokens + stop_at + linear)

# mean accuracy/subject by voice, target_response, data_source, and linear
exp |> 
	group_by(subject, data_source, mask_added_tokens, stop_at, linear, voice, target_response) |>
	summarize(correct = mean(correct)) |>
	ggplot(aes(x=voice, y=as.numeric(correct), fill=target_response)) +
	geom_boxplot(position='dodge', width=0.9) +
	linear_labels + 
	ylim(0, 1) +
	scale_x_discrete(
		'Template',
		breaks = c('SVO active', 'OVS passive', 'OSV active'),
		labels = c('SVO', 'OVS', '(OSV)')
	) +
	ylab('Pr. Correct (means)') +
	scale_fill_discrete('Target response') +
	ggtitle(paste0('Pr. Correct by Voice (subject means)')) +
	facet_grid(data_source ~ mask_added_tokens + stop_at + linear)

# log RT by subject, data_source, linear, voice, and target_response
exp |> 
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
	ggplot(aes(x=voice, y=log.RT, fill=target_response)) +
	geom_boxplot() +
	linear_labels_humans +
	scale_x_discrete(
		'Template',
		breaks = c('SVO active', 'OVS passive', 'OSV active'),
		labels = c('SVO', 'OVS', '(OSV)')
	) +
	ylab('Log RT') +
	scale_fill_discrete('Target response') +
	ggtitle(sprintf('Log RT by Voice (>%s sec and >%s s.d. from mean by subject removed)', MAX_RT_IN_SECONDS, OUTLIER_RT_SDS)) +
	facet_grid(. ~ linear)

# mean log RT by subject, data_source, linear, voice, and target_response
exp |> 
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
	group_by(subject, data_source, linear, voice, target_response) |>
	summarize(log.RT = mean(log.RT)) |>
	ggplot(aes(x=voice, y=log.RT, fill=target_response)) +
	geom_boxplot() +
	linear_labels_humans +
	scale_x_discrete(
		'Template',
		breaks = c('SVO active', 'OVS passive', 'OSV active'),
		labels = c('SVO', 'OVS', '(OSV)')
	) +
	ylab('Log RT (means)') +
	scale_fill_discrete('Target response') +
	ggtitle(sprintf('Log RT by Voice (subject means, >%s sec and >%s s.d. by subject removed)', MAX_RT_IN_SECONDS, OUTLIER_RT_SDS)) +
	facet_grid(. ~ linear)

# F scores/subject by data_source, linear, voice, and target_response
exp |> 
	select(
		subject, data_source, mask_added_tokens, 
		stop_at, voice, target_response, linear, f.score
	) |>
	distinct() |>
	mutate(f.score = case_when(is.na(f.score) ~ 0, TRUE ~ f.score)) |>
	ggplot(aes(x=voice, y=f.score, fill=target_response)) +
	geom_boxplot(position='dodge', width=0.9) +
	linear_labels + 
	# geom_violin(position='dodge', width=0.9, alpha=0.3) +
	ylim(0, 1) +
	scale_x_discrete(
		'Template',
		breaks = c('SVO active', 'OVS passive', 'OSV active'),
		labels = c('SVO', 'OVS', '(OSV)')
	) +
	ylab('F score') +
	scale_fill_discrete('Target response') +
	ggtitle(paste0('Subject F scores by Voice')) +
	facet_grid(data_source ~ mask_added_tokens + stop_at + linear)

########################################################################
###################### PRE-FINE-TUNING (MODELS ONLY) ###################
########################################################################
# pre-fine-tuning accuracy by voice, target_response, and data_source
exp |> 
	filter(data_source != 'human') |>
	select(-correct) |> 
	droplevels() |>
	rename(correct = correct.pre.training) |>
	ggplot(aes(x=voice, y=as.numeric(correct), fill=target_response)) +
	stat_summary(fun=mean, geom='bar', position='dodge', width=0.9) +
	stat_summary(fun.data=beta_ci, geom='errorbar', width=0.33, position=position_dodge(0.9)) +
	stat_summary(
		fun.data=\(y) data.frame(y=mean(y), label=sprintf('%.2f', mean(y)), fill='white'), 
		geom='label', position=position_dodge(0.9), show.legend=FALSE
	) +
	ylim(0, 1) +
	scale_x_discrete(
		'Template',
		breaks = c('SVO active', 'OVS passive', 'OSV active'),
		labels = c('SVO', 'OVS', '(OSV)')
	) +
	ylab('Pr. Correct') +
	scale_fill_discrete('Target response') +
	ggtitle(paste0('Pr. Correct by Voice (pre-fine-tuning)')) +
	facet_grid(data_source ~ mask_added_tokens + stop_at)
	
# pre-fine-tuning accuracy by voice, target_response, and data_source (subject means)
exp |> 
	filter(data_source != 'human') |>
	select(-correct) |> 
	droplevels() |>
	rename(correct = correct.pre.training) |>
	group_by(subject, data_source, mask_added_tokens, stop_at, voice, target_response) |>
	summarize(correct = mean(correct)) |>
	ggplot(aes(x=voice, y=as.numeric(correct), fill=target_response)) +
	geom_point(
		shape=21,
		cex=2,
		position=position_jitterdodge(dodge.width=0.9, jitter.width=0.45, jitter.height=0.0)
	) +
	geom_violin(alpha=0.3) +
	ylim(0, 1) +
	scale_x_discrete(
		'Template',
		breaks = c('SVO active', 'OVS passive', 'OSV active'),
		labels = c('SVO', 'OVS', '(OSV)')
	) +
	ylab('Pr. Correct') +
	scale_fill_discrete('Target response') +
	ggtitle(paste0('Pr. Correct by Voice (subject means, pre-fine-tuning)')) +
	facet_grid(data_source ~ mask_added_tokens + stop_at)

# pre-fine-tuning accuracy by voice, target_response, data_source, and linear
exp |>
	filter(data_source != 'human') |>
	select(-correct) |> 
	droplevels() |>
	rename(correct = correct.pre.training) |>
	ggplot(aes(x=voice, y=as.numeric(correct), fill=target_response)) +
	stat_summary(fun=mean, geom='bar', position='dodge', width=0.9) +
	stat_summary(fun.data=beta_ci, geom='errorbar', width=0.33, position=position_dodge(0.9)) +
	stat_summary(
		fun.data=\(y) data.frame(y=mean(y), label=sprintf('%.2f', mean(y)), fill='white'), 
		geom='label', position=position_dodge(0.9), show.legend=FALSE
	) +
	linear_labels_no_humans + 
	ylim(0, 1) +
	scale_x_discrete(
		'Template',
		breaks = c('SVO active', 'OVS passive', 'OSV active'),
		labels = c('SVO', 'OVS', '(OSV)')
	) +
	ylab('Pr. Correct') +
	scale_fill_discrete(
		'Target response',
		breaks = c('Subject target', 'Object target'),
		labels = c('Subject target', 'Object target')
	) +
	ggtitle(paste0('Pr. Correct by Voice (pre-fine-tuning)')) +
	facet_grid(data_source ~ mask_added_tokens + stop_at + linear)

# pre-fine-tuning mean accuracy/subject by voice, target_response, data_source, and linear
exp |> 
	filter(data_source != 'human') |>
	select(-correct) |> 
	droplevels() |>
	rename(correct = correct.pre.training) |>
	group_by(subject, data_source, mask_added_tokens, stop_at, linear, voice, target_response) |>
	summarize(correct = mean(correct)) |>
	ggplot(aes(x=voice, y=as.numeric(correct), fill=target_response)) +
	geom_boxplot(position='dodge', width=0.9) +
	linear_labels_no_humans + 
	ylim(0, 1) +
	scale_x_discrete(
		'Template',
		breaks = c('SVO active', 'OVS passive', 'OSV active'),
		labels = c('SVO', 'OVS', '(OSV)')
	) +
	ylab('Pr. Correct (means)') +
	scale_fill_discrete('Target response') +
	ggtitle(paste0('Pr. Correct by Voice (subject means, pre-fine-tuning)')) +
	facet_grid(data_source ~ mask_added_tokens + stop_at + linear)

# F scores pretraining/subject by data_source, linear, voice, and target_response
exp |> 
	filter(data_source != 'human') |>
	droplevels() |>
	select(-f.score) |>
	rename(f.score = f.score.pre.training) |>
	select(
		subject, data_source, mask_added_tokens, 
		stop_at, voice, target_response, linear, f.score
	) |>
	distinct() |>
	mutate(f.score = case_when(is.na(f.score) ~ 0, TRUE ~ f.score)) |>
	ggplot(aes(x=voice, y=f.score, fill=target_response)) +
	geom_boxplot(position='dodge', width=0.9) +
	linear_labels_no_humans + 
	# geom_violin(position='dodge', width=0.9, alpha=0.3) +
	ylim(0, 1) +
	scale_x_discrete(
		'Template',
		breaks = c('SVO active', 'OVS passive', 'OSV active'),
		labels = c('SVO', 'OVS', '(OSV)')
	) +
	ylab('F score') +
	scale_fill_discrete('Target response') +
	ggtitle(paste0('Subject F scores by Voice (pre-fine-tuning)')) +
	facet_grid(data_source ~ mask_added_tokens + stop_at + linear)

########################################################################
###################### FILLERS #########################################
########################################################################

# accuracy by voice, target_response, and data_source
filler |> 
	ggplot(aes(x=voice, y=as.numeric(correct), fill=target_response)) +
	stat_summary(fun=mean, geom='bar', position='dodge', width=0.9) +
	stat_summary(fun.data=beta_ci, geom='errorbar', width=0.33, position=position_dodge(0.9)) +
	stat_summary(
		fun.data=\(y) data.frame(y=mean(y), label=sprintf('%.2f', mean(y)), fill='white'), 
		geom='label', position=position_dodge(0.9), show.legend=FALSE
	) +
	ylim(0, 1) +
	scale_x_discrete(
		'Template',
		breaks = c('SVO active', 'OVS passive', 'OSV active'),
		labels = c('SVO', 'OVS', '(OSV)')
	) +
	ylab('Pr. Correct') +
	scale_fill_discrete('Target response') +
	ggtitle(paste0('Pr. Correct by Voice (fillers)')) +
	facet_grid(data_source ~ mask_added_tokens + stop_at)

# accuracy by voice, target_response, and data_source (subject means)
filler |> 
	group_by(subject, data_source, mask_added_tokens, stop_at, voice, target_response) |>
	summarize(correct = mean(correct)) |>
	ggplot(aes(x=voice, y=as.numeric(correct), fill=target_response)) +
	geom_point(
		shape=21,
		cex=2,
		position=position_jitterdodge(dodge.width=0.9, jitter.width=0.45, jitter.height=0.0)
	) +
	geom_violin(alpha=0.3) +
	ylim(0, 1) +
	scale_x_discrete(
		'Template',
		breaks = c('SVO active', 'OVS passive', 'OSV active'),
		labels = c('SVO', 'OVS', '(OSV)')
	) +
	ylab('Pr. Correct') +
	scale_fill_discrete('Target response') +
	ggtitle(paste0('Pr. Correct by Voice (subject means, fillers)')) +
	facet_grid(data_source ~ mask_added_tokens + stop_at)

# accuracy by voice, target_response, data_source, and linear
filler |>
	ggplot(aes(x=voice, y=as.numeric(correct), fill=target_response)) +
	stat_summary(fun=mean, geom='bar', position='dodge', width=0.9) +
	stat_summary(fun.data=beta_ci, geom='errorbar', width=0.33, position=position_dodge(0.9)) +
	stat_summary(
		fun.data=\(y) data.frame(y=mean(y), label=sprintf('%.2f', mean(y)), fill='white'), 
		geom='label', position=position_dodge(0.9), show.legend=FALSE
	) +
	linear_labels + 
	ylim(0, 1) +
	scale_x_discrete(
		'Template',
		breaks = c('SVO active', 'OVS passive', 'OSV active'),
		labels = c('SVO', 'OVS', '(OSV)')
	) +
	ylab('Pr. Correct') +
	scale_fill_discrete(
		'Target response',
		breaks = c('Subject target', 'Object target'),
		labels = c('Subject target', 'Object target')
	) +
	ggtitle(paste0('Pr. Correct by Voice (fillers)')) +
	facet_grid(data_source ~ mask_added_tokens + stop_at + linear)

# mean accuracy/subject by voice, target_response, data_source, and linear
filler |> 
	group_by(subject, data_source, mask_added_tokens, stop_at, linear, voice, target_response) |>
	summarize(correct = mean(correct)) |>
	ggplot(aes(x=voice, y=as.numeric(correct), fill=target_response)) +
	geom_boxplot(position='dodge', width=0.9) +
	linear_labels + 
	ylim(0, 1) +
	scale_x_discrete(
		'Template',
		breaks = c('SVO active', 'OVS passive', 'OSV active'),
		labels = c('SVO', 'OVS', '(OSV)')
	) +
	ylab('Pr. Correct (means)') +
	scale_fill_discrete('Target response') +
	ggtitle(paste0('Pr. Correct by Voice (subject means, fillers)')) +
	facet_grid(data_source ~ mask_added_tokens + stop_at + linear)

# log RT by subject, data_source, linear, voice, and target_response
filler |> 
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
	ggplot(aes(x=voice, y=log.RT, fill=target_response)) +
	geom_boxplot() +
	linear_labels_humans +
	scale_x_discrete(
		'Template',
		breaks = c('SVO active', 'OVS passive', 'OSV active'),
		labels = c('SVO', 'OVS', '(OSV)')
	) +
	ylab('Log RT') +
	scale_fill_discrete('Target response') +
	ggtitle(paste0('Log RT by Voice (>3 s.d. by subject removed, fillers)')) +
	facet_grid(. ~ linear)

# mean log RT by subject, data_source, linear, voice, and target_response
filler |> 
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
	group_by(subject, data_source, linear, voice, target_response) |>
	summarize(log.RT = mean(log.RT)) |>
	ggplot(aes(x=voice, y=log.RT, fill=target_response)) +
	geom_boxplot() +
	linear_labels_humans +
	scale_x_discrete(
		'Template',
		breaks = c('SVO active', 'OVS passive', 'OSV active'),
		labels = c('SVO', 'OVS', '(OSV)')
	) +
	ylab('Log RT (means)') +
	scale_fill_discrete('Target response') +
	ggtitle(paste0('Log RT by Voice (subject means, >3 s.d. by subject removed, fillers)')) +
	facet_grid(. ~ linear)
	
# accuracy by voice, target_response, and data_source pre-fine-tuning
filler |> 
	filter(data_source != 'human') |>
	droplevels() |>
	select(-correct) |>
	rename(correct = correct.pre.training) |> 
	ggplot(aes(x=voice, y=as.numeric(correct), fill=target_response)) +
	stat_summary(fun=mean, geom='bar', position='dodge', width=0.9) +
	stat_summary(fun.data=beta_ci, geom='errorbar', width=0.33, position=position_dodge(0.9)) +
	stat_summary(
		fun.data=\(y) data.frame(y=mean(y), label=sprintf('%.2f', mean(y)), fill='white'), 
		geom='label', position=position_dodge(0.9), show.legend=FALSE
	) +
	ylim(0, 1) +
	scale_x_discrete(
		'Template',
		breaks = c('SVO active', 'OVS passive', 'OSV active'),
		labels = c('SVO', 'OVS', '(OSV)')
	) +
	ylab('Pr. Correct') +
	scale_fill_discrete('Target response') +
	ggtitle(paste0('Pr. Correct by Voice (pre-fine-tuning, fillers)')) +
	facet_grid(data_source ~ .)

########################################################################
###################### MOST RECENT SUBJECTS ############################
########################################################################
# accuracy
exp |> 
	s_mr() |>
	ggplot(aes(x=voice, y=as.numeric(correct), fill=target_response)) +
	stat_summary(fun=mean, geom='bar', position='dodge', width=0.9) +
	stat_summary(fun.data=beta_ci, geom='errorbar', width=0.33, position=position_dodge(0.9)) +
	stat_summary(
		fun.data=\(y) data.frame(y=mean(y), label=sprintf('%.2f', mean(y)), fill='white'), 
		geom='label', position=position_dodge(0.9), show.legend=FALSE
	) +
	ylim(0, 1) +
	scale_x_discrete(
		'Template',
		breaks = c('SVO active', 'OVS passive', 'OSV active'),
		labels = c('SVO', 'OVS', '(OSV)')
	) +
	ylab('Pr. Correct') +
	scale_fill_discrete('Target response') +
	ggtitle(paste0('Pr. Correct by Voice (', MOST_RECENT_SUBJECTS[[1]], '–', MOST_RECENT_SUBJECTS[[length(MOST_RECENT_SUBJECTS)]], ')')) +
	facet_grid(. ~ subject + linear)

# log RTs
exp |> 
	s_mr() |>
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
	ggplot(aes(x=voice, y=log.RT, fill=target_response)) +
	geom_boxplot() +
	scale_x_discrete(
		'Template',
		breaks = c('SVO active', 'OVS passive', 'OSV active'),
		labels = c('SVO', 'OVS', '(OSV)')
	) +
	ylab('Log RT') +
	scale_fill_discrete('Target response') +
	ggtitle(paste0(sprintf('Log RT by Voice (>%s sec and >%s s.d. from mean by subject removed)', MAX_RT_IN_SECONDS, OUTLIER_RT_SDS), ' (', MOST_RECENT_SUBJECTS[[1]], '–', MOST_RECENT_SUBJECTS[[length(MOST_RECENT_SUBJECTS)]], ')')) +
	facet_grid(. ~ subject + linear)

# F scores
exp |> 
	s_mr() |>
	select(
		subject, data_source, mask_added_tokens, 
		stop_at, voice, target_response, linear, f.score
	) |>
	distinct() |>
	mutate(f.score = case_when(is.na(f.score) ~ 0, TRUE ~ f.score)) |>
	ggplot(aes(x=voice, y=f.score, fill=target_response)) +
	geom_boxplot(position='dodge', width=0.9) +
	# geom_violin(position='dodge', width=0.9, alpha=0.3) +
	ylim(0, 1) +
	scale_x_discrete(
		'Template',
		breaks = c('SVO active', 'OVS passive', 'OSV active'),
		labels = c('SVO', 'OVS', '(OSV)')
	) +
	ylab('F score') +
	scale_fill_discrete('Target response') +
	ggtitle(paste0('Subject F scores by Voice (', MOST_RECENT_SUBJECTS[[1]], '–', MOST_RECENT_SUBJECTS[[length(MOST_RECENT_SUBJECTS)]], ')')) +
	facet_grid(. ~ subject + linear)

###################### FILLERS ########################################
# accuracy
filler |> 
	s_mr() |>
	ggplot(aes(x=voice, y=as.numeric(correct), fill=target_response)) +
	stat_summary(fun=mean, geom='bar', position='dodge', width=0.9) +
	stat_summary(fun.data=beta_ci, geom='errorbar', width=0.33, position=position_dodge(0.9)) +
	stat_summary(
		fun.data=\(y) data.frame(y=mean(y), label=sprintf('%.2f', mean(y)), fill='white'), 
		geom='label', position=position_dodge(0.9), show.legend=FALSE
	) +
	ylim(0, 1) +
	scale_x_discrete(
		'Template',
		breaks = c('SVO active', 'OVS passive', 'OSV active'),
		labels = c('SVO', 'OVS', '(OSV)')
	) +
	ylab('Pr. Correct') +
	scale_fill_discrete('Target response') +
	ggtitle(paste0('Pr. Correct by Voice (fillers) (', MOST_RECENT_SUBJECTS[[1]], '–', MOST_RECENT_SUBJECTS[[length(MOST_RECENT_SUBJECTS)]], ')')) +
	facet_grid(. ~ subject + linear)

# log RTs
filler |> 
	s_mr() |>
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
	ggplot(aes(x=voice, y=log.RT, fill=target_response)) +
	geom_boxplot() +
	scale_x_discrete(
		'Template',
		breaks = c('SVO active', 'OVS passive', 'OSV active'),
		labels = c('SVO', 'OVS', '(OSV)')
	) +
	ylab('Log RT') +
	scale_fill_discrete('Target response') +
	ggtitle(paste0('Log RT by Voice (>3 s.d. by subject removed, fillers) (', MOST_RECENT_SUBJECTS[[1]], '–', MOST_RECENT_SUBJECTS[[length(MOST_RECENT_SUBJECTS)]], ')')) +
	facet_grid(. ~ subject + linear)