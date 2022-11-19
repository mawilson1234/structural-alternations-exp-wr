# Load libraries
library(plyr)
library(ggh4x)
library(purrr)
library(ggpubr)
library(stringr)
library(tidyverse)
library(data.table)
library(reticulate)

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

# We use python to read the gzipped files since it is MUCH faster than doing it in R
py_run_string('import pandas as pd')
py_run_string('from glob import glob')
py_run_string('from tqdm import tqdm')
py_run_string(
	paste0(
		'csvs = glob("C:/Users/mawilson/OneDrive - Yale University/',
		'CLAY Lab/structural-alternations/outputs/fheO/*bert*/wr-margs*/',
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
	rename(
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
		item = match(sentence_num, sentence_num |> unique()),
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
		training = 'SVO+OSV',
		seen_in_training = case_when(
			token_type == 'tuning' ~ 'True',
			token_type == 'eval added' ~ 'False',
			token_type == 'eval special' ~ NA_character_
		),
		response = case_when(
			odds_ratio > 0 ~ target_response,
			TRUE ~ gsub('.*\\/(.*)', '\\1', ratio_name)
		),
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
		subject = match(
			random_seed, 
			random_seed |> unique()
		),
		args_group = case_when(
						condition == 'experimental' ~ args_group,
						condition == 'filler' ~ gsub('syn_(.*?)_ext_for_human_exp', '\\1', eval_data)
					)
	) |>
	select(
		subject, condition, training, item, word, target_response, other_arg_type, args_group, sentence_type, 
		sentence, adverb, seen_in_training, data_source, response,
		log_probability, other_log_probability, other_arg_type,
		correct, correct.pre.training, odds_ratio, odds_ratio_pre_post_difference, 
		voice, mask_added_tokens, stop_at, model_id
	) |> 
	arrange(subject, item, word, adverb)

get_sentences_results <- function(df, file) {
	if (file.exists(file)) {
		return (fread(file) |> as_tibble())
	}
	
	pairs <- df |>
		select(word, target_response, args_group) |>
		distinct() |> 
		arrange(args_group, target_response, word) |>
		dlply(
			.(args_group), 
			function(x) {
				data.frame(x) |> 
					select(-args_group) |>
					dlply(
						.(target_response),
						\(x) data.frame(x) |> 
							select(-target_response) |> 
							pull(word)
					)
			}
		) |>
		lapply(
			\(args_group) {
				cross(args_group)
			}
		)
	
	new.df <- data.frame()
	for (args_group2 in names(pairs)) {
		cat('Processing args_group', args_group2, '\n')
		for (i in seq_along(pairs[[args_group2]])) {
			cat('Working on pair', i, 'of', length(pairs[[args_group2]]), '...', '\r')
			pair <- pairs[[args_group2]][[i]]
			new.df <- rbind(
				new.df,
				df |>
					filter(
						args_group == args_group2,
						word %in% pair
					) |> 
					ungroup() |>
					group_by(model_id, mask_added_tokens, stop_at, sentence) |>
					mutate(
						logprob_correct_subj 	= log_probability[word == pair$`[subj]` & target_response == '[subj]'],
						logprob_correct_obj 	= log_probability[word == pair$`[obj]`  & target_response == '[obj]'],
						logprob_wrong_subj 		= other_log_probability[word == pair$`[subj]` & target_response == '[subj]'],
						logprob_wrong_obj 		= other_log_probability[word == pair$`[obj]`  & target_response == '[obj]'],
						odds_ratio_sent 		= (logprob_correct_subj + logprob_correct_obj) - (logprob_wrong_subj + logprob_wrong_obj),
						correct 				= odds_ratio_sent > 0
					) |> 
					select(-log_probability, -other_arg_type, -response, -other_log_probability, -odds_ratio, -correct.pre.training, -odds_ratio_pre_post_difference, -seen_in_training) |>
					pivot_wider(names_from=target_response, values_from=word) |>
					rename(subj=`[subj]`, obj=`[obj]`, odds_ratio = odds_ratio_sent) |>
					ungroup()
			)
		}
		cat('\n\n')
	}
	
	write.csv(new.df, file, row.names=FALSE)
	
	return (new.df)
}

model.results.sentences <- model.results |>
	get_sentences_results(file='model-results-sents-details.csv.gz')

results <- model.results

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
		sentence_type == 'perfect transitive'
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
		target_response = gsub('\\[(.*?)\\]', '\\1', target_response)
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

model.results.sentences <- model.results.sentences |>
	mutate(
		linear = case_when(
					subject %in% purely.linear.by.arguments.both$subject ~ 'Linear',
					TRUE ~ 'Non-linear'
				)
	)

# read in cosine similarity files for models
# to determine whether success distinguishing
# pre-fine-tuning BLORKED from non-BLORKED targets
# predicts accuracy post-fine-tuning
py_run_string(
	paste0(
		'cossim_csvs = glob("C:/Users/mawilson/OneDrive - Yale University/',
		'CLAY Lab/structural-alternations/outputs/fheO/*bert*/wr-margs*/',
		'**/*cossims.csv.gz", recursive=True)'
	)
)
py_run_string('cossim_results = pd.concat([pd.read_csv(f) for f in tqdm(cossim_csvs)], ignore_index=True)')

cossim.results <- py$cossim_results |> 
	as_tibble() |>
	mutate(
		data_source = gsub('(B|b)ert', 'BERT', str_to_title(model_name)),
		token = gsub('^\u0120', '', token) # formatting bug with roberta models
	)

# get mean cosine similarity (with and without correction for each model)
cossim.means <- cossim.results |> 
	filter(
		!grepl('most similar$', target_group),
		correction == 'all_but_the_top'
	) |>
	mutate(eval_epoch = case_when(eval_epoch == 0 ~ as.character(eval_epoch), TRUE ~ epoch_criteria)) |>
	group_by(model_id, target_group, eval_epoch) |>
	summarize(mean_cossim_to_targets = mean(cossim)) |>
	filter(
		eval_epoch != 0,
		target_group == 'BLORKED'
	) |> 
	ungroup() |>
	select(-target_group, -eval_epoch)

results <- results |> left_join(cossim.means)
model.results.sentences <- model.results.sentences |> left_join(cossim.means)

# get all excluded subjects
all.excluded.subjects <- c(
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
				subject %in% less.than.75.on.fillers$subject ~ paste0(why, '<75% on fillers; '),
				TRUE ~ why
			),
		why = case_when(
				subject %in% not.above.chance.on.training.str$subject ~ paste0(why, 'n.d. from chance on training structure; '),
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

full.results <- results
full.sentence.results <- model.results.sentences
results <- results |>
	filter(!(subject %in% excluded.for.reasons.other.than.nonlinear$subject)) |>
	droplevels()
	
model.results.sentences <- model.results.sentences |>
	filter(!(subject %in% excluded.for.reasons.other.than.nonlinear$subject)) |>
	droplevels()

# split to separate data frames
exp <- results |>
	filter(condition == 'experimental') |>
	select(-condition)

exp.sents <- model.results.sentences |>
	filter(condition == 'experimental') |>
	select(-condition)

filler <- results |>
	filter(condition == 'filler') |>
	select(-condition)

filler.sents <- model.results.sentences |>
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

# mean accuracy by mean cosine similarity to targets (models only)
exp |>
	filter(!is.na(mean_cossim_to_targets)) |>
	droplevels() |>
	group_by(subject, data_source, voice, target_response, mean_cossim_to_targets, mask_added_tokens, stop_at) |>
	summarize(correct = mean(correct)) |>
	ggplot(aes(x=mean_cossim_to_targets, y=as.numeric(correct), fill=target_response)) +
	geom_point(shape=21, cex=2) +
	geom_smooth(method='lm') +
	ylim(0, 1) +
	xlab('Mean cosine similarity to best targets for blorked (determined pre-fine-tuning)') +
	ylab('Pr. Correct') +
	scale_fill_discrete('Target response') +
	ggtitle(paste0('Pr. Correct by mean cosine similarity to blorked targets')) +
	facet_grid2(data_source ~ mask_added_tokens + stop_at + voice + target_response, scales='free_x', independent='x')

# mean accuracy by mean cosine similarity to targets (models only) and linear
exp |>
	filter(!is.na(mean_cossim_to_targets)) |>
	droplevels() |>
	group_by(subject, data_source, voice, target_response, mean_cossim_to_targets, linear, mask_added_tokens, stop_at) |>
	summarize(correct = mean(correct)) |>
	ggplot(aes(x=mean_cossim_to_targets, y=as.numeric(correct), fill=target_response)) +
	geom_point(shape=21, cex=2) +
	geom_smooth(method='lm') +
	ylim(0, 1) +
	xlab('Mean cosine similarity to best targets for blorked (determined pre-fine-tuning)') +
	ylab('Pr. Correct') +
	scale_fill_discrete('Target response') +
	ggtitle(paste0('Pr. Correct by mean cosine similarity to blorked targets')) +
	facet_grid2(data_source ~ mask_added_tokens + stop_at + linear + voice + target_response, scales='free_x', independent='x')

exp.sents |>
	mutate(
		mask_added_tokens = as.factor(mask_added_tokens) |> fct_relevel('Mask blork', "Don't mask blork"),
		voice = as.factor(voice) |> fct_relevel('SVO active', 'OVS passive', 'OSV active'),
		subj_pref_in_subj_position = logprob_correct_subj > logprob_wrong_obj,
		obj_pref_in_obj_position = logprob_correct_obj > logprob_wrong_subj,
		both_pref = subj_pref_in_subj_position & obj_pref_in_obj_position
	) |>
	pivot_longer(
		c(subj_pref_in_subj_position, obj_pref_in_obj_position, both_pref),
		names_to='preference'
	) |>
	mutate(
		preference = as.factor(preference) |> 
						fct_relevel('subj_pref_in_subj_position', 'obj_pref_in_obj_position', 'both_pref')
	) |>
	ggplot(aes(x=voice, y=as.numeric(value), fill=preference)) +
	stat_summary(fun=mean, geom='bar', position='dodge', width=0.9) +
	stat_summary(fun.data=beta_ci, geom='errorbar', width=0.33, position=position_dodge(0.9)) +
	stat_summary(
		fun.data=\(y) data.frame(y=mean(y), label=sprintf('%.2f', mean(y)), fill='white'), 
		geom='label', position=position_dodge(0.9), show.legend=FALSE
	) +
	scale_x_discrete(
		'Template',
		breaks = c('SVO active', 'OVS passive', 'OSV active'),
		labels = c('SVO', 'OVS', '(OSV)')
	) +
	scale_fill_discrete(
		'Preference',
		breaks = c('subj_pref_in_subj_position', 'obj_pref_in_obj_position', 'both_pref'),
		labels = c('p(s|S) > p(o|S)', 'p(o|O) > p(s|O)', 'Both')
	) +
	ylab('Pr. of sentences') +
	expand_limits(y=c(0,1)) +
	ggtitle('Pr. of sentences by voice and relative probabilities of arguments') +
	facet_grid(data_source ~ mask_added_tokens + stop_at)

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
	
filler.sents |>
	droplevels() |>
	mutate(
		mask_added_tokens = as.factor(mask_added_tokens) |> fct_relevel('Mask blork', "Don't mask blork"),
		voice = as.factor(voice) |> fct_relevel('SVO active', 'OVS passive'),
		subj_pref_in_subj_position = logprob_correct_subj > logprob_wrong_obj,
		obj_pref_in_obj_position = logprob_correct_obj > logprob_wrong_subj,
		both_pref = subj_pref_in_subj_position & obj_pref_in_obj_position
	) |>
	pivot_longer(
		c(subj_pref_in_subj_position, obj_pref_in_obj_position, both_pref),
		names_to='preference'
	) |>
	mutate(
		preference = as.factor(preference) |> 
						fct_relevel('subj_pref_in_subj_position', 'obj_pref_in_obj_position', 'both_pref')
	) |>
	ggplot(aes(x=voice, y=as.numeric(value), fill=preference)) +
	stat_summary(fun=mean, geom='bar', position='dodge', width=0.9) +
	stat_summary(fun.data=beta_ci, geom='errorbar', width=0.33, position=position_dodge(0.9)) +
	stat_summary(
		fun.data=\(y) data.frame(y=mean(y), label=sprintf('%.2f', mean(y)), fill='white'), 
		geom='label', position=position_dodge(0.9), show.legend=FALSE
	) +
	scale_x_discrete(
		'Template',
		breaks = c('SVO active', 'OVS passive'),
		labels = c('SVO', 'OVS')
	) +
	scale_fill_discrete(
		'Preference',
		breaks = c('subj_pref_in_subj_position', 'obj_pref_in_obj_position', 'both_pref'),
		labels = c('p(s|S) > p(o|S)', 'p(o|O) > p(s|O)', 'Both')
	) +
	ylab('Pr. of sentences') +
	expand_limits(y=c(0,1)) +
	ggtitle('Pr. of sentences by voice and relative probabilities of arguments (fillers)') +
	facet_grid(data_source ~ mask_added_tokens + stop_at)
