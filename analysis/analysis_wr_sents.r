# Load libraries
library(plyr)
library(ggh4x)
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

model.results <- fread('results-models-sents.csv.gz') |>
	as_tibble() |>
	mutate(data_source = gsub('(B|b)ert', 'BERT', str_to_title(model_name)))

# convert model results to format comparable with results from humans
model.results <- model.results |>
	mutate(
		condition = case_when(
						eval_data == 'syn_blorked_SVO-OSV_for_human_exp' ~ 'experimental',
						TRUE ~ 'filler'
					)
	) |>
	# filter out the unwanted eval for the training tokens from the filler items
	filter(
		# keep everything from the experimental conditions
		(condition == 'experimental' & (grepl('(egg|pearl|sheep|milk)', full_ratio_name) && grepl('(tomato|ruby|lobster|blood)', full_ratio_name))) | 
		# keep everything from fillers only when the token_type is "eval special"
		(condition == 'filler' & (!grepl('(egg|pearl|sheep|milk|tomato|ruby|lobster|blood)', full_ratio_name)))
	) |> 
	mutate(
		item = sentence_num,
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
			unique(random_seed)
		),
		args_group = case_when(
						condition == 'experimental' ~ args_group,
						condition == 'filler' ~ gsub('syn_(.*?)_ext_for_human_exp', '\\1', eval_data)
					)
	) |>
	filter(training == 'SVO+OSV') |>
	select(
		subject, condition, training, item, args_group, sentence_type, 
		sentence, adverb, data_source, full_ratio_name, odds_ratio,
		correct, correct.pre.training, voice, mask_added_tokens, stop_at, model_id
	) |> 
	arrange(subject, item, adverb)

# merge model results with human results
results <- model.results |> 
	mutate(
		subject = as.factor(subject),
		args_group = paste(gsub('\\_', '+', args_group), 'args') |> 
						fct_relevel(
							'red+white args',
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
		data_source = fct_relevel(data_source, 'BERT', 'DistilBERT', 'RoBERTa'),
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
		stop_at, training, voice, correct
	) |>
	filter(voice %in% c('SVO active', 'OVS passive')) |> 
	mutate(
		correct = case_when(
					voice %like% 'active' ~ !correct,
					voice %like% 'passive' ~ correct
				)
	) |> 
	group_by(
		subject, mask_added_tokens, stop_at, 
		training, data_source
	) |>
	summarize(
		n_active = length(correct[voice %like% 'active']),
		n_passive = length(correct[voice %like% 'passive']),
		inv.correct = sum(correct[voice %like% 'active']),
		pass.correct = sum(correct[voice %like% 'passive']),
	) |>
	rowwise() |>
	mutate(
		p.value = chisq.test(
							matrix(c(
								inv.correct, 	n_active  - inv.correct,
								pass.correct, 	n_passive - pass.correct
						), ncol=2))$p.value,
		# if both numbers are 0, it is indistinguishable from linear, 
		# but chisq can't tell us that (it gives NaN)
		# convert to a number (Inf) for ease of exclusion below
		p.value = case_when(
							is.na(p.value) ~ Inf,
							TRUE ~ p.value
						)
	) |>
	filter(p.value > 0.05) |> 
	ungroup()

# tag linear performance
results <- results |>
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
py_run_string('import pandas as pd')
py_run_string('from glob import glob')
py_run_string('from tqdm import tqdm')
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

results <- results |>
	left_join(cossim.means)

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
# accuracy by voice, and data_source
exp |> 
	ggplot(aes(x=voice, y=as.numeric(correct))) +
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
	ggtitle(paste0('Pr. Correct by Voice')) +
	facet_grid(data_source ~ mask_added_tokens + stop_at)
	
# accuracy by voice, and data_source (subject means)
exp |> 
	group_by(subject, data_source, mask_added_tokens, stop_at, voice) |>
	summarize(correct = mean(correct)) |>
	ggplot(aes(x=voice, y=as.numeric(correct))) +
	geom_point(
		shape=21,
		cex=2,
		position=position_jitter(width=0.45, height=0.0)
	) +
	geom_violin(alpha=0.3) +
	ylim(0, 1) +
	scale_x_discrete(
		'Template',
		breaks = c('SVO active', 'OVS passive', 'OSV active'),
		labels = c('SVO', 'OVS', '(OSV)')
	) +
	ylab('Pr. Correct') +
	ggtitle(paste0('Pr. Correct by Voice (subject means)')) +
	facet_grid(data_source ~ mask_added_tokens + stop_at)

# accuracy by voice, data_source, and linear
exp |>
	ggplot(aes(x=voice, y=as.numeric(correct))) +
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
	ggtitle(paste0('Pr. Correct by Voice')) +
	facet_grid(data_source ~ mask_added_tokens + stop_at + linear)

# mean accuracy/subject by voice, data_source, and linear
exp |> 
	group_by(subject, data_source, mask_added_tokens, stop_at, linear, voice) |>
	summarize(correct = mean(correct)) |>
	ggplot(aes(x=voice, y=as.numeric(correct))) +
	geom_boxplot(position='dodge', width=0.9) +
	linear_labels_no_humans + 
	ylim(0, 1) +
	scale_x_discrete(
		'Template',
		breaks = c('SVO active', 'OVS passive', 'OSV active'),
		labels = c('SVO', 'OVS', '(OSV)')
	) +
	ylab('Pr. Correct (means)') +
	ggtitle(paste0('Pr. Correct by Voice (subject means)')) +
	facet_grid(data_source ~ mask_added_tokens + stop_at + linear)

# mean accuracy by mean cosine similarity to targets (models only)
exp |>
	filter(!is.na(mean_cossim_to_targets)) |>
	droplevels() |>
	group_by(subject, data_source, voice, mean_cossim_to_targets, mask_added_tokens, stop_at) |>
	summarize(correct = mean(correct)) |>
	ggplot(aes(x=mean_cossim_to_targets, y=as.numeric(correct))) +
	geom_point(shape=21, cex=2) +
	geom_smooth(method='lm') +
	ylim(0, 1) +
	xlab('Mean cosine similarity to best targets for blorked (determined pre-fine-tuning)') +
	ylab('Pr. Correct') +
	ggtitle(paste0('Pr. Correct by mean cosine similarity to blorked targets')) +
	facet_grid2(data_source ~ mask_added_tokens + stop_at + voice, scales='free_x', independent='x')

# mean accuracy by mean cosine similarity to targets (models only) and linear
exp |>
	filter(!is.na(mean_cossim_to_targets)) |>
	droplevels() |>
	group_by(subject, data_source, voice, mean_cossim_to_targets, linear, mask_added_tokens, stop_at) |>
	summarize(correct = mean(correct)) |>
	ggplot(aes(x=mean_cossim_to_targets, y=as.numeric(correct))) +
	geom_point(shape=21, cex=2) +
	geom_smooth(method='lm') +
	ylim(0, 1) +
	xlab('Mean cosine similarity to best targets for blorked (determined pre-fine-tuning)') +
	ylab('Pr. Correct') +
	ggtitle(paste0('Pr. Correct by mean cosine similarity to blorked targets')) +
	facet_grid2(data_source ~ mask_added_tokens + stop_at + linear + voice, scales='free_x', independent='x')

########################################################################
###################### PRE-FINE-TUNING (MODELS ONLY) ###################
########################################################################
# pre-fine-tuning accuracy by voice, and data_source
exp |> 
	filter(data_source != 'human') |>
	select(-correct) |> 
	droplevels() |>
	rename(correct = correct.pre.training) |>
	ggplot(aes(x=voice, y=as.numeric(correct))) +
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
	ggtitle(paste0('Pr. Correct by Voice (pre-fine-tuning)')) +
	facet_grid(data_source ~ mask_added_tokens + stop_at)
	
# pre-fine-tuning accuracy by voice, and data_source (subject means)
exp |> 
	filter(data_source != 'human') |>
	select(-correct) |> 
	droplevels() |>
	rename(correct = correct.pre.training) |>
	group_by(subject, data_source, mask_added_tokens, stop_at, voice) |>
	summarize(correct = mean(correct)) |>
	ggplot(aes(x=voice, y=as.numeric(correct))) +
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
	ggtitle(paste0('Pr. Correct by Voice (subject means, pre-fine-tuning)')) +
	facet_grid(data_source ~ mask_added_tokens + stop_at)

# pre-fine-tuning accuracy by voice, data_source, and linear
exp |>
	filter(data_source != 'human') |>
	select(-correct) |> 
	droplevels() |>
	rename(correct = correct.pre.training) |>
	ggplot(aes(x=voice, y=as.numeric(correct))) +
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
	ggtitle(paste0('Pr. Correct by Voice (pre-fine-tuning)')) +
	facet_grid(data_source ~ mask_added_tokens + stop_at + linear)

# pre-fine-tuning mean accuracy/subject by voice, data_source, and linear
exp |> 
	filter(data_source != 'human') |>
	select(-correct) |> 
	droplevels() |>
	rename(correct = correct.pre.training) |>
	group_by(subject, data_source, mask_added_tokens, stop_at, linear, voice) |>
	summarize(correct = mean(correct)) |>
	ggplot(aes(x=voice, y=as.numeric(correct))) +
	geom_boxplot(position='dodge', width=0.9) +
	linear_labels_no_humans + 
	ylim(0, 1) +
	scale_x_discrete(
		'Template',
		breaks = c('SVO active', 'OVS passive', 'OSV active'),
		labels = c('SVO', 'OVS', '(OSV)')
	) +
	ylab('Pr. Correct (means)') +
	ggtitle(paste0('Pr. Correct by Voice (subject means, pre-fine-tuning)')) +
	facet_grid(data_source ~ mask_added_tokens + stop_at + linear)

# F scores pretraining/subject by data_source, linear, voice
exp |> 
	filter(data_source != 'human') |>
	droplevels() |>
	select(-f.score) |>
	rename(f.score = f.score.pre.training) |>
	select(
		subject, data_source, mask_added_tokens, 
		stop_at, voice, linear, f.score
	) |>
	distinct() |>
	mutate(f.score = case_when(is.na(f.score) ~ 0, TRUE ~ f.score)) |>
	ggplot(aes(x=voice, y=f.score)) +
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
	ggtitle(paste0('Subject F scores by Voice (pre-fine-tuning)')) +
	facet_grid(data_source ~ mask_added_tokens + stop_at + linear)

########################################################################
###################### FILLERS #########################################
########################################################################

# accuracy by voice, and data_source
filler |> 
	ggplot(aes(x=voice, y=as.numeric(correct))) +
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
	ggtitle(paste0('Pr. Correct by Voice (fillers)')) +
	facet_grid(data_source ~ mask_added_tokens + stop_at)

# accuracy by voice, and data_source (subject means)
filler |> 
	group_by(subject, data_source, mask_added_tokens, stop_at, voice) |>
	summarize(correct = mean(correct)) |>
	ggplot(aes(x=voice, y=as.numeric(correct))) +
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
	ggtitle(paste0('Pr. Correct by Voice (subject means, fillers)')) +
	facet_grid(data_source ~ mask_added_tokens + stop_at)

# accuracy by voice, data_source, and linear
filler |>
	ggplot(aes(x=voice, y=as.numeric(correct))) +
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
	ggtitle(paste0('Pr. Correct by Voice (fillers)')) +
	facet_grid(data_source ~ mask_added_tokens + stop_at + linear)

# mean accuracy/subject by voice, data_source, and linear
filler |> 
	group_by(subject, data_source, mask_added_tokens, stop_at, linear, voice) |>
	summarize(correct = mean(correct)) |>
	ggplot(aes(x=voice, y=as.numeric(correct))) +
	geom_boxplot(position='dodge', width=0.9) +
	linear_labels + 
	ylim(0, 1) +
	scale_x_discrete(
		'Template',
		breaks = c('SVO active', 'OVS passive', 'OSV active'),
		labels = c('SVO', 'OVS', '(OSV)')
	) +
	ylab('Pr. Correct (means)') +
	ggtitle(paste0('Pr. Correct by Voice (subject means, fillers)')) +
	facet_grid(data_source ~ mask_added_tokens + stop_at + linear)

# log RT by subject, data_source, linear, voice
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
	ggplot(aes(x=voice, y=log.RT)) +
	geom_boxplot() +
	linear_labels_humans +
	scale_x_discrete(
		'Template',
		breaks = c('SVO active', 'OVS passive', 'OSV active'),
		labels = c('SVO', 'OVS', '(OSV)')
	) +
	ylab('Log RT') +
	ggtitle(paste0('Log RT by Voice (>3 s.d. by subject removed, fillers)')) +
	facet_grid(. ~ linear)

# mean log RT by subject, data_source, linear, voice
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
	group_by(subject, data_source, linear, voice) |>
	summarize(log.RT = mean(log.RT)) |>
	ggplot(aes(x=voice, y=log.RT)) +
	geom_boxplot() +
	linear_labels_humans +
	scale_x_discrete(
		'Template',
		breaks = c('SVO active', 'OVS passive', 'OSV active'),
		labels = c('SVO', 'OVS', '(OSV)')
	) +
	ylab('Log RT (means)') +
	ggtitle(paste0('Log RT by Voice (subject means, >3 s.d. by subject removed, fillers)')) +
	facet_grid(. ~ linear)
	
# accuracy by voice, and data_source pre-fine-tuning
filler |> 
	filter(data_source != 'human') |>
	droplevels() |>
	select(-correct) |>
	rename(correct = correct.pre.training) |> 
	ggplot(aes(x=voice, y=as.numeric(correct))) +
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
	ggtitle(paste0('Pr. Correct by Voice (pre-fine-tuning, fillers)')) +
	facet_grid(data_source ~ .)