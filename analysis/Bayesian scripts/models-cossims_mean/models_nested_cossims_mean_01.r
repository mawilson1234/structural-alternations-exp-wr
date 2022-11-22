source('Bayesian scripts/summary_functions.r')

fit.model(
	data.type='accuracy_cossims_mean',
	model.type='nested',
	data.file='accuracy-data-nested.csv',
	data.function=function(df) { return (df |> filter(data_source == 'BERT') |> droplevels()) },
	formulae.file='nested_model_formulae_cossims_mean.rds',
	formula.no=1,
	model.lists.file='model_lists_accuracy_cossims_mean.rds',
	model.no=1
)