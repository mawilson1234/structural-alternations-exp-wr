source('Bayesian scripts/summary_functions.r')

fit.model(
	data.type='accuracy_cossims',
	model.type='nested',
	data.file='accuracy-data-nested.csv',
	data.function=function(df) { return (df |> filter(data_source == 'BERT') |> droplevels()) },
	formulae.file='nested_model_formulae_cossims.rds',
	formula.no=1,
	model.lists.file='model_lists_accuracy_cossims.rds',
	model.no=1
)