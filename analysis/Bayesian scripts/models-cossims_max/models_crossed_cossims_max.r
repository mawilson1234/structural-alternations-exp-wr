source('Bayesian scripts/summary_functions.r')

fit.model(
	data.type='accuracy_cossims_max',
	model.type='crossed',
	data.file='accuracy-data.csv',
	data.function=function(df) { return (df |> filter(data_source == 'BERT') |> droplevels()) },
	formulae.file='crossed_model_formula_cossims_max.rds',
	formula.no=1,
	model.lists.file='model_lists_accuracy_cossims_max.rds',
	model.no=1
)