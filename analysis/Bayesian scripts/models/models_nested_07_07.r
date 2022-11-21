source('Bayesian scripts/summary_functions.r')

fit.model(
	data.type='accuracy_all',
	model.type='nested',
	data.file='accuracy-data-nested.csv',
	data.function=ident,
	formulae.file='nested_model_formulae_accuracy.rds',
	formula.no=7,
	model.lists.file='model_lists_accuracy_all.rds',
	model.no=7
)