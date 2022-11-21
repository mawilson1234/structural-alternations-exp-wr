source('Bayesian scripts/summary_functions.r')

fit.model(
	data.type='accuracy_all',
	model.type='crossed',
	data.file='accuracy-data.csv',
	data.function=ident,
	formulae.file='crossed_model_formula_accuracy.rds',
	formula.no=1,
	model.lists.file='model_lists_accuracy_all.rds',
	model.no=7
)