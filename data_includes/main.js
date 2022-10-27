PennController.ResetPrefix(null) // Shorten command names (keep this line here)
DebugOff()

SetCounter("setcounter")

var counterOverride = 0

var required_to_pass = 0.75
var max_attempts = 5

var blank_style = {
	border: '1px solid #000', 
	width: '5.75em', 
	position: 'relative', 
	'padding-top': '5px',
	top: '-3px'
}

var dropped_word_style = {
	border: '', padding: '', width: blank_style['width'], 
	'text-align': 'center',
	'margin-left': '-0.5em', 'margin-top': '-0px'
}

var centered_justified_style = {
	"text-align": "justify", 
	margin: '0 auto', 
	'margin-bottom': '3em',
	width: '30em'
}

function SepWithN(sep, main, n) {
	this.args = [sep,main];

	this.run = function(arrays) {
		assert(arrays.length == 2, "Wrong number of arguments (or bad argument) to SepWithN");
		assert(parseInt(n) > 0, "N must be a positive number");
		let sep = arrays[0];
		let main = arrays[1];

		if (main.length <= 1)
			return main
		else {
			let newArray = [];
			while (main.length){
				for (let i = 0; i < n && main.length>0; i++)
					newArray.push(main.pop());
				for (let j = 0; j < sep.length && main.length > 0; ++j)
					newArray.push(sep[j]);
			}
			return newArray;
		}
	}
}
function sepWithN(sep, main, n) { return new SepWithN(sep, main, n); }

Sequence(
	"setcounter",
	"consent",
	"instruction1",
	randomize("trial_prac"),
	"instruction2",
	randomize("trial_train"), 'post-training',
	randomize("trial_train_rep1"), 'post-training',
	randomize("trial_train_rep2"), 'post-training',
	randomize("trial_train_rep3"), 'post-training',
	randomize("trial_train_rep4"), 'post-training',
	"instruction3",
	sepWithN('break', randomize("trial"), 57),
	"feedback",
	SendResults(),
	"bye"
)

newTrial("consent",
	newText(
		"Before starting the experiment, you will need to give consent. " + 
		"Please click <a href='https://campuspress.yale.edu/michaelwilson/files/2022/09/consent.pdf' target='_blank'>here</a> to download the consent form for this study. " +
		"If you read it and agree to participate in this study, click 'I Agree' below. " + 
		"If you do not agree to participate in this study, you can leave this page by closing the tab or window."
	)
		.css(centered_justified_style)
		.print()
	,
	
	newButton("Agree", "I Agree")
		.center()
		.print()
		.wait()
).setOption("countsForProgressBar", false)

newTrial("instruction1",
	newText(
		"In this experiment, you will be doing a fill-in-the-blank task. " + 
		"First, you will see a sentence with two blanks in it. " +
		"Below it, there will be a dash. " + 
		"After a delay to give you time to read the sentence, a word will appear in a box where the dash was. " +
		"When the word appears, you should drag it to the blank " +
		"in the sentence where you think it goes best. " +
		"After you have filled a blank with the word, you will see a \"Next\" button, which you can click " +
		"to go to the next item.<p />" +
		
		"Since you will only fill in one of the two blanks, " +
		"<b>the sentence will still be missing one word at the end of each trial</b>.<p />" + 
		
		"First, you will do some practice to get you used to how this works. " +
		"During the practice, you will get feedback on whether you chose the correct blank for the word, " +
		"and if you chose incorrectly, you should move the word to the correct blank to continue. " +
		"However, you will not get feedback during the main part of the experiment.<p />" +
		
		"Click below when you are ready to begin practice."
	)
		.css(centered_justified_style)
		.print()
	,

	newButton("Click here to begin practice!")
		.center()
		.print()
		.wait()
).setOption("countsForProgressBar", false)

var feedback_trial = label => item => {
	var word_num	 = Math.floor(Math.random() * 8)
	var target_res   = label.startsWith('trial_train') ? item['target_response'] : (word_num <= 3 ? '[subj]' : '[obj]')
	var word		 = label === 'trial_prac' ? item['word_' + word_num] : item.word
	var presentence  = item.sentence.match(/^(.*?)(?=\[(su|o)bj\])/g)[0] + '&nbsp;'
	var midsentence  = item.sentence.match(/(?:\[(su|o)bj\])(.*?)(?=\[(su|o)bj\])/)[2] + '&nbsp;'
	var midsentence  = midsentence.startsWith(',') ? midsentence : '&nbsp;' + midsentence
	var postsentence = item.sentence.match(/.*(?:\[(su|o)bj\])(.*?)$/)[2]
	var postsentence = postsentence.startsWith(',') | postsentence.startsWith('.') ? postsentence : '&nbsp;' + postsentence
	var first_arg	 = item.sentence.match(/\[(su|o)bj\]/g)[0]
	var second_arg   = item.sentence.match(/\[(su|o)bj\]/g)[1]
	
	return newTrial(label,
		newText("word", word).css({width: '', border: '1px solid #000', padding: '3px'}).center(),
		
		newMouseTracker("mouse").log(),
		newFunction("startmouse", async () => {
			await new Promise(r => getText("word")._element.jQueryContainer.mousedown(r))
			getMouseTracker("mouse").start()._runPromises()
		}),
		
		newVar('trial_no')
			.global()
			.test.is(v => v >= 1)
				.success(getVar('trial_no').set(v => label === 'trial_prac' ? 0 : v + 1))
				.failure(getVar('trial_no').set(v => label === 'trial_prac' ? 0 : 1))
		,
		newVar('responses', []).global(),
		newVar('grandaverage', 0).global()
			.test.is(v => v >= required_to_pass).success(end()),
		newVar('firstdropped', 'no drop yet'),
		
		newText("container", "").center().css({display: "flex", 'margin-bottom': '3em'}).print(),
		newText(presentence).print(getText("container")),
		newText(first_arg, " ").css(blank_style).print(getText("container")),
		newText(midsentence).print(getText("container")),
		newText(second_arg, " ").css(blank_style).print(getText("container")),
		newText(postsentence).print(getText("container")),
		
		newText("placeholder", "&mdash;").center().print(),
		newTimer("wait", item.sentence.split(" ").length * 325).start().wait(),
		getText("placeholder").remove(),
		
		getText("word").print(),
	   	
		getFunction("startmouse").call(),
		
		newText("correct", "Good job&mdash;that's the right choice!").css('color', 'rgb(34, 139, 34)').center(),
		newText("incorrect", "That's not the right one&mdash;try moving the word to the other blank!").css('color', 'rgb(188, 74, 60)').center(),
		
		newDragDrop("dd", "bungee")
			.log("all")
			.addDrop(getText(first_arg), getText(second_arg))
			.addDrag(getText("word"))
			.callback(
				getText("correct").remove(), getText("incorrect").remove(),
				self.test.dropped(getText(target_res))
					.success(
						getText("correct").print(),
						getMouseTracker("mouse").stop(),
						getVar('firstdropped').test.is(v => v === 'no drop yet')
							.success(
								getVar('trial_no').test.is(v => label === 'trial_train' ? v > 12 : (label === 'trial_prac' ? false : true))
									.success(getVar('responses').set(v => [true, ...v]))
							)
					)
					.failure(
						getText("incorrect").print(),
						getVar('firstdropped').test.is(v => v === 'no drop yet')
							.success(
								getVar('firstdropped').set('dropped already'),
								// if it is the first training set, we only want to count accuracy for the second half of the items
								// since there are 24 items, indexed from 0, this is true if the trial_no is greater than 11
								// if the label is not trial_train, it is trial_train_rep, and we always want to record the accuracy since
								// these have only 12 items. thus, we run a check that will always return true
								// to deal with the fact that we don't care about accuracy of the earlier trials, we just don't set anything
								// if the check fails
								getVar('trial_no').test.is(v => label === 'trial_train' ? v > 12 : (label === 'trial_prac' ? false : true))
									.success(getVar('responses').set(v => [false, ...v]))
							)
					),
					getText("word").css(dropped_word_style)
			)
			.offset('0.5em', '0.1em', getText(first_arg), getText(second_arg))
			.wait(self.test.dropped(getText(target_res)))
			.removeDrag(getText("word"))
			.removeDrop(getText(first_arg), getText(second_arg))
		,
		
		newButton("next", "Next").center().print().wait().remove()
	)
	.log('item'				, item.item)
	.log('word'				, word)
	.log('target_response' 	, target_res)
	.log('args_group'	  	, item.group)
	.log('sentence_type'   	, item.sentence_type)
	.log('sentence'			, item.sentence)
	.log('adverb'		  	, item.adverb)
	.log('seen_in_training'	, 'True')
}

Template("practice.csv", feedback_trial('trial_prac'))

newTrial('post-training',
	newVar('message')
		.global()
	,
	newVar('attempts', 0)
		.global()
		.set(v => v + 1)
	,
	newVar('grandaverage')
		.global()
		.test.is(v => v >= required_to_pass)
		.failure(
			getVar('grandaverage')
				.set(getVar('responses'))
				.set(v => v.filter(r => r == true).length/v.length)
		)
	,
	getVar('grandaverage')
		.test.is(v => v >= required_to_pass)
		.success(
			getVar('message')
				.test.is('Great job!')
					.success(end())
					.failure(getVar('message').set('Great job!'))
		)
		.failure(
			getVar('attempts')
				.test.is(v => v < max_attempts)
					.success(
						getVar('message').set(
							'Please try again. Remember, you should try to pay attention ' +
							'to where different words go best in sentences with <i>blork</i>.'
						)
					)
					.failure(getVar('message').set(''))
		)
	,
	newVar('grandaveragepercent')
		.set(getVar('grandaverage'))
		.set(v => Math.round(v * 100) + '%.')
	,
	newVar('responses').global().set([]),
	newText("Your first-choice accuracy was&nbsp;")
		.after(
			newText().text(getVar('grandaveragepercent'))
		)
		.css('margin-bottom', '3em')
		.center()
		.print()
	,
	getVar('message')
		.test.is(v => v === 'Great job!')
			.success(
				newText()
					.text(getVar('message'))
					.css('margin-bottom', '3em')
					.center()
					.print()
			)
			.failure(
				newText()
					.text(getVar('message'))
					.css(centered_justified_style)
					.print()
			)
	,
	newButton('Next')
		.center()
		.print()
		.wait()
)

newTrial("instruction2",
	newText(
		"You have now finished the practice session! Next, you will move on " +
		"to the first part of the experiment, which is a training session.<p />" +
		
		"During training, you will learn about a new word, <i>blork</i>. " +
		"You will see sentences like the following: "
	)
		.css(centered_justified_style)
		.print()
	,
	
	newText("container", "").center().css({display: "flex", 'margin-bottom': '3em'}).print(),
	newText("The&nbsp;").print(getText("container")),
	newText("firstblank", " ").css(blank_style).print(getText("container")),
	newText("&nbsp;has always blorked the&nbsp;").print(getText("container")),
	newText("secondblank", " ").css(blank_style).print(getText("container")),
	newText(".").print(getText("container")),
	
	newText(
		"Just like in the sentences you saw during the practice session, " +
		"you will find that some words go better in certain slots than others in sentences with <i>blork</i>."+ 
		"Your task during this training phase is to figure out this pattern: where do different words go best " +
		"in sentences with <i>blork</i>?<p />" +
		
		"As you begin training, you should start by guessing which blank the word should go in. " +
		"If you guess wrong, you will see a message that you should have chosen the other blank. " +
		"Then, you'll need to move the word from the wrong blank to the right one to continue on. " +
		"You should use this feedback to help you learn.<p />" +
		
		"Because we want you to learn as much as you can about the pattern of words in sentences with <i>blork</i>, " +
		"we would like you to continue the training process until you are able to choose correctly 75% of the time. " +
		"We'll let you repeat the training session a number of times until you reach 75% accuracy, " +
		"and give you feedback on how you did at the end of each session. " +
		"In any case, don't worry if you have difficulty: there is a limit on how many sessions " +
		"we will ask you to do, even if you aren't able to reach 75%.<p />" +
		
		"The first several trials of the first training session will be a warm-up. " +
		"This warm-up won't count towards your accuracy, so feel free to guess for a few trials " +
		"while you are figuring out where different words go best in sentences with <i>blork</i>. " +
		"We ask that you please don't write anything down, and just try to figure things out on your own.<p />" +
		
		"When you are finished with the training session, you will see one more message before " +
		"going on to the last part of the experiment, which is a test session.<p />" +
		"Click below when you are ready to begin the training session."
	)
		.css(centered_justified_style)
		.print()
	,
	
	newButton("Click here to begin training!")
		.center()
		.print()
		.wait()
)

Template("train.csv", feedback_trial('trial_train'))
Template("train_rep1.csv", feedback_trial('trial_train_rep1'))
Template("train_rep2.csv", feedback_trial('trial_train_rep2'))
Template("train_rep1.csv", feedback_trial('trial_train_rep3'))
Template("train_rep1.csv", feedback_trial('trial_train_rep4'))

newTrial("instruction3",
	newText(
		"Now, you are ready to start the last part of the experiment! " +
		"In this part, which is a test session, you will see many sentences. " +
		"Most will have the new word, <i>blork</i>, which you just learned about, " +
		"and others will have words you already know.<p />" +
		
		"Remember, your job is to decide which blank the word below the sentence goes best in. " +
		"During the test session, you will not get any feedback about your choice, " +
		"and you will not be able to change your first choice. Make sure to try your best, and good luck!<p />" +
		
		"During this part of the experiment, you will have the opportunity to take two short breaks partway through. " +
		"We ask that you take only as much time as you need, so that you don't forget what you just " +
		"learned about words that go with <i>blork</i>!<p />" +
		
		"Click below when you are ready to begin the test session."
	)
		.css(centered_justified_style)
		.print()
	,

	newButton("Click here to begin the test session!")
		.center()
		.print()
		.wait()
)

/*
Template("pretrial.csv", variable => 
	newTrial("pretrial",
		// store the sentence in a variable so we can modify it
		newVar("sentence", variable.sentence)
			.log()
		,
		// reverse order of placeholders 50% of the time
		newFunction("XXXX_last", () => Math.random() <= 0.5)
			.call()
			.test.is(1)
			.success(
				getVar("sentence")
					.set(
						v => v
							.replace("XXXX", "ZZZZ")
							.replace("YYYY", "XXXX")
							.replace("ZZZZ", "YYYY")
					)
			)
		,
		
		newText("sentence")
			.before(newText("p", "<p>"))
			.text(getVar("sentence"))
			.after(newText("close_p", "</p>"))
			.center()
			.print()
		,
		
		newText("sep", "___________________________________________")
			.center()
			.print()
		,
		
		newText("question", "<p>Where is <i>" + variable.word + "</i> more likely to go?</p>")
			.center()
			.print()
		,
		
		newSelector("position")
		,
		
		newCanvas("buttons", 200, 50)
			.add(			  0, 0, newButton("XXXX").selector("position"))
			.add("right at 100%", 0, newButton("YYYY").selector("position"))
			.center()
			.print()
		,
		
		getSelector("position")
			.shuffle()
			.once()
			.wait()
			.log()
		,
		
		getCanvas("buttons")
			.remove()
		,
		
		newButton("Next")
			.center()
			.print()
			.wait()
	)
	.log("group"		, variable.group)
	.log("item"		 , variable.item)
	.log("word"		 , variable.word)
	.log("args_group"   , variable.args_group)
	.log("sentence_type", variable.sentence_type)
)
*/

var trial = group_label => item => {
	var word_num		 = Math.floor(Math.random() * 12)
	var target_res		 = group_label == 'filler_group' ? (word_num <= 5 ? '[subj]' : '[obj]') : item.target_response
	var word			 = group_label == 'filler_group' ? item['word_' + word_num] : item.word
	var presentence		 = item.sentence.match(/^(.*?)(?=\[(su|o)bj\])/g)[0] + '&nbsp;'
	var midsentence 	 = item.sentence.match(/(?:\[(su|o)bj\])(.*?)(?=\[(su|o)bj\])/)[2] + '&nbsp;'
	var midsentence 	 = midsentence.startsWith(',') ? midsentence : '&nbsp;' + midsentence
	var postsentence	 = item.sentence.match(/.*(?:\[(su|o)bj\])(.*?)$/)[2]
	var postsentence 	 = postsentence.startsWith(',') | postsentence.startsWith('.') ? postsentence : '&nbsp;' + postsentence
	var first_arg		 = item.sentence.match(/\[(su|o)bj\]/g)[0]
	var second_arg		 = item.sentence.match(/\[(su|o)bj\]/g)[1]
	var seen_in_training = group_label == 'filler_group' ? 'NA' : item.seen_in_training
	
	return newTrial("trial",		
		newText("container", "").center().css({display: "flex", 'margin-bottom': '3em'}).print(),
		newText(presentence).print(getText("container")),
		newText(first_arg, " ").css(blank_style).print(getText("container")),
		newText(midsentence).print(getText("container")),
		newText(second_arg, " ").css(blank_style).print(getText("container")),
		newText(postsentence).print(getText("container")),
		
		newText("placeholder", "&mdash;").center().print(),
		newTimer("wait", item.sentence.split(" ").length * 325).start().wait(),
		getText("placeholder").remove(),
		
		newText("word", word).css({border: '1px solid #000', padding: '3px'}).center().print(),
		
		newMouseTracker("mouse").log(),
		newFunction(async () => {
			await new Promise(r => getText("word")._element.jQueryContainer.mousedown(r))
			getMouseTracker("mouse").start()._runPromises()
		}).call(),
		
		newDragDrop("dd", "bungee")
			.log("all")
			.addDrop(getText(first_arg), getText(second_arg))
			.addDrag(getText("word"))
			.callback()
			.offset('0.5em', '0.1em', getText(first_arg), getText(second_arg))
			.wait()
			.removeDrag(getText("word"))
			.removeDrop(getText(first_arg), getText(second_arg))
		,
		
		getMouseTracker("mouse").stop(),
		
		getText("word").css(dropped_word_style),
		
		newButton("next", "Next").center().print().wait().remove()
	)
	.log('item'				, item.item)
	.log('word'				, word)
	.log('target_response' 	, target_res)
	.log('args_group'	  	, item[group_label])
	.log('sentence_type'   	, item.sentence_type)
	.log('sentence'			, item.sentence)
	.log('adverb'		  	, item.adverb)
	.log('seen_in_training'	, seen_in_training)
}

Template("stimuli.csv", trial("group"))
Template("fillers.csv", trial("filler_group"))

newTrial('break',
	newText('You may now take a short break. ' +
			'Please don\'t take too long, so you don\'t forget what you know about <i>blork</i>! ' +
			'Click below when you are ready to return to the experiment.')
		.print()
	,
	newButton('click', 'Click here to return to the experiment!')
		.center()
		.print()
		.wait()
)

newTrial("feedback",
	newText(
		"If you have any feedback on the experiment, please type it here. " +
		"We would be especially interested to hear if you have any thoughts " +
		"about whether you thought of words that might be similar in meaning to <i>blork</i>.<p />" +
		
		"We would also like to know what guesses you may have about what determines where different " +
		"words can go with <i>blork</i>.<p />" +
		
		"If you don't have any feedback, " +
		"you can leave this blank and continue by pressing \"Send\" below."
	)
		.css(centered_justified_style)
		.print()
	,

	newTextInput("feedback", "")
		.center()
		.log()
		.lines(0)
		.size(420, 200)
		.print()
	,
	
	newText("<p />").center().print(),

	newButton("Send")
		.center()
		.print()
		.wait()
)

newTrial("bye",
	newText(
		"Thank you for participating!<p />" +
		
		"Please go to the following web page to verify your participation: " +
		"<a href='https://app.prolific.co/submissions/complete?cc=CEU2H725' target='_blank'>" +
		"https://app.prolific.co/submissions/complete?cc=CEU2H725" +
		"</a>."
	)
		.css({'margin': 'auto', 'text-align': 'center', 'width': '30em'})
		.print()
	,
	
	newButton().wait()  // Wait for a click on a non-displayed button = wait here forever
)
.setOption("countsForProgressBar", false)