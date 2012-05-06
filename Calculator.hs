module Calculator 
	( compile
	, compileAndRun) where
		
import Calculator.Interpreter
import Calculator.Parsing

compileAndRun = (fmap run) . compile

