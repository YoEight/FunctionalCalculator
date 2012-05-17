module Calculator (compile, compileAndRun) where
		
import Calculator.Interpreter
import Calculator.Parser.Expression

compileAndRun = (interpret =<<) . compile

