# Baby Wolfram Alpha

Baby Wolfram Alpha is a simple symbolic computation framework which was a project assignment for Programming in Haskell course at Faculty of Electrical Engineering and Computing in Zagreb, academic year 2016/2017.
The idea is that you can write an arithmetic expression such as: 2 ∗ x + 3 ∗ y ^ 2 + cos y and then do transformations on this expression such as evaluation at a given point, getting a partial derivative, adding it with another expression and so on.

## Descripton

Baby Wolfram Alpha supports:
1. the basic arithmetic operators: +, -, *, /, ^
	- the power function ( ^ ) will always have a number as it’s second argument
2. the unary operators: +, -
3. the basic functions: sin, cos, exp, log
4. the brackets: ( )
5. integer and decimal numbers
6. the arbitrary number of variables consisting of single or more characters


Examples:
```
1. The basic arithmetic operators:
	- +:	1 + sonGoku + 1 + chiChi + 2 + sonGohan + sonGoten
	- -:	hairlessNappa - rapunzelRaditz + 9001
	- *:	( divineDragonShenron + 7 * dragonBalls ) * supernaturalHeaventlyPower
	- /:	nirvanaSearchingPiccolo / bestMateForeverKrilin + whatIfFusionCharacterPrilin
		( theReturnOfCoolCooler / 10 ) / 10
	- ^:	welcomeToTheDarkSideGokuBlack ^ ( - 1000 ) + mightySonGoku ^ 1000
		( kamehamehaaaaaaaaa ^ 9000 ) ^ 9000

2. The unary operators:
		superSpeedyBubbles - 50 * ( - banana ) + hahahahahaKingKai

3. The basic functions:
	- f = (sin, cos, exp, log):
		2 + f 0
		91 * 2 ^ 2 + f ( personalityDisorderedMajinBuu )

4. The brackets:
		( 10 + ( 15 * ( exp studMasterRoshi ) - 11) * 10 )

5. Integer and decimal numbers:
		- 11.158 + 58 * 123456.5

6. The arbitrary number of variables:
		sin ( superSaiyanGodGoku * itsOver9000 + greatApeVegeta - sadisticTyrantFrieza - sleeeeeeeepyBeerus * 0 )

```

Implemented features:
1. Create an expression from input - **createExpression** function:
	- requirement:
		- input needs to be entered in a valid format (every token needs to be separeted with whitespace)

    ```
		-- valid: x * 2 + 3 * ( y + 1 ) + sin x
		-- not valid: x*2 + 3-(y +1 ) + sin x

		testTree = createExpression "2 * ( x ^ 2 ) + sin ( y + x )"

  	```

2. Derive the expression by any variable present in the expression - **deriveBy** function:
	```
	-- expression: 2 * ( x ^ 2 ) + sin ( y + x )
	-- expression derived by x: 0 * x ^ 2 + 2 * 2 * x ^ 1 * 1 + cos ( y + x ) * ( 0 + 1 )
	-- expression derived by x (beautify): 2 * 2 * x + cos ( y + x )

	testDeriveByX = deriveBy testTree "x"
	testDeriveByX = beautify $ deriveBy testTree "x"

	```

3. Substitute the variable in the expression with certain value - **substitute** function:
	```
	-- expression: 2 * ( x ^ 2 ) + sin ( y + x )
	-- expression with substituted variable x: 2 * 0 ^ 2 + sin ( y + 0 )

	testSubsTreeX = substitute "x" testTree 0

	```

4. Evaluate the expression - **evaluate** function:
	- requirement:
		- before an expression can be evaluated, all variables in the expression need to be substituted with certain value

  	```
		-- expression: 2 * ( 2 ^ 2 ) + sin 0
		-- evaluated expression: 8

		testTree2 = createExpression "2 * ( 2 ^ 2 ) + sin 0"

		testEvaluate = evaluate $ testTree2

  	```

5. Beautify the expression - **beautify** function:
	```
	-- expression: ( 2 * ( 2 ^ 0 ) ) + 52.2 ^ 1 - ( sin ( 11 * 0 ) )
	-- beautify expression: 2 + 52.2 - sin 0

	testTree3 = createExpression "( 2 * ( 2 ^ 0 ) ) + 52.2 ^ 1 - ( sin ( 11 * 0 ) )"

	testBeautify = beautify $ testTree3

	```

## Running the project

```
git clone https://github.com/mdrabic5/baby-wolfram-alpha.git

cd BabyWolframAlpha/

ghc -isrc/ Main.hs

./Main
```

## Contributing

Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.


## License

Licensed under the [BSD3 License](LICENSE).
