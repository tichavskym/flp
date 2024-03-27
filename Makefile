install:
	ghc -o flp-fun  -Wall main.hs 

test:
	echo "Requires test suite to be present in current directory"
	time python3 test_flp.py --test_type training

