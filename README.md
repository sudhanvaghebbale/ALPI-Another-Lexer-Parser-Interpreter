# ALPI - Another Lexer Parser Interpreter
This repository contains code, sample programs and the entire documentation regarding the implementation details of our new programming language, **ALPI - Another Lexer Parser Interpreter**.

## Tools used to build ALPI
* OS: MacOS and Linux
* Lexer: Python3 using the NLTK package
* Parser: Prolog
* Runtime: Prolog

## How to install and run ALPI?
1. Download this repository either directly from GitHub or just use `gitclone https://github.com/sudhanvaghebbale/SER502-Spring2020-Team26.git ` 
2. Go to `SER502-Spring2020-Team26/src/runtime`
3. Check the path for Python and swipl by typing:
` where python` and `where swipl`
4. Insert these paths in their respective files at the top of the script like:
`#!/opt/anaconda3/envs/prolog/bin/python` and `#!/usr/local/bin/swipl`
5. Get current path by typing `pwd` in the terminal.
6. Put current path in bash script for Python and swipl
    1. `vim ALP`
    2. edit this line to `python your/path/to/here/SER502-Spring2020-Team26/src/runtime/lexer.py`
    3. Same for pl file.
7. Put the current path for script.txt and tokens.txt to `lexer.py` and `program.pl` respectively.
8. Run `chmod +x ALP` and `./ALP` to execute (Run these two everytime a change is made).

#### Misc
1. Install NLTK tokenizer by `pip install nltk`
2. Under lexer.py file uncomment the following if you're running the NLTK tokenizer for the first time. 
```
import nltk
nltk.download('punkt')
```


#### Youtube Video Link: [https://youtu.be/6mHQA34Qi9o](https://youtu.be/6mHQA34Qi9o)
