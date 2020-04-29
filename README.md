# SER502-Spring2020-Team26
This repository contains code, sample programs and the entire documentation regarding the implementation details of our new programming language, **ALPI - Another Lexer Parser Interpreter**.

## Tools used to build ALPI
* OS: MacOS and Linux
* Lexer: Python3 using the NLTK package
* Parser: Prolog
* Runtime: Prolog

## How to install and run ALPI?
1. Download our repository either directly from github or just `gitclone https://github.com/sudhanvaghebbale/SER502-Spring2020-Team26.git ` 
2. Go to `SER502-Spring2020-Team26/src/runtime`
3. Check you path for python and swipl by typing  ` where python`,
    `where swipl`
4. Put these paths in their respective files at the top of scripts like so `#!/opt/anaconda3/envs/prolog/bin/python` and `#!/usr/local/bin/swipl`
5. Get current path like so `pwd`
6. Put current path in bash script for python and swipl
    1. `vim ALP`
    2. edit this line to `python your/path/to/here/SER502-Spring2020-Team26/src/runtime/lexer.py`
    3. Same for pl file.
7. Put current path for script.txt and tokens.txt to `lexer.py` and for `program.pl` respectively.
8. Run `chmod +x ALP` and `./ALP` to execute (Run these two everytime a change is made).

#### Misc
1. Install NLTK tokenizer by `pip install nltk`
2. Under lexer.py file uncomment the following if you're running the NLTK tokenizer for the first time. 
```
import nltk
nltk.download('punkt')
```


* Youtube Video Link: 

[Video](https://youtu.be/6mHQA34Qi9o "Named link title")
