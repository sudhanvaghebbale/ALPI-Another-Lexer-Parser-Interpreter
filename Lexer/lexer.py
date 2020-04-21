from nltk.tokenize import word_tokenize
import re

# Need the below lines if running for the first time

# import nltk
# nltk.download('punkt')

class Lexer():
    def __init__(self,FileName):
        self.file = FileName
        self.data = self.readFile()
        self.strg = ''


    def readFile(self):
        with open (self.file, 'r') as f:
            data = self.removeComments(f.read())
        
        return data


    def returnData(self):
        return self.data

    def tokenize(self):
        for ls in self.data:
            self.strg += ls
        
        return word_tokenize(self.strg)

    def removeComments(self,data):
        
        def replacer(match):
            s = match.group(0)
            if s.startswith('/'):
                return "\n" * s.count( "\n" ) # note: a space and not an empty string
            else:
                return s

        pattern = re.compile(r'//.*?$|/\*.*?\*/|\'(?:\\.|[^\\\'])*\'|"(?:\\.|[^\\"])*"',re.DOTALL | re.MULTILINE)

        return re.sub(pattern, replacer, data) 




if __name__ =='__main__':
    res_ =  Lexer('./script.txt').tokenize()
    # check = ['begin', 'num', 'x', '=', '2', ';', 'num', 'r', ';', 'num', 'u', '=', '0', ';', 'num', 'z', ';', 'num', 'v', ';', 'str', 's', '=', '"', 'sudhanva', '"', ';', 'num', 'y', '=', '0', ';', 'for', 'i', 'in', 'range', '(', '1', ',' , '5', ')', '{', 'x', '=', 'x', '+', '1', ';', 'y', '=', 'y', '+', '1', '}', ';', 'while', '(', 'not', 'u', '==', '3', ')', '{', 'z', '=', 'x', '*', '2', ';', 'u', '=', 'u', '+', '1','}',';', 'if', '(', 'z', '>', 'x', ')', '{', 'v', '=', '1',';', '}', 'else', '{', 'v', '=', '0',';', '}', ';', 'x', '<', '3', '?', 'r', '=', '0', ':', 'r', '=' , '1', ';', 'end']
    # print(len(check))
    for j,i in enumerate(res_):
        if i =='``' or i =="''":
            res_[j] = '"'
    
    strg = ''
    for i in res_:
        strg += i+','+'\n'

    with open('tokens.txt','w') as f:
        f.write(strg)
 

    
    





