from nltk.tokenize import word_tokenize


class Lexer():
    def __init__(self,FileName):
        self.file = FileName
        self.data = self.readFile()
        self.strg = ''


    def readFile(self):
        with open (self.file, 'r') as f:
            data = f.readlines()
        return data

    def tokenize(self):
        for ls in self.data:
            self.strg += ls
        
        return word_tokenize(self.strg)




if __name__ =='__main__':
    res_ =  Lexer('./script.txt').tokenize()
    with open('tokens.txt','w') as f:
        f.write(str(res_))
    print('Tokens compiled to tokens.txt')

    
    





