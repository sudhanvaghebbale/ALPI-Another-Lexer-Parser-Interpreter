'''

# Authors: Jubanjan Dhar, Ankit Vutukuri, Harmish Ganatra, Sudhanva Hebbale
# Purpose: Lexical Analyzer to generate tokens that will be fed to the parser.
# Version : 1.0
# Date Created: 13 April 2020

'''

#!/opt/anaconda3/envs/prolog/bin/python

from nltk.tokenize import word_tokenize
import re
import sys
from operator import add
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



def handleSpace(res_,operand):
     # tokezie corner cases for addition i+ 1, i+ 1 or i+1;
    posPLUS1 = [pos for pos, char in enumerate(res_) if operand in char and len(char) != 1]
    if len(posPLUS1) == 0:
        #print('found nothing for {0}'.format(operand))
        return res_

    posPLUS = []
    for i in posPLUS1:
        if res_[i] =='++' or res_[i] == '--' or res_[i] == '==':
            pass
        else:
            posPLUS.append(i)
    
    th = []
    tw = []
    for i in posPLUS:
        sp = res_[i].split(operand)
        if sp[1] != '':
            if sp[0] != '':
                '''x+1'''
                th.append(i)
            else:
                tw.append(i)
        else:
            tw.append(i)

    ''' x+1'''
    th_ = [x for x in range(0,len(th)+1) if x % 2 == 0]
    posPLUS_th = list(map(add, th, th_))
    for i in posPLUS_th:
        res_[i:i] = [res_[i].split(operand)[0],operand,res_[i].split(operand)[1]]
        del res_[i+3]

    posPLUS_1 = [pos for pos, char in enumerate(res_) if operand in char and len(char) != 1]
    posPLUS_ = []
    for i in posPLUS_1:
        if res_[i] =='++' or res_[i] == '--' or res_[i] == '==':
            pass
        else:
            posPLUS_.append(i)
    tw = []
    for i in posPLUS_:
        sp = res_[i].split(operand)
        if sp[1] != '':
            if sp[0] != '':
                '''x+1'''
                pass
            else:
                tw.append(i)
        else:
            tw.append(i)

    '''x +1, x+ 1'''
    tw_ = range(len(tw))
    posPLUS_tw = list(map(add, tw, tw_))
    for i in posPLUS_tw:
        sp = res_[i].split(operand)
        if sp[0] == '':
            res_[i:i] = [operand,sp[1]]
            del res_[i+2]
        else:
            res_[i:i] = [sp[0],operand]
            del res_[i+2]
    return res_


def isFloat(string):
    try:
        float(string)
        return True
    except ValueError:
        return False


if __name__ =='__main__':
    res_ =  Lexer('/Users/jubanjanmacbook/PycharmProjects/502_Project/SER502-Spring2020-Team26/src/runtime/script.txt').tokenize()
    
    ''' handle quotes'''


    for j,i in enumerate(res_):
        if i =='``' or i =="''":
            res_[j] = '"'



    # tokenize increment operation, since tokenizer tokenizes decrement only
    pos = [pos for pos, char in enumerate(res_) if '++' in char and len(char)>2 ]
    if len(pos) > 0:
        x_ = range(len(pos))
        pos_ = list( map(add, pos, x_) )
        for i in pos_:
            res_[i:i] = [res_[i].split('++')[0],'++']
            del res_[i+2]
            
    
    # tokenize i==1,i ==1, i== 1
    pos_eq1 = [pos for pos, char in enumerate(res_) if '==' in char]
    if len(pos_eq1) != 0:
        pos_eq = []
        for i in pos_eq1:
            if res_[i] == '==':
                pass
            else:
                pos_eq.append(i)

        x_eq = range(len(pos_eq))
        pos_ = list( map(add, pos_eq, x_eq) )
        for i in pos_:
            sp = res_[i].split('==')
            if sp[0] =='':
                res_[i:i] = ['==',sp[1]]
                del res_[i+2]
            else:
                res_[i:i] = [sp[0],'==']
                del res_[i+2]
            

    #handle x=1, x =1, x= 1           
    res_1 = handleSpace(res_,'=')
    #handle x*1, x *1, x* 1
    res_2 = handleSpace(res_1,'*')
    #handle x/1, x /1, x/ 1
    res_3 = handleSpace(res_2,'/')
    #handle x+1, x +1, x+ 1
    res_4 = handleSpace(res_3,'+')
    #handle x-1, x -1, x- 1
    res_f = handleSpace(res_4,'-')



    

    strg = ''
    for i in res_f:
        if i.isnumeric():
            strg += i+"."+'\n'
        elif isFloat(i):
            strg += i+"."+'\n'
        else:
            strg += "'"+str(i)+"'"+'.'+'\n'
    
   

    with open('tokens.txt','w') as f:
        f.write(strg)
 

    
    





