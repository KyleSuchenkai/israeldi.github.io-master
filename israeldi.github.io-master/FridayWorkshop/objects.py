# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

# primities (integers, booleans)

"""
print(3 + 4)
print(not True)

# objects, like Strings
s = '45$'
print(s.isdigit())
"""

class Animal:
    
    def __init__(self, name_):
        self.name = name_
    
#    def __repr__(self):
#        return self.name

class Dog(Animal):
    
    def __init__(self, name_):
        self.name = name_
    
    def move(self):
        print(self.name + ' runs')
        
class Bird(Animal):
    def __init__(self, name_):
        self.name = name_
    
    def move(self):
        print(self.name + ' flies')
    

loki = Dog('Loki')
#loki.move()

tweetie = Bird('Tweetie')
#tweetie.move()

animals = [loki, tweetie]
for a in animals:
    a.move()
       
