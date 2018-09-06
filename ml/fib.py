# -*- coding: utf-8 -*-
"""
Created on Wed Aug 22 18:24:04 2018

@author: pwend
"""

def fib(x):
    if x<=2:
        return(1)
    else:
        return(fib(x-1)+fib(x-2))
        
        
print(fib(25))