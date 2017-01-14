import random
import math
import scipy
import numpy as np

# 1. Strings
# a)
parrot = "It is dead, that is what is wrong with it."

# b)
len(parrot)

# c)
i = 0
for c in parrot:
    if c.isalpha(): i = i + 1
    
print "number of letters:", i

# d)
ParrotWords = parrot.split(" ")
print "ParrotWords:", ParrotWords

# e)
ParrotWords = " ".join(ParrotWords)
print "ParrotWords:", ParrotWords

# 2. Loops and list comprehensions
# a)
for number in range(5, 11):
    print "The next number in the loop is ", number

# b)
while random.uniform(0,1) < 0.9:
    print "The random number is smaller than 0:9."

# c)
names = ['Ludwig','Rosa','Mona','Amadeus']
for name in names:
    print "The name %s is nice" % name

# d)
nLetters = [None] * len(names)
for i, name in enumerate(names):
   nLetters[i] = len(name)
print "nLetters: ", nLetters

# e)
nLetters = [len(name) for name in names]
print "nLetters: ", nLetters

# f)
shortLong = ["long" if len(name) > 4 else "short" for name in names]
print "shortLong: ", shortLong

# g)
for name, sl in zip(names, shortLong):
    print "The name %s is a %s name" % (name, sl)

# 3. Dictionaries
# a)
Amadeus = {'Sex':'M', 'Algebra':8, 'History':13}
print "Amadeus: ", Amadeus

# b)
Rosa = {'Sex':'F', 'Algebra':19, 'History':22}
Mona = {'Sex':'F', 'Algebra':6, 'History':27}
Ludwig = {'Sex':'M', 'Algebra':9, 'History':5}

# c)
students = {'Amadeus':Amadeus, 'Rosa':Rosa, 'Mona':Mona, 'Ludwig':Ludwig}
print "Amadeus's History score is: ", students['Amadeus']['History']

# d)
students['Karl'] = {'Sex':'M', 'Algebra':14, 'History':10}

# e)
for student in students:
    print "Student %s scored %d on the Algebra exam and %d on the History exam" % (student, students[student]["Algebra"], students[student]["History"]) 

# 4. Vectors and arrays
# a)
list1 = [1,3,4]
list2 = [5,6,9]
#list1*list2
#it does not work.

# b)
array1 = scipy.array(list1)
array2 = scipy.array(list2)
array1*array2

# c)
matrix1 = scipy.array([list1, list2])
matrix2 = scipy.array([[1,0,0],[0,2,0],[0,0,3]])
matrix1*matrix2
# It does not work.
# ValueError: operands could not be broadcast together with shapes (2,3) (3,3)
# ndarray objects are just n-dimensional containers.
# it does not behave as matrices unless specific functions are used to perform matrix operations.

# d)
x = scipy.array([list1, list2])
y = scipy.array([[1,0,0],[0,2,0],[0,0,3]])
print np.dot(x,y)

    
# 5. Functions
# a)
def CircleArea(radius):
    return math.pi * radius ** 2

# b)
def CircleArea(radius):
    if radius < 0:
        print "The radius must be positive."
    else:
        return math.pi * radius ** 2
# print "The aread of circle with r=1 is: ", CircleArea(1)
# print "The aread of circle with r=-1 is: ", CircleArea(-1)

# c)
def RectangleArea(base,height):
    return base * height



