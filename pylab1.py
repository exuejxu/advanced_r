#!/usr/bin/env python

import random
import numpy
import math

# 1. Strings
# a)
parrot = "It is dead, that is what is wrong with it."

# b)
i = 0
for c in parrot:
    i = i + 1

print "number of characters:", i

# c)
i = 0
for c in parrot:
    if c.isalpha():
        i = i + 1

print "number of letters:", i

# d)
ParrotWords = parrot.split(" ")
print "ParrotWords:", ParrotWords


# e)
sentence = " ".join(ParrotWords)
print "Merged sentence:", sentence


# 2. Loops and list comprehensions
# a)
for number in range(5, 11):
    print "The next number in the loop is ", number

# b)
while random.uniform(0, 1) < 0.9:
    print "The random number is smaller than 0.9"


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
shortLong = ['long'if len(name) > 4 else 'short' for name in names]
print "shortLong: ", shortLong

# g)
for name, sl in zip(names, shortLong):
    print "The name {0} is a {1} name".format(name, sl)


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
    print "Student {0} has scored {1} on the Algebra exam \
and {2} on the History exam".format(student, students[student]['Algebra'], students[student]['History'])


# 4. Vectors and arrays
# a)
list1 = [1,3,4]
list2 = [5,6,9]
#list1 * list2

# b)
array1 = numpy.asarray(list1)
array2 = numpy.asarray(list2)
print "array1 * array2 = ", array1 * array2

# c)
matrix1 = numpy.matrix([list1, list2])
matrix2 = numpy.matrix([[1,0,0], [0,2,0], [0,0,3]])
print matrix1 * matrix2


# 5. Functions
# a)
def CircleArea(radius):
    if radius < 0:
        print "The radius must be positive"
        return None
    return math.pi * radius * radius

print "The aread of circle with r=2 is: ", CircleArea(2)

# b)
print "The aread of circle with r=-2 is: ", CircleArea(-2)

# c)
def RectangleArea(base,height):
 return base * height
