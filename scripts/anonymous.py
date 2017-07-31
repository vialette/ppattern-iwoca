import os
import sys
import fileinput
from glob import glob

PATH = "../src"

line_authors   = "(c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette"
line_anonymous = "(c) anonymous, 2016-1017"

def replace(filenames, line_to_search, line_to_replace):
    for filename in filenames:
        print("processing {}".format(filename))
        if filename == '../src/Data/Algorithm/PPattern.hs':
            with fileinput.FileInput(filename, inplace = True) as file:
                for line in file:
                    print(line.replace(line_to_search, line_to_replace), end = '')

def anonymous(filenames):
    replace(filenames, line_authors, line_anonymous)

def deanonymous(filenames):
    replace(filenames, line_anonymous, line_authors)

if __name__ == '__main__':
    filenames = [f for e in os.walk(PATH) for f in glob(os.path.join(e[0], '*.hs'))]
    anonymous(filenames)
