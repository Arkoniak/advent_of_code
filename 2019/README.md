## AoC 2019 Navigation

Each day has its own separate directory.

In each directory there are following files:

- day##.jl - program with solution for the corresponding day. Consists of two functions `part1` and `part2` plus all necessary auxiliary functions.

- input.txt - input for the corresponding day (if available). I keep them so code can be run, but you should remove them, so AoC wouldn't be spoiled. Be honest to yoursef!

- test##.txt - sometimes it was necessary to make test files, during solution development.

- ##experiments.ipynb - jupyter notebooks where I did all experiments during solution development. Can be rather unstructured, should be ignored most of the time.

Also, there is two additional directories. 

- intcode - contains complete code for intcode virtual machine. Important, that it was used during refactoring of days 2, 5, 7, 9, so these days do not contain original code (but it still can be found in jupyter notebooks)

- misc - unimportant.
