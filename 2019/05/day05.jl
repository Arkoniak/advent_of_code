include("../intcode/intcode.jl")

#################################
# Part 1

function part1()
    vm = Prog(readline("input.txt"))
    out = run(vm, 1)
    println("Part 1: ", out[end])
end

part1()

#################################
# Part 2

function part2()
    vm = Prog(readline("input.txt"))
    out = run(vm, 5)
    println("Part 2: ", out[end])
end

part2()
