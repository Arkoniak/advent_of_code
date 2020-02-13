###########################
# Part 1

include("../intcode/intcode.jl")

function part1()
    vm = Prog(readline("input.txt"))
    println(run(vm, 1))
    # println("Part 1: ", run(prog, 1)[1])
end

part1()

########################
# Part 2

function part2()
    vm = Prog(readline("input.txt"))
    println(run(vm, 2))
    # println("Part 2: ", run(prog, 2)[1])
end

part2()
