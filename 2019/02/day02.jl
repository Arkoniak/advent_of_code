include("../intcode/intcode.jl")

#################################
# Part 1
function part1()
    vm = Prog(readline("input.txt"))
    vm.code[2] = 12
    vm.code[3] = 2
    run(vm)
    println("Part 1: ", vm.code[1])
end

part1()

#################################
# Part 2

function part2()
    for noun in 0:99, verb in 0:99
        vm = Prog(readline("input.txt"))
        vm.code[2] = noun
        vm.code[3] = verb
        run(vm)
        if vm.code[1] == 19690720
            println("Part 2: ", noun*100 + verb)
            break
        end
    end
end

part2()
