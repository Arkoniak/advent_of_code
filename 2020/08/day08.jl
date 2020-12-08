using Revise
using AOC2020
using AOC2020: STOP
using AOC2020: NOP, JMP
using Setfield

vm = vm"""
nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6
"""

function part1(vm)
    res = zeros(Int, length(vm))
    while true
        vm.state == STOP && return (vm, vm.regs)
        res[vm.cursor] == 1 && return (vm, vm.regs)
        res[vm.cursor] += 1
        vm = shag(vm)
    end
end

@assert part1(vm)[2] == 5
println("Part 1: ", part1(VM(open("input.txt")))[2])

function part2(vm)
    for i in 1:length(vm)
        vm1 = deepcopy(vm)
        ins = vm1.code[i]
        if ins.op == NOP
            vm1.code[i] = @set ins.op = JMP
        elseif ins.op == JMP
            vm1.code[i] = @set ins.op = NOP
        else
            continue
        end
        vm1, res = part1(vm1)
        vm1.state == STOP && return res
    end
end

@assert part2(vm) == 8
println("Part 2: ", part2(VM(open("input.txt"))))
