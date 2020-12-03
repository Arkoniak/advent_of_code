using Revise
using ReTest
using Setfield

includet("vm.jl")
using .VirtualMachine
runtests(VirtualMachine)

vm = VM("input.txt")
reg(VirtualMachine.run(vm), B)

@set! vm.regs[1] = 1
reg(VirtualMachine.run(vm), B)
