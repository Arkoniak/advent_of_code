module VirtualMachine

using ReTest
using Setfield

export VM, A, B, reg

@enum OP HLF TPL INC JMP JIE JIO
@enum REG A=1 B=2

const D = Dict(
    "hlf" => HLF,
    "tpl" => TPL,
    "inc" => INC,
    "jmp" => JMP,
    "jie" => JIE,
    "jio" => JIO
)

struct Instruction
    name::OP
    r::REG
    offset::Int
end

struct VM
    program::Vector{Instruction}
    regs::Tuple{Int, Int}
    idx::Int
end

function VM(filename, dd = D)
    prog = Instruction[]
    for line in eachline(filename)
        op = dd[line[1:3]]
        if op != JMP
            reg = line[5] == 'a' ? A : B
        end
        if op in (HLF, TPL, INC)
            push!(prog, Instruction(op, reg, 0))
        elseif op != JMP
            offset = parse(Int, strip(line[7:end]))
            push!(prog, Instruction(op, reg, offset))
        else
            offset = parse(Int, strip(line[5:end]))
            push!(prog, Instruction(op, A, offset))
        end
    end

    return VM(prog, (0, 0), 1)
end

function step(vm::VM)
    t = vm.program[vm.idx]
    r = Int(t.r)
    if t.name == HLF
        @set! vm.regs[r] = vm.regs[r] ÷ 2
        @set! vm.idx += 1
    elseif t.name == TPL
        @set! vm.regs[r] *= 3
        @set! vm.idx += 1
    elseif t.name == INC
        @set! vm.regs[r] += 1
        @set! vm.idx += 1
    elseif t.name == JMP
        @set! vm.idx += t.offset
    elseif t.name == JIE
        if vm.regs[r] & 0x01 == 0x00
            @set! vm.idx += t.offset
        else
            @set! vm.idx += 1
        end
    else
        if vm.regs[r] == 1
            @set! vm.idx += t.offset
        else
            @set! vm.idx += 1
        end
    end

    return vm
end

function run(vm::VM)
    while true
        vm = step(vm)
        ((vm.idx < 1) | (vm.idx > length(vm.program))) && return vm
    end
end

reg(vm::VM, r::REG) = vm.regs[Int(r)]

function reset(vm::VM)
    @set! vm.regs = (0, 0)
    @set vm.idx = 1
end

@testset "Simple loading" begin
    input = """inc a
jio a, +2
tpl a
inc a
"""
    vm = VM(IOBuffer(input))
    @test length(vm.program) == 4
    @test getfield.(vm.program, :name) == [INC, JIO, TPL, INC]
    @test vm.program[2].offset == 2

    input = """inc a
jio a, -2
tpl a
inc a
"""

    vm = VM(IOBuffer(input))
    @test vm.program[2].offset == -2
end

@testset "Step testing" begin
    vm = VM(IOBuffer("hlf a"))
    @set! vm.regs[1] = 10
    vm = step(vm)
    @test vm.regs[1] == 5
    @test vm.idx == 2

    vm = VM(IOBuffer("jie b, -1"))
    @set! vm.regs[2] = 10
    vm = step(vm)
    @test vm.idx == 0

    @set! vm.idx = 1
    @set! vm.regs[2] = 9
    vm = step(vm)
    @test vm.idx == 2
end

@testset "Run test" begin
    input = """inc a
jio a, +2
tpl a
inc a
"""

    vm = VM(IOBuffer(input))
    vm = run(vm)
    @test reg(vm, A) == 2
end

end # module
