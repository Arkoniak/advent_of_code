module AOC2020

using Setfield
using UnPack
import Base: length

export VM, shag, exec, @vm_str, Instruction

@enum OP NOP JMP ACC
@enum STATE RUN STOP

struct Instruction
    op::OP
    v::Int
end

struct VM
    cursor::Int
    code::Vector{Instruction}
    regs::Int
    state::STATE
end

const RE = Dict(r"nop ([+-]\d+)" => m -> Instruction(NOP, parse(Int, m[1])),
                r"acc ([+-]\d+)" => m -> Instruction(ACC, parse(Int, m[1])),
                r"jmp ([+-]\d+)" => m -> Instruction(JMP, parse(Int, m[1])),)

function Instruction(s::T) where {T <: AbstractString}
    for (re, f) in RE
        m = match(re, s)
        !isnothing(m) && return f(m) 
    end
end

VM(s::T) where {T <: AbstractString} = VM(IOBuffer(s))

function VM(io::IO)
    code = Instruction[]
    for line in eachline(io)
        push!(code, Instruction(line))
    end

    return VM(1, code, 0, RUN)
end

macro vm_str(s)
    VM(s)
end

Base.length(vm::VM) = length(vm.code)

function shag(vm::VM)
    vm.state == STOP && return vm
    @unpack op, v = vm.code[vm.cursor]
    if op == NOP
        @set! vm.cursor += 1
    elseif op == JMP
        @set! vm.cursor += v
    elseif op == ACC
        @set! vm.cursor += 1
        @set! vm.regs += v
    end

    if vm.cursor > length(vm) || vm.cursor < 0
        @set! vm.state = STOP
    end

    return vm
end

function exec(vm::VM)
end

end # module
