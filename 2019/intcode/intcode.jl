module IntCode
using StaticArrays
struct FastHashInt{T<:Integer}; i::T; end

Base.:(==)(x::FastHashInt, y::FastHashInt) = x.i == y.i
Base.hash(x::FastHashInt, h::UInt) = xor(UInt(x.i), h)

export Prog, run
############## IntCode ##############################
# states are
# 0 - running
# 1 - waiting for input
# 99 - halt

mutable struct Prog
    code::Vector{Int}
    cur::Int
    input::Vector{Int}
    output::Vector{Int}
    relative_base::Int
    state::Int
    ascii::Bool
    # modes::Dict{FastHashInt{Int}, Tuple{Int, Vector{Int}}}
    modes::Vector{Tuple{Int, Tuple{Int, Int, Int}}}
end

str2prog(s) = parse.(Int, split(s, ","))

function Prog(s::String; mem = 10000, ascii = false)
    modes = Vector{Tuple{Int, Tuple{Int, Int, Int}}}(undef, 22300)
    for i in 0:2, j in 0:2, k in 0:2
        for op in vcat(0:9, 99)
            modes[10000*i + 1000*j + 100*k + op + 1] = (op, (k, j, i))
        end
    end
    Prog(vcat(str2prog(s), zeros(Int, mem)), 1, [], [], 0, 0, ascii, modes)
end

copy_code(p::Prog) = Prog(copy(p.code), 1, [], [], 0, 0, p.ascii)

@inline function take(prog::Prog, mode, offset)
    if mode == 0
        return @inbounds prog.code[prog.code[prog.cur + offset] + 1]
    elseif mode == 1
        return @inbounds prog.code[prog.cur + offset]
    else
        return @inbounds prog.code[prog.code[prog.cur + offset] + prog.relative_base + 1]
    end
end

@inline function update!(prog::Prog, value, offset, mode = 0)
    if mode == 0
        @inbounds prog.code[prog.code[prog.cur + offset] + 1] = value
    elseif mode == 2
        @inbounds prog.code[prog.code[prog.cur + offset] + prog.relative_base + 1] = value
    end
end

function step(prog::Prog)
    @inbounds op_code = prog.code[prog.cur]
    op, modes = prog.modes[op_code + 1]

    if op == 99
        prog.state = 99
    elseif op == 1
        a1 = take(prog, modes[1], 1)
        a2 = take(prog, modes[2], 2)
        update!(prog, a1 + a2, 3, modes[3])
        prog.cur += 4
    elseif op == 2
        a1 = take(prog, modes[1], 1)
        a2 = take(prog, modes[2], 2)
        update!(prog, a1 * a2, 3, modes[3])
        prog.cur += 4
    elseif op == 3
        if isempty(prog.input)
            prog.state = 1
        else
            update!(prog, popfirst!(prog.input), 1, modes[1])
            prog.cur += 2
        end
    elseif op == 4
        push!(prog.output, take(prog, modes[1], 1))
        prog.cur += 2
    elseif op == 5
        a1 = take(prog, modes[1], 1)
        a2 = take(prog, modes[2], 2)
        prog.cur = a1 != 0 ? a2 + 1 : prog.cur + 3
    elseif op == 6
        a1 = take(prog, modes[1], 1)
        a2 = take(prog, modes[2], 2)
        prog.cur = a1 == 0 ? a2 + 1 : prog.cur + 3
    elseif op == 7
        a1 = take(prog, modes[1], 1)
        a2 = take(prog, modes[2], 2)
        update!(prog, a1 < a2 ? 1 : 0, 3, modes[3])
        prog.cur += 4
    elseif op == 8
        a1 = take(prog, modes[1], 1)
        a2 = take(prog, modes[2], 2)
        update!(prog, a1 == a2 ? 1 : 0, 3, modes[3])
        prog.cur += 4
    elseif op == 9
        prog.relative_base += take(prog, modes[1], 1)
        prog.cur += 2
    end

    prog.state
end

function run(prog::Prog, input::Vector{Int})
    prog.input = input
    prog.state = 0
    run(prog)
end

run(prog::Prog, input::Int) = run(prog, [input])
run(prog::Prog, input::String) = run(prog, Int.(collect(input)))

function run(prog)
    clear!(prog)
    while step(prog) == 0 end

    if prog.ascii
        return join(Char.(prog.output))
    else
        return prog.output
    end
end

clear!(prog) = (prog.output = [])
end
