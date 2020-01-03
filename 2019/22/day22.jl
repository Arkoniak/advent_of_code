########################
# Part 1

function generate_permutation(v::Vector{T}, s::S) where {T, S <: AbstractString}
    if match(r"deal into new stack", s) != nothing return reverse(v) end
    m = match(r"cut (-?[0-9]+)", s)
    if m != nothing
        x = mod(parse(Int, m[1]), length(v)) + 1
        return vcat(v[x:end], v[1:(x - 1)])
    end
    m = match(r"deal with increment ([0-9]+)", s)
    if m != nothing
        inc = parse(Int, m[1])
        v1 = copy(v)
        for i in 1:length(v)
            v1[mod((i - 1)*inc, length(v)) + 1] = v[i]
        end
        return v1
    end
end

function generate_permutations(v::Vector{T}, s::Vector{S}) where {T, S <: AbstractString}
    v1 = v
    for sel in s
        v1 = generate_permutation(v1, sel)
    end
    v1
end

function part1()
    s = readlines("input.txt")
    v = generate_permutations(collect(0:10006), s)
    println("Part 1: ", argmax(v .== 2019) - 1)
end

part1()

##########################
# Part 2

using Mods

struct Transformation
    a::Int128
    b::Int128
    L::Int128
end

function apply(t::Transformation, s::S) where {S <: AbstractString}
    if match(r"deal into new stack", s) != nothing return Transformation(mod(-t.a, t.L), mod(t.L - t.b - 1, t.L), t.L) end
    
    m = match(r"cut (-?[0-9]+)", s)
    if m != nothing
        x = parse(Int128, m[1])
        return Transformation(t.a, mod(t.b - x, t.L), t.L)
    end
    
    m = match(r"deal with increment ([0-9]+)", s)
    if m != nothing
        inc = parse(Int128, m[1])
        return Transformation(mod(t.a*inc, t.L), mod(t.b*inc, t.L), t.L)
    end
end

function inverse(t::Transformation)
    m = Mod(t.a, t.L)^-1
    Transformation(m.val, mod(-t.b*m.val, t.L), t.L)
end

function pow(t::Transformation, k::T) where T <: Number
    a = Mod(t.a, t.L)
    b = Mod(t.b, t.L)
    one = Mod(1, t.L)
    a1 = a^k
    b1 = (a - one)^(-1) * (a1 - one)*b
    
    Transformation(a1.val, b1.val, t.L)
end

function Transformation(L::T, ss::Vector{S}) where {T <: Number, S <: AbstractString}
    t0 = Transformation(Int128(1), Int128(0), Int128(L))
    for s in ss
        t0 = apply(t0, s)
    end
    t0
end


Base.:getindex(t::Transformation, i) = mod(t.a*i + t.b, t.L)

function part2()
    t = Transformation(119315717514047, readlines("input.txt"))
    t2 = pow(t, 101741582076661)
    tinv = inverse(t2)
    println("Part 2: ", tinv[2020])
end

part2()
