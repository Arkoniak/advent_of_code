using Setfield
using ReTest
import Automa
import Automa.RegExp: @re_str
const re = Automa.RegExp
@enum FIELD BYR IYR EYR HGT HCL ECL PID CID JUNK

include("machines.jl")

valhgt("60in", 1, 4)
valhgt("190cm", 1, 5)
valhgt("190in", 1, 5)
valhgt("190", 1, 3)
valhcl("#0123ah", 1, 7)
@btime valyear("a2015b", 2, 5, 2010, 2023)
valecl("amc", 1, 3)

valecl2(val) = val in ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

@btime valecl("amb", 1, 3)

@btime valecl2("amb")

function vald2(s, p0, p2)
    count(i -> s[i] in '0':'9', p0:p2) == p2 - p0 + 1
end

function vald3(s)
    !isnothing(match(r"^\d{3}$", s))
end

@btime vald2("x123a", 2, 4)
@btime vald3("123")

input = read("input_p2_valid.txt");
res = parse2(input)

using BenchmarkTools
input = read("input.txt");
@btime parse2($input);
# 980.894 μs (7918 allocations: 509.28 KiB)
# 959.256 μs (4990 allocations: 326.28 KiB) # Filtering out !valid
# 924.117 μs (3891 allocations: 251.94 KiB) # Automa PID validation
# 227.751 μs (3309 allocations: 200.41 KiB) # Automa ECL validation
# 183.675 μs (2113 allocations: 125.66 KiB) # Automa YEAR validation
# 144.208 μs (1168 allocations: 60.88 KiB)  # Automa HCL validation
# 106.659 μs (0 allocations: 0 bytes)       # Automa HGT validation

machine2 = let
    field = re"[a-z][a-z][a-z]"
    value = re"[0-9a-z#]+"
    record = field * re":" * value
    line = record * re.rep(re" " * record)
    passport = re.rep(line * re"\n")
    passports = passport * re.rep(re"\n" * passport)

    Automa.compile(passports)
end

@eval function parse3(data::Union{String,Vector{UInt8}})
    $(Automa.generate_init_code(context, machine2))
    
    # p_end and p_eof were set to 0 and -1 in the init code,
    # we need to set them to the end of input, i.e. the length of `data`.
    p_end = p_eof = lastindex(data)
    
    # mark = 0
    # cnt = 0
    # total = 0
    # local fld
    # local s
    # local valid
    $(Automa.generate_exec_code(context, machine2, Dict{Symbol, Expr}()))

    # We need to make sure that we reached the accept state, else the 
    # input did not parse correctly
    iszero(cs) || error("failed to parse on byte ", p, "; State: ", cs)
    return cs
    # return (; answer = cnt, total = total)
end;

input = read("input.txt");
@btime parse3($input)

########################################
# Tests
########################################
@testset "Validate parser" begin
    input = read("input_p2_valid.txt");
    res = parse2(input)
    @test res.answer == 4
    @test res.total == 4

    input = read("input_p2_invalid.txt");
    res = parse2(input)
    @test res.answer == 0
    @test res.total == 4

    input = read("input.txt");
    res = parse2(input)
    @test res.answer == 156
    @test res.total == 255
end

runtests()
