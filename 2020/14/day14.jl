########################################
# Part 1
########################################
using Underscores

bitmask(mask, s) = @_ map(c -> c[1] == 'X' ? c[2] : c[1], zip(mask, s)) |> join |> parse(Int, __; base = 2)

function part1(file)
    d = Dict{Int, Int}()
    re = [r"mask = (.{36}+)", r"mem\[(\d+)\] = (\d+)"]
    mask = ""
    for line in eachline(file)
        for (i, rexp) in enumerate(re)
            m = match(rexp, line)
            isnothing(m) && continue
            if i == 1
                mask = m[1]
            else
                n = parse(Int, m[1])
                k = bitmask(mask, last(bitstring(parse(Int, m[2])), 36))
                d[n] = k
            end
        end
    end
    sum(values(d))
end

println("Part 1: ", part1("input.txt"))

########################################
# Part 2
########################################
function bitaddr(mask, addr)
    masked = map(c -> c[1] == '0' ? (c[2] - '0') : c[1] == '1' ? 1 : -1, zip(mask, addr))
    L = length(masked)
    cnt = 2^(count(==(-1), masked))
    res = Vector{Int}(undef, cnt)
    for i in 1:cnt
        m = i - 1
        x = 0
        for j in length(masked):-1:1
            if masked[j] != -1
                x |= (masked[j] << (L - j))
            else
                x |= ((m & 0x01) << (L - j))
                m = m >> 1
            end
        end
        res[i] = x
    end
    res
end

function part2(file)
    d = Dict{Int, Int}()
    re = [r"mask = (.{36}+)", r"mem\[(\d+)\] = (\d+)"]
    mask = ""
    for line in eachline(file)
        for (i, rexp) in enumerate(re)
            m = match(rexp, line)
            isnothing(m) && continue
            if i == 1
                mask = m[1]
            else
                n, val = parse.(Int, m.captures)
                addrs = bitaddr(mask, last(bitstring(n), 36))
                for a in addrs
                    d[a] = val
                end
            end
        end
    end
    sum(values(d))
end

println("Part 2: ", part2("input.txt"))

########################################
# Misc
########################################
for (a, b) in zip("asd", "zxc")
    println(a, ": ", b)
end

function estimate(file)
    res = 0
    for line in eachline(file)
        m = match(r"mask = (.{36}+)", line)
        isnothing(m) && continue
        res += 2^count(==('X'), m[1])
    end
    res
end

estimate("input.txt")

bitaddr("000000000000000000000000000000X1001X", last(bitstring(42), 36))

using BenchmarkTools
@benchmark part2("input.txt")

########################################
# Tests
########################################
module Tests
using ReTest
using Main: bitmask, part1, bitaddr, part2

@testset "All" begin
@testset "Misc" begin
    @test bitmask("0XX", "111") == 3
    @test bitmask("XXX", "111") == 7
    @test bitmask("XX1", "000") == 1

    @test bitaddr("000000000000000000000000000000X1001X", last(bitstring(42), 36)) == [26, 27, 58, 59]
    @test bitaddr("00000000000000000000000000000000X0XX", last(bitstring(26), 36)) == [16, 17, 18, 19, 24, 25, 26, 27]
end

@testset "Part 1" begin
    @test part1("input_test.txt") == 165
end

@testset "Part 2" begin
    @test part2("input_test2.txt") == 208
end

end
end # module

Tests.runtests(Tests)
