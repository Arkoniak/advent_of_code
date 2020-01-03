#######################
# Part 1
struct FFTIter
    n::Int
    seq::Vector{Int}
    offset::Int
end

Base.:iterate(iter::FFTIter, state = iter.offset) = (iter.seq[mod(div(state, iter.n), length(iter.seq)) + 1], state + 1)

fft(n, seq = [0, 1, 0, -1], offset = 1) = FFTIter(n, seq, offset)

struct PhaseIter
    start::Vector{Int}
    n::Int
end

function Base.:iterate(iter::PhaseIter, state = (iter.start, 0))
    data, cnt = state
    if cnt >= iter.n return nothing end
    
    res = zeros(Int, length(data))
    for j in 1:length(data)
        total = 0
        for (i, x) in enumerate(fft(j))
            total += x*data[i]
            if i == length(data) break end
        end
        res[j] = mod(abs(total), 10)
    end
    
    (res, (res, cnt + 1))
end

phase(start, n) = PhaseIter(start, n)

function phase2(data)
		res = zeros(Int, length(data))
		res[end] = data[end]
		for i in (length(data) - 1):-1:1
				res[i] = mod(res[i+1] + data[i], 10)
		end
		res
end

function part1()
		res = nothing
		for x in phase(parse.(Int, collect(readline("input.txt"))), 100)
				res = x
		end

		println("Part 1: ", join(res[1:8]))
end

part1()

##################################
# Part 2
function part2()
    s = readline("input.txt")
    offset = parse(Int, s[1:7])
    data0 = parse.(Int, collect(s))
    s0 = mod(offset, length(data0)) + 1
    itn = 10000 - div(offset, length(data0)) - 1
    data = data0[s0:end]
    for i in 1:itn
        data = vcat(data, data0)
    end

    for i in 1:100
        data = phase2(data)
    end
    println("Part 2: ", join(data)[1:8])
end

part2()
