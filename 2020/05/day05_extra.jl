@_  |>
    replace()

map("FBFBBFFRLR") do c
    c ∈ ('R', 'B') ? '1' : '0'
end

seatid(s) = parse(Int, map(c -> c ∈ ('R', 'B') ? '1' : '0', s), base = 2)
seatid("FBFBBFFRLR")

d = Dict{String, Int}(
    "FBFBBFFRLR" => 126
)

using BenchmarkTools

@btime $d[$"FBFBBFFRLR"]

function xxx(d, s)
    d[s]
end

@btime xxx($d, $"FBFBBFFRLR")
seatid(x) = mapreduce(x -> x & 0x6 == 2, (x,y) -> (x<<1) | y, transcode(UInt8, x), init = 0)

@btime seatid($Ref("FBFBBFFRLR")[])

function ticket(x)
    x = bitstring(x)[end-9:end]
    map(c -> c == '0' ? 'F' : 'B', x[1:7]) * map(c -> c == '0' ? 'L' : 'R', x[8:10])
end

ticket(357)
