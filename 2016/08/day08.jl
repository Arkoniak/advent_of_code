const rexs = [r"rect (\d+)x(\d+)",
              r"rotate column x=(\d+) by (\d+)",
              r"rotate row y=(\d+) by (\d+)"]
function process!(led, s)
    for (i, re) in enumerate(rexs)
        m = match(re, s)
        isnothing(m) && continue
        a, b = parse.(Int, m.captures)
        if i == 1
            @view(led[1:a, 1:b]) .= 1
        elseif i == 2
            a += 1
            v = Vector{Int}(undef, size(led, 2))
            for j in axes(led, 2)
                v[mod1(j + b, size(led, 2))] = led[a, j]
            end
            @views led[a, :] .= v
        else
            a += 1
            v = Vector{Int}(undef, size(led, 1))
            for j in axes(led, 1)
                v[mod1(j + b, size(led, 1))] = led[j, a]
            end
            @views led[:, a] .= v
        end
    end
    led
end

led = zeros(Int, 7, 3)
process!(led, "rect 3x2")
process!(led, "rotate column x=1 by 1")
process!(led, "rotate row y=0 by 4")
process!(led, "rotate column x=1 by 1")
sum(led)

led = zeros(Int, 50, 6)
for line in eachline("input.txt") 
    process!(led, line)
end
println("Part 1: ", sum(led))

for i in 1:6
    for s in @view led[:, i]
        s == 0 ? print(" ") : print("X")
    end
    println("")
end

print("\033c")

########################################
# Addendum
########################################
led = zeros(Int, 50, 6)
for line in eachline("input.txt") 
    process!(led, line)
    print("\033c")
    for i in 1:6
        for s in @view led[:, i]
            s == 0 ? print(" ") : print("X")
        end
        println("")
    end
    
    sleep(0.08)
end
