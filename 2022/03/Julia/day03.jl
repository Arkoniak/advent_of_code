priority(c) = c in 'a':'z' ? (c - 'a' + 1) : (c - 'A' + 27)

function process(row)
    l = length(row) ÷ 2
    priority(only(intersect(Set(collect(row[1:l])), Set(collect(row[l+1:2*l])))))
end

# Part 1
sum(process, eachline("input.txt"))

# Part 2
function part2(filename)
    buf = Vector{Set{Char}}()
    cnt = 0
    s = 0
    for row in eachline(filename)
        push!(buf, Set(row))
        cnt += 1
        if cnt == 3
            s += priority(only(intersect(buf...)))
            buf = Vector{Set{Char}}()
            cnt = 0
        end
    end
    s
end

part2("input.txt")
