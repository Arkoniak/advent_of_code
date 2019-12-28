###########################
# Part 1
function is_passwordlike(d)
    distinct = 1
    total = 1
    d1 = mod(d, 10)
    d = div(d, 10)
    while d > 0
        d2 = mod(d, 10)
        if d2 > d1
            return false
        elseif d2 < d1
            distinct += 1
        end
        total += 1
        d1 = d2
        d = div(d, 10)
    end

    return distinct != total
end

function part1()
    total = 0
    for i in 137683:596253
        if is_passwordlike(i) total += 1 end
    end

    println("Part 1: ", total)
end

part1()

############################
# Part 2

function is_passwordlike2(d)
    distinct = 1
    total = 1
    d1 = mod(d, 10)
    d = div(d, 10)
    match_group = 1
    pair_exist = false
    while d > 0
        d2 = mod(d, 10)
        if d2 > d1
            return false
        elseif d2 < d1
            distinct += 1
            if match_group == 2 pair_exist = true end
            match_group = 1
        else
            match_group += 1
        end
        total += 1
        d1 = d2
        d = div(d, 10)
    end

    return (distinct != total) && (pair_exist || match_group == 2)
end

function part2()
    total = 0
    for i in 137683:596253
        if is_passwordlike2(i) total += 1 end
    end

    println("Part 2: ", total)
end

part2()
