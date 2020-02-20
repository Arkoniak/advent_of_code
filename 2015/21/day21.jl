using Combinatorics

function get_boss(file = "input.txt")
    readlines(joinpath(@__DIR__, file)) |> x -> split.(x, ":") |>
        x -> map(y -> strip(y[2]), x) |> x -> parse.(Int, x) |>
        x -> (hit=x[1], dmg=x[2], arm=x[3])
end

@inline function items()
(weapons=(
    (cost=8,  dmg=4, arm=0),
    (cost=10, dmg=5, arm=0),
    (cost=25, dmg=6, arm=0),
    (cost=40, dmg=7, arm=0),
    (cost=74, dmg=8, arm=0)
),
armor=(
    (cost=13,  dmg=0, arm=1),
    (cost=31,  dmg=0, arm=2),
    (cost=53,  dmg=0, arm=3),
    (cost=75,  dmg=0, arm=4),
    (cost=102, dmg=0, arm=5)
),
rings=(
    (cost=25,  dmg=1, arm=0),
    (cost=50,  dmg=2, arm=0),
    (cost=100, dmg=3, arm=0),
    (cost=20,  dmg=0, arm=1),
    (cost=40,  dmg=0, arm=2),
    (cost=80,  dmg=0, arm=3)
))
end

function get_player(i, j, karr)
    armor = 0
    damage = 0
    cost = 0
    cost += items().weapons[i].cost
    damage += items().weapons[i].dmg

    if j != 0
        cost += items().armor[j].cost
        armor += items().armor[j].arm
    end
    for r in items().rings[karr]
        cost += r.cost
        armor += r.arm
        damage += r.dmg
    end

    (hit = 100, dmg = damage, arm = armor), cost
end

function hit(attacker, defender)
    blow = attacker.dmg - defender.arm
    blow = blow < 1 ? 1 : blow
    (hit=defender.hit - blow, dmg=defender.dmg, arm=defender.arm)
end

boss
player = get_player(1, 0, [4, 5])[1]
hit(boss, player)


function fight(player, boss)
    while true
        boss = hit(player, boss)
        boss.hit <= 0 && return true

        player = hit(boss, player)
        player.hit <= 0 && return false
    end
end

function part1()
    boss = get_boss()
    rings = union(collect(combinations(1:length(items().rings), 1)),
        collect(combinations(1:length(items().rings), 2)))
    push!(rings, [])

    res = []

    for i in 1:length(items().weapons), j in 0:length(items().armor), karr in rings
        player, cost = get_player(i, j, karr)
        if fight(player, boss)
            push!(res, (cost = cost, args = (i, j, karr)))
        end
    end

    println("Part 1: $(minimum(first.(res)))")
end

part1()

##############################

function part2()
    boss = get_boss()
    rings = union(collect(combinations(1:length(items().rings), 1)),
        collect(combinations(1:length(items().rings), 2)))
    push!(rings, [])

    res = []

    for i in 1:length(items().weapons), j in 0:length(items().armor), karr in rings
        player, cost = get_player(i, j, karr)
        if !fight(player, boss)
            push!(res, (cost = cost, args = (i, j, karr)))
        end
    end

    println("Part 1: $(maximum(first.(res)))")
end

part2()
