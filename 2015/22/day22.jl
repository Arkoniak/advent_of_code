using ReTest
using Setfield

struct State
    hp::Int
    mana::Int
    total::Int
    casts::Vector{String}
    shield::Int
    poison::Int
    recharge::Int
    bosshp::Int
    bosshit::Int
end

State(; bosshp, bosshit) = State(50, 500, 0, [], 0, 0, 0, bosshp, bosshit)

bosshit(s) = @set s.hp = s.shield == 0 ? s.hp - s.bosshit : s.hp - max(s.bosshit - 7, 1)
function missile(s)
    @set! s.mana -= 53
    @set! s.total += 53
    @set! s.bosshp -= 4
end
function drain(s)
    @set! s.mana -= 73
    @set! s.total += 73
    @set! s.hp += 2
    @set! s.bosshp -= 2
end
function shield(s)
    @set! s.mana -= 113
    @set! s.total += 113
    @set! s.shield = 6
end
function poison(s)
    @set! s.mana -= 173
    @set! s.total += 173
    @set! s.poison = 6
end
poisonhit(s) = @set s.bosshp -= 3

function recharge(s)
    @set! s.mana -= 229
    @set! s.total += 229
    @set! s.recharge = 5
end
rechargeeffect(s) = @set s.mana += 101

function applyeffects(s)
    s = s.poison > 0 ? poisonhit(s) : s
    s = s.recharge > 0 ? rechargeeffect(s) : s
    @set! s.shield = max(0, s.shield - 1)
    @set! s.poison = max(0, s.poison - 1)
    @set! s.recharge = max(0, s.recharge - 1)
end

function turn(s, spell)
    s = applyeffects(s)
    s = spell(s)
    s.mana < 0 && return s     # spell is not legit
    s.bosshp <= 0 && return s # Win
    s = applyeffects(s)
    s.bosshp <= 0 && return s # Win
    bosshit(s)
end

function part1(s0)
    win = @set s0.total = typemax(Int)
    stack = [s0]
    spells = [missile, drain, shield, poison, recharge]
    while !isempty(stack)
        s = pop!(stack)
        casts = copy(s.casts)
        for spell in spells
            s1 = turn(s, spell)::State
            s1.mana < 0 && continue
            s1.hp <= 0 && continue
            @set! s1.casts = copy(casts)
            push!(s1.casts, string(spell))
            if s1.bosshp <= 0
                win = s1.total < win.total ? s1 : win
            else
                s1.total < win.total && push!(stack, s1)
            end
        end
    end

    return win
end

s = State(bosshp = 13, bosshit = 8)
s = @set s.hp = 10
s = @set s.mana = 250
part1(s)

s = State(bosshp = 14, bosshit = 8)
s = @set s.hp = 10
s = @set s.mana = 250
part1(s)

s = State(bosshp = 55, bosshit = 8)
part1(s).total

########################################
# Part 2
########################################

function turn2(s, spell)
    @set! s.hp -= 1
    s.hp <= 0 && return s
    s = applyeffects(s)
    s = spell(s)
    s.mana < 0 && return s     # spell is not legit
    s.bosshp <= 0 && return s # Win
    s = applyeffects(s)
    s.bosshp <= 0 && return s # Win
    bosshit(s)
end

function part2(s0)
    win = @set s0.total = typemax(Int)
    stack = [s0]
    spells = [missile, drain, shield, poison, recharge]
    while !isempty(stack)
        s = pop!(stack)
        casts = copy(s.casts)
        for spell in spells
            s1 = turn2(s, spell)::State
            s1.mana < 0 && continue
            s1.hp <= 0 && continue
            @set! s1.casts = copy(casts)
            push!(s1.casts, string(spell))
            if s1.bosshp <= 0
                win = s1.total < win.total ? s1 : win
            else
                s1.total < win.total && push!(stack, s1)
            end
        end
    end

    return win
end

s = State(bosshp = 13, bosshit = 8)
s = @set s.hp = 10
s = @set s.mana = 250
part2(s)

s = State(bosshp = 14, bosshit = 8)
s = @set s.hp = 10
s = @set s.mana = 250
part2(s)

s = State(bosshp = 55, bosshit = 8)
part2(s).total

########################################
# Tests
########################################
@code_warntype part1(s)
@code_warntype part2(s)

@testset "Basic effects" begin
    s = State(bosshp = 10, bosshit = 8)
    s1 = @set s.poison = 1
    s1 = @set s1.recharge = 1
    s1 = @set s1.shield = 1
    s1 = applyeffects(s1)
    @test s1.bosshp == 7
    @test s1.poison == 0
    @test s1.shield == 0
    @test s1.recharge == 0
    @test s1.mana == 601

    s2 = applyeffects(s1)
    @test s1.bosshp == 7
    @test s1.poison == 0
    @test s1.shield == 0
    @test s1.recharge == 0
    @test s1.mana == 601
end

runtests()
