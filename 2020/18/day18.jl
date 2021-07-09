########################################
# Part 1
########################################

function apply(op, x, y)
    if op == 'Z'
        return x
    elseif op == '+'
        return x + y
    else
        return x * y
    end
end

function calculate(s, i = 1)
    state = 0
    res = 0
    inner = 0
    op = '+'

    while true
        i > length(s) && return apply(op, res, inner), i
        c = s[i]
        i += 1
        if c in '0':'9'
            state = 0
            inner = 10*inner + c - '0'
        elseif c == '('
            inner, i = calculate(s, i)
            res = apply(op, res, inner)
            op = 'Z'
            inner = 0
        elseif c == ')'
            return apply(op, res, inner), i
        elseif c == ' '
            state == 1 && continue
            res = apply(op, res, inner)
            op = 'Z'
            inner = 0
        elseif c == '*'
            state = 1
            op = '*'
        elseif c == '+'
            state = 1
            op = '+'
        end
    end
end

mapreduce(x -> calculate(x)[1], +, eachline("input.txt")) |> x -> println("Part 1: $x")

########################################
# Part 2
########################################
mutable struct Node
    op::Char
    val::Int
    parent::Union{Node, Nothing}
    left::Union{Node, Nothing}
    right::Union{Node, Nothing}
end
Node(op::Char) = Node(op, 0, nothing, nothing, nothing)
Node(val::Int) = Node('Z', val, nothing, nothing, nothing)
isleaf(n::Node) = n.op == 'Z'
isopen(n::Node) = n.left === nothing
isvalue(n::Node) = isleaf(n) || n.val == 1
root(n::Node) = n.parent === nothing ? n : root(n.parent)

function build_tree(s, i = 1)
    node = Node(0)
    active = node
    while i <= length(s)
        c = s[i]
        i += 1
        if c == '('
            node, i = build_tree(s, i)
        elseif c in '0':'9'
            node.val = node.val * 10 + c - '0'
        elseif c in ('+', '*')
            node.op = c
        else # c in (' ', ')')

            if isvalue(node)
                if isopen(active)
                    active = node
                else
                    active.right = node
                    node.parent = active
                end
            else
                if isvalue(active)
                    active.parent = node
                    node.left = active
                elseif node.op == '+' && active.op == '*'
                    node.left = active.right
                    active.right.parent = node
                    node.parent = active
                    active.right = node
                else
                    node.left = active
                    node.parent = active.parent
                    if node.parent !== nothing
                        node.parent.right = node
                    end
                    active.parent = node
                end
                active = node
            end
            if c == ')' 
                rt = root(active)
                rt.val = 1
                return rt, i
            end
            node = Node(0)
        end
    end
    if isopen(active)
        active = node
    else
        active.right = node
    end
    rt = root(active)
    rt.val = 1
    return rt, length(s) + 1
end

function calc(n::Node)
    isleaf(n) && return n.val
    l = calc(n.left)
    r = calc(n.right)
    n.op == '+' ? l + r : l * r
end

function calc(s::AbstractString)
    n = build_tree(s)[1]
    calc(n)
end

mapreduce(x -> calc(x), +, eachline("input.txt")) |> x -> println("Part 2: $x")

########################################
# Misc
########################################
# function print_tree(n::Node)
#     if isleaf(n) 
#         print(n.val)
#         return
#     end
#     print(n.op, "(")
#     print_tree(n.left)
#     print(",")
#     print_tree(n.right)
#     print(")")
# end
# function print_tree(::Nothing)
#     print("_")
# end

# x = build_tree("2 + 3")[1]
# calc(x)

# x.op
# x.left.val
# x.right.val
# x.right.left

# calculate("2 * 3")
# calculate("2 * 3 + (4 * 5)")

# x = build_tree("2 * 3 + (4 * 5)")[1]
# calc(x)

# calc("2 * 3 + (4 * 5)")

# x.op
# x.left.val
# x.right
# x = build_tree("1 + 2 * 3 + 4 * 5 + 6")[1]
# calc(x)
# x = build_tree("1 + 2 * 3 + 4 * 5")[1]
# x = build_tree("1 + 2 * 3")[1]
# print_tree(x)
# x.right.parent.right
# print_tree(x.right.left)


########################################
# Tests
########################################
module Tests
using ReTest

using Main: calculate, calc
# using Main: part1

@testset "Misc" begin
end

@testset "Part 1" begin
    @test calculate("2 * 3 + (4 * 5)")[1] == 26
    @test calculate("1 + (2 * 3) + (4 * (5 + 6))")[1] == 51
    @test calculate("5 + (8 * 3 + 9 + 3 * 4 * 3)")[1] == 437
    @test calculate("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")[1] == 12240
    @test calculate("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")[1] == 13632
end

@testset "Part 2" begin
    @test calc("1 + 2 * 3 + 4 * 5 + 6") == 231
    @test calc("1 + (2 * 3) + (4 * (5 + 6))") == 51
    @test calc("2 * 3 + (4 * 5)") == 46
    @test calc("5 + (8 * 3 + 9 + 3 * 4 * 3)") == 1445
    @test calc("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))") == 669060
    @test calc("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2") == 23340
end

end # module

Tests.runtests(Tests)
