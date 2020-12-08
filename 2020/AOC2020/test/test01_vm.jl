module TestVM

using Test
using AOC2020
using AOC2020: NOP, JMP, ACC

@testset "DAY 08" begin
    @testset "utils" begin
    vm = vm"""
    nop +0
    acc +1
    jmp -2
    """

    @test length(vm) == 3
    end

    @testset "NOP" begin
        vm = vm"""
        nop +1
        """
        @test vm.cursor == 1
        @test vm.code[1].op == NOP
        @test vm.regs == 0

        vm = shag(vm)
        @test vm.cursor == 2
        @test vm.regs == 0
    end

    @testset "JMP" begin
        vm = vm"""
        jmp +5
        """
        @test vm.cursor == 1
        @test vm.code[1].op == JMP
        @test vm.regs == 0

        vm = shag(vm)
        @test vm.cursor == 6
        @test vm.regs == 0
    end

    @testset "JMP" begin
        vm = vm"""
        acc +5
        """
        @test vm.cursor == 1
        @test vm.code[1].op == ACC
        @test vm.regs == 0

        vm = shag(vm)
        @test vm.cursor == 2
        @test vm.regs == 5

        vm = vm"""
        acc -5
        """
        @test vm.cursor == 1
        @test vm.code[1].op == ACC
        @test vm.regs == 0

        vm = shag(vm)
        @test vm.cursor == 2
        @test vm.regs == -5
    end
end

end # module
