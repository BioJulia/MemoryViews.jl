using Test
using MemViews

@testset "Tests" begin
    @test ImmutableMemView(view(codeunits(view("abcdefgh", 2:8)), 1:7)) == 0x62:0x68
end
