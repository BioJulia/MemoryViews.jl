using Test
using MemViews

MUT_BACKINGS = Any[
    # Arrays
    UInt8[1,2],
    [4, 7],
    Float32[2,9],
    Vector{AbstractDict{Int, String}}(undef, 3),

    # Memory
    Memory{String}(undef, 3),

    # Views
    view([1,2,3,4,5,6], 1:5),
    view(Memory{UInt8}(), 1:0),
]

@testset "Mutability" begin
    for mut in MUT_BACKINGS
        @test MemView(mut) isa MutableMemView
    end

    for immut in Any[
        "abc",
        codeunits("abcd"),
        view("adskd", 2:4),
        codeunits(view("dsaas", 1:3)),
    ]
        @test MemView(immut) isa ImmutableMemView
    end

    for nonmem in [
        nothing,
        missing,
        5,
        (1, 3, 5),
        view([1,2,3,4], 1:2:3),
    ]
        @test_throws Exception MemView(nonmem)
    end
end

@testset "Immutable views are immutable" begin
    mem = MemView("abc")
    @test mem isa ImmutableMemView{UInt8}
    mutmem = MemView(collect(codeunits("def")))

    @test_throws Exception mem[1] = 2
    @test_throws Exception reverse!(mem)
    @test_throws Exception copy!(mem, mutmem)
    @test_throws Exception unsafe_copyto!(mem, 1, mutmem, 1, 2)
end

# Span of views
@testset "Span of views" begin
    mem = MemView("abc")
    @test length(mem) == 3
    @test first(mem) == UInt8('a')
    @test last(mem) == UInt8('c')

    memory = Memory{Float32}(undef, 6)
    mem = MemView(memory)
    @test all(i == j for (i, j) in zip(mem, memory))
    @test length(mem) == length(memory)
    @test mem == memory

    v = view(view(rand(UInt16, 19), 2:11), 3:9)
    mem = MemView(v)
    @test mem == v

    s = SubString(Test.GenericString("dslkjad"), 2:5)
    # This is not implemented
    @test_throws Exception MemView(s)
end

memlen(x) = length(MemView(x))
@testset "Zero allocation" begin
    for v in MUT_BACKINGS
        memlen(v) # compile
        @test @allocated(memlen(v)) == 0
    end
end

@testset "Indexing" begin
    @testset "Scalar indexing" begin
        s = "abcdefghijklmn"
        cu = codeunits(s)
        mem = MemView(s)

        @test mem[3] == cu[3]
        for i in [
            -100, -4, -1, 0, length(cu) + 1, length(cu) + 100,
        ]
            @test_throws BoundsError mem[i]
        end
    end

    @testset "AbstractUnitRange indexing" begin
        s = "abcdefghijklmn"
        cu = codeunits(s)
        mem = MemView(s)

        for i in Any[
            2:6,
            Int32(4):Int32(9),
            0x05:0x0a,
            Base.OneTo(11),
            :,
            5:4,
            100:99,
            -500:-501,
        ]
            @test mem[i] == cu[i]
        end

        mem = MemView([9, 4, 2, 1, 8])
        mem2 = mem[3:end]
        @test typeof(mem) == typeof(mem2)
        mem[3] = 10
        # No copying
        @test mem2 == [10, 1, 8]
    end


    @testset "setindex!" begin
        v = Int16[32, 924, 231, 0, -145]
        mem = MemView(v)
        mem[1] = -500
        @test v == mem == [-500, 924, 231, 0, -145]
        mem[end] = 2
        @test v == mem == [-500, 924, 231, 0, 2]
        mem[2:end-2] = 5:6
        @test v == mem == [-500, 5, 6, 0, 2]
        mem .= 99
        @test v == mem == [99, 99, 99, 99, 99]
        mem[:] .= 0
        @test mem == v == fill(0, 5)
    end
end

@testset "Iteration" begin
    mem = MemView(UInt16[])
    @test iterate(mem) === nothing

    mem = MemView("xp")
    (a, s) = iterate(mem)
    (b, s) = iterate(mem, s)
    @test iterate(mem, s) === nothing
    @test (a, b) == (UInt8('x'), UInt8('p'))

    for mut in MUT_BACKINGS
        if all(i -> isassigned(mut, i), eachindex(mut))
            @test collect(mut) == collect(MemView(mut))
        end
    end
end

@testset "Pointers" begin
    memory = Memory{UInt16}(undef, 10)
    mem = MemView(memory)[3:7]
    @test pointer(mem) == pointer(memory) + 4
    @test pointer(mem, 3) == pointer(memory) + 8

    v = view(rand(UInt32, 100), 19:55)
    mem = MemView(v)
    @test pointer(mem) == pointer(v)
    @test pointer(mem, 4) == pointer(v, 4)

    v = ["kls", "dsddaefe", "", "adsad"]
    mem = MemView(v)[2:end]
    @test pointer(v) + 8 == pointer(mem)
    @test pointer(v, 2) == pointer(mem)
    @test pointer(v, 3) == pointer(mem, 2)
end

@testset "Misc functions" begin
    @testset "Copying" begin
        # Immutable
        mem = MemView("abcdef")
        @test copy(mem) == mem

        # Mutable
        v = [1,2,3,4,5]
        mem = MemView(v)[2:4]
        mem2 = copy(mem)
        mem[1] = 9
        mem2[2] = 10
        @test v == [1,9,3,4,5]
        @test mem == [9,3,4]
        @test mem2 == [2,10,4]
        # Only makes a copy of the needed data
        @test length(mem2.ref.mem) == length(mem2)
    end

    @testset "Parentindices" begin
        mem = MemView(view(codeunits("lkdjfldfe"), 3:8))[2:6]
        @test parentindices(mem) == 4:8

        mem = MemView(UInt32[2,5,2,1,6,8])[4:end]
        @test parentindices(mem) == 4:6

        mem = MemView(view(Vector{String}(undef, 10), 5:7))
        @test parentindices(mem) == 5:7
    end

end