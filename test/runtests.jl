using Test
using MemViews

MUT_BACKINGS = Any[
    # Arrays
    UInt8[1, 2],
    [4, 7],
    Float32[2, 9],
    Vector{AbstractDict{Int, String}}(undef, 3),

    # Memory
    Memory{String}(undef, 3),

    # Views
    view([1, 2, 3, 4, 5, 6], 1:5),
    view(Memory{UInt8}(), 1:0),
]

@testset "Mutability" begin
    for mut in MUT_BACKINGS
        @test MemView(mut) isa MutableMemView
    end

    for immut in
        Any["abc", codeunits("abcd"), view("adskd", 2:4), codeunits(view("dsaas", 1:3))]
        @test MemView(immut) isa ImmutableMemView
    end

    for nonmem in [nothing, missing, 5, (1, 3, 5), view([1, 2, 3, 4], 1:2:3)]
        @test_throws Exception MemView(nonmem)
    end

    @testset "Unsafe mutability" begin
        v = [1.0, 2.0, 3.0]
        m = ImmutableMemView(v)
        m2 = MutableMemView(MemViews.unsafe, m)
        m2[2] = 5.0
        @test v == [1.0, 5.0, 3.0]
    end
end

@testset "More construction" begin
    mem = MemView([1, 2, 3])
    @test MemView(mem) === mem

    mem = MemView(view("abc", 2:3))
    @test mem isa ImmutableMemView{UInt8}
    @test mem == [0x62, 0x63]
end

@testset "Unsafe casting" begin
    v = UInt32[htol(0x04030201), htol(0x08070605)]
    mem = as_bytes(MemViews.unsafe, MemView(v))
    @test mem == 0x01:08
    mem[2] = 0x09
    @test mem == [1, 9, 3, 4, 5, 6, 7, 8]

    mem = MemView("abc")
    @test as_bytes(MemViews.unsafe, mem) === mem
end

@testset "Immutable views are immutable" begin
    mem = MemView("abc")
    @test mem isa ImmutableMemView{UInt8}
    @test ImmutableMemView(mem) === mem
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
        for i in [-100, -4, -1, 0, length(cu) + 1, length(cu) + 100]
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

    @testset "Views of memviews" begin
        mem = MemView(rand(3, 4))
        mem2 = view(mem, 4:7)
        @test mem2 === mem[4:7]
        mem2 .= [1.0, 2.0, 3.0, 4.0]
        @test mem[4:7] == [1.0, 2.0, 3.0, 4.0]
    end

    @testset "setindex!" begin
        v = Int16[32, 924, 231, 0, -145]
        mem = MemView(v)
        mem[1] = -500
        @test v == mem == [-500, 924, 231, 0, -145]
        mem[end] = 2
        @test v == mem == [-500, 924, 231, 0, 2]
        mem[2:(end - 2)] = 5:6
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
        v = [1, 2, 3, 4, 5]
        mem = MemView(v)[2:4]
        mem2 = copy(mem)
        mem[1] = 9
        mem2[2] = 10
        @test v == [1, 9, 3, 4, 5]
        @test mem == [9, 3, 4]
        @test mem2 == [2, 10, 4]
        # Only makes a copy of the needed data
        @test length(mem2.ref.mem) == length(mem2)
    end

    @testset "Parentindices" begin
        mem = MemView(view(codeunits("lkdjfldfe"), 3:8))[2:6]
        @test parentindices(mem) == 4:8

        mem = MemView(UInt32[2, 5, 2, 1, 6, 8])[4:end]
        @test parentindices(mem) == 4:6

        mem = MemView(view(Vector{String}(undef, 10), 5:7))
        @test parentindices(mem) == 5:7
    end

    @testset "Similar and empty" begin
        mem = MemView(Int16[6, 4, 3])
        @test typeof(empty(mem)) == typeof(mem)
        @test isempty(empty(mem))

        mem2 = empty(mem, Int8)
        @test isempty(mem2)
        @test typeof(mem2) == MutableMemView{Int8}

        mem = MemView("abc")
        mem2 = similar(mem)
        @test length(mem2) == length(mem)
        @test typeof(mem2) == typeof(mem)

        mem = MemView(String["", "", ""])
        mem2 = similar(mem, Int, 4)
        @test length(mem2) == 4
        @test eltype(mem2) == Int

        mem = empty(ImmutableMemView{Tuple{Int, UInt8}})
        @test isempty(mem)
        @test mem isa ImmutableMemView{Tuple{Int, UInt8}}
        mem = empty(MutableMemView{Float16})
        @test isempty(mem)
        @test mem isa MutableMemView{Float16}
    end

    @testset "Sizeof" begin
        @test sizeof(MemView("abc")) == 3
        @test sizeof(MemView([1, 2, 3])) == 3 * sizeof(Int)
        @test sizeof(MemView(String["", "", "", ""])) == 4 * sizeof(Int)
    end

    @testset "Copyto" begin
        # Copy!
        v1 = [5, 2, 1, 9, 8]
        v2 = [0, 2, 6, 3, 9]
        mem1 = MemView(v1)
        mem2 = MemView(v2)
        copy!(mem1, mem2)
        @test v1 == v2
        @test mem1 == mem2

        @test_throws BoundsError copy!(MemView([1]), MemView([1, 2]))
        @test_throws BoundsError copy!(MemView([1, 2]), MemView([1]))

        # Copyto!
        v1 = [4, 2, 6, 7, 9]
        v2 = [1, 5, 2, 3]
        copyto!(MemView(v1), MemView(v2))
        @test v1 == [1, 5, 2, 3, 9]
        @test_throws BoundsError copyto!(MemView(v2), MemView(v1))

        # unsafe_copyto!
        v1 = [3, 6, 2, 1]
        v2 = [0, 9, 5]
        unsafe_copyto!(MemView(v1), MemView(v2))
        @test v1 == [0, 9, 5, 1]
        v2 = rand(Int, 4)
        unsafe_copyto!(MemView(v1), MemView(v2))
        @test v2 == v1
    end

    @testset "Find" begin
        mem = MemView([4, 3, 2])
        @test findfirst(==(2), mem) == 3

        mem = MemView(Int8[6, 2, 7, 0, 2])
        @test findfirst(iszero, mem) == 4
        @test findfirst(==(Int8(0)), mem) == 4

        mem = MemView(UInt8[1, 4, 2, 5, 6])
        @test findnext(==(0x04), mem, 1) == 2
        @test findnext(==(0x04), mem, 3) === nothing
    end
end

@testset "MemKind" begin
    @test MemKind(Vector{Int16}) == IsMemory(MutableMemView{Int16})
    @test MemKind(typeof(codeunits(view("abc", 2:3)))) == IsMemory(ImmutableMemView{UInt8})
    @test MemKind(typeof(view(Memory{String}(undef, 3), Base.OneTo(2)))) ==
          IsMemory(MutableMemView{String})
    @test MemKind(Matrix{Nothing}) == IsMemory(MutableMemView{Nothing})
    @test MemKind(Memory{Int32}) == IsMemory(MutableMemView{Int32})
    @test MemKind(typeof(view([1], 1:1))) == IsMemory(MutableMemView{Int})

    @test inner(IsMemory(MutableMemView{Int32})) == MutableMemView{Int32}
    @test inner(IsMemory(ImmutableMemView{Tuple{String, Int}})) ==
          ImmutableMemView{Tuple{String, Int}}

    @test MemKind(SubString{String}) == NotMemory()
    @test MemKind(String) == NotMemory()
    @test MemKind(Int) == NotMemory()
    @test MemKind(Nothing) == NotMemory()
    @test MemKind(Union{}) == NotMemory()
    @test_throws Exception inner(NotMemory())
end
