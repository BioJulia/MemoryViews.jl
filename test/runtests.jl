using Test
using FixedSizeArrays
using MemoryViews
using Aqua
using StringViews: StringView

using MemoryViews: DelimitedIterator, Mutable, Immutable

MemoryViews.MemoryView(s::GenericString) = MemoryView(s.string)

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
        @test MemoryView(mut) isa MutableMemoryView
    end

    for immut in
        Any["abc", codeunits("abcd"), view("adskd", 2:4), codeunits(view("dsaas", 1:3))]
        @test MemoryView(immut) isa ImmutableMemoryView
    end

    for nonmem in [nothing, missing, 5, (1, 3, 5), view([1, 2, 3, 4], 1:2:3)]
        @test_throws Exception MemoryView(nonmem)
    end

    @testset "Unsafe mutability" begin
        v = [1.0, 2.0, 3.0]
        m = ImmutableMemoryView(v)
        m2 = MutableMemoryView(MemoryViews.unsafe, m)
        m2[2] = 5.0
        @test v == [1.0, 5.0, 3.0]
    end
end

@testset "More construction" begin
    mem = MemoryView([1, 2, 3])
    @test MemoryView(mem) === mem

    mem = MemoryView(view("abc", 2:3))
    @test mem isa ImmutableMemoryView{UInt8}
    @test mem == [0x62, 0x63]

    for s in [view("", 1:0), view("abc", 4:3), view("abc", 10:2)]
        m = MemoryView(s)
        @test m isa ImmutableMemoryView{UInt8}
        @test isempty(m)
    end

    s = Test.GenericString("abγδf")
    @test codeunits(s) == MemoryView(s)
    @test codeunits(s[2:(end - 2)]) == MemoryView(s)[2:(end - 1)]

    x = [1, 2, 3]
    @test MemoryView{Int}(x) isa MutableMemoryView{Int}
    @test ImmutableMemoryView{Int}(x) isa ImmutableMemoryView{Int}
    @test_throws TypeError MemoryView{UInt32}(x)
    @test_throws TypeError ImmutableMemoryView{UInt32}(x)
end

@testset "Immutable views are immutable" begin
    mem = MemoryView("abc")
    @test mem isa ImmutableMemoryView{UInt8}
    @test ImmutableMemoryView(mem) === mem
    mutmem = MemoryView(collect(codeunits("def")))

    @test_throws Exception mem[1] = 2
    @test_throws Exception reverse!(mem)
    @test_throws Exception copy!(mem, mutmem)
    @test_throws Exception unsafe_copyto!(mem, 1, mutmem, 1, 2)
end

@testset "Mightalias" begin
    v = [1, 2, 3, 4, 5]
    m1 = MemoryView(v)[2:3]
    @test Base.mightalias(m1, MemoryView(v)[1:2])
    @test Base.mightalias(m1, MemoryView(v)[3:4])
    @test Base.mightalias(m1, MemoryView(v)[1:4])
    @test !Base.mightalias(m1, MemoryView(v)[1:1])
    @test !Base.mightalias(m1, MemoryView(v)[4:5])

    v = string.(collect("abcdefgh"))
    v1 = view(v, 2:6)
    v2 = view(v, 6:7)
    @test Base.mightalias(MemoryView(v), MemoryView(v1))
    @test Base.mightalias(MemoryView(v), MemoryView(v2))
    @test Base.mightalias(MemoryView(v), MemoryView(view(v, 7:8)))
    @test !Base.mightalias(MemoryView(v1), MemoryView(view(v, 7:8)))

    v1 = [1, 2, 3]
    v2 = UInt[1, 2, 3]
    @test !Base.mightalias(v1, v2)
    @test Base.mightalias(MemoryView(v1)[2:2], v1)
    @test Base.mightalias(view(v1, 2:3), MemoryView(v1))
end

# Span of views
@testset "Span of views" begin
    mem = MemoryView("abc")
    @test length(mem) == 3
    @test first(mem) == UInt8('a')
    @test last(mem) == UInt8('c')

    memory = Float32[0.1, -119.2, 150.3, Inf, -Inf]
    mem = MemoryView(memory)
    @test all(i === j for (i, j) in zip(mem, memory))
    @test length(mem) == length(memory)
    @test mem == memory

    v = view(view(rand(UInt16, 19), 2:11), 3:9)
    mem = MemoryView(v)
    @test mem == v
end

struct CharString <: AbstractString
    x::Vector{Char}
end
Base.codeunit(s::CharString, i::Int) = reinterpret(UInt32, s.x[i])
Base.codeunit(s::CharString) = UInt32
Base.ncodeunits(s::CharString) = length(s.x)
Base.thisind(s::CharString, i) = i
Base.isvalid(s::CharString, i::Integer) = in(i, 1:ncodeunits(s))
CharString(s::String) = CharString(collect(s))

function Base.nextind(s::CharString, i::Int, n::Int)
    return if iszero(n)
        if !(iszero(i) || in(i, eachindex(s.x)))
            throw(BoundsError(s, i))
        else
            i
        end
    else
        in(i, 0:lastindex(s.x)) ? i + n : throw(BoundsError(s, i))
    end
end

function Base.iterate(s::CharString, i::Int = 1)
    return i > ncodeunits(s) ? nothing : (s.x[i], i + 1)
end

MemoryViews.MemoryView(s::CharString) = MemoryView(s.x)

@testset "Substrings" begin
    for (s, i) in [
            ("æøåac", 1:5),
            ("æøåac", 2:4),
            ("æøåac", 4:3),
            ("æøåac", 1:1),
            ("", 1:0),
            ("儒家孟子", 2:4),
        ]
        ss = SubString(CharString(s), i)
        v1 = MemoryView(ss)
        @test v1 isa ImmutableMemoryView{Char}
        v2 = ImmutableMemoryView(collect(s)[i])
        @test v1 == v2
    end

    for (s, i) in [
            ("æøåac", 1:8),
            ("æøåac", 3:7),
            ("æøåac", 5:4),
            ("æøåac", 1:1),
            ("", 1:0),
            ("儒家孟子", 4:10),
        ]
        ss = SubString(s, i)
        @test MemoryView(ss) isa ImmutableMemoryView{UInt8}
        @test MemoryView(ss) == MemoryView(String(ss))
    end
end

memlen(x) = length(MemoryView(x))
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
        mem = MemoryView(s)

        @test mem[3] == cu[3]
        for i in [-100, -4, -1, 0, length(cu) + 1, length(cu) + 100]
            @test_throws BoundsError mem[i]
        end
    end

    @testset "AbstractUnitRange indexing" begin
        s = "abcdefghijklmn"
        cu = codeunits(s)
        mem = MemoryView(s)

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

        mem = MemoryView([9, 4, 2, 1, 8])
        mem2 = mem[3:end]
        @test typeof(mem) == typeof(mem2)
        mem[3] = 10
        # No copying
        @test mem2 == [10, 1, 8]

        # Base.OneTo
        mem = MemoryView(b"abcdefg")
        v = mem[Base.OneTo(3)]
        @test v == b"abc"
        v = mem[Base.OneTo(0)]
        @test isempty(v)
        v = mem[Base.OneTo(7)]
        @test v === mem
        mem = MemoryView("")
        v = mem[Base.OneTo(0)]
        @test mem === v

        @test_throws BoundsError mem[Base.OneTo(8)]
        @test_throws BoundsError mem[Base.OneTo(typemax(Int))]
    end

    @testset "Views of memviews" begin
        mem = MemoryView(rand(3, 4))
        mem2 = view(mem, 4:7)
        @test mem2 === mem[4:7]
        mem2 .= [1.0, 2.0, 3.0, 4.0]
        @test mem[4:7] == [1.0, 2.0, 3.0, 4.0]
    end

    @testset "setindex!" begin
        v = Int16[32, 924, 231, 0, -145]
        mem = MemoryView(v)
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
    mem = MemoryView(UInt16[])
    @test iterate(mem) === nothing

    mem = MemoryView("xp")
    (a, s) = iterate(mem)
    (b, s) = iterate(mem, s)
    @test iterate(mem, s) === nothing
    @test (a, b) == (UInt8('x'), UInt8('p'))

    for mut in MUT_BACKINGS
        if all(i -> isassigned(mut, i), eachindex(mut))
            @test collect(mut) == collect(MemoryView(mut))
        end
    end
end

@testset "Pointers" begin
    memory = Memory{UInt16}(undef, 10)
    mem = MemoryView(memory)[3:7]
    @test pointer(mem) == pointer(memory) + 4
    @test pointer(mem, 3) == pointer(memory) + 8

    v = view(rand(UInt32, 100), 19:55)
    mem = MemoryView(v)
    @test pointer(mem) == pointer(v)
    @test pointer(mem, 4) == pointer(v, 4)

    v = ["kls", "dsddaefe", "", "adsad"]
    mem = MemoryView(v)[2:end]
    @test pointer(v) + 8 == pointer(mem)
    @test pointer(v, 2) == pointer(mem)
    @test pointer(v, 3) == pointer(mem, 2)
end

@testset "Misc functions" begin
    @testset "Copying" begin
        # Immutable
        mem = MemoryView("abcdef")
        @test copy(mem) == mem

        # Mutable
        v = [1, 2, 3, 4, 5]
        mem = MemoryView(v)[2:4]
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
        mem = MemoryView(view(codeunits("lkdjfldfe"), 3:8))[2:6]
        @test parentindices(mem) == (4:8,)

        mem = MemoryView(UInt32[2, 5, 2, 1, 6, 8])[4:end]
        @test parentindices(mem) == (4:6,)

        mem = MemoryView(view(Vector{String}(undef, 10), 5:7))
        @test parentindices(mem) == (5:7,)
    end

    @testset "Similar and empty" begin
        mem = MemoryView(Int16[6, 4, 3])
        @test typeof(empty(mem)) == typeof(mem)
        @test isempty(empty(mem))

        mem2 = empty(mem, Int8)
        @test isempty(mem2)
        @test typeof(mem2) == MutableMemoryView{Int8}

        mem = MemoryView("abc")
        mem2 = similar(mem)
        @test length(mem2) == length(mem)
        @test typeof(mem2) == MutableMemoryView{UInt8}

        mem = MemoryView(String["", "", ""])
        mem2 = similar(mem, Int, 4)
        @test length(mem2) == 4
        @test eltype(mem2) == Int

        mem = empty(ImmutableMemoryView{Tuple{Int, UInt8}})
        @test isempty(mem)
        @test mem isa ImmutableMemoryView{Tuple{Int, UInt8}}
        mem = empty(MutableMemoryView{Float16})
        @test isempty(mem)
        @test mem isa MutableMemoryView{Float16}
    end

    @testset "Sizeof" begin
        @test sizeof(MemoryView("abc")) == 3
        @test sizeof(MemoryView([1, 2, 3])) == 3 * sizeof(Int)
        @test sizeof(MemoryView(String["", "", "", ""])) == 4 * sizeof(Int)
    end

    @testset "Copyto" begin
        # Copy!
        v1 = [5, 2, 1, 9, 8]
        v2 = [0, 2, 6, 3, 9]
        mem1 = MemoryView(v1)
        mem2 = MemoryView(v2)
        copy!(mem1, mem2)
        @test v1 == v2
        @test mem1 == mem2

        @test_throws BoundsError copy!(MemoryView([1]), MemoryView([1, 2]))
        @test_throws BoundsError copy!(MemoryView([1, 2]), MemoryView([1]))

        # Copyto!
        v1 = [4, 2, 6, 7, 9]
        v2 = [1, 5, 2, 3]
        copyto!(MemoryView(v1), MemoryView(v2))
        @test v1 == [1, 5, 2, 3, 9]
        @test_throws BoundsError copyto!(MemoryView(v2), MemoryView(v1))

        # unsafe_copyto!
        v1 = [3, 6, 2, 1]
        v2 = [0, 9, 5]
        unsafe_copyto!(MemoryView(v1), MemoryView(v2))
        @test v1 == [0, 9, 5, 1]
        v2 = rand(Int, 4)
        unsafe_copyto!(MemoryView(v1), MemoryView(v2))
        @test v2 == v1
    end

    @testset "Reverse and reverse!" begin
        for v in [
                ["a", "abc", "a", "c", "kij"],
                [0x09, 0x05, 0x02, 0x01],
                [1.0f0, -10.0f5, Inf32, Inf32],
                [nothing, nothing, nothing],
            ]
            @test reverse!(MemoryView(copy(v))) == MemoryView(reverse(v))
            mem = MemoryView(v)
            rev = reverse(mem)
            @test typeof(rev) == typeof(mem)
            @test rev.ref != mem.ref
            @test rev == reverse(v)
            @test_throws Exception reverse!(ImmutableMemoryView(v))
        end
        mem = MemoryView("abcd")
        rev = reverse(mem)
        @test rev == b"dcba"
        @test rev isa ImmutableMemoryView{UInt8}
    end

    @testset "Cmp" begin
        for (a, b, y) in [
                ([0x01], [0x00], 1),
                (UInt8[], UInt8[], 0),
                ([0x02, 0x03], [0x02, 0x03, 0x01], -1),
                ([0x02, 0x03, 0x01], [0x02, 0x03], 1),
                ([0x9f], [0x9f], 0),
                ([0x01, 0x03, 0x02], [0x01, 0x04], -1),
            ]
            @test cmp(MemoryView(a), MemoryView(b)) == y
        end
    end

    @testset "Parent" begin
        mem = Memory{UInt16}(undef, 3)
        vec = Base.wrap(Array, mem, (3,))
        v = MemoryView(vec)
        @test parent(v) === mem
        @test parent(ImmutableMemoryView(mem)) === mem
    end

    @testset "Split first and last and at" begin
        for mem in Any[
                MemoryView(b"abcde"),
                MemoryView(Any["abc", "def", "ghi"]),
                ImmutableMemoryView(rand(2, 2)),
            ]
            @test split_first(mem) == (mem[1], mem[2:end])
            @test split_last(mem) == (mem[end], mem[1:(end - 1)])
            @test split_at(mem, 1) == (mem[1:0], mem[1:end])
            @test split_at(mem, 2) == (mem[1:1], mem[2:end])
            @test split_at(mem, lastindex(mem)) == (mem[1:(end - 1)], mem[end:end])
            @test split_at(mem, lastindex(mem) + 1) == (mem[1:end], mem[1:0])
            mem = mem[2:2]
            @test split_first(mem) == (mem[1], mem[2:end])
            @test split_last(mem) == (mem[end], mem[1:(end - 1)])
            mem = mem[1:0]
            @test_throws BoundsError split_first(mem)
            @test_throws BoundsError split_last(mem)
        end

        # Split empty mem at
        mem = MemoryView(UInt16[])
        (v1, v2) = split_at(mem, 1)
        @test v1 == v2
        @test isempty(v1)
    end

    @testset "Split unaligned" begin
        for v in Any[["abc", "def"], Union{Int, UInt}[1, 2, 3, 4], Signed[4, 1, 2]]
            @test_throws Exception split_unaligned(MemoryView(v), Val(1))
        end
        v = MemoryView(collect(0x00:0x3f))[2:end]
        @test_throws Exception split_unaligned(v, Val(3))
        @test_throws Exception split_unaligned(v, Val(0))
        @test_throws Exception split_unaligned(v, Val(-2))

        @test split_unaligned(v, Val(1)) == split_at(v, 1)
        @test split_unaligned(v, Val(4)) == split_at(v, 4)
        @test split_unaligned(v, Val(8)) == split_at(v, 8)
        @test split_unaligned(v, Val(16)) == split_at(v, 16)

        v = v[2:4]
        @test split_unaligned(v, Val(16)) == split_at(v, length(v) + 1)
        @test split_unaligned(v, Val(8)) == split_at(v, length(v) + 1)

        v = MemoryView(collect(0x0000:0x003f))[3:end]
        @test split_unaligned(v, Val(1)) == split_at(v, 1)
        @test split_unaligned(v, Val(4)) == split_at(v, 1)
        @test split_unaligned(v, Val(8)) == split_at(v, 3)
        @test split_unaligned(v, Val(16)) == split_at(v, 7)
    end

    @testset "Find" begin
        @testset "Generic find" begin
            mem = ImmutableMemoryView([1, 2, 3, 4])
            @test findfirst(isodd, mem) == 1
            @test findfirst(isodd, mem[2:end]) == 2
            @test findfirst(mem[1:0]) === nothing

            @test findlast(isodd, mem) == 3
            @test findlast(isodd, mem[1:2]) == 1
            @test findlast(isodd, mem[1:0]) === nothing

            @test findnext(isodd, mem, 0x02) == 3
            @test findnext(isodd, mem, 3) == 3
            @test findnext(isodd, mem, 0x04) === nothing
            @test findnext(isodd, mem, 10) === nothing

            @test_throws BoundsError findnext(isodd, mem, 0)
            @test_throws BoundsError findnext(isodd, mem, -1)

            @test findprev(isodd, mem, 4) == 3
            @test findprev(isodd, mem, 0x03) == 3
            @test findprev(isodd, mem, 2) == 1
            @test findprev(isodd, mem, 0x00) === nothing
            @test findprev(isodd, mem, -10) === nothing

            @test_throws BoundsError findprev(isodd, mem, 5)
            @test_throws BoundsError findprev(isodd, mem, 7)
        end

        @testset "Memchr routines" begin
            for T in Any[Int8, UInt8]
                mem = MemoryView(T[6, 2, 7, 0, 2, 1])
                @test findfirst(iszero, mem) == 4
                @test findfirst(==(T(2)), mem) == 2
                @test findnext(==(T(2)), mem, 3) == 5
                @test findnext(==(T(7)), mem, 4) === nothing
                @test findnext(==(T(2)), mem, 7) === nothing
                @test_throws BoundsError findnext(iszero, mem, 0)
                @test_throws BoundsError findnext(iszero, mem, -3)

                @test findlast(iszero, mem) == 4
                @test findprev(iszero, mem, 3) === nothing
                @test findprev(iszero, mem, 4) == 4
                @test findprev(==(T(2)), mem, 5) == 5
                @test findprev(==(T(2)), mem, 4) == 2
                @test findprev(==(T(9)), mem, 3) === nothing
                @test findprev(==(T(2)), mem, -2) === nothing
                @test findprev(iszero, mem, 0) === nothing
                @test_throws BoundsError findprev(iszero, mem, 7)
            end
            mem = MemoryView(Int8[2, 3, -1])
            @test findfirst(==(0xff), mem) === nothing
            @test findprev(==(0xff), mem, 3) === nothing
        end

        @testset "Find" begin
            mem = MemoryView([4, 3, 2])
            @test findfirst(==(2), mem) == 3

            mem = MemoryView(Int8[6, 2, 7, 0, 2])
            @test findfirst(iszero, mem) == 4
            @test findfirst(==(Int8(0)), mem) == 4

            mem = MemoryView(UInt8[1, 4, 2, 5, 6])
            @test findnext(==(0x04), mem, 1) == 2
            @test findnext(==(0x04), mem, 3) === nothing
        end
    end
end

@testset "Iterators.reverse" begin
    for v in Any[AbstractString["abc", "def", ""], Char['a', 'b'], UInt32[], Int16[9, 2, 1]]
        mem = MemoryView(v)
        it = Iterators.reverse(mem)
        @test length(it) == length(mem)
        @test collect(it) == reverse(mem)
        @test Iterators.reverse(it) === ImmutableMemoryView(mem)
    end
end

@testset "split_each" begin
    it = split_each(b"abcdba", 0x00)
    @test eltype(it) == ImmutableMemoryView{UInt8}
    @test it isa DelimitedIterator{UInt8, Immutable}
    it = split_each(["abc", "def", "ghi"], "")
    @test eltype(it) == MutableMemoryView{String}
    @test it isa DelimitedIterator{String, Mutable}

    @test collect(split_each(b"", 0x00)) == ImmutableMemoryView{UInt8}[]
    @test collect(split_each([1, 2, 3, 3, 4, 5, 2, 3], 3)) == [[1, 2], [], [4, 5, 2], []]
    @test collect(split_each(String[], "")) == String[]
    @test collect(split_each([1.0, 0.0, 2.0, -0.0, 5], -0.0)) == [[1.0, 0.0, 2.0], [5.0]]
    @test collect(split_each([Dict()], Dict())) == [[], []]
end

@testset "Equality" begin
    v = rand(UInt, 10)
    m1 = MemoryView(v)
    m2 = MemoryView(copy(v))
    @test m1 == m2
    @test m1 !== m2

    m2 = m2[1:(end - 1)]
    @test m1 != m2
    m1 = m1[1:(end - 2)]
    @test m1 != m2
    m2 = m2[1:(end - 1)]
    @test m1 == m2

    # These only differ in the type metadata, make sure they are distinguished
    m1 = MemoryView(Union{Int, UInt}[-1])
    m2 = MemoryView(Union{Int, UInt}[typemax(UInt)])
    @test m1 != m2

    m1 = MemoryView([1, 2, 3])
    m2 = ImmutableMemoryView([1, 2, 3])
    @test m1 == m2
    @test m2 == m1
end

@testset "IO" begin
    # Empty IO
    buf = IOBuffer()
    v = fill(0xaa, 25)
    @test iszero(readbytes!(buf, MemoryView(v)))
    @test all(==(0xaa), v)

    # Buffer running EOF
    data = b"Hello, world!"
    buf = IOBuffer(data)
    @test readbytes!(buf, MemoryView(v)) == length(data)
    @test v == vcat(data, fill(0xaa, 25 - length(data)))

    # With nb being lower
    data = b"Hello, world!"
    buf = IOBuffer(data)
    v = fill(0xaa, 25)
    readbytes!(buf, MemoryView(v), 7)
    @test v[1:8] == b"Hello, \xaa"

    # With nb being higher than the vector length
    data = b"Hello, world!"
    buf = IOBuffer(data)
    v = fill(0xaa, 8)
    readbytes!(buf, MemoryView(v), 10)
    @test v == b"Hello, w"
end

@testset "Base arrays" begin
    @testset "Memory construction" begin
        v = ImmutableMemoryView([5, 2, 1])
        @test Memory(v) isa Memory{Int}
        @test Memory(v) == v

        @test Memory{Int}(v) isa Memory{Int}
        @test Memory{Int}(v) == v
        @test Memory{Int}(v) !== parent(v)

        @test isempty(Memory{Int}(v[1:0]))
    end

    @testset "Vector construction" begin
        v = ImmutableMemoryView(["abc", "def", "hi"])
        @test Vector(v) isa Vector{String}
        @test Vector(v) == v

        @test Vector{String}(v) isa Vector{String}
        @test Vector{String}(v) == v

        @test isempty(Vector{String}(v[1:0]))
    end

    @testset "Vector/Memory copying" begin
        v = [5, 1, 3, 6, 7, 2]
        m = ImmutableMemoryView([6, 2, 1])
        @test copyto!(v, m) === v
        @test v == [6, 2, 1, 6, 7, 2]

        @test copy!(v, m) === v
        @test v == m

        @test_throws MethodError copy!(Memory{Int}(undef, 2), m)

        v = Memory{Int}(undef, 3)
        @test copy!(v, m) === v
        @test v == m
    end

    @testset "Vector append!" begin
        m = ImmutableMemoryView([7, 2, 1])
        v = Int[]
        @test append!(v, m) === v
        @test v == m

        m = m[1:0]
        append!(v, m)
        @test v == [7, 2, 1]

        @test append!(v, MemoryView([2, 1])) === v
        @test v == [7, 2, 1, 2, 1]
    end
end

@testset "From parts" begin
    v = [1 2; 3 4]
    ref = Base.cconvert(Ptr, v)
    mem = unsafe_from_parts(ref, 3)
    @test mem == [1, 3, 2]
    @test unsafe_from_parts(ref, 2) == [1, 3]
    @test isempty(unsafe_from_parts(ref, 0))
    @test mem isa MutableMemoryView{Int}
end

@testset "MemoryKind" begin
    @test MemoryKind(Vector{Int16}) == IsMemory(MutableMemoryView{Int16})
    @test MemoryKind(typeof(codeunits(view("abc", 2:3)))) ==
        IsMemory(ImmutableMemoryView{UInt8})
    @test MemoryKind(typeof(view(Memory{String}(undef, 3), Base.OneTo(2)))) ==
        IsMemory(MutableMemoryView{String})
    @test MemoryKind(Matrix{Nothing}) == IsMemory(MutableMemoryView{Nothing})
    @test MemoryKind(Memory{Int32}) == IsMemory(MutableMemoryView{Int32})
    @test MemoryKind(typeof(view([1], 1:1))) == IsMemory(MutableMemoryView{Int})

    @test inner(IsMemory(MutableMemoryView{Int32})) == MutableMemoryView{Int32}
    @test inner(IsMemory(ImmutableMemoryView{Tuple{String, Int}})) ==
        ImmutableMemoryView{Tuple{String, Int}}

    @test MemoryKind(SubString{String}) == NotMemory()
    @test MemoryKind(String) == NotMemory()
    @test MemoryKind(Int) == NotMemory()
    @test MemoryKind(Nothing) == NotMemory()
    @test MemoryKind(Union{}) == NotMemory()
    @test_throws Exception inner(NotMemory())
end

@testset "StringViews" begin
    # Backed by mutable array
    s = StringView([0x01, 0x02])
    @test MemoryView(s) isa MutableMemoryView{UInt8}
    @test MemoryView(s) == [0x01, 0x02]
    @test MemoryKind(typeof(s)) == IsMemory{MutableMemoryView{UInt8}}()

    # Backed by immutable string data
    s = StringView(view(codeunits("abcd"), 2:4))
    @test MemoryView(s) isa ImmutableMemoryView{UInt8}
    @test MemoryView(s) == codeunits("bcd")
    @test MemoryKind(typeof(s)) == IsMemory{ImmutableMemoryView{UInt8}}()

    # Not backed by memory
    s = StringView(view(0x61:0x65, 2:4))
    @test_throws MethodError MemoryView(s)
    @test MemoryKind(typeof(s)) == NotMemory()
end

@testset "FixedSizeArrays" begin
    for A in [
            FixedSizeArray([1, 2, 3]),
            FixedSizeArray(zeros(Float32, 4, 3)),
            FixedSizeArray(fill("abc", 4, 3)),
            FixedSizeArray{Int16}(undef, 2, 2),
            FixedSizeArray(b"Hello, world!"),
        ]
        mem = MemoryView(A)
        @test length(mem) == length(A)
        @test mem == vec(A)
        @test typeof(mem) == MutableMemoryView{eltype(A)}
        @test MemoryKind(typeof(A)) == IsMemory(MutableMemoryView{eltype(A)})
    end
end

Aqua.test_all(MemoryViews; ambiguities = false)
