using Documenter, MemoryViews

DocMeta.setdocmeta!(MemoryViews, :DocTestSetup, :(using MemoryViews); recursive = true)

makedocs(;
    sitename = "MemoryViews.jl",
    modules = [MemoryViews],
    pages = [
        "MemoryViews" => "index.md",
        "MemoryViews in interfaces" => "interfaces.md",
        "MemoryViews in Base" => "base.md",
        "Migrating from 0.4 to 0.5" => "migration.md",
        "Reference" => "reference.md",
    ],
    authors = "Jakob Nybo Nissen",
    checkdocs = :public,
    remotes = nothing,
)

deploydocs(;
    repo = "github.com/BioJulia/MemoryViews.jl.git",
    push_preview = true,
    deps = nothing,
    make = nothing,
)
