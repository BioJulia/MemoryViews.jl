module StringViewsExt

using StringViews: StringView
import MemoryViews: MemoryView, MemoryKind

MemoryView(s::StringView) = MemoryView(codeunits(s))
MemoryKind(::Type{StringView{A}}) where {A} = MemoryKind(A)

end # module
