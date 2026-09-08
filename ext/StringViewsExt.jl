module StringViewsExt

using StringViews: StringView
import MemoryViews: MemoryView

MemoryView(s::StringView) = MemoryView(codeunits(s))

end # module
