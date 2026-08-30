# AGENTS.md
* Do not use underscores for internal names. Only use underscores in the case a public method immediately forwards to an internal implementation for dispatch reasons; in that case a public function `foo` may forward to `_foo`.
* After changes, format with Runic: Attempt `runic -i .`, and alert the user if Runic is not installed or on your $PATH.
* Performance: All boundschecking operations needs to have boundschecking in a `@boundscheck` block.
* Performance: Beware of unnecessary error checks when converting between Int and UInt, even when we know they are impossible (e.g. UInt(length(::Vector)) can never throw because no Vector length can be < 0). Remove these checks by casting with `%`
