# variant_task
Type-safe union interface implementation from C++17 standard, excluding:
 - constructors and assignment operators, taking `std::initializer_list`
 - `monostate`
 - `hash`
 - 100% compliance with non-deleted assignment operators and constructors from underlying union - should be fixed by inflating corresponding code in 1.5 times
