/// An example API interface for interacting with smart contracts,
/// demonstrating how to implement conditional compilation.
@cfg_attr(test, contract("0x000000000000000000000"))
@cfg_attr(not(test), contract("0x000000000000000000001"))
pub extern fn withdraw(from: string, to: string, value: u256) -> string;
