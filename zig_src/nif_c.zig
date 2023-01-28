// This module has a purpose. I was having type conflicts for C structure types
// when using @cInclude in two different zig modules. That is why it is important
// to have the @cInclude done only once.

pub usingnamespace @cImport({
    @cInclude("erl_nif.h");
});
