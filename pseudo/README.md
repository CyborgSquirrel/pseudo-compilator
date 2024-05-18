If you're trying to build the compiler, while in a `nix develop` environment,
and you get an error that looks like this:

```
error: No suitable version of LLVM was found system-wide or pointed
```

Then run `cargo clean`, and try building again.
