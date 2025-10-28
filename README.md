# gleam_manifold

> ![WARNING]
> This library depends on internal implementation details of Gleam's `erlang/process` module. It is not officially supported and may break with future Gleam releases. Use at your own risk.

Gleam bindings to [Manifold](https://github.com/discord/manifold) - an Elixir library for fast message passing between BEAM nodes.

## What is Manifold?

Manifold is an Elixir library originally developed by Discord that optimizes sending the same message to many processes. Instead of sending messages sequentially (which can be slow with thousands of processes), Manifold uses a divide-and-conquer approach that distributes the work across multiple sender processes, achieving much better performance at scale.

## Installation

This library is intentionally not published to hex, because it depends on internal implementation details of Gleam's `erlang/process` module. If Gleam opens up the Subject type, we can implement this library in a more official way and publish to hex.

For now, please depend on it as a git dependency in your `gleam.toml`:

```toml
[dependencies]
gleam_manifold = { git = "git@github.com:otters/gleam_manifold.git", ref = "<commit hash>" }
```

## Usage

### Type-safe sending to Subjects

The primary way to use this library is with Gleam's type-safe `Subject` system:

```gleam
import gleam/erlang/process
import gleam_manifold as manifold

pub fn example() {
  let subject = process.new_subject()

  // Send a message through Manifold
  manifold.send(subject, "Hello world")
}
```

### Sending to PIDs

For cases where you need to send to raw PIDs (not type-safe):

```gleam
import gleam_manifold as manifold

pub fn send_to_pid(pid: process.Pid) {
  manifold.send_pid(pid, "Hello")
}
```

### Multicasting to multiple PIDs

Send the same message to multiple processes at once:

```gleam
import gleam_manifold as manifold

pub fn broadcast_join(pids: List(process.Pid), name: String) {
  manifold.send_multi_pid(pids, "Hello, " <> name)
}
```

## Implementation Notes

### Internals Hack

This library uses internals of Gleam's `erlang/process` implementation by unwrapping the opaque `Subject` type in Erlang. This is technically a hack since `Subject` is meant to be opaque, but it's necessary to extract the PID and reference tag needed for sending to Gleam's Subject type. The implementation pattern matches on the internal `{subject, Pid, Ref}` tuple structure in `gleam_manifold_ffi.erl`.

### Current Limitations

1. **No options parameter support**: The Elixir Manifold library supports an options parameter for tuning performance characteristics (like partition size). This Gleam wrapper doesn't currently expose these options, though this could be added in the future.

2. **No named subjects support**: Named subjects (registered process names) are not currently supported. The library will fail with an assertion error if you try to use a named subject. This is fixable but hasn't been implemented yet.

## Testing

Run the tests with:

```bash
gleam test
```

## License

See the LICENSE file in the repository.
