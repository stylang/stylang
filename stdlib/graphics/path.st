//! Paths represent the outline of a shape which can be filled, stroked,
//! used as a clipping path, or any combination of the three.

use super::types::*;

/// The "moveto" commands (M or m) establish a new current point. The effect is as if the "pen"
/// were lifted and moved to a new location. A path data segment (if there is one) must begin
/// with a "moveto" command. Subsequent "moveto" commands (i.e., when the "moveto" is not the
/// first command) represent the start of a new subpath:
@runtime
pub extern fn move_to(absolute: bool, points: ...Coord) -> draw;

/// Close the current subpath by drawing a straight line from the current point to current
/// subpath's initial point.
@runtime
pub extern fn close_path() -> draw;
