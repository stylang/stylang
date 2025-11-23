//! This document demonstrates how to design a
//! Newtonian mechanics calculator using Stylang.

use std::convert::*;
use std::ops::*;
use std::unit::*;


/// force value.
pub enum Force {
    @unit("N")
    Newton(f32),
    @unit("kN")
    KiloNewton(f32),
}

/// acceleration `ms_2`
@unit("ms_2")
pub struct Accel(pub f32);

/// Mass `kg` or `g`
@unit("kg")
pub struct Mass(pub f32);

fn main() {

    let f: Force = 12kg * 10ms_2;

    // The result is `Accel`
    let acc = 10kN / f;
}
