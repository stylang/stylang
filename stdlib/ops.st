//! # Overloadable operators.
//! Implementing these traits allows you to overload certain operators.

/// The addition operator +.
pub trait Add<Rhs = Self> {
    type Output;

    fn add(self, rhs: Rhs) -> Self::Output;
}

/// The addition assignment operator +=.
pub trait AddAssign<Rhs = Self> {
    fn add_assign(&mut self, rhs: Rhs);
}

/// The multiplication operator *.
pub trait Mul<Rhs = Self> {
    type Output;

    // Required method
    fn mul(self, rhs: Rhs) -> Self::Output;
}

/// The pow operator **.
pub trait Pow<Rhs = Self> {
    type Output;

    // Required method
    fn pow(self, rhs: Rhs) -> Self::Output;
}

/// The pow assignment operator **=.
pub trait PowAssign<Rhs = Self> {
    // Required method
    fn pow_assign(&mut self, rhs: Rhs);
}

/// The multiplication assignment operator *=.
pub trait MulAssign<Rhs = Self> {
    // Required method
    fn mul_assign(&mut self, rhs: Rhs);
}
