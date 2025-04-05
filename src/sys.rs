#[cfg(unix)]
mod unix;
#[cfg(windows)]
mod windows;
#[cfg(target_os = "uefi")]
mod uefi;

#[cfg(unix)]
pub use unix::*;
#[cfg(windows)]
pub use windows::*;

#[cfg(target_os = "uefi")]
pub use uefi::*;

#[cfg(all(not(windows), not(target_os = "uefi")))]
pub use std::fs::canonicalize;