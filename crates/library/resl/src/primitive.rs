#![allow(non_camel_case_types)]

use std::ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Sub, SubAssign};

use super::gpu;

#[gpu]
#[derive(Clone, Copy, Debug, Default)]
#[repr(C, align(8))]
#[cfg_attr(reslc, reslc::primitive(vec2_f32))]
pub struct vec2_f32(pub f32, pub f32);

#[gpu]
#[derive(Clone, Copy, Eq, Debug, Default)]
#[repr(C, align(8))]
#[cfg_attr(reslc, reslc::primitive(vec2_u32))]
pub struct vec2_u32(pub u32, pub u32);

#[gpu]
#[derive(Clone, Copy, Eq, Debug, Default)]
#[repr(C, align(8))]
#[cfg_attr(reslc, reslc::primitive(vec2_i32))]
pub struct vec2_i32(pub i32, pub i32);

#[gpu]
#[derive(Clone, Copy, Eq, Debug, Default)]
#[cfg_attr(reslc, reslc::primitive(vec2_bool))]
pub struct vec2_bool(pub bool, pub bool);

#[gpu]
#[derive(Clone, Copy, Debug, Default)]
#[repr(C, align(16))]
#[cfg_attr(reslc, reslc::primitive(vec3_f32))]
pub struct vec3_f32(pub f32, pub f32, pub f32);

#[gpu]
#[derive(Clone, Copy, Eq, Debug, Default)]
#[repr(C, align(16))]
#[cfg_attr(reslc, reslc::primitive(vec3_u32))]
pub struct vec3_u32(pub u32, pub u32, pub u32);

#[gpu]
#[derive(Clone, Copy, Eq, Debug, Default)]
#[repr(C, align(16))]
#[cfg_attr(reslc, reslc::primitive(vec3_i32))]
pub struct vec3_i32(pub i32, pub i32, pub i32);

#[gpu]
#[derive(Clone, Copy, Eq, Debug, Default)]
#[cfg_attr(reslc, reslc::primitive(vec3_bool))]
pub struct vec3_bool(pub bool, pub bool, pub bool);

#[gpu]
#[derive(Clone, Copy, Debug, Default)]
#[repr(C, align(16))]
#[cfg_attr(reslc, reslc::primitive(vec4_f32))]
pub struct vec4_f32(pub f32, pub f32, pub f32, pub f32);

#[gpu]
#[derive(Clone, Copy, Eq, Debug, Default)]
#[repr(C, align(16))]
#[cfg_attr(reslc, reslc::primitive(vec4_u32))]
pub struct vec4_u32(pub u32, pub u32, pub u32, pub u32);

#[gpu]
#[derive(Clone, Copy, Eq, Debug, Default)]
#[repr(C, align(16))]
#[cfg_attr(reslc, reslc::primitive(vec4_i32))]
pub struct vec4_i32(pub i32, pub i32, pub i32, pub i32);

#[gpu]
#[derive(Clone, Copy, Eq, Debug, Default)]
#[cfg_attr(reslc, reslc::primitive(vec4_bool))]
pub struct vec4_bool(pub bool, pub bool, pub bool, pub bool);

macro_rules! impl_vec_partial_eq {
    ($name:ident, $el_ty_:ident, ($($el_id:tt : $el_ty:ident),*)) => {
        #[gpu]
        impl PartialEq<$name> for $name {
            fn eq(&self, other: &$name) -> bool {
                $(self.$el_id == other.$el_id) && *
            }
        }
    }
}

impl_vec_partial_eq!(vec2_f32, f32, (0: f32, 1: f32));
impl_vec_partial_eq!(vec2_u32, u32, (0: u32, 1: u32));
impl_vec_partial_eq!(vec2_i32, i32, (0: i32, 1: i32));
impl_vec_partial_eq!(vec2_bool, bool, (0: bool, 1: bool));
impl_vec_partial_eq!(vec3_f32, f32, (0: f32, 1: f32, 2: f32));
impl_vec_partial_eq!(vec3_u32, u32, (0: u32, 1: u32, 2: u32));
impl_vec_partial_eq!(vec3_i32, i32, (0: i32, 1: i32, 2: i32));
impl_vec_partial_eq!(vec3_bool, bool, (0: bool, 1: bool, 2: bool));
impl_vec_partial_eq!(vec4_f32, f32, (0: f32, 1: f32, 2: f32, 3: f32));
impl_vec_partial_eq!(vec4_u32, u32, (0: u32, 1: u32, 2: u32, 3: u32));
impl_vec_partial_eq!(vec4_i32, i32, (0: i32, 1: i32, 2: i32, 3: i32));
impl_vec_partial_eq!(vec4_bool, bool, (0: bool, 1: bool, 2: bool, 3: bool));

macro_rules! impl_vec_arith {
    ($name:ident, $el_ty_:ident, ($($el_id:tt : $el_ty:ident),*)) => {
        #[gpu]
        impl Add<$name> for $name {
            type Output = $name;

            #[cfg_attr(reslc, reslc::intrinsic(add))]
            fn add(self, rhs: $name) -> Self::Output {
                $name($(self.$el_id + rhs.$el_id),*)
            }
        }

        #[gpu]
        impl AddAssign<$name> for $name {
            fn add_assign(&mut self, rhs: $name) {
                *self = self.add(rhs);
            }
        }

        #[gpu]
        impl Add<$el_ty_> for $name {
            type Output = $name;

            #[cfg_attr(reslc, reslc::intrinsic(add))]
            fn add(self, rhs: $el_ty_) -> Self::Output {
                $name($(self.$el_id + rhs),*)
            }
        }

        #[gpu]
        impl AddAssign<$el_ty_> for $name {
            fn add_assign(&mut self, rhs: $el_ty_) {
                *self = self.add(rhs);
            }
        }

        #[gpu]
        impl Add<$name> for $el_ty_ {
            type Output = $name;

            #[cfg_attr(reslc, reslc::intrinsic(add))]
            fn add(self, rhs: $name) -> Self::Output {
                $name($(self + rhs.$el_id),*)
            }
        }

        #[gpu]
        impl Sub<$name> for $name {
            type Output = $name;

            #[cfg_attr(reslc, reslc::intrinsic(sub))]
            fn sub(self, rhs: $name) -> Self::Output {
                $name($(self.$el_id - rhs.$el_id),*)
            }
        }

        #[gpu]
        impl SubAssign<$name> for $name {
            fn sub_assign(&mut self, rhs: $name) {
                *self = self.sub(rhs);
            }
        }

        #[gpu]
        impl Sub<$el_ty_> for $name {
            type Output = $name;

            #[cfg_attr(reslc, reslc::intrinsic(sub))]
            fn sub(self, rhs: $el_ty_) -> Self::Output {
                $name($(self.$el_id - rhs),*)
            }
        }

        #[gpu]
        impl SubAssign<$el_ty_> for $name {
            fn sub_assign(&mut self, rhs: $el_ty_) {
                *self = self.sub(rhs);
            }
        }

        #[gpu]
        impl Sub<$name> for $el_ty_ {
            type Output = $name;

            #[cfg_attr(reslc, reslc::intrinsic(sub))]
            fn sub(self, rhs: $name) -> Self::Output {
                $name($(self - rhs.$el_id),*)
            }
        }

        #[gpu]
        impl Mul<$name> for $name {
            type Output = $name;

            #[cfg_attr(reslc, reslc::intrinsic(mul))]
            fn mul(self, rhs: $name) -> Self::Output {
                $name($(self.$el_id * rhs.$el_id),*)
            }
        }

        #[gpu]
        impl MulAssign<$name> for $name {
            fn mul_assign(&mut self, rhs: $name) {
                *self = self.mul(rhs);
            }
        }

        #[gpu]
        impl Mul<$el_ty_> for $name {
            type Output = $name;

            #[cfg_attr(reslc, reslc::intrinsic(mul))]
            fn mul(self, rhs: $el_ty_) -> Self::Output {
                $name($(self.$el_id * rhs),*)
            }
        }

        #[gpu]
        impl MulAssign<$el_ty_> for $name {
            fn mul_assign(&mut self, rhs: $el_ty_) {
                *self = self.mul(rhs);
            }
        }

        #[gpu]
        impl Mul<$name> for $el_ty_ {
            type Output = $name;

            #[cfg_attr(reslc, reslc::intrinsic(mul))]
            fn mul(self, rhs: $name) -> Self::Output {
                $name($(self * rhs.$el_id),*)
            }
        }

        #[gpu]
        impl Div<$name> for $name {
            type Output = $name;

            #[cfg_attr(reslc, reslc::intrinsic(div))]
            fn div(self, rhs: $name) -> Self::Output {
                $name($(self.$el_id / rhs.$el_id),*)
            }
        }

        #[gpu]
        impl DivAssign<$name> for $name {
            fn div_assign(&mut self, rhs: $name) {
                *self = self.div(rhs);
            }
        }

        #[gpu]
        impl Div<$el_ty_> for $name {
            type Output = $name;

            #[cfg_attr(reslc, reslc::intrinsic(div))]
            fn div(self, rhs: $el_ty_) -> Self::Output {
                $name($(self.$el_id / rhs),*)
            }
        }

        #[gpu]
        impl DivAssign<$el_ty_> for $name {
            fn div_assign(&mut self, rhs: $el_ty_) {
                *self = self.div(rhs);
            }
        }

        #[gpu]
        impl Div<$name> for $el_ty_ {
            type Output = $name;

            #[cfg_attr(reslc, reslc::intrinsic(div))]
            fn div(self, rhs: $name) -> Self::Output {
                $name($(self / rhs.$el_id),*)
            }
        }
    };
}

impl_vec_arith!(vec2_f32, f32, (0: f32, 1: f32));
impl_vec_arith!(vec2_i32, i32, (0: i32, 1: i32));
impl_vec_arith!(vec2_u32, u32, (0: u32, 1: u32));
impl_vec_arith!(vec3_f32, f32, (0: f32, 1: f32, 2: f32));
impl_vec_arith!(vec3_i32, i32, (0: i32, 1: i32, 2: i32));
impl_vec_arith!(vec3_u32, u32, (0: u32, 1: u32, 2: u32));
impl_vec_arith!(vec4_f32, f32, (0: f32, 1: f32, 2: f32, 3: f32));
impl_vec_arith!(vec4_i32, i32, (0: i32, 1: i32, 2: i32, 3: i32));
impl_vec_arith!(vec4_u32, u32, (0: u32, 1: u32, 2: u32, 3: u32));
