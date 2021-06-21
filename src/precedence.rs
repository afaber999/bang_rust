
use Precedence::{P0,P1,P2, P3, PMax};
use variant_count::VariantCount;


#[derive(Debug, Clone, Copy,  VariantCount, PartialEq, PartialOrd)]
pub enum Precedence {
    P0,
    P1,
    P2,
    P3,
    PMax,
}

impl Precedence {
    pub fn next( current : Self ) -> Self {
        match current {
            P0 => P1,
            P1 => P2,
            P2 => P3,
            P3 | PMax => PMax,
        }
    } 
}