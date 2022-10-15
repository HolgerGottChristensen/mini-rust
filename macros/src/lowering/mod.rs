use chalk_integration::error::RustIrError;
use chalk_integration::interner::ChalkIr;
use chalk_integration::{Identifier, TypeKind};
use chalk_integration::lowering::Lower;

mod program;
mod lowering;
mod program_lowerer;
mod env;

pub use env::Env;
pub use program::environment;


pub type LowerResult<T> = Result<T, RustIrError>;

pub trait LowerTypeKind {
    fn lower_type_kind(&self) -> LowerResult<TypeKind>;
}

pub trait LowerWithEnv {
    type Lowered;

    fn lower(&self, env: &Env) -> LowerResult<Self::Lowered>;
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct VariableKind(pub Identifier);

impl Lower for VariableKind {
    type Lowered = chalk_ir::WithKind<ChalkIr, Identifier>;
    fn lower(&self) -> Self::Lowered {
        let kind = chalk_ir::VariableKind::Ty(chalk_ir::TyVariableKind::General);
        chalk_ir::WithKind::new(kind, self.0.clone())
    }
}

pub trait LowerParameterMap {
    fn synthetic_parameters(&self) -> Option<chalk_ir::WithKind<ChalkIr, Identifier>>;
    fn declared_parameters(&self) -> Vec<VariableKind>;
    fn all_parameters(&self) -> Vec<chalk_ir::WithKind<ChalkIr, Identifier>> {
        self.synthetic_parameters()
            .into_iter()
            .chain(self.declared_parameters().iter().map(|id| id.lower()))
            .collect()

        /* TODO: switch to this ordering, but adjust *all* the code to match

        self.declared_parameters()
            .iter()
            .map(|id| id.lower())
            .chain(self.synthetic_parameters()) // (*) see below
            .collect()
         */

        // (*) It is important that the declared parameters come
        // before the synthetic parameters in the ordering. This is
        // because of traits, when used as types, only have the first
        // N parameters in their kind (that is, they do not have Self).
        //
        // Note that if `Self` appears in the where-clauses etc, the
        // trait is not object-safe, and hence not supposed to be used
        // as an object. Actually the handling of object types is
        // probably just kind of messed up right now. That's ok.
    }
}