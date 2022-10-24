use std::collections::BTreeMap;
use chalk_integration::error::RustIrError;
use chalk_integration::Identifier;
use chalk_integration::interner::ChalkIr;
use chalk_ir::{BoundVar, DebruijnIndex};
use chalk_ir::fold::Shift;

pub struct Env {
    /*
    pub adt_ids: &'k AdtIds,
    pub adt_kinds: &'k AdtKinds,
    pub fn_def_ids: &'k FnDefIds,
    pub fn_def_kinds: &'k FnDefKinds,
    pub closure_ids: &'k ClosureIds,
    pub closure_kinds: &'k ClosureKinds,
    pub trait_ids: &'k TraitIds,
    pub trait_kinds: &'k TraitKinds,
    pub opaque_ty_ids: &'k OpaqueTyIds,
    pub opaque_ty_kinds: &'k OpaqueTyVariableKinds,
    pub associated_ty_lookups: &'k AssociatedTyLookups,
    pub auto_traits: &'k AutoTraits,
    pub foreign_ty_ids: &'k ForeignIds,
    pub generator_ids: &'k GeneratorIds,
    pub generator_kinds: &'k GeneratorKinds,
    /// GenericArg identifiers are used as keys, therefore
    /// all identifiers in an environment must be unique (no shadowing).
     */
    pub parameter_map: ParameterMap,
}

pub type ParameterMap = BTreeMap<Identifier, chalk_ir::WithKind<ChalkIr, BoundVar>>;

