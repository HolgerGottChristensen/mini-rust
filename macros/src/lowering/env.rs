use std::collections::BTreeMap;
use chalk_integration::{Identifier, TypeKind};
use chalk_integration::interner::ChalkIr;
use chalk_ir::{BoundVar, DebruijnIndex, VariableKinds};
use chalk_ir::interner::HasInterner;
use crate::lowering::LowerResult;

pub type AdtIds = BTreeMap<Identifier, chalk_ir::AdtId<ChalkIr>>;
pub type FnDefIds = BTreeMap<Identifier, chalk_ir::FnDefId<ChalkIr>>;
pub type ClosureIds = BTreeMap<Identifier, chalk_ir::ClosureId<ChalkIr>>;
pub type TraitIds = BTreeMap<Identifier, chalk_ir::TraitId<ChalkIr>>;
pub type GeneratorIds = BTreeMap<Identifier, chalk_ir::GeneratorId<ChalkIr>>;
pub type OpaqueTyIds = BTreeMap<Identifier, chalk_ir::OpaqueTyId<ChalkIr>>;
pub type AdtKinds = BTreeMap<chalk_ir::AdtId<ChalkIr>, TypeKind>;
pub type FnDefKinds = BTreeMap<chalk_ir::FnDefId<ChalkIr>, TypeKind>;
pub type ClosureKinds = BTreeMap<chalk_ir::ClosureId<ChalkIr>, TypeKind>;
pub type TraitKinds = BTreeMap<chalk_ir::TraitId<ChalkIr>, TypeKind>;
pub type AutoTraits = BTreeMap<chalk_ir::TraitId<ChalkIr>, bool>;
pub type OpaqueTyVariableKinds = BTreeMap<chalk_ir::OpaqueTyId<ChalkIr>, TypeKind>;
pub type GeneratorKinds = BTreeMap<chalk_ir::GeneratorId<ChalkIr>, TypeKind>;

pub type ParameterMap = BTreeMap<Identifier, chalk_ir::WithKind<ChalkIr, BoundVar>>;


#[derive(Clone, Debug)]
pub struct Env<'k> {
    pub adt_ids: &'k AdtIds,
    pub adt_kinds: &'k AdtKinds,
    pub fn_def_ids: &'k FnDefIds,
    pub fn_def_kinds: &'k FnDefKinds,
    pub trait_ids: &'k TraitIds,
    pub trait_kinds: &'k TraitKinds,
    pub opaque_ty_ids: &'k OpaqueTyIds,
    pub opaque_ty_kinds: &'k OpaqueTyVariableKinds,
    pub auto_traits: &'k AutoTraits,
    pub generator_ids: &'k GeneratorIds,
    pub generator_kinds: &'k GeneratorKinds,
    /// GenericArg identifiers are used as keys, therefore
    /// all identifiers in an environment must be unique (no shadowing).
    pub parameter_map: ParameterMap,
}

impl Env<'_> {
    /// Introduces new parameters, shifting the indices of existing
    /// parameters to accommodate them. The indices of the new binders
    /// will be assigned in order as they are iterated.
    pub fn introduce<I>(&self, binders: I) -> Self
        where
            I: IntoIterator<Item = chalk_ir::WithKind<ChalkIr, chalk_integration::Identifier>>,
            I::IntoIter: ExactSizeIterator,
    {
        // As binders to introduce we recieve `ParameterKind<Ident>`,
        // which we need to transform into `(Ident, ParameterKind<BoundVar>)`,
        // because that is the key-value pair for ParameterMap.
        // `swap_inner` lets us do precisely that, replacing `Ident` inside
        // `ParameterKind<Ident>` with a `BoundVar` and returning both.
        let binders = binders.into_iter().enumerate().map(|(i, k)| {
            let (kind, name) = k.into();
            (
                name,
                chalk_ir::WithKind::new(kind, BoundVar::new(DebruijnIndex::INNERMOST, i)),
            )
        });
        let len = binders.len();

        // For things already in the parameter map, we take each existing key-value pair
        // `(Ident, ParameterKind<BoundVar>)` and shift in the inner `BoundVar`.
        let parameter_map: ParameterMap = self
            .parameter_map
            .iter()
            .map(|(k, v)| (k.clone(), v.map_ref(|b| b.shifted_in())))
            .chain(binders)
            .collect();
        if parameter_map.len() != self.parameter_map.len() + len {
            panic!("{:?}", "Panic in env.rs");
            //return Err(RustIrError::DuplicateOrShadowedParameters);
        }
        Env {
            parameter_map,
            ..*self
        }
    }

    pub fn in_binders<I, T, OP>(&self, binders: I, op: OP) -> LowerResult<chalk_ir::Binders<T>>
        where
            I: IntoIterator<Item = chalk_ir::WithKind<ChalkIr, Identifier>>,
            I::IntoIter: ExactSizeIterator,
            T: HasInterner<Interner = ChalkIr>,
            OP: FnOnce(&Self) -> LowerResult<T>,
    {
        let binders: Vec<_> = binders.into_iter().collect();
        let env = self.introduce(binders.iter().cloned());
        Ok(chalk_ir::Binders::new(
            VariableKinds::from_iter(ChalkIr, binders.iter().map(|v| v.kind.clone())),
            op(&env)?,
        ))
    }
}