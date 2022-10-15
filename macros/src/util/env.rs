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

impl Env {
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
}