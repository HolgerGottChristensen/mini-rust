use std::collections::{BTreeMap, HashSet};
use std::fmt;
use std::sync::Arc;
use chalk_integration::{Identifier, tls, TypeKind};
use chalk_integration::error::ChalkError;
use chalk_integration::interner::ChalkIr;
use chalk_integration::program_environment::ProgramEnvironment;
use chalk_integration::query::{LoweringDatabase, Upcast};
use chalk_ir::{AdtId, AliasTy, AssocTypeId, Binders, CanonicalVarKinds, ClosureId, Environment, FnDefId, ForeignDefId, GeneratorId, GenericArg, Goal, Goals, ImplId, Lifetime, OpaqueTy, OpaqueTyId, ProgramClause, ProgramClauseImplication, ProgramClauses, ProjectionTy, SeparatorTraitRef, Substitution, TraitId, Ty, TyKind, UnificationDatabase, Variance};
use chalk_ir::debug::Angle;
use chalk_solve::clauses::builder::ClauseBuilder;
use chalk_solve::clauses::program_clauses::ToProgramClauses;
use chalk_solve::rust_ir::{AdtDatum, AdtRepr, AdtSizeAlign, AssociatedTyDatum, AssociatedTyValue, AssociatedTyValueId, ClosureKind, FnDefDatum, FnDefInputsAndOutputDatum, GeneratorDatum, GeneratorWitnessDatum, ImplDatum, OpaqueTyDatum, TraitDatum, WellKnownTrait};
use chalk_solve::RustIrDatabase;
use chalk_solve::split::Split;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Program {
    /// From ADT name to item-id. Used during lowering only.
    pub adt_ids: BTreeMap<Identifier, AdtId<ChalkIr>>,

    /// For each ADT:
    pub adt_kinds: BTreeMap<AdtId<ChalkIr>, TypeKind>,

    pub adt_variances: BTreeMap<AdtId<ChalkIr>, Vec<Variance>>,

    pub fn_def_ids: BTreeMap<Identifier, FnDefId<ChalkIr>>,

    pub fn_def_kinds: BTreeMap<FnDefId<ChalkIr>, TypeKind>,

    pub fn_def_variances: BTreeMap<FnDefId<ChalkIr>, Vec<Variance>>,

    //pub closure_ids: BTreeMap<Identifier, ClosureId<ChalkIr>>,

    //pub closure_upvars: BTreeMap<ClosureId<ChalkIr>, Binders<Ty<ChalkIr>>>,

    //pub closure_kinds: BTreeMap<ClosureId<ChalkIr>, TypeKind>,

    /// For each generator
    pub generator_ids: BTreeMap<Identifier, GeneratorId<ChalkIr>>,

    pub generator_kinds: BTreeMap<GeneratorId<ChalkIr>, TypeKind>,

    pub generator_data: BTreeMap<GeneratorId<ChalkIr>, Arc<GeneratorDatum<ChalkIr>>>,

    pub generator_witness_data: BTreeMap<GeneratorId<ChalkIr>, Arc<GeneratorWitnessDatum<ChalkIr>>>,

    /// From trait name to item-id. Used during lowering only.
    pub trait_ids: BTreeMap<Identifier, TraitId<ChalkIr>>,

    /// For each trait:
    pub trait_kinds: BTreeMap<TraitId<ChalkIr>, TypeKind>,

    /// For each ADT:
    pub adt_data: BTreeMap<AdtId<ChalkIr>, Arc<AdtDatum<ChalkIr>>>,

    pub adt_reprs: BTreeMap<AdtId<ChalkIr>, Arc<AdtRepr<ChalkIr>>>,

    pub adt_size_aligns: BTreeMap<AdtId<ChalkIr>, Arc<AdtSizeAlign>>,

    pub fn_def_data: BTreeMap<FnDefId<ChalkIr>, Arc<FnDefDatum<ChalkIr>>>,

    //pub closure_inputs_and_output: BTreeMap<ClosureId<ChalkIr>, Binders<FnDefInputsAndOutputDatum<ChalkIr>>>,

    // Weird name, but otherwise would overlap with `closure_kinds` above.
    //pub closure_closure_kind: BTreeMap<ClosureId<ChalkIr>, ClosureKind>,

    /// For each impl:
    pub impl_data: BTreeMap<ImplId<ChalkIr>, Arc<ImplDatum<ChalkIr>>>,

    /// For each associated ty value `type Foo = XXX` found in an impl:
    //pub associated_ty_values: BTreeMap<AssociatedTyValueId<ChalkIr>, Arc<AssociatedTyValue<ChalkIr>>>,

    // From opaque type name to item-id. Used during lowering only.
    pub opaque_ty_ids: BTreeMap<Identifier, OpaqueTyId<ChalkIr>>,

    /// For each opaque type:
    pub opaque_ty_kinds: BTreeMap<OpaqueTyId<ChalkIr>, TypeKind>,

    /// For each opaque type:
    pub opaque_ty_data: BTreeMap<OpaqueTyId<ChalkIr>, Arc<OpaqueTyDatum<ChalkIr>>>,

    /// Stores the hidden types for opaque types
    pub hidden_opaque_types: BTreeMap<OpaqueTyId<ChalkIr>, Arc<Ty<ChalkIr>>>,

    /// For each trait:
    pub trait_data: BTreeMap<TraitId<ChalkIr>, Arc<TraitDatum<ChalkIr>>>,

    /// For each trait lang item
    pub well_known_traits: BTreeMap<WellKnownTrait, TraitId<ChalkIr>>,

    /// For each user-specified clause
    pub custom_clauses: Vec<ProgramClause<ChalkIr>>,

    /// Store the traits marked with `#[object_safe]`
    pub object_safe_traits: HashSet<TraitId<ChalkIr>>,
}

impl RustIrDatabase<ChalkIr> for Program {
    fn custom_clauses(&self) -> Vec<ProgramClause<ChalkIr>> {
        todo!()
    }

    fn associated_ty_data(&self, ty: AssocTypeId<ChalkIr>) -> Arc<AssociatedTyDatum<ChalkIr>> {
        todo!()
    }

    fn trait_datum(&self, trait_id: TraitId<ChalkIr>) -> Arc<TraitDatum<ChalkIr>> {
        todo!()
    }

    fn adt_datum(&self, adt_id: AdtId<ChalkIr>) -> Arc<AdtDatum<ChalkIr>> {
        todo!()
    }

    fn generator_datum(&self, generator_id: GeneratorId<ChalkIr>) -> Arc<GeneratorDatum<ChalkIr>> {
        todo!()
    }

    fn generator_witness_datum(&self, generator_id: GeneratorId<ChalkIr>) -> Arc<GeneratorWitnessDatum<ChalkIr>> {
        todo!()
    }

    fn adt_repr(&self, id: AdtId<ChalkIr>) -> Arc<AdtRepr<ChalkIr>> {
        todo!()
    }

    fn adt_size_align(&self, id: AdtId<ChalkIr>) -> Arc<AdtSizeAlign> {
        todo!()
    }

    fn fn_def_datum(&self, fn_def_id: FnDefId<ChalkIr>) -> Arc<FnDefDatum<ChalkIr>> {
        todo!()
    }

    fn impl_datum(&self, impl_id: ImplId<ChalkIr>) -> Arc<ImplDatum<ChalkIr>> {
        todo!()
    }

    fn associated_ty_value(&self, id: AssociatedTyValueId<ChalkIr>) -> Arc<AssociatedTyValue<ChalkIr>> {
        todo!()
    }

    fn opaque_ty_data(&self, id: OpaqueTyId<ChalkIr>) -> Arc<OpaqueTyDatum<ChalkIr>> {
        todo!()
    }

    fn hidden_opaque_type(&self, id: OpaqueTyId<ChalkIr>) -> Ty<ChalkIr> {
        todo!()
    }

    fn impls_for_trait(&self, trait_id: TraitId<ChalkIr>, parameters: &[GenericArg<ChalkIr>], binders: &CanonicalVarKinds<ChalkIr>) -> Vec<ImplId<ChalkIr>> {
        todo!()
    }

    fn local_impls_to_coherence_check(&self, trait_id: TraitId<ChalkIr>) -> Vec<ImplId<ChalkIr>> {
        todo!()
    }

    fn impl_provided_for(&self, auto_trait_id: TraitId<ChalkIr>, ty: &TyKind<ChalkIr>) -> bool {
        todo!()
    }

    fn well_known_trait_id(&self, well_known_trait: WellKnownTrait) -> Option<TraitId<ChalkIr>> {
        todo!()
    }

    fn program_clauses_for_env(&self, environment: &Environment<ChalkIr>) -> ProgramClauses<ChalkIr> {
        todo!()
    }

    fn interner(&self) -> ChalkIr {
        ChalkIr
    }

    fn is_object_safe(&self, trait_id: TraitId<ChalkIr>) -> bool {
        todo!()
    }

    fn closure_kind(&self, closure_id: ClosureId<ChalkIr>, substs: &Substitution<ChalkIr>) -> ClosureKind {
        todo!()
    }

    fn closure_inputs_and_output(&self, closure_id: ClosureId<ChalkIr>, substs: &Substitution<ChalkIr>) -> Binders<FnDefInputsAndOutputDatum<ChalkIr>> {
        todo!()
    }

    fn closure_upvars(&self, closure_id: ClosureId<ChalkIr>, substs: &Substitution<ChalkIr>) -> Binders<Ty<ChalkIr>> {
        todo!()
    }

    fn closure_fn_substitution(&self, closure_id: ClosureId<ChalkIr>, substs: &Substitution<ChalkIr>) -> Substitution<ChalkIr> {
        todo!()
    }

    fn unification_database(&self) -> &dyn UnificationDatabase<ChalkIr> {
        todo!()
    }

    fn discriminant_type(&self, ty: Ty<ChalkIr>) -> Ty<ChalkIr> {
        todo!()
    }
}

impl tls::DebugContext for Program {
    fn debug_adt_id(
        &self,
        adt_id: AdtId<ChalkIr>,
        fmt: &mut fmt::Formatter<'_>,
    ) -> Result<(), fmt::Error> {
        if let Some(k) = self.adt_kinds.get(&adt_id) {
            write!(fmt, "{}", k.name)
        } else {
            fmt.debug_struct("InvalidAdtId")
                .field("index", &adt_id.0)
                .finish()
        }
    }

    fn debug_trait_id(
        &self,
        trait_id: TraitId<ChalkIr>,
        fmt: &mut fmt::Formatter<'_>,
    ) -> Result<(), fmt::Error> {
        if let Some(k) = self.trait_kinds.get(&trait_id) {
            write!(fmt, "{}", k.name)
        } else {
            fmt.debug_struct("InvalidTraitId")
                .field("index", &trait_id.0)
                .finish()
        }
    }

    fn debug_assoc_type_id(
        &self,
        assoc_type_id: AssocTypeId<ChalkIr>,
        fmt: &mut fmt::Formatter<'_>,
    ) -> Result<(), fmt::Error> {
        todo!()
    }

    fn debug_opaque_ty_id(
        &self,
        opaque_ty_id: OpaqueTyId<ChalkIr>,
        fmt: &mut fmt::Formatter<'_>,
    ) -> Result<(), fmt::Error> {
        if let Some(k) = self.opaque_ty_kinds.get(&opaque_ty_id) {
            write!(fmt, "{}", k.name)
        } else {
            fmt.debug_struct("InvalidOpaqueTyId")
                .field("index", &opaque_ty_id.0)
                .finish()
        }
    }

    fn debug_fn_def_id(
        &self,
        fn_def_id: FnDefId<ChalkIr>,
        fmt: &mut fmt::Formatter<'_>,
    ) -> Result<(), fmt::Error> {
        if let Some(k) = self.fn_def_kinds.get(&fn_def_id) {
            write!(fmt, "{}", k.name)
        } else {
            fmt.debug_struct("InvalidFnDefId")
                .field("index", &fn_def_id.0)
                .finish()
        }
    }

    fn debug_alias(
        &self,
        alias_ty: &AliasTy<ChalkIr>,
        fmt: &mut fmt::Formatter<'_>,
    ) -> Result<(), fmt::Error> {
        match alias_ty {
            AliasTy::Projection(projection_ty) => self.debug_projection_ty(projection_ty, fmt),
            AliasTy::Opaque(opaque_ty) => self.debug_opaque_ty(opaque_ty, fmt),
        }
    }

    fn debug_projection_ty(
        &self,
        projection_ty: &ProjectionTy<ChalkIr>,
        fmt: &mut fmt::Formatter<'_>,
    ) -> Result<(), fmt::Error> {
        let (associated_ty_data, trait_params, other_params) = self.split_projection(projection_ty);
        write!(
            fmt,
            "<{:?} as {:?}{:?}>::{}{:?}",
            &trait_params[0],
            associated_ty_data.trait_id,
            Angle(&trait_params[1..]),
            associated_ty_data.name,
            Angle(other_params)
        )
    }

    fn debug_opaque_ty(
        &self,
        opaque_ty: &OpaqueTy<ChalkIr>,
        fmt: &mut fmt::Formatter<'_>,
    ) -> Result<(), fmt::Error> {
        write!(fmt, "{:?}", opaque_ty.opaque_ty_id)
    }

    fn debug_ty(&self, ty: &Ty<ChalkIr>, fmt: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        let interner = self.interner();
        write!(fmt, "{:?}", ty.kind(interner).debug(interner))
    }

    fn debug_lifetime(
        &self,
        lifetime: &Lifetime<ChalkIr>,
        fmt: &mut fmt::Formatter<'_>,
    ) -> Result<(), fmt::Error> {
        let interner = self.interner();
        write!(fmt, "{:?}", lifetime.data(interner))
    }

    fn debug_generic_arg(
        &self,
        generic_arg: &GenericArg<ChalkIr>,
        fmt: &mut fmt::Formatter<'_>,
    ) -> Result<(), fmt::Error> {
        let interner = self.interner();
        write!(fmt, "{:?}", generic_arg.data(interner).inner_debug())
    }

    fn debug_variable_kinds(
        &self,
        variable_kinds: &chalk_ir::VariableKinds<ChalkIr>,
        fmt: &mut fmt::Formatter<'_>,
    ) -> Result<(), fmt::Error> {
        let interner = self.interner();
        write!(fmt, "{:?}", variable_kinds.as_slice(interner))
    }

    fn debug_variable_kinds_with_angles(
        &self,
        variable_kinds: &chalk_ir::VariableKinds<ChalkIr>,
        fmt: &mut fmt::Formatter<'_>,
    ) -> Result<(), fmt::Error> {
        let interner = self.interner();
        write!(fmt, "{:?}", variable_kinds.inner_debug(interner))
    }

    fn debug_canonical_var_kinds(
        &self,
        variable_kinds: &chalk_ir::CanonicalVarKinds<ChalkIr>,
        fmt: &mut fmt::Formatter<'_>,
    ) -> Result<(), fmt::Error> {
        let interner = self.interner();
        write!(fmt, "{:?}", variable_kinds.as_slice(interner))
    }

    fn debug_goal(
        &self,
        goal: &Goal<ChalkIr>,
        fmt: &mut fmt::Formatter<'_>,
    ) -> Result<(), fmt::Error> {
        let interner = self.interner();
        write!(fmt, "{:?}", goal.data(interner))
    }

    fn debug_goals(
        &self,
        goals: &Goals<ChalkIr>,
        fmt: &mut fmt::Formatter<'_>,
    ) -> Result<(), fmt::Error> {
        let interner = self.interner();
        write!(fmt, "{:?}", goals.debug(interner))
    }

    fn debug_program_clause_implication(
        &self,
        pci: &ProgramClauseImplication<ChalkIr>,
        fmt: &mut fmt::Formatter<'_>,
    ) -> Result<(), fmt::Error> {
        let interner = self.interner();
        write!(fmt, "{:?}", pci.debug(interner))
    }

    fn debug_program_clause(
        &self,
        clause: &ProgramClause<ChalkIr>,
        fmt: &mut fmt::Formatter<'_>,
    ) -> Result<(), fmt::Error> {
        let interner = self.interner();
        write!(fmt, "{:?}", clause.data(interner))
    }

    fn debug_program_clauses(
        &self,
        clauses: &ProgramClauses<ChalkIr>,
        fmt: &mut fmt::Formatter<'_>,
    ) -> Result<(), fmt::Error> {
        let interner = self.interner();
        write!(fmt, "{:?}", clauses.as_slice(interner))
    }

    fn debug_substitution(
        &self,
        substitution: &Substitution<ChalkIr>,
        fmt: &mut fmt::Formatter<'_>,
    ) -> Result<(), fmt::Error> {
        let interner = self.interner();
        write!(fmt, "{:?}", substitution.debug(interner))
    }

    fn debug_separator_trait_ref(
        &self,
        separator_trait_ref: &SeparatorTraitRef<'_, ChalkIr>,
        fmt: &mut fmt::Formatter<'_>,
    ) -> Result<(), fmt::Error> {
        let interner = self.interner();
        write!(fmt, "{:?}", separator_trait_ref.debug(interner))
    }

    fn debug_quantified_where_clauses(
        &self,
        clauses: &chalk_ir::QuantifiedWhereClauses<ChalkIr>,
        fmt: &mut fmt::Formatter<'_>,
    ) -> Result<(), fmt::Error> {
        let interner = self.interner();
        write!(fmt, "{:?}", clauses.as_slice(interner))
    }

    fn debug_constraints(
        &self,
        constraints: &chalk_ir::Constraints<ChalkIr>,
        fmt: &mut fmt::Formatter<'_>,
    ) -> Result<(), fmt::Error> {
        let interner = self.interner();
        write!(fmt, "{:?}", constraints.as_slice(interner))
    }

    fn debug_variances(
        &self,
        variances: &chalk_ir::Variances<ChalkIr>,
        fmt: &mut fmt::Formatter<'_>,
    ) -> Result<(), fmt::Error> {
        let interner = self.interner();
        write!(fmt, "{:?}", variances.as_slice(interner))
    }
}


pub fn environment(program: Program) -> Result<Arc<ProgramEnvironment>, ChalkError> {

    // Construct the set of *clauses*; these are sort of a compiled form
    // of the data above that always has the form:
    //
    //       forall P0...Pn. Something :- Conditions
    let mut program_clauses = program.custom_clauses.clone();

    let builder = &mut ClauseBuilder::new(&program, &mut program_clauses);

    let env = chalk_ir::Environment::new(builder.interner());

    /*program
        .trait_data
        .values()
        .for_each(|d| d.to_program_clauses(builder, &env));*/

    program
        .adt_data
        .values()
        .for_each(|d| d.to_program_clauses(builder, &env));

    /*for (&auto_trait_id, _) in program
        .trait_data
        .iter()
        .filter(|(_, auto_trait)| auto_trait.is_auto_trait())
    {
        for &adt_id in program.adt_data.keys() {
            let ty = chalk_ir::TyKind::Adt(adt_id, Substitution::empty(builder.interner()));
            chalk_solve::clauses::push_auto_trait_impls(builder, auto_trait_id, &ty)
                .map_err(|_| ())
                .unwrap();
        }
    }*/

    /*for datum in program.impl_data.values() {
        // If we encounter a negative impl, do not generate any rule. Negative impls
        // are currently just there to deactivate default impls for auto traits.
        if datum.is_positive() {
            datum.to_program_clauses(builder, &env);
            datum
                .associated_ty_value_ids
                .iter()
                .map(|&atv_id| program.associated_ty_value(atv_id))
                .for_each(|atv| atv.to_program_clauses(builder, &env));
        }
    }*/

    Ok(Arc::new(ProgramEnvironment::new(program_clauses)))
}