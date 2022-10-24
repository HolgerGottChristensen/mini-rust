use chalk_integration::lowering::Lower;
use crate::lowering::LowerResult;
use crate::lowering::program::Program;
use crate::lowering::program_lowerer::ProgramLowerer;
use crate::mini_file::MiniFile;

impl Lower for MiniFile {
    type Lowered = LowerResult<Program>;

    fn lower(&self) -> Self::Lowered {
        let mut lowerer = ProgramLowerer::default();

        // Make a vector mapping each thing in `items` to an id,
        // based just on its position:
        let raw_ids = self
            .items
            .iter()
            .map(|_| lowerer.next_item_id())
            .collect::<Vec<_>>();

        //lowerer.extract_associated_types(self, &raw_ids)?;
        lowerer.extract_ids(self, &raw_ids)?;
        lowerer.lower(self, &raw_ids)
    }
}
