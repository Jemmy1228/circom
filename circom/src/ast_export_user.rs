use program_structure::program_archive::ProgramArchive;
use ast_export::export_ast;
use ast_export::ExportConfig;

pub fn export_project(program_archive: &ProgramArchive, config: ExportConfig) {
    export_ast(program_archive, config);
}
